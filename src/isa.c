#include <isa.h>
#include <memorymap.h>
#include <stdint.h>
#include <utils.h>

/* CPU related instructions */
void nop(gb_cpu_t *gb_cpu)
{
    /*
        The reason why we are incrementing PC is that it signifies opcode fetch
        But since there is only one opcode for this instruction and we are using
        function pointers in order to get to instructions, I saw it fit to handle
        PC increment separately for the 1-opcode instructions.

        The same rule applies for all 1-opcode instructions. Don't get confused if
        you see this later
    */
    gb_cpu->gb_reg.PC++;
    return;
}

void stop(gb_cpu_t *gb_cpu)
{
    /*
        STOP
        Instructions on how to implement this command come from multiple sources. It seems that
        when the GB is pushed into this state, the screen goes white (with one dark line?) and 
        all processes are stopped until a button is pressed. Some websites state this is via 
        the Button interrupt. 
    */
    gb_cpu->gb_reg.PC++;
}

void halt(gb_cpu_t *gb_cpu)
{
    /*
        HALT.
        Sigh.
        Okay.
        Let's talk.
        So, at its base the operation of HALT is pretty simple. Documentation states that HALT 
        stops all CPU operation until an interrupt is fired. If so, then the CPU wakes up and 
        the ISR for that interrupt is triggered. Then normal execution follows. This, of course,
        seems to be the case when IME is 1. If IME is 1, then we check for pending interrupts 
        by performing IE & IF and if not 0, then halt terminates, ISR fired, BOOM done. Get back
        to work. However. There is the case of IME==0. One of two things might happen:
            - IME==0 && (IE & IF == 0): Here when halt is hit, we wait until IE & IF != 0 (i.e 
            an interrupt is pending), stop halt and continue normal exec. ISR is not fired as
            IME=0
            - IME==0 && (IE & IF != 0): In this specific case, the byte after the HALT opcode
            is read twice. This is a known GB bug.
                - If an ei instruction is followed immediately by a halt: ei takes one inst
                delay to execute. So it will enable when halt is executed. Now, halt is called
                but if there is a pending interrupt, we will go to ISR now that ei has enabled
                interrupts. After coming back from ISR, however, we will not come out of the
                halt. We now wait for another interrupt to come out of the halt.
        
        Why. Just. Why.
    */
    gb_cpu->gb_reg.PC++;
    while (1)
    {
        uint8_t _ie = mem_read(gb_cpu, GB_INTR_ENAB_REG);
        uint8_t _if = mem_read(gb_cpu, GB_INTR_FLAG_REG);
        if ((_ie & _if) != 0)
            break;
    }
    
    if (gb_cpu->ime != 0)
        /* CALL ISR HERE */

    return;
}

void ccf(gb_cpu_t *gb_cpu)
{
    /*
        Clear Carry Flag: clears the carry flag if set
        Pretty straightforward: just clears the carry flag. cy=(cy xor 1).
    */
    gb_cpu->gb_reg.PC++;
    
    // Flag rules are - 0 0 C
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY)) ? \
        clear_carry(gb_cpu) : set_carry(gb_cpu);

    return;
}

void scf(gb_cpu_t *gb_cpu)
{
    /*
        Set Carry Flag: sets the carry flag if set
        Pretty straightforward: just sets the carry flag. cy=1.
    */
    gb_cpu->gb_reg.PC++;

    // Flag rules are - 0 0 1
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    set_carry(gb_cpu);

    return;
}

void di(gb_cpu_t *gb_cpu)
{
    /*
        Disable Interrupts: Disables the master interrupt enable flag
        IME=0. Note to self: Idk how this will help as C does not understand
        nor can fire an ISR. IDK what to do for this.
    */
    gb_cpu->gb_reg.PC++;
    gb_cpu->ime = 0;
    return;
}

void ei(gb_cpu_t *gb_cpu)
{
    /*
        Enable interrupts: Enables the master interrupt enable flag
        IME=1.
    */
    gb_cpu->gb_reg.PC++;
    gb_cpu->ime = 1;
    return;
}

/* Load/Store instructions */
void ld(gb_cpu_t *gb_cpu)
{
    /*
        load instruction
        This one is huge. When I say huge, I mean HUGE. There are 91 of them
        For the most part, there aren't any outliers in the instructions here,
        nothing too out of the ordinary. Since there are too many instructions
        here, I won't be explaining the outliers here, but rather at the place
        where I handled the case. Other than that the instruction is pretty
        cookie cutter, take data from register/memory, and place it in register/
        memory. I would like to state the types of ld instructions there are.

            - ld rr, d16: load immediate 16bit data to a 16bit register.
            - ld (rr), A: load accumulator data into memory located at address placed
            in 16bit register.
            - ld (rr+)/(rr-), A: Load accumulator data into memory located at address
            placed in 16bit register and then increment/decrement the 16bit register
            - ld r, d8: Load 8bit immediate value into 8bit register
            - ld (rr), d8: Load 8bit immediate value into memory located at address
            placed in 16bit register.
            - ld A, (rr): load 8bit value from memory located at address placed in
            16bit register into accumulator
            - ld A, (rr+)/(rr-): Load 8bit value from memory located at address placed
            in 16bit register into accumulator and increment/decrement the 16bit register
            - ld r,r: Load 8bit value from 8bit register into 8bit register
            - ld r, (rr): Load 8bit value from memory located at address placed in 16bit
            register into 8bit register
            - ld (rr), r: Load 8bit value from 8bit register into memory located at address
            placed in 16bit register.
            - ld (a8), A: Load 8bit value from accumulator into memory at address (FF00+a8)
            - ld A, (a8): Load 8bit value from memory at address (FF00+a8) into accumulator
            - ld (C), A: Load 8bit value from accumulator into memory at address (FF00+C)
            - Ld A, (C): Load 8bit value from memory at address (FF00+C) into accumulator
            - ld HL, SP+r8: Load SP + r8 (r8 is a signed 8bit number) into HL.
            - ld SP, HL: Load 16bit value from HL into SP.
            - ld (a16), A: Load 8bit value from accumulator into memory located at 16bit
            immediate address.
            - ld A, (a16): Load 8bit value from memory located at 16bit immediate address
            into accumulator.

        YEAH. That many. Get ready for some sleepless nights if you tryna code yourself.
    */
    uint8_t opcode, data8, offset;
    uint8_t *reg8ptr, *reg16high_ptr, *reg16low_ptr;
    uint16_t data16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Case ld SP, HL
    if (opcode == 0xF9)
    {
        gb_cpu->gb_reg.SP = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                            (uint16_t)(gb_cpu->gb_reg.HL.L);
        return;
    }

    /*
        Case HL, SP+r8.
        There are a few things to talk about this instruction.
        Remember that r8 here is a "signed" number. Which means that it is a 2's complement
        version of a number, and it can be either positive or negative. And it is 8bit, so
        it can hold values between -128 to 127. This allows us to load values from below and
        above the stack. Remember that this works only because if we have the 2's complement
        version of the number, then addition yeilds results based on the sign of the number
        in question. For instance, take 28 - 2 (or 28 + (-2) ??). Simple enough, should give
        us 26. But how does that work in binary?

                28:         0 0 0 1  1 1 0 0
                -2:         1 1 1 1  1 1 1 0
               ------------------------------
                26:    1    0 0 0 1  1 0 1 0
        
        Ignoring the carry that is generated, we can clearly see 2's complement gives us
        exactly what we want from this operation. Same happens here with SP+r8, but only
        difference is that we work with 16bit there. So what is the difference?

        Well, remember that we are adding an 8bit data value to a 16bit data value. And the
        signed number representation will be an 8bit, not a 16bit representation of the number.
        Viusalize like this if you do not understand:

                28: 0 0 0 0  0 0 0 0  0 0 0 1  1 1 0 0
                -2: 0 0 0 0  0 0 0 0  1 1 1 1  1 1 1 0 - This is not 2's complement of -2 in 16bit
               ----------------------------------------

        What you want is something more like this:
                28: 0 0 0 0  0 0 0 0  0 0 0 1  1 1 0 0
                -2: 1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 0 - This is 2's complement of -2 in 16bit
               ----------------------------------------

        We can make this happen by something called "sign extension". It's simple, really. Just
        copy the MSB "sign bit" across the bits that we have to fill.
    */
    if (opcode == 0xF8)
    {
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
        data16 = gb_cpu->gb_reg.SP + (uint16_t)((data8 & 0x80) ? (0xFF00 | data8) : (0x0000 | data8));

        gb_cpu->gb_reg.HL.H = (uint8_t)((data16 & 0xFF00) >> 8);
        gb_cpu->gb_reg.HL.L = (uint8_t)(data16 & 0x00FF);

        clear_zero(gb_cpu);
        clear_addsub(gb_cpu);
        (((gb_cpu->gb_reg.SP & 0x0F)+(data8 & 0x0F)) & 0x10) ? \
        set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
        (((gb_cpu->gb_reg.SP & 0xFF)+(uint16_t)(data8 & 0xFF)) & 0x100) ? \
        set_carry(gb_cpu) : clear_carry(gb_cpu);
        return;
    }

    /*
        This takes care of three sets of instructions quite similar to each other
        - LDH (a8), A   - LD (C), A    - LD (a16), A
        - LDH A, (a8)   - LD A, (C)    - LD A, (a16)
        First two sets use operands (The operands marked as "(a8)") as addresses
        with (0xFF00 + 8bitdata). This is for I/O operation. The I/O devices such
        as the gameboy control buttons are mapped as memory locations starting from
        0xFF00. So, if we need to access the state of the buttons, we simply read
        address (0xFF00 + (offset of input)). That offset is the 8bit operand for the
        first two instruction sets. One is immediate data, one is from C register.

        The third set of instructions is direct access: We provide the 16bit address
        as one of the operands directly.

        So to understand the logic of below code is simple:
            Step 1) Check if the opcode is for 16bit direct addressing mode. If yes,
            fetch the address from memory as next two bits after opcode. If no, we have
            to decide between C and memory location one byte after opcode.
                Step 1a) If opcode is for memory location one byte after opcode, fetch
                that. Else fetch data within C register. In both cases add fetched result
                to 0xFF00.
            Step 2) Check whether we have to write to resolved address or we have to read
            from resolved address and write to accumulator. Perform operation accordingly.
    */
    if ((opcode & 0xF0) >= 0xE0)
    {
        ((opcode & 0x0F) == 0x0A) ?
        (data16 = (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.PC++]) | \
                  (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.PC++] << 8)) :
        (data16 = 0xFF00 + (uint16_t)(((opcode & 0x0F) == 0x00) ? (gb_cpu->gb_mem[gb_cpu->gb_reg.PC++]) : \
                                                                  (gb_cpu->gb_reg.BC.C)));
        ((opcode & 0xF0) == 0xE0) ? (mem_write(gb_cpu, data16, gb_cpu->gb_reg.AF.A)) : \
                                    (gb_cpu->gb_reg.AF.A = mem_read(gb_cpu, data16));
        return;
    }

    /*
        Case ld rr, d16.
        This is relatively simple. Nothing major. Fetch the data we need to load into 16bit
        register from memory, two bytes after the opcode. We also have to resolve which 16bit
        register we have to load into. That logic we have looked at before so we may re-use
        that to our advantage. More explanation within code comments.
    */
    if (((opcode & 0x0F) == 0x01) && ((opcode & 0xF0) <= 0x30))
    {
        // Fetch 16bit immediate value from memory.
        data16 = (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.PC++]) | \
            (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.PC++] << 8);

        // Hardcoding case where SP is used as the register to load into
        if (opcode == 0x31)
        {
            gb_cpu->gb_reg.SP = data16;
            return;
        }
        // For all register sets other than SP
        else
        {
            // Calculate which 16bit register to use.
            offset = 2*(((opcode & 0xF0) >> 4) + 1);
            // Resolve higher and lower byte pointer to selected 16bit register
            reg16high_ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
            reg16low_ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset+1);

            // Load the 16bit value into the 16bit register
            *reg16high_ptr = (uint8_t)((data16 & 0xFF00) >> 8);
            *reg16low_ptr = (uint8_t)(data16 & 0x00FF);
            return;
        }
    }

    /*
        This takes care of two case sets:
            - ld (rr), A; ld (rr+/-), A
            - ld A, (rr); ld A, (rr+/-)

        Instead of thinking up something mathematical, I just took the L and switch cased it
        Apologize for this. It was 3 in the morning and I did not have the energy. You can, of
        course, use the 8bit register resolution logic we used previously to resolve this as
        well. It really is the same.
        There is one difference tho. Notice how for (HL) we have (HL+)/(HL-). This simply
        means that after getting the data from address stored in HL and loading that in A,
        we have to increment/decrement HL. No getting overwhelmed from this. Let's just code
        the case separately, now that we have placed switch cases. So after we put the 16bit
        data from HL into a temporary variable, simply increment/decrement HL. No worry.

        After resolving the address to use for memory fetch, based on the opcode we decide
        whether we load into the address or load from the address into accumulator. I'll
        mark the HL+/HL- case separately.
    */
    if ((((opcode & 0x0F) == 0x02) | ((opcode & 0x0F) == 0x0A)) && ((opcode & 0xF0) <= 0x30))
    {
        switch (opcode)
        {
            case 0x0A:
            case 0x02:
                data16 = (uint16_t)(gb_cpu->gb_reg.BC.B << 8) | (uint16_t)(gb_cpu->gb_reg.BC.C);
                break;
            
            case 0x1A:
            case 0x12:
                data16 = (uint16_t)(gb_cpu->gb_reg.DE.D << 8) | (uint16_t)(gb_cpu->gb_reg.DE.E);
                break;

            // (HL+)
            case 0x2A:
            case 0x22:
                data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
                gb_cpu->gb_reg.HL.H = (uint16_t)(((data16 + 1) & 0xFF00) >> 8);
                gb_cpu->gb_reg.HL.L = (uint16_t)((data16 + 1) & 0x00FF);
                break;

            // (HL-)
            case 0x3A:
            case 0x32:
                data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
                gb_cpu->gb_reg.HL.H = (uint16_t)(((data16 - 1) & 0xFF00) >> 8);
                gb_cpu->gb_reg.HL.L = (uint16_t)((data16 - 1) & 0x00FF);
                break;
        }

        ((opcode & 0x0F) == 0x02) ? \
            (mem_write(gb_cpu, data16, gb_cpu->gb_reg.AF.A)) : \
            (gb_cpu->gb_reg.AF.A = mem_read(gb_cpu, data16));
        return;
    }

    /*
        Case ld r, d8.
        Nothing of note here. I'll mark the steps but it's just the same as what we have done uptil
        now.
    */
    if ((((opcode & 0x0F) == 0x06) | ((opcode & 0x0F) == 0x0E)) && ((opcode & 0xF0) <= 0x30))
    {
        // All cases fetch 8bit immediate data from memory
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
        
        /*
            Hardcoding two cases:
                - One where we load into memory at address stored in HL
                - One where we load into A
        */
        if (opcode == 0x36)
        {
            data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
            mem_write(gb_cpu, data16, data8);
            return;
        }
        if (opcode == 0x3E)
        {
            gb_cpu->gb_reg.AF.A = data8;
            return;
        }

        /*
            Same logic as before to resolve which 8bit register to select
            And then get a reference to register. Use the reference to load resolved 8bit
            data obtained before into it.
        */
        offset = 2*(((opcode & 0xF0) >> 4) + 1);
        reg8ptr = (uint8_t *)((unsigned long)(&(gb_cpu->gb_reg))+offset+ \
                             (((opcode & 0x0F) == 0x0E) ? 1 : 0));
        
        *reg8ptr = data8;
        return;
    }

    /*
        Here it comes.
        The hail mary.
        This is the GIANT BLOCK of ld instructions right in the middle of the opcode table.
        (16x4 - 1) ld instructions. A giant **** you. But we cracking this. We ain't gonna
        back down.

        We shall first resolve the 0x7x row. This is because it uses (HL) as location to
        load data into. Since I was dumb enough not to fix the order of the register structure
        definitions correctly, we now have to accomodate for that everytime.
    */
    if ((opcode & 0xF0) == 0x70)
    {
        // opcodes on and before 0x07 use (HL)
        if ((opcode & 0x0F) <= 0x07)
        {
            data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
            /*
                This might be a new method of selecting an 8bit register. Just gonna highlight
                how this works real quick.
                7       0   1   2   3   4   5   6
                A   F   B   C   D   E   H   L   x
                Above is how opcodes map to 8bit registers.

                Now if we want to get to A, we need offset to be 0. But from opcode we are
                getting 7. To get to B, we need offset to be 2, but from opcode we are getting
                0. So, taking these two pieces of information, we can formulate a plan.
                To get to B safely, we need to add 2 to the opcode. But, we also need to loopback
                to zero for A, while preserving offset for B. In comes modulo operator. We simply
                modulo by (7+2), so that when opcode is 7, (7+2) % 9 will land us to 0 safely.
            */
            offset = ((opcode & 0x0F) + 2) % 9;
            data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));

            // Write to calculated address with data received from calculated register.
            mem_write(gb_cpu, data16, data8);
            return;
        }
        // Now do the same as above, only accumulator is used as the register to load data into.
        else
        {
            if (opcode == 0x7E)
            {
                data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
                data8 = mem_read(gb_cpu, data16);
            }
            else
            {
                offset = ((opcode & 0x0F) + 2) % 9;
                data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
            }
            gb_cpu->gb_reg.AF.A = data8;
            return;
        }
    }

    /*
        Now that the outliers are out of the way, code the modular chunk of the ld opcode
        chunk. There is a simple pattern to them along rows/columns. Two patterns acutally.

        B, B    B, C    B, D    B, E    B, H    B, L    B, (HL)    B, A    C, B ....
        D, B                                                               E, B ....
        H, B                                                               L, B ....
        (HL), B                                                            A, B ....

        We can do the pattern easily. B, C, D, E, H, L, (HL) A along a row until 0x07 and
        repeat the pattern following that. Along the column we do, B D H (HL) before 0x07
        and C E L A after that.
        For the row pattern we simply re-use the 8bit register resolution we used above.
        For the column pattern we re-use the 8bit register resolution we used before, and
        add 1 to the offset for opcodes after 0x07 to get the C E L A pattern. After all
        this autism we shall be able to take care of most cases except two small things:
            - We will have to hardcode 0x06 and 0x0E rows as they use (HL). Same old issue
            - Talking about the column pattern, to repeat B C D E H L (HL) A after 0x07 we
            will have to account for the fact that 0x08 has to loopback to 0x00. And then
            we can re-use our logic for B C D E H L (HL) A. Again, loopback word indicates
            the use of modulo.
    */

    // Fetch register to load data into
    offset = 2*(((opcode & 0xF0) >> 4) + 1);
    reg8ptr = (uint8_t *)((unsigned long)(&(gb_cpu->gb_reg))+offset+ \
                             (((opcode & 0x0F) <= 0x07) ? 0 : 1));

    // Fetch data that has to be loaded into register
    if ((opcode & 0x0F) == 0x06 || (opcode & 0x0F) == 0x0E)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((opcode & 0x0F) % 8) + 2) % 9;
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
    }
    
    *reg8ptr = data8;
    return;
}

/* Increment/Decrement instructions */
void inc(gb_cpu_t *gb_cpu)
{
    /*
        Increment: increment value in register/memory
        So. With increment we have three rows in the 16x16 isa.
        One row manipulates BC, DE, HL, SP
        Another row manipulates B, D, H, (HL)
        And the last manipulates C, E, L, A

        My register definitions hints at the fact that I may not need 
        to create a case-bsaed implementation for every single opcode.
        What I can simply do is use the offsets, and the fact that the
        patterning is done with registers following each other one below
        the other. For instance, Pattern #2 is essentially the higher byte
        of BC, DE, HL. Pattern #3 is similarly the lower byte. I can use
        that to my advantage and compute the offset of the register I need
        to manipulate using the opcode given. The ones not following 
        pattern, i.e, the last part of each pattern, I have hardcoded. 
        I couldn't find any pattern to why specifically they were chosen. 
        Same comment goes for decrement operation, so not putting any 
        comment there.
    */
    uint8_t idx, reg_set, data8, opcode;
    uint16_t data16;
    uint8_t *data;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    /*
        This is logic to calculate the offset to the registers
        2 * helps hop from one 16 bit register to another. It
        also helps skip the AF register. The opcode layout is
        such that in a specific column(s), inc/dec instructions
        are clumped together. As you go down the column, you move
        down the register set (See patterns in topmost comment).
        A simple rule of thumb for inc/dec instruction: lower nibble
        decides the pattern we will be encountering, and the upper 
        nibble decides which register to choose within the pattern.

        Hope that makes sense.
    */
    idx = 2*(((opcode & 0xF0) >> 4)+1);

    // Hardcoding cases. This is hardcoding for pattern #1.
    if ((opcode & 0x0F) == 0x03)
    {
        /* 
            Hardcoding the case where 4th entry in pattern does not
            follow the register offsets.
        */
        if (idx == 0x08)
        {
            gb_cpu->gb_reg.SP++;
            return;
        }

        /*
            Using a higher byte pointer and a lower byte pointer to
            get the entire 16-bit pseudo-register. Let's take BC as a 
            16-bit register, for instance. The idea is that datahigh
            will point to 'B' and datalow will point to 'C'. We then
            amalgamate them to get 16bit data as if it was a 16bit 
            register. After increment, we break the 16bit data and
            store it back into two 8bit registers as they originally
            were
        */
        uint8_t *datahigh;
        uint8_t *datalow;
        datahigh = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+idx);
        datalow = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+idx+1);
        data16 = (uint16_t)(*datahigh << 8) | (uint16_t)(*datalow);
        ++data16;

        *datahigh = (uint8_t)((data16 & 0xFF00) >> 8);
        *datalow = (uint8_t)(data16 & 0x00FF);
        return;
    }

    /*
        Now that pattern #1 is coded, this decides whether we wish to
        work with pattern #2 or #3. Essentially, the two patterns are
        deciding whether we work with higher or lower byte of the 16bit
        registers.
    */
    reg_set = ((opcode & 0x0F) == 0x04)?0:1;

    // Again, hardcoding 4th entry for pattern #2 and #3
    if (idx == 0x08)
    {
        if (!reg_set)
        {
            data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                        (uint16_t)(gb_cpu->gb_reg.HL.L);
            data8 = mem_read(gb_cpu, data16) + 1;
            mem_write(gb_cpu, data16, data8);
            data = &data8;
            goto flag_update;
        }
        else
        {
            data = (uint8_t *)(&(gb_cpu->gb_reg.AF.A));
            (*data)++;
            goto flag_update;
        }
    }

    /*
        Now that we know the offset, and which register (higher/lower)
        we wish to access, we can add these to the base address to land
        on the precise register that we want.
    */
    data = \
    (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+idx+reg_set);
    (*data)++;

    // Inc/Dec follows the "Z 0 H -" rule for flags
flag_update:
    (*data == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    ((*data & 0x0F) == 0x00) ? set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
    return;
}

void dec(gb_cpu_t *gb_cpu)
{
    /*
        Decrement operation: decrement value of register/memory
        See increment comment for info on implementation.
        The code is same as increment too. All comments there
        are valid here too.
    */
    uint8_t idx, reg_set, data8, opcode;
    uint16_t data16;
    uint8_t *data;

    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    idx = 2*(((opcode & 0xF0) >> 4)+1);

    if ((opcode & 0x0F) == 0x0B)
    {
        if (idx == 0x08)
        {
            gb_cpu->gb_reg.SP--;
            return;
        }
        uint8_t *datahigh;
        uint8_t *datalow;
        datahigh = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+idx);
        datalow = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+idx+1);
        data16 = (uint16_t)(*datahigh << 8) | (uint16_t)(*datalow);
        --data16;

        *datahigh = (uint8_t)((data16 & 0xFF00) >> 8);
        *datalow = (uint8_t)(data16 & 0x00FF);
        return;
    }

    reg_set = ((opcode & 0x0F) == 0x05)?0:1;

    if (idx == 0x08)
    {
        if (!reg_set)
        {
            data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                        (uint16_t)(gb_cpu->gb_reg.HL.L);
            data8 = mem_read(gb_cpu, data16) + 1;
            mem_write(gb_cpu, data16, data8);
            data = &data8;
            goto flag_update;
        }
        else
        {
            data = (uint8_t *)((&gb_cpu->gb_reg.AF.A));
            (*data)--;
            goto flag_update;
        }
    }

    data = \
    (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+idx+reg_set);
    (*data)--;

flag_update:
    (*data == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    set_addsub(gb_cpu);
    ((*data & 0x0F) == 0x0F) ? clear_halfcarry(gb_cpu) : set_halfcarry(gb_cpu);
    return;
}

/* Arithmetic operations */
void add(gb_cpu_t *gb_cpu)
{
    /*
        Add: Add two registers/register with memory
        There is A LOT to talk about here. I'll take this up step by step
        There are three different types of add instruction and they are
        clustered differently around the opcode table.
            - ADD A, r: A = A + (insert 8 bit register here). Instead of
                8 bit register there is one opcode using (HL) and one
                opcode using 8 bit immediate data.
            - ADD HL, rr: HL = HL + (insert 16bit register here). Instead
                of 16bit register one opcode uses SP. (It is a 16bit register
                ik but it is not a general purpose register.)
            - ADD SP, d8: SP = SP + d8. d8 is the next byte in memory
                after the opcode.
        Let's talk about each of them. There is something in each of them.
        Mainly how the flags work tbh.
        The option #1 has flags Z 0 H C. Now the half carry for this is
        generated at bit 3 and carry is generated at bit 7. Nothing special
        really.
        The option #2 has flags - 0 H C. Half carry for this is generated
        at bit "12" (not 3) and carry is generated at bit 15.
        The option #3 has flags 0 0 H C. This was a bit of a curveball. Even
        though SP is a 16bit register, we still generate halfcarry for this
        instruction at bit "3" and carry is generated at bit "7". Don't even
        ask me why. Just let it slide. To get info on where I got this from,
        take a look at this stackoverflow:

        https://stackoverflow.com/questions/57958631/
        game-boy-half-carry-flag-and-16-bit-instructions-especially-opcode-0xe8

        There is also the question of how to calculate the halfcarry/carry.
        So for the half carry we have the advantage of the fact that the bits
        that directly result in its generation and the halfcarry itself are
        within the data size that we are using. For example, for 8bit addition,
        the half carry is generated at bit 3. Within the 8 bit size. The
        method to calculate halfcarry that I have used is as follows:
        bit 3:
            (((operand1 & 0x0F)+(operand2 & 0x0F)) & 0x10) ? set : reset;
        bit 12:
            (((operand1 & 0x0FFF)+(operand2 & 0x0FFF)) & 0x1000) ? set : reset;
        I might have used an excess of variables than required on this one,
        but i had to for a) My sanity b) Clarity on what I was doing.
    */
    uint8_t opcode, data8, offset, orgAval;
    uint16_t data16, primary16, result16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcoding opcode ADD SP, r8
    if (opcode == 0xE8)
    {
        // Fetching operan values
        primary16 = gb_cpu->gb_reg.SP;
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

        // Performing add and storing result in SP
        result16 = primary16 + (uint16_t)data8;
        gb_cpu->gb_reg.SP = result16;

        /*
            Update flags. ADD SP, r8 follows 0 0 H C rule.
            Remember, here H is triggered from bit 3 and C is triggered from
            bit 7
        */
        clear_zero(gb_cpu);
        clear_addsub(gb_cpu);
        // A+B > 0xFF
        (((uint8_t)(primary16 & 0x0F)+(data8 & 0x0F)) & 0x10) ? \
            set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
        (((primary16 & 0x00FF)+data8) & 0x0100) ? \
            set_carry(gb_cpu) : clear_carry(gb_cpu);
        return;
    }

    // Case ADD A, r. 8 bit addition.
    if (opcode == 0xC6 || (opcode & 0xF0) == 0x80)
    {
        // Hardcoding opcode case where 8 bit operand is fetched from memory
        if (opcode == 0xC6)
            data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
        /* 
            Hardcoding opcode case where 8 bit operand is fetched from memory indexed
            by address stored in HL
        */
        else if (opcode == 0x86)
        {
            data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
            data8 = mem_read(gb_cpu, data16);
        }
        // Hardcoding case where other operand is A itself
        else if (opcode == 0xC6)
            data8 = gb_cpu->gb_reg.AF.A;
        // With all the special cases asside, we may now work with the generic case
        else
        {
            /*
                We have worked out how this offset is calculated multiple times now. Skipping
                explanation. To keep it simple, we skip the AF register and land on B. From there,
                value of opcode's MSB controls which register we land on.
            */
            offset = 2+((opcode & 0xF0) >> 4);
            data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
        }

        // Saving value of A before addition for flag calculation purposes
        orgAval = gb_cpu->gb_reg.AF.A;
        // Performing 8bit add and saving data in accumulator
        gb_cpu->gb_reg.AF.A += data8;

        /*
            ADD A, r follows Z 0 H C rule. Bear in mind here the carry is generated at bit 7
            and halfcarry is generated at bit 3.
        */
        (gb_cpu->gb_reg.AF.A == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
        clear_addsub(gb_cpu);
        (((orgAval & 0x0F)+(data8 & 0x0F)) & 0x10) ?
            set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
        (orgAval > (0xFF - data8)) ?
            set_carry(gb_cpu) : clear_carry(gb_cpu);
        return;
    }

    /*
        Now we take care of the ADD HL, rr case. This is a 16 bit addition instruction.
        Fetching operand 1, which is data present in HL
    */
    primary16 =
        (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);

    // Hardcoding case where opcode uses SP as operand 2
    if (opcode == 0x39)
        data16 = gb_cpu->gb_reg.SP;
    /*
        Coding generic case. Again same offset logic is incorporated. Skip AF, we land
        on B. From there, to access each 16 bit register, we multiply obtained offset
        by 2.
    */
    else
    {
        offset = 2*(((opcode & 0xF0) >> 4)+1);
        data16 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
    }

    // Perform the 16bit add and store result in HL
    result16 = primary16+data16;
    gb_cpu->gb_reg.HL.H = (uint8_t)((result16 & 0xFF00) >> 8);
    gb_cpu->gb_reg.HL.L = (uint8_t)(result16 & 0x00FF);

    /*
        Update flags. ADD HL, rr follows - 0 H C rule bit with a twist.
        PLEASE take note, halfcarry is generated at bit 11 and carry is
        generated at bit 15
    */
    clear_addsub(gb_cpu);
    (((primary16 & 0x0FFF)+(data16 & 0x0FFF)) & 0x1000) ? \
        set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
    (primary16 > (0xFFFF - data16)) ?
        set_carry(gb_cpu) : clear_carry(gb_cpu);
    return;
}

void adc(gb_cpu_t *gb_cpu)
{
    /*
        ADC: Add 8bit register/memory value along with carry to accumulator
        This is fairly simple: it is same as the add instruction logic with
        lesser branches to implement and the carry flag added to the result.

        There is a little cheezing I did to calculate carryout after the adc
        is complete. I am not proud of it, but I don't think I can come up
        with something better by myself. So essentially instead of saving
        the result in an 8bit temporary variable, I have stored the result
        in a 16bit register: Idea being that I can simply check for
        (register & 0x100) to check for a carryout after adc. As it so happens
        the methods I had been using uptil now are not really gonna work for
        adc. For instance, let A = 0xFF, n = 0xFF, cy = 1.
            adc is A + n + cy.
            If there is a carry, then A + n + cy > 0xFF
                                    => A > (0xFF - cy) - n
                                    => 0xFF > (0xFF - 0x01) - 0xFF
                                    => 0xFF > 0xFE - 0xFF
                                    => 0xFF > 0xFF
            which is a false statement.
        But with 16bit register, things become something like so:
            adc is A + n + cy
            result16 = A + n + cy
            result16 = 0xFF + 0xFF + 0x01
            result16 = 0x1FF
            carryout = (result & 0x100) = (0x1FF & 0x100) = 0x100
        which makes it easier. But is kinda cheating if you ask me.
    */
    uint8_t opcode, data8, offset, carryval;
    uint16_t result, data16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcoding case where we fetch 8bit operand from memory
    if (opcode == 0xCE)
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    /*
        Hardcoding case where we fetch 8bit operand from memory located
        at address stored in 16bit register HL.
    */
    else if (opcode == 0x8E)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    // Case where we fetch 8bit operand from 8bit registers
    else
    {
        // 8bit register resolution. We know the drill by now
        offset = (((opcode & 0x0F) - 8) + 2) % 9;
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
    }
    // Get the current carry value
    carryval = (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY)) >> FLAG_CARRY;
    // Save the result in a 16bit store: we need it so as explained before
    result = (uint16_t)(gb_cpu->gb_reg.AF.A)+(uint16_t)(data8)+ (uint16_t)carryval;

    /*
        Flag rule is Z 0 H C
        Already explained the carry rule, so not going over that again
    */
    (result == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    (((gb_cpu->gb_reg.AF.A & 0x0F)+(data8 & 0x0F)+carryval) & 0x10) ? \
        set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
    (result & 0x100) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    // Finally, now that all is done, we may store the final value in A
    gb_cpu->gb_reg.AF.A = (uint8_t)(result & 0xFF);

    return;
}

void sub(gb_cpu_t *gb_cpu)
{
    uint8_t opcode, data8, offset, result;
    uint16_t data16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcoding case where we fetch 8bit operand from memory
    if (opcode == 0xD6)
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    /*
        Hardcoding case where we fetch 8bit operand from memory located
        at address stored in 16bit register HL.
    */
    else if (opcode == 0x96)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    // Case where we fetch 8bit operand from 8bit registers
    else
    {
        // 8bit register resolution. We know the drill by now
        offset = ((opcode & 0x0F) + 2) % 9;
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
    }
    // Save the result in an 8bit store
    result = gb_cpu->gb_reg.AF.A-data8;

    /*
        Flag rule is Z 1 H C
        Already explained the carry rule, so not going over that again
    */
    (result == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    set_addsub(gb_cpu);
    (((gb_cpu->gb_reg.AF.A & 0x0F)-(data8 & 0x0F)) & 0x10) ? \
        set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
    (gb_cpu->gb_reg.AF.A < data8) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    // Finally, now that all is done, we may store the final value in A
    gb_cpu->gb_reg.AF.A = result;

    return;
}

void sbc(gb_cpu_t *gb_cpu)
{
    /*
        SBC: Subtract 8bit register/memory and carry from accumulator
        This is same as adc, but A - n - cy is what we do. There is
        one change to the way carry is calculated. With borrow we
        can't cheeze it. So I cheezed it in another way. Which is
        to simply check for A < (n - cy). I am ashamed already,
        no need to look at me.
    */
    uint8_t opcode, data8, offset, carryval, result;
    uint16_t data16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcoding case where we fetch 8bit operand from memory
    if (opcode == 0xDE)
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    /*
        Hardcoding case where we fetch 8bit operand from memory located
        at address stored in 16bit register HL.
    */
    else if (opcode == 0x9E)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    // Case where we fetch 8bit operand from 8bit registers
    else
    {
        // 8bit register resolution. We know the drill by now
        offset = (((opcode & 0x0F) - 8) + 2) % 9;
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
    }
    // Get the current carry value
    carryval = (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY)) >> FLAG_CARRY;
    // Save the result in an 8bit store
    result = (uint8_t)(gb_cpu->gb_reg.AF.A)-(uint8_t)(data8)-(uint8_t)carryval;

    /*
        Flag rule is Z 1 H C
        Already explained the carry rule, so not going over that again
    */
    (result == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    set_addsub(gb_cpu);
    (((gb_cpu->gb_reg.AF.A & 0x0F)-(data8 & 0x0F)-carryval) & 0x10) ? \
        set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
    (gb_cpu->gb_reg.AF.A < (data8 - carryval)) ? \
        set_carry(gb_cpu) : clear_carry(gb_cpu);

    // Finally, now that all is done, we may store the final value in A
    gb_cpu->gb_reg.AF.A = result;

    return;
}

void daa(gb_cpu_t *gb_cpu)
{
    /*
        DAA: Decimal adjust accumulator to BCD format
        Sigh.
        I have made a severe, lapse in my judgement.
        I kept this instruction for last.
        And that ultimately led to my downfall.

        Man. I have faced a lot of failure in my life.
        And everytime I fell, I rose back up. But this one
        This one, simple, goddamn instruction.
        It broke me.
        It made me realize just how insignificant I really
        was.
        It made me understand just how bad my math was.
        blah blah blah

        Let's get into the meat of this nonsense instruction.
        DAA assumes that before its execution there was an
        add/subtract operation done with packed BCD operands.

        Imagine that scenario. Let's take two packed BCD numbers.
        op1 = 0x90 and op2 = 0x90. If we add them using an adder,
        we get 0x120. Or rather, answer = 0x20 and carry = 1.
        But with BCD addition what we should have gotten was 0x180.
        Or rather, answer = 0x80 and carry = 1.

        This is where DAA comes in. It "adjusts" the accumulator from
        a hexadecimal addition output to packed BCD. Let's go over the
        basics. Shall we?

        Any hexadecimal number has range 0-F. Any BCD number has range
        0-9. So if we land on Ah, we should go to 0 when we go to BCD.
        Ah + 6h is 10h. So we corrected for it and got the carry we
        wanted as well. So the base math is clear. Simply add 6h.

        Now let's talk about the final boss of this instruction.
        Carries.
        *queue scary opera music*
        Let's try to understand. Let's say we add 0x90 to 0x90. The answer
        is 0x20 with 1 carry. So if I mask the higher nibble I get 2h. I
        look at that and go, "Hey, this does not need any decimal adjust"
        and just move on. But wait. We wanted 0x80 and carry 1. So we
        did it wrong. And the answer to this was to correct for the carry.
        So, if we have a carry OR we have a number over 9 (A-F), we have to
        correct to BCD. AND we need to set the carry. You can come up with
        all the scenarios and realize that a carry will be generated in the
        following case:
            - Previous instruction was an add instruction
            - There is a carry from previous instruction OR the higher nibble
            exceeds 9.
        
        Subtraction is a bit different. With subtraction, we aren't really going
        to overflow unless there is a borrow from another bit. Think about it.
        You are subtracting 0-9 number from a 0-9 number. You won't really
        overflow unless there is a borrow. So we check and correct for the flags.

        I didn't get this instruction as much. TBH if you ask me a question tom
        I'll just dodge you and pretend I didn't hear. This is the best explanation
        I can do.
    */

    // Fetch opcode from memory
    gb_cpu->gb_reg.PC++;

    // Case when last instruction was an addition
    if (!(gb_cpu->gb_reg.AF.F & (1 << FLAG_BCD_ADDSUB)))
    {
        /*
            In case of addition, we need to worry about both carry and overflow
            Correct for higher nibble first cause then there is no need to worry
            carry when correcting for the lower nibble.
            Man TBH I did not understand why. If you want to know where I got this
            from here's the reference:
            https://forums.nesdev.org/viewtopic.php?t=15944#:~:text=The%20DAA%20
            instruction%20adjusts%20the,%2C%20lower%20nybble%2C%20or%20both

            I just can't wrap my head around this for some reason.
        */
        if (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY) || \
            (gb_cpu->gb_reg.AF.A > 0x99))
        {
            gb_cpu->gb_reg.AF.A += 0x60;
            set_carry(gb_cpu);
        }
        // This corrects for lower nibble
        if (gb_cpu->gb_reg.AF.F & (1 << FLAG_BCD_HALF_CARRY) || \
            ((gb_cpu->gb_reg.AF.A & 0x0F) > 0x09))
        {
            gb_cpu->gb_reg.AF.A += 0x06;
        }
    }
    // In case of subtraction, no need to correct for overflow
    else
    {
        // Correct upper nibble
        if (gb_cpu->gb_reg.AF.A & (1 << FLAG_CARRY))
        {
            gb_cpu->gb_reg.AF.A -= 0x60;
        }
        // Correct lower nibble
        if (gb_cpu->gb_reg.AF.A & (1 << FLAG_BCD_HALF_CARRY))
        {
            gb_cpu->gb_reg.AF.A -= 0x06;
        }
    }

    /*
        Flags are Z - 0 -
        Key thing to note here is:
        DAA is the only instruction that cares about the half-carry flag
        and heavily uses the addsub flag.
        So DAA generating flag information is not really relevant to the
        next instruction. I guess that's why most of the flag information
        generated by DAA is discarded.
    */
    (gb_cpu->gb_reg.AF.A == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_halfcarry(gb_cpu);

    return;
}

void _and(gb_cpu_t *gb_cpu)
{
    /*
        _and: Perform bitwise and with the accumulator
        Nothing of note. Simple enough. Bitwise AND with the register A.
    */
    uint8_t opcode, data8, offset;
    uint16_t data16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcode case where 8bit data is fetched from memory
    if (opcode == 0xE6)
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    // Hardcode case where 8bit data is fetched from memory located at address stored in HL
    else if (opcode == 0xA6)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    // Cases where general purpose registers are used to get 8bit data
    else
    {
        // Checkout ld instruction's comments to see how this resolution works
        offset = ((opcode & 0x0F) + 2) % 9;
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg) + offset));
    }

    // Perform bitwise AND with accumulator
    gb_cpu->gb_reg.AF.A &= data8;

    // Flags for this instruction are Z 0 1 0
    (gb_cpu->gb_reg.AF.A == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    set_halfcarry(gb_cpu);
    clear_carry(gb_cpu);
    return;
}

void _or(gb_cpu_t *gb_cpu)
{
    /*
        _or: Perform bitwise or with the accumulator
        Okay so there is one thing to talk about here.
        There is OR A instruction at opcode 0xB7. Which basically
        means A = A | A. Now why would you need that instruction,
        I don't know. But it is there. So yeah.
    */
    uint8_t opcode, data8;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    /*
        The main thing to understand about the _or instruction is
        that all of the opcodes have accumulator as one of the 
        operands. Essentially, we bitwise or the accumulator with
        register/memory value and then store the result in the 
        accumulator again.
    */

    // Hardcoding case where opcode uses 8bit immediate value
    if (opcode == 0xF6)
    {
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    }
    /*
        Hardcoding case where we get the 8bit data from memory
        Address is present in HL register
    */
    else if (opcode == 0xB6)
    {
        uint16_t addr;

        addr = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, addr);
    }
    /*
        We use the same philosophy as we always have. We use 
        the fact that we can use the opcode as an offset to
        access the register that the opcode wants us to take
        the 8bit data from. Only difference is that this time,
        the OR opcodes are along a row. So the lower byte will
        act as the offset
    */
    else
    {
        uint8_t idx;

        idx = ((opcode & 0x0F) == 0x07) ? 0 : (2 + (opcode & 0x0F));
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+idx));
    }

    // Actual OR operation performed here
    gb_cpu->gb_reg.AF.A |= data8;

    /*
        Update flags
        OR has a Z 0 0 0 philosophy
    */
    (gb_cpu->gb_reg.AF.A == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    clear_carry(gb_cpu);
    return;
}

void _xor(gb_cpu_t *gb_cpu)
{
    /*
        _xor: Perform bitwise xor over accumulator
        Nothing of note. Just a bitwise operation.
    */
    uint8_t opcode, data8, offset;
    uint16_t data16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcode case where 8bit data is fetched from memory
    if (opcode == 0xEE)
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    // Hardcode case where 8bit data is fetched from memory located at address stored in HL
    else if (opcode == 0xAE)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    // Cases where general purpose registers are used to get 8bit data
    else
    {
        // Checkout ld instruction's comments to see how this resolution works
        offset = (((opcode & 0x0F) - 8) + 2) % 9;
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg) + offset));
    }

    // Perform bitwise AND with accumulator
    gb_cpu->gb_reg.AF.A ^= data8;

    // Flags for this instruction are Z 0 0 0
    (gb_cpu->gb_reg.AF.A == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    clear_carry(gb_cpu);
    return;
}

void cp(gb_cpu_t *gb_cpu)
{
    /*
        cp: compare accumulator with data
        Now. Few things.
        CP is "compare", but on a hardware level it really is a subtraction
        between accumulator and operand (A - n), and then using the result
        in order to gauge whether A < n, A = n or A > n. This is also why
        half-carry and carry is set when this operation is invoked.

        Our approach will be to take advantage of the fact that our hardware
        will already do this for us and hence use C's abstraction of <, ==, >
        operators to implement this instruction.
    */
    uint8_t opcode, data8, offset;
    uint16_t data16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcode case where we fetch the 8bit operand from memory
    if (opcode == 0xFE)
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    /*
        Hardcode case where we fetch the 8bit operand from memory located at
        address stored in 16bit register HL
    */
    else if (opcode == 0xBE)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    // Fetch operand by using resolution for selecting 8bit register
    else
    {
        offset = (((opcode & 0x0F) - 8) + 2) & 9;
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
    }

    /*
        Flag rules are Z 1 H C
        Z : Set if A == n
        N : Always set to 1
        H : Set if there is borrow from bit4
        C : Set if A < n
    */
    (gb_cpu->gb_reg.AF.A == data8) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    set_addsub(gb_cpu);
    ((gb_cpu->gb_reg.AF.A & 0x0F) < (data8 & 0x0F)) ? \
        set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
    (gb_cpu->gb_reg.AF.A < data8) ? set_carry(gb_cpu) : clear_carry(gb_cpu);
    return;
}

void cpl(gb_cpu_t *gb_cpu)
{
    /*
        cpl: Complement accumulator.
        This instruction flips the bits of the accumulator.
        Let's look at the truth table of what that would look like
        for one bit

        input   |   output
        ------------------
           0    |    1
           1    |    0

        Looks awful lot similar to the XOR gate, does it not?

           A    |   B   |   output
        --------------------------
           0    |   0   |     0
        !!!!!!!!!!!!!!!!!!!!!!!!!!
        !  0    |   1   |     1  !
        !!!!!!!!!!!!!!!!!!!!!!!!!!
           1    |   0   |     1
        !!!!!!!!!!!!!!!!!!!!!!!!!!
        !  1    |   1   |     0  !
        !!!!!!!!!!!!!!!!!!!!!!!!!!

        So, if we simply take the accumulator, and XOR it with 0xFF
        we are good, right? Yup! It is as simple as that.
    */
    // Accomodate for opcode fetch
    gb_cpu->gb_reg.PC++;
    // A = A XOR 0xFF to flip the bits
    gb_cpu->gb_reg.AF.A ^= 0xFF;

    // Flag rules are - 1 1 -
    set_addsub(gb_cpu);
    set_halfcarry(gb_cpu);

    return;
}

/* Rotate Instructions */
void rlca(gb_cpu_t *gb_cpu)
{
    /*
        Rotate left affect carry.
        This a rotate instruction that acts on the accumulator. The old bit7 is copied to
        both carry and the new bit0 of the accumulator. To understand this here is a chart
                        -----
           |----------->|   |   Carry Flag
           |            -----
           |--------------------------------|
        ---------------------------------   |
        | <-| <-| <-| <-| <-| <-| <-| <-|<--|
        ---------------------------------
          7   6   5   4   3   2   1   0
    */
    uint8_t bit7;

    gb_cpu->gb_reg.PC++;
    // Save the old 7th bit temporarily
    bit7 = (gb_cpu->gb_reg.AF.A & 0x80) >> 7;
    /*
        We perform a left shift by one, and insert the old 7th bit at the end of the result
        This makes the operation into a "rotate" from a "shift".
    */
    gb_cpu->gb_reg.AF.A = (gb_cpu->gb_reg.AF.A << 1);
    gb_cpu->gb_reg.AF.A |= bit7;

    // If the old 7th bit is 1, set the carry. Else reset
    bit7 ? set_carry(gb_cpu) : clear_carry(gb_cpu);
    clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    return;
}

void rrca(gb_cpu_t *gb_cpu)
{
    /*
        Rotate right affect carry.
        Same op as rlca, just rotate to the right.
        Look at rlca for more info.
    */
    uint8_t bit0;

    gb_cpu->gb_reg.PC++;
    // Save the old 0th bit temporarily
    bit0 = gb_cpu->gb_reg.AF.A & 0x01;
    /*
        We perform a left shift by one, and insert the old 0th bit at the end of the result
        This makes the operation into a "rotate" from a "shift".
    */
    gb_cpu->gb_reg.AF.A = (gb_cpu->gb_reg.AF.A >> 1);
    gb_cpu->gb_reg.AF.A |= (bit0 << 7);

    // If the old 7th bit is 1, set the carry. Else reset
    bit0 ? set_carry(gb_cpu) : clear_carry(gb_cpu);
    clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    return;
}

void rla(gb_cpu_t *gb_cpu)
{
    /*
        Rotate A left through carry.
        This is a rotate operation with the carry also involved. Not just for bit copy, but
        as a bit considered in the rotation as well. If that confuses you, here is a diagram

          |-----------------------------------------|
          |                                         |
        -----   ---------------------------------   |
        | C | <-| <-| <-| <-| <-| <-| <-| <-| <-| <-|
        -----   ---------------------------------
                    7   6   5   4   3   2   1   0

        It is simple really. Shift everything to the left, and bit7 is shifted to the carry.
        What was present in the carry is shifted to bit0.
    */
    uint8_t bit7;

    gb_cpu->gb_reg.PC++;
    // Save the 0th bit temporarily
    bit7 = (gb_cpu->gb_reg.AF.A & 0x80) >> 7;
    
    /*
        Shift everything to the left, and place the old value of carry into the 0th bit.
        That way carry bit becomes part of the rotate.
    */
    gb_cpu->gb_reg.AF.A = (gb_cpu->gb_reg.AF.A << 1);
    gb_cpu->gb_reg.AF.A |= ((gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY)) >> FLAG_CARRY);

    // Shift bit7 to the carry bit.
    bit7 ? set_carry(gb_cpu) : clear_carry(gb_cpu);
    clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    return;
}

void rra(gb_cpu_t *gb_cpu)
{
    /*
        Rotate right through carry.
        Same as rla, just rotate to the right.
        See rla for more info.
    */
    uint8_t bit0;

    gb_cpu->gb_reg.PC++;
    // Save the 0th bit temporarily
    bit0 = gb_cpu->gb_reg.AF.A & 0x01;
    
    /*
        Shift everything to the left, and place the old value of carry into the 0th bit.
        That way carry bit becomes part of the rotate.
    */
    gb_cpu->gb_reg.AF.A = (gb_cpu->gb_reg.AF.A >> 1);
    gb_cpu->gb_reg.AF.A |= ((gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY)) << (7 - FLAG_CARRY));

    // Shift bit7 to the carry bit.
    bit0 ? set_carry(gb_cpu) : clear_carry(gb_cpu);
    clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    return;
}

/* Stack Instructions */
void push(gb_cpu_t *gb_cpu)
{
    /*
        Push: Pushes 16bit register onto stack
        Sigh
        Again. AGAIN
        We need to talk.
        So.
        Here's how push works.
        First we DECREMENT 1 from SP. Then we load most significant byte
        onto the memory location where SP is pointing now. Next we 
        decrement 1 from SP again. Then we load least significant byte
        onto the memory location SP is pointing to now. So. If SP is empty.
        We leave one memory location blank? Picture this (if stack empty):

            |-----| <--- original SP points here (no data?)
            |-----| <--- load MSB here
            |-----| <--- new SP points here (load LSB here)
            |-----|
            |-----|
        
        See the issue? There is a memory location that is left blank right?
    */
    uint8_t opcode, offset;

    // Get opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    /*
        Calculate the offset to get 16 bit register required by opcode. 
        Logic is simple: 2* helps jump to next 16bit register, skipping the 
        least significant 8bit register of the last 16bit register. All push
        opcodes are in one single column with the MSB of the opcode changing.
        So opcode & 0xF0 fetches the change. We subtract from 0x0B as after
        the shift operation, we will get 0x0C,0x0D,0x0E,0x0F (refer opcode
        table). So to get it into 1,2,3,4 range, we do the subtract.
        Finally, we modulo 8 as 0xF5 opcode is PUSH AF, which is the first
        16bit register in the register table. So to make the offset point to
        AF, the computation of the offset for 0xF5 opcode should loopback to
        0, while keeping the rest of the offsets as 2,4,6. Hence the modulo 8.
    */
    offset = (2*(((opcode & 0xF0) >> 4) - 0x0B))%8;

    /*
        Stack push operation:
            - Step1: Decrement SP
            - Step2: Store MSB at SP
            - Step3: Decrement SP
            - Step4: Store LSB at SP
    */
    gb_cpu->gb_mem[--gb_cpu->gb_reg.SP] = \
        *((uint8_t *)((unsigned long)(&(gb_cpu->gb_reg))+offset));
    gb_cpu->gb_mem[--gb_cpu->gb_reg.SP] = \
        *((uint8_t *)((unsigned long)(&(gb_cpu->gb_reg))+offset+1));
    return;
}

void pop(gb_cpu_t *gb_cpu)
{
    /*
        Pop: pops 16bit data onto given register from stack
        Pop operation is exactly the opposite of Push operation.
        You probably won't be able to count the number of times that has
        been said on the subject of stack. So instead of leaving it at
        that, imma explain the process just for the guys that find it
        difficult to reverse PUSHs actions to make POP. 
        In case of POP, we first take the 8bit value from memory at 
        the address where SP is currently pointing to and store it in
        the LSB register. In case of BC that would be C. We then increment
        the stack pointer and similarly access another 8bit value to set
        the MSB. In case of BC that would be B. Again SP is incremented.
        And that's about it.
        There is one thing to talk about actually. The opcode table for 
        GB states that the last opcode POP AF sets the flags? Don't get
        confused by this incase you drunk coding or coding this at 3 am.
        The flags are simply set because you are popping to AF. A'F'. F
        is where the flags are. Simply saying because dumb me got confused
        here at 3am working on POP.
    */
    uint8_t opcode, offset;
    uint8_t *datahigh, *datalow;

    // Get opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    /*
        Calculate the offset to get 16 bit register required by opcode. 
        See comment for same line in PUSH
    */
    offset = (2*(((opcode & 0xF0) >> 4) - 0x0B))%8;

    /*
        datahigh is the pointer to the 8bit register that acts as the
        most significant byte of the corresponding 16bit register.
        datalow is similarly the least significant byte.
        If that is a mouthful simply imagine a 16bit register, say BC.
        datahigh points to B, datalow points to C.
    */
    datahigh = (uint8_t *)((unsigned long)(&(gb_cpu->gb_reg))+offset);
    datalow = (uint8_t *)((unsigned long)(&(gb_cpu->gb_reg))+offset+1);

    /*
        Pop operation. 
            - Step1: Store the 8bit value at current SP in LSB
            - Step2: Increment SP
            - Step3: Store the 8bit value at current SP in MSB
            - Step4: Increment SP

        Exact opposite operation of PUSH. As advertized.
    */
    *(datalow) = gb_cpu->gb_mem[gb_cpu->gb_reg.SP++];
    *(datahigh) = gb_cpu->gb_mem[gb_cpu->gb_reg.SP++];
    return;
}

/* Jump/Call Instructions */
void jr(gb_cpu_t *gb_cpu)
{
    /*
        Jump with offset
        Essentially this instruction adds an offset as the jump to the existing program
        counter after opcode and offset fetch has been executed. It is having both
        unconditional/conditional variants.
        TAKE NOTE: If the condition passes, the offset is added after the PC fetches current
        instruction's opcode and offset. Don't get confused like I did: imagine it this way:
            - Step 1: Fetch instruction at current PC. PC++
            - Step 2: Fetch jump offset. PC++. Now PC is pointing at the instruction next to
            the jump instruction.
            - Step 3: Check the condition based on what opcode is fired. If we unconditional,
            add the offset to the PC and jump succeeded. If conditional, then add offset to
            PC if condition passes.
            Simple as that. No confusion. Here's a diagram to (un)confuse you more.
        original PC     offset      PC here after opcode and offset fetch
            |           |           |-------------------------------------|
            20H         03H         88H         98H         4AH         ABH
                                                |                       |
                                    Land here if cond == false          land here if cond == true
    */
    uint8_t opcode, jmpoffset8, jmpyn = 1;

    // Fetch opcode and offset from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    jmpoffset8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    /*
        There are only 5 opcodes of JR. Thought I'd just hardcode them. Making logic and
        all for 5 instructions felt like overkill
    */
    switch (opcode)
    {
        // Unconditional jump
        case 0x18:
            jmpyn = 1;
            break;

        // Jump if not zero
        case 0x20:
            (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO)) ? (jmpyn = 0) : (jmpyn = 1);
            break;

        // Jump if not carry
        case 0x30:
            (gb_cpu->gb_reg.AF.A & (1 << FLAG_CARRY)) ? (jmpyn = 0) : (jmpyn = 1);
            break;

        // Jump if zero
        case 0x28:
            (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO)) ? (jmpyn = 1) : (jmpyn = 0);
            break;

        // Jump if carry
        case 0x38:
            (gb_cpu->gb_reg.AF.A & (1 << FLAG_CARRY)) ? (jmpyn = 1) : (jmpyn = 0);
            break;
    }

    // In case the condition passed, or we have an unconditonal jump, then update PC
    if (jmpyn)
        gb_cpu->gb_reg.PC += jmpoffset8;

    return;
}

void jp(gb_cpu_t *gb_cpu)
{
    /*
        Jump: Conditional/Unconditional jump to 16bit address
        We may directly manipulate the PC by using this instruction.
        One of the variants is to just jump to a specified 16 bit
        immediate address unconditionally. Another is to jump to a
        16 bit address stored in HL unconditionally. The rest are
        conditional jumps to an immediate 16 bit address given that
        Zero/Carry flag is set or not.
        Few things to talk about here.

        #1 DO NOT GET CONFUSED if you see opcode tables online writing
        the E9 opcode as JP (HL). You may think that you have to go to
        the address that HL is pointing to and get the jump address from
        there. But that is not the case. Directly use the 16 bit value
        placed in HL as the jump address.

        #2 Remember that when you read the opcode, the PC automatically
        increments. Do not increment PC after fetch/decode/exec or before
        it. Just increment it WHEN you read the opcode. It will be infinitely
        convenient to you down the line. Trust me. If you aren't reading
        opcode on some instruction, then manually add the PC increment.
        Understand this: If you keep PC increment after fetch/decode/exec,
        then, jumps will be inaccurate. You will jump, set PC to what addr
        jump wants you to set, say 0x1000, and then your cycle will
        increment PC after jump has been handled, landing you at 0x1001.
        Which isn't correct.
    */
    uint8_t opcode, lsb, msb;
    uint16_t addr16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcoding case where we fetch address from HL
    if (opcode == 0xE9)
    {
        lsb = gb_cpu->gb_mem[gb_cpu->gb_reg.HL.L];
        msb = gb_cpu->gb_mem[gb_cpu->gb_reg.HL.H];
        /*
            We have the address. Jump directly.
            No conditions for this case.
        */
        goto jmp;
    }

    /*
        Calculate the 16 bit address.
        Notice that the 16 bit address is not the only reason why
        we perform this operation. It is done to also increment the
        PC to the next instruction in case that the conditions are
        not met for the conditional jumps.
    */
    lsb = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    msb = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Hardcoding for unconditional jump
    if (opcode == 0xC3)
        goto jmp;

    /*
        Conditional Jumps.
        There are four different opcodes for four different conditions
            - NZ: Jump if not zero (zero flag not set)
            - NC: Jump if no carry (carry flag not set)
            - Z:  Jump if zero (zero flag set)
            - C:  Jump if carry (carry flag set)
    */
    switch (opcode & 0x0F)
    {
        case 0x02:
            if ((opcode & 0xF0) == 0xC0)
            {
                if(gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO))
                    goto nojmp;
                else
                    goto jmp;
                if(gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY))
                    goto nojmp;
                else
                    goto jmp;
            }
            break;

        case 0x0A:
            if ((opcode & 0xF0) == 0xC0)
            {
                if (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO))
                    goto jmp;
                else 
                    goto nojmp;
                if (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY))
                    goto jmp;
                else
                    goto nojmp;
            }
            break;
    }

    /*
        If we are allowed to jump, then jump to the 16bit address
        Else, do not set the PC. Realize that we have already
        accounted for the fact that if the conditions fail for the
        jump, we move on to the instruction after the jump.
    */
jmp:
    addr16 = (uint16_t)(msb << 8) | (uint16_t)lsb;
    gb_cpu->gb_reg.PC = addr16;

nojmp:
    return;
}

void call(gb_cpu_t *gb_cpu)
{
    /*
        Call: call a procedure defined in code
        Call is very similar to jp. It is a conditional/unconditional jump
        that takes 16bit immediate value as address. But there is one key
        difference: we do intend to come back. While a jump instruction is
        one directional: once the jump happens we do not intend to go back
        to the place of call unless another jump takes us there. With call,
        we save the instruction next to the call instruction and come back
        to this saved instruction once execution of the block of code
        contained in the place we are jumping to has finished executing.
        Call is used in conjunction with a "ret" at the end of the block
        of code we are jumping to. This block of code is termed as a
        "procedure". The idea is that since we can come back to the original
        place of execution, we may call the "procedure" over and over again,
        recycling originally written piece of code.
    */
    uint8_t opcode;
    uint16_t data16;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Save the immediate 16bit address from memory
    data16 = (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.PC++]) | \
             (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.PC++] << 8);

    // Save the current PC into stack. Remember we intend to come back later
    gb_cpu->gb_mem[--gb_cpu->gb_reg.SP] = \
        ((gb_cpu->gb_reg.PC & 0xFF00) >> 8);
    gb_cpu->gb_mem[--gb_cpu->gb_reg.SP] = \
        (gb_cpu->gb_reg.PC & 0x00FF);

    /*
        Using switch case as there are only 5 cases. I remember using resolution
        technique for another instruction (jp?) having only 5 cases and was messy
        from the perspective of readability. I'll see which one fits best and use
        that going forward.
    */
    switch (opcode)
    {
        // jump if Zero flag not set
        case 0xC4:
            if (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO))
                goto nocall;
            else
                goto call;
            break;

        // jump if Zero flag set
        case 0xCC:
            if (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO))
                goto call;
            else
                goto nocall;
            break;

        // jump if Carry flag not set
        case 0xD4:
            if (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY))
                goto nocall;
            else
                goto call;
            break;

        // jump if Carry flag set
        case 0xDC:
            if (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY))
                goto call;
            else
                goto nocall;
            break;

        // unconditional jump
        case 0xCD:
            goto call;
            break;
    }

// If the condition is true, update PC before return
call:
    gb_cpu->gb_reg.PC = data16;
// If condition is false, do not update PC, return directly
nocall:
    return;
}

void ret(gb_cpu_t *gb_cpu)
{
    /*
        ret: Return from a procedure.
        Nothing complex here: it is simply a return from a procedure
        Complete opposite of call. We just fetch the saved PC in stack
        and go back to where we came from. The jump back may also be
        conditional/unconditional.
    */
    uint8_t opcode;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    switch (opcode)
    {
        // return if Zero flag not set
        case 0xC0:
            if (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO))
                goto noendcall;
            else
                goto endcall;
            break;

        // return if Zero flag set
        case 0xD0:
            if (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO))
                goto endcall;
            else
                goto noendcall;
            break;

        // return if Carry flag not set
        case 0xC8:
            if (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY))
                goto noendcall;
            else
                goto endcall;
            break;

        // return if Carry flag set
        case 0xD8:
            if (gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY))
                goto endcall;
            else
                goto noendcall;
            break;

        // unconditional return
        case 0xC9:
            goto endcall;
            break;
    }

// If condition is true, return back to stored PC in stack
endcall:
    gb_cpu->gb_reg.PC = (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.SP++]) | \
                        (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.SP++] << 8);
// If condition is false, continue execution in procedure
noendcall:
    return;
}

void reti(gb_cpu_t *gb_cpu)
{
    /*
        reti: Return and enable interrupts.
        reti is an unconditonal return from a procedure that also enables
        interrupts in the process. The process of return is same as one
        in ret, so refer to that if confused about the PC set
    */
    gb_cpu->gb_reg.PC++;
    gb_cpu->ime = 1;
    gb_cpu->gb_reg.PC = (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.SP++]) | \
                        (uint16_t)(gb_cpu->gb_mem[gb_cpu->gb_reg.SP++] << 8);
    return;
}

/* Jump vectors */
void rst(gb_cpu_t *gb_cpu)
{
    /*
        Restart: Sets PC to reserved jump locations.
        I am not really sure exactly what is the use of the RST instruction,
        or really why the jump vectors are reserved. So I am just going to
        explain RST the way it is and try to understand the reason for its
        existence in the meantime.

        So RST is a jump instruction that jumps to specified reserved locations
        in memory called "jump vectors". 00,08,10,18,20,28,30,38H. There are
        definitions to what these locations are reserved for in the gameboy
        manual. Refer to that perhaps if you want to learn more about RST.

        First we push the current PC (mind you, this is after fetching opcode,
        so we have incremented PC once) to the stack. Then, we set the PC to
        the corresponding jump vector decided by the opcode.
    */
    uint8_t opcode, offset;

    // Fetch opcode from memory
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    // Push PC to stack
    gb_cpu->gb_mem[--gb_cpu->gb_reg.SP] = \
        (uint8_t)((gb_cpu->gb_reg.PC & 0xFF00) >> 8);
    gb_cpu->gb_mem[--gb_cpu->gb_reg.SP] = \
        (uint8_t)(gb_cpu->gb_reg.PC & 0x00FF);

    /*
        Calculate which jump vector to use
        The calculation for this is also simple.
        So the jump vectors are 00,08,10,18,20,28,30,38
        And the opcodes are     C7,CF,D7,DF,E7,EF,F7,FF
        See the pattern? The least signigicant byte decides whether the
        jump vector has 0 or 8 as the least significant byte. The MSB
        similarly decides the MSB of the jump vector. As usual, imma
        explain the offset calculation. Just for clarity.

        From what we see, when the LSB of the opcode is 7, then the lsb
        of the jump vector is 0 and F means the lsb is 8. So I parse the
        opcode to get its lsb, and modulo 7 is done to make the parsed
        value fall in either of: (0, if lsb is 7) or (1, if lsb is F).
        Multipy that with 0x08H, we get either 0x00H or 0x08H. Bingo.
        Now, the msb in the opcode. The pattern is apparent. Every
        third opcode is one step up from the last. And it starts from
        0xC0. We have done this before. Get it to 0,1,2,3 range by
        subtracting from it 0xC0. Looking at the jump vectors, we see
        we have to move in 0x10 increments every third opcode. So we
        multiply the 0,1,2,3 range calculation with 0x10. And now, we
        merege the two calculations. Simple as that.
    */
    offset = (0x10*(((opcode & 0xF0) >> 4) - 0x0C)) | (0x08*((opcode & 0x0F) % 7));

    // Set the PC to the jump vector
    gb_cpu->gb_reg.PC = (uint16_t)(0x0000 + offset);
    return;
}

/* Prefix CB instructions */

void rlc(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        RLC: Rotate left.
        Thing about this is: it's already implemented as rlca in vanilla
        instructions but with this one the op is not on the accumulator
        but a register of your choosing. We already know by now how this
        works: so no comments for this one. In fact, most prefix cb imma
        just skip comments, especially rotations.
    */
    uint8_t *reg8ptr;
    uint8_t data8, offset, bit7;
    uint16_t data16;

    if (cb_opcode == 0x06)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = ((cb_opcode & 0x0F) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    bit7 = (data8 & 0x80) >> 7;
    data8 = ((data8 << 1) | bit7);
    (cb_opcode == 0x06) ? (mem_write(gb_cpu, data16, data8)) : \
                          (*reg8ptr = data8);
    
    (data8 == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    (bit7) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    return;
}

void rrc(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        RRC: rotate right
    */
    uint8_t *reg8ptr;
    uint8_t data8, offset, bit0;
    uint16_t data16;

    if (cb_opcode == 0x0E)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((cb_opcode & 0x0F) - 8) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    bit0 = (data8 & 0x01);
    data8 = ((data8 >> 1) | (bit0 << 7));
    (cb_opcode == 0x0E) ? (mem_write(gb_cpu, data16, data8)) : \
                          (*reg8ptr = data8);
    
    (data8 == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    (bit0) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    return;
}

void rl(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        RL: rotate left with carry
    */
    uint8_t *reg8ptr;
    uint8_t data8, offset, bit7;
    uint16_t data16;

    if (cb_opcode == 0x16)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = ((cb_opcode & 0x0F) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    bit7 = (data8 & 0x80) >> 7;
    data8 = (data8 << 1) | \
            ((gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY)) >> FLAG_CARRY);
    (cb_opcode == 0x16) ? mem_write(gb_cpu, data16, data8) : \
                          (*reg8ptr = data8);

    (data8 == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    (bit7) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    return;
}

void rr(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        RR: rotate right with carry
    */
    uint8_t *reg8ptr;
    uint8_t data8, offset, bit0;
    uint16_t data16;

    if (cb_opcode == 0x1E)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((cb_opcode & 0x0F) - 8) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    bit0 = (data8 & 0x01);
    data8 = (data8 << 1) | \
            ((gb_cpu->gb_reg.AF.F & (1 << FLAG_CARRY)) << (7 - FLAG_CARRY));
    (cb_opcode == 0x1E) ? mem_write(gb_cpu, data16, data8) : \
                          (*reg8ptr = data8);

    (data8 == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    (bit0) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    return;
}

void sla(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        SLA: shift left arithmetic and bit7 saved in carry
    */
    uint8_t *reg8ptr;
    uint8_t data8, offset, bit7;
    uint16_t data16;

    if (cb_opcode == 0x26)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((cb_opcode & 0x0F)) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    bit7 = (data8 & 0x80) >> 7;
    data8 = (data8 << 1);
    (cb_opcode == 0x26) ? (mem_write(gb_cpu, data16, data8)) : \
                          (*reg8ptr = data8);

    (data8 == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    (bit7) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    return;
}

void sra(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        SRA: shift right arithmetic and bit0 saved in carry
        Gaps filled with bit7
    */
    uint8_t *reg8ptr;
    uint8_t data8, offset, bit0, bit7;
    uint16_t data16;

    if (cb_opcode == 0x2E)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((cb_opcode & 0x0F) - 8) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    bit0 = (data8 & 0x01);
    bit7 = (data8 & 0x80);
    data8 = (data8 >> 1) | (bit7);
    (cb_opcode == 0x2E) ? (mem_write(gb_cpu, data16, data8)) : \
                          (*reg8ptr = data8);

    (data8 == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    (bit0) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    return;
}

void swap(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        SWAP: swap the lower and higher nibble
    */
    uint8_t *reg8ptr;
    uint8_t data8, offset, temp;
    uint16_t data16;

    if (cb_opcode == 0x36)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = ((cb_opcode & 0x0F) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    temp = (data8 & 0x0F);
    data8 = ((data8 & 0xF0) >> 4) | (temp << 4);
    (cb_opcode == 0x36) ? mem_write(gb_cpu, data16, data8) : \
                          (*reg8ptr = data8);

    (data8 == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    clear_carry(gb_cpu);

    return;
}

void srl(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        SRL: shift right logical and bit0 saved in carry.
        MSB is put 0.
    */
    uint8_t *reg8ptr;
    uint8_t data8, offset, bit0;
    uint16_t data16;

    if (cb_opcode == 0x3E)
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((cb_opcode & 0x0F) - 8) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    bit0 = (data8 & 0x01);
    data8 = (data8 >> 1);
    (cb_opcode == 0x3E) ? (mem_write(gb_cpu, data16, data8)) : \
                          (*reg8ptr = data8);

    (data8 == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
    clear_addsub(gb_cpu);
    clear_halfcarry(gb_cpu);
    (bit0) ? set_carry(gb_cpu) : clear_carry(gb_cpu);

    return;
}

void bit(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        BIT: test if bit number is set or not.
        There are a few things to talk about here, I'll go over them
        in the comments below.
    */
    uint8_t testbit, data8, offset;
    uint16_t data16;

    /*
        Talk point one: resolve which bit to test.
        There is a distinct pattern here that we can exploit:
            x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  xA  xB  xC  xD  xE  xF
        4x   0   0   0   0   0   0   0   0   1   1   1   1   1   1   1   1
        5x   2   2   2   2   2   2   2   2   3   3   3   3   3   3   3   3
        6x   4   4   4   4   4   4   4   4   5   5   5   5   5   5   5   5
        7x   6   6   6   6   6   6   6   6   7   7   7   7   7   7   7   7

        As one can see, in a row, we increment by 1 once cb_opcode reaches x8
        and each row moves test bit in multiples of 2. 

        The first thing we do is convert row index to a sequence moving in
        multiples of 2. Basically take 4 5 6 7 and convert it to 0 2 4 6.
        Step 1 to that is adjust the base. Subtracting 4 from 4 5 6 7 makes
        it into 0 1 2 3. Next step, make the 1 step sequence into a sequence
        of evens. 2 * (n) help us with that. Finally, we get 2 * (n - 4) where
        n is 4 5 6 7.
    */
    testbit = (2 * (((cb_opcode & 0xF0) >> 4) - 4)) + \
              (((cb_opcode & 0x0F) < 0x08) ? 0 : 1);
    
    // Resolving where to get 8bit data from. We have done this before
    if (((cb_opcode & 0x0F) == 0x06) | ((cb_opcode & 0x0F) == 0x0E))
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((cb_opcode & 0x0F) % 8) + 2) % 9;
        data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
    }

    // Set zero flag if selected bit number is 0
    (data8 & (1 << testbit)) ? clear_zero(gb_cpu) : set_zero(gb_cpu);
    clear_addsub(gb_cpu);
    set_halfcarry(gb_cpu);

    return;
}

void res(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        RES: reset bit number of selected register
        The resolution method is same as BIT so re-using that
    */
    uint8_t *reg8ptr;
    uint8_t testbit, data8, offset;
    uint16_t data16;

    testbit = (2 * (((cb_opcode & 0xF0) >> 4) - 8)) + \
              (((cb_opcode & 0x0F) < 0x08) ? 0 : 1);
    
    // Resolving where to get 8bit data from. We have done this before
    if (((cb_opcode & 0x0F) == 0x06) | ((cb_opcode & 0x0F) == 0x0E))
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((cb_opcode & 0x0F) % 8) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    data8 &= ~(1 << testbit);
    ((cb_opcode & 0x0F) == 0x06) | ((cb_opcode & 0x0F) == 0x0E) ? \
    (mem_write(gb_cpu, data16, data8)) : (*reg8ptr = data8);

    /* No flag updates */
    return;
}

void set(gb_cpu_t *gb_cpu, uint8_t cb_opcode)
{
    /*
        SET: Set bit on bit number for specific register
    */
    uint8_t *reg8ptr;
    uint8_t testbit, data8, offset;
    uint16_t data16;

    testbit = (2 * (((cb_opcode & 0xF0) >> 4) - 0x0C)) + \
              (((cb_opcode & 0x0F) < 0x08) ? 0 : 1);
    
    // Resolving where to get 8bit data from. We have done this before
    if (((cb_opcode & 0x0F) == 0x06) | ((cb_opcode & 0x0F) == 0x0E))
    {
        data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
                 (uint16_t)(gb_cpu->gb_reg.HL.L);
        data8 = mem_read(gb_cpu, data16);
    }
    else
    {
        offset = (((cb_opcode & 0x0F) % 8) + 2) % 9;
        reg8ptr = (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset);
        data8 = *reg8ptr;
    }

    data8 |= (1 << testbit);
    ((cb_opcode & 0x0F) == 0x06) | ((cb_opcode & 0x0F) == 0x0E) ? \
    (mem_write(gb_cpu, data16, data8)) : (*reg8ptr = data8);

    /* No flag updates */
    return;
}

/* Call the prefix CB Instructions */
void pfcb(gb_cpu_t *gb_cpu)
{
    uint8_t cb_opcode;

    gb_cpu->gb_reg.PC++;
    cb_opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
    prefix_cb[cb_opcode](gb_cpu, cb_opcode);

    return;
}