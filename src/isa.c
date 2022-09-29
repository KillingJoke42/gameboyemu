#include <isa.h>
#include <memorymap.h>
#include <stdint.h>
#include <utils.h>

/* CPU related instructions */
void nop(gb_cpu_t *gb_cpu)
{
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
    while (1)
    {
        uint8_t _ie = mem_read(gb_cpu, GB_INTR_ENAB_REG);
        uint8_t _if = mem_read(gb_cpu, GB_INTR_FLAG_REG);
        if (_ie & _if != 0)
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
    gb_cpu->gb_reg.AF.F = (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO));
    return;
}

void scf(gb_cpu_t *gb_cpu)
{
    /*
        Set Carry Flag: sets the carry flag if set
        Pretty straightforward: just sets the carry flag. cy=1.
    */
    gb_cpu->gb_reg.PC++;
    gb_cpu->gb_reg.AF.F = (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO)) | (1 << FLAG_CARRY);
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

}

void ldh(gb_cpu_t *gb_cpu)
{

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

    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

    if (opcode == 0xE8)
    {
        primary16 = gb_cpu->gb_reg.SP;
        data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];

        result16 = primary16 + (uint16_t)data8;
        gb_cpu->gb_reg.SP = result16;

        clear_zero(gb_cpu);
        clear_addsub(gb_cpu);
        // A+B > 0xFF
        (((uint8_t)(primary16 & 0x0F)+(data8 & 0x0F)) & 0x10) ? \
            set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
        (((primary16 & 0x00FF)+data8) & 0x0100) ? \
            set_carry(gb_cpu) : clear_carry(gb_cpu);
        return;
    }

    if (opcode == 0xC6 || (opcode & 0xF0) == 0x80)
    {
        if (opcode == 0xC6)
            data8 = gb_cpu->gb_mem[gb_cpu->gb_reg.PC++];
        else if (opcode == 0x86)
        {
            data16 = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);
            data8 = mem_read(gb_cpu, data16);
        }
        else if (opcode == 0xC6)
            data8 = gb_cpu->gb_reg.AF.A;
        else
        {
            offset = 2+((opcode & 0xF0) >> 4);
            data8 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
        }

        orgAval = gb_cpu->gb_reg.AF.A;
        gb_cpu->gb_reg.AF.A += data8;

        (gb_cpu->gb_reg.AF.A == 0) ? set_zero(gb_cpu) : clear_zero(gb_cpu);
        clear_addsub(gb_cpu);
        (((orgAval & 0x0F)+(data8 & 0x0F)) & 0x10) ?
            set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
        (orgAval > (0xFF - data8)) ?
            set_carry(gb_cpu) : clear_carry(gb_cpu);
        return;
    }

    primary16 =
        (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)(gb_cpu->gb_reg.HL.L);

    if (opcode == 0x39)
        data16 = gb_cpu->gb_reg.SP;
    else
    {
        offset = 2*(((opcode & 0xF0) >> 4)+1);
        data16 = *((uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+offset));
    }

    result16 = primary16+data16;
    gb_cpu->gb_reg.HL.H = (uint8_t)((result16 & 0xFF00) >> 8);
    gb_cpu->gb_reg.HL.L = (uint8_t)(result16 & 0x00FF);

    clear_addsub(gb_cpu);
    (((primary16 & 0x0FFF)+(data16 & 0x0FFF)) & 0x1000) ? \
        set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
    (primary16 > (0xFFFF - data16)) ?
        set_carry(gb_cpu) : clear_carry(gb_cpu);
    return;
}

void adc(gb_cpu_t *gb_cpu)
{

}

void sub(gb_cpu_t *gb_cpu)
{

}

void sbc(gb_cpu_t *gb_cpu)
{

}

void daa(gb_cpu_t *gb_cpu)
{

}

void _and(gb_cpu_t *gb_cpu)
{

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

}

void cp(gb_cpu_t *gb_cpu)
{

}

void cpl(gb_cpu_t *gb_cpu)
{

}

/* Rotate Instructions */
void rlca(gb_cpu_t *gb_cpu)
{

}

void rrca(gb_cpu_t *gb_cpu)
{

}

void rla(gb_cpu_t *gb_cpu)
{

}

void rra(gb_cpu_t *gb_cpu)
{

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

}

void ret(gb_cpu_t *gb_cpu)
{

}

void reti(gb_cpu_t *gb_cpu)
{

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

/* Prefix CB Instructions */
void pfcb(gb_cpu_t *gb_cpu)
{

}