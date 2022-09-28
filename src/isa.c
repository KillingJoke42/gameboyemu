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
    gb_cpu->gb_reg.AF.F = (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO));
    return;
}

void scf(gb_cpu_t *gb_cpu)
{
    /*
        Set Carry Flag: sets the carry flag if set
        Pretty straightforward: just sets the carry flag. cy=1.
    */
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
    gb_cpu->ime = 0;
    return;
}

void ei(gb_cpu_t *gb_cpu)
{
    /*
        Enable interrupts: Enables the master interrupt enable flag
        IME=1.
    */
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
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC];

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

    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC];

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
    ((*data & 0x0F) == 0x0F) ? set_halfcarry(gb_cpu) : clear_halfcarry(gb_cpu);
    return;
}

/* Arithmetic operations */
void add(gb_cpu_t *gb_cpu)
{

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
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC];

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
        data8 = gb_cpu->gb_mem[++gb_cpu->gb_reg.PC];
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
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC];

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
    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC];

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

}

/* Prefix CB Instructions */
void pfcb(gb_cpu_t *gb_cpu)
{

}