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
        to manipulate using the opcode given. The ones not following pattern,
        i.e, the last part of each pattern, I have hardcoded. I couldn't find
        any pattern to why specifically they were chosen. Same comment goes
        for decrement operation, so not putting any comment there.
    */
    uint8_t idx, reg_set, data8, opcode;
    uint16_t data16;
    uint8_t *data;

    opcode = gb_cpu->gb_mem[gb_cpu->gb_reg.PC];

    idx = 2*(((opcode & 0xF0) >> 4)+1);

    if ((opcode & 0x0F) == 0x03)
    {
        if (idx == 0x08)
        {
            gb_cpu->gb_reg.SP++;
            return;
        }
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

    reg_set = ((opcode & 0x0F) == 0x04)?0:1;

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

    data = \
    (uint8_t *)((unsigned long)(&gb_cpu->gb_reg)+idx+reg_set);
    (*data)++;

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
        See increment comment for info on implementation
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

}

void pop(gb_cpu_t *gb_cpu)
{

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