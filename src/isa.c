#include <isa.h>
#include <memorymap.h>
#include <stdint.h>

/* CPU related instructions */
void nop(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{
    return;
}

void stop(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{
    /*
        STOP
        Instructions on how to implement this command come from multiple sources. It seems that
        when the GB is pushed into this state, the screen goes white (with one dark line?) and 
        all processes are stopped until a button is pressed. Some websites state this is via 
        the Button interrupt. 
    */
}

void halt(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
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

void ccf(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{
    /*
        Clear Carry Flag: clears the carry flag if set
        Pretty straightforward: just clears the carry flag. cy=(cy xor 1).
    */
    gb_cpu->gb_reg.AF.F = (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO));
    return;
}

void scf(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{
    /*
        Set Carry Flag: sets the carry flag if set
        Pretty straightforward: just sets the carry flag. cy=1.
    */
    gb_cpu->gb_reg.AF.F = (gb_cpu->gb_reg.AF.F & (1 << FLAG_ZERO)) | (1 << FLAG_CARRY);
}

void di(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{
    /*
        Disable Interrupts: Disables the master interrupt enable flag
        IME=0. Note to self: Idk how this will help as C does not understand
        nor can fire an ISR. IDK what to do for this.
    */
    gb_cpu->ime = 0;
    return;
}

void ei(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{
    /*
        Enable interrupts: Enables the master interrupt enable flag
        IME=1.
    */
    gb_cpu->ime = 1;
    return;
}

/* Load/Store instructions */
void ld(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void ldh(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

/* Increment/Decrement instructions */
void inc(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void dec(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

/* Arithmetic operations */
void add(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void adc(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void sub(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void sbc(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void daa(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void _and(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void _or(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void _xor(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void cp(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void cpl(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

/* Rotate Instructions */
void rlca(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void rrca(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void rla(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void rra(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

/* Stack Instructions */
void push(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void pop(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

/* Jump/Call Instructions */
void jr(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void jp(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void call(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void ret(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

void reti(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

/* Jump vectors */
void rst(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}

/* Prefix CB Instructions */
void pfcb(gb_cpu_t *gb_cpu, isa_data_t *isa_data)
{

}