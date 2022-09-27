/*
    Instruction Set Architecture
    I am attempting something that I have never done: optimization
    Normally I would have approached the ISA by using switch 
    statements, but this time I will be using function pointers.
    I will create one structure for data transfer to the function as
    per the instruction and the instruction itself will decide what
    data to pick from the data structure. If that makes any sense.
    The idea is to generalize all functions so that the ISA can be 
    implemented using a table (an array, if you will) that automatically
    takes care of opcodes as well, as the function pointer to an instruction
    will be stored at the index corresponding to its opcode.
*/
#ifndef __ISA_H__
#define __ISA_H__

#include <gbcpu.h>
#include <stdlib.h>
#include <stdint.h>

/*
    ISA DATA
    isa_data_t is (ideally)(hopefully)(hopefully) supposed to encompass
    any data requirement that the opcode (instruction) asks for. It is 
    the only way the function pointer method can be implemented for this
    ISA as each instruction takes different inputs and performs different
    operations on the cpu/memory.
*/
typedef struct {
    uint8_t opcode;
    uint8_t pc;
    uint8_t data8bit;
    uint16_t data16bit;
    uint8_t reg;
} isa_data_t;

/* CPU related instructions */
void nop(gb_cpu_t *gb_cpu);               /* No Operation */
void stop(gb_cpu_t *gb_cpu);              /* low power standby mode ? */
void halt(gb_cpu_t *gb_cpu);              /* Halt operations until interrupt occurs */
void ccf(gb_cpu_t *gb_cpu);               /* Set carry as carry XOR 1 */
void scf(gb_cpu_t *gb_cpu);               /* Set carry flag to 1 */
void di(gb_cpu_t *gb_cpu);                /* Disable Interrupts */
void ei(gb_cpu_t *gb_cpu);                /* Enable Interrupts */
/* Load/Store instructions */
void ld(gb_cpu_t *gb_cpu);                /* Load operation */
void ldh(gb_cpu_t *gb_cpu);               /* Load Halfword */

/* Increment/Decrement instructions */
void inc(gb_cpu_t *gb_cpu);               /* Increment operation */
void dec(gb_cpu_t *gb_cpu);               /* Decrement operation */

/* Arithmetic operations */
void add(gb_cpu_t *gb_cpu);               /* Add operation */
void adc(gb_cpu_t *gb_cpu);               /* Add with carry operation */
void sub(gb_cpu_t *gb_cpu);               /* Subtraction operation */
void sbc(gb_cpu_t *gb_cpu);               /* Subtract with carry operation */
void daa(gb_cpu_t *gb_cpu);               /* Decimal adjust A ? */
void _and(gb_cpu_t *gb_cpu);              /* Bitwise AND operation */
void _or(gb_cpu_t *gb_cpu);               /* Bitwise OR operation */
void _xor(gb_cpu_t *gb_cpu);              /* Bitwise XOR operation */
void cp(gb_cpu_t *gb_cpu);                /* Compare operation */
void cpl(gb_cpu_t *gb_cpu);               /* A = A xor FF ? */

/* Rotate Instructions */
void rlca(gb_cpu_t *gb_cpu);              /* Rotate left A */
void rrca(gb_cpu_t *gb_cpu);              /* Rotate right A */
void rla(gb_cpu_t *gb_cpu);               /* Rotate A left through carry */
void rra(gb_cpu_t *gb_cpu);               /* Rotate A right through carry */

/* Stack Instructions */
void push(gb_cpu_t *gb_cpu);              /* Push register data to stack */
void pop(gb_cpu_t *gb_cpu);               /* Pop to register */

/* Jump/Call Instructions */
void jr(gb_cpu_t *gb_cpu);                /* Relative jump */
void jp(gb_cpu_t *gb_cpu);                /* Jump to */
void call(gb_cpu_t *gb_cpu);              /* Call procedure */
void ret(gb_cpu_t *gb_cpu);               /* Return from procedure */
void reti(gb_cpu_t *gb_cpu);              /* Return and enable interrupts */

/* Jump vectors */
void rst(gb_cpu_t *gb_cpu);               /* Jump vector call */

/* Prefix CB Instructions */
void pfcb(gb_cpu_t *gb_cpu);              /* Prefix CB instructions ? */

/* Instruction table indexed by opcode */
static void (*isa[])(gb_cpu_t *) =
//          0     1     2     3     4     5     6     7     8     9     A     B     C     D     E     F
{
/* 0 */  nop,   ld,   ld,  inc,  inc,  dec,   ld, rlca,   ld,  add,   ld,  dec,  inc,  dec,   ld, rrca,
/* 1 */ stop,   ld,   ld,  inc,  inc,  dec,   ld,  rla,   jr,  add,   ld,  dec,  inc,  dec,   ld,  rra,
/* 2 */   jr,   ld,   ld,  inc,  inc,  dec,   ld,  daa,   jr,  add,   ld,  dec,  inc,  dec,   ld,  cpl,
/* 3 */   jr,   ld,   ld,  inc,  inc,  dec,   ld,  scf,   jr,  add,   ld,  dec,  inc,  dec,   ld,  ccf,
/* 4 */   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,
/* 5 */   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,
/* 6 */   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,
/* 7 */   ld,   ld,   ld,   ld,   ld,   ld, halt,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,   ld,
/* 8 */  add,  add,  add,  add,  add,  add,  add,  add,  adc,  adc,  adc,  adc,  adc,  adc,  adc,  adc,
/* 9 */  sub,  sub,  sub,  sub,  sub,  sub,  sub,  sub,  sbc,  sbc,  sbc,  sbc,  sbc,  sbc,  sbc,  sbc,
/* A */ _and, _and, _and, _and, _and, _and, _and, _and, _xor, _xor, _xor, _xor, _xor, _xor, _xor, _xor,
/* B */  _or,  _or,  _or,  _or,  _or,  _or,  _or,  _or,   cp,   cp,   cp,   cp,   cp,   cp,   cp,   cp,
/* C */  ret,  pop,   jp,   jp, call, push,  add,  rst,  ret,  ret,   jp, pfcb, call, call,  adc,  rst,
/* D */  ret,  pop,   jp, NULL, call, push,  sub,  rst,  ret, reti,   jp, NULL, call, NULL,  sbc,  rst,
/* E */  ldh,  pop,   ld, NULL, NULL, push, _and,  rst,  add,   jp,   ld, NULL, NULL, NULL, _xor,  rst,
/* F */  ldh,  pop,   ld,   di, NULL, push,  _or,  rst,   ld,   ld,   ld,   ei, NULL, NULL,   cp,  rst
};

#endif