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
uint8_t nop(gb_cpu_t *gb_cpu);               /* No Operation */
uint8_t stop(gb_cpu_t *gb_cpu);              /* low power standby mode ? */
uint8_t halt(gb_cpu_t *gb_cpu);              /* Halt operations until interrupt occurs */
uint8_t ccf(gb_cpu_t *gb_cpu);               /* Set carry as carry XOR 1 */
uint8_t scf(gb_cpu_t *gb_cpu);               /* Set carry flag to 1 */
uint8_t di(gb_cpu_t *gb_cpu);                /* Disable Interrupts */
uint8_t ei(gb_cpu_t *gb_cpu);                /* Enable Interrupts */
/* Load/Store instructions */
uint8_t ld(gb_cpu_t *gb_cpu);                /* Load operation */
uint8_t ldh(gb_cpu_t *gb_cpu);               /* Load Halfword */

/* Increment/Decrement instructions */
uint8_t inc(gb_cpu_t *gb_cpu);               /* Increment operation */
uint8_t dec(gb_cpu_t *gb_cpu);               /* Decrement operation */

/* Arithmetic operations */
uint8_t add(gb_cpu_t *gb_cpu);               /* Add operation */
uint8_t adc(gb_cpu_t *gb_cpu);               /* Add with carry operation */
uint8_t sub(gb_cpu_t *gb_cpu);               /* Subtraction operation */
uint8_t sbc(gb_cpu_t *gb_cpu);               /* Subtract with carry operation */
uint8_t daa(gb_cpu_t *gb_cpu);               /* Decimal adjust A ? */
uint8_t _and(gb_cpu_t *gb_cpu);              /* Bitwise AND operation */
uint8_t _or(gb_cpu_t *gb_cpu);               /* Bitwise OR operation */
uint8_t _xor(gb_cpu_t *gb_cpu);              /* Bitwise XOR operation */
uint8_t cp(gb_cpu_t *gb_cpu);                /* Compare operation */
uint8_t cpl(gb_cpu_t *gb_cpu);               /* A = A xor FF ? */

/* Rotate Instructions */
uint8_t rlca(gb_cpu_t *gb_cpu);              /* Rotate left A */
uint8_t rrca(gb_cpu_t *gb_cpu);              /* Rotate right A */
uint8_t rla(gb_cpu_t *gb_cpu);               /* Rotate A left through carry */
uint8_t rra(gb_cpu_t *gb_cpu);               /* Rotate A right through carry */

/* Stack Instructions */
uint8_t push(gb_cpu_t *gb_cpu);              /* Push register data to stack */
uint8_t pop(gb_cpu_t *gb_cpu);               /* Pop to register */

/* Jump/Call Instructions */
uint8_t jr(gb_cpu_t *gb_cpu);                /* Relative jump */
uint8_t jp(gb_cpu_t *gb_cpu);                /* Jump to */
uint8_t call(gb_cpu_t *gb_cpu);              /* Call procedure */
uint8_t ret(gb_cpu_t *gb_cpu);               /* Return from procedure */
uint8_t reti(gb_cpu_t *gb_cpu);              /* Return and enable interrupts */

/* Jump vectors */
uint8_t rst(gb_cpu_t *gb_cpu);               /* Jump vector call */

/* Prefix CB Instructions */
uint8_t pfcb(gb_cpu_t *gb_cpu);              /* Prefix CB instructions ? */

/* Instruction table indexed by opcode */
static uint8_t (*isa[])(gb_cpu_t *) =
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
/* E */   ld,  pop,   ld, NULL, NULL, push, _and,  rst,  add,   jp,   ld, NULL, NULL, NULL, _xor,  rst,
/* F */   ld,  pop,   ld,   di, NULL, push,  _or,  rst,   ld,   ld,   ld,   ei, NULL, NULL,   cp,  rst
};

/* Prefix CB instructions */

uint8_t  rlc(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t  rrc(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t   rl(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t   rr(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t  sla(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t  sra(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t swap(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t  srl(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t  bit(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t  res(gb_cpu_t *gb_cpu, uint8_t cb_opcode);
uint8_t  set(gb_cpu_t *gb_cpu, uint8_t cb_opcode);

static uint8_t (*prefix_cb[])(gb_cpu_t *, uint8_t) = 
//          0     1     2     3     4     5     6     7     8     9     A     B     C     D     E     F
{
/* 0 */  rlc,  rlc,  rlc,  rlc,  rlc,  rlc,  rlc,  rlc,  rrc,  rrc,  rrc,  rrc,  rrc,  rrc,  rrc,  rrc,
/* 1 */   rl,   rl,   rl,   rl,   rl,   rl,   rl,   rl,   rr,   rr,   rr,   rr,   rr,   rr,   rr,   rr,
/* 2 */  sla,  sla,  sla,  sla,  sla,  sla,  sla,  sla,  sra,  sra,  sra,  sra,  sra,  sra,  sra,  sra,
/* 3 */ swap, swap, swap, swap, swap, swap, swap, swap,  srl,  srl,  srl,  srl,  srl,  srl,  srl,  srl,
/* 4 */  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,
/* 5 */  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,
/* 6 */  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,
/* 7 */  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,  bit,
/* 8 */  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,
/* 9 */  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,
/* A */  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,
/* B */  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,  res,
/* C */  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,
/* D */  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,
/* E */  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,
/* F */  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set,  set
};

void processInterrupts(gb_cpu_t *gb_cpu);
#endif