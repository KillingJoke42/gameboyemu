/* 
    GameBoy CPU emulation
    GameBoy made use of the Sharp LR35902, which is a hybrid of the intel 8080
    and the Zilog Z80. From what I can understand, features were mostly removed
    and a few added in lieu of making it application specific to the nintendo 
    gameboy. Structre definitons for the CPU core and the register definitions 
    for the same can be found here.
*/

#ifndef __GBCPU_H__
#define __GBCPU_H__

#include <stdint.h>

// This is the speed of the gameboy (4194304) / 60 (60 FPS)
#define GB_CLK_SPEED    4194304
#define MAXCYCLES       69905

#define set_carry(gb_cpu) \
    (gb_cpu->gb_reg.AF.F |= (1 << FLAG_CARRY))

#define clear_carry(gb_cpu) \
    (gb_cpu->gb_reg.AF.F &= ~(1 << FLAG_CARRY))

#define set_zero(gb_cpu) \
    (gb_cpu->gb_reg.AF.F |= (1 << FLAG_ZERO))

#define clear_zero(gb_cpu) \
    (gb_cpu->gb_reg.AF.F &= ~(1 << FLAG_ZERO))

#define set_halfcarry(gb_cpu) \
    (gb_cpu->gb_reg.AF.F |= (1 << FLAG_BCD_HALF_CARRY))

#define clear_halfcarry(gb_cpu) \
    (gb_cpu->gb_reg.AF.F &= ~(1 << FLAG_BCD_HALF_CARRY))

#define set_addsub(gb_cpu) \
    (gb_cpu->gb_reg.AF.F |= (1 << FLAG_BCD_ADDSUB))

#define clear_addsub(gb_cpu) \
    (gb_cpu->gb_reg.AF.F &= ~(1 << FLAG_BCD_ADDSUB))
    
/*
    Flag register definitions
    The flag reigster is an 8bit register that is used to detect and store information
    about the output after an arithmetic operation has been performed. These can later
    be used in conditional jumps and case detection. Consists of:
        - Zero Flag: Set when output is zero
        - Carry Flag: It is set in three cases
            - Addition leads to 8/16bit overflow
            - Subtraction results in output below zero
            - Rotate/Shift operation shifts out a '1' bit
        - BCD Flags: As per the docs, these flags are rarely used. They seem to only be
                        used in the case of DAA instruction. I don't really know if I 
                        would need that for GB. But I'll code it in anyway. Under these
                        BCD Flags are two types:
                            - N Flag: Checks whether previous operation was an addition
                                        or subtraction operation
                            - H Flag: Holds carry-out generated after 4 bits in BCD 
                                        arithmetic. Note that C is still used for 8
                                        bit carryout.
*/

enum flag_reg_def {
    FLAG_CARRY = 4,
    FLAG_BCD_HALF_CARRY,
    FLAG_BCD_ADDSUB,
    FLAG_ZERO
};

/* 
    GameBoy is byte addressable, with 8-bit bus as well. So all registers are 8 bit.
    However hybrid registers is also a concept implemented with gameboy wherein two
    8-bit registers may be clubbed together to form a 16 bit (virtual) register that
    may have 16 bit operations performed on it. So here goes. Register definitions
*/
typedef struct {
    struct {
        uint8_t A;
        uint8_t F;
    } AF;

    struct {
        uint8_t B;
        uint8_t C;
    } BC;

    struct {
        uint8_t D;
        uint8_t E;
    } DE;

    struct {
        uint8_t H;
        uint8_t L;
    } HL;

    /* Stack Pointer */
    uint16_t SP;
    /* Program Counter */
    uint16_t PC;
} gb_reg_t;


/* 
    GameBoy Core.
    Now that we have defined the registers and the memory module, we may define
    the actual core itself. The core will contain the 64k memory (as address bus
    is 16bit) and the register definitions. There is also the Interrupt master 
    enable (IME) which masks all interrupts even if flags for them are set provided
    IME=0
*/
typedef struct {
    gb_reg_t gb_reg;
    uint8_t *gb_mem;
    uint8_t ime;
    uint32_t timerCounter;
    uint32_t dividerCounter;
} gb_cpu_t;

#endif