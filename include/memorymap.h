/*
    GameBoy Emulator attempt: hwmap.h
    Contains definitions and emulated structres for the GameBoy Color
*/

#ifndef __HWMAP_H__
#define __HWMAP_H__

#include <gbcpu.h>
#include <stdint.h>

/*
    Game Boy Memory Mapping
    
    -----------------   0xFFFF
    | INT_WORK_RAM  |
    |               |
    |               |
    |               |
    -----------------   0xC000
    | EXT_WORK_RAM  |
    |               |
    -----------------   0xA000
    | DISPLAY_RAM   |
    |               |
    -----------------   0x8000
    | HOME          |
    |               |
    |               |
    |               |
    -----------------   0x4000
    | FIXED_HOME    |
    |               |
    |               |
    |               |
    -----------------   0x0000
*/

#define GB_TOTAL_ADDRESSABLE_MEMORY         0xFFFF      /* 64 Kb address bus */
#define GB_GAME_ROM_BASE_ADDR               0x0100

#define GB_ROM_BANK_FIXED_START             0x0000
#define GB_ROM_BANK_SWITCHABLE_START        0x4000
#define GB_VIDEO_RAM_START                  0x8000
#define GB_EXT_RAM_START                    0xA000
#define GB_WORK_RAM_0_START                 0xC000
#define GB_WORK_RAM_1_START                 0xD000
/* Address E000 - FDFF is ECHO of C000-DDFF (not used) */
#define GB_SPRITE_ATTR_TABLE_START          0xFE00
/* Address FEA0 - FEFF is not usable */
#define GB_HIGH_RAM_START                   0xFF80

#define GB_INTR_ENAB_REG                    0xFFFF
#define GB_INTR_FLAG_REG                    0xFF0F

/* No clue what these are used for. Docs just say: "jump vectors for x" */
// extern uint16_t jump_vectors_RST[] = {0x0000, 0x0008, 0x0010, 0x0018, 0x0020, 0x0028, 0x0030, 0x0038};
// extern uint16_t jump_vectors_int[] = {0x0040, 0x0048, 0x0050, 0x0058, 0x0060};

/* 
    I/O Ranges.
    One of the things Nintendo did with the SHARP is remove I/O Registers that were 
    available in INTEL 8080. So to interface hardware, we map them to locations in
    memory from where they may be accessed. Here is the definitions for those memory
    locations
*/
#define GB_JOYPAD_INPUT_REG                 0xFF00
#define GB_SERIAL_TXRX_START                0xFF01
#define GB_TIMER_START                      0xFF04
#define GB_SOUND_START                      0xFF10
#define GB_WAVE_PATTERN_START               0xFF30
#define GB_LCD_CONTROL_START                0xFF40
#define GB_BOOT_ROM_DISABLE_REG             0xFF50

/*
    Serial Transfer Registers
    Two game boys could communicate with each other using Serial communication. These registers
    will help. One gameboy generates the clock that both gameboys will use to transfer data.
    Each cycle, a bit is received.
    
    SB - Serial Transfer Data. Before a data transfer, the next byte is held in this register.
    During a data transfer, this register is an amalgamation of outgoing and incoming bits.
    Each cycle, a bit is shifted from the left over the wire and a bit is shifted in from the
    reception end.
    SC - Serial Transfer Control.
        Bit 7 = Transfer Start Flag (0 = No transfer in progress, 1 = Transfer in progress or requested)
        Bit 1 = Clock Speed (0 = Normal, 1 = Fast (CGB))
        Bit 0 = Shift Clock (0 = External, 1 = Internal)

    When two gameboys want to communicate, TX GB writes 0x81 to SC and the byte to be sent to SB.
    Once the transfer is complete, bit 7 of SC is cleared and Serial Interrupt Handler is called.
    The slave gameboy "might" set the bit 7 of its SC and will clear it at the end of the transfer
    if it "bothered" to set the bit in the first place. No I don't know what "might" means here.
    Further reasearch is definitely required.
*/
#define GB_SERIAL_TRANSFER_DATA             0xFF01
#define GB_SERIAL_TRANSFER_CONTROL          0xFF02

#define POWERUP_STACK_ADDR_CNT              31

static uint16_t powerup_stack_addrs[POWERUP_STACK_ADDR_CNT] = \
{
    0xFF05, 0xFF06, 0xFF07, 0xFF10, 0xFF11, 0xFF12,
    0xFF14, 0xFF16, 0xFF17, 0xFF19, 0xFF1A, 0xFF1B,
    0xFF1C, 0xFF1E, 0xFF20, 0xFF21, 0xFF22, 0xFF23,
    0xFF24, 0xFF25, 0xFF26, 0xFF40, 0xFF42, 0xFF43,
    0xFF45, 0xFF47, 0xFF48, 0xFF49, 0xFF4A, 0xFF4B,
    0xFFFF
};

static uint8_t powerup_stack_init[POWERUP_STACK_ADDR_CNT] = \
{
    0x00, 0x00, 0x00, 0x80, 0xBF, 0xF3,
    0xBF, 0x3F, 0x00, 0xBF, 0x7F, 0xFF, 
    0x9F, 0xBF, 0xFF, 0x00, 0x00, 0xBF,
    0x77, 0xF3, 0xF1, 0x91, 0x00, 0x00,
    0x00, 0xFC, 0xFF, 0xFF, 0x00, 0x00,
    0x00
};

/* Memory Read/Write handler */
void mem_write(gb_cpu_t *gb_cpu, uint16_t addr, uint8_t val);
uint8_t mem_read(gb_cpu_t *gb_cpu, uint16_t addr);

#endif