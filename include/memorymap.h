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

/* Memory Read/Write handler */
void mem_write(gb_cpu_t *gb_cpu, uint16_t addr, uint8_t val);
uint8_t mem_read(gb_cpu_t *gb_cpu, uint16_t addr);

#endif