#include <gbcpu.h>
#include <utils.h>
#include <memorymap.h>

/* 
    Memory Write: Write data to GB RAM
    Inputs:
        - addr: address to write value in
        - val:  value to be placed at addr
    Returns:
        - void
*/
void mem_write(gb_cpu_t *gb_cpu, uint16_t addr, uint8_t val)
{
    if (addr == 0xFF0F)
    {
        GBEMU_PRINT(("Val written to 0xFF0F = %02X. Value before: %02X\n", val, mem_read(gb_cpu, addr)));
    }
    gb_cpu->gb_mem[addr] = val;
    return;
}

/*
    Memory Read: Read data to GB RAM
    Input:
        - addr: address to read from
    Returns:
        - memval: unsigned 8bit value stored at addr
*/
uint8_t mem_read(gb_cpu_t *gb_cpu, uint16_t addr)
{
    return gb_cpu->gb_mem[addr];
}