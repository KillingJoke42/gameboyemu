#include <stdio.h>
#include <gbcpu.h>
#include <isa.h>
#include <utils.h>
#include <string.h>
#include <memorymap.h>

void Update(gb_cpu_t *gb_cpu)
{
    uint32_t cyclesThisFrame = 0;
    while (cyclesThisFrame < MAXCYCLES)
    {
        register_dump(gb_cpu);
        cyclesThisFrame += isa[gb_cpu->gb_mem[gb_cpu->gb_reg.PC]](gb_cpu);
        processInterrupts(gb_cpu);
    }

    return;
}

int main(void)
{
    gb_cpu_t *gb_cpu;
    game_file_t *game_data;

    gb_cpu = (gb_cpu_t *)malloc(sizeof(gb_cpu_t));
    gb_cpu->gb_mem = (uint8_t *)malloc(sizeof(uint8_t) * 0x10000);

    reset_register_init(gb_cpu);

    game_data = open_gb_rom("/home/anantraina/gameboyemu/ROMS/gb-test-roms/cpu_instrs/individual/02-interrupts.gb");
    load_gb_rom(gb_cpu, game_data);

    free(game_data);

    free(gb_cpu->gb_mem);
    free(gb_cpu);
    return 0;
}