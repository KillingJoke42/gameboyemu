#include <stdio.h>
#include <gbcpu.h>
#include <isa.h>
#include <utils.h>
#include <string.h>
#include <memorymap.h>

int main(void)
{
    gb_cpu_t *gb_cpu;
    game_file_t *game_data;
    uint8_t instrs = 50;

    gb_cpu = (gb_cpu_t *)malloc(sizeof(gb_cpu_t));
    gb_cpu->gb_mem = (uint8_t *)malloc(sizeof(uint8_t) * 0x10000);

    reset_register_init(gb_cpu);

    // GBEMU_PRINT(("Hello, GameBoy!\n"));
    game_data = open_gb_rom("/home/rainaanant/gameboyemu/ROMS/gb-test-roms/cpu_instrs/individual/11-op a,(hl).gb");
    load_gb_rom(gb_cpu, game_data);

    free(game_data);

    char c;
    while (1)
    {
        GBEMU_PRINT(("PC: %x; opcode: %x\n", gb_cpu->gb_reg.PC, \
                        gb_cpu->gb_mem[gb_cpu->gb_reg.PC]));
        isa[gb_cpu->gb_mem[gb_cpu->gb_reg.PC]](gb_cpu);
        if (gb_cpu->gb_mem[0xFF02] == 0x81)
        {
            GBEMU_PRINT(("%c", gb_cpu->gb_mem[0xFF01]));
            gb_cpu->gb_mem[0xFF02] = 0x00;
        }
        register_dump(gb_cpu);
        while ((c = getchar()) != '\n' && c != EOF);
    }

    free(gb_cpu->gb_mem);
    free(gb_cpu);
    return 0;
}