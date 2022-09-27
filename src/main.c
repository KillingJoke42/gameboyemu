#include <stdio.h>
#include <gbcpu.h>
#include <isa.h>
#include <utils.h>
#include <string.h>

int main(void)
{
    // game_file_t *game_data;

    // GBEMU_PRINT(("Hello, GameBoy!\n"));
    // game_data = open_gb_rom("/home/anantraina/gameboyemu/ROMS/tetris.gb");

    gb_cpu_t *gb_cpu;
    isa_data_t *isa_data;

    gb_cpu = (gb_cpu_t *)malloc(sizeof(gb_cpu_t));
    isa_data = (isa_data_t *)malloc(sizeof(isa_data_t));
    memset(isa_data, 0, sizeof(isa_data_t));
    memset(gb_cpu, 0, sizeof(gb_cpu_t));

    isa_data->opcode = 0x05;
    gb_cpu->gb_reg.BC.B = 0x00;
    //gb_cpu->gb_reg.HL.L = 0xFF;
    dec(gb_cpu);

    // uint16_t output16 = ((uint16_t)(gb_cpu->gb_reg.HL.H << 8) | (uint16_t)gb_cpu->gb_reg.HL.L);
    // printf("New value of HL is %x\n", output16);
    printf("New value of B is %x\n", gb_cpu->gb_reg.BC.B);
    printf("Flags are %x\n", gb_cpu->gb_reg.AF.F);
    return 0;
}