#include <utils.h>
#include <memorymap.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

game_file_t *open_gb_rom(char *fpath)
{
    FILE *rom;
    game_file_t *game_data;
    int fread_err, pos;

    game_data = (game_file_t *)malloc(sizeof(game_file_t));

    rom = fopen(fpath, "rb");
    if (rom == NULL)
    {
        GBEMU_PRINT(("Unable to open ROM: provided file is not a GB ROM\n"));
        return NULL;
    }
    
    fseek(rom, 0, SEEK_END);
    game_data->sz = ftell(rom);
    GBEMU_PRINT(("Size of ROM is: %d\n", game_data->sz));
    rewind(rom);

    game_data->game = (uint8_t *)malloc(sizeof(uint8_t) * game_data->sz);

    for (pos = 0; pos < game_data->sz; pos++)
    {
        // fread_err = fread(game_data->game, sizeof(uint8_t), game_data->sz, rom);
        // if (fread_err != (game_data->sz / sizeof(char)))
        // {
        //     GBEMU_PRINT(("Error: Unable to read rom. Error occurred or reached EOF\n"));
        //     fclose(rom);
        //     return NULL;
        // }
        fread(&game_data->game[pos], 1, 1, rom);
    }

    fclose(rom);
    return game_data;
}

void ram_init(gb_cpu_t *gb_cpu)
{
    int i;

    for (i = 0; i < GB_TOTAL_ADDRESSABLE_MEMORY; i++)
        gb_cpu->gb_mem[i] = 0x00;
}

void reset_register_init(gb_cpu_t *gb_cpu)
{
    ram_init(gb_cpu);

    gb_cpu->gb_reg.AF.A = 0x01;
    gb_cpu->gb_reg.AF.F = 0xB0;

    gb_cpu->gb_reg.BC.B = 0x00;
    gb_cpu->gb_reg.BC.C = 0x13;

    gb_cpu->gb_reg.DE.D = 0x00;
    gb_cpu->gb_reg.DE.E = 0xD8;

    gb_cpu->gb_reg.HL.H = 0x01;
    gb_cpu->gb_reg.HL.L = 0x4D;

    gb_cpu->gb_reg.SP = 0xFFFE;
    gb_cpu->gb_reg.PC = GB_GAME_ROM_BASE_ADDR;
    gb_cpu->ime = 0x00;

    for (int i = 0; i < POWERUP_STACK_ADDR_CNT; i++)
        mem_write(gb_cpu, powerup_stack_addrs[i], powerup_stack_init[i]);

    // Comment this later!!!!
    mem_write(gb_cpu, 0xFF44, 0x90);
}

void load_gb_rom(gb_cpu_t *gb_cpu, game_file_t *game_file)
{
    int i;

    for (i = 0; i < game_file->sz; i++)
    {
        gb_cpu->gb_mem[i] = \
        game_file->game[i];
    }
    return;
}

void register_dump(gb_cpu_t *gb_cpu)
{
    uint16_t af, bc, de, hl;
    /*
        af = (uint16_t)(gb_cpu->gb_reg.AF.A << 8) | \
            (uint16_t)(gb_cpu->gb_reg.AF.F);
        bc = (uint16_t)(gb_cpu->gb_reg.BC.B << 8) | \
            (uint16_t)(gb_cpu->gb_reg.BC.C);
        de = (uint16_t)(gb_cpu->gb_reg.DE.D << 8) | \
            (uint16_t)(gb_cpu->gb_reg.DE.E);
        hl = (uint16_t)(gb_cpu->gb_reg.HL.H << 8) | \
            (uint16_t)(gb_cpu->gb_reg.HL.L);
    */
    
    GBEMU_PRINT(("A: %02X F: %02X B: %02X C: %02X D: %02X E: %02X H: %02X L: %02X SP: %04X PC: 00:%04X (%02X %02X %02X %02X)\n", 
                    gb_cpu->gb_reg.AF.A, gb_cpu->gb_reg.AF.F, gb_cpu->gb_reg.BC.B, gb_cpu->gb_reg.BC.C,
                    gb_cpu->gb_reg.DE.D, gb_cpu->gb_reg.DE.E, gb_cpu->gb_reg.HL.H, gb_cpu->gb_reg.HL.L,
                    gb_cpu->gb_reg.SP, gb_cpu->gb_reg.PC,
                    gb_cpu->gb_mem[gb_cpu->gb_reg.PC], gb_cpu->gb_mem[gb_cpu->gb_reg.PC+1],
                    gb_cpu->gb_mem[gb_cpu->gb_reg.PC+2], gb_cpu->gb_mem[gb_cpu->gb_reg.PC+3]));
    return;
}