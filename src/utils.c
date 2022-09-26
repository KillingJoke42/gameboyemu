#include <utils.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

game_file_t *open_gb_rom(char *fpath)
{
    FILE *rom;
    // uint8_t *rbuffer;
    // int sz;
    int fread_err;
    game_file_t *game_data;

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
    fseek(rom, 0, SEEK_SET);

    game_data->game = (uint8_t *)malloc(sizeof(uint8_t) * game_data->sz);

    // rbuffer = (uint8_t *)malloc(sizeof(uint8_t) * sz);
    fread_err = fread(game_data->game, sizeof(uint8_t), game_data->sz, rom);
    if (fread_err != (game_data->sz / sizeof(char)))
    {
        GBEMU_PRINT(("Error: Unable to read rom. Error occurred or reached EOF\n"));
        fclose(rom);
        return NULL;
    }

    fclose(rom);
    return game_data;
}