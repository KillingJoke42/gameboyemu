#include <stdio.h>
#include <utils.h>

int main(void)
{
    game_file_t *game_data;

    // game_data = (game_file_t *)malloc(sizeof(game_file_t));

    GBEMU_PRINT(("Hello, GameBoy!\n"));
    game_data = open_gb_rom("/home/anantraina/gameboyemu/ROMS/tetris.gb");

    return 0;
}