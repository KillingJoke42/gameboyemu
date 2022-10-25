#ifndef __UTILS_H__
#define __UTILS_H__

#include <isa.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#ifdef GBEMU_DBG
#define GBEMU_PRINT(x) printf x
#else
#define GBEMU_PRINT(x)
#endif

#define OFFSETOF(type, elem) (unsigned long)(&((type *)0)->elem)

typedef struct {
    uint8_t *game;
    int sz;
} game_file_t;

game_file_t *open_gb_rom(char *fpath);
void ram_init(gb_cpu_t *gb_cpu);
void reset_register_init(gb_cpu_t *gb_cpu);
void load_gb_rom(gb_cpu_t *gb_cpu, game_file_t *game_file);
void register_dump(gb_cpu_t *gb_cpu);

#endif