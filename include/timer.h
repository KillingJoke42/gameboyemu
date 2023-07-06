#ifndef __TIMER_H__
#define __TIMER_H__

#include <stdint.h>
#include <gbcpu.h>
#include <memorymap.h>

#define DIV         0xFF04
#define TIMA        0xFF05
#define TMA         0xFF06
#define TAC         0xFF07

static int gb_clk_spd[] = {1024, 16, 64, 256};

#define IS_CLOCK_ENABLED(gb_cpu_ptr)        ((mem_read(gb_cpu_ptr, TAC) & 0x04))
#define GET_CLOCK_FREQ(gb_cpu_ptr)          ((mem_read(gb_cpu_ptr, TAC) & 0x03))
#define SET_CLOCK_FREQ(gb_cpu_ptr)          (gb_cpu_ptr->timerCounter = \
                                            (gb_clk_spd[GET_CLOCK_FREQ(gb_cpu_ptr)]))

void UpdateTimers(gb_cpu_t *gb_cpu, uint8_t cycles);

#endif