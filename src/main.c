#include <stdio.h>
#include <gbcpu.h>
#include <isa.h>
#include <utils.h>
#include <string.h>
#include <timer.h>
#include <memorymap.h>

void Update(gb_cpu_t *gb_cpu)
{
    uint32_t cyclesThisFrame = 0;
    uint8_t cycles;

    while (cyclesThisFrame < MAXCYCLES)
    {
#ifdef GBEMU_DBG
        //register_dump(gb_cpu);
#endif
        cycles = isa[gb_cpu->gb_mem[gb_cpu->gb_reg.PC]](gb_cpu);
        cyclesThisFrame += cycles;
        UpdateTimers(gb_cpu, cycles);
        processInterrupts(gb_cpu);
    }

    return;
}

#ifdef GBEMU_DBG
void SingleInstrExec(gb_cpu_t *gb_cpu)
{
    uint32_t maxInst = 161502;
    uint32_t instrNum = 0;
    uint8_t cycles;

    while (instrNum++ != maxInst)
    {
#if defined(GBEMU_DBG) && defined(REGDUMP)
        register_dump(gb_cpu);
#endif
        cycles = isa[gb_cpu->gb_mem[gb_cpu->gb_reg.PC]](gb_cpu);
        UpdateTimers(gb_cpu, cycles);
        processInterrupts(gb_cpu);
    }

    return;
}
#endif

int main(void)
{
    gb_cpu_t *gb_cpu;
    game_file_t *game_data;

    gb_cpu = (gb_cpu_t *)malloc(sizeof(gb_cpu_t));
    gb_cpu->gb_mem = (uint8_t *)malloc(sizeof(uint8_t) * (GB_TOTAL_ADDRESSABLE_MEMORY + 1));
    gb_cpu->timerCounter = 1024;
    gb_cpu->dividerCounter = 0;

    reset_register_init(gb_cpu);

    game_data = open_gb_rom("/home/anantraina/gameboyemu/ROMS/gb-test-roms/cpu_instrs/individual/02-interrupts.gb");
    load_gb_rom(gb_cpu, game_data);
    // while (1)
    //     Update(gb_cpu);
    SingleInstrExec(gb_cpu);

    free(game_data);

    free(gb_cpu->gb_mem);
    free(gb_cpu);
    return 0;
}