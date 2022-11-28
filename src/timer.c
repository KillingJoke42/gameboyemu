#include <timer.h>
#include <stdint.h>

void UpdateDividerRegister(gb_cpu_t *gb_cpu, uint8_t cycles)
{
    gb_cpu->dividerCounter += cycles;

    if (gb_cpu->dividerCounter >= 255)
    {
        gb_cpu->dividerCounter = 0;
        gb_cpu->gb_mem[DIV]++;
    }
}

void UpdateTimers(gb_cpu_t *gb_cpu, uint8_t cycles)
{
    UpdateDividerRegister(gb_cpu, cycles);

    if (IS_CLOCK_ENABLED(gb_cpu))
    {
        gb_cpu->timerCounter -= cycles;

        if (gb_cpu->timerCounter <= 0)
        {
            SET_CLOCK_FREQ(gb_cpu);

            if (mem_read(gb_cpu, TIMA) == 255)
            {
                mem_write(gb_cpu, TIMA, mem_read(gb_cpu, TMA));
                mem_write(gb_cpu, GB_INTR_FLAG_REG, (mem_read(gb_cpu, GB_INTR_FLAG_REG) | 0x04));
            }
            else
            {
                mem_write(gb_cpu, TIMA, (mem_read(gb_cpu, TIMA) + 1));
            }
        }
    }
}