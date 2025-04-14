
#ifdef MaRTE_stm32f4
# include "ARMCM4_FP.h"
#elif defined(MaRTE_stm32f769disco)
# include "ARMCM7_DP.h"
#else
# error "Unexpected target"
#endif

void marte_hal_init_timer (uint32_t period_in_ticks)
{
  SysTick_Config(period_in_ticks);
}
