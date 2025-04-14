// MaRTE OS
// Simple LED tests
//
// Uses the 'stm32cube_fw_f7' library:
//   https://www.st.com/en/embedded-software/stm32cubef7.html
// See header files in MaRTE OS directory:
// 'stm32f_arch/hwi/stm32cube_stm32f769disco/Drivers/BSP/STM32F769I-Discovery/'

#include <stdio.h>
#include <unistd.h>
#include "stm32f7xx_hal.h"
#include "stm32f769i_discovery.h"

int main(void) {
  BSP_LED_Init(LED1);
  BSP_LED_Init(LED2);

  while (1) {
    sleep(1);
    printf("Toggle\n");
    BSP_LED_Toggle(LED2);
    BSP_LED_Toggle(LED1);
  }

  return 0;
}
