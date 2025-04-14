// MaRTE OS
// Simple test of the touch screen
//
// Uses the 'stm32cube_fw_f7' library:
//   https://www.st.com/en/embedded-software/stm32cubef7.html
// See header files in MaRTE OS directory:
// 'stm32f_arch/hwi/stm32cube_stm32f769disco/Drivers/BSP/STM32F769I-Discovery/'

#include <stdio.h>
#include <unistd.h>
#include "stm32f7xx_hal.h"
#include "stm32f769i_discovery.h"
#include "stm32f769i_discovery_lcd.h"
#include "stm32f769i_discovery_ts.h"

TS_StateTypeDef TS_State;

int main (void) {
  printf("Touch demo\n");
  
  if (BSP_TS_Init(BSP_LCD_GetXSize(), BSP_LCD_GetYSize())) {
    printf("Error: BSP_TS_Init\n");
  }
  
  while(1) {
    if (BSP_TS_GetState(&TS_State)) {
      printf("Error: BSP_TS_GetState\n");
    }
    
    //printf("Touch:%d(%d, %d)\n", (int)TS_State.touchDetected,
    //       (int)TS_State.touchX[0], (int)TS_State.touchY[0]);
    
    if (TS_State.touchDetected) {
      BSP_LCD_FillRect(TS_State.touchX[0]-2, TS_State.touchY[0]-2, 4, 4);
    }
    
    usleep(100);
  }

  return 0;
}
