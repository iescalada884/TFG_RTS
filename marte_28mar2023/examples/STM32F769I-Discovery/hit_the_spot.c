// MaRTE OS
// Simple game to "hit the spot" that appears in the screen.
//
// Uses the 'stm32cube_fw_f7' library:
//   https://www.st.com/en/embedded-software/stm32cubef7.html
// See header files in MaRTE OS directory:
// 'stm32f_arch/hwi/stm32cube_stm32f769disco/Drivers/BSP/STM32F769I-Discovery/'

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <pthread.h>
#include "stm32f7xx_hal.h"
#include "stm32f769i_discovery.h"
#include "stm32f769i_discovery_lcd.h"
#include "stm32f769i_discovery_ts.h"

// radius of spots to hit
#define SPOT_RADIUS 30

// radius of the spots for the hit animation
#define SPOT_RADIUS_BIG (SPOT_RADIUS * 1.5)

// get a random point in range:
//  X -> [SPOT_RADIUS_BIG .. x_rand_max + SPOT_RADIUS_BIG - 1]
//  Y -> [SPOT_RADIUS_BIG .. y_rand_max + SPOT_RADIUS_BIG - 1]
Point get_rand_point(int x_rand_max, int y_rand_max) {
  Point point;
  point.X = rand() % x_rand_max + SPOT_RADIUS_BIG;
  point.Y = rand() % y_rand_max  + SPOT_RADIUS_BIG;
  return point;
}

// Draw circle with the desired color
void draw_circle(Point center, uint16_t radius, uint32_t color) {
  BSP_LCD_SetTextColor(color);
  BSP_LCD_FillCircle(center.X, center.Y, radius);
}
  
#define MISSES_MAX 10
#define SPOT_BASE_COLOR 0xff000000
#define TOUCH_DETECT_TRIES_INIT 4000
const struct timespec touch_detect_ts = {0, 100000000}; 

int main (void) {
  const int X_Max = BSP_LCD_GetXSize();
  const int Y_Max = BSP_LCD_GetYSize();
  const int x_rand_max = X_Max - SPOT_RADIUS_BIG * 2;
  const int y_rand_max = Y_Max - SPOT_RADIUS_BIG * 2;
  
  BSP_LCD_SetFont(&Font24);
  
  if (BSP_TS_Init(X_Max, Y_Max)) {
    printf("Error: BSP_TS_Init\n");
    return -1;
  }
  
  // games loop
  while(1) {
    BSP_LCD_DisplayStringAt(X_Max/2, Y_Max/2,
                            (unsigned char *) "Hit the spot!",
                            LEFT_MODE); 
    sleep(1);
    BSP_LCD_Clear(LCD_COLOR_WHITE);
    
    int misses_number = 0;
    int hits_number = 0;
    int spots_counter = 0;
    uint32_t spot_color = SPOT_BASE_COLOR + 0xffff;
  
    // start new game
    while(misses_number < MISSES_MAX) {
      // draw the spot at a random location
      Point point = get_rand_point(x_rand_max, y_rand_max);
      draw_circle(point, SPOT_RADIUS, spot_color);

      
      // the number of tries is reduced as the spot counter increases
      const int detect_tries = TOUCH_DETECT_TRIES_INIT - 
      spots_counter * TOUCH_DETECT_TRIES_INIT / 70;
      
      // give time to the player to touch the screen on the spot    
      int i = 0;
      int hit = 0;
      while(i < detect_tries && !hit) {
        TS_StateTypeDef TS_State;
        BSP_TS_GetState(&TS_State);
        if (TS_State.touchDetected &&
            abs(TS_State.touchX[0] - point.X) <= SPOT_RADIUS &&
            abs(TS_State.touchY[0] - point.Y) <= SPOT_RADIUS) {
            hit = 1;
          }
        i++;
      }
    
      if (hit) {
        hits_number++;
        
        // draw hit animation
        draw_circle(point, SPOT_RADIUS_BIG, LCD_COLOR_ORANGE);   
        nanosleep(&touch_detect_ts, NULL);    
        draw_circle(point, SPOT_RADIUS_BIG, LCD_COLOR_WHITE);   
        nanosleep(&touch_detect_ts, NULL);           
        draw_circle(point, SPOT_RADIUS_BIG, LCD_COLOR_ORANGE);   
        nanosleep(&touch_detect_ts, NULL); 
        
      } else {
        // miss
        misses_number++;
        
        // spot gets darker as the number of misses increases
        int color_change = 0xFF * (MISSES_MAX - misses_number) / MISSES_MAX;
        spot_color = SPOT_BASE_COLOR + (color_change<<8) + color_change;
      }
    
      // erase the spot
      draw_circle(point, SPOT_RADIUS_BIG, LCD_COLOR_WHITE); 
     
      // show current number of hits at the center of the screen
      BSP_LCD_SetTextColor(LCD_COLOR_DARKMAGENTA); 
      char hits_number_str[6];   
      snprintf(hits_number_str, 6, "%d", hits_number);
      BSP_LCD_DisplayStringAt(X_Max/2, Y_Max/2,
                              (unsigned char *) hits_number_str,
                              LEFT_MODE);
      spots_counter++;
    }
  
    // end of game: write final hit score 
    char hits_number_str[16];   
    snprintf(hits_number_str, 16, "Hits:%d", hits_number);
    BSP_LCD_DisplayStringAt(X_Max/2, Y_Max/2,
                            (unsigned char *) hits_number_str,
                            LEFT_MODE);
    sleep(4);
  }

  return 0;
}
