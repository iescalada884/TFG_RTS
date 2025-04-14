#include <Arduino.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <misc/timespec_operations.h>

#define US_DELAY 500
#define US_DELAY_SHORT 40
#define MS_DELAY 1500

int main (void) {
  struct timespec ts0, ts1;
  uint32_t ms0, ms1;
  uint32_t us0, us1;
  
  printf("delays_stm32duino\n");
  
  printf("delay(%d)\n", MS_DELAY);
  ms0 = millis();
  clock_gettime(CLOCK_MONOTONIC, &ts0);
  delay(MS_DELAY);
  clock_gettime(CLOCK_MONOTONIC, &ts1);
  ms1 = millis();
  decr_timespec(&ts1, &ts0);
  printf(" Measured delay (clock_gettime):%ss\n", show_timespec_s(&ts1));
  printf(" Measured delay (millis):%dms\n", ms1 - ms0);
  
  printf("usleep(%d)\n", US_DELAY);
  us0 = micros();
  clock_gettime(CLOCK_MONOTONIC, &ts0);
  usleep(US_DELAY);
  clock_gettime(CLOCK_MONOTONIC, &ts1);
  us1 = micros();
  decr_timespec(&ts1, &ts0);
  printf(" Measured delay (clock_gettime):%ss\n", show_timespec_s(&ts1));
  printf(" Measured delay (micros):%dus\n", us1 - us0);
  
  printf("delayMicroseconds(%d)\n", US_DELAY);
  us0 = micros();
  clock_gettime(CLOCK_MONOTONIC, &ts0);
  delayMicroseconds(US_DELAY);
  clock_gettime(CLOCK_MONOTONIC, &ts1);
  us1 = micros();
  decr_timespec(&ts1, &ts0);
  printf(" Measured delay (clock_gettime):%ss\n", show_timespec_s(&ts1));
  printf(" Measured delay (micros):%dus\n", us1 - us0);
  
  printf("delayMicroseconds(%d)\n", US_DELAY_SHORT);
  us0 = micros();
  clock_gettime(CLOCK_MONOTONIC, &ts0);
  delayMicroseconds(US_DELAY_SHORT);
  clock_gettime(CLOCK_MONOTONIC, &ts1);
  us1 = micros();
  decr_timespec(&ts1, &ts0);
  printf(" Measured delay (clock_gettime):%ss\n", show_timespec_s(&ts1));
  printf(" Measured delay (micros):%dus\n", us1 - us0);

  return 0;
}
