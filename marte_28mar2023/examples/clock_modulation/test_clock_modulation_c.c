/*!
 * @file test_clock_modulation.c
 *
 * @brief simple test for the clock modulation
 *
 * This file contains a basic test for the clock modulation
 *
 * NOTE: clock modulation is used to change the CPU frequency
 *
 * @version 0.1
 *
 * @date 20-May-2009
 *
 * @author Daniel Medina <medinad@unican.es>
 *
 */

#include <stdio.h>
#include <sys/clock_modulation.h>

extern void eat (float for_seconds);
extern void adjust (void);
extern unsigned long long marte__hal__tsc__read_tsc();

int main () {

  unsigned long long tsc1, tsc2, full_tsc=0;
  int perf, i=0, ret;

  if ( clock_modulation_is_supported () == 0 ) { 
    printf("Error: clock_modulation NOT supported\n");
    return -1;
  }

  adjust();
  ret = clock_modulation_calibrate();
  if ( ret ) {
    printf("Error: Not adjusted\n");
    return -1;
  }
  printf ("adjusted\n");
  printf ("        Dutycycle            tsc \n");
  printf (" N|ratio| level |   tsc   | ratio\n");

  for ( perf = 0x20 ; perf > 0x11; perf -= 2) {
    i = 0;
    if ( perf > 0x1F ) {
      ret = clock_modulation_set_dutycyclelevel(CLOCK_MODULATION_DUTYCYCLE_100);
      if ( ret != 0 ) {
        printf("Error: set_dutycyclelevel 100. %i\n", ret);
      }
    } else {
      ret = clock_modulation_set_dutycyclelevel(perf);
      if ( ret != 0 ){
        printf("Error: set_dutycyclelevel. %i\n", ret);
      }
    }
    printf("%x", perf );

    tsc1 =marte__hal__tsc__read_tsc();
    eat(0.1);
    tsc2 =marte__hal__tsc__read_tsc();

    if (perf == 0x20) {full_tsc = tsc2 - tsc1; }

    printf("|%.3f| %x\t", clock_modulation_get_dutycycle(perf) , clock_modulation_get_dutycyclelevel() );
    printf(" %llu| %.3f", tsc2-tsc1, (float)full_tsc/(float)(tsc2-tsc1));
    printf("\n");

  }

  ret = clock_modulation_set_dutycyclelevel(CLOCK_MODULATION_DUTYCYCLE_100);
  if (ret != 0 ) {
    printf("error 3. %i\n", ret);
  }
  return 0;
}

