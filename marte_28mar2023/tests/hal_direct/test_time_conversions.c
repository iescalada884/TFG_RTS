//  Test for all architectures
/*
 * test_time_conversions.c
 *
 * Test the conversions from timespec/Duration to HWTime and vice versa.
 *
 */

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>

#include "marte_hal_for_tests.h"

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define NS_PER_S 1000000000

// timespec conversions
hw_time_t marte__timespec__timespec_to_hwtime(struct timespec *ts);
struct timespec marte__timespec__hwtime_to_timespec(hw_time_t hwt);

// suspension time minimun
extern hw_time_t marte__kernel__suspension_time_minimum;

# define MARGIN_HWT 1

#if MARTE_ARCHITECTURE == ARCH_STM32F
# define CPU_FREQ 168000000
# define HWT_HZ 10000
# define MARGIN_S (2.0 * 1.0 / HWT_HZ)

#else
# define MARGIN_S 0.00001
#endif

int are_equal_ts_hwt(struct timespec ts, hw_time_t hwt, double margin_s) {
  double ts_double_s = ts.tv_sec + (double) ts.tv_nsec / NS_PER_S;
  double hwt_double_s = (double) hwt / HWT_HZ;
  double dif = ts_double_s - hwt_double_s;
  if (dif < 0) {
    dif = -dif;
  }
  
  return dif < margin_s;
}

int are_equal_hwt(hw_time_t hwt0, hw_time_t hwt1, hw_time_t margin) {
  hw_time_t dif = hwt0 > hwt1 ?  hwt0 - hwt1 : hwt1 - hwt0;
  return dif < margin;
}


int main () { 
#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif
  
  // test clock and CPU freqs
  {
    hw_time_t hwclock_frequency;
    hw_time_t cpu_frequency;
    hwclock_frequency = marte__hal__get_hwclock_frequency();
    cpu_frequency = marte__hal__cpu_frequency(); 
    
    printf("HW clock freq:%u\n", (unsigned)hwclock_frequency);
    printf("CPU freq:%u\n", (unsigned)cpu_frequency);
    printf("Suspension HWTime minimun: %u\n", (unsigned) marte__kernel__suspension_time_minimum);
    assert(hwclock_frequency > 0);
    assert(cpu_frequency > 0);
#if MARTE_ARCHITECTURE != ARCH_STM32F    
    assert(marte__kernel__suspension_time_minimum > 0);
#endif
  
#if MARTE_ARCHITECTURE == ARCH_STM32F
    assert(cpu_frequency == CPU_FREQ);
    assert(cpu_frequency > hwclock_frequency);
#endif
  }
  
  // Tests conversions to timespec
  printf("\nConversiones timespec <-> hwtime:\n");
  {
    hw_time_t hwts[] = {0, 10, 123, 4567, 10000, 234567};
    int hwts_size = 6;
    struct timespec ts;
    hw_time_t hwt;
    
    for(int i=0; i<hwts_size; i++) {      
      ts = marte__timespec__hwtime_to_timespec(hwts[i]);      
      hwt = marte__timespec__timespec_to_hwtime(&ts);
      printf(" hwt0=%6d  =>  ts=%2ds%9uns  =>  hwt1=%6d\n", (int) hwts[i], ts.tv_sec, ts.tv_nsec, (int) hwt);
      
      assert(are_equal_ts_hwt(ts, hwts[i], MARGIN_S));
      assert(are_equal_hwt(hwts[i], hwt, MARGIN_HWT));     
    }
  }
    
  
  // Tests conversions to duration
  //{
  //  hw_time_t hwt;
  //  duration_t dur;
  //}

  printf("Test OK\n");
  return 0;
}
