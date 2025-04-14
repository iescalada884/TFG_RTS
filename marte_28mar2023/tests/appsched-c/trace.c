
#include <stdio.h>
#include <time.h>
#include <misc/timespec_operations.h>

// initial time
double init_time;


void trace_set_initial_time() {
  struct timespec current_time;
  clock_gettime (CLOCK_MONOTONIC, &current_time);

  init_time = timespec_to_double(&current_time);
  printf("Initial time: %f\n", init_time);
}


void trace_show_current_time(char * msj) {
  struct timespec current_time;
  clock_gettime (CLOCK_MONOTONIC, &current_time);
	
  printf("%s: %f\n", msj,
	 timespec_to_double(&current_time) - init_time);
}

void trace_show_time(char * msj, const struct timespec *ts) {
  printf("%s: %f\n", msj,
	 timespec_to_double(ts) - init_time);
}

