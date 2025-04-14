#include <time.h>

struct edf_sched_param {
  struct timespec period;
};

void *edf_scheduler (void *arg);
