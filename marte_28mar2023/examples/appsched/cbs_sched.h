#include <time.h>

#define MAX_THREADS 30

#define NEW_JOB_SIGNAL       (SIGRTMIN+1)

struct cbs_sched_param {
  struct timespec deadline;
  struct timespec period;
  int cbs;
  struct timespec mx_budget;
};


void *edf_scheduler (void *arg);
