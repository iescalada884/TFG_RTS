

/* Mutex PCP specific scheduling parameters */
struct pcp_mutex_specific_param {
  int ceiling;
};

/* PCP scheduler thread body */
void * pcp_scheduler (void *arg);
