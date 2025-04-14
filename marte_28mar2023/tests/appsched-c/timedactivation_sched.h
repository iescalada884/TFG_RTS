#ifndef	_TIMEDACTIVATION_SCHED_H_
#define _TIMEDACTIVATION_SCHED_H_

#define TIME_MARGIN 0.001  // one millisecond

extern double expected_thread_activation_time;
extern int new_thread_invoked;

void timedactivation_sched_init(void * sched_data);

void timedactivation_sched_new_thread(void * sched_data, pthread_t thread,
				      posix_appsched_actions_t * actions);

#endif // _TIMEDACTIVATION_SCHED_H_ 


