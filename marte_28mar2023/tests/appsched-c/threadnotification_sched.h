#ifndef	_THREADNOTIFICATION_SCHED_H_
#define _THREADNOTIFICATION_SCHED_H_

#define TIME_MARGIN 0.001  // one millisecond

extern double expected_thread_execution_time;
extern int thread_notification_invoked;

void threadnotification_sched_init(void * sched_data);

void threadnotification_sched_new_thread(void * sched_data, pthread_t thread,
					 posix_appsched_actions_t * actions);

void threadnotification_sched_notification_for_thread
   (void * sched_data, pthread_t thread,
    posix_appsched_actions_t * actions);

#endif // _THREADNOTIFICATION_SCHED_H_ 
