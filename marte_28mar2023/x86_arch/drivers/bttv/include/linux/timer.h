#ifndef __LINUX_TIMER__H
#define __LINUX_TIMER__H 

struct timer_linux_list {
  //	struct list_head entry;
  //	unsigned long expires;

  //	spinlock_t lock;
  //	unsigned long magic;

	void (*function)(unsigned long);
        unsigned long data;

  //	struct tvec_t_base_s *base;

	/* Added by Nino */
	int event_timer;
};

extern void init_linux_timer(struct timer_linux_list * timer);
/*{
	timer->event_timer = -1;
	timer->function = NULL;
	}*/

#endif
