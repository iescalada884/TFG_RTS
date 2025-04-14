#ifndef _LINUX_WAIT_H
#define _LINUX_WAIT_H

#include <linux/list.h>
#include <linux/stddef.h>
//#include <linux/spinlock.h>

#define FASTCALL(x) x

struct __wait_queue {
	unsigned int flags;
};

typedef struct __wait_queue wait_queue_t;

struct __wait_queue_head {
  //	spinlock_t lock;
	struct list_head task_list;
};
typedef struct __wait_queue_head wait_queue_head_t;

#endif
