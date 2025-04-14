#ifndef _LINUX_FS_H
#define _LINUX_FS_H

#include <compbttv.h>
#include <linux/list.h>
//#include <linux/spinlock.h>

#define READ 0
#define WRITE 1


struct inode;

struct file {
	struct list_head	f_list;
  //	struct dentry		*f_dentry;
  //	struct vfsmount         *f_vfsmnt;
  //	struct file_operations	*f_op;
  //	atomic_t		f_count;
	unsigned int 		f_flags;
  //	mode_t			f_mode;
	loff_t			f_pos;
  //	struct fown_struct	f_owner;
	unsigned int		f_uid, f_gid;
	int			f_error;
  //	struct file_ra_state	f_ra;

	unsigned long		f_version;
	void			*f_security;

	/* needed for tty driver, and maybe others */
	void			*private_data;

	/* Used by fs/eventpoll.c to link all the hooks to this file */
	struct list_head	f_ep_links;
  //	spinlock_t		f_ep_lock;
};

struct file_operations {
	int (*open) (int);
	int (*release) (int);
	int (*ioctl) (int, unsigned int, void *);
	ssize_t (*read) (int, char *, size_t, loff_t *);
	int (*mmap) (int, struct vm_area_struct *);
	ssize_t (*write) (int, const char *, size_t, loff_t *);
};

struct inode { 
  void * inode;
  
}; 

#endif /* _LINUX_FS_H */
