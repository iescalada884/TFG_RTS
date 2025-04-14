/*
 * device.h - generic, centralized driver model
 *
 * Copyright (c) 2001-2003 Patrick Mochel <mochel@osdl.org>
 *
 * This file is released under the GPLv2
 *
 * See Documentation/driver-model/ for more information.
 */

#ifndef _LINUX_DEVICE_H_
#define _LINUX_DEVICE_H_

#include <compbttv.h>
#include <linux/list.h>
#include <asm/semaphore.h>

#define BUS_ID_SIZE 20

struct device;
struct device_driver;
struct class;
struct class_device;

struct bus_type {
	char			* name;

  //	struct subsystem	subsys;
  //	struct kset		drivers;
  //	struct kset		devices;

	int		(*match)(struct device * dev, struct device_driver * drv);
	struct device * (*add)	(struct device * parent, char * bus_id);
	int		(*hotplug) (struct device *dev, char **envp, 
				    int num_envp, char *buffer, int buffer_size);
	int		(*suspend)(struct device * dev, u32 state);
	int		(*resume)(struct device * dev);
};

struct device_driver {
	char			* name;
	struct bus_type		* bus;

	sem_t	                unload_sem;
  //	struct kobject		kobj;
	struct list_head	devices;

	int	(*probe)	(struct device * dev);
	int 	(*remove)	(struct device * dev);
	void	(*shutdown)	(struct device * dev);
	int	(*suspend)	(struct device * dev, u32 state, u32 level);
	int	(*resume)	(struct device * dev, u32 level);
};

struct class {
	char			* name;

  //	struct subsystem	subsys;
	struct list_head	children;
	struct list_head	interfaces;

	int	(*hotplug)(struct class_device *dev, char **envp, 
			   int num_envp, char *buffer, int buffer_size);

	void	(*release)(struct class_device *dev);
};

struct class_device {
	struct list_head	node;

  //	struct kobject		kobj;
	struct class		* class;	/* required */
	struct device		* dev;		/* not necessary, but nice to have */
	void			* class_data;	/* class-specific data */

	char	class_id[BUS_ID_SIZE];	/* unique to this class */
};

struct device {
	struct list_head node;		/* node in sibling list */
	struct list_head bus_list;	/* node in bus's list */
	struct list_head driver_list;
	struct list_head children;
	struct device 	* parent;

  	struct completion * complete;	/* Notification for freeing device. */
  //	struct kobject kobj;
	char	bus_id[BUS_ID_SIZE];	/* position on parent bus */

	struct bus_type	* bus;		/* type of bus device is on */
	struct device_driver *driver;	/* which driver has allocated this
					   device */
	void		*driver_data;	/* data private to the driver */
	void		*platform_data;	/* Platform specific data (e.g. ACPI,
					   BIOS data relevant to device) */
  //	struct dev_pm_info	power;
	u32		power_state;	/* Current operating state. In
					   ACPI-speak, this is D0-D3, D0
					   being fully functional, and D3
					   being off. */

	unsigned char *saved_state;	/* saved device state */
	u32		detach_state;	/* State to enter when device is
					   detached from its driver. */

	u64		*dma_mask;	/* dma mask (if dma'able device) */

	void	(*release)(struct device * dev);
};

static inline void *
dev_get_drvdata (struct device *dev)
{
	return dev->driver_data;
}

static inline void
dev_set_drvdata (struct device *dev, void *data)
{
	dev->driver_data = data;
}

extern struct device legacy_bus;

#define class_device_register(a) 
#define device_create_file(a,b) 
#define class_device_register(a) 
#define device_register(a) 

#define class_device_unregister(a)
#define device_remove_file(a,b)
#define device_unregister(a)

#define driver_register(a) 0
#define driver_unregister(a)

#define bus_register(a) 0
#define bus_unregister(a)

#define class_register(a) 0
#define class_unregister(a) 

/* driverfs interface for exporting device attributes */
struct attribute {
	char			* name;
  //	struct module 		* owner;
	mode_t			mode;
};
struct device_attribute {
	struct attribute	attr;
	ssize_t (*show)(struct device * dev, char * buf);
	ssize_t (*store)(struct device * dev, const char * buf, size_t count);
};

#define DEVICE_ATTR(_name,_mode,_show,_store) \
struct device_attribute dev_attr_##_name = { 		\
	.attr = {.name = __stringify(_name), .mode = _mode},	\
	.show	= _show,				\
	.store	= _store,				\
};

/* debugging and troubleshooting/diagnostic helpers. */
struct class_device_attribute {
	struct attribute	attr;
	ssize_t (*show)(struct class_device *, char * buf);
	ssize_t (*store)(struct class_device *, const char * buf, size_t count);
};

#define CLASS_DEVICE_ATTR(_name,_mode,_show,_store)		\
struct class_device_attribute class_device_attr_##_name = { 	\
	.attr = {.name = __stringify(_name), .mode = _mode },	\
	.show	= _show,					\
	.store	= _store,					\
};


#define dev_printk(level, dev, format, arg...)	\
	printk(level "%s %s: " format , (dev)->driver->name , (dev)->bus_id , ## arg)

#ifdef DEBUG
#define dev_dbg(dev, format, arg...)		\
	dev_printk(KERN_DEBUG , dev , format , ## arg)
#else
#define dev_dbg(dev, format, arg...) do {} while (0)
#endif

#define dev_err(dev, format, arg...)		\
	dev_printk(KERN_ERR , dev , format , ## arg)
#define dev_info(dev, format, arg...)		\
	dev_printk(KERN_INFO , dev , format , ## arg)
#define dev_warn(dev, format, arg...)		\
	dev_printk(KERN_WARNING , dev , format , ## arg)


#endif /* _DEVICE_H_ */
