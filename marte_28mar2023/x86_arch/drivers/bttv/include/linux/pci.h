#ifndef __LINUX_PCI__H
#define __LINUX_PCI__H

#include <sys/pci.h>
#include <compbttv.h>
#include <linux/device.h>


/* This defines the direction arg to the DMA mapping routines. */
#define PCI_DMA_BIDIRECTIONAL	0
#define PCI_DMA_TODEVICE	1
#define PCI_DMA_FROMDEVICE	2
#define PCI_DMA_NONE		3

#define DEVICE_COUNT_COMPATIBLE	4
#define DEVICE_COUNT_IRQ	2
#define DEVICE_COUNT_DMA	2
#define DEVICE_COUNT_RESOURCE	12






#define PCI_ANY_ID (~0)
extern int pci_pci_problems;
#define PCIPCI_FAIL		1
#define PCIPCI_TRITON		2
#define PCIPCI_NATOMA		4
#define PCIPCI_VIAETBF		8
#define PCIPCI_VSFX		16
#define PCIPCI_ALIMAGIK		32


struct pci_device_id {
	__u32 vendor, device;		/* Vendor and device ID or PCI_ANY_ID*/
	__u32 subvendor, subdevice;	/* Subsystem ID's or PCI_ANY_ID */
	__u32 class, class_mask;	/* (class,subclass,prog-if) triplet */
	__u32 driver_data;	/* Data private to the driver */
};

struct pci_driver {
	struct list_head node;
	char *name;
	struct pci_device_id *id_table;	/* must be non-NULL for probe to be called */
	int  (*probe)  (struct pci_device *dev, struct pci_device_id *id);	/* New device inserted */
	void (*remove) (struct pci_device *dev);	/* Device removed (NULL if not a hot-plug capable driver) */
	int  (*suspend) (struct pci_device *dev, u32 state);	/* Device suspended */
	int  (*resume) (struct pci_device *dev);	                /* Device woken up */
	int  (*enable_wake) (struct pci_device *dev, u32 state, int enable);   /* Enable wake event */

	struct device_driver	driver;
  //	struct pci_dynids dynids;
};

/*
 * a helper function which helps ensure correct pci_driver
 * setup and cleanup for commonly-encountered hotplug/modular cases
 *
 * This MUST stay in a header, as it checks for -DMODULE
 */
extern int pci_module_init(struct pci_driver *drv);

extern const char * pci_name(struct pci_device *dev);

#define pci_set_dma_mask(a,b) 0
#define pci_resource_start(dev,bar)   ((dev)->pci_region[(bar)].base_addr)
#define pci_resource_len(dev,bar)     ((dev)->pci_region[(bar)].size)

#define request_mem_region(a,b,c) !0
#define release_mem_region(a,b)
#define pci_unregister_driver(a)

/*
 *  If we set up a device for bus mastering, we need to check the latency
 *  timer as certain crappy BIOSes forget to set it properly.
 */
extern unsigned int pcibios_max_latency;

extern void pcibios_set_master(struct pci_device *dev);

/**
 * pci_set_master - enables bus-mastering for device dev
 * @dev: the PCI device to enable
 *
 * Enables bus-mastering on the device and calls pcibios_set_master()
 * to do the needed arch specific settings.
 */
extern void pci_set_master(struct pci_device *dev);


/* Similar to the helpers above, these manipulate per-pci_dev
 * driver-specific data.  They are really just a wrapper around
 * the generic device structure functions of these calls.
 */
extern void *pci_get_drvdata (struct pci_device *dev);

extern void pci_set_drvdata (struct pci_device *dev, void *data);

extern int pci_dma_supported(struct pci_device *hwdev, u64 mask);

extern void pci_free_consistent(struct pci_device *hwdev, size_t size,
				void *vaddr, dma_addr_t dma_handle);

extern void *dma_alloc_coherent(struct pci_device *dev, size_t size,
				dma_addr_t *dma_handle, int gfp);

extern void *pci_alloc_consistent(struct pci_device *hwdev, size_t size,
				  dma_addr_t *dma_handle);

#endif
