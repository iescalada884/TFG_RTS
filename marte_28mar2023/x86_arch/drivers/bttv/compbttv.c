#include <compbttv.h>
#include <linux/device.h>
#include <linux/pci.h>
#include <linux/timer.h>
#include <debug_marte.h>

unsigned int pcibios_max_latency = 255;


/*void complete(struct completion *c) {
  }*/

void init_linux_timer(struct timer_linux_list * timer)
{
	timer->event_timer = -1;
	timer->function = NULL;
}

int pci_enable_device(struct pci_device *dev)
{
  return 0;
}

int pci_module_init(struct pci_driver *drv)
{
  int n=0;
  struct pci_device temp;

  /* initialize common driver fields */
  drv->driver.name = drv->name;
  drv->driver.bus = NULL;

  while((drv->id_table[n].vendor!=0) && (drv->id_table[n].device!=0)){
    if (pci_find_device(drv->id_table[n].vendor,drv->id_table[n].device,NULL,&temp)==0)
      break;
    n+=1;
  }
  if((drv->id_table[n].vendor==0) && (drv->id_table[n].device==0))
    return -1;
  
  if(drv->probe(&temp,drv->id_table)!=0)
    return -1;

  return 0;
}
const char * pci_name(struct pci_device *dev){

  return dev->name;
}

void pcibios_set_master(struct pci_device *dev)
{
	u8 lat;
	pci_read_config_byte(dev, PCI_LATENCY_TIMER, &lat);
	if (lat < 16)
		lat = (64 <= pcibios_max_latency) ? 64 : pcibios_max_latency;
	else if (lat > pcibios_max_latency)
		lat = pcibios_max_latency;
	else
		return;
	printk(KERN_DEBUG "PCI: Setting latency timer of device %s to %d\n", pci_name(dev), lat);
	pci_write_config_byte(dev, PCI_LATENCY_TIMER, lat);
}

void pci_set_master(struct pci_device *dev)
{
	u16 cmd;

	pci_read_config_word(dev, PCI_COMMAND, &cmd);
	if (! (cmd & PCI_COMMAND_MASTER)) {
		printk("PCI: Enabling bus mastering for device %s\n", pci_name(dev));
		cmd |= PCI_COMMAND_MASTER;
		pci_write_config_word(dev, PCI_COMMAND, cmd);
	}
	pcibios_set_master(dev);
}

void * private_data;

void *pci_get_drvdata (struct pci_device *dev)
{
  return private_data;
}

void pci_set_drvdata (struct pci_device *dev, void *data)
{
	private_data=data;
}

int pci_dma_supported(struct pci_device *hwdev, u64 mask)
{
  return 1;
}

void pci_free_consistent(struct pci_device *hwdev, size_t size,
		    void *vaddr, dma_addr_t dma_handle)
{
  //	free((void *)dma_handle);
}

void *pci_alloc_consistent(struct pci_device *hwdev, size_t size,
		     dma_addr_t *dma_handle)
{
	return dma_alloc_coherent(hwdev == NULL ? NULL : hwdev, size, dma_handle, GFP_ATOMIC);
}


/*********************************************************************/

void *kmalloc(size_t size, int flags)
{
	return malloc(size);
}

void kfree(void * x)
{
  free(x);
}



void *vmalloc(unsigned long size) {

  return malloc(size);

}

void * vmalloc_32(size_t size)
{
        void *mem;
        unsigned long diff;
                                                                                                                             
        mem = malloc(size+12);
                                                                                                                             
        diff = (unsigned long)((((unsigned long)mem/4)+1)*4-(unsigned long)mem);
                                                                                                                             
        *(unsigned long *)(mem+diff) = (diff | 0x80000000);

        return (mem+diff+4);
                                                                                                                             
}

void vfree(void *addr)
{
  if (addr == NULL || *(unsigned long *)(addr-4) == 0) return;

  if ((*(unsigned long *)(addr-4) & 0x80000000) == 0x80000000) {
	free(addr-(*(unsigned long *)(addr-4) & 0x7FFFFFFF)-4);
        *(unsigned long *)(addr-4) = 0;
	return;
  }

  free(addr);

  return;
}


void *dma_alloc_coherent(struct pci_device *dev, size_t size,
                           dma_addr_t *dma_handle, int gfp)
{
        void *ret;
	//  set_break_point_here;

        ret = (void *)vmalloc_32(size);

        if (ret != NULL) {
                memset(ret, 0, size);
                *dma_handle = (dma_addr_t)ret;
        }
        return ret;
}


unsigned long long read_time(void)
{
  unsigned long long ret;
  struct timespec time;

  clock_gettime(CLOCK_MONOTONIC,&time);

  ret=(unsigned long long)time.tv_nsec+1000000000; /*nanosecs*/

  ret=ret/1000; /*microsecs*/

  return ret;
};
