#include <stdio.h>
#include <sys/pci.h>	
#include "svgalib_helper.h"
#include "interrupt.h"
#include "libvga.h"
#include <stdint.h>


static struct sh_pci_device graph_dev;
static struct pci_device dev;


/*MaRTE OS*/
/*Retocada la función casi del todo*/
int init_vgapci(unsigned int vendor,unsigned int device)
{
    int i;
    int ret;
    uint32_t result;

    printf("Initializing PCI BUS...\n");
    
    memset(&graph_dev,0,sizeof(struct sh_pci_device));

    /*Inicio la búsqueda de la tarjeta*/
    ret=pci_find_device(vendor,device,NULL,&dev);
    
    if (ret==-1){
      printf("Driver no localizado");
      return -1;
    }
	    
    memcpy(&(graph_dev.dev),&dev,sizeof(struct pci_device));
    graph_dev.vendor=dev.vendor;
    graph_dev.id=dev.dev_id;
	    
    printf("VGA Vendor:%.4x id:%.4x bus:%d dev:%d\n",\
    graph_dev.vendor,graph_dev.id,dev.bus,dev.devfn);

    for(i=0;i<6;i++){
	uint32_t t, len;
	pci_read_config_dword(&dev,16+4*i,&result);
	if(result) {
		pci_write_config_dword(&dev,16+4*i,0xffffffff);
		pci_read_config_dword(&dev,16+4*i,&t);
		pci_write_config_dword(&dev,16+4*i,result);
		len = ~(t&~0xf)+1;
		if(len) {
		  graph_dev.mem[i]=dev.pci_region[i].base_addr;
		  graph_dev.flags[i]=(uint32_t) (0x80 | (result&0xf));
                  graph_dev.len[i]=len;
		  graph_dev.mask[i]=t&~0xf;
		  printf("region=%d base=%lx len=%ld mask=%lx \n",i,graph_dev.mem[i],graph_dev.len[i],graph_dev.mask[i]);
		}
	}
    }
    vga_init_vsync(&graph_dev);

    return 0;

}

uint32_t __svgalib_pci_read_config_dword(int pos, int address)
{
    uint32_t t;

    pci_read_config_dword(&dev, (uint8_t)address, &t);

    return t;

};

static int proc_pci_read_config(uint32_t *buf, int size)
{
   int i;

   for(i=0;i<size;i++) {
       buf[i]=__svgalib_pci_read_config_dword(0,i*4);
   }

   return 0;

};

/*
   find a vga device of the specified vendor, and return
   its configuration (64 dwords) in conf
   return zero if device found.
*/

int __svgalib_pci_find_vendor_vga(unsigned int vendor, unsigned long *conf, int cont)
{
  uint32_t buf[64];

  proc_pci_read_config(buf,64);

  if(((buf[0]&0xffff)==vendor)&&
    (((buf[2]>>16)&0xffff)==0x0300)) { /* VGA Class */
       memcpy(conf,buf,256);
       return 0;
    }

  return 1;

}
