#ifndef __SVGALIB_HELPER__
#define __SVGALIB_HELPER__


#define MAX_NR_DEVICES 15

#define address_t unsigned long

struct sh_pci_device {
   unsigned short vendor;
   unsigned short id;
   unsigned char revision;
   struct pci_dev *dev;
   address_t mem[6];
   address_t len[6];
   address_t mask[6];
   int flags[6];
   unsigned long iobase;
   int (*test_vsync)(struct sh_pci_device *);
   void (*ack_vsync)(struct sh_pci_device *);
   void (*enable_vsync)(struct sh_pci_device *);
};


typedef struct {
  int port;
  int length;
  unsigned char* string;
} io_string_t;

typedef struct {
   int port;
   unsigned int val;
} io_t;

typedef struct {
   int pcipos;
   unsigned int address;
   unsigned long val;
} pcic_t;

typedef struct {
   void *win;
   void *lfb;
} windowing_t;

#define SVGALIB_HELPER_IOC_MAGIC 0xB3

#define SVGALIB_HELPER_IOCSOUTB		_IOR(SVGALIB_HELPER_IOC_MAGIC,1,io_t)
#define SVGALIB_HELPER_IOCSOUTW		_IOR(SVGALIB_HELPER_IOC_MAGIC,2,io_t)
#define SVGALIB_HELPER_IOCSOUTL		_IOR(SVGALIB_HELPER_IOC_MAGIC,3,io_t)
#define SVGALIB_HELPER_IOCGINB		_IOW(SVGALIB_HELPER_IOC_MAGIC,4,io_t)
#define SVGALIB_HELPER_IOCGINW		_IOW(SVGALIB_HELPER_IOC_MAGIC,5,io_t)
#define SVGALIB_HELPER_IOCGINL		_IOW(SVGALIB_HELPER_IOC_MAGIC,6,io_t)

#define SVGALIB_HELPER_IOCSPCIOUTB	_IOR(SVGALIB_HELPER_IOC_MAGIC,11,pcic_t)
#define SVGALIB_HELPER_IOCSPCIOUTW	_IOR(SVGALIB_HELPER_IOC_MAGIC,12,pcic_t)
#define SVGALIB_HELPER_IOCSPCIOUTL	_IOR(SVGALIB_HELPER_IOC_MAGIC,13,pcic_t)
#define SVGALIB_HELPER_IOCGPCIINB	_IOW(SVGALIB_HELPER_IOC_MAGIC,14,pcic_t)
#define SVGALIB_HELPER_IOCGPCIINW	_IOW(SVGALIB_HELPER_IOC_MAGIC,15,pcic_t)
#define SVGALIB_HELPER_IOCGPCIINL	_IOW(SVGALIB_HELPER_IOC_MAGIC,16,pcic_t)
#define SVGALIB_HELPER_IOCGPCIAPLEN	_IOW(SVGALIB_HELPER_IOC_MAGIC,17,pcic_t)

#define SVGALIB_HELPER_IOCDVMA		_IO(SVGALIB_HELPER_IOC_MAGIC,7)
#define SVGALIB_HELPER_IOCSWIND		_IOR(SVGALIB_HELPER_IOC_MAGIC,8,windowing_t)

#define SVGALIB_HELPER_IOCIOPERM	_IO(SVGALIB_HELPER_IOC_MAGIC,9)
#define SVGALIB_HELPER_IOCSREPOUTB	_IOR(SVGALIB_HELPER_IOC_MAGIC,10,io_t)

#define SVGALIB_HELPER_IOCGI810GTT	_IOW(SVGALIB_HELPER_IOC_MAGIC,128,unsigned int *)
#define SVGALIB_HELPER_IOCGI810GTTE	_IOW(SVGALIB_HELPER_IOC_MAGIC,129,unsigned int *)

#define SVGALIB_HELPER_IOCSWRITEB	_IOR(SVGALIB_HELPER_IOC_MAGIC,21,io_t)
#define SVGALIB_HELPER_IOCSWRITEW	_IOR(SVGALIB_HELPER_IOC_MAGIC,22,io_t)
#define SVGALIB_HELPER_IOCSWRITEL	_IOR(SVGALIB_HELPER_IOC_MAGIC,23,io_t)
#define SVGALIB_HELPER_IOCGREADB	_IOW(SVGALIB_HELPER_IOC_MAGIC,24,io_t)
#define SVGALIB_HELPER_IOCGREADW	_IOW(SVGALIB_HELPER_IOC_MAGIC,25,io_t)
#define SVGALIB_HELPER_IOCGREADL	_IOW(SVGALIB_HELPER_IOC_MAGIC,26,io_t)

#define SVGALIB_HELPER_IOCWAITRETRACE	_IO(SVGALIB_HELPER_IOC_MAGIC,31)

struct inode {};

int svgalib_helper_ioctl( struct inode *inode, unsigned int cmd, unsigned long arg);


#endif


