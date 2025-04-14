/*If you want to use this test you need a TRIO64*/
/*If you haven't got it you must change part of the code*/


#include <stdio.h> //Para sscanf
#include <vga.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/pci_ids.h>
#include "xpm.h"
#include <debug_marte.h>

/*Hasta el momento debemos pasar la fotografia como un include*/
#include "mi_foto.xpm"	//Nombre de fotograf�a
#define MIFOTO mi_foto_xpm

//#define INITSTR G800x600x64K //SVGAlib standard mode definitions
//#define VENDOR PCI_VENDOR_ID_S3//Video vendor

#define INITSTR G320x240x256
#define VENDOR VGA
#define CARD PCI_DEVICE_ID_S3_TRIO64V2 //Video driver

struct xpm foto;

int main()
{
  point_t point1, point2, point3;
  // init_serial_communication_with_gdb (SERIAL_PORT_1);
  // set_break_point_here;

  if (init_vga_with_associated_mem(INITSTR,VENDOR,CARD)){
    exit(1);
  }

  if(init_struct_xpm(&foto,MIFOTO)==1)
    exit(-1);

  point1.x=0;
  point1.y=0;
  point2.x=319;
  point2.y=239;

  draw_image_complete(foto,point1);

  sleep(5);

  //vga_rectangle_fill(point1,point2,0);

  //point1.x=0;
  //point1.y=0;
  //point2.x=10;
  //point2.y=10;
  //point3.x=100;
  //point3.y=100;
  //draw_image_partial(foto,point1,point2,point3);
  //sleep(5);
  exit(0);
}













