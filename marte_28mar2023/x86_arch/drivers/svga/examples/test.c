/*The compilation sentence is
  mgcc -I(svga_dir) test.c -L. -lsvga -lm*/


#include <stdio.h> //Para sscanf
#include <vga.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/pci_ids.h>
#include <debug_marte.h>
#include <time.h>
#include <misc/timespec_operations.h>

#define INITSTR G320x240x256
#define VENDOR VGA//Video vendor
#define CARD PCI_DEVICE_ID_S3_TRIO64V2//Video driver

int main()
{ 	
  int i;
  point_t point1;
  //init_serial_communication_with_gdb (SERIAL_PORT_1);
  //set_break_point_here;		 
  
  printf("Card=%d\n",PCI_DEVICE_ID_S3_TRIO64V2);
  sleep(5);	   
  if (init_vga(INITSTR,VENDOR,CARD)){
    exit(1);
  }
  /*Drawing all our palette*/
  for(i=0;i<240;i++){
    vga_setcolor(i);
    vga_drawline(0,i,319,i);
  }
  sleep(5); 

  /*Changing only the palette*/
  restoregray_palette();
  
  /*Drawing a gray line*/
  vga_setcolor(14);
  for(i=5;i<100;i++) 
    vga_drawpixel(i,5);
  vga_setcolor(13); 
  for(i=5;i<100;i++)
    vga_drawpixel(7,i); 
  sleep(5); 

  /*All the screen blue*/
  restorepalette_default();
  full_blue_screen();
  point_t p = {10,10}; // write text
  vga_text ("hello", p, 0, 1); 

  sleep(5);

  /*Drawing a circumference and a circle*/
  point1.x=160;
  point1.y=120;

  vga_circumference(point1,80,7);
  vga_circle(point1,50,12);
  sleep(5); 

  /*Text mode*/
  vga_setmode(0);
  printf("\nThat is all friends.\n");
  exit(0);
}













