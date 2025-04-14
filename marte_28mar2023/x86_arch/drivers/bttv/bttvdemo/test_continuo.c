#include <unistd.h>
#include <pthread.h>
#include <sys/pci_ids.h>
#include <misc/error_checks.h>
#include <misc/timespec_operations.h>
#include <misc/console_management.h>
#include <bttv_marte.h>
#include <vga.h>


#define INITSTR G320x240x256

#define VENDOR VGA//Video vendor
#define CARD PCI_DEVICE_ID_S3_TRIO64V2//Video driver

#define WIDTH 320
#define HEIGHT 240

#define COLOUR16

#define FORMAT VIDEO_PALETTE_GREY
#define BYTES_PP 1


#define FRAME_GRABBER_NUMBER 0

void * una_imagen;


void elaborate_image(unsigned char *imageptr,struct timespec tim);

static int sigo_vivo=1;

/* Send the grab command */
void *grab_task(void *arg) {
  struct video_multiple_buffer *fbuf=arg;
  struct timespec medidas;

  while(sigo_vivo){
    
    if(wait_for_next_image(fbuf,una_imagen,WIDTH*HEIGHT*BYTES_PP,&medidas)!=0)
      break;

#ifndef NO_GRAFICO
    draw_image_2(HEIGHT,WIDTH,una_imagen);
    //    elaborate_image(una_imagen,medidas);
#else
    elaborate_image(fbuf->base[atomic_read(&bttvs[0].buffer_in_use)],medidas);
#endif
  }
  sleep(30);
  printc("Esto esta mal\n");

  return NULL;                                   
}

void elaborate_image(unsigned char *imageptr,
		     struct timespec tim)
{

#ifdef NO_GRAFICO
  int biu;

  biu=atomic_read(&bttvs[0].field);
  printf("El puntero es 0x%x\n",(int)imageptr);
  printf("biu %d\tSec = %d Nanosec = %d\n",biu,tim.tv_sec,tim.tv_nsec);
#else
  int i,j;
  for(i=0;i<WIDTH;i++)
    for(j=0;j<HEIGHT;j++){
      vga_setcolor(*(imageptr+i+j*WIDTH));
      vga_drawpixel(i,j);
    } 
  
#endif /*NO_GRAFICO*/
}


int main()
{
  pthread_t t;
  int ret;
  struct video_multiple_buffer buf;

  una_imagen=malloc(WIDTH*HEIGHT*BYTES_PP);

  if(init_video_multibuffer(HEIGHT,WIDTH,FORMAT,NON_INTERLACED,&buf)!=0)
    return (-1);
  
  if(start_frame_grabber(FRAME_GRABBER_NUMBER,&buf)==-1)
    return (-1);

#ifndef NO_GRAFICO
  if (init_vga(INITSTR,VENDOR,CARD)) {
    return (-11);
  }

  restoregray_palette();
#endif /*NO_GRAFICO*/
  reset_blocking_mode();
  set_raw_mode();

  // Create threads
  ret=pthread_create (&t, NULL, grab_task, &buf);

  do{
    sleep(1);
  }while(getchar()!='q');


  sigo_vivo=0;
  sleep(3);
  vga_setmode(TEXT);
  sleep(5);

  return 0;
}
