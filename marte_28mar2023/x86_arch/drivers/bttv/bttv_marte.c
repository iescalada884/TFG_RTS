#include <linux/ioctl.h>
#include "video_marte.h"
#include "bttvp.h"
#include <linux/i2c.h>
#include "bttv_marte.h"
//#include <misc/timespec_operations.h>

extern sem_t vsync;

struct bttv *btv;

int start_frame_grabber(int frame_grabber,
			struct video_multiple_buffer *fbuf) {

  struct video_window win;
  struct video_channel chan;
  struct video_tuner tuner;
  struct video_picture vpic;
  int on=1;
  int res;

  sem_init(&vsync,0,0);

  bttv_init();

  if((frame_grabber>0) && (frame_grabber<bttv_num))
    return(-8);

  if (frame_grabber>=BTTV_MAX)
    return(-6);

  fbuf->frame_grabber=frame_grabber;

  btv=&bttvs[frame_grabber];

  /* Init videodev driver */
  bttv_open(frame_grabber);

  /* Select the input channel */
  res = bttv_ioctl(frame_grabber,VIDIOCGCHAN,(void *)&chan);
  if (res!=0)
    return -1;

  chan.channel = DEFAULT_CHANNEL;
  chan.type = VIDEO_VC_TUNER; 
  chan.norm = VIDEO_TYPE_CAMERA;
                                                                                                                             
  res = bttv_ioctl(frame_grabber,VIDIOCSCHAN,(void *)&chan);
  if (res!=0)
    return -1;

  /* Enable the tuner */
  res = bttv_ioctl(frame_grabber,VIDIOCGTUNER,(void *)&tuner);
  if (res!=0)
    return -1;

  tuner.tuner = 0;
  tuner.mode = VIDEO_MODE_PAL;

  res = bttv_ioctl(frame_grabber,VIDIOCSTUNER,(void *)&tuner);
  if (res!=0)
    return -1;

  /* Select palette and depth */
  res = bttv_ioctl(frame_grabber,VIDIOCGPICT,(void *)&vpic);
  if (res!=0)
    return -1;

  vpic.palette = (__u16)fbuf->format;
  vpic.depth = (__u16)fbuf->depth;
  
  vpic.brightness = 35000;
  vpic.hue = 32000;
  vpic.contrast = 32000;
  vpic.colour = 32000;                           
                                                                                                 
  res = bttv_ioctl(frame_grabber,VIDIOCSPICT,(void *)&vpic);
  if (res!=0)
    return -1;

  res = bttv_ioctl(frame_grabber,VIDIOCGWIN,(void *)&win);
  if (res!=0)
    return -1;

  win.x = 0;
  win.y = 0;
  
  win.width = (__u32)fbuf->width;
 
  if(fbuf->mode==NON_INTERLACED)
    win.height = (__u32)fbuf->height*2;
  else if(fbuf->mode==INTERLACED)
    win.height = (__u32)fbuf->height;
  else
    return -1;

  res = bttv_ioctl(frame_grabber,VIDIOCSWIN,(void *)&win);
  if (res!=0)
    return -1;

  /* Set the buffers*/
  res = bttv_ioctl(frame_grabber,VIDIOCSMFBUF,(void *)(fbuf));
  if (res!=0)
    return -1;

  /* Init the capture */
  res=bttv_ioctl(frame_grabber,VIDIOCONTINUOSCAPTURE,(void *)&on);
  if (res!=0)
    return -1;
  
  /* This order is used to activate the grabber interruptions, */
  res=bttv_ioctl(frame_grabber,VIDIOCSYNC,(void *)(NULL));
  if (res!=0)
    return -1;
  
  
  return 0;
}

int wait_for_next_image(struct video_multiple_buffer *fbuf,
			void * image,
			int size_of_image,
			struct timespec *timestamp) {

  struct bttv *btv=&bttvs[fbuf->frame_grabber];
  int biu,field;
  
  sem_wait(&vsync);
  if(size_of_image!=fbuf->bytesperline*fbuf->height)
    return -9;

  if(NULL==image || NULL==timestamp)
    return -8;


  biu=atomic_read(&btv->buffer_in_use);
  field=atomic_read(&btv->field);

  if(field==VIDEO_CAPTURE_EVEN){
    memcpy(timestamp,&fbuf->timestamp_even[biu],sizeof(struct timespec));
    memcpy(image,fbuf->base[biu],fbuf->bytesperline*fbuf->height);
  }else{
    memcpy(timestamp,&fbuf->timestamp_odd[biu],sizeof(struct timespec));
    memcpy(image,fbuf->base[biu]+fbuf->bytesperline*fbuf->height,fbuf->bytesperline*fbuf->height);
  }
  return 0;
}

int get_last_image(struct video_multiple_buffer *fbuf,
		   void * image,
		   int size_of_image,
		   struct timespec *timestamp) {

  struct bttv *btv=&bttvs[fbuf->frame_grabber];
  int biu,field;
  
  if(size_of_image!=fbuf->bytesperline*fbuf->height)
    return -9;

  if(NULL==image || NULL==timestamp)
    return -8;

  biu=atomic_read(&btv->buffer_in_use);
  field=atomic_read(&btv->field);

  if(field==VIDEO_CAPTURE_EVEN){
    memcpy(timestamp,&fbuf->timestamp_even[biu],sizeof(struct timespec));
    memcpy(image,fbuf->base[biu],fbuf->bytesperline*fbuf->height);
  }else{
    memcpy(timestamp,&fbuf->timestamp_odd[biu],sizeof(struct timespec));
    memcpy(image,fbuf->base[biu]+fbuf->bytesperline*fbuf->height,fbuf->bytesperline*fbuf->height);
  }
  return 0;
}


char* message_error(int error){
  switch(error){
  case 0:
    return NULL;
  case -1:
    return("General error. Contact with MaRTE OS development group");
  case -2:
    return("Mode parameter is no correct");
  case -3:
    return("Width or height parameter is no correct");
  case -4:
    return("The width/height relation is no 4/3");
  case -5:
    return("Format not supported");
  case -6:
    return("Not enough memory for internal driver");
  case -7:
    return("Frame grabber already initialize. Probably your frame grabber number is wrong");
  case -8:
    return("Image or timestamp parameter is no correct");
  case -9:
    return("Size of your buffer is no correct");
  default:
    return("This code is unknown");
  }
}
