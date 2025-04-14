#include "video_marte.h"
#include <malloc.h>
#include <linux/errno.h>
/*>MaRTE OS*/

#include <compbttv.h>
//#include <linux/fs.h>
#include "bttv_marte.h"

#define VIDEO_NUM_DEVICES	256
#define VIDEO_NAME              "novideo4linux"

static struct video_device *video_device[VIDEO_NUM_DEVICES];


int linux_class_register(struct class * cls)
{
	return 0;
}


/* Class Device Stuff */

int linux_class_device_create_file(struct class_device * class_dev)
{
	int error = -EINVAL;
	if (class_dev)
		error = 0;//sysfs_create_file(&class_dev->kobj, &attr->attr);
	return error;
}


int videodev_init_linux(void)
{
  return 1;
}

struct video_device *video_device_alloc(void)
{
	struct video_device *vfd;

	vfd = kmalloc(sizeof(*vfd),GFP_KERNEL);
	if (NULL == vfd)
		return NULL;
	memset(vfd,0,sizeof(*vfd));
	return vfd;
}

void video_device_release(struct video_device *vfd)
{
	kfree(vfd);
}
void video_unregister_device(struct video_device *vfd)
{
}


static void video_release(struct class_device *cd)
{
	struct video_device *vfd = container_of(cd, struct video_device, class_dev);

#if 1 /* needed until all drivers are fixed */
	if (!vfd->release)
		return;
#endif
	vfd->release(vfd);
}

static struct class video_class = {
        .name    = VIDEO_NAME,
	.release = video_release,
};

/**
 *	video_register_device - register video4linux devices
 *	@vfd:  video device structure we want to register
 *	@type: type of device to register
 *	@nr:   which device number (0 == /dev/video0, 1 == /dev/video1, ...
 *             -1 == first free)
 *	
 *	The registration code assigns minor numbers based on the type
 *	requested. -ENFILE is returned in all the device slots for this
 *	category are full. If not then the minor field is set and the
 *	driver initialize function is called (if non %NULL).
 *
 *	Zero is returned on success.
 *
 *	Valid types are
 *
 *	%VFL_TYPE_GRABBER - A frame grabber
 *
 *	%VFL_TYPE_VTX - A teletext device
 *
 *	%VFL_TYPE_VBI - Vertical blank data (undecoded)
 *
 *	%VFL_TYPE_RADIO - A radio card	
 */

int video_register_device(struct video_device *vfd, int type, int nr)
{
	int i=0;
	int base;
	int end;
	char *name_base;
	
	switch(type)
	{
		case VFL_TYPE_GRABBER:
			base=0;
			end=64;
			name_base = "video";
			break;
		case VFL_TYPE_VTX:
			base=192;
			end=224;
			name_base = "vtx";
			break;
		case VFL_TYPE_VBI:
			base=224;
			end=240;
			name_base = "vbi";
			break;
		case VFL_TYPE_RADIO:
			base=64;
			end=128;
			name_base = "radio";
			break;
		default:
			return -1;
	}

	/* pick a minor number */
	//down(&videodev_lock);
	if (-1 == nr) {
		/* use first free */
		for(i=base;i<end;i++)
			if (NULL == video_device[i])
				break;
		if (i == end) {
			//up(&videodev_lock);
			return -ENFILE;
		}
	} else {
		/* use the one the driver asked for */
		i = base+nr;
		if (NULL != video_device[i]) {
			//up(&videodev_lock);
			return -ENFILE;
		}
	}
	video_device[i]=vfd;
	vfd->minor=i;
	//up(&videodev_lock);

	sprintf(vfd->devfs_name, "v4l/%s%d", name_base, i - base);
	//	devfs_mk_cdev(MKDEV(VIDEO_MAJOR, vfd->minor),
	//			S_IFCHR | S_IRUSR | S_IWUSR, vfd->devfs_name);
	//init_MUTEX(&vfd->lock);

	/* sysfs class */
        memset(&vfd->class_dev, 0x00, sizeof(vfd->class_dev));
	vfd->class_dev.class       = &video_class;
	//  set_break_point_here;
	strncpy(vfd->class_dev.class_id, vfd->devfs_name + 4, BUS_ID_SIZE);
	//	class_device_register(&vfd->class_dev);
	linux_class_device_create_file(&vfd->class_dev);
	//	linux_class_device_create_file(&vfd->class_dev);

#if 1 /* needed until all drivers are fixed */
	if (!vfd->release)
		printk(KERN_WARNING "videodev: \"%s\" has no release callback. "
		       "Please fix your driver for proper sysfs support, see "
		       "http://lwn.net/Articles/36850/\n", vfd->name);
#endif
	return 0;
}

//****************************************************************************************************************************
/*MaRTE OS*/

int init_video_multibuffer(int height,
			   int width,
			   int format,
			   int mode,
			   struct video_multiple_buffer *buffer){

  int i;
  int depth_in_bits;
  int depth_in_bytes;

  //  set_break_point_here;

  if(mode!=NON_INTERLACED && mode!=INTERLACED)
    return -2;

  if(mode==INTERLACED){
    if((height>576) || (height<0) || (width>768) || (width<0))
      return -3;
  }else{
    if((height>288) || (height<0) || (width>384) || (width<0))
      return -3;
  }

  if((height*4/3)!=width)
    return -4;

  if((format!=VIDEO_PALETTE_GREY) && (format!=VIDEO_PALETTE_RGB565) \
     && (format!=VIDEO_PALETTE_RGB24) && (format!=VIDEO_PALETTE_RGB32))
    return -5;

  switch(format)
    {
    case VIDEO_PALETTE_GREY:
    case VIDEO_PALETTE_HI240:
    case VIDEO_PALETTE_RAW:
      depth_in_bits=8;
      depth_in_bytes=1;
      break;
    case VIDEO_PALETTE_YUV410P:
      depth_in_bits=9;
      depth_in_bytes=2;
      break;
    case VIDEO_PALETTE_YUV411:
    case VIDEO_PALETTE_YUV420:
    case VIDEO_PALETTE_YUV411P:
    case VIDEO_PALETTE_YUV420P:
      depth_in_bits=12;
      depth_in_bytes=2;
      break;
    case VIDEO_PALETTE_RGB565:
    case VIDEO_PALETTE_RGB555:
    case VIDEO_PALETTE_YUV422:
    case VIDEO_PALETTE_YUYV:
    case VIDEO_PALETTE_UYVY:
    case VIDEO_PALETTE_YUV422P:
      depth_in_bits=16;
      depth_in_bytes=2;
      break;
    case VIDEO_PALETTE_RGB24:
      depth_in_bits=24;
      depth_in_bytes=3;
      break;
    case VIDEO_PALETTE_RGB32:
      depth_in_bits=32;
      depth_in_bytes=4;
      break;
    default:
      return -5;
    }

  buffer->n_buffers = NUMBER_BUFFERS;
  for(i=0;i<buffer->n_buffers;i++){
    if(mode==NON_INTERLACED)
      buffer->base[i] = malloc(height*width*depth_in_bytes*2);
    else
      buffer->base[i] = malloc(height*width*depth_in_bytes);
    if(buffer->base[i]==NULL)
      return -6;
  }

  buffer->height = height;
  buffer->width = width;
  buffer->bytesperline = width*depth_in_bytes;
  buffer->depth = depth_in_bits;
  buffer->format = format;
  buffer->mode = mode;
  return 0;
}

int destroy_video_multibuffer(struct video_multiple_buffer *buffer){
  int i;

  for(i=0;i<buffer->n_buffers;i++){
    free(buffer->base[i]);
  }
  return 0;
}

void * init_image(struct video_multiple_buffer *buffer)
{
  int * ret;
  ret=malloc(buffer->height*buffer->bytesperline);
  return ret;
}
