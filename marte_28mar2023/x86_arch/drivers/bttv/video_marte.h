/* From linux (linux/videodev.h y linux/videodev2.h)*/

#ifndef VIDEO_MARTE_H
#define VIDEO_MARTE_H

#include <sys/types.h>
#include <compbttv.h>
#include <linux/videodev2.h>
//#include <linux/fs.h>
#include <linux/device.h>
#include <time.h>

struct video_device {
	/* device info */
	struct pci_device *dev;
	char name[32];
	int type;       /* v4l1 */
	int type2;      /* v4l2 */
	int hardware;
	int minor;

	/* device ops + callbacks */
  	struct file_operations *fops;
	void (*release)(struct video_device *vfd);


#if 1 /* to be removed in 2.7.x */
	/* obsolete -- fops->owner is used instead */
	struct module *owner;
	/* dev->driver_data will be used instead some day.
	 * Use the video_{get|set}_drvdata() helper functions,
	 * so the switch over will be transparent for you.
	 * Or use {pci|usb}_{get|set}_drvdata() directly. */
	void *priv;
#endif

	/* for videodev.c intenal usage -- please don't touch */
	int users;                     /* video_exclusive_{open|close} ... */
	sem_t lock;         /* ... helper function uses these   */
	char devfs_name[64];           /* devfs */
	struct class_device class_dev; /* sysfs */
};

#define VIDEO_MAJOR	81

#define VFL_TYPE_GRABBER	0
#define VFL_TYPE_VBI		1
#define VFL_TYPE_RADIO		2
#define VFL_TYPE_VTX		3

extern int video_register_device(struct video_device *, int type, int nr);
extern void video_unregister_device(struct video_device *);

/*<MaRTE OS*/
/* extern struct video_device* video_devdata(struct file*); */

/* #define to_video_device(cd) container_of(cd, struct video_device, class_dev) */
/* static inline void */
/* video_device_create_file(struct video_device *vfd, */
/* 			 struct class_device_attribute *attr) */
/* { */
/* 	class_device_create_file(&vfd->class_dev, attr); */
/* } */
/* static inline void */
/* video_device_remove_file(struct video_device *vfd, */
/* 			 struct class_device_attribute *attr) */
/* { */
/* 	class_device_remove_file(&vfd->class_dev, attr); */
/* } */

/* /\* helper functions to alloc / release struct video_device, the */
/*    later can be used for video_device->release() *\/ */


extern int videodev_init(void);
extern struct video_device *video_device_alloc(void);
extern void video_device_release(struct video_device *vfd);

/* /\* helper functions to access driver private data. *\/ */
/* static inline void *video_get_drvdata(struct video_device *dev) */
/* { */
/* 	return dev->priv; */
/* } */

/* static inline void video_set_drvdata(struct video_device *dev, void *data) */
/* { */
/* 	dev->priv = data; */
/* } */

/* extern int video_exclusive_open(struct inode *inode, struct file *file); */
/* extern int video_exclusive_release(struct inode *inode, struct file *file); */
/* extern int video_usercopy(struct inode *inode, struct file *file, */
/* 			  unsigned int cmd, unsigned long arg, */
/* 			  int (*func)(struct inode *inode, struct file *file, */
/* 				      unsigned int cmd, void *arg)); */
/*MaRTE OS>*/

#define VID_TYPE_CAPTURE	1	/* Can capture */
#define VID_TYPE_TUNER		2	/* Can tune */
#define VID_TYPE_TELETEXT	4	/* Does teletext */
#define VID_TYPE_OVERLAY	8	/* Overlay onto frame buffer */
#define VID_TYPE_CHROMAKEY	16	/* Overlay by chromakey */
#define VID_TYPE_CLIPPING	32	/* Can clip */
#define VID_TYPE_FRAMERAM	64	/* Uses the frame buffer memory */
#define VID_TYPE_SCALES		128	/* Scalable */
#define VID_TYPE_MONOCHROME	256	/* Monochrome only */
#define VID_TYPE_SUBCAPTURE	512	/* Can capture subareas of the image */
#define VID_TYPE_MPEG_DECODER	1024	/* Can decode MPEG streams */
#define VID_TYPE_MPEG_ENCODER	2048	/* Can encode MPEG streams */
#define VID_TYPE_MJPEG_DECODER	4096	/* Can decode MJPEG streams */
#define VID_TYPE_MJPEG_ENCODER	8192	/* Can encode MJPEG streams */

struct video_capability
{
	char name[32];
	int type;
	int channels;	/* Num channels */
	int audios;	/* Num audio devices */
	int maxwidth;	/* Supported width */
	int maxheight;	/* And height */
	int minwidth;	/* Supported width */
	int minheight;	/* And height */
};

/*MaRTE OS*/
#define DEFAULT_CHANNEL 1

struct video_channel
{
	int channel;
	char name[32];
	int tuners;
	__u32  flags;
#define VIDEO_VC_TUNER		1	/* Channel has a tuner */
#define VIDEO_VC_AUDIO		2	/* Channel has audio */
	__u16  type;
#define VIDEO_TYPE_TV		1
#define VIDEO_TYPE_CAMERA	2	
	__u16 norm;			/* Norm set by channel */
};

struct video_tuner
{
	int tuner;
	char name[32];
	unsigned long rangelow, rangehigh;	/* Tuner range */
	__u32 flags;
#define VIDEO_TUNER_PAL		1
#define VIDEO_TUNER_NTSC	2
#define VIDEO_TUNER_SECAM	4
#define VIDEO_TUNER_LOW		8	/* Uses KHz not MHz */
#define VIDEO_TUNER_NORM	16	/* Tuner can set norm */
#define VIDEO_TUNER_STEREO_ON	128	/* Tuner is seeing stereo */
#define VIDEO_TUNER_RDS_ON      256     /* Tuner is seeing an RDS datastream */
#define VIDEO_TUNER_MBS_ON      512     /* Tuner is seeing an MBS datastream */
	__u16 mode;			/* PAL/NTSC/SECAM/OTHER */
#define VIDEO_MODE_PAL		0
#define VIDEO_MODE_NTSC		1
#define VIDEO_MODE_SECAM	2
#define VIDEO_MODE_AUTO		3
	__u16 signal;			/* Signal strength 16bit scale */
};

struct video_picture
{
	__u16	brightness;
	__u16	hue;
	__u16	colour;
	__u16	contrast;
	__u16	whiteness;	/* Black and white only */
	__u16	depth;		/* Capture depth */
	__u16   palette;	/* Palette in use */
#define VIDEO_PALETTE_GREY	1	/* Linear greyscale */
#define VIDEO_PALETTE_HI240	2	/* High 240 cube (BT848) */
#define VIDEO_PALETTE_RGB565	3	/* 565 16 bit RGB */
#define VIDEO_PALETTE_RGB24	4	/* 24bit RGB */
#define VIDEO_PALETTE_RGB32	5	/* 32bit RGB */	
#define VIDEO_PALETTE_RGB555	6	/* 555 15bit RGB */
#define VIDEO_PALETTE_YUV422	7	/* YUV422 capture */
#define VIDEO_PALETTE_YUYV	8
#define VIDEO_PALETTE_UYVY	9	/* The great thing about standards is ... */
#define VIDEO_PALETTE_YUV420	10
#define VIDEO_PALETTE_YUV411	11	/* YUV411 capture */
#define VIDEO_PALETTE_RAW	12	/* RAW capture (BT848) */
#define VIDEO_PALETTE_YUV422P	13	/* YUV 4:2:2 Planar */
#define VIDEO_PALETTE_YUV411P	14	/* YUV 4:1:1 Planar */
#define VIDEO_PALETTE_YUV420P	15	/* YUV 4:2:0 Planar */
#define VIDEO_PALETTE_YUV410P	16	/* YUV 4:1:0 Planar */
#define VIDEO_PALETTE_PLANAR	13	/* start of planar entries */
#define VIDEO_PALETTE_COMPONENT 7	/* start of component entries */
};

struct video_audio
{
	int	audio;		/* Audio channel */
	__u16	volume;		/* If settable */
	__u16	bass, treble;
	__u32	flags;
#define VIDEO_AUDIO_MUTE	1
#define VIDEO_AUDIO_MUTABLE	2
#define VIDEO_AUDIO_VOLUME	4
#define VIDEO_AUDIO_BASS	8
#define VIDEO_AUDIO_TREBLE	16	
#define VIDEO_AUDIO_BALANCE	32
	char    name[16];
#define VIDEO_SOUND_MONO	1
#define VIDEO_SOUND_STEREO	2
#define VIDEO_SOUND_LANG1	4
#define VIDEO_SOUND_LANG2	8
        __u16   mode;
        __u16	balance;	/* Stereo balance */
        __u16	step;		/* Step actual volume uses */
};

struct video_clip
{
	__s32	x,y;
	__s32	width, height;
	struct	video_clip *next;	/* For user use/driver use only */
};

struct video_window
{
	__u32	x,y;			/* Position of window */
	__u32	width,height;		/* Its size */
	__u32	chromakey;
	__u32	flags;
	struct	video_clip *clips;	/* Set only */
	int	clipcount;
#define VIDEO_WINDOW_INTERLACE	1
#define VIDEO_WINDOW_CHROMAKEY	16	/* Overlay by chromakey */
#define VIDEO_CLIP_BITMAP	-1
/* bitmap is 1024x625, a '1' bit represents a clipped pixel */
#define VIDEO_CLIPMAP_SIZE	(128 * 625)
};

struct video_capture
{
	__u32 	x,y;			/* Offsets into image */
	__u32	width, height;		/* Area to capture */
	__u16	decimation;		/* Decimation divider */
	__u16	flags;			/* Flags for capture */
#define VIDEO_CAPTURE_ODD		0	/* Temporal */
#define VIDEO_CAPTURE_EVEN		1
};


//#include <time.h>
struct video_buffer
{
	void	*base;
	int	height,width;
	int	depth;
	int	bytesperline;
	struct timespec timestamp;
};

/*<MaRTE OS*/
#define NUMBER_BUFFERS 3

struct video_multiple_buffer
{
  int      frame_grabber;
  int      n_buffers;
  void    *base[NUMBER_BUFFERS];
  int      height;
  int      width;
  int      depth;
  int      bytesperline;
  int      format;
  int      mode;
  struct timespec    timestamp_odd[NUMBER_BUFFERS];
  struct timespec    timestamp_even[NUMBER_BUFFERS];
};
/*MaRTE OS>*/

struct video_mmap
{
	unsigned	int frame;		/* Frame (0 - n) for double buffer */
	int		height,width;
	unsigned	int format;		/* should be VIDEO_PALETTE_* */
};

struct video_key
{
	__u8	key[8];
	__u32	flags;
};


#define VIDEO_MAX_FRAME		32

struct video_mbuf
{
	int	size;		/* Total memory to map */
	int	frames;		/* Frames */
	int	offsets[VIDEO_MAX_FRAME];
};
	

#define 	VIDEO_NO_UNIT	(-1)

	
struct video_unit
{
	int 	video;		/* Video minor */
	int	vbi;		/* VBI minor */
	int	radio;		/* Radio minor */
	int	audio;		/* Audio minor */
	int	teletext;	/* Teletext minor */
};

struct vbi_format {
	__u32	sampling_rate;	/* in Hz */
	__u32	samples_per_line;
	__u32	sample_format;	/* VIDEO_PALETTE_RAW only (1 byte) */
	__s32	start[2];	/* starting line for each frame */
	__u32	count[2];	/* count of lines for each frame */
	__u32	flags;
#define	VBI_UNSYNC	1	/* can distingues between top/bottom field */
#define	VBI_INTERLACED	2	/* lines are interlaced */
};

/* video_info is biased towards hardware mpeg encode/decode */
/* but it could apply generically to any hardware compressor/decompressor */
struct video_info
{
	__u32	frame_count;	/* frames output since decode/encode began */
	__u32	h_size;		/* current unscaled horizontal size */
	__u32	v_size;		/* current unscaled veritcal size */
	__u32	smpte_timecode;	/* current SMPTE timecode (for current GOP) */
	__u32	picture_type;	/* current picture type */
	__u32	temporal_reference;	/* current temporal reference */
	__u8	user_data[256];	/* user data last found in compressed stream */
	/* user_data[0] contains user data flags, user_data[1] has count */
};

/* generic structure for setting playback modes */
struct video_play_mode
{
	int	mode;
	int	p1;
	int	p2;
};

/* for loading microcode / fpga programming */
struct video_code
{
	char	loadwhat[16];	/* name or tag of file being passed */
	int	datasize;
	__u8	*data;
};

#define VIDIOCGCAP		_IOR('v',71,struct video_capability)	/* Get capabilities */
#define VIDIOCGCHAN		_IOWR('v',72,struct video_channel)	/* Get channel info (sources) */
#define VIDIOCSCHAN		_IOW('v',73,struct video_channel)	/* Set channel 	*/
#define VIDIOCGTUNER		_IOWR('v',74,struct video_tuner)	/* Get tuner abilities */
#define VIDIOCSTUNER		_IOW('v',75,struct video_tuner)		/* Tune the tuner for the current channel */
#define VIDIOCGPICT		_IOR('v',76,struct video_picture)	/* Get picture properties */
#define VIDIOCSPICT		_IOW('v',77,struct video_picture)	/* Set picture properties */
#define VIDIOCCAPTURE		_IOW('v',78,int)			/* Start, end capture */
#define VIDIOCGWIN		_IOR('v',79, struct video_window)	/* Get the video overlay window */
#define VIDIOCSWIN		_IOW('v',80, struct video_window)	/* Set the video overlay window - passes clip list for hardware smarts , chromakey etc */
#define VIDIOCGFBUF		_IOR('v',81, struct video_buffer)	/* Get frame buffer */
#define VIDIOCSFBUF		_IOW('v',82, struct video_buffer)	/* Set frame buffer - root only */
#define VIDIOCKEY		_IOR('v',83, struct video_key)		/* Video key event - to dev 255 is to all - cuts capture on all DMA windows with this key (0xFFFFFFFF == all) */
#define VIDIOCGFREQ		_IOR('v',84, unsigned long)		/* Set tuner */
#define VIDIOCSFREQ		_IOW('v',85, unsigned long)		/* Set tuner */
#define VIDIOCGAUDIO		_IOR('v',86, struct video_audio)	/* Get audio info */
#define VIDIOCSAUDIO		_IOW('v',87, struct video_audio)	/* Audio source, mute etc */
#define VIDIOCSYNC		_IOW('v',88, int)			/* Sync with mmap grabbing */
#define VIDIOCMCAPTURE		_IOW('v',89, struct video_mmap)		/* Grab frames */
#define VIDIOCGMBUF		_IOR('v',90, struct video_mbuf)		/* Memory map buffer info */
#define VIDIOCGUNIT		_IOR('v',91, struct video_unit)		/* Get attached units */
#define VIDIOCGCAPTURE		_IOR('v',92, struct video_capture)	/* Get subcapture */
#define VIDIOCSCAPTURE		_IOW('v',93, struct video_capture)	/* Set subcapture */
#define VIDIOCSPLAYMODE		_IOW('v',94, struct video_play_mode)	/* Set output video mode/feature */
#define VIDIOCSWRITEMODE	_IOW('v',95, int)			/* Set write mode */
#define VIDIOCGPLAYINFO		_IOR('v',96, struct video_info)		/* Get current playback info from hardware */
#define VIDIOCSMICROCODE	_IOW('v',97, struct video_code)		/* Load microcode into hardware */
#define	VIDIOCGVBIFMT		_IOR('v',98, struct vbi_format)		/* Get VBI information */
#define	VIDIOCSVBIFMT		_IOW('v',99, struct vbi_format)		/* Set VBI information */
/*<MaRTE OS*/
#define	VIDIOCONTINUOSCAPTURE	_IOW('v',100, int)		        /* Start end capture MARTE OS */
#define VIDIOCGMFBUF		_IOR('v',101, struct video_multiple_buffer)	/* Get frame buffer */
#define VIDIOCSMFBUF		_IOW('v',102, struct video_multiple_buffer)	/* Set frame buffer - root only */

/*MaRTE OS>*/

#define BASE_VIDIOCPRIVATE	192		/* 192-255 are private */

#define VID_HARDWARE_BT848	1


#endif /*VIDEO_MARTE_H*/
