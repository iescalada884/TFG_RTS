#include "video_marte.h"

#ifndef BTTV_MARTE_H
#define BTTV_MARTE_H

/* This is the first function to call. 
   
   The mode value determines if we want to use the complete image (INTERLACED)
   or field-to-field (NON_INTERLACED).

   The height and width values specify the size of the capture. The value is 
   dependent of the mode value. If value is INTERLACED, the maximun value that
   we can put is width=768 and height=576. With NON_INTERLACED 
   width=384 and height=288. It is necessary that these values are 4/3 
   proportion 


   The format value can be one of this labels

           VIDEO_PALETTE_GREY            Linear greyscale
	   VIDEO_PALETTE_RGB565          565 16 bit RGB
	   VIDEO_PALETTE_RGB24           24bit RGB
	   VIDEO_PALETTE_RGB32           32bit RGB
   That labels are defined in video_marte.h . In the future, I expect that there were
   more formats.Any volunteer?

   The last value,buffer, is an output parameter that identify 
   the frame grabber
   The return is 0 if it is right.
   
*/

#define INTERLACED 0
#define NON_INTERLACED 1

extern int init_video_multibuffer(int height,
				  int width,
				  int format,
				  int mode,
				  struct video_multiple_buffer *buffer);

/* This is the last function to call. Dealocate struct video_multiple_buffer*/

extern int destroy_video_multibuffer(struct video_multiple_buffer *buffer);

/* This is function is called after init_video_multibuffer and is the core
   of the library.*/

extern int start_frame_grabber(int frame_grabber,
			       struct video_multiple_buffer *fbuf);

/* This function alloc memory for the capture image. We need the height,
   width and bytes per pixel of the image.*/
extern void * init_image(struct video_multiple_buffer *fbuf);

/* This function puts in image parameter the next valid field/frame from 
   the frame grabber fbuf and the instant of that capture en timestamp 
   parameter. The size_of_image value corresponds with expected size of 
   the capture.
   The return is 0 if it is right.
*/

extern int wait_for_next_image(struct video_multiple_buffer *fbuf,
			       void * image,
			       int size_of_image,
			       struct timespec *timestamp);

/* This function puts in image parameter the actual valid field/frame from 
   the frame grabber fbuf and the instant of that capture en timestamp 
   parameter. The size_of_image value corresponds with expected size of 
   the capture.
   The return is 0 if it is right.
*/

extern int get_last_image(struct video_multiple_buffer *fbuf,
			  void * image,
			  int size_of_image,
			  struct timespec *timestamp);

/*This function return a string with the different errors. This can be use
  with same function of print family*/
extern char* message_error(int error);

#endif /*BTTV_MARTE_H*/
