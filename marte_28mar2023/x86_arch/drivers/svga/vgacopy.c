#include "vga.h"
#include "libvga.h"
#include <stdlib.h>

int vga_scroll_ver(point_t point1, point_t point2, int lines){
  
  int inicio,salida,i;
  unsigned char * colors=NULL;
  vga_modeinfo *modeinfo;
  
  modeinfo = vga_getmodeinfo(vga_getcurrentmode());
  
  if ((point1.x<0)&&(point1.y<0)&&(point2.x>modeinfo->width)&&(point2.y>modeinfo->height))
    return -1;
  if (lines>0){
    if ((lines+point1.y)>modeinfo->height){
      return -1;
    }else{	
      if (lines+point2.y>modeinfo->height-1)
	salida=modeinfo->height-(lines+point1.y);
      else
	salida=point2.y-point1.y;
    }  
    colors=(char *) malloc ((point2.x-point1.x)*2);
    for (i=salida;i>=0;i--){
      vga_getscansegment(colors, point1.x, point1.y+i, (point2.x-point1.x)*modeinfo->bytesperpixel);
      vga_drawscansegment(colors, point1.x, point1.y+i+lines, (point2.x-point1.x)*modeinfo->bytesperpixel);
    }
  }else{
    if ((lines+point2.y)<0)
      return -1;
    else{
      if (lines+point1.y<0)
	inicio=-lines-point1.y;
      else
	inicio=0;
    }
    for (i=inicio;i<(point2.y-point1.y);i++){
      vga_getscansegment(colors, point1.x, point1.y+i,( point2.x-point1.x)*modeinfo->bytesperpixel);
      vga_drawscansegment(colors, point1.x, point1.y+i+lines, (point2.x-point1.x)*modeinfo->bytesperpixel);
    }
  }
  return 0;
}


	
int vga_scroll_hor(point_t point1, point_t point2, int lines){
	
  int anchura,i;
  unsigned char *colors=NULL;
  vga_modeinfo *modeinfo;
  	
  modeinfo = vga_getmodeinfo(vga_getcurrentmode());
  
  if ((point1.x<0)&&(point1.y<0)&&(point2.x>=modeinfo->width)&&(point2.y>=modeinfo->height))
    return -1;
  
  if (lines>0){
    if ((lines+point1.x)>modeinfo->width)
      return -1;
    
    if (lines+point2.x>modeinfo->width-1)
      anchura=modeinfo->height-(lines+point1.x);
    else
      anchura=point2.x-point1.x;
    for (i=point1.y;i<point2.y+1;i++){
      vga_getscansegment(colors, point1.x, i, (anchura)*modeinfo->bytesperpixel);
      vga_drawscansegment(colors, point1.x+lines, i,(anchura)*modeinfo->bytesperpixel);
    }
    
  }else{
    if ((lines+point2.x)<0)
      return -1;
    if ((lines+point1.x)<0)
      anchura=point2.x+lines;
    else
      anchura=point2.x-point1.x;
    
    for (i=0;i<(point2.y-point1.y);i++){
      vga_getscansegment(colors, point1.x, point2.y-i, (anchura)*modeinfo->bytesperpixel);
      vga_drawscansegment(colors, point1.x+lines, point2.y-i, (anchura)*modeinfo->bytesperpixel);
    }
  }
  return 0;
}

int vga_copy(slice_t semiframe, point_t point1, point_t point2){
	
  int i,j=0;
  unsigned int tamano;
  vga_modeinfo *modeinfo;
  
	
  modeinfo = vga_getmodeinfo(vga_getcurrentmode());
  
  if ((point1.x<0)&&(point1.y<0)&&(point2.x>=modeinfo->width)&&(point2.y>=modeinfo->height))
    return -1;

  tamano=(point2.x-point1.x)*(point2.y-point1.y)*modeinfo->bytesperpixel;

  semiframe.buffer= (char *) malloc(tamano);
	
  if (semiframe.buffer==NULL)
    return -1;
  
  semiframe.high=point2.y-point1.y;
  semiframe.width=point2.x-point1.x;

  for (i=0;i<semiframe.width;i++){
    vga_getscansegment(&semiframe.buffer[j], point1.x,point1.y+i,(point2.x-point1.x));
    if (i==semiframe.width-1)
      j+=(point2.x-point1.x)*modeinfo->bytesperpixel;;
  }
  return 0;
}



int vga_paste(slice_t semiframe, point_t point1){
	
  int i,j=0;
  vga_modeinfo *modeinfo;
  
	
  modeinfo = vga_getmodeinfo(vga_getcurrentmode());
  
  if ((point1.x<0)&&(point1.y<0))
    return -1;

  if (semiframe.buffer==NULL)
    return -1;

  for (i=0;i<semiframe.high;i++){
    vga_drawscansegment(&semiframe.buffer[j], point1.x,point1.y+i,semiframe.width);
    if (i==semiframe.width-1)
      j+=semiframe.width*modeinfo->bytesperpixel;;
  }
  return 0;
}

void draw_image(int height, int width, unsigned int *imageptr)
{
  unsigned char * temp=(unsigned char *)imageptr;
  int i,j;
  for(i=0;i<width;i++)
    for(j=0;j<height;j++){
      vga_setcolor(*(temp+i+j*width));
      vga_drawpixel(i,j);
    } 
}

void draw_image_2(int height, int width, unsigned int *imageptr)
{
  unsigned char * temp=(unsigned char *)imageptr;
  if(CP==0)
    restoregray_palette();
  if (MODEX) {
    int i,j,k;
    //      __svgalib_outseq(0x01,__svgalib_inseq(0x01) | 0x20);
      //      __svgalib_attscreen(0);
    //    __svgalib_vga_outgra(0x00,0);
    //    __svgalib_vga_outgra(0x01,0);
    //    vga_screenoff();
    for(k=0;k<4;k++){
      __svgalib_outseq(0x02,1 << k);
      for(j=0;j<height;j++){
	for(i=0;i<width/4;i++){
	  *(GM+j * CI.xbytes + i)=*(temp+(i*4+k)+j*width);
	}
      }
    }
    //    __svgalib_vga_outgra(0x01,15);
    //    __svgalib_vga_outgra(0x00,15);
    //      __svgalib_outseq(0x01,__svgalib_inseq(0x01) & 0xdf);
    //          __svgalib_attscreen(0x20);
    //    vga_screenon();
  }
}
