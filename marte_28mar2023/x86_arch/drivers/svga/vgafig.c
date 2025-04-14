#include "vga.h"
#include <math.h>
#include "driver.h"
#include <debug_marte.h>


/*Not run with 2 and 16 colours*/
/*For now, only runs with 256 colors*/
/*Copy blocks of pixels faster than pixel by pixel. The initial block pixels must have the same format that the final(15 bits ->15 bits right , 24-> 32 wrong)*/
/*The x and y values are the point of the screen since we want to print the block*/
int vga_cpyblock(int width,int height,void *block)
{
  int i,j;
  //  void *h;
  //  int v;
  if (MODEX) {
    /* select plane */
    //    set_break_point_here;
    /*First plane of the block*/
    __svgalib_outseq(0x02,1);
    /* write color to pixel */
    for (i=0;i<width/4;i++)
      for (j=0;j<height;j++){
	*(GM+j * CI.xbytes + ((i*4) >> 2))=*((char*)(block+i*4+(j*width)));
      }
    //    set_break_point_here;
    /*Second plane of the block*/
    __svgalib_outseq(0x02,2);
    /* write color to pixel */
    for (i=0;i<width/4;i++)
      for (j=0;j<height;j++){
	*(GM+j * CI.xbytes + ((i*4) >> 2))=*((char*)(block+i*4+1+(j*width)));
      }

    //    set_break_point_here;
    /*Third plane of the block*/
    __svgalib_outseq(0x02,4);
    /* write color to pixel */
    for (i=0;i<width/4;i++)
      for (j=0;j<height;j++){
	*(GM+j * CI.xbytes + ((i*4) >> 2))=*((char*)(block+i*4+2+(j*width)));
      }
    //    set_break_point_here;
    /*Fourth plane of the block*/
    __svgalib_outseq(0x02,8);
    /* write color to pixel */
    for (i=0;i<width/4;i++)
      for (j=0;j<height;j++){
	*(GM+j * CI.xbytes + ((i*4) >> 2))=*((char*)(block+i*4+3+(j*width)));
      }

    //    set_break_point_here;
    return 0;
  }

  return -1;
}


int redondeo(float z)
{
  int i;
  float j;
  i=(int)z;
  j=z-(float)z;
  j=j*2.0;
  i=i+(int)j;
  return(i);
}

void vga_pixel(point_t point, unsigned int color)
{
  vga_setcolor(color);
  vga_drawpixel(point.x,point.y);
}

void vga_line(point_t point1, point_t point2, unsigned int color){
  vga_setcolor(color);
  vga_drawline(point1.x,point1.y,point2.x,point2.y);
}

void discircum(unsigned int x, unsigned int y, point_t centre)
{
  vga_drawpixel(centre.x + x, centre.y + y);
  vga_drawpixel(centre.x + x, centre.y - y);
  vga_drawpixel(centre.x - x, centre.y + y);
  vga_drawpixel(centre.x - x, centre.y - y);
  vga_drawpixel(centre.x + y, centre.y + x);
  vga_drawpixel(centre.x + y, centre.y - x);
  vga_drawpixel(centre.x - y, centre.y + x);
  vga_drawpixel(centre.x - y, centre.y - x);
}

void vga_circumference(point_t centre, unsigned int radio, unsigned int color)
{
  int x, y, d;

  vga_setcolor(color);

  if (radio < 1) {
    vga_drawpixel(centre.x,centre.y);
    return;
  }
  x = 0;
  y = radio;
  d = 1 - radio;
  discircum(x, y, centre);
  while (x < y) {
    if (d < 0)
      d += x * 2 + 3;
    else {
      d += x * 2 - y * 2 + 5;
      y--;
    }
    x++;
    discircum(x, y, centre);
  }
}

void discpixels(unsigned int x, unsigned int y, point_t centre)
{
  vga_drawline(centre.x + x, centre.y + y, centre.x + x, centre.y - y);
  vga_drawline(centre.x - x, centre.y + y, centre.x - x, centre.y - y);
  vga_drawline(centre.x + y, centre.y + x, centre.x + y, centre.y - x);
  vga_drawline(centre.x - y, centre.y + x, centre.x - y, centre.y - x);
}

void vga_circle(point_t centre, unsigned int radio, unsigned int color)
{
  int x, y, d;

  vga_setcolor(color);

  if (radio < 1) {
    vga_drawpixel(centre.x,centre.y);
    return;
  }
  x = 0;
  y = radio;
  d = 1 - radio;
  discpixels(x, y, centre);
  while (x < y) {
    if (d < 0)
      d += x * 2 + 3;
    else {
      d += x * 2 - y * 2 + 5;
      y--;
    }
    x++;
    discpixels(x, y, centre);
  }
}

/**
 * vga_rectangle_fill()
 *
 * draws a rectangle by drawing vertical lines with vga_drawline
 */

void vga_rectangle_fill(point_t point1, point_t point2, unsigned int color)
{
        int x;

        vga_setcolor(color);

        for(x=point1.x; x<= point2.x; x++) {
                vga_drawline(x, point1.y, x, point2.y);
        }
}

void vga_rectangle(point_t point1, point_t point2, unsigned int color)
{
  vga_setcolor(color);
  vga_drawline(point1.x,point1.y,point1.x,point2.y);
  vga_drawline(point1.x,point1.y,point2.x,point1.y);
  vga_drawline(point2.x,point1.y,point2.x,point2.y);
  vga_drawline(point1.x,point2.y,point2.x,point2.y);
}

void vga_ellipse(point_t point1, point_t point2, unsigned int color)
{
  int i,valor;
  unsigned int a, b;
  float raiz,acuadrado,bcuadrado;
  point_t centre,decision;

  a=redondeo((float)(point2.x-point1.x)/2.0);
  b=redondeo((float)(point2.y-point1.y)/2.0);
  bcuadrado=(float)(b*b);
  acuadrado=(float)(a*a);
  centre.x=point1.x+a;
  centre.y=point1.y+b;

  decision.x=redondeo((float)acuadrado/sqrt(acuadrado+bcuadrado));
  decision.y=redondeo((float)bcuadrado/sqrt(acuadrado+bcuadrado));


  vga_setcolor(color);
  vga_drawpixel(point1.x,centre.y);
  vga_drawpixel(point2.x,centre.y);

  for (i=0;i<decision.y+1;i++){
    raiz=a*sqrt(1.0-((float)(i*i)/bcuadrado));
    valor=redondeo(raiz);
    vga_drawpixel(centre.x-valor,centre.y+i);
    vga_drawpixel(centre.x+valor,centre.y+i);
    vga_drawpixel(centre.x-valor,centre.y-i);
    vga_drawpixel(centre.x+valor,centre.y-i);
  }
  for (i=0;i<decision.x+1;i++){
    raiz=b*sqrt(1.0-((float)(i*i)/acuadrado));
    valor=redondeo(raiz);
    vga_drawpixel(centre.x-i,centre.y+valor);
    vga_drawpixel(centre.x+i,centre.y+valor);
    vga_drawpixel(centre.x-i,centre.y-valor);
    vga_drawpixel(centre.x+i,centre.y-valor);
  }
}

void vga_ellipse_fill(point_t point1, point_t point2, unsigned int color){
  int i,valor;
  unsigned int a, b;
  float raiz,bcuadrado;
  point_t centre;

  a=redondeo((float)(point2.x-point1.x)/2.0);
  b=redondeo((float)(point2.y-point1.y)/2.0);
  bcuadrado=(float)(b*b);
  centre.x=point1.x+a;
  centre.y=point1.y+b;

  vga_setcolor(color);
  vga_drawline(point1.x,centre.y,point2.x,centre.y);
  for (i=1;i<b+1;i++){
    raiz=a*sqrt(1.0-((float)(i*i)/bcuadrado));
    valor=redondeo(raiz);
    vga_drawline(centre.x-valor,centre.y+i,centre.x+valor,centre.y+i);
    vga_drawline(centre.x-valor,centre.y-i,centre.x+valor,centre.y-i);
  }
}


void vga_polygon(point_t * points, unsigned int n_points, unsigned int color)
{
  int i;

  vga_setcolor(color);

  vga_drawline(points[n_points].x,points[n_points].y,points[0].x,points[0].y);

  for (i=0;i<n_points-2;i++)
    vga_drawline(points[i].x,points[i].y,points[i+1].x,points[i+1].y);

}


void vga_polyline(point_t * points, unsigned int n_points, unsigned int color)
{
  int i;
  vga_setcolor(color);

  for (i=0;i<n_points-1;i++)
    vga_drawline(points[i].x,points[i].y,points[i+1].x,points[i+1].y);
}

void full_blue_screen(void)
{
  int i;

  if(CP!=0)
    restorepalette_default();
  vga_setcolor(32);
  vga_drawline(0,0,0,CI.ydim-1);
  for (i=1;i<=CI.ydim;i++){
    //dibuja lineas para formar el rectangulo
    vga_drawline(0,i,CI.xdim-1,i);
  }
  //  restoregray_palette(0);
}


/*extern void vga_arc(point_t point1, point_t point2, unsigned int angle1, unsigned int angle2, unsigned int color);
{

}
*/







