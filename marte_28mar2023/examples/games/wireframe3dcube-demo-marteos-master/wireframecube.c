/*---------------------------------------------------------------------------
-- Wireframe3DCube Demo                                                    --
-----------------------------------------------------------------------------
--                                                                         --
-- Wireframe3DCube is a demo example for MaRTE OS.                         --
--                                                                         --
-- author: Alvaro Garcia Cuesta                                            --
-- email: alvaro@binarynonsense.com                                        --
-- website: www.binarynonsense.com                                         --
--                                                                         --
-- file: wireframecube.c                                                   --
--                                                                         --
-- this file contains the main logic of the demo                           --
-----------------------------------------------------------------------------
-- License                                                                 --
-----------------------------------------------------------------------------
--                                                                         --
-- This is free software; you can redistribute it and/or modify it         --
-- under the terms of the GNU General Public License version 2 as          --
-- published by the Free Software Foundation.                              --
-- See COPYING file for more info about the license                        --
--                                                                         --
-----------------------------------------------------------------------------
-- last update: 28 Aug 2014                                                --
---------------------------------------------------------------------------*/

#include "vga.h"
#include <assert.h> // assert
#include <time.h>   // timespec, nanosleep, time
#include <misc/timespec_operations.h>  // double_to_timespec
#include <string.h> //memset
#include <stdlib.h> //rand,srand
#include <stdio.h> //sprintf
#include <misc/console_management.h> // reset_blocking.. set_raw_mode
#include <math.h>


#define PCI_DEVICE_ID_S3_TRIO64V2 35073
#define VGA_WIDTH 320
#define VGA_HEIGHT 200
#define PI 3.14159265358979323846 //dPi = 4.0 * atan(1.0);

double const PERIOD = 1.0/30.0;

int ret;  
int gameOver; 
unsigned char *buffer; // virtual canvas

double TimespecDiff(struct timespec start, struct timespec end);

void Init();
void Blit();
void CleanUp();

float angle, x[8], y[8], z[8], rx[8], ry[8], rz[8], scrx[8], scry[8];


void DrawLine (unsigned char* buf, float x1, float y1, float x2, float y2)
{  
  double hl=fabs(x2-x1), vl=fabs(y2-y1), length=(hl>vl)?hl:vl;
    float deltax=(x2-x1)/(float)length, deltay=(y2-y1)/(float)length;
    
    int i;
    for (i=0; i<(int)length; i++)
    {  
      unsigned long x=(int)(x1+=deltax), y=(int)(y1+=deltay);
      if ((x<320)&&(y<200)) buf[320*y + x]=1;
    }
} 

 void DrawCube (unsigned char* buffer, float xa, float ya, float za)
 {
    float mat[4][4]; // Determine rotation matrix
    
    int i;
    
    float xdeg=xa*3.1416f/180, ydeg=ya*3.1416f/180, zdeg=za*3.1416f/180;
    float sx=(float)sin(xdeg), sy=(float)sin(ydeg), sz=(float)sin(zdeg);
    float cx=(float)cos(xdeg), cy=(float)cos(ydeg), cz=(float)cos(zdeg);
    
    mat[0][0]=cx*cz+sx*sy*sz, mat[1][0]=-cx*sz+cz*sx*sy, mat[2][0]=cy*sx;
    mat[0][1]=cy*sz, mat[1][1]=cy*cz, mat[2][1]=-sy;
    mat[0][2]=-cz*sx+cx*sy*sz, mat[1][2]=sx*sz+cx*cz*sy, mat[2][2]=cx*cy;
    
    for (i=0; i<8; i++) // Rotate and apply perspective
    {  
      rx[i]=x[i]*mat[0][0]+y[i]*mat[1][0]+z[i]*mat[2][0];
      ry[i]=x[i]*mat[0][1]+y[i]*mat[1][1]+z[i]*mat[2][1];
      rz[i]=x[i]*mat[0][2]+y[i]*mat[1][2]+z[i]*mat[2][2]+300;
      scrx[i]=(rx[i]*200)/rz[i]+160, scry[i]=(ry[i]*200)/rz[i]+100;
    }
    
    for (i=0; i<4; i++) // Actual drawing
    {  
      DrawLine (buffer, scrx[i], scry[i], scrx[i+4], scry[i+4]);
      DrawLine (buffer, scrx[i], scry[i], scrx[(i+1)%4], scry[(i+1)%4]);
      DrawLine (buffer, scrx[i+4], scry[i+4], scrx[((i+1)%4)+4], scry[((i+1)%4)+4]);
    }
}

int main()
{ 
    int i;
    for(i=0; i<8; i++) // Define the cube
    { 
      x[i]=(float)(50-100*(((i+1)/2)%2));
      y[i]=(float)(50-100*((i/2)%2)), z[i]=(float)(50-100*((i/4)%2));
    }
    
    Init();  
    
    double deltaTime = 0;
    struct timespec timeLastFrame, timeCurrentFrame;
    
    clock_gettime(CLOCK_REALTIME, &timeLastFrame);
   
    while(!gameOver)
    {  
        clock_gettime(CLOCK_REALTIME, &timeCurrentFrame);
        deltaTime = TimespecDiff(timeLastFrame, timeCurrentFrame);
        // UPDATE FRAME /////////////////////////////////////////////

        //printf("time elapsed: %8.9f \n", deltaTime);
        DrawCube (buffer, angle, 360-angle, 0);
        
        angle += 90.0f * deltaTime; 
        
        if (angle >= 360) 
            angle = 0;
        
        Blit();
        
        memset(buffer, 0, VGA_WIDTH*VGA_HEIGHT);

        // END UPDATE FRAME /////////////////////////////////////////    
        timeLastFrame = timeCurrentFrame;        
    }  
    
    CleanUp();
    return 0;

}//main

void Init()
{    
    ret = init_vga (G320x200x256, VGA, PCI_DEVICE_ID_S3_TRIO64V2);            
    
    vga_setpalette(0, 0, 0, 0);//black
    vga_setpalette(1, 255, 255, 255);//white 
    vga_setpalette(2, 0, 255, 0);//green
   
    buffer = (unsigned char *)malloc(VGA_WIDTH*VGA_HEIGHT);//virtual canvas
    memset(buffer, 0, VGA_WIDTH*VGA_HEIGHT);    
    Blit();
            
    reset_blocking_mode ();
    set_raw_mode();
  
    gameOver = 0;
}

void Blit()
{
    //vga_waitretrace();
    vga_drawscansegment(buffer,0,0,VGA_WIDTH*VGA_HEIGHT); 
}

void CleanUp()
{
    free(buffer);
}

double TimespecDiff(struct timespec start, struct timespec end)
{
  double startSecs = start.tv_sec + start.tv_nsec / 1000000000.0;
  double endSecs = end.tv_sec + end.tv_nsec / 1000000000.0;
  return endSecs - startSecs;
}


