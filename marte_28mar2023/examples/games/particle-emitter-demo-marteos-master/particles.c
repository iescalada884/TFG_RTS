/*---------------------------------------------------------------------------
-- Particle Emitter Demo                                                   --
-----------------------------------------------------------------------------
--                                                                         --
-- This is a demo example for MaRTE OS.                                    --
--                                                                         --
-- author: Alvaro Garcia Cuesta                                            --
-- email: alvaro@binarynonsense.com                                        --
-- website: www.binarynonsense.com                                         --
--                                                                         --
-- file: particles.c                                                       --
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
-- last update: 24 Feb 2010                                                --
---------------------------------------------------------------------------*/

#include "vga.h"
#include <assert.h> // assert
#include <time.h>   // timespec, nanosleep, time
#include <misc/timespec_operations.h>  // double_to_timespec
#include <string.h> //memset
#include <stdlib.h> //rand,srand
#include <stdio.h> //sprintf
#include <misc/console_management.h> // reset_blocking.. set_raw_mode


#include  <math.h>
#include  <time.h>
#include  <float.h> //DBL_MAX

//////CONFIG///////////
#define PCI_DEVICE_ID_S3_TRIO64V2 35073
#define VGA_WIDTH 320
#define VGA_HEIGHT 200

int ret;  
int gameOver; 
struct timespec period_ts; 
unsigned char *buffer; // virtual canvas


typedef struct
{
  float lifetime;                       // total lifetime of the particle
  float decay;                          // decay speed of the particle
  int color;                          // color values of the particle
  float x,y;                 // position of the particle
  float xspeed,yspeed;           // speed of the particle
  int active;                       // is particle active or not?
} particle;

#define NUM_PARTICLES 1000
particle particles[NUM_PARTICLES];

void CreateParticle(int i)
{
     particles[i].lifetime= 60;
     particles[i].decay=1;
     particles[i].color = 60;
     particles[i].x= 160.0;
     particles[i].y= 200.0;
     particles[i].xspeed = (400.0/60.0)-(float)(rand()%80000)/100.0/60.0;
     particles[i].yspeed = (800.0/60.0)-(float)(rand()%20000)/100.0/60.0;
     //particles[i].xspeed = (rand()%5)*50/60.0;;     
     //particles[i].yspeed = (rand()%6)*150/60.0;//(float)(rand()%3)*(1/60.0);
     particles[i].active = 1;
}

void EvolveParticle()
{   
  int i;

  for(i=0;i<NUM_PARTICLES;i++)
  {      // evolve the particle parameters
    particles[i].lifetime-=particles[i].decay;
    particles[i].x-=particles[i].xspeed;
    particles[i].y-=particles[i].yspeed;
    particles[i].yspeed-=3.0*10/60.0;
    particles[i].color=particles[i].lifetime;
  }
}

void DrawObjects()
{

    int i;
    memset(buffer, 0, VGA_WIDTH*VGA_HEIGHT);
    
    for (i=0;i<NUM_PARTICLES;i++)
    {
      if(particles[i].y>198) particles[i].lifetime=0.0;
      
      if((particles[i].active==1) && (particles[i].lifetime>0.0)){
	//particles[i].xpos
	if(particles[i].y>=0&&particles[i].y<200&&particles[i].x>=0&&particles[i].x<320){
	    int index =((int)particles[i].y)*320+(particles[i].x);
	    buffer[index]=particles[i].color;
	}
      } 
      else CreateParticle(i);
    }
      
    EvolveParticle();
}



int main()
{ 
    ret = init_vga(G320x200x256, VGA, PCI_DEVICE_ID_S3_TRIO64V2);            
    
    int i;
    for(i=0;i<64;i++){
        vga_setpalette(i, i, i, i);
    }
    
    for(i=0;i<64;i++){
        vga_setpalette(i+64, i, 0, 0);
    }
    
    for(i=0;i<64;i++){
        vga_setpalette(i+64+64, 0, i, 0);
    }
   
    buffer = (unsigned char *)malloc(VGA_WIDTH*VGA_HEIGHT);//virtual canvas
    memset(buffer, 0, VGA_WIDTH*VGA_HEIGHT);    
            
    set_raw_mode();

    unsigned int seed = (unsigned) time(NULL);
    srand(seed);
    /*srand(seed);rand();*/

    float period = 1.0/60.0;
    //period_ts = double_to_timespec(period);  //old versions of MaRTE OS
    double_to_timespec(period,&period_ts); //new versions of MaRTE OS
    
    while(1){
        
        DrawObjects();
        //flip
        vga_drawscansegment(buffer,0,0,VGA_WIDTH*VGA_HEIGHT); 
        nanosleep(&period_ts, NULL);

    }
    
    
    while(getchar()==-1);

    return 0;
}



