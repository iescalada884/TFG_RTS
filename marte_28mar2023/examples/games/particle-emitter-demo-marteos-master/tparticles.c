/*---------------------------------------------------------------------------
-- Particle Emitter Demo - 'Threaded' version                              --
-----------------------------------------------------------------------------
--                                                                         --
-- This is a demo example for MaRTE OS.                                    --
--                                                                         --
-- author: Alvaro Garcia Cuesta                                            --
-- email: alvaro@binarynonsense.com                                        --
-- website: www.binarynonsense.com                                         --
--                                                                         --
-- file: tparticles.c                                                      --
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

#include <pthread.h>
#include <misc/error_checks.h>

//////CONFIG///////////
#define PCI_DEVICE_ID_S3_TRIO64V2 35073
#define VGA_WIDTH 320
#define VGA_HEIGHT 200

int ret;  
int gameOver; 
struct timespec period_ts; 
unsigned char *buffer;//virtual canvas


typedef struct{
  float lifetime;                       // total lifetime of the particle
  float decay;                          // decay speed of the particle
  int color;                          // color values of the particle
  float x,y;                 // position of the particle
  float xspeed,yspeed;           // speed of the particle
  int active;                       // is particle active or not?
} particle;

#define NUM_PARTICLES 900
struct shared_data {
  pthread_mutex_t mutex;
  particle particles[NUM_PARTICLES];
};

struct thread_data{
    struct shared_data * sharedData;    
    struct timespec period_ts;
    int color;
    int index;
    int numParticles;
};

/* Periodic thread */
void * threadParticle (void *arg)
{
  struct thread_data *threadData = (struct thread_data *)arg;
  struct shared_data *sharedData = threadData->sharedData;
  
  
  struct timespec my_period, next_activation;

  my_period = threadData->period_ts;
  
  clock_gettime(CLOCK_MONOTONIC, &next_activation);

  // Do "useful" work and wait until next period
  while (1) {
  
    incr_timespec (&next_activation, &my_period);

    //////////////////////////////////////    
    int i;
    pthread_mutex_lock (&sharedData->mutex);      
    for (i=threadData->index;i<threadData->index+threadData->numParticles;i++){
    
        if(sharedData->particles[i].y>198) sharedData->particles[i].lifetime=0.0;
        if((sharedData->particles[i].active!=1) || (sharedData->particles[i].lifetime<=0.0)){//dead
        
            //InitParticle
            sharedData->particles[i].lifetime= 60;
            sharedData->particles[i].decay=1;
            sharedData->particles[i].color = threadData->color;
            sharedData->particles[i].x= 160.0;
            sharedData->particles[i].y= 200.0-(float)(rand()%80000)/100.0/60.0;
            sharedData->particles[i].xspeed = (400.0/60.0)-(float)(rand()%80000)/100.0/60.0;
            sharedData->particles[i].yspeed = (800.0/60.0)-(float)(rand()%20000)/100.0/60.0;
            sharedData->particles[i].active = 1;
        
        }
        //UpdateParticles
        sharedData->particles[i].lifetime-=sharedData->particles[i].decay;
        sharedData->particles[i].x-=sharedData->particles[i].xspeed;
        sharedData->particles[i].y-=sharedData->particles[i].yspeed;
        sharedData->particles[i].yspeed-=3.0*10/60.0;
        sharedData->particles[i].color-=sharedData->particles[i].decay;
    }
    
    pthread_mutex_unlock (&sharedData->mutex);   
    //////////////////////////////////////

    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &next_activation, NULL);
  }
}

/* Periodic thread */
void * threadPainter (void *arg){

  struct thread_data *threadData = (struct thread_data *)arg;
  struct shared_data *sharedData = threadData->sharedData;
  
  
  struct timespec my_period, next_activation;

  my_period = threadData->period_ts;
  
  clock_gettime(CLOCK_MONOTONIC, &next_activation);

  // Do "useful" work and wait until next period
  while (1) {
  
    incr_timespec (&next_activation, &my_period);

    //////////////////////////////////////    
    int i;
    memset(buffer, 0, VGA_WIDTH*VGA_HEIGHT); 
    pthread_mutex_lock (&sharedData->mutex);      
    for (i=0;i<NUM_PARTICLES;i++){
        if((sharedData->particles[i].active==1) && (sharedData->particles[i].lifetime>0.0)){
            if(sharedData->particles[i].y>=0&&sharedData->particles[i].y<200&&sharedData->particles[i].x>=0&&sharedData->particles[i].x<320){
               int index =((int)sharedData->particles[i].y)*320+(sharedData->particles[i].x);
               buffer[index]=sharedData->particles[i].color;
            }
        }
    }
    pthread_mutex_unlock (&sharedData->mutex);
    //flip
    vga_drawscansegment(buffer,0,0,VGA_WIDTH*VGA_HEIGHT);     
    //////////////////////////////////////

    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &next_activation, NULL);
    
  }
}

int main(){ 

    ret = init_vga(G320x200x256, VGA, PCI_DEVICE_ID_S3_TRIO64V2);            
    
    int i;
    for(i=0;i<64;i++){
        vga_setpalette(i, 0, 0, i);
    }
    
    for(i=0;i<64;i++){
        vga_setpalette(i+64, i, 0, 0);
    }
    
    for(i=0;i<64;i++){
        vga_setpalette(i+64+64, 0, i, 0);
    }
    
    for(i=0;i<64;i++){
        vga_setpalette(i+64+64+64, i, i, i);
    }
   
    buffer = (unsigned char *)malloc(VGA_WIDTH*VGA_HEIGHT);//virtual canvas
    memset(buffer, 0, VGA_WIDTH*VGA_HEIGHT);    
            
    set_raw_mode();

    unsigned int seed = (unsigned) time(NULL);
    srand(seed);
    /*srand(seed);rand();*/   
    
    pthread_mutexattr_t mutexattr;
    pthread_t tparticles,tpainter;
    pthread_attr_t attr;
    
    int ret;
    float period;    
    
    struct shared_data data;
    struct thread_data threadData;
    threadData.sharedData=&data;  
    struct thread_data threadData2;
    threadData2.sharedData=&data; 
    struct thread_data threadData3;
    threadData3.sharedData=&data; 
    
    
    pthread_mutexattr_init (&mutexattr);
    if ((ret = pthread_mutex_init (&data.mutex,&mutexattr)))
        printf ("error in mutex init:%s\n", strerror (ret));
        
    // Create threads with default scheduling parameters (SCHED_FIFO)
    CHK( pthread_attr_init (&attr) );
    
    period = 1.0/60.0;
    double_to_timespec(period,&period_ts); //new versions of MaRTE OS
    threadData.period_ts=period_ts;
    pthread_create (&tpainter, &attr, threadPainter, &threadData);

    period = 1.0/60.0;
    double_to_timespec(period,&period_ts); //new versions of MaRTE OS
    threadData.period_ts=period_ts;
    threadData.color=63+64;
    threadData.index=0;
    threadData.numParticles=300;
    pthread_create (&tparticles, &attr, threadParticle, &threadData);
    
    //nanosleep(&period_ts, NULL);

    
    period = 1.0/60.0;
    double_to_timespec(period,&period_ts); //new versions of MaRTE OS
    threadData2.period_ts=period_ts;
    threadData2.color=63;
    threadData2.index=300;
    threadData2.numParticles=300;
    pthread_create (&tparticles, &attr, threadParticle, &threadData2);

    
    period = 1.0/60.0;
    double_to_timespec(period,&period_ts); //new versions of MaRTE OS
    threadData3.period_ts=period_ts;
    threadData3.color=63+64+64;
    threadData3.index=600;
    threadData3.numParticles=300;
    pthread_create (&tparticles, &attr, threadParticle, &threadData3);
    
    
    
    while(getchar()==-1);

    return 0;
}



