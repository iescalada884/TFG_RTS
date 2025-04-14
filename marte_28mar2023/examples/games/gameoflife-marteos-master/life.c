/*---------------------------------------------------------------------------
--                                GoLife                                   -- 
-----------------------------------------------------------------------------
--                                                                         --
--  GoLife is an example game for MaRTE OS.                                --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  email:    alvaro@binarynonsense.com                                    --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: life.c                                                           --
--                                                                         --
--  this file contains the main logic of the program                       --
-----------------------------------------------------------------------------
--                               License                                  -- 
-----------------------------------------------------------------------------
--                                                                         --
-- GoLife is free software; you can redistribute it and/or modify it       --
-- under the terms of the GNU General Public License version 2 as          -- 
-- published by the Free Software Foundation.                              --
-- See COPYING file for more info about the license                        --
--                                                                         --
-----------------------------------------------------------------------------
--                        last update: 13 Jun 09                           --
---------------------------------------------------------------------------*/

#include "vga.h"
#include <assert.h> // assert
#include <time.h>   // timespec, nanosleep, time
#include <misc/timespec_operations.h>  // double_to_timespec
#include <string.h> //memset
#include <stdlib.h> //rand,srand
#include <stdio.h> //sprintf
#include <misc/console_management.h> // reset_blocking.. set_raw_mode


//////CONFIG///////////
#define PCI_DEVICE_ID_S3_TRIO64V2 35073
#define VGA_WIDTH 320
#define VGA_HEIGHT 200
#define MAP_WIDTH 50
#define MAP_HEIGHT 50
#define CELL_WIDTH 4
int const wrap = 0;
double const PERIOD = 1.0/5.0;//generations per second
///////////////////////

int ret;  
int gameOver; 
struct timespec period_ts; 
unsigned char *buffer;//virtual canvas

unsigned char map[MAP_WIDTH][MAP_HEIGHT];
unsigned char tempMap[MAP_WIDTH][MAP_HEIGHT];

////FUNCTIONS DECLARATIONS////
void init();
char menu();
void initMaps(char key);
int isCellON(int x, int y);
int numberCellsOn(int x, int y);
void evolve();
void swapMaps();
void drawRightArea();
void drawMap();
void flip();
void cleanUp();

//Create Maps:
void randomMap();
void gosperGlider();
/////////////////////////////

int main(){ 
    
    char gen[50];
    int generation=1;
    
    init();     
    drawRightArea();   
    
    while(!gameOver){         
        
                      
        drawMap();
        flip(); 
        
        sprintf(gen,"%d", generation); 
        vga_text(gen , conv_to_point((MAP_WIDTH*CELL_WIDTH)+30,VGA_HEIGHT-20), 0, 1);
        
        evolve();   
        swapMaps(); 
         
        generation++;
              
        nanosleep(&period_ts, NULL);
    }  
    
    cleanUp();
    return 0;

}//main

void init(){
    
    ret = init_vga(G320x200x256, VGA, PCI_DEVICE_ID_S3_TRIO64V2);            
    
    //period_ts = double_to_timespec(period);  //old versions of MaRTE OS
    double_to_timespec(PERIOD,&period_ts); //new versions of MaRTE OS
    
    vga_setpalette(1, 0, 0, 0);//black
    vga_setpalette(0, 255, 255, 255);//white 
   
    buffer = (unsigned char *)malloc(VGA_WIDTH*VGA_HEIGHT);//virtual canvas
    memset(buffer, 1, VGA_WIDTH*VGA_HEIGHT);    
            
    set_raw_mode();
    initMaps(menu());    
    
    gameOver = 0;
}

void initMaps(char key){
    int x,y;
    
    for (x=0;x<MAP_WIDTH;x++){
        for (y=0;y<MAP_HEIGHT;y++){
            map[x][y]=tempMap[x][y]=0;
        }
    }
    
    //fill the map with something interesting
    //this are just two examples
    if (key=='1'){
        randomMap();
    }else if (key=='2'){
        gosperGlider();
    }

}

int isCellON(int x, int y){
    
    //wrap around
    if(wrap) {
        if (x<0) x+=MAP_WIDTH;
        if (x>=MAP_WIDTH) x-=MAP_WIDTH;
        if (y<0) y+=MAP_HEIGHT;
        if (y>=MAP_HEIGHT) y-=MAP_HEIGHT;
    }else{
        if (x<0 || x>=MAP_WIDTH || y<0 || y>=MAP_HEIGHT) return 0;
    }
    //end wrap around
    return map[x][y];
}

int numberCellsOn(int x, int y){
    
    int cellsOn =
            isCellON(x-1,y-1)+isCellON(x,y-1)+isCellON(x+1,y-1)+
            isCellON(x-1,y)+                  isCellON(x+1,y)+
            isCellON(x-1,y+1)+isCellON(x,y+1)+isCellON(x+1,y+1);// the eight neighbours        
        
    return cellsOn;
}
   
void evolve(){
    
    int x,y,onCells;
    for (x=0;x<MAP_WIDTH;x++){
        for (y=0;y<MAP_HEIGHT;y++){
            onCells=numberCellsOn(x,y);
            if ( isCellON(x,y) ){//cellON
                if(onCells<2 || onCells>3){//dies (underpopulation or overcrowding)
                    tempMap[x][y]=0;
                }
            }else{//cell off
                if(onCells==3){//birth
                    tempMap[x][y]=1;
                }
            }
        }
    } 
    
}

void swapMaps(){

    int x,y;
    for (x=0;x<MAP_WIDTH;x++){
        for (y=0;y<MAP_HEIGHT;y++){
            map[x][y]=tempMap[x][y];
        }
    }
}


void drawMap(){
    
    int x,y,j,k;
    
    for (x=0;x<MAP_WIDTH;x++){//AWFUL
        for (y=0;y<MAP_HEIGHT;y++){
            for(j=0;j<CELL_WIDTH;j++){
                for(k=0;k<CELL_WIDTH;k++){
                    buffer[ (((y*CELL_WIDTH)+k)*VGA_WIDTH) + ((x*CELL_WIDTH)+j) ]=map[x][y];
                }
            }
            
        }
    }
}

void drawRightArea(){
    int x,y;
    for (x=MAP_WIDTH;x<VGA_WIDTH;x++){//AWFUL
        for (y=0;y<VGA_HEIGHT;y++){
            
            buffer[ (y*VGA_WIDTH) + x ]=1;            
            
        }
    }
}

void flip(){
    
    vga_waitretrace();
    vga_drawscansegment(buffer,0,0,VGA_WIDTH*VGA_HEIGHT);
     
}

void cleanUp(){
    
    free(buffer);
    
}

////MAPS/////
void randomMap(){
    
    //trying to make something pseudo-random but interesting
    unsigned char gliderPattern[3][3]={
            {1,0,0},
            {0,1,1},
            {1,1,0}
    };
    unsigned char pattern_5[5][5]={
        {1,1,1,0,1},
        {1,0,0,0,0},
        {0,0,0,1,1},
        {0,1,1,0,1},
        {1,0,1,0,1}
    };
    unsigned int init_length, seed, x, y, j, k;
    seed = (unsigned) time(NULL);
    srand(seed);
    init_length = (MAP_WIDTH * MAP_HEIGHT) / 2;
    do {
        x = rand() % MAP_WIDTH;
        y = rand() % MAP_HEIGHT;

        map[x][y]=tempMap[x][y] = 1;
    } while (--init_length);
    
    init_length = 20;
    do {
        
        x = rand() % MAP_WIDTH-6;
        y = rand() % MAP_HEIGHT-6;
        for(j=0;j<5;j++){
            for(k=0;k<5;k++){
                map[x+j][y+k]=tempMap[x+j][y+k] = pattern_5[k][j];
            }
        }

        
    } while (--init_length);
    
    init_length = 30;
    do {
        
        x = rand() % MAP_WIDTH-4;
        y = rand() % MAP_HEIGHT-4;
        for(j=0;j<3;j++){
            for(k=0;k<3;k++){
                map[x+j][y+k]=tempMap[x+j][y+k] = gliderPattern[k][j];
            }
        }

        
    } while (--init_length);
    
    init_length = 15;
    do {
        
        x = rand() % MAP_WIDTH-4;
        y = rand() % MAP_HEIGHT-4;
        for(j=0;j<3;j++){
            for(k=0;k<3;k++){
                map[x+j][y+k]=tempMap[x+j][y+k] = 1;
            }
        }

        
    } while (--init_length);
}

void gosperGlider(){
    
    //my tribute to Bill Gosper, one of the original MIT hackers
    unsigned short x,y;
     ////GOSPER's GLIDER GUN
    unsigned char pattern[9][36]={
        {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1},
        {0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1},
        {1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
        {1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
        {0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    };
    for(x=0;x<36;x++){
        for(y=0;y<9;y++){
            map[x][y+5]=tempMap[x][y+5] = pattern[y][x];
        }
    }
    
}

/////menu
char menu(){    
    
    vga_text("Choose initial map:" , conv_to_point(10,20), 1, 0);
    vga_text("Press 1 for Random Map" , conv_to_point(10,40), 1, 0);
    vga_text("Press 2 for Gosper's Glider Gun" , conv_to_point(10,60), 1, 0);
    int done=0;
    char key;
    while(!done){
        key = getchar();
        if(key=='1' || key == '2') done=1;
    }
    return key;
}
