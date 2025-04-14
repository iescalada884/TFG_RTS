/*---------------------------------------------------------------------------
--                     Simple Chess for MaRTE OS                           -- 
-----------------------------------------------------------------------------
--                                                                         --
--  A simple single-player chess game for MaRTE OS                         --
--                                                                         --
--  author:   Álvaro García                                                --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: draw.h                                                           --
--                                                                         --
-----------------------------------------------------------------------------
--                               License                                  -- 
-----------------------------------------------------------------------------
--                                                                         --
-- This is free software; you can redistribute it and/or modify it         --
-- under the terms of the GNU General Public License version 2 as          -- 
-- published by the Free Software Foundation.                              --
-- See COPYING file for more info about the license                        --
--                                                                         --
-----------------------------------------------------------------------------
--                        last update: 09 Jun 09                           --
---------------------------------------------------------------------------*/

#ifndef MAIN_H
#define MAIN_H

#include "vga.h"
#include <assert.h> // assert
#include <time.h>   // timespec, nanosleep
#include <math.h>   // sin
#include <misc/timespec_operations.h>  // double_to_timespec
#include <misc/console_management.h> // reset_blocking.. set_raw_mode
#include <string.h> //memset




#define PCI_DEVICE_ID_S3_TRIO64V2 35073

//VARIABLES///////////////////

int gameOver;
double period;
struct timespec period_ts;
int screenWidth,screenHeight;

unsigned char *backBuffer;



void initVGA();

void initPalette();

void draw_board(unsigned char *backBuffer,int screenWidth,int chessBoardX, int chessBoardY);

void draw_pieces(unsigned char *backBuffer,int screenWidth, unsigned char board[64+3],int chessBoardX, int chessBoardY);

void draw_cursor(unsigned char *backBuffer,int chessBoardX, int chessBoardY,int screenWidth,int cursorX,int cursorY);

void draw_selected(unsigned char *backBuffer,int chessBoardX, int chessBoardY,int screenWidth,int x1,int y1);

void blit(unsigned char *backBuffer,int screenWidth,int screenHeight);

#endif
