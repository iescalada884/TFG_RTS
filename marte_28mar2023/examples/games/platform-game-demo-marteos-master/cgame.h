/*---------------------------------------------------------------------------
--                            Platform Game Demo                           -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  email:    alvaro@binarynonsense.com                                    --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: cgame.h                                                          --
--                                                                         --
--  this file contains [...]                                               --
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
--                      last update: 24 Aug 2014                           --
---------------------------------------------------------------------------*/

#ifndef CGAME_H
#define CGAME_H

#include "vga.h"
#include <assert.h> // assert
#include <time.h>   // timespec, nanosleep
#include <math.h>   // sin
#include <misc/timespec_operations.h>  // double_to_timespec
#include <misc/console_management.h> // reset_blocking.. set_raw_mode
#include <string.h> //memset


#include "csprite.h"

#define PCI_DEVICE_ID_S3_TRIO64V2 35073

class CGame
{
    private:

        CSprite *mario;        
        int gameOver;

    public:
        
        double deltaTime;
        struct timespec timeLastFrame, timeCurrentFrame;  
        
        //CGame();
        unsigned char *buffer;
        void Init();
//         void CheckForMovement();
        void DrawCanvas();
        void Blit();
        int GameLoop();
        
        double TimespecDiff(struct timespec start, struct timespec end);
};
#endif
