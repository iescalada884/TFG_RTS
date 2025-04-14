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
--  file: csprite.h                                                        --
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
--                        last update: 08 Oct 09                           --
---------------------------------------------------------------------------*/

#ifndef CSPRITE_H
#define CSPRITE_H

#include "vga.h"
#include <stdio.h>

class CFrame
{
    
public:
    
    int (* pixelColor)[50][55];
    
};

enum states {IDLE, WALKING, JUMPING};
enum directions {LEFT, RIGHT};

class CSprite
{
    
private:
    
    float posX, posY;
    int numFrames;
        
public:  
    
    CFrame *frames; 
    CSprite();
    CSprite(int nFrames);
    
    states state;
    states previousState;
    directions direction;
    
    int frameState[3][6];
    int frameNumber;
    float timeSinceFrameChange;
    
    int jumpDistance;
    float speedX; // pixels per second
    
    void Init();
    float GetPosX(){return posX;};
    float GetPosY(){return posY;};
    void SetPosX(float x){posX=x;};
    void SetPosY(float y){posY=y;};
    
    void CheckForMovement(float deltaTime);
    void Draw(unsigned char *buffer);
};

#endif
