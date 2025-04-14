/*---------------------------------------------------------------------------
--                          Vertical Shmup Demo                            -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: csprite.cpp                                                      --
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
--                      last update: 25 Aug 2014                           --
---------------------------------------------------------------------------*/

#include "csprite.h"

CSprite :: CSprite()
{    
    numFrames=1;
    frames=new CFrame[numFrames];
    actualFrame=0;
    state=FREE;
    direction=RIGHT;        
}

CSprite :: CSprite(int nFrames)
{    
    numFrames=nFrames;
    frames=new CFrame[numFrames];
    actualFrame=0;
    state=FREE;
    direction=RIGHT;        
}

int CSprite :: getWidth ()
{
    return frames[actualFrame].width;
}

int CSprite :: getHeight ()
{
    return frames[actualFrame].height;
}



void CSprite :: checkForMovement ()
{   

    
}

void CSprite :: draw (unsigned char *buffer, int screenWidth, float deltaTime)
{ 
    int x = getPosX();
    int y = getPosY();
    int color, width, height;        
    height = frames[actualFrame].height;
    width = frames[actualFrame].width;
    
    int i,j;
    for(i=0; i<width; i++)
    {
        for(j=0; j<height; j++)
        {
            color = frames[actualFrame].pixelColor[height * j + i];
            if(color != 251)
            {
                buffer[screenWidth * (y+j) + (x+i)] = color;
            }
        }
    } 
}

void CSprite :: draw (CFrame *frame, unsigned char *buffer, int screenWidth){    
    
    int x = getPosX();
    int y = getPosY();
    int color, width, height;        
    height = frame->height;
    width = frame->width;
    
    int i, j;
    for(i=0; i<width; i++)
    {
        for(j=0; j<height; j++)
        {
            color=frame->pixelColor[height*j+i];
            if(color!=251)
            {
                buffer[screenWidth*(y+j)+(x+i)]=color;
            }
        }
    }        
}

int CSprite::collision(CSprite *otherSprite) 
{
    int w1,h1,w2,h2,x1,y1,x2,y2;

    w1 = getWidth();
    h1 = getHeight(); 
    w2 = otherSprite->getWidth();
    h2 = otherSprite->getHeight();
    x1 = getPosX();
    y1 = getPosY();
    x2 = otherSprite->getPosX();
    y2 = otherSprite->getPosY();

    if (((x1+w1) > x2) && ((y1+h1) > y2) && ((x2+w2) > x1) && ((y2+h2) > y1)) 
    {
        return 1;
    } 
    else 
    {
        return 0;
    }
}
