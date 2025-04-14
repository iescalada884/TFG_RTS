/*---------------------------------------------------------------------------
--                          Vertical Shmup Demo                            -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: cship.cpp                                                        --
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

#include "cship.h"

CShip :: CShip()
{
    numExplosionFrames=1;
    actualExplosionFrame=0;
    explosionFrames=new CFrame[numExplosionFrames];
    maxMissiles=10;
    missiles=new CSprite[maxMissiles];  
    
    timeSinceExplosionFrameChange = 0;
}

CShip :: CShip(int nFrames, int nExplosionFrames) : CSprite(nFrames)
{ 
    numExplosionFrames=nExplosionFrames;
    actualExplosionFrame=0;
    explosionFrames=new CFrame[numExplosionFrames];
    maxMissiles=10;
    missiles=new CSprite[maxMissiles];  
    
    timeSinceExplosionFrameChange = 0;
}

void CShip :: draw(unsigned char *buffer, int screenWidth, float deltaTime)
{
    if(state == ACTIVE)
    {
        timeSinceExplosionFrameChange = 0;
        
        CSprite::draw(&frames[actualFrame], buffer,screenWidth);
        //CSprite::draw(&explosionFrames[actualExplosionFrame], buffer,screenWidth);
    }
    else if(state == EXPLODING)
    {
        if(timeSinceExplosionFrameChange > 0.1f)
        {
            timeSinceExplosionFrameChange = 0;
            actualExplosionFrame++;
        }
        else
        {
            timeSinceExplosionFrameChange += deltaTime;
        }
        
        if(actualExplosionFrame < numExplosionFrames)
        {
            CSprite::draw(&explosionFrames[actualExplosionFrame], buffer,screenWidth);            
            
        }
        else
        {
            state = FREE;
        }
    }
}

void CShip :: shotMissile()
{
    int i;
    int free = -1;

    // ¿Hay alguna bala libre?
    for (i=0 ; i<maxMissiles ; i++) 
    {
        if (missiles[i].state == FREE)
            free=i;
    }
    if (free>=0) 
    {
        missiles[free].setPosX(getPosX());
        missiles[free].setPosY(getPosY() - 15);

        missiles[free].state = ACTIVE;
    }
}

