/*---------------------------------------------------------------------------
--                          Vertical Shmup Demo                            -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: cship.h                                                          --
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

#ifndef CSHIP_H
#define CSHIP_H

#include "vga.h"
#include <stdio.h>

#include "cframe.h"
#include "csprite.h"


class CShip : public CSprite
{
    private:
        
    public:
    //         states state;
    //         directions direction;
        CFrame *explosionFrames;
        int numExplosionFrames;
        int actualExplosionFrame;
        float timeSinceExplosionFrameChange;
        
        int maxMissiles;
        CSprite *missiles;        
        
        CShip();
        CShip(int nFrames, int nExplosionFrames);
        
        void draw(unsigned char *buffer, int screenWidth, float deltaTime = 0);
        void shotMissile();
};

#endif
