/*---------------------------------------------------------------------------
--                          Vertical Shmup Demo                            -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: cenemies.cpp                                                     --
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

#include "cenemies.h"

CEnemySimple :: CEnemySimple(int nFrames, int nExplosionFrames) : CShip(nFrames, nExplosionFrames)
{
}

void CEnemySimple :: move(float distance, int screenWidth, int screenHeight)
{
    if (direction == RIGHT) 
    {
        setPosX(getPosX() + distance);
        
        if (getPosX() > screenWidth - frames[actualFrame].width)
        {
            direction = LEFT;
        }
    }
    else if (direction == LEFT)
    {
        setPosX(getPosX() - distance);
        
        if (getPosX() < 0)
        {
            direction = RIGHT;
        }
    }
}
