/*---------------------------------------------------------------------------
--                          Vertical Shmup Demo                            -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: cenemies.h                                                       --
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

#ifndef CENEMIES_H
#define CENEMIES_H

#include "cship.h"

//10 normal 9 zigzag one boss

class CEnemySimple : public CShip
{
    private:
        
    public:
        
        CEnemySimple(int nFrames, int nExplosionFrames);
        void move(float distance, int screenWidth, int screenHeight); 
};

#endif
