/*---------------------------------------------------------------------------
--                          Vertical Shmup Demo                            -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: main.cpp                                                         --
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

#include "cgame.h"
//changes in marte 1.9 Jan:
//vga.h CPP_BEGIN_DECLS CPP_END_DECLS extern int SEQ01;
//extern unsigned char CR11,CR38,CR39,CR40;
//misc/console_management.h CPP_BEGIN_DECLS CPP_END_DECLS
//had to make extern char str_timespec_s[40]; in misc/timespec_operations.h

int main(void)
{    
    CGame game;
    game.gameLoop();              

    return 0;
}//main
