/*---------------------------------------------------------------------------
--                          Vertical Shmup Demo                            -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: keyboard.h                                                       --
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


#ifndef KEYBOARD_H
#define KEYBOARD_H

    extern unsigned char pressedKeys[128];

    void init_keyboard();

#endif
