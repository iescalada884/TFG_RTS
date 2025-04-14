/*---------------------------------------------------------------------------
--                          Vertical Shmup Demo                            -- 
-----------------------------------------------------------------------------
--                                                                         --
--  This is an example demo for MaRTE OS.                                  --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: keyboard.c                                                       --
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

#include <intr.h> // may need to add CPP_BEGIN_DECLS & CPP_END_DECLS in this marte's file
//#include <stdio.h>
#include <sys/pio.h>

#include "keyboard.h"

unsigned char pressedKeys[128] = {0};

static int gamingKBD_irq_handler (void *area, intr_t irq)
{
    unsigned char scancode = inb(0x60);
    if (scancode & 0x80)
    {//key released
        pressedKeys[scancode - 128] = 0;
    }
    else
    {//key pressed
        pressedKeys[scancode] = 1;
    }
    //return POSIX_INTR_HANDLED_NOTIFY;
    //return POSIX_INTR_HANDLED_DO_NOT_NOTIFY;
    return 0;
}

void init_keyboard()
{
    
//     int i;
//     for(i=0;i<128;i++){
//         pressedKeys[i]=0;
//     }
    
    posix_intr_associate(KEYBOARD_HWINTERRUPT, gamingKBD_irq_handler, ((void *)0), 0); //((void *)0)==NULL
    
}


/*int posix_intr_associate (intr_t intr,
                          int (*intr_handler) (void * area, intr_t intr),
                          volatile void * area,
                          size_t areasize);*/
//posix_intr_associate(int, int (*)(void*, int), void volatile*, unsigned int)

