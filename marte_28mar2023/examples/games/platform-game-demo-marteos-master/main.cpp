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
--                        last update: 08 Oct 09                           --
---------------------------------------------------------------------------*/

#include "cgame.h"

int main(){
    
       CGame game;
       
       game.Init();
       game.GameLoop();              
       
       return 0;

}//main

/* NOTES

//changes in marte 1.9 Jan:
//vga.h CPP_BEGIN_DECLS CPP_END_DECLS extern int SEQ01;
//extern unsigned char CR11,CR38,CR39,CR40;
//misc/console_management.h CPP_BEGIN_DECLS CPP_END_DECLS
//had to make extern char str_timespec_s[40]; in misc/timespec_operations.h
// definimos el tamaño de la pantalla

DOUBLE BUFFERING

http://en.wikipedia.org/wiki/Double_buffering#Double_buffering_in_computer_graphics

Double buffering in computer graphics

In computer graphics, double buffering (sometimes called ping-pong buffering) is a technique used to reduce or remove visible artifacts from the drawing process. It may be implemented in either software or hardware.

Computer monitors constantly redraw the visible video page (at around 60 times a second), and so it is difficult to make changes to the video page (such as creation or movement of complex objects onscreen) without the monitor showing the partial results before the graphics operation is complete. This often results in ugly artifacts such as flickering and tearing.

A software implementation of double buffering uses a video page stored in system RAM that all drawing operations are written to. When a drawing operation is considered complete, the whole page, or a portion of it, is copied into the video RAM (VRAM) in one operation. This is generally synchronised so that copy operation is ahead of the monitor's raster beam so that ideally (if the copy is faster than the video beam) artifacts are avoided. This software method is not always flawless, and has a higher overhead than the hardware method. Most notably, double buffering necessarily requires more video memory and CPU time than single buffering because of the video memory allocated for the buffer itself and the synchronisation copy respectively.

The hardware method is also known as page flipping. In this method, two graphics pages in VRAM are used. At any one time, one page is actively being displayed by the monitor, while the other, background page is being drawn. When drawing is complete, the roles of the two pages are switched, so that the previously shown page is now being modified, and the previously drawn page is now being shown. The page-flip is typically accomplished by modifying the value of a pointer to the beginning of the display data in the video memory.

The hardware method guarantees artifacts will not be seen as long as the pages are switched over during the monitor's vertical blank period when no video data is being drawn. This method requires twice the amount of VRAM that is required for a single video page. The currently active and visible buffer is called the front buffer, while the background page is called the back buffer.
///////////////
http://personales.mundivia.es/jap/demlinux.htm

Pintando en la pantalla

Bien, lo que hagamos ahora depende de como empezemos a programar. Podemos escribir directamente en la memoria de video, o hacer un buffer en memoria conveccional y luego volcarlo a la memoria de video. Yo me inclino por la segunda, aunque la funcion vga_drawpixel(), y vga_drawline() estan definidas en el vga.h

Para crear un buffer, simplemente hay que llamar a la funcion malloc(), y reservar la memoria necesaria. Comio estamos en 32 bits y todo eso, nos olvidamos de tamaños y segmentos...

// definimos el tamaño de la pantalla
#define VGASIZE 320*200

// definimos un puntero
unsigned char *buffer;

// reservamos memoria
         buffer = (unsigned char *)malloc(VGASIZE);
// limpiamos el buffer
         memset(buffer, 0, VGASIZE);

Una vez que hemos hecho todo lo que queremos en este buffer (cargar un dibujo, pintar unas lineas, etc) tenemos que volcar este buffer a video. Segun el profiler, esta función no es muy lenta, y si tu tarjeta lo permite, incluso creo que usa las funciones de aceleración, si estan disponibles, con lo que la velocidad se incrementa realmente...

Para volcar a video, no es necesario saber la dirección base, ni cosas de esas, simplemente se hace una copia:

// volcamos a video
vga_drawscansegment(buffer,0,0,VGASIZE);

buffer: es el puntero al buffer que reservamos antes.

0,0: son las coordenadas de la esquina superior donde queremos copiar. O sea, la esquina superior.

VGASIZE: el numero de bytes que hemos definido antes. Es el tamaño del buffer (en este caso 320x200)

Con esto ya tenemos nuestro dibujo en la pantalla. Si queremos hacer animación, nos sera util esperar por el retrazo de la pantalla. Para ello disponemos de la función

   // esperamos por el retrazo
     vga_waitretrace();

que no tiene parametros. Esta función hace que la animación no parpadee, si va demasiado deprisa.
*/
