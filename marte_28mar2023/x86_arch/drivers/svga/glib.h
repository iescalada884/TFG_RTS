/*
 * Project: S.Ha.R.K.
 *
 * Coordinators: 
 *   Giorgio Buttazzo    <giorgio@sssup.it>
 *   Paolo Gai           <pj@gandalf.sssup.it>
 *
 * Authors     : 
 *   Paolo Gai           <pj@gandalf.sssup.it>
 *   Massimiliano Giorgi <massy@gandalf.sssup.it>
 *   Luca Abeni          <luca@gandalf.sssup.it>
 *   (see the web pages for full authors list)
 *
 * ReTiS Lab (Scuola Superiore S.Anna - Pisa - Italy)
 *
 * http://www.sssup.it
 * http://retis.sssup.it
 * http://shark.sssup.it
 */

/**
 ------------
 CVS :        $Id: glib.h 98 2006-01-26 18:45:04Z mario $

 File:        $File$
 Revision:    $Revision: 98 $
 Last update: $Date: 2006-01-26 19:45:04 +0100 (jue, 26 ene 2006) $
 ------------

**/

/*
 * Copyright (C) 2000 Luca Abeni
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#ifndef __GLIB_H__
#define __GLIB_H__

/*<MaRTE OS*/
//#include <ll/sys/types.h>
//#include "ll/sys/cdefs.h"
#include "tipos.h"
/*MaRTE OS>*/

int grx_setbuffer(BYTE *vbuf,BYTE bpp, WORD w, WORD h);

void grx_pixel(WORD x, WORD y, DWORD color);
DWORD grx_getpixel(WORD x, WORD y);
//void grx_getimage(WORD x1, WORD y1, WORD x2, WORD y2, BYTE *buf);
//void grx_putimage(WORD x1, WORD y1, WORD x2, WORD y2, BYTE *buf);
void grx_rectangle(WORD x1, WORD y1, WORD x2, WORD y2, DWORD color);
void grx_rectangle_fill(WORD x1, WORD y1, WORD x2, WORD y2, DWORD color);
void grx_line(WORD x1, WORD y1, WORD x2, WORD y2, DWORD color);
void grx_text(char *text, WORD x, WORD y, DWORD fg, DWORD bg);
void grx_circle(WORD sx, WORD sy, WORD r, DWORD c);
void grx_circumference(WORD sx, WORD sy, WORD r, DWORD c);
void grx_clear(DWORD color);

#endif
