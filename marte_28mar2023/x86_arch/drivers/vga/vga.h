/*---------------------------------------------------------------------------
--                              VGA driver                                 -- 
-----------------------------------------------------------------------------
--                                                                         --
--  My VGA driver for MaRTE OS                                             --
--                                                                         --
--  author:   Álvaro García                                                --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: vga.h                                                            --
--                                                                         --
--                                                                         --
-----------------------------------------------------------------------------
--                      last update: 01 May 2009                           --
---------------------------------------------------------------------------*/

#define TEXT_MODE 0
#define G320x200x256 1
#define G320x240x256 2  // mode x

extern void vga_pixel(int x,int y,int color);
extern void vga_clear_screen();
extern void vga_clear_screen_modex();
extern void vga_setpalette(int number, int r, int g, int b);
extern void vga_init(int mode);
