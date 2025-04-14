------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                          'V g a _ M a r t e'
--
--                                 Spec
--
--
--
--  File 'vga_marte.ads'                                              By MAR.
--
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------
with Interfaces.C;
with Interfaces.C.Strings;
with System;
--  with Tipos; use Tipos;
package Vga_Marte is

   type Point_T is record
      X : Interfaces.C.Unsigned;
      Y : Interfaces.C.Unsigned;
   end record;
   pragma Convention(C,Point_T);

   -- Mode constants --
   --------------------

   TEXT : constant Interfaces.C.Int :=0;
   G640x480x16 : constant Interfaces.C.Int :=4;
   G320x240x256 : constant Interfaces.C.Int :=6;

   -- Vendors --
   -------------

   VGA : constant Interfaces.C.Int :=1;

   -- Cards --
   -----------

   PCI_DEVICE_ID_S3_TRIO64V2: constant Interfaces.C.Int := 35073;


   -- Pixel transformations --
   ---------------------------

   function Rgb15
     (R : in Interfaces.C.Int;
      G : in Interfaces.C.Int;
      B : in Interfaces.C.Int)
     return Interfaces.C.Unsigned_Short;
   pragma Import(C,Rgb15,"rgb15");

   function Rgb16
     (R : in Interfaces.C.Int;
      G : in Interfaces.C.Int;
      B : in Interfaces.C.Int)
     return Interfaces.C.Unsigned_Short;
   pragma Import(C,Rgb16,"rgb16");

   function Rgb24
     (R : in Interfaces.C.Int;
      G : in Interfaces.C.Int;
      B : in Interfaces.C.Int)
     return Interfaces.C.Unsigned;
   pragma Import(C,Rgb24,"rgb24");

   function Rgb32
     (R : in Interfaces.C.Int;
      G : in Interfaces.C.Int;
      B : in Interfaces.C.Int)
     return Interfaces.C.Unsigned;
   pragma Import(C,Rgb32,"rgb32");

   function Rgba
     (R : in Interfaces.C.Int;
      G : in Interfaces.C.Int;
      B : in Interfaces.C.Int)
     return Interfaces.C.Unsigned;
   pragma Import(C,Rgba,"rgba");


   function Conversor_32_To_16
     (Rgb : in Interfaces.C.Unsigned)
     return Interfaces.C.Unsigned_Short;
   pragma Import(C,Conversor_32_To_16,"conversor_32_a_16");

   function Conversor_24_To_16
     (Rgb : in Interfaces.C.Unsigned)
     return Interfaces.C.Unsigned_Short;
   pragma Import(C,Conversor_24_To_16,"conversor_24_a_16");

   function Conversor_32_To_15
     (Rgb : in Interfaces.C.Unsigned)
     return Interfaces.C.Unsigned_Short;
   pragma Import(C,Conversor_32_To_15,"conversor_32_a_15");

   function Conversor_24_To_15
     (Rgb : in Interfaces.C.Unsigned)
     return Interfaces.C.Unsigned_Short;
   pragma Import(C,Conversor_24_To_15,"conversor_24_a_15");


   -- Initialize operations --
   ---------------------------

   function Init_Vga_With_Associated_Mem
     (Mode   : in Interfaces.C.Int;
      Vendor : in Interfaces.C.Int;
      Card   : in Interfaces.C.Int)
     return Interfaces.C.Int;
   pragma Import(C,Init_Vga_With_Associated_Mem,
                   "init_vga_with_associated_mem");

   function Init_Vga
     (Mode   : in Interfaces.C.Int;
      Vendor : in Interfaces.C.Int;
      Card   : in Interfaces.C.Int)
     return Interfaces.C.Int;
   pragma Import(C,Init_Vga,"init_vga");

   function Vga_Setmode
     (Mode : in Interfaces.C.Int)
     return Interfaces.C.Int;
   pragma Import(C,Vga_Setmode,"vga_setmode");


   -- Drawing operations --
   -----------------------
   function Vga_Setcolor
     (Color : in Interfaces.C.Int)
     return Interfaces.C.Int;
   pragma Import(C,Vga_Setcolor,"vga_setcolor");

   function Vga_Drawpixel
     (X : in Interfaces.C.Int;
      Y : in Interfaces.C.Int)
     return Interfaces.C.Int;
   pragma Import(C,Vga_Drawpixel,"vga_drawpixel");

   function Vga_Getpixel
     (X : in Interfaces.C.Int;
      Y : in Interfaces.C.Int)
     return Interfaces.C.Int;
   pragma Import(C,Vga_Getpixel,"vga_getpixel");

   --  WARNING: this import is not tested
   function Vga_Scroll_Ver
     (Point1 : in Point_T;
      Point2 : in Point_T;
      Lines  : in Interfaces.C.Int)
      return Interfaces.C.Int;
   pragma Import(C,Vga_Scroll_Ver,"vga_scroll_ver");

   --  WARNING: this import is not tested
   function Vga_Scroll_Hor
     (Point1 : in Point_T;
      Point2 : in Point_T;
      Lines  : in Interfaces.C.Int)
      return Interfaces.C.Int;
   pragma Import(C,Vga_Scroll_Hor,"vga_scroll_hor");

   function Vga_Drawscanline
     (Line : in Interfaces.C.Int;
      Colors : in System.Address) --Integer Pointer
     return Interfaces.C.Int;
   pragma Import(C,Vga_Drawscanline,"vga_drawscanline");

   function Vga_Drawscansegment
     (Colors : in System.Address; -- Integer Pointer
      X      : in Interfaces.C.Int;
      Y      : in Interfaces.C.Int;
      Length : in Interfaces.C.Int)
     return Interfaces.C.Int;
   pragma Import(C,Vga_Drawscansegment,"vga_drawscanline");

   procedure Vga_Pixel
     (Point : in Point_T;
      Color : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Pixel,"vga_pixel");
   pragma Import_Procedure
     (Internal        => Vga_Pixel,
      Parameter_Types =>
        (Point_T,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value));

   procedure Vga_Line
     (Point1 : in Point_T;
      Point2 : in Point_T;
      Color  : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Line,"vga_line");
   pragma Import_Procedure
     (Internal        => Vga_Line,
      Parameter_Types =>
        (Point_T,
         Point_T,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value, Value));

   procedure Vga_Rectangle
     (Point1 : in Point_T;
      Point2 : in Point_T;
      Color  : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Rectangle,"vga_rectangle");
   pragma Import_Procedure
     (Internal        => Vga_Rectangle,
      Parameter_Types =>
        (Point_T,
         Point_T,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value, Value));

   procedure Vga_Rectangle_Fill
     (Point1 : in Point_T;
      Point2 : in Point_T;
      Color  : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Rectangle_Fill,"vga_rectangle_fill");
   pragma Import_Procedure
     (Internal        => Vga_Rectangle_Fill,
      Parameter_Types =>
        (Point_T,
         Point_T,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value, Value));

   procedure Vga_Circumference
     (Centre : in Point_T;
      Radio : in Interfaces.C.Unsigned;
      Color : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Circumference,"vga_circumference");
   pragma Import_Procedure
     (Internal        => Vga_Circumference,
      Parameter_Types =>
        (Point_T,
         Interfaces.C.Unsigned,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value, Value));

   procedure Vga_Circle
     (Centre : in Point_T;
      Radio : in Interfaces.C.Unsigned;
      Color : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Circle,"vga_circle");
   pragma Import_Procedure
     (Internal        => Vga_Circle,
      Parameter_Types =>
        (Point_T,
         Interfaces.C.Unsigned,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value, Value));

   --  A correct way of using Vga_Text would be using a wrapper function like
   --  this:
   --     procedure Vga_Text_Wrapper
   --       (Text : in String;
   --        Point : in Vga_Marte.Point_T;
   --        Color_Foreground : in Interfaces.C.Unsigned;
   --        Color_Background : in Interfaces.C.Unsigned)
   --     is
   --        Chars_Ptr_Text : Interfaces.C.Strings.Chars_Ptr;
   --     begin
   --        Chars_Ptr_Text := Interfaces.C.Strings.New_String (Text);
   --        Vga_Marte.VGA_Text (Chars_Ptr_Text, Point,
   --                            Color_Foreground, Color_Background);
   --        Interfaces.C.Strings.Free (Chars_Ptr_Text);
   --     end Vga_Text_Wrapper;
   procedure Vga_Text
     (Text : in Interfaces.C.Strings.Chars_Ptr;
      Point : in Point_T;
      Color_foreground : in Interfaces.C.Unsigned;
      Color_background : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Text,"vga_text");
   pragma Import_Procedure
     (Internal        => Vga_Text,
      Parameter_Types =>
        (Interfaces.C.Strings.Chars_Ptr,
         Point_T,
         Interfaces.C.Unsigned,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value, Value, Value));

   procedure Vga_Ellipse
     (Point1 : in Point_T;
      Point2 : in Point_T;
      Color  : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Ellipse,"vga_ellipse");
   pragma Import_Procedure
     (Internal        => Vga_Ellipse,
      Parameter_Types =>
        (Point_T,
         Point_T,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value, Value));

   procedure Vga_Ellipse_Fill
     (Point1 : in Point_T;
      Point2 : in Point_T;
      Color  : in Interfaces.C.Unsigned);
   pragma Import(C,Vga_Ellipse_Fill,"vga_ellipse_fill");
   pragma Import_Procedure
     (Internal        => Vga_Ellipse_Fill,
      Parameter_Types =>
        (Point_T,
         Point_T,
         Interfaces.C.Unsigned),
      Mechanism       => (Value, Value, Value));

   procedure Draw_Image
     (Width    : in Interfaces.C.Unsigned;
      Height   : in Interfaces.C.Unsigned;
      Imageptr : in System.Address); --Integer Pointer
   pragma Import(C,Draw_Image,"draw_image_2");

   procedure Full_Blue_Screen;
   pragma Import(C,Full_Blue_Screen,"full_blue_screen");

   procedure Restoregray_Palette;
   pragma Import(C,Restoregray_Palette,"restoregray_palette");

   procedure Restorepalette_Default;
   pragma Import(C,Restorepalette_Default,"restorepalette_default");

end Vga_Marte;
