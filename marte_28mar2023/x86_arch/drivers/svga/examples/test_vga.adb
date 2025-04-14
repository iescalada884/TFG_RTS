--  To compile this program execute:
--
--  $ mgnatmake test_vga.adb -largs -lsvga -lm
--
with VGA_Marte;
use VGA_Marte;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

procedure Test_VGA is
   pragma linker_options ("-ldrivers -lsvga -lm");

   use type Interfaces.C.Int;

   Error : Interfaces.C.Int;
   Point1 : Point_T;

   Init_Error, Color_Error, Mode_Error : exception;
   Chars_Ptr_Text : Interfaces.C.Strings.Chars_Ptr;

begin

   Error:=Init_Vga(G320X240X256,VGA,PCI_DEVICE_ID_S3_TRIO64V2);
   if Error/=0 then
      raise Init_Error;
   end if;

   --Drawing all our palette
   for I in Interfaces.C.Unsigned range 0..239 loop
      --Error:=vga_setcolor(i);
      --if Error/=0 then
      --   raise Color_Error;
      --end if;
      Vga_Line((0,I),(319,I),I);
   end loop;
   delay 5.0;

   --Changing only the palette
   Restoregray_Palette;

   --Drawing a gray line
   Error:=Vga_Setcolor(14);
   if Error/=0 then
      raise Color_Error;
   end if;
   for I in Interfaces.C.Int range 5..99 loop
      Error:=Vga_Drawpixel(I,5);
   end loop;

   Error:=Vga_Setcolor(13);
   if Error/=0 then
      raise Color_Error;
   end if;
   for I in Interfaces.C.Int range 5..99 loop
      Error:=Vga_Drawpixel(7,I);
   end loop;
   delay 5.0;

   --All the screen blue
   Restorepalette_Default;
   Full_Blue_Screen;

   -- write text
   Chars_Ptr_Text := Interfaces.C.Strings.New_String ("hello");
   Vga_Marte.VGA_Text (Chars_Ptr_Text, (10,10), 0, 1);
   Interfaces.C.Strings.Free (Chars_Ptr_Text);

   delay 5.0;

   --Drawing a circumference and a circle
   Point1.X:=160;
   Point1.Y:=120;

   Vga_Circumference(Point1,80,7);
   Vga_Circle(Point1,50,12);
   delay 5.0;

   -- Text mode
   Error:=Vga_Setmode(TEXT);
   if Error/=0 then
      raise Mode_Error;
   end if;
   Put_Line("That is all friends");

exception
   when E:others =>
      -- Text mode
      Error:=Vga_Setmode(TEXT);
      Put_Line(Ada.Exceptions.Exception_Message(E));
      Vga_Text (Interfaces.C.Strings.New_String
                (Ada.Exceptions.Exception_Message(E)), (10,10), 0, 1);

end Test_VGA;
