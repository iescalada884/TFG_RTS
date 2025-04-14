------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'C o n s o l e _ M a n a g e m e n t'
--
--                                 Body
--
--
--  File 'console_management.adb'                                     By MAR.
--
--  MaRTE on Linux version of this package.
--
--  Not complete functionality.
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
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with Text_IO;
package body Console_Management is

--   pragma Linker_Options ("console_management_c.o");

   function To_U8 is new Ada.Unchecked_Conversion (Source => Colors,
                                                   Target => Unsigned_8);
   function To_U8 is new Ada.Unchecked_Conversion (Source => BkColors,
                                                   Target => Unsigned_8);

   package Int_IO is new Text_IO.Integer_IO (Num => Integer);

   ------------
   --  Beep  --
   ------------
   procedure Beep is
   begin
      Text_IO.Put (Item => ASCII.BEL);
   end Beep;

   ----------------
   --  Put_Char  --
   ----------------
   --
   --  When trying to write fast to console some Device_Error
   --  exceptions appear. This procedure ignores them.
   procedure Put_Char (Item : Character) is
   begin
      loop
         begin
            Text_IO.Put (Item);
            exit;
         exception
            when Text_IO.Device_Error =>
               null; --  Retry
         end;
      end loop;
   end Put_Char;

   -----------------
   --  Put_Number --
   -----------------
   --
   --  When trying to write fast to console some Device_Error
   --  exceptions appear. This procedure ignores them.
   procedure Put_Number (N : Integer) is
   begin
      if N > 9 then
         loop
            begin
               Int_IO.Put  (Item => N / 10, Width => 1);
               exit;
            exception
               when Text_IO.Device_Error =>
                  raise; --  OJO
                  --  null; --  Retry
            end;
         end loop;
      end if;
      loop
         begin
            Int_IO.Put  (Item => N mod 10, Width => 1);
            exit;
         exception
            when Text_IO.Device_Error =>
               null; --  Retry
         end;
      end loop;
   end Put_Number;

   ----------------------------------------------------------------------------
   -- Change the Text Attributes ----------------------------------------------
   ----------------------------------------------------------------------------

   procedure Set_Text_Background_Color (BkC : in BkColors) is
   begin
      Put_Char (Item => ASCII.ESC);
      Put_Char (Item => '[');
      Put_Number  (Bkc'Enum_Rep);
      Put_Char (Item => 'm');
   end Set_Text_Background_Color;
   --  Establish backgroung color for the characters to be printed on
   --  screen.

   procedure Set_Text_Color (C : in Colors) is
   begin
      Put_Char (Item => ASCII.ESC);
      Put_Char (Item => '[');
      Put_Number  (C'Enum_Rep);
      Put_Char (Item => 'm');
   end Set_Text_Color;
   --  Establish foregroung color for the characters to be printed on
   --  screen.

   procedure Set_HighVideo is begin null; end Set_HighVideo;
   --  Establish high video for the characters to be printed on screen.

   procedure Set_LowVideo is begin null; end Set_LowVideo;
   --  Establish low video for the characters to be printed on screen.

   procedure Set_Blink is
   begin
      Put_Char (Item => ASCII.ESC);
      Put_Char (Item => '[');
      Put_Char (Item => '7');
      Put_Char (Item => 'm');
   end Set_Blink;
   --  Establish blinking mode for the characters to be printed on screen.

   procedure Cancel_Blink is begin null; end Cancel_Blink;
   --  Establish non-blinking mode for the characters to be printed on screen.



   ---------------------------------------------------------------------------
   -- Position the cursor ----------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Set_Cursor (To: in Position) is
   begin
      Set_Cursor (To.Row, To.Column);
   end Set_Cursor;

   procedure Set_Cursor (Row : in Rows; Column : in Columns) is
   begin
      Text_IO.New_Line;
      Put_Char (Item => ASCII.ESC);
      Put_Char ('[');
      Put_Number (Row + 1);
      Put_Char (Item => ';');
      Put_Number (Column + 1);
      Put_Char (Item => 'f');
   end Set_Cursor;
   --  Establish the position from where the characteres will be printed.


   ---------------------------------------------------------------------------
   -- Clear the screen -------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Clear_Screen is
   begin
      Put_Char (Item => ASCII.ESC);
      Put_Char (Item => '[');
      Put_Char (Item => '2');
      Put_Char (Item => 'J');
   end Clear_Screen;
   --  The whole screen takes the active background color.


   ---------------------------------------------------------------------------
   -- Console input configuration --------------------------------------------
   ---------------------------------------------------------------------------

   procedure Set_Cooked_Mode is begin null; end Set_Cooked_Mode;
   -- In this mode line editing is allowed, the textual unit of input is an
   -- entire "line" of text, where a "line" is a sequence of characters
   -- terminated by the line termination character CR. Thus, characters
   -- typed in this mode are not immediately made available to the calling
   -- program. They are first buffered to allow the user to perform line
   -- editing (erase characteres) on them.

   procedure Set_Raw_Mode is begin null; end Set_Raw_Mode;
   -- Every character is made available to the calling program as soon as
   -- it is typed, so no line editing is available in this mode.

   procedure Enable_Echo is
      procedure Console_Management_Enable_Echo;
      pragma Import (C, Console_Management_Enable_Echo,
                       "console_management_enable_echo");
   begin
      Console_Management_Enable_Echo;
   end Enable_Echo;
   -- Echo input characters

   procedure Disable_Echo is
      procedure Console_Management_Disable_Echo;
      pragma Import (C, Console_Management_Disable_Echo,
                       "console_management_disable_echo");
   begin
      Console_Management_Disable_Echo;
   end Disable_Echo;
   -- Input characters are not echoed

   procedure Set_Blocking_Mode is begin null; end Set_Blocking_Mode;
   --  Default behaviour. Applications wait in 'getchar' until there
   --  are characters available.

   procedure Reset_Blocking_Mode is begin null;   end Reset_Blocking_Mode;
   --  'getchar' returns -1 inmediately when there is no characters
   --  available at the moment of the call.

end Console_Management;
