------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'C o n s o l e _ M a n a g e m e n t'
--
--                                 Spec
--
--
--  File 'console_management.ads'                                     By MAR.
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
package Console_Management is

   subtype Rows is Natural range 0 .. 24;

   subtype Columns is Natural range 0 .. 79;

   type Position is
     record
       Row    : Rows;
       Column : Columns;
     end record;
   pragma Convention (C, Position);

   -- Upper left corner of the screen
   UpLeft_Position   : constant Position := (Row => 0, Column => 0);
   -- Lower right corner of the screen
   DownRight_Position: constant Position := (Row    => Rows'Last,
                                             Column => Columns'Last);

   -- Text (foreground) Colors
   type Colors is
     (Black,
      Red,
      Green,
      Brown,
      Blue,
      Magenta,
      Cyan,
      LightGray,
      DarkGray,
      LightRed,
      LightGreen,
      Yellow,
      LightBlue,
      LightMagenta,
      LightCyan,
      White);

   -- Background colors
   type BkColors is
      (Black,
       Red,
       Green,
       Brown,
       Blue,
       Magenta,
       Cyan,
       LightGray);

   ------------
   --  Beep  --
   ------------
   procedure Beep;

   ---------------------------------------------------------------------------
   -- Change the Text Attributes ----------------------------------------------
   ----------------------------------------------------------------------------

   procedure Set_Text_Background_Color (BkC : in BkColors);
   pragma Export (C, Set_Text_Background_Color, "set_text_background_color");
   --  Establish backgroung color for the characters to be printed on
   --  screen.

   procedure Set_Text_Color (C : in Colors);
   pragma Export (C, Set_Text_Color, "set_text_color");
   --  Establish foregroung color for the characters to be printed on
   --  screen.

   procedure Set_HighVideo;
   pragma Export (C, Set_HighVideo, "set_highvideo");
   --  Establish high video for the characters to be printed on screen.

   procedure Set_LowVideo;
   pragma Export (C, Set_LowVideo, "set_lowvideo");
   --  Establish low video for the characters to be printed on screen.

   procedure Set_Blink;
   pragma Export (C, Set_Blink, "set_blink");
   --  Establish blinking mode for the characters to be printed on screen.

   procedure Cancel_Blink;
   pragma Export (C, Cancel_Blink, "cancel_blink");
   --  Establish non-blinking mode for the characters to be printed on screen.



   ---------------------------------------------------------------------------
   -- Position the cursor ----------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Set_Cursor (To : in Position);
   pragma Export (C, Set_Cursor, "set_cursor");
   pragma Export_Procedure (Internal => Set_Cursor, External => "set_cursor",
                              Mechanism => Reference);
   procedure Set_Cursor (Row : in Rows; Column : in Columns);
   --  Establish the position from where the characteres will be printed.


   ---------------------------------------------------------------------------
   -- Clear the screen -------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Clear_Screen;
   pragma Export (C, Clear_Screen, "clrscr");
   --  The whole screen takes the active background color.


   ---------------------------------------------------------------------------
   -- Console input configuration --------------------------------------------
   ---------------------------------------------------------------------------

   procedure Set_Cooked_Mode;
   pragma Export (C, Set_Cooked_Mode, "set_cooked_mode");
   -- In this mode line editing is allowed, the textual unit of input is an
   -- entire "line" of text, where a "line" is a sequence of characters
   -- terminated by the line termination character CR. Thus, characters
   -- typed in this mode are not immediately made available to the calling
   -- program. They are first buffered to allow the user to perform line
   -- editing (erase characteres) on them.

   procedure Set_Raw_Mode;
   pragma Export (C, Set_Raw_Mode, "set_raw_mode");
   -- Every character is made available to the calling program as soon as
   -- it is typed, so no line editing is available in this mode.

   procedure Enable_Echo;
   pragma Export (C, Enable_Echo, "enable_echo");
   -- Echo input characters

   procedure Disable_Echo;
   pragma Export (C, Disable_Echo, "disable_echo");
   -- Input characters are not echoed

   procedure Set_Blocking_Mode;
   pragma Export (C, Set_Blocking_Mode, "set_blocking_mode");
   --  Default behaviour. Applications wait in 'getchar' until there
   --  are characters available.

   procedure Reset_Blocking_Mode;
   pragma Export (C, Reset_Blocking_Mode, "reset_blocking_mode");
   --  'getchar' returns -1 inmediately when there is no characters
   --  available at the moment of the call.

private
     --  Foreground colors
     for Colors use
       (Black        => 30,
        Red          => 31,
        Green        => 32,
        Brown        => 33,
        Blue         => 34,
        Magenta      => 35,
        Cyan         => 36,
        LightGray    => 37,
        DarkGray     => 90,
        LightRed     => 91,
        LightGreen   => 92,
        Yellow       => 93,
        LightBlue    => 94,
        LightMagenta => 95,
        LightCyan    => 96,
        White        => 97);
     for Colors'Size use 8;

     -- Background colors
     for BkColors use
        (Black        => 40,
         Red          => 41,
         Green        => 42,
         Brown        => 43,
         Blue         => 44,
         Magenta      => 45,
         Cyan         => 46,
         LightGray    => 47);
     for BkColors'Size use 8;
end Console_Management;
