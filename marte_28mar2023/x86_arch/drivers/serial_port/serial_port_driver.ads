------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'S e r i a l _ P o r t _ D r i v e r'
--
--                                  Spec
--
--
--  File 'serial_port_driver.ads'                             By Fguerreira
--
--
--  IOCTL options and other values used by the serial port
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

with MaRTE.Integer_Types; use MaRTE.Integer_Types;


package Serial_Port_Driver is


   subtype Byte         is MaRTE.Integer_Types.Unsigned_8;
   subtype Control_Char is MaRTE.Integer_Types.Unsigned_8;
   subtype Tc_Flag      is MaRTE.Integer_Types.Unsigned_32;
   subtype Speed        is MaRTE.Integer_Types.Unsigned_32;



   --  Magic control character value to disable the associated feature
   VALUE_DISABLE : constant MaRTE.Integer_Types.Unsigned_8 := 16#FF#;


   -------------
   --  Flags  --
   -------------

   --  Input
   type Input_Flags is (Ignore_Break, Break_Interrupt, Ignore_Parity,
                        Parity_Mark, Check_Parity, Strip_Bit, NL_To_NLCR,
                        Ignore_CR, CR_To_NL, Out_Flow_Control, In_Flow_Control,
                        Any_Char);

   Input_Flags_Mask : constant array (Input_Flags)
     of MaRTE.Integer_Types.Unsigned_32 :=
     (Ignore_Break     => 16#00000001#,    --  ignore BREAK condition
      Break_Interrupt  => 16#00000002#,    --  map BREAK to SIGINTR
      Ignore_Parity    => 16#00000004#,    --  ignore (discard) parity errors
      Parity_Mark      => 16#00000008#,    --  mark parity and framing errors
      Check_Parity     => 16#00000010#,    --  enable checking of parity errors
      Strip_Bit        => 16#00000020#,    --  strip 8th bit off chars
      NL_To_NLCR       => 16#00000040#,    --  map NL into CR
      Ignore_CR        => 16#00000080#,    --  ignore CR
      CR_To_NL         => 16#00000100#,    --  map CR to NL (ala CRMOD)
      Out_Flow_Control => 16#00000200#,    --  enable output flow control
      In_Flow_Control  => 16#00000400#,    --  enable input flow control
      Any_Char         => 16#00000800#);   --  any char will restart after stop


   --  Output
   type Output_Flags is (Output_Process, NL_To_NLCR);

   Output_Flags_Mask : constant array (Output_Flags)
     of MaRTE.Integer_Types.Unsigned_32 :=
     (Output_Process   => 16#00000001#,     --  enable following out processing
      NL_To_NLCR       => 16#00000002#);    --  map NL to CR-NL (ala CRMOD)


   --  Control
   type Control_Flags is (Char_Size_Mask, Char_Size_5, Char_Size_6,
                          Char_Size_7, Char_Size_8, Stop_Bits, Receiver_Ready,
                          Parity_Enable, Odd_Parity, Hang_Up, Ignore_Modem);

   Control_Flags_Mask : constant array (Control_Flags)
     of MaRTE.Integer_Types.Unsigned_32 :=
     (Char_Size_Mask   => 16#00000300#,    --  character size mask
      Char_Size_5      => 16#00000000#,    --  5 bits (pseudo)
      Char_Size_6      => 16#00000100#,    --  6 bits
      Char_Size_7      => 16#00000200#,    --  7 bits
      Char_Size_8      => 16#00000300#,    --  8 bits
      Stop_Bits        => 16#00000400#,    --  send 2 stop bits
      Receiver_Ready   => 16#00000800#,    --  enable receiver
      Parity_Enable    => 16#00001000#,    --  parity enable
      Odd_Parity       => 16#00002000#,    --  odd parity, else even
      Hang_Up          => 16#00004000#,    --  hang up on last close
      Ignore_Modem     => 16#00008000#);   --  ignore modem status lines


   --  Local
   type Local_Flags is (Echo_Erase, Echo_Kill, Echo_Enable, Echo_NL,
                        Signal_Enable, Canonic_Input, Enable_Discard,
                        Stop_Background, No_Flush);

   Local_Flags_Mask : constant array (Local_Flags)
     of MaRTE.Integer_Types.Unsigned_32 :=
     (Echo_Erase       => 16#00000002#,    --  visually erase chars
      Echo_Kill        => 16#00000004#,    --  echo NL after line kill
      Echo_Enable      => 16#00000008#,    --  enable echoing
      Echo_NL          => 16#00000010#,    --  echo NL even if ECHO is off
      Signal_Enable    => 16#00000080#,    --  enable signals INTR, QUIT, SUSP
      Canonic_Input    => 16#00000100#,    --  canonicalize input lines
      Enable_Discard   => 16#00000400#,    --  enable DISCARD and LNEXT
      Stop_Background  => 16#00400000#,    --  stop background jobs from output
      No_Flush         => 16#80000000#);   --  don't flush after interrupt


   --  Speed
   type Standard_Speeds is (B50, B75, B110, B150, B200, B300, B600, B1200,
                            B1800, B2400, B3600, B4800, B7200, B9600, B14400,
                            B19200, B28800, B38400, B57600, B115200);

   Speed_Values : constant array (Standard_Speeds)
     of MaRTE.Integer_Types.Unsigned_32 :=
     (B50     => 50,
      B75     => 75,
      B110    => 110,
      B150    => 150,
      B200    => 200,
      B300    => 300,
      B600    => 600,
      B1200   => 1200,
      B1800   => 1800,
      B2400   => 2400,
      B3600   => 3600,
      B4800   => 4800,
      B7200   => 7200,
      B9600   => 9600,
      B14400  => 14400,
      B19200  => 19200,
      B28800  => 28800,
      B38400  => 38400,
      B57600  => 57600,
      B115200 => 115200);


   --  Control Characters

   Control_Char_Number : constant MaRTE.Integer_Types.Unsigned_8 := 20;

   type Control_Chars_Set is (End_Of_File, End_Of_Line, Erase, Kill,
                              Interrupt, Quit, Suspend, Start, Stop,
                              Min, Time);

   Control_Char_Index : constant array (Control_Chars_Set)
     of MaRTE.Integer_Types.Unsigned_8 :=
     (End_Of_File =>  1,
      End_Of_Line =>  2,
      Erase       =>  4,
      Kill        =>  6,
      Interrupt   =>  9,
      Quit        => 10,
      Suspend     => 11,
      Start       => 13,
      Stop        => 14,
      Min         => 17,
      Time        => 18);

   type Control_Char_Array is
     array (1 .. Control_Char_Number) of Control_Char;

   Control_Characters_Default : constant Control_Char_Array :=
     (1  => Character'Pos ('D') - 64,    --  End_Of_File.
      2  => VALUE_DISABLE,               --  End_Of_Line.
      4  => Character'Pos ('H') - 64,    --  Erase.
      6  => Character'Pos ('U') - 64,    --  Kill.
      9  => Character'Pos ('C') - 64,    --  Interrupt.
      10 => Character'Pos ('\') - 64,    --  Quit.
      11 => Character'Pos ('Z') - 64,    --  Suspend.
      13 => Character'Pos ('Q') - 64,    --  Start.
      14 => Character'Pos ('S') - 64,    --  Stop
      others => 0);


   --  ---------------------------
   --  Serial Port 'Ioctl' Options
   --  ---------------------------
   --
   --  Any change here should be also performed in
   --  'include/drivers/serial_port_driver.h'
   --
   --  Third argument of ioctl function must be an access to a
   --  'Ioctl_Data' for values 'Set_Attributes', 'Get_Attributes',
   --  'Set_Speed' and 'Get_Speed'.
   --
   --  Third argument of ioctl function (arg) must be an access to a
   --  'Type_Timespec.Timespec' record for the value
   --  'Set_Relative_Timeout'.  A null pointer in this case means no
   --  timeout
   type Ioctl_Options is
     (Set_Attributes,      --  Ioctl_Options => 0
      --  Set attributes to be written into the UART regs

      Get_Attributes,         --  Ioctl_Options => 1
      --  Get attributes from the UART registers

      Set_Speed,              --  Ioctl_Options => 2
      --  Set input/output speed

      Get_Speed,              --  Ioctl_Options => 3
      --  Get input/output speed

      Enable_Interrupts,      --  Ioctl_Options => 4
      --  Enable reception and transmission interrupts

      Disable_Interrupts,     --  Ioctl_Options => 5
      --  Disable reception and transmission interrupts

      Get_Last_Line_Error,    --  Ioctl_Options => 6
      --  Get error bits in the line status register for the last line
      --  status error detected. An access to a unsigned byte is
      --  expected in the third argument of ioctl function

      Reset_Last_Line_Error,  --  Ioctl_Options => 7
      --  Reset last line error

      Flush,                  -- Ioctl_Options => 8
      --  Flush the buffer

      Set_Relative_Timeout    -- Ioctl_Options => 9
      --  Set the relative timeout for blocking read operations
      );



   --  ---------------------------
   --  Serial Port 'Ioctl' Data
   --  ------------------------

   type Ioctl_Data is record
      Input_Flag         : Tc_Flag;
      Output_Flag        : Tc_Flag;
      Control_Flag       : Tc_Flag;
      Local_Flag         : Tc_Flag;
      Control_Characters : Control_Char_Array;
      Input_Speed        : Speed;
      Output_Speed       : Speed;
   end record;


   --  Basic default termios settings for a typical raw-mode console.
   Raw_Mode : constant Ioctl_Data :=
     (Input_Flag         => 0,
      Output_Flag        => 0,
      Control_Flag       => Control_Flags_Mask (Char_Size_8),
      Local_Flag         => 0,
      Control_Characters => Control_Characters_Default,
      Input_Speed        => Speed_Values (B9600),
      Output_Speed       => Speed_Values (B9600)
      );


   --  Basic default termios settings for a typical cooked-mode console.
   Cooked_Mode : constant Ioctl_Data :=
     (Input_Flag  =>
        Input_Flags_Mask (CR_To_NL) or Input_Flags_Mask (Out_Flow_Control),

      Output_Flag  => Output_Flags_Mask (Output_Process),

      Control_Flag => Control_Flags_Mask (Char_Size_8),

      Local_Flag  =>
        Local_Flags_Mask (Echo_Enable) or Local_Flags_Mask (Echo_Erase) or
      Local_Flags_Mask (Echo_Kill) or Local_Flags_Mask (Canonic_Input),

      Control_Characters => Control_Characters_Default,

      Input_Speed  => Speed_Values (B9600),

      Output_Speed => Speed_Values (B9600)
      );

end Serial_Port_Driver;

