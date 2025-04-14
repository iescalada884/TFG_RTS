------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V1.58  Sep 2006
--
--                            'P S / 2 _ M o u s e'
--
--                                   Spec
--
--
--  File 'ps2_mouse.ads'                                               By AMC.
--
--  -- Information:
--  -- Spec for the PS/2 Mouse Driver
--  ---------------------------------------------------------------------------
--   Copyright (C) 2003   Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@ctr.unican.es
--                      Michael Gonz�lez Harbour      mgh@ctr.unican.es
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
with System;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with MaRTE.Integer_Types;
with Drivers_MaRTE; use Drivers_MaRTE;
with MaRTE.HAL.IO; use MaRTE.HAL.IO;
with MaRTE.HAL; use MaRTE.HAL;
with MaRTE_Semaphores;
with Debug_Marte; use Debug_Marte; --  For debugging
with FIFO_Queue; pragma Elaborate_All (FIFO_Queue);

package PS2_Mouse is

   subtype Byte is MaRTE.Integer_Types.Unsigned_8;

   --  V. 0.0.1.
   MOUSE_DRV_VER : constant MaRTE.Integer_Types.Unsigned_32 := 16#00000001#;

   ---------------------------
   -- 8042 controller ports --
   ---------------------------
   Input_Buffer     : constant MaRTE.HAL.IO.IO_Port := 16#0060#;
   Output_Buffer    : constant MaRTE.HAL.IO.IO_Port := 16#0060#;
   Status_Register  : constant MaRTE.HAL.IO.IO_Port := 16#0064#;
   Control_Register : constant MaRTE.HAL.IO.IO_Port := 16#0064#;

   -------------------------------
   -- Type 'Flag' for registers --
   -------------------------------
   --  An alternative representation for Register's flags
   --  could be the Boolean type.
   --  However, this time we'll use the enumerated type 'Flag'
   type Flag is (Cleared, Set);
   for Flag use (Cleared => 0, Set => 1);
   for Flag'Size use 1;

   ---------------------------------------------
   -- Status Register in PS/2-Compatible Mode --
   ---------------------------------------------
   type Status_Reg is record
      OBF  :  Flag;   --  Output Buffer Full Flag
      IBF  :  Flag;   --  Input Buffer Full Flag
      SYS  :  Flag;   --  System Flag
      A2   :  Flag;   --  Address Line A2 (Command / Data Flag)
      INH  :  Flag;   --  Inhibit Flag
      MOBF : Flag;   --  Mouse Output Buffer Full Flag
      TO   : Flag;   --  General Timeout Flag
      PERR : Flag;   --  Parity Error Flag
   end record;
   for Status_Reg use record
      OBF   at 0 range 0 .. 0;
      IBF   at 0 range 1 .. 1;
      SYS   at 0 range 2 .. 2;
      A2    at 0 range 3 .. 3;
      INH   at 0 range 4 .. 4;
      MOBF  at 0 range 5 .. 5;
      TO    at 0 range 6 .. 6;
      PERR  at 0 range 7 .. 7;
   end record;
   for Status_Reg'Size use 8;
   for Status_Reg'Alignment use 2;
   for Status_Reg'Bit_Order use System.Low_Order_First; -- bit 0 is the LSB

   ------------------------------------------
   -- Command Byte in PS/2-Compatible Mode --
   ------------------------------------------

   type Command_Byte is record
      INT        : Flag;   --  Input Buffer Full Interrupt Flag
      INT2       : Flag;   --  Mouse Input Buffer Full Interrupt Flag
      SYS        : Flag;   --  System Flag
      RESERVED_1 : Flag;
      EN         : Flag;   --  Enable (0) & Disable (1) Keyboard
      EN2        : Flag;   --  Enable (0) & Disable (1) Mouse
      XLAT       : Flag;   --  Translate Scan Codes Flag
      RESERVED_2 : Flag;
   end record;
   for Command_Byte use record
      INT        at 0 range 0 .. 0;
      INT2       at 0 range 1 .. 1;
      SYS        at 0 range 2 .. 2;
      RESERVED_1 at 0 range 3 .. 3;
      EN         at 0 range 4 .. 4;
      EN2        at 0 range 5 .. 5;
      XLAT       at 0 range 6 .. 6;
      RESERVED_2 at 0 range 7 .. 7;
   end record;
   for Command_Byte'Size use 8;
   for Command_Byte'Alignment use 2;
   for Command_Byte'Bit_Order use System.Low_Order_First;

   ------------------------------------
   -- 8042 controller basic commands --
   ------------------------------------

   Read_Command_Byte          : constant Byte := 16#20#;
   Write_Command_Byte         : constant Byte := 16#60#;
   Get_Version_Number         : constant Byte := 16#A1#;
   Get_Password               : constant Byte := 16#A4#;
   Set_Password               : constant Byte := 16#A5#;
   Check_Password             : constant Byte := 16#A6#;
   Disable_Mouse_Interface    : constant Byte := 16#A7#;
   Enable_Mouse_Interface     : constant Byte := 16#A8#;
   Mouse_Interface_Test       : constant Byte := 16#A9#;
   Controller_Self_Test       : constant Byte := 16#AA#;
   Keyboard_Interface_Test    : constant Byte := 16#AB#;
   Disable_Keyboard_Interface : constant Byte := 16#AD#;
   Enable_Keyboard_Interface  : constant Byte := 16#AE#;
   Get_Version                : constant Byte := 16#AF#;
   Write_Mouse_Device         : constant Byte := 16#D4#;
   --  Sends commands to auxiliary PS/2 device
   --  Useful values for the Command Byte
   Mouse_Interrupts_On  : constant Byte := 16#47#; --  Enable Mouse Interr.
   Mouse_Interrupts_Off : constant Byte := 16#65#; --  Disable Mouse Interr.

   -----------------------------------
   -- PS/2 Mouse Modes of Operation --
   -----------------------------------
   type Mode_Of_Operation is
     (Reset_Mode, Stream_Mode, Remote_Mode, Wrap_Mode);

   -------------------------------------------
   -- PS/2 Standard Mouse Accepted Commands --
   -------------------------------------------
   --  Standard PS2 Mouse Commands --
   type Standard_Command is
     (Set_Scaling_1_1,
      Set_Scaling_2_1,
      Set_Resolution,
      Status_Request,
      Set_Stream_Mode,
      Read_Data,
      --   Reset_Wrap_Mode, -- Not yet implemented
      --   Set_Wrap_Mode,   -- Not yet implemented
      Set_Remote_Mode,
      Get_Device_ID,
      Set_Sample_Rate,
      Enable_Data_Reporting,
      Disable_Data_Reporting,
      Set_Defaults,
      --   Resend,      -- Not yet implemented
      Reset);
   for Standard_Command use
     (Set_Scaling_1_1        => 16#E6#,
      Set_Scaling_2_1        => 16#E7#,
      Set_Resolution         => 16#E8#,
      Status_Request         => 16#E9#,
      Set_Stream_Mode        => 16#EA#,
      Read_Data              => 16#EB#,
      --  Reset_Wrap_Mode    => 16#EC#,
      --  Set_Wrap_Mode      => 16#EE#,
      Set_Remote_Mode        => 16#F0#,
      Get_Device_ID          => 16#F2#,
      Set_Sample_Rate        => 16#F3#,
      Enable_Data_Reporting  => 16#F4#,
      Disable_Data_Reporting => 16#F5#,
      Set_Defaults           => 16#F6#,
      --  Resend             => 16#FE#,
      Reset                  => 16#FF#);
   for Standard_Command'Size use 8;

--     A former representation of PS/2 Commands:
--     Reset:           constant Byte := 16#FF#;
--     Resend:          constant Byte := 16#FE#;
--     Set_Defaults:        constant Byte := 16#F6#;
--     Disable_Data_Reporting:  constant Byte := 16#F5#;
--     Enable_Data_Reporting:   constant Byte := 16#F4#;
--     Set_Sample_Rate:     constant Byte := 16#F3#;
--     Get_Device_ID:       constant Byte := 16#F2#;
--     Set_Remote_Mode:     constant Byte := 16#F0#;
--     Set_Wrap_Mode:       constant Byte := 16#EE#;
--     Reset_Wrap_Mode:     constant Byte := 16#EC#;
--     Read_Data:       constant Byte := 16#EB#;
--     Set_Stream_Mode:     constant Byte := 16#EA#;
--     Status_Request:      constant Byte := 16#E9#;
--     Set_Resolution:      constant Byte := 16#E8#;
--     Set_Scaling_2_1:     constant Byte := 16#E7#;
--     Set_Scaling_1_1:     constant Byte := 16#E6#;

   --------------------------
   -- PS/2 Mouse Responses --
   --------------------------
   type Response is
     (Ret_PS2_ID,
      Ret_BAT,
      Ret_ACK,
      Ret_ERR,
      Ret_NAK);
   for Response use
     (Ret_PS2_ID => 16#00#,
      Ret_BAT    => 16#AA#,
      Ret_ACK    => 16#FA#,
      Ret_ERR    => 16#FC#,
      Ret_NAK    => 16#FE#);
   for Response'Size use 8;

   ----------------------
   -- PS/2 Mouse Types --
   ----------------------
   type Mouse_Type is
     (PS2_Standard,  --  -> The only implemented one
      PS2PP,
      PS2TPP,
      GENPS,
      IMPS,
      IMEX,
      SYNAPTICS);
   for Mouse_Type use
     (PS2_Standard  => 1,
      PS2PP     => 2,
      PS2TPP        => 3,
      GENPS     => 4,
      IMPS      => 5,
      IMEX      => 6,
      SYNAPTICS => 7);

   -----------------------------
   -- PS/2 Mouse Control Byte --
   -----------------------------
   type PS2_Mouse_Buttons is mod 8;
   for PS2_Mouse_Buttons'Size use 3;

   type Control_Byte is record
      Y_Ovf   : Boolean;
      X_Ovf   : Boolean;
      Y_Sign  : Boolean;
      X_Sign  : Boolean;
      Cntnt   : Boolean;  --  must be True (=1)
      Buttons : PS2_Mouse_Buttons;
      --  Detail of Buttons :
      ------------------------------------------------
      -- Middle_Button   Right_Button   Left_Button --
      ------------------------------------------------
   end record;
   for Control_Byte use record
      Buttons   at 0 range 0 .. 2;
      Cntnt     at 0 range 3 .. 3;
      X_Sign    at 0 range 4 .. 4;
      Y_Sign    at 0 range 5 .. 5;
      X_Ovf     at 0 range 6 .. 6;
      Y_Ovf     at 0 range 7 .. 7;
   end record;
   for Control_Byte'Size use 8;
   for Control_Byte'Alignment use 2;
   for Control_Byte'Bit_Order use System.Low_Order_First;

   --------------------------
   -- Conversion Functions --
   --------------------------

   function Unsigned_8_To_Reg is new
     Ada.Unchecked_Conversion
       (MaRTE.Integer_Types.Unsigned_8, Status_Reg);

   function Unsigned_8_To_Control_Byte is new
     Ada.Unchecked_Conversion
       (MaRTE.Integer_Types.Unsigned_8, Control_Byte);

   ---------------------------------------------------------------------------
   -- General Constants ------------------------------------------------------
   ---------------------------------------------------------------------------

   PS2_Packet_Size : constant := 3;
   --  Size (in bytes) of the mouse packet using the standard PS2 protocol
   --  Change this value when implementing any other protocol

   Mouse_Buffer_Size : constant := 512;
   --  Maximum number of mouse packets in input buffer.
   --  Must be greater than 2
   --  A power of 2 value is recommended

   Max_Retries : constant := 500000;
   --  Maximum number of retries of every operation in the driver

   Poll_Retries_Constant : constant := 60;
   --  Constant used for Default Retries when polling the 8042 controller
   --  fields

   Max_Delay : constant Duration := 0.500;
   --  Maximum Delay of every operation in the driver

   Poll_Delay_Constant : constant Duration := 0.030;
   --  Constant used for Default Time Delay when polling the 8042 Controller
   --  fields

   ------------------------------------------------
   -- Standard PS/2 Mouse Data Packet Definition --
   ------------------------------------------------
   type Packet_Bytes is array (1 .. PS2_Packet_Size) of Byte;
   for Packet_Bytes'Size use PS2_Packet_Size * Byte'Size;
   pragma Pack (Packet_Bytes);

   type Mouse_Event is record
      Timestamp  :  MaRTE.HAL.HWTime;
      Mouse_Byte :  Packet_Bytes;
   end record;
   pragma Pack (Mouse_Event);

   type Time_Byte_Array is array
                        (1 .. (MaRTE.HAL.HWTime'Size) / 8) of Byte;
   for Time_Byte_Array'Size use MaRTE.HAL.HWTime'Size;

   function To_Time_Byte_Array is new
     Ada.Unchecked_Conversion
       (MaRTE.HAL.HWTime, Time_Byte_Array);
   --  We will use this function in the driver's "Read" method to retrieve
   --  time as a byte array

   function To_HWTime is new
     Ada.Unchecked_Conversion
       (Time_Byte_Array, MaRTE.HAL.HWTime);

   ---------------------------
   -- Mouse Circular Buffer --
   ---------------------------
   --  subtype Datum is Mouse_Event;
   subtype Datum is Packet_Bytes;
   package Mouse_Queue is new FIFO_Queue (Datum, Mouse_Buffer_Size);

   -----------------------------
   -- Mouse Object Definition --
   -----------------------------
   type PS2_Mouse is record
      Vendor        : Unbounded_String;
      Name          : Unbounded_String;
      Protocol      : Mouse_Type := PS2_Standard;
      Blocking_Mode : Boolean    := True;
      --  When 'Blocking_Mode' is set, tasks are blocked in case there
      --  are not enough mouse packets in buffer to fulfill a read
      --  operation. Otherwise, read operation returns immediately the
      --  available mouse packets (maybe none).
      Command       : Standard_Command;
      --  Command to send
      Param_To_Send : Byte          := 0;
      --  Parameter to send (for Set_Sample_Rate & Set_Resolution commands)
      Receive_Buffer    : Packet_Bytes  := (others => 0);
      --  Buffer to receive data from the mouse
      Command_Ack   : Response;
      --  Response to a command from the Mouse
      Param_Ack     : Response;
      --  Response to a param (for Set_Sample_Rate & Set_Resolution commands)
      Current_Mode  : Mode_Of_Operation := Stream_Mode;
      Data_Reporting_On : Boolean       := False;
      Buttons       : PS2_Mouse_Buttons := 0;
      X_Pos         : Integer           := 0;
      Y_Pos         : Integer           := 0;
      --  Maximum absolute position of the mouse in X and Y coordinates
      Max_X         : Integer := 8192;
      Max_Y         : Integer := 8192;

      Last_Event_Time : MaRTE.HAL.HWTime     := 0;
   end record;
   pragma Pack (PS2_Mouse);

   type PS2_Mouse_Ac is access PS2_Mouse;

   ----------------------------------------------
   -- Internal Function & Procedure Prototypes --
   ----------------------------------------------
   subtype Retries_T is Integer range 0 .. Max_Retries;
   subtype Time_Delay_T is Duration range 0.0 .. Max_Delay;

   function Create_Mouse_Semaphores return Int;

   function Initialize_Mouse return Int;

   procedure Flush_Pending_Input_Bytes (N_Retries : Retries_T := Max_Retries;
                                        N_Bytes  : Natural);

   --  PS2_Mouse_Packet_Process gets data from PS/2 I/O Port, then inserts
   --  them in the queue and updates the PS2_Mouse object. It must be used
   --  with the interrupts disabled.
   procedure PS2_Mouse_Packet_Process;
   --  Process the Standard PS/2 Mouse Movement Data Packet
   --  (Standard PS/2 Protocol)

   procedure Poll_Mouse_IBF
     (N_Retries     : in    Retries_T    := Poll_Retries_Constant;
      Time_Delay    : in    Time_Delay_T := Poll_Delay_Constant;
      Wait          : in    Boolean      := True);

   procedure Poll_Mouse_OBF
     (N_Retries     : in    Retries_T   := Poll_Retries_Constant;
      Time_Delay    : in    Time_Delay_T    := Poll_Delay_Constant;
      Wait          : in    Boolean     := True);

   procedure PS2_Mouse_Send_Byte
     (N_Retries     : in    Retries_T   := Poll_Retries_Constant;
      Time_Delay    : in    Time_Delay_T    := Poll_Delay_Constant;
      Wait          : in    Boolean     := True;
      Byte_To_Send  : in    Byte);

   procedure Mouse_Send_Command
                        (New_Mouse  : in out PS2_Mouse;
                         N_Retries  : in Retries_T    := Poll_Retries_Constant;
                         Time_Delay : in Time_Delay_T := Poll_Delay_Constant;
                         Wait       : in Boolean      := True);

   -------------------------------
   -- Synchronization Semaphore --
   -------------------------------
   Synchro_Sem : aliased MaRTE_Semaphores.Semaphore;

   ------------------------
   --  'Ioctl' Commands  --
   ------------------------
   type Ioctl_Command is
     (PS2_Command,
      --  In this case, the field "Command" of the parameter Ioctl_Data
      --  must contain a valid PS2 Mouse Command (of type Standard_Command)
      --  whereas the field "Param_To_Send" must contain (if necessary) a valid
      --  parameter for the selected command

      Set_Blocking_Mode,
      --  Tasks are blocked in case there are not enough mouse packets in
      --  buffer to fulfill a read operation.

      Reset_Blocking_Mode,
      --  Read operations returns immediately the available mouse packets
      --  (maybe none).

      Enable_Mouse,
      --  This will enable the physical interface between the mouse
      --  and the controller

      Disable_Mouse,
      --  This will "disconnect" the physical interface to the controller

      Get_Button_Status,
      Get_Mouse_Position,
      Set_Mouse_Position,
      Get_Mouse_Protocol,
      Set_Mouse_Protocol,
      Get_Device_Name
     );

   for Ioctl_Command use (PS2_Command         => 160,
                          Set_Blocking_Mode   => 161,
                          Reset_Blocking_Mode => 162,
                          Enable_Mouse        => 163,
                          Disable_Mouse       => 164,
                          Get_Button_Status   => 165,
                          Get_Mouse_Position  => 166,
                          Set_Mouse_Position  => 167,
                          Get_Mouse_Protocol  => 168,
                          Set_Mouse_Protocol  => 169,
                          Get_Device_Name     => 170);

   -----------------
   --  Ioctl Data --
   -----------------
   subtype Ioctl_Data is PS2_Mouse;

private

   -------------------------
   -- Error States Machine --
   -------------------------
   package Mouse_Errors is

      type Code is
        (Mouse_Timeout, Poll_Timeout,
         Mouse_Parity_Error, Mouse_Command_Error, Mouse_Bad_Parameter,
         Queue_Full, Queue_Empty);
      for Code use
        (Mouse_Timeout => 1,
         Poll_Timeout => 2,
         Mouse_Parity_Error => 4,
         Mouse_Command_Error => 8,
         Mouse_Bad_Parameter => 16,
         Queue_Full => 32,
         Queue_Empty => 64);

      procedure Set_Correct;
      procedure Add_Code (The_Code : in Code);
      procedure Delete_Code (The_Code : in Code);
      procedure Set_Code (The_Code : in Code);
      function Included_Code (The_Code : in Code) return Boolean;
      function No_Error return Boolean;

   end Mouse_Errors;

   ----------------------
   -- Global Variables --
   ----------------------
   Mouse : PS2_Mouse_Ac;

end PS2_Mouse;
