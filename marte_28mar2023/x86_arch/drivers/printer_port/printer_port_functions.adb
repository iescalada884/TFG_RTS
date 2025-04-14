------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                'P r i n t e r _ P o r t _ F u n c t i o n s'
--
--                                   Body
--
--
--  File 'printer_port_functions.adb'                                  By MAR.
--
--
--  Driver for the PC printer port.
--
--  This file contains all the data types necessary for applications
--  in order to use the driver.
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
with Printer_Port;

with Drivers_MaRTE;  use Drivers_MaRTE;
with MaRTE.HAL.IO; use MaRTE.HAL.IO;
with MaRTE.Integer_Types;

package body Printer_Port_Functions is

   --  Parallel Port Registers
   PP_BASE_REG    : constant IO_Port := 16#378#; -- Base lpt1
   PP_DATA_REG    : constant IO_Port := 0; -- Data port offset
   PP_STATUS_REG  : constant IO_Port := 1; -- Status port offset
   PP_CONTROL_REG : constant IO_Port := 2; -- Control port offset

   --  Control bits
   PP_IENABLE : constant MaRTE.Integer_Types.Unsigned_8 := 16#10#;

   --  Last byte written in the control register
   Last_Control_Byte : MaRTE.Integer_Types.Unsigned_8 := 2#0000_1011#;

   --------------
   --  Create  --
   --------------
   function Create return Int is
   begin
      Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);
      return 0;
   end Create;

   ------------
   --  Read  --
   ------------
   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Ssize_T is
      use type Drivers_MaRTE.Buffer_Length;
   begin
      if Bytes = 0 then
         return 0;
      end if;
      Buffer_Ptr.all (1) := Inb_P (PP_BASE_REG + PP_DATA_REG);
      return 1;
   end Read;

   -------------
   --  Write  --
   -------------
   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Ssize_T is
      use type Drivers_MaRTE.Buffer_Length;
   begin
      if Bytes = 0 then
         return 0;
      end if;
      Outb_P (PP_BASE_REG + PP_DATA_REG, Buffer_Ptr.all (1));
      return 1;
   end Write;

   ---------------------
   --  Control lines  --
   ---------------------
   --  Control lines
   type Control_Lines is (Strobe, Auto_Feed, Init_Printer, Select_Input);

   --  Status lines
   type Status_Lines is (Busy, Acknowledge, Paper_End, Device_Select, Error);

   Set_Control_Lines_Masks :
     constant array (Printer_Port.Ioctl_Commads range
                     Printer_Port.Set_Strobe .. Printer_Port.Set_Select_Input)
     of MaRTE.Integer_Types.Unsigned_8 :=
     (Printer_Port.Set_Strobe       => 2#0000_0001#,
      Printer_Port.Set_Auto_Feed    => 2#0000_0010#,
      Printer_Port.Set_Init_Printer => 2#0000_0100#,
      Printer_Port.Set_Select_Input => 2#0000_1000#);

   Reset_Control_Lines_Masks :
     constant array (Printer_Port.Ioctl_Commads range
                     Printer_Port.Reset_Strobe ..
                     Printer_Port.Reset_Select_Input)
     of MaRTE.Integer_Types.Unsigned_8 :=
     (Printer_Port.Reset_Strobe       => 2#1111_1110#,
      Printer_Port.Reset_Auto_Feed    => 2#1111_1101#,
      Printer_Port.Reset_Init_Printer => 2#1111_1011#,
      Printer_Port.Reset_Select_Input => 2#1111_0111#);

   Status_Lines_Masks :
     constant array (Printer_Port.Ioctl_Commads range
                     Printer_Port.Read_Busy .. Printer_Port.Read_Error)
     of MaRTE.Integer_Types.Unsigned_8 :=
     (Printer_Port.Read_Busy          => 2#1000_0000#,
      Printer_Port.Read_Acknowledge   => 2#0100_0000#,
      Printer_Port.Read_Paper_End     => 2#0010_0000#,
      Printer_Port.Read_Device_Select => 2#0001_0000#,
      Printer_Port.Read_Error         => 2#0000_1000#);

   -------------
   --  Ioctl  --
   -------------
   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac) return Int is
      use Printer_Port;
      use type MaRTE.Integer_Types.Unsigned_8;
   begin
      case Request is
         when Printer_Port.Ioctl_Commads'Enum_Rep (Enable_Interrupts) =>
            Last_Control_Byte := Last_Control_Byte or PP_IENABLE;
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);

         when Printer_Port.Ioctl_Commads'Enum_Rep (Disable_interrupts) =>
            Last_Control_Byte := Last_Control_Byte and not PP_IENABLE;
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);

         when Printer_Port.Ioctl_Commads'Enum_Rep (Set_Strobe) =>
            Last_Control_Byte :=
              Last_Control_Byte or Set_Control_Lines_Masks (Set_Strobe);
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);

         when Printer_Port.Ioctl_Commads'Enum_Rep (Set_Auto_Feed) =>
            Last_Control_Byte :=
              Last_Control_Byte or Set_Control_Lines_Masks (Set_Auto_Feed);
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);

         when Printer_Port.Ioctl_Commads'Enum_Rep (Set_Init_Printer) =>
            Last_Control_Byte :=
              Last_Control_Byte or Set_Control_Lines_Masks (Set_Init_Printer);
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);

         when Printer_Port.Ioctl_Commads'Enum_Rep (Set_Select_Input) =>
            Last_Control_Byte :=
              Last_Control_Byte or Set_Control_Lines_Masks (Set_Select_Input);
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);


         when Printer_Port.Ioctl_Commads'Enum_Rep (Reset_Strobe) =>
            Last_Control_Byte := Last_Control_Byte
              and Reset_Control_Lines_Masks (Reset_Strobe);
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);

         when Printer_Port.Ioctl_Commads'Enum_Rep (Reset_Auto_Feed) =>
            Last_Control_Byte := Last_Control_Byte
              and Reset_Control_Lines_Masks (Reset_Auto_Feed);
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);

         when Printer_Port.Ioctl_Commads'Enum_Rep (Reset_Init_Printer) =>
            Last_Control_Byte := Last_Control_Byte
              and Reset_Control_Lines_Masks (Reset_Init_Printer);
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);

         when Printer_Port.Ioctl_Commads'Enum_Rep (Reset_Select_Input) =>
            Last_Control_Byte := Last_Control_Byte
              and Reset_Control_Lines_Masks (Reset_Select_Input);
            Outb_P (PP_BASE_REG + PP_CONTROL_REG, Last_Control_Byte);


         when Printer_Port.Ioctl_Commads'Enum_Rep (Read_Busy) =>
            if (Inb_P (PP_BASE_REG + PP_STATUS_REG) and
                Status_Lines_Masks (Read_Busy)) /= 0        then
               Ioctl_Data_Ptr.all (1) := 1;
            else
               Ioctl_Data_Ptr.all (1) := 0;
            end if;

         when Printer_Port.Ioctl_Commads'Enum_Rep (Read_Acknowledge) =>
            if (Inb_P (PP_BASE_REG + PP_STATUS_REG) and
                Status_Lines_Masks (Read_Acknowledge)) /= 0        then
               Ioctl_Data_Ptr.all (1) := 1;
            else
               Ioctl_Data_Ptr.all (1) := 0;
            end if;

         when Printer_Port.Ioctl_Commads'Enum_Rep (Read_Paper_End) =>
            if (Inb_P (PP_BASE_REG + PP_STATUS_REG) and
                Status_Lines_Masks (Read_Paper_End)) /= 0        then
               Ioctl_Data_Ptr.all (1) := 1;
            else
               Ioctl_Data_Ptr.all (1) := 0;
            end if;

         when Printer_Port.Ioctl_Commads'Enum_Rep (Read_Device_Select) =>
            if (Inb_P (PP_BASE_REG + PP_STATUS_REG) and
                Status_Lines_Masks (Read_Device_Select)) /= 0        then
               Ioctl_Data_Ptr.all (1) := 1;
            else
               Ioctl_Data_Ptr.all (1) := 0;
            end if;

         when Printer_Port.Ioctl_Commads'Enum_Rep (Read_Error) =>
            if (Inb_P (PP_BASE_REG + PP_STATUS_REG) and
                Status_Lines_Masks (Read_Error)) /= 0        then
               Ioctl_Data_Ptr.all (1) := 1;
            else
               Ioctl_Data_Ptr.all (1) := 0;
            end if;


         when others =>
            return Int (-1);
      end case;

      return 0;
   end Ioctl;

end Printer_Port_Functions;
