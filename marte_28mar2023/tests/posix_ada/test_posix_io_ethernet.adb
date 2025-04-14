--  Test for: x86
with Text_IO; use Text_IO;
with Ethernet_Driver;
with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;
with POSIX;
with POSIX_IO;

with Reports;

procedure Test_Posix_IO_Ethernet is
   pragma Priority (8);

   type Ethernet_Address is new Stream_Element_Array (1 .. 6);
   for Ethernet_Address'Size use 48;
   type Ethernet_Address_Octect is range 0 .. 16#FF#;
   package Str_Int_IO is new Text_IO.Integer_IO (Ethernet_Address_Octect);
   subtype Station_String is String (1 .. 17);

   procedure Ioctl_Net is
      new POSIX_IO.Generic_Ioctl (Ethernet_Driver.Ioctl_Cmd_Addr,
                                  Ethernet_Driver.Eth_Addr_Ioctl_Arg);

   procedure Ioctl_Protocol is
      new POSIX_IO.Generic_Ioctl (Ethernet_Driver.Ioctl_Cmd_Protocol,
                                  Ethernet_Driver.Eth_Proto_Arg);

   function Ioctl_Addr_To_Ethernet_Address is
      new Ada.Unchecked_Conversion (Ethernet_Driver.Eth_Addr_Ioctl_Arg,
                                    Ethernet_Address);

   function Ethernet_Address_To_String_Address
     (Eth_Addr : in Ethernet_Address)
     return String is
      Index : Integer := 1;
      Str2 : String (1 .. 6);
      Sta_String : Station_String;
   begin
      for I in Ethernet_Address'Range loop
         Index := 1 + 3*(Integer (I) - 1);
         if Eth_Addr (I) > 16#F# then
            Str_Int_IO.Put (Str2, Ethernet_Address_Octect (Eth_Addr (I)), 16);
            Sta_String (Index .. Index + 1) := Str2 (4 .. 5);
         else
            Str_Int_IO.Put (Str2, Ethernet_Address_Octect (Eth_Addr (I)), 16);
            Sta_String (Index .. Index) := "0";
            Sta_String (Index + 1 .. Index + 1) := Str2 (5 .. 5);
         end if;
         if I /= 6 then
            Sta_String (Index + 2 .. Index + 2) := ":";
         end if;
      end loop;
      return String (Sta_String);
   end Ethernet_Address_To_String_Address;

   Network_Fd : POSIX_IO.File_Descriptor;

   Device_Name : constant String := "/dev/eth0";
   RTEP_Protocol_Number : constant := 16#A000#;

   Ioctl_Net_Data : Ethernet_Driver.Eth_Addr_Ioctl_Arg;
   Ioctl_Net_Protocol : Ethernet_Driver.Eth_Proto_Arg :=
      Ethernet_Driver.Eth_Proto_Arg (RTEP_Protocol_Number);
   Current_Station_Address : Ethernet_Address;
   Max_Ethernet_Frame : constant := 1514; -- without the FCS
   Eth_Frame : Stream_Element_Array (1 .. Max_Ethernet_Frame);
   Last_Rx : Ada.Streams.Stream_Element_Offset;
   Last_Tx : Ada.Streams.Stream_Element_Offset;
begin

   Reports.Init;

   Network_Fd := POSIX_IO.Open
      (POSIX.To_POSIX_String (Device_Name),
       POSIX_IO.Read_Write);
   --  Once oppened we get the MAC address of the interface.
   Ioctl_Net (Network_Fd, Ethernet_Driver.Eth_Hardware_Address,
              Ioctl_Net_Data);
   Current_Station_Address := Ioctl_Addr_To_Ethernet_Address (Ioctl_Net_Data);
   Put_Line ("MAC Address: "&Ethernet_Address_To_String_Address (Current_Station_Address));
   --  Set the reception Filter
   Ioctl_Protocol (Network_Fd, Ethernet_Driver.Set_Protocol_Filter,
                   Ioctl_Net_Protocol);
   POSIX_IO.Read (Network_Fd, Eth_Frame, Last_Rx);
   POSIX_IO.Write (Network_Fd, Eth_Frame (1 .. Last_Rx), Last_Tx);
   POSIX_IO.Close (Network_Fd);

   Reports.Test_OK;

end Test_Posix_IO_Ethernet;
