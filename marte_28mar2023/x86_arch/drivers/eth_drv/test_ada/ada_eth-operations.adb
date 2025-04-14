--------------------------------------------------------------------------------
-- Performs operations to manipulate the interface within Ethernet.
-- This is the implementation.
--------------------------------------------------------------------------------
package body Ada_Eth.Operations is

   Current_Station_Address : Eth_Address;

   -----------------------------------------------------------------------------
   -- Opens the interface given by its name. It only accepts IP packets.
   -- It doesn't blocks the calls we make withit this interface.
   -- @param Device_Name : in String -> The name of the network interface
   -- used to open it. E.g.: '/dev/eth0'.
   -- @return POSIX_IO.File_Descriptor -> The file descriptor used to manipulate
   -- the interface.
   -----------------------------------------------------------------------------
   function Non_Blocking_Open (Device_Name : in String)
                               return POSIX_IO.File_Descriptor is
      Network_Fd : POSIX_IO.File_Descriptor;
      Ioctl_Net_Protocol : MaRTE.Integer_Types.Unsigned_16 :=
        MaRTE.Integer_Types.Unsigned_16 (Ada_Eth.Eth_Frame.Eth_Proto_Type_IP);
      Unused_Parameter : MaRTE.Integer_Types.Unsigned_16 := 0; -- This parameter is used, but its value isn't important.
   begin
      -- Opens the interface.
      Network_Fd := POSIX_IO.Open (POSIX.To_POSIX_String (Device_Name), POSIX_IO.Read_Write);

      -- Sets a filter to receive only the specified protocol.
      Ioctl_Protocol (Network_Fd, Ethernet_Driver.Set_Protocol_Filter, Ioctl_Net_Protocol);

      -- Sets the option that makes the calls to not block.
      Ioctl_Block (Network_Fd, Ethernet_Driver.Eth_Non_Blocking_Read, Unused_Parameter);

      return Network_Fd;
   end Non_Blocking_Open;

   -----------------------------------------------------------------------------
   -- Opens the interface given by its name. It only accepts IP packets.
   -- It blocks the calls we make withit this interface.
   -- @param Device_Name : in String -> The name of the network interface
   -- used to open it. E.g.: '/dev/eth0'.
   -- @return POSIX_IO.File_Descriptor -> The file descriptor used to manipulate
   -- the interface.
   -----------------------------------------------------------------------------
   function Blocking_Open (Device_Name : in String)
                           return POSIX_IO.File_Descriptor is
      Network_Fd : POSIX_IO.File_Descriptor;
      Ioctl_Net_Protocol : MaRTE.Integer_Types.Unsigned_16 :=
        MaRTE.Integer_Types.Unsigned_16 (Ada_Eth.Eth_Frame.Eth_Proto_Type_IP);
      Unused_Parameter : MaRTE.Integer_Types.Unsigned_16 := 0;
      Ioctl_Net_Data : Ethernet_Driver.Eth_Addr_Ioctl_Arg;

   	function Ioctl_Addr_To_Eth_Address is new Ada.Unchecked_Conversion
      	(Ethernet_Driver.Eth_Addr_Ioctl_Arg, Eth_Address);

   begin
      -- Opens the interface.
      Network_Fd := POSIX_IO.Open (POSIX.To_POSIX_String (Device_Name), POSIX_IO.Read_Write);

      -- Sets a filter to receive only the specified protocol.
      Ioctl_Protocol (Network_Fd, Ethernet_Driver.Set_Protocol_Filter, Ioctl_Net_Protocol);

      -- Sets the option that makes the calls to block.
      Ioctl_Block (Network_Fd, Ethernet_Driver.Eth_Blocking_Read, Unused_Parameter);

      -- Get the MAC
      Ioctl_Net (Network_Fd, Ethernet_Driver.Eth_Hardware_Address,
                 Ioctl_Net_Data);

      Current_Station_Address :=
         Ioctl_Addr_To_Eth_Address (Ioctl_Net_Data);

      return Network_Fd;
   end Blocking_Open;

   -----------------------------------------------------------------------------
   -- Closes the file specified by its file descriptor.
   -- @param File_Descriptor : in POSIX_IO.File_Descriptor -> The file
   -- descriptor of the interface to close.
   -----------------------------------------------------------------------------
   procedure Close (File_Descriptor : in POSIX_IO.File_Descriptor) is
   begin
      POSIX_IO.Close (File_Descriptor);
   end Close;

   -----------------------------------------------------------------------------
   -- Reads an ethernet frame from the specified interface.
   -- @param Network_Fd : in POSIX_IO.File_Descriptor -> The file descriptor
   -- of the interface from which we read.
   -- @param Eth_Frm : out Ada_Eth.Eth_Frame.Eth_Frame -> The returned
   -- ethernet frame.
   -- @param Num_Bytes : out Integer -> Number of bytes read.
   -----------------------------------------------------------------------------
   procedure Read (Network_Fd : in POSIX_IO.File_Descriptor;
                   Eth_Frm : out Ada_Eth.Eth_Frame.Eth_Frame;
                   Num_Bytes : out Integer) is
      Unformat_Received_Frame : Ada.Streams.Stream_Element_Array
        (1 .. Ada_Eth.Eth_Frame.Eth_Max_Length);
      for Unformat_Received_Frame'Address use Eth_Frm'Address;
   begin
      POSIX_IO.Read (Network_Fd, Unformat_Received_Frame, Ada.Streams.Stream_Element_Offset (Num_Bytes) );
   end Read;

   -----------------------------------------------------------------------------
   -- Reads an ethernet frame from the specified interface. Returns a pointer
   -- to that frame.
   -- @param Network_Fd : in POSIX_IO.File_Descriptor -> The file descriptor
   -- of the interface from which we read.
   -- @param Eth_Frm_Ac : in Ada_Eth.Eth_Frame.Eth_Frame_Ac -> The returned
   -- pointer to the ethernet frame.
   -- @param Num_Bytes : out Integer -> Number of bytes read.
   -----------------------------------------------------------------------------
   procedure Read_Ac (Network_Fd : in POSIX_IO.File_Descriptor;
                      Eth_Frm_Ac : in Ada_Eth.Eth_Frame.Eth_Frame_Ac;
                      Num_Bytes  : out Integer) is
      Unformat_Received_Frame : Ada.Streams.Stream_Element_Array
        (1 .. Ada_Eth.Eth_Frame.Eth_Max_Length);
      for Unformat_Received_Frame'Address use Eth_Frm_Ac.all'Address;
   begin
      POSIX_IO.Read (Network_Fd, Unformat_Received_Frame, Ada.Streams.Stream_Element_Offset (Num_Bytes) );
   end Read_Ac;

   -----------------------------------------------------------------------------
   -- Returns the MAC address owned by our device (NIC).
   -- @return Eth_Address
   -----------------------------------------------------------------------------
   --  TODO: Check if descriptor is open
   function Current_MAC_Address return Eth_Address is
   begin
       return Current_Station_Address;
   end Current_MAC_Address;



end Ada_Eth.Operations;
