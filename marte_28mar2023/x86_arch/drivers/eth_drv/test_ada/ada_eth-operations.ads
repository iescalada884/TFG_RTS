with POSIX_IO;
with POSIX;
with Ethernet_Driver;
with Ada.Streams;
with Ada_Eth.Eth_Frame;
with Ada.Unchecked_Conversion;

--------------------------------------------------------------------------------
-- Performs operations to manipulate the interface within Ethernet.
-- This is the specification.
--------------------------------------------------------------------------------
package Ada_Eth.Operations is

   -----------------------------------------------------------------------------
   -- Opens the interface given by its name. It only accepts IP packets.
   -- It doesn't blocks the calls we make withit this interface.
   -- @param Device_Name : in String -> The name of the network interface
   -- used to open it. E.g.: '/dev/eth0'.
   -- @return POSIX_IO.File_Descriptor -> The file descriptor used to manipulate
   -- the interface.
   -----------------------------------------------------------------------------
   function Non_Blocking_Open (Device_Name : in String)
                               return POSIX_IO.File_Descriptor;

   -----------------------------------------------------------------------------
   -- Opens the interface given by its name. It only accepts IP packets.
   -- It blocks the calls we make withit this interface.
   -- @param Device_Name : in String -> The name of the network interface
   -- used to open it. E.g.: '/dev/eth0'.
   -- @return POSIX_IO.File_Descriptor -> The file descriptor used to manipulate
   -- the interface.
   -----------------------------------------------------------------------------
   function Blocking_Open (Device_Name : in String)
                           return POSIX_IO.File_Descriptor;

   -----------------------------------------------------------------------------
   -- Closes the file specified by its file descriptor.
   -- @param File_Descriptor : in POSIX_IO.File_Descriptor -> The file
   -- descriptor of the interface to close.
   -----------------------------------------------------------------------------
   procedure Close (File_Descriptor : in POSIX_IO.File_Descriptor);

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
                   Num_Bytes : out Integer);

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
                      Num_Bytes  : out Integer);

   -----------------------------------------------------------------------------
   -- Makes a conversion between a pointer to a stream element array and an
   -- ethernet frame.
   -- @param Source : Ada.Streams.Stream_Element_Array -> A pointer to a stream
   -- element array.
   -- @param Target : Ada_Eth.Eth_Frame.Eth_Frame_Ac -> A pointer to an
   -- ethernet frame.
   -----------------------------------------------------------------------------
   function To_Eth_Frame_Ac is
      new Ada.Unchecked_Conversion (Source => Ada.Streams.Stream_Element_Array,
                                    Target => Ada_Eth.Eth_Frame.Eth_Frame_Ac);

   -----------------------------------------------------------------------------
   -- Writes a stream of data in the specified interface.
   -- @param File : in POSIX_IO.File_Descriptor -> The file descriptor of the
   -- interface where we write.
   -- @param Buffer : in Ada.Streams.Stream_Element_Array -> Data to be writen.
   -- @param Last : out Ada.Streams.Stream_Element_Offset -> The number of
   -- data to be writen.
   -- @param Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals ->
   -- It does not have to be set (it has a default value).
   -----------------------------------------------------------------------------
   procedure Write (File : in POSIX_IO.File_Descriptor;
                    Buffer : in Ada.Streams.Stream_Element_Array;
                    Last : out Ada.Streams.Stream_Element_Offset;
                    Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals)
                    renames POSIX_IO.Write;

   -----------------------------------------------------------------------------
   -- Returns the MAC address owned by our device (NIC).
   -- @return Eth_Address
   -----------------------------------------------------------------------------
   --  TODO: Check if descriptor is open
   function Current_MAC_Address return Eth_Address;

private

   -----------------------------------------------------------------------------
   -- Creates a reception filter.
   -----------------------------------------------------------------------------
   procedure Ioctl_Protocol is new POSIX_IO.Generic_Ioctl
     (Ethernet_Driver.Ioctl_Cmd_Protocol, Ethernet_Driver.Eth_Proto_Arg);


   -----------------------------------------------------------------------------
   -- Creates a scheme to block/unblock calls.
   -----------------------------------------------------------------------------
   procedure Ioctl_Block is new POSIX_IO.Generic_Ioctl
     (Ethernet_Driver.Ioctl_Cmd_No_Args, Ethernet_Driver.Eth_Proto_Arg);

   -----------------------------------------------------------------------------
   -- Inquiries the current MAC.
   -----------------------------------------------------------------------------
   procedure Ioctl_Net is new POSIX_IO.Generic_Ioctl
      (Ethernet_Driver.Ioctl_Cmd_Addr, Ethernet_Driver.Eth_Addr_Ioctl_Arg);

end Ada_Eth.Operations;
