with MaRTE.Integer_Types;
with Ada_Eth;
with Ada_Eth.Eth_Frame;
with Ada_Eth.Operations;

with POSIX_IO;

with Ada.Streams;
with Ada.Text_IO;

procedure Test_Ethernet is

   use Ada_Eth;
   use Ada;
   use Ada.Streams;
   use Ada.Text_IO;

   package Net renames Ada_Eth.Operations;

   --  Configuration parameters for each particular example
   Device_Name : constant String := "/dev/eth0";
   Server_MAC  : constant Eth_Address := (16#00#,16#30#,16#64#,16#07#,16#A2#,16#62#);
   Client_MAC  : constant Eth_Address := (16#00#,16#30#,16#64#,16#07#,16#A2#,16#60#);

   -----------------------------

   -- To_Stream_Element_Array --

   -----------------------------

   function To_Stream_Element_Array
     (Data : in String)
     return Streams.Stream_Element_Array
   is
      use Streams;

      Result : Stream_Element_Array
        (Stream_Element_Offset (Data'First)
         .. Stream_Element_Offset (Data'Last));
   begin
      for K in Data'Range loop
         Result (Stream_Element_Offset (K)) := Character'Pos (Data (K));
      end loop;
      return Result;
   end To_Stream_Element_Array;

   ---------------

   -- To_String --

   ---------------

   function To_String
     (Data : in Streams.Stream_Element_Array)
     return String
   is
      Result : String (Integer (Data'First) .. Integer (Data'Last));
   begin
      for K in Data'Range loop
         Result (Integer (K)) := Character'Val (Data (K));
      end loop;
      return Result;
   end To_String;

   Me          : Eth_Address;
   The_Protocol: constant Eth_Frame.Eth_Protocol := 16#0800#;

   File        : POSIX_IO.File_Descriptor;
   The_Frame   : Eth_Frame.Eth_Frame;

   Data        : constant String := "Hello Ethernet";
   The_Buffer  : Streams.Stream_Element_Array := To_Stream_Element_Array (Data);

   Frame_Size  : Streams.Stream_Element_Offset;
   Last_Tx     : Streams.Stream_Element_Offset;
   Bytes_Rcv   : Streams.Stream_Element_Offset;

   Frame_Send  : Streams.Stream_Element_Array (1 .. Eth_Frame.Eth_Max_Length);
   for Frame_Send'Address use The_Frame'Address;

begin

   Ada.Text_IO.Put_Line ("Starting...");

   File := Net.Blocking_Open (Device_Name => Device_Name);

   Me := Net.Current_MAC_Address;

   if (Me = Server_MAC) then
      Ada.Text_IO.Put_Line ("Server:");
      Net.Read (Network_Fd => File,
                Eth_Frm    => The_Frame,
                Num_Bytes  => Integer (Bytes_Rcv));

      Frame_Send (1 .. Eth_Frame.Eth_Max_Payload) := Stream_Element_Array
        (Eth_Frame.Get_Payload(Frame => The_Frame));

      Ada.Text_IO.Put_Line
        (To_String (Frame_Send (1 .. Bytes_Rcv - Eth_Frame.Eth_Header_Length)));

   elsif (Me = Client_MAC) then
      Ada.Text_IO.Put_Line ("Client:");
      delay 2.0;

      Eth_Frame.Set_Dest_MAC (Dest_MAC => Server_MAC,
                             Frame    => The_Frame);

      Eth_Frame.Set_Source_MAC (Source_MAC => Client_MAC,
                                Frame      => The_Frame);

      Eth_Frame.Set_Proto_Type (Proto_Type => The_Protocol,
                                Frame      => The_Frame);

      Eth_Frame.Set_Payload (Payload => Eth_Frame.Eth_Payload (The_Buffer),
                             Frame   => The_Frame);

      Ada.Text_IO.Put_Line (Integer'Image (The_Buffer'Length));

      Frame_Size := Eth_Frame.Eth_Header_Length + The_Buffer'Length;

      Ada.Text_IO.Put_Line (Frame_Size'Img);

      Net.Write (File           => File,
                 Buffer         => Frame_Send (1 .. Frame_Size),
                 Last           => Last_Tx);
   else
       raise Program_Error;
   end if;

end Test_Ethernet;
