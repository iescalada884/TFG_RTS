
--------------------------------------------------------------------------------
-- Represents an ethernet frame, with records to store it
-- and functions/procedures to handle it. This is the implementation.
--------------------------------------------------------------------------------
package body Ada_Eth.Eth_Frame is

   -----------------------------------------------------------------------------
   -- Sets the destination MAC in the frame.
   -- @param Dest_MAC : in Eth_Address -> The destination MAC to insert.
   -- @param Frame : in out Eth_Frame -> The frame in which the destination MAC
   -- is inserted.
   -----------------------------------------------------------------------------
   procedure Set_Dest_MAC (Dest_MAC : in Eth_Address;
                           Frame : in out Eth_Frame) is
   begin
      Frame.Dest_MAC := Dest_MAC;
   end Set_Dest_MAC;

   -----------------------------------------------------------------------------
   -- Gets the destination MAC from the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the destination MAC
   -- is taken.
   -- @return Eth_Address -> The returned destination MAC.
   -----------------------------------------------------------------------------
   function Get_Dest_MAC (Frame : in Eth_Frame)
                          return Eth_Address is
   begin
      return Frame.Dest_MAC;
   end Get_Dest_MAC;

   -----------------------------------------------------------------------------
   -- Sets the source MAC in the frame.
   -- @param Source_MAC : in Eth_Address -> The source MAC to insert.
   -- @param Frame : in out Eth_Frame -> The frame in which the source MAC
   -- is inserted.
   -----------------------------------------------------------------------------
   procedure Set_Source_MAC (Source_MAC : in Eth_Address;
                             Frame : in out Eth_Frame) is
   begin
      Frame.Source_MAC := Source_MAC;
   end Set_Source_MAC;

   -----------------------------------------------------------------------------
   -- Gets the source MAC from the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the source MAC
   -- is taken.
   -- @return Eth_Address -> The returned source MAC.
   -----------------------------------------------------------------------------
   function Get_Source_MAC (Frame : in Eth_Frame)
                            return Eth_Address is
   begin
      return Frame.Source_MAC;
   end Get_Source_MAC;

   -----------------------------------------------------------------------------
   -- Sets the protocol type which is encapsulated in the frame.
   -- @param Proto_Type : in Eth_Protocol -> The protocol type to insert.
   -- @param Frame : in out Eth_Frame -> The frame in which the protocol type
   -- is inserted.
   -----------------------------------------------------------------------------
   procedure Set_Proto_Type (Proto_Type : in Eth_Protocol;
                             Frame : in out Eth_Frame) is
      Array_Origin : Eth_Protocol_Array;
      for Array_Origin'Address use Proto_Type'Address;
      Array_Reorder : Eth_Protocol_Array;
   begin
      Array_Reorder (1) := Array_Origin (2);
      Array_Reorder (2) := Array_Origin (1);
      Frame.Proto_Type := Array_Reorder;
   end Set_Proto_Type;

   -----------------------------------------------------------------------------
   -- Gets the protocol type which is encapsulated from the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the protocol type
   -- is taken.
   -- @return Eth_Protocol -> The returned protocol type.
   -----------------------------------------------------------------------------
   function Get_Proto_Type (Frame : in Eth_Frame)
                            return Eth_Protocol is
      Proto_Type : Eth_Protocol;
      Array_Reorder : Eth_Protocol_Array;
      for Proto_Type'Address use Array_Reorder'Address;
   begin
      Array_Reorder (1) := Frame.Proto_Type (2);
      Array_Reorder (2) := Frame.Proto_Type (1);
      return Proto_Type;
   end Get_Proto_Type;

   -----------------------------------------------------------------------------
   -- Sets the payload which is encapsulated in the frame.
   -- @param Payload : in Eth_Payload -> The payload type to insert.
   -- @param Frame : in out Eth_Frame -> The frame in which the payload
   -- is inserted.
   -----------------------------------------------------------------------------
   procedure Set_Payload (Payload : in Eth_Payload;
                          Frame : in out Eth_Frame) is
   begin
      for I in 1 .. Payload'Length loop
         Frame.Payload (Ada.Streams.Stream_Element_Offset (I) )
           := Payload (Ada.Streams.Stream_Element_Offset (I) );
      end loop;
   end Set_Payload;

   -----------------------------------------------------------------------------
   -- Gets the payload which is encapsulated in the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the payload is taken.
   -- @return Eth_Payload -> The returned payload.
   -----------------------------------------------------------------------------
   function Get_Payload (Frame : in Eth_Frame)
                         return Eth_Payload is
   begin
      return Frame.Payload;
   end Get_Payload;

   -----------------------------------------------------------------------------
   -- Gets a pointer to the payload which is encapsulated in the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the payload is taken.
   -- @return Eth_Payload -> The returned payload.
   -----------------------------------------------------------------------------
   function Get_Payload_Ac (Frame : in Eth_Frame_Ac)
                            return Eth_Payload_Ac is
   begin
      return Frame.Payload'Unrestricted_Access;
   end Get_Payload_Ac;

end Ada_Eth.Eth_Frame;
