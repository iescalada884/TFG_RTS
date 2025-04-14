with Ada.Streams;

--------------------------------------------------------------------------------
-- Represents an ethernet frame, with records to store it
-- and functions/procedures to handle it. This is the specification.
--------------------------------------------------------------------------------
package Ada_Eth.Eth_Frame is

   use type Ada.Streams.Stream_Element_Offset;

   -- Ethernet size constants.
   Eth_Max_Length : constant Ada.Streams.Stream_Element_Offset := 1514;
   Eth_Header_Length : constant Ada.Streams.Stream_Element_Offset := 14;
   Eth_Max_Payload : constant Ada.Streams.Stream_Element_Offset :=
     Eth_Max_Length - Eth_Header_Length;

   -- Represents the protocol that Ethernet encapsulates.
   type Eth_Protocol is new MaRTE.Integer_Types.Unsigned_16;
   type Eth_Protocol_Array is private;

   -- Carries the data that Ethernet encapsulates.
   type Eth_Payload is new Ada.Streams.Stream_Element_Array;

   -- Pointer to an Ethernet payload.
   type Eth_Payload_Ac is access all Eth_Payload;

   -- Records the entire Ethernet frame.
   type Eth_Frame is private;

   -- Pointer to an Ethernet frame.
   type Eth_Frame_Ac is access all Eth_Frame;

   -- Pointer to a stream element. It is used as a pointer to a payload
   -- because an Ethernet payload is composed of stream elements.
   type Eth_SE_Ac is access all Ada.Streams.Stream_Element;

   -- Ethernet encapsulated protocol codes.
   Eth_Proto_Type_IP : constant Eth_Protocol := 16#0800#; -- IP.

   -----------------------------------------------------------------------------
   -- Sets the destination MAC in the frame.
   -- @param Dest_MAC : in Eth_Address -> The destination MAC to insert.
   -- @param Frame : in out Eth_Frame -> The frame in which the destination MAC
   -- is inserted.
   -----------------------------------------------------------------------------
   procedure Set_Dest_MAC (Dest_MAC : in Eth_Address;
                           Frame : in out Eth_Frame);

   -----------------------------------------------------------------------------
   -- Gets the destination MAC from the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the destination MAC
   -- is taken.
   -- @return Eth_Address -> The returned destination MAC.
   -----------------------------------------------------------------------------
   function Get_Dest_MAC (Frame : in Eth_Frame)
                          return Eth_Address;

   -----------------------------------------------------------------------------
   -- Sets the source MAC in the frame.
   -- @param Source_MAC : in Eth_Address -> The source MAC to insert.
   -- @param Frame : in out Eth_Frame -> The frame in which the source MAC
   -- is inserted.
   -----------------------------------------------------------------------------
   procedure Set_Source_MAC (Source_MAC : in Eth_Address;
                             Frame : in out Eth_Frame);

   -----------------------------------------------------------------------------
   -- Gets the source MAC from the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the source MAC
   -- is taken.
   -- @return Eth_Address -> The returned source MAC.
   -----------------------------------------------------------------------------
   function Get_Source_MAC (Frame : in Eth_Frame)
                            return Eth_Address;

   -----------------------------------------------------------------------------
   -- Sets the protocol type which is encapsulated in the frame.
   -- @param Proto_Type : in Eth_Protocol -> The protocol type to insert.
   -- @param Frame : in out Eth_Frame -> The frame in which the protocol type
   -- is inserted.
   -----------------------------------------------------------------------------
   procedure Set_Proto_Type (Proto_Type : in Eth_Protocol;
                             Frame : in out Eth_Frame);

   -----------------------------------------------------------------------------
   -- Gets the protocol type which is encapsulated from the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the protocol type
   -- is taken.
   -- @return Eth_Protocol -> The returned protocol type.
   -----------------------------------------------------------------------------
   function Get_Proto_Type (Frame : in Eth_Frame)
                            return Eth_Protocol;

   -----------------------------------------------------------------------------
   -- Sets the payload which is encapsulated in the frame.
   -- @param Payload : in Eth_Payload -> The payload type to insert.
   -- @param Frame : in out Eth_Frame -> The frame in which the payload
   -- is inserted.
   -----------------------------------------------------------------------------
   procedure Set_Payload (Payload : in Eth_Payload;
                          Frame : in out Eth_Frame);

   -----------------------------------------------------------------------------
   -- Gets the payload which is encapsulated in the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the payload is taken.
   -- @return Eth_Payload -> The returned payload.
   -----------------------------------------------------------------------------
   function Get_Payload (Frame : in Eth_Frame)
                         return Eth_Payload;

   -----------------------------------------------------------------------------
   -- Gets a pointer to the payload which is encapsulated in the frame.
   -- @param Frame : in Eth_Frame -> The frame from which the payload is taken.
   -- @return Eth_Payload -> The returned payload.
   -----------------------------------------------------------------------------
   function Get_Payload_Ac (Frame : in Eth_Frame_Ac)
                            return Eth_Payload_Ac;

private

   -- Internal definition to store the protocol type.
   type Eth_Protocol_Array is array (1 .. 2) of MaRTE.Integer_Types.Unsigned_8;
   for Eth_Protocol_Array'Size use 16;
   pragma pack (Eth_Protocol_Array);

   -- Ethernet frame record definition.
   type Eth_Frame is record
      Dest_MAC   : Eth_Address;
      Source_MAC : Eth_Address;
      Proto_Type : Eth_Protocol_Array;
      Payload    : aliased Eth_Payload (1 .. Eth_Max_Payload); -- All the space don't have to be used.
   end record;
   for Eth_Frame use record
      Dest_MAC at 0 range 0 .. 47;
      Source_MAC at 6 range 0 .. 47;
      Proto_Type at 12 range 0 .. 15;
      Payload at 14 range 0 .. (Eth_Max_Payload * 8 - 1);
   end record;
   pragma pack (Eth_Frame);

end Ada_Eth.Eth_Frame;
