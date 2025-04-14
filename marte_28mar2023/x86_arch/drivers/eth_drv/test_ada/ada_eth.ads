with MaRTE.Integer_Types;

--------------------------------------------------------------------------------
-- Parent package to store common types between Ethernet, IP and UDP protocols.
--------------------------------------------------------------------------------
package Ada_Eth is

   -- Represents a MAC address.
   type Eth_Address is array (1 .. 6) of MaRTE.Integer_Types.Unsigned_8;
   for Eth_Address'Size use 48;
   pragma pack (Eth_Address);

   -- Represents an IP address.
   type IP_Address is array (1 .. 4) of MaRTE.Integer_Types.Unsigned_8;
   for IP_Address'Size use 32;
   pragma pack (IP_Address);

end Ada_Eth;
