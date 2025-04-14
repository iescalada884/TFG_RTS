with System; --  for System.Address
with MaRTE.Integer_Types;  use MaRTE.Integer_Types; -- Unsigned_8/16

package DMA is

   type DMA_Controller is (DMAC1, DMAC2);
   type DMA_Channel is (Ch_0, Ch_1, Ch2, Ch_3);
   type DMA_Mode is new Unsigned_8;
   --  Mode selection
   Demand         : constant DMA_Mode :=  2#0000_0000#;
   Single         : constant DMA_Mode :=  2#0100_0000#;
   Block          : constant DMA_Mode :=  2#1000_0000#;
   Cascade        : constant DMA_Mode :=  2#1100_0000#;
   --  Address increment/decrement
   Addr_Increment : constant DMA_Mode :=  2#0000_0000#;
   Addr_Decrement : constant DMA_Mode :=  2#0010_0000#;
   --  Auto-initialization enable
   Single_Cycle   : constant DMA_Mode :=  2#0000_0000#;
   Auto_Init      : constant DMA_Mode :=  2#0001_0000#;
   --  Transfer bits
   Verifiy        : constant DMA_Mode :=  2#0000_0000#;
   IO_To_Memory   : constant DMA_Mode :=  2#0000_0100#;
   Memory_To_IO   : constant DMA_Mode :=  2#0000_1000#;

   procedure DMA_Disable (DMAC    : in     DMA_Controller;
                          Channel : in     DMA_Channel);

   procedure DMA_Program (DMAC      : in     DMA_Controller;
                          Channel   : in     DMA_Channel;
                          Mode      : in     DMA_Mode;
                          Buffer_Ac : in     System.Address;
                          Length    : in     Unsigned_16);

   procedure DMA_Enable (DMAC    : in     DMA_Controller;
                         Channel : in     DMA_Channel);

private
   for DMA_Channel'Size use 8;
   for DMA_Channel use (Ch_0 => 16#00#, Ch_1 => 16#01#, Ch2 => 16#02#, Ch_3 => 16#03#);
end DMA;