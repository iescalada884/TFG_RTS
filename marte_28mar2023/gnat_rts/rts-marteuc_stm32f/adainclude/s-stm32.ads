package System.STM32 is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate (System.STM32);

   --  Constants for RCC CR register

   subtype PLLM_Range is Integer range 2 .. 63;
   subtype PLLN_Range is Integer range 50 .. 432;
   subtype PLLP_Range is Integer range 2 .. 8
     with Static_Predicate => (case PLLP_Range is
                                 when 2 | 4 | 6 | 8 => True,
                                 when others => False);
   subtype PLLQ_Range is Integer range 2 .. 15;

   subtype HSECLK_Range is Integer range   1_000_000 ..  26_000_000;
   subtype PLLIN_Range  is Integer range     950_000 ..   2_000_000;
   subtype PLLVC0_Range is Integer range 192_000_000 .. 432_000_000;
   subtype PLLOUT_Range is Integer range  24_000_000 .. 216_000_000;
   subtype SYSCLK_Range is Integer range           1 .. 216_000_000;
   subtype HCLK_Range   is Integer range           1 .. 216_000_000;
   subtype PCLK1_Range  is Integer range           1 ..  54_000_000;
   subtype PCLK2_Range  is Integer range           1 .. 108_000_000;
   subtype SPII2S_Range is Integer range           1 ..  37_500_000;
   pragma Unreferenced (SPII2S_Range);

   --  These internal low and high speed clocks are fixed (do not modify)

   HSICLK : constant := 16_000_000;
   LSICLK : constant :=     32_000;
end System.STM32;