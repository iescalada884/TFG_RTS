--  This package provides the appropriate mapping for the interrupts registers.

pragma Restrictions (No_Elaboration_Code);

with MaRTE.Integer_Types;
with System;
package MaRTE.HAL.Interrupts_Registers is
   pragma Preelaborate;

   subtype Register_32 is MaRTE.Integer_Types.Unsigned_32;

   -- System Timer
   type ST_BASE_Array is array (0 .. 31) of Register_32;
   ST_BASE : ST_BASE_Array;
   for ST_BASE'Address use System'To_Address (16#2000_3000#);

   --Registers offsets (in the unit of 4 bytes)
   Control_Status : constant Integer := 0;
   Counter_Lower  : constant Integer := 1;
   Counter_Higher : constant Integer := 2;
   Compare0       : constant Integer := 3;
   Compare1       : constant Integer := 4;
   Compare2       : constant Integer := 5;
   Compare3       : constant Integer := 6;

   -- Interrupts Register
   type VIC_BASE_Array is array (0 .. 31) of Register_32;
   VIC_BASE : VIC_BASE_Array;
   for VIC_BASE'Address use System'To_Address (16#2000_B200#);

   -- Register offsets (in the unit of 4 bytes)
   Pending_Basic : constant Integer := 0;
   Pending1      : constant Integer := 1;
   Pending2      : constant Integer := 2;
   FIQ_Control   : constant Integer := 3;
   Enable1       : constant Integer := 4;
   Enable2       : constant Integer := 5;
   Enable_Basic  : constant Integer := 6;
   Disable1      : constant Integer := 7;
   Disable2      : constant Integer := 8;
   Disable_Basic : constant Integer := 9;

end MaRTE.HAL.Interrupts_Registers;
