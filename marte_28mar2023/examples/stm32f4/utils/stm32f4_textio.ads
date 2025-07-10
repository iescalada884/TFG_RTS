--  Stm32f4_TextIO.ads
--  Text_IO-like Ada wrapper for STM32F4 UART console, modeled after MaRTE.Direct_IO

with System;

package Stm32f4_TextIO is
   pragma Preelaborate;

   procedure Put (Str : in String);
   procedure Put (N : in Integer; Base : in Positive := 10);
   procedure Put_Line (Str : in String);
   procedure New_Line;
   procedure Initialize;
   procedure Error (Msg : in String; Fatal : in Boolean := False);
   procedure Put_Error (Msg : in String; Fatal : in Boolean := False);

   type Output_Mode is (SEMIHOST, UART);

   --  control output mode
   procedure Set_Mode (New_Mode : in Output_Mode);
   --  0 for semihosting (marteIO), 1 for UART console
   function Get_Active_Mode return Output_Mode;
   
private
   -- Internal state
   Is_Initialized : Boolean := False;  -- True if the text IO is initialized
   Mode : Output_Mode := SEMIHOST;  -- 0 para semihosting, 1 para UART
   -- Internal error functions
   procedure Output (Str : in String);
   procedure Output_Error (Str : in String);
   
end Stm32f4_TextIO;
