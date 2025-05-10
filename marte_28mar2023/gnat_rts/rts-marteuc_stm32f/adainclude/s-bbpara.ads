with System.STM32;
with System.BB.MCU_Parameters;

package System.BB.Parameters is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate (System.BB.Parameters);

   Has_FPU : constant Boolean := True;
   Has_VTOR : constant Boolean := True;
   Has_OS_Extensions : constant Boolean := True;
   Is_ARMv6m : constant Boolean := False;

   subtype Interrupt_Range is Integer
     range -1 .. MCU_Parameters.Number_Of_Interrupts;

   Context_Buffer_Capacity : constant := 10;

   Interrupt_Stack_Size : constant := 2 * 1024;
end System.BB.Parameters;
