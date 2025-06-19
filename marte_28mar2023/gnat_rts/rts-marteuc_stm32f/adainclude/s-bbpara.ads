with System.STM32;
with System.BB.Board_Parameters;
with System.BB.MCU_Parameters;

package System.BB.Parameters is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate (System.BB.Parameters);

   Clock_Frequency : constant := Board_Parameters.Main_Clock_Frequency;
   pragma Compile_Time_Error
      (Clock_Frequency not in System.STM32.SYSCLK_Range,
         "bad Clock_Frequency value");
   Ticks_Per_Second : constant := Clock_Frequency;

   --  Set the requested SYSCLK frequency. Setup_Pll will try to set configure
   --  PLL to match this value when possible or reset the board.

   Has_FPU : constant Boolean := True;
   Has_VTOR : constant Boolean := True;
   Has_OS_Extensions : constant Boolean := True;
   Is_ARMv6m : constant Boolean := False;

   ----------------
   -- Interrupts --
   ----------------

   subtype Cortex_Priority_Bits_Width is Integer range 1 .. 8;
   --  The number of bits used by the hardware for priority levels, i.e.,
   --  within the BASEPRI register and IP priority registers. Priorities are
   --  at most eight bits wide but usually fewer. The value varies both with
   --  vendor and chip.

   NVIC_Priority_Bits : constant Cortex_Priority_Bits_Width := 4;
   --  The number of bits allocated by this specific hardware implementation.

   subtype Interrupt_Range is Integer
     range -1 .. MCU_Parameters.Number_Of_Interrupts;

   Context_Buffer_Capacity : constant := 10;

   Interrupt_Stack_Size : constant := 2 * 1024;

   Trap_Vectors : constant := 17;
   --  While on this target there is little difference between interrupts
   --  and traps, we consider the following traps:
   --
   --    Name                        Nr
   --
   --    Reset_Vector                 1
   --    NMI_Vector                   2
   --    Hard_Fault_Vector            3
   --    Mem_Manage_Vector            4
   --    Bus_Fault_Vector             5
   --    Usage_Fault_Vector           6
   --    SVC_Vector                  11
   --    Debug_Mon_Vector            12
   --    Pend_SV_Vector              14
   --    Sys_Tick_Vector             15
   --    Interrupt_Request_Vector    16
   --
   --  These trap vectors correspond to different low-level trap handlers in
   --  the run time. Note that as all interrupt requests (IRQs) will use the
   --  same interrupt wrapper, there is no benefit to using separate vectors
   --  for each.
   ----------
   -- CPUs --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board? Nope, this is a single-core
end System.BB.Parameters;
