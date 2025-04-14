------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                   'H a r d w a r e _ I n t e r f a c e'
--
--                                  Body
--
--
--  File 'hardware_interface.adb'                                   By MAR.
--
--  STM32F Version.
--
--  This package is the border between the hardware dependent and hardware
--  independent parts of the kernel.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------
with System;

pragma Warnings (Off);
with System.BB.Parameters;
with System.BB.Board_Parameters;
pragma Warnings (On);

with Ada.Unchecked_Conversion;

with System.Storage_Elements;
with System.Machine_Code; use System.Machine_Code;
with Interfaces;
with GNAT.IO;

with MaRTE.HAL.Registers;

package body MaRTE.HAL is
   package BBOPA renames System.BB.Board_Parameters;
   package SSE renames System.Storage_Elements;

   Initialized : Boolean := False;

   use type HWTime, Interfaces.Unsigned_32;


   NL : constant String := ASCII.LF & ASCII.HT;
   --  Asm instruction separator

   --------------------------------------------------------------------------
   -- Interrupts ------------------------------------------------------------
   --------------------------------------------------------------------------

   Interrupt_Nesting_Count : Unsigned_8 := 16#80#;
   --  16#80# => Outside_Of_Interrupts
   pragma Export (C, Interrupt_Nesting_Count, "base_irq_nest");

   ---------------------
   -- Interrupt Stack --
   ---------------------

   --  Based on s-bbinte.adb

   type Interrupt_Stack_Space is new SSE.Storage_Array
     (1 .. SSE.Storage_Offset (BBParam.Interrupt_Stack_Size));
   pragma Suppress_Initialization (Interrupt_Stack_Space);
   for Interrupt_Stack_Space'Alignment use 8;
   --  Type used to represent the stack area for each interrupt. The stack must
   --  be aligned to the CPU specific alignment to hold the largest registers.

   Interrupt_Stack : Interrupt_Stack_Space;
   pragma Linker_Section (Interrupt_Stack, ".interrupt_stacks");

   ---------------------
   -- Timer_Interrupt --
   ---------------------

   function Timer_Interrupt return HW_Interrupt is
   begin
      return Sys_Tick_Vector;
   end Timer_Interrupt;

   ----------------------------------
   -- Default_HW_Interrupt_Handler --
   ----------------------------------

   procedure Default_HW_Interrupt_Handler (State : in Trap_State_Ac) is
   begin
      GNAT.IO.Put ("Unexpected interrupt");
      GNAT.IO.Put (Integer (State.IRQ_Num));
      GNAT.IO.New_Line;
   end Default_HW_Interrupt_Handler;

   -------------------
   -- Timer_Handler --
   -------------------

   Timer_Handler_Ac  : HW_Interrupt_Handler;

   ---------------------------
   -- Timer_Handler_Wrapper --
   ---------------------------

   procedure Timer_Handler_Wrapper;
   pragma Export (C, Timer_Handler_Wrapper, "__gnat_sys_tick_trap");
   --  Body defined bellow, along with the rest of the time related stuff

   ----------------------------------
   -- Install_HW_Interrupt_Handler --
   ----------------------------------

   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler) is
   begin
      pragma Compile_Time_Warning (True, "Only install timer handler");

      if Int_Num = Sys_Tick_Vector then
         Timer_Handler_Ac := Handler;

      else
         pragma Assert (False);
         null;
      end if;
   end Install_HW_Interrupt_Handler;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsid i", Volatile => True);
   end Disable_Interrupts;


   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsie i"   & ASCII.LF & ASCII.HT
                               & "dsb"     & ASCII.LF & ASCII.HT
                               & "isb",
                               Clobber => "memory", Volatile => True);
      --  dsb and isb are Data/Instruction Synchronization Barriers
   end Enable_Interrupts;


   ----------------------------------------------------
   -- Hardware_Interrupt_Controller_Enable_Interrupt --
   ----------------------------------------------------

   procedure Hardware_Interrupt_Controller_Enable_Interrupt
     (Int : in HW_Interrupt) is
   begin
      null;
   end Hardware_Interrupt_Controller_Enable_Interrupt;

   -----------------------------------------------------
   -- Hardware_Interrupt_Controller_Disable_Interrupt --
   -----------------------------------------------------

   procedure Hardware_Interrupt_Controller_Disable_Interrupt
     (Int : in HW_Interrupt) is
   begin
      null;
   end Hardware_Interrupt_Controller_Disable_Interrupt;

   ------------------------------------------
   -- Enable_Hardware_Interrupt_Controller --
   ------------------------------------------

   procedure Enable_Hardware_Interrupt_Controller is
   begin
      null;
   end Enable_Hardware_Interrupt_Controller;

   ----------------------------
   -- Are_Interrupts_Enabled --
   ----------------------------

   function Are_Interrupts_Enabled return Boolean is
      Vect_Active : Interfaces.Unsigned_32;
   begin
      Vect_Active := Registers.ICSR and Registers.ICSR_VECTACTIVE_Mask;
      return Registers.PRIMASK = 0 and
        Vect_Active /= Interfaces.Unsigned_32 (Sys_Tick_Vector) and
        Vect_Active /= Interfaces.Unsigned_32 (PendSV_Vector);
   end Are_Interrupts_Enabled;

   ------------------------------
   -- Get_Current_HW_Interrupt --
   ------------------------------

   function Get_Current_HW_Interrupt (State : in Trap_State_Ac)
                                     return HW_Interrupt is
   begin
      return State.IRQ_Num;
   end Get_Current_HW_Interrupt;

   ----------------------------
   -- End_Of_Timer_Interrupt --
   ----------------------------

   procedure End_Of_Timer_Interrupt is
   begin
      null;
   end End_Of_Timer_Interrupt;

   ---------------------------------------------------------------------------
   -- Save and Restore the Flags Register ------------------------------------
   ---------------------------------------------------------------------------

   EFlags_Interrupts_Enabled : constant := 1;
   EFlags_Interrupts_Disabled : constant := 0;

   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer) is
   begin
      pragma Compile_Time_Warning (True, "Save_Flags_And_Disable_Interrupts");
      EFlags := (if Are_Interrupts_Enabled
                 then EFlags_Interrupts_Enabled
                 else EFlags_Interrupts_Disabled);
      Disable_Interrupts;
   end Save_Flags_And_Disable_Interrupts;

   function Save_Flags return Integer is
   begin
      pragma Assert (False);  --  Never used
      if Are_Interrupts_Enabled then
         return EFlags_Interrupts_Enabled;
      else
         return EFlags_Interrupts_Disabled;
      end if;
   end Save_Flags;

   procedure Restore_Flags (EFlags : in Integer) is
   begin
      pragma Compile_Time_Warning (True, "Restore_Flags");
      if EFlags = EFlags_Interrupts_Enabled then
         Enable_Interrupts;
      end if;
   end Restore_Flags;

   ---------------------------------------------------------------------------
   -- Bit Operations ---------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Bit_Set (Bit_Field : in out Integer_Types.Unsigned_32;
                      Bit : in Integer_Types.Unsigned_32) is
      function set_bit_from_zeros(Bit_To_Set: in Integer_Types.Unsigned_32)
                                  return Integer_Types.Unsigned_32;
      pragma Import(C, set_bit_from_zeros, "set__bit__from__zeros");
      --  pragma Inline_Always(set_bit_from_zeros);
   begin
      System.Machine_Code.Asm
        ("orr %0, %1, %2",
         Integer_Types.Unsigned_32'Asm_Output ("=r", Bit_Field),
         (Integer_Types.Unsigned_32'Asm_Input ("0", Bit_Field),
          Integer_Types.Unsigned_32'Asm_Input ("r", Set_Bit_From_Zeros(Bit))),
         "", True);
   end Bit_Set;

   procedure Bit_Reset (Bit_Field : in out Integer_Types.Unsigned_32;
                        Bit : in Integer_Types.Unsigned_32) is
      function set_bit_from_zeros(Bit_To_Set: in Integer_Types.Unsigned_32)
                                  return Integer_Types.Unsigned_32;
      pragma Import(C, set_bit_from_zeros, "set__bit__from__zeros");
      --  pragma Inline_Always(set_bit_from_zeros);
   begin
      System.Machine_Code.Asm
        ("bic %0, %1, %2",
         Integer_Types.Unsigned_32'Asm_Output ("=r", Bit_Field),
         (Integer_Types.Unsigned_32'Asm_Input ("0", Bit_Field),
          Integer_Types.Unsigned_32'Asm_Input ("r", Set_Bit_From_Zeros(Bit))),
         "", True);
   end Bit_Reset;

   --------------------------------------------------------------------------
   -- Time and Timers -------------------------------------------------------
   --------------------------------------------------------------------------

   -- Time from reset (in units of 1/HWT_HZ)

   Now : HWTime := 0;
   pragma Volatile (Now);

   --  'HWT_HZ' is the number of 'HWTime' units per second.

   HWT_HZ : constant HWTime := 10_000;

   --  Should have the same value than OSI.Ticks_Per_Second

   procedure HAL_IncTick
     with Import => True, Convention => C, External_Name => "HAL_IncTick";
   pragma Weak_External (HAL_IncTick);
   --  Defined in stm32f7xx_hal.c (stm32cube library)
   --  The address of this Weak_External symbol will be Null_Address if the
   --  stm32cube library is not included in the linking stage.

   ---------------------------
   -- Timer_Handler_Wrapper --
   ---------------------------

   --  Exported as "__gnat_sys_tick_trap"

   procedure Timer_Handler_Wrapper is
      procedure Start_Interrupt;
      pragma Import (C, Start_Interrupt, "scheduler__start_interrupt");

      procedure End_Interrupt;
      pragma Import (C, End_Interrupt, "scheduler__end_interrupt");

      procedure Do_Scheduling;
      pragma Import (C, Do_Scheduling, "do_scheduling");

      use type System.Address;

   begin
      pragma Compile_Time_Warning
        (True, "Mover Handler_Wrapper a otro paquete para todas las IRQs");

      pragma Assert (not Are_Interrupts_Enabled);
      pragma Assert (Registers.IPSR /= Registers.IPSR_Thread_Mode);

      Start_Interrupt;

      Interrupt_Nesting_Count := Interrupt_Nesting_Count + 1;

      --  Increment time

      Now := Now + 1;
      if HAL_IncTick'Address /= System.Null_Address and then
        Now mod 10 = 0
      then
         --  Increment time in the STM32Cube library (1Khz clock, see
         --  uwTickFreq initialization in stm32f7xx_hal.c)

         HAL_IncTick;
      end if;

      --  Execute timer handler

      Timer_Handler_Ac (null);

      Interrupt_Nesting_Count := Interrupt_Nesting_Count - 1;

      if Interrupt_Nesting_Count = 0 then
--           GNAT.IO.Put (" IRQHdlr->DoSched ");
         --  Interrupt_Nesting_Count := 0x80 done en Do_Scheduling
         Do_Scheduling;

      else
         pragma Assert (Interrupt_Nesting_Count = 16#80#);

--           GNAT.IO.Put (" IRQHdlr->End_Int ");
         End_Interrupt;

      end if;

   end Timer_Handler_Wrapper;

   ---------------------
   -- Get_HWTime_Slow --
   ---------------------

   function Get_HWTime_Slow return HWTime is
      Tmp : HWTime;
   begin
      Disable_Interrupts;
      Tmp := Now;
      Enable_Interrupts;
      return Tmp;
   end Get_HWTime_Slow;

   ----------------
   -- Get_HWTime --
   ----------------

   function Get_HWTime return HWTime is
   begin
      pragma Assert (not Are_Interrupts_Enabled);
      return Now;
   end Get_HWTime;

   function To_HWT_NS is new Ada.Unchecked_Conversion (Duration, HWTime);
   function To_Duration is new Ada.Unchecked_Conversion (HWTime, Duration);
   NS_Per_S : constant HWTime := 10#1#E9;

   ------------------------
   -- HWTime_To_Duration --
   ------------------------

   function HWTime_To_Duration (Th : in HWTime) return Duration is
      S : HWTime; -- := Th;
   begin
      pragma Assert (HWT_HZ /= 0);
      S := Th / HWT_HZ;
      return To_Duration (S * NS_Per_S +
                          ((Th - S * HWT_HZ) * NS_Per_S) /
                          HWT_HZ);
   end HWTime_To_Duration;
   pragma Inline (HWTime_To_Duration);

   ------------------------
   -- Duration_To_HWTime --
   ------------------------

   function Duration_To_HWTime (D : in Duration) return HWTime is
      S : HWTime; -- := To_HWT_NS (D);
   begin
      pragma Assert (HWT_HZ /= 0);
      S  := To_HWT_NS (D) / NS_Per_S;
      return S * HWT_HZ +
             ((To_HWT_NS (D) - S * NS_Per_S) * HWT_HZ) / NS_Per_S;
   end Duration_To_HWTime;
   pragma Inline (Duration_To_HWTime);

   ---------------------------------
   -- Safe_Longest_Timer_Interval --
   ---------------------------------

   function Safe_Longest_Timer_Interval return HWTime is
   begin
      --  Not used
      return HWTime'Last;
   end Safe_Longest_Timer_Interval;

   ---------------------------
   -- Get_HWClock_Frequency --
   ---------------------------

   function Get_HWClock_Frequency return HWTime is
   begin
      pragma Assert (HWT_HZ /= 0);
      return HWT_HZ;
   end Get_HWClock_Frequency;

   -------------------
   -- CPU_Frequency --
   -------------------
   function CPU_Frequency return HWTime is
   begin
      return BBOPA.Main_Clock_Frequency;
   end CPU_Frequency;

   -------------------
   -- Program_Timer --
   -------------------

   procedure Program_Timer (Timer           : in HW_Timers;
                            Interval        : in HWTime;
                            Next_Activation : out HWTime) is
      pragma Unreferenced (Interval);
   begin
      --pragma Assert (Timer = X);
      pragma Assert (not Are_Interrupts_Enabled);

      Next_Activation := Now + 1;
   end Program_Timer;

   ----------------------------
   -- RTC_HWTime_Since_Epoch --
   ----------------------------
   function RTC_HWTime_Since_Epoch return HWTime is
   begin
      return 0;
   end RTC_HWTime_Since_Epoch;

   ---------------------------------------------------------------------------
   -- Context Swich ----------------------------------------------------------
   ---------------------------------------------------------------------------

   Old_Task : System.Address with Volatile;
   New_Task : System.Address with Volatile;

   --------------------
   -- Context_Switch --
   --------------------

   --  In this architecture, this procedure triggers the PendSV interrrupt.
   --  The context switch is preformed in the PendSV handler
   --  (Perform_Context_Switch).

   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address) is
   begin
      pragma Assert (not Are_Interrupts_Enabled);

--        GNAT.IO.Put_Line ("-TrigCS-");

      HAL.Old_Task := Old_Task;
      HAL.New_Task := New_Task;

      --  Trigger context switch (make PendSV interrupt pending)

      Registers.ICSR := Registers.ICSR_Pend_SV_Set;

      --  Memory must be clobbered, as task switching causes a task to signal,
      --  which means its memory changes must be visible to all other tasks.

      Asm ("", Volatile => True, Clobber => "memory");
   end Context_Switch;

   -------------------------------
   -- Is_Context_Switch_Pending --
   -------------------------------

   function Is_Context_Switch_Pending return Boolean is
   begin
      return (Registers.ICSR and Registers.ICSR_Pend_SV_Set) /= 0;
   end Is_Context_Switch_Pending;

   ----------------------------
   -- Perform_Context_Switch --
   ----------------------------

   --  PendSV interrupt handler ("__gnat_pend_sv_trap").
   --  This interrupt is triggered each time a context switch is required.
   --  Based on BB.CPU_Primitives.Context_Switch_Trigger.Pend_SV_Handler

   procedure Perform_Context_Switch;
   pragma Machine_Attribute (Perform_Context_Switch, "naked");
   pragma Export (Asm, Perform_Context_Switch, "__gnat_pend_sv_trap");
   --  This assembly routine needs to save and restore registers without
   --  interference. The "naked" machine attribute communicates this to GCC.

   procedure Perform_Context_Switch is
   begin
--      GNAT.IO.Put_Line ("-PendSV-");
      Asm
        (Template =>
         "ldr r2,=marte__hal__old_task" & NL & -- Load address of Old_Task
         "mrs  r12, PSP "     & NL & -- Retrieve current PSP
         "ldr  r3, [r2]"      & NL & -- Load address of running context buffer

         --  If floating point is enabled, we may have to save the non-volatile
         --  floating point registers, and save bit 4 of the LR register, as
         --  this will indicate whether the floating point context was saved
         --  or not.

         (if not BBParam.Has_FPU then "" -- No FP context to save
          else
            "tst  lr, #16"            & NL &  -- if FPCA flag was set,
            "itte  eq"                & NL &  -- then
            "vstmdbeq r12!,{s16-s31}" & NL &  --   save FP context below PSP
            "addeq  r12, #1"          & NL &  --   save flag in bit 0 of PSP
            "subne  lr, #16"          & NL) & -- else set FPCA flag in LR

         --  Store R4-R11 and PSP (stored in R12) in the context buffer. The
         --  context buffer is not on the stack.
         (if BBParam.Is_ARMv6m then
              --  Save context using armv6-m instructions
              "stm  r3!, {r4-r7}"     & NL &
              "mov  r4, r8"           & NL &
              "mov  r5, r9"           & NL &
              "mov  r6, r10"          & NL &
              "mov  r7, r11"          & NL &
              "stm  r3!, {r4-r7}"     & NL &
              "mov  r4, r12"          & NL &
              "stm  r3!, {r4}"        & NL
         else
              "stm  r3, {r4-r12}"     & NL) & -- Save context

         "ldr  r3,=marte__hal__new_task" & NL &
         "ldr  r3, [r3]"  & NL & -- Load address of new context
         "str  r3, [r2]"  & NL & -- Update value of __gnat_running_thread_table

         --  Load R4-R11 and PSP (stored in R12) from the new context buffer
         (if BBParam.Is_ARMv6m then
                --  Load context using armv6-m instructions
                "movs r2, #0x20"  & NL &
                "add  r2, r3, r2" & NL &  -- Move R2 where PSP is stored in
                                    NL &  -- the context buffer.
                "ldr  r4, [r2]"   & NL &  -- Load PSP from context buffer
                "mov  r12, r4"    & NL &  -- Set new stack

                "movs r2, #0x10"    & NL &
                "add  r2, r3, r2"   & NL & -- Move R2 where R8 is stored
                "ldm  r2!, {r4-r7}" & NL & -- Load R8-R11 from context buffer
                "mov  r8,  r4"      & NL &
                "mov  r9,  r5"      & NL &
                "mov  r10, r6"      & NL &
                "mov  r11, r7"      & NL &

                "mov  r2, r3" & NL & -- Move R2 where R4 is stored in
                                NL & -- the context buffer.
                "ldm  r2!, {r4-r7}" & NL   -- Load R4-R7 from context buffer
         else
                "ldm  r3, {r4-r12}" & NL) & -- Load context and new PSP

         --  If floating point is enabled, check bit 0 of PSP to see if we
         --  need to restore the floating point context.

         (if not BBParam.Has_FPU then ""     -- No FP context to restore
          else
            "tst  r12, #1"            & NL &  -- if FPCA was set,
            "itte  ne"                & NL &  -- then
            "subne r12, #1"           & NL &  --   remove flag from PSP
            "vldmiane r12!,{s16-s31}" & NL &  --   Restore FP context
            "addeq lr, #16"           & NL) & -- else clear FPCA flag in LR

         --  Finally, update PSP and perform the exception return

         "msr  PSP, r12" & NL &        -- Update PSP
         "bx   lr",                    -- return to caller
         Volatile => True);
   end Perform_Context_Switch;

   -----------------------
   -- Change_To_Context --
   -----------------------

   procedure Change_To_Context (New_Task : in System.Address) is
   begin
      pragma Compile_Time_Warning (True, "Change_To_Context not implemented");
      pragma Assert (False);
      null;
   end Change_To_Context;

   ---------------------------------------------
   -- Change_Return_Address_Of_Preempted_Task --
   ---------------------------------------------

   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address)
   is
      procedure C_Change_LR(Top_Of_Stack    : in Unsigned_32;
                            New_Ret_Address : in System.Address);
      pragma Import (C, C_Change_LR, "change_lr");
   begin
      pragma Compile_Time_Warning (True,
                                   "Change_Return_Address_Of_Preempted_Task");
      pragma Assert (False);
      C_Change_LR(Top_Of_Stack, New_Ret_Address);
   end Change_Return_Address_Of_Preempted_Task;

   -----------------------
   -- Init_Task_Context --
   -----------------------

   --  Based on BB.CPU_Primitives.Initialize_Context

   procedure Init_Task_Context (Context_Ac : access Arch_Specific_Context_T;
                                Task_Wrapper_Address : System.Address;
                                Task_Body_Address : System.Address;
                                Stack_Pointer   : Integer_Types.Unsigned_32) is
      use System.Storage_Elements;
      pragma Compile_Time_Error (Integer_Address'Size /= System.Word_Size,
                                 "Not 32B");

      type Hardware_Context is record
         R0, R1, R2, R3   : Integer_Address;
         R12, LR, PC, PSR : Integer_Address;
      end record;
      --  Frame pushed onto the stack on exception entry

      SP_Process : constant Context_Id := 8;

      HW_Ctx_Bytes : constant Integer_Address := Hardware_Context'Size / 8;
      New_SP       : constant System.Address :=
        To_Address ((Integer_Address (Stack_Pointer) - HW_Ctx_Bytes) and not 4);

      HW_Ctx : Hardware_Context with Address => New_SP;

   begin
      HW_Ctx := (R0     => To_Integer (Task_Body_Address),
                 PC     => To_Integer (Task_Wrapper_Address),
                 PSR    => 2**24, -- Set thumb bit
                 others => 0);

      Context_Ac.all := (SP_process => New_SP, others => System.Null_Address);
   end Init_Task_Context;

   -------------------------------
   -- Main_Task_Stack_Top_Limit --
   -------------------------------

   function Main_Task_Stack_Top_Limit return System.Address is
      Stack_Start : Integer
        with Import => True, Convention => C, External_Name => "__stack_start";
   begin
      return Stack_Start'Address;
   end Main_Task_Stack_Top_Limit;

   function Main_Task_Stack_Base return System.Address is
      Stack_End : Integer
        with Import => True, Convention => C, External_Name => "__stack_end";
   begin
      return Stack_End'Address;
   end Main_Task_Stack_Base;

   -------------------------
   -- Finish_Hardware_Use --
   -------------------------

   procedure Finish_Hardware_Use is
   begin
      null;
   end Finish_Hardware_Use;

   --------------------
   -- Initialize_CPU --
   --------------------

   --  Initialize vector table (in Cortex-M4) and faults
   --  Taken from Initialize_CPU (s-bbcppr.adb)

   procedure Initialize_CPU;

   procedure Initialize_CPU is
--        Interrupt_Stack_Table : array (System.Multiprocessors.CPU)
--          of System.Address;
--        pragma Import (Asm, Interrupt_Stack_Table, "interrupt_stack_table");
      --  Table containing a pointer to the top of the stack for each processor
      --  M2OS uses the same stack for interrupts and threads

      VTOR : System.Address with Volatile,
        Address => System.Storage_Elements.To_Address (16#E000_ED08#);
      --  Vec. Table Offset

      type Word is mod 2**System.Word_Size;

      AIRCR : Word with Volatile,  -- App Int/Reset Ctrl
        Address => System.Storage_Elements.To_Address (16#E000_ED0C#);
      CCR   : Word with Volatile, -- Config. Control
        Address => System.Storage_Elements.To_Address (16#E000_ED14#);
      SHPR1 : Word with Volatile, -- Sys Hand  4- 7 Prio
        Address => System.Storage_Elements.To_Address (16#E000_ED18#);
      SHPR2 : Word with Volatile, -- Sys Hand  8-11 Prio
        Address => System.Storage_Elements.To_Address (16#E000_ED1C#);
      SHPR3 : Word with Volatile, -- Sys Hand 12-15 Prio
        Address => System.Storage_Elements.To_Address (16#E000_ED20#);
      SHCSR : Word with Volatile, -- Sys Hand Ctrl/State
        Address => System.Storage_Elements.To_Address (16#E000_ED24#);

      System_Vectors : constant System.Address;
      pragma Import (Asm, System_Vectors, "__vectors");
      --  Defined in handler_stm32f769disco.S o handler_stm32f4.S depending on the architecture

      use type SSE.Storage_Offset;

      Interrupt_Stack_Last_DWord_Address : constant System.Address :=
        Interrupt_Stack (Interrupt_Stack_Space'Last - 7)'Address;
      --  Address of the last double word in the stack

   begin
      if BBParam.Has_OS_Extensions then
         --  Switch the stack pointer to SP_process (PSP)

         Asm ("mrs r0, MSP" & NL &
                "msr PSP, r0" & NL &
                "mrs r0, CONTROL" & NL &
                "movs r1, #2" & NL &
                "orr r0,r0,r1" & NL &
                "msr CONTROL,r0" & NL &
                "mrs r0, CONTROL",
              Clobber => "r0,r1",
              Volatile => True);

         --  Initialize SP_main (MSP)

         Asm ("msr MSP, %0",
              Inputs => System.Address'Asm_Input
                ("r", Interrupt_Stack_Last_DWord_Address),
              Volatile => True);
      end if;

      if System.BB.Parameters.Has_VTOR then
         --  Replaces the initial vector table (set in start-rom.S) by this
         --  other (more complete) table.
         --  Not in Cortex-M0
         VTOR := System_Vectors'Address;
      end if;

      --  Set configuration: stack is 8 byte aligned, trap on divide by 0,
      --  no trap on unaligned access, can enter thread mode from any level.

      CCR := CCR or 16#211#;

      --  In MaRTE all handlers run at the maximum priority (0). Consequently
      --  SysTick and PendSV handlers can not preempt each other.
      --  IN GNAT BB the PendSV handler runs at the lowest priority..

      SHPR1 := 0;
      SHPR2 := 0;
      SHPR3 := 16#00_00_00_00#;

      if not System.BB.Parameters.Is_ARMv6m then

         --  Write the required key (16#05FA#) and desired PRIGROUP value. We
         --  configure this to 3, to have 16 group priorities

         --  Not in Cortex-M0

         AIRCR := 16#05FA_0300#;
         pragma Assert (AIRCR = 16#FA05_0300#); --  Key value is swapped
      end if;

      --  Enable usage, bus and memory management fault

      SHCSR := SHCSR or 16#7_0000#;

      --  Call context switch hardware initialization
      --  Initialize_Context_Switch; (does nothing in GNAT BB)

      --  Unmask Fault

      Asm ("cpsie f", Volatile => True);
   end Initialize_CPU;

   ----------------
   -- Initialize --
   ----------------

   --  Part of the code is based on Initialize_CPU (s-bbcppr.adb)

   procedure Initialize is
      procedure MaRTE_HAL_Init_Timer (Period_In_Ticks : Interfaces.Unsigned_32);
      pragma Import (C, MaRTE_HAL_Init_Timer, "marte_hal_init_timer");
      --  From stm32f_arch/hwi/stm32f4/marte_hal_init_timer.c

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion (System.Address, Interfaces.Unsigned_32);

      procedure Premain
        with Import, Convention => C, External_Name => "_Z7premainv";
      procedure Arduino_Init
            with Import, Convention => C, External_Name => "init";
      --pragma Import (C, Arduino_Init, "init");
      --  procedure Arduino_InitVariant
      --      with Import, Convention => C, External_Name => "initVariant";
   begin
      pragma Assert (System.Address'Size = Interfaces.Unsigned_32'Size);

       --  Initialize vector table (in Cortex-M4) and faults

      Initialize_CPU;

      pragma Assert (Registers.IPSR = Registers.IPSR_Thread_Mode);

      Disable_Interrupts;
      pragma Assert (not Are_Interrupts_Enabled);

      --Premain;
      --Arduino_Init;
      --  Arduino_InitVariant;

      --  Init SysTimer: program SysTimer to produce an interrupt each
      --  Period_In_Ticks core clock ticks.
      MaRTE_HAL_Init_Timer
        (Period_In_Ticks =>
           Interfaces.Unsigned_32 (BBOPA.Main_Clock_Frequency / HWT_Hz));
      --  Period [Ticks] = Clock_Frequency [Ticks/s] / HWTime_HZ [1/s]
      --  In STM32F4     = 168_000_000 / 10_000 = 16_800


      --  Sys_Tick and PendSV handlers should run at highest priority (0)

      pragma Assert (Registers.Sys_Tick_Handler_Prio = 0);
      pragma Assert (Registers.PendSV_Handler_Prio = 0);

      Initialized := True;
   end Initialize;

end MaRTE.HAL;
