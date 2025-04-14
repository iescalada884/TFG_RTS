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
--  GNAT BB ARM  Version.
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
with System.BB.Threads;
with System.BB.Threads.Queues;
with System.BB.CPU_Primitives;
with System.BB.Protection;
with System.BB.Board_Parameters;
pragma Warnings (On);

with Ada.Unchecked_Conversion;

with System.Machine_Code; use System.Machine_Code;
with Interfaces;
with GNAT.IO;

package body MaRTE.HAL is

   Initialized : Boolean := False;

   use type HWTime;

   --------------------------------------------------------------------------
   -- Interrupts ------------------------------------------------------------
   --------------------------------------------------------------------------

   Interrupt_Nesting_Count : Unsigned_8 := 16#80#;
   --  16#80# => Outside_Of_Interrupts
   pragma Export (C, Interrupt_Nesting_Count, "base_irq_nest");

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

   procedure Timer_Handler_Wrapper is
      procedure Start_Interrupt;
      pragma Import (C, Start_Interrupt, "scheduler__start_interrupt");

      procedure End_Interrupt;
      pragma Import (C, End_Interrupt, "scheduler__end_interrupt");

      procedure Do_Scheduling;
      pragma Import (C, Do_Scheduling, "do_scheduling");

   begin
      pragma Compile_Time_Warning
        (True, "Mover Handler_Wrapper a otro paquete para todas las IRQs");

      Start_Interrupt;

      Interrupt_Nesting_Count := Interrupt_Nesting_Count + 1;

      --  Execute timer handler

      Timer_Handler_Ac (null);

      Interrupt_Nesting_Count := Interrupt_Nesting_Count - 1;

      if Interrupt_Nesting_Count = 0 then
--           GNAT.IO.Put (" IRQHdlr->DoSched ");
         --  Interrupt_Nesting_Count := 0x80  done en Do_Scheduling
         Do_Scheduling;

      else
--           GNAT.IO.Put (" IRQHdlr->End_Int ");
         End_Interrupt;
      end if;

   end Timer_Handler_Wrapper;

   ----------------------------------
   -- Install_HW_Interrupt_Handler --
   ----------------------------------

   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler) is
   begin
      Timer_Handler_Ac := Handler;

      if Int_Num = Sys_Tick_Vector then
         System.BB.Time.Set_MaRTE_Alarm_Handler (Timer_Handler_Wrapper'Access);

      else
         null;
         pragma Compile_Time_Warning
           (True,
            "Solo se instala la IRQ del timer, las demas no");
      end if;
   end Install_HW_Interrupt_Handler;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      System.BB.CPU_Primitives.Disable_Interrupts;
   end Disable_Interrupts;


   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      System.BB.CPU_Primitives.Enable_Interrupts (Level => 0);
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
   begin
      pragma Compile_Time_Warning (True, "Are_Interrupts_Enabled");
      return False;
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

   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer) is
   begin
      pragma Compile_Time_Warning
        (True, "Save_Flags_And_Disable_Interrupts calls BB.Enter_Kernel");
      EFlags := 0;
      System.BB.Protection.Enter_Kernel;
   end Save_Flags_And_Disable_Interrupts;

   function Save_Flags return Integer is
      Aux : Boolean;
   begin
      pragma Assert (False);  --  Never used
      Aux := Are_Interrupts_Enabled;
      if Aux = True then
         return 1;
      else
         return 0;
      end if;
   end Save_Flags;

   procedure Restore_Flags (EFlags : in Integer) is
   begin
      pragma Compile_Time_Warning
        (True, "Restore_Flags calls BB.Leave_Kernel");
      System.BB.Protection.Leave_Kernel;
   end Restore_Flags;

   ---------------------------------------------------------------------------
   -- Bit Operations ---------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Bit_Set (Bit_Field : in out Unsigned_32;
                      Bit : in Unsigned_32) is
      function set_bit_from_zeros(Bit_To_Set: in Unsigned_32) return Unsigned_32;
      pragma Import(C, set_bit_from_zeros, "set__bit__from__zeros");
      --  pragma Inline_Always(set_bit_from_zeros);
   begin
      System.Machine_Code.Asm ("orr %0, %1, %2",
                               Unsigned_32'Asm_Output ("=r", Bit_Field),
           (Unsigned_32'Asm_Input ("0", Bit_Field),
         Unsigned_32'Asm_Input ("r", set_bit_from_zeros(Bit))), "", True);
   end Bit_Set;

   procedure Bit_Reset (Bit_Field : in out Unsigned_32;
                        Bit : in Unsigned_32) is
      function set_bit_from_zeros(Bit_To_Set: in Unsigned_32) return Unsigned_32;
      pragma Import(C, set_bit_from_zeros, "set__bit__from__zeros");
      --  pragma Inline_Always(set_bit_from_zeros);
   begin
      System.Machine_Code.Asm ("bic %0, %1, %2",
                               Unsigned_32'Asm_Output ("=r", Bit_Field),
           (Unsigned_32'Asm_Input ("0", Bit_Field),
         Unsigned_32'Asm_Input ("r", set_bit_from_zeros(Bit))), "", True);
   end Bit_Reset;

   ---------------------
   -- Get_HWTime_Slow --
   ---------------------

   function Get_HWTime_Slow return HWTime is
   begin
      return System.BB.Time.Clock;
   end Get_HWTime_Slow;

   ----------------
   -- Get_HWTime --
   ----------------

   function Get_HWTime return HWTime is
   begin
      return System.BB.Time.Clock;
   end Get_HWTime;

   HWT_HZ : constant HWTime := System.Bb.Board_Parameters.Main_Clock_Frequency;

   function To_HWT_NS is new Unchecked_Conversion (Duration, HWTime);
   function To_Duration is new Unchecked_Conversion (HWTime, Duration);
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
      return HWT_HZ;
   end CPU_Frequency;

   -------------------
   -- Program_Timer --
   -------------------

   procedure Program_Timer (Timer           : in HW_Timers;
                            Interval        : in HWTime;
                            Next_Activation : out HWTime) is
   begin
      System.BB.Time.Update_Alarm (Get_HWTime + Interval);
      Next_Activation := System.BB.Time.Get_Next_Timeout (CPU_Id => 1);
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

   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address) is
      function To_BB_Thread_Id is
        new Unchecked_Conversion (System.Address,
                                  System.BB.Threads.Thread_Id);
   begin
      pragma Compile_Time_Warning
        (True,
         "Falta configurar el context de las tareas recién creadas");

      --  Sets Old_Task and new tasks context

      System.BB.Threads.Queues.Running_Thread_Table (1) :=
        To_BB_Thread_Id (Old_Task);
      System.BB.Threads.Queues.First_Thread_Table (1) :=
        To_BB_Thread_Id (New_Task);

      -- A context switch is required

      System.BB.CPU_Primitives.Context_Switch;
   end Context_Switch;

   -----------------------
   -- Change_To_Context --
   -----------------------

   procedure Change_To_Context (New_Task : in System.Address) is
   begin
      pragma Compile_Time_Warning
        (True, "Change_To_Context not implemented");
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
      pragma Assert (False);
      C_Change_LR(Top_Of_Stack, New_Ret_Address);
   end Change_Return_Address_Of_Preempted_Task;

   -----------------------
   -- Init_Task_Context --
   -----------------------

   procedure Init_Task_Context (Context_Ac : access Arch_Specific_Context_T;
                                Task_Wrapper_Address : System.Address;
                                Task_Body_Address : System.Address;
                                Stack_Pointer   : Unsigned_32) is
      function To_Address is
         new Unchecked_Conversion (Unsigned_32, System.Address);
   begin
      System.BB.CPU_Primitives.Initialize_Context
        (Buffer          => Context_Ac,
         Program_Counter => Task_Wrapper_Address,
         Argument        => Task_Body_Address,
         Stack_Pointer   => To_Address (Stack_Pointer));
   end Init_Task_Context;


   -------------------------------
   -- Main_Task_Stack_Top_Limit --
   -------------------------------

   function Main_Task_Stack_Top_Limit return System.Address is
   begin
      return System.Null_Address;
   end Main_Task_Stack_Top_Limit;

   function Main_Task_Stack_Base return System.Address is
   begin
      return System.Null_Address;
   end Main_Task_Stack_Base;

   -------------------------
   -- Finish_Hardware_Use --
   -------------------------
   procedure Finish_Hardware_Use is
   begin
      null;
   end Finish_Hardware_Use;

   Environment_Thread : aliased System.BB.Threads.Thread_Descriptor;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize is
   begin
--      System.BB.Threads.Initialize (Environment_Thread'Access,
--                                    System.Default_Priority);
      pragma Compile_Time_Warning
        (True,
         "Queues debe retornar Environment_Thread Como Running_Task");

      Initialized := True;
   end Initialize;

end MaRTE.HAL;
