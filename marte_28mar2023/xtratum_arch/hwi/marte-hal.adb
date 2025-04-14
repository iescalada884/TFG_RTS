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
--  XtratuM version.
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
with Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;

with MaRTE.HAL.Processor_Registers;
with MaRTE.HAL.RTC;

with Interfaces;

package body MaRTE.HAL is

   Initialized : Boolean := False;

   -----------------------
   -- XtratuM constants --
   -----------------------

   XM_OK : constant := 0;  --  No error

   XM_HW_CLOCK   : constant := 0; --  Monotonic clock
   XM_EXEC_CLOCK : constant := 1; --  Partition execution-time clock

   --------------------------------------------------------------------------
   -- Interrupts ------------------------------------------------------------
   --------------------------------------------------------------------------

   -----------------------
   -- Xtratum_Interrupt --
   -----------------------

   function Xtratum_Interrupt (XM_Trap : HW_Interrupt) return XM_Interrupt is
   begin
      return XM_Interrupt ((XM_Trap - XM_VT_EXT_FIRST) + XAL_XMEXT_TRAP);
   end Xtratum_Interrupt;

   ---------------------
   -- Timer_Interrupt --
   ---------------------

   function Timer_Interrupt return HW_Interrupt is
   begin
      return XAL_XMEXT_TRAP + (XM_VT_EXT_HW_TIMER - XM_VT_EXT_FIRST);
   end Timer_Interrupt;

   ----------------------------------
   -- Default_HW_Interrupt_Handler --
   ----------------------------------

   procedure Default_HW_Interrupt_Handler (State : in Trap_State_Ac) is
      function XM_Write_Console (Str : System.Address; Length: Integer) --  BORRAR
         return Integer;
      --  int XM_write_console(char *buffer, int length);
      pragma Import (C, XM_Write_Console, "XM_write_console");
      Str : String := " -INTR- ";
      Num : String := State.IRQ_Num'Img;
      Ret : Integer;
   begin
      Ret := XM_Write_Console (Str'Address, 8);
      Ret := XM_Write_Console (Num'Address, Num'Length);
   end Default_HW_Interrupt_Handler;

   -----------------------------
   -- User_Interrupt_Handlers --
   -----------------------------

   User_Interrupt_Handlers : array (HW_Interrupt) of HW_Interrupt_Handler :=
      (others => Default_HW_Interrupt_Handler'Access);

   --------------
   -- IRQ flag --
   --------------

   Outside_Of_Interrupts : constant := 16#80#;
   Invoke_Do_Scheduling_Mask : constant := 16#7F#; -- not 16#80#;

   IRQ_Flag : Unsigned_8 := Outside_Of_Interrupts;
   pragma Export (C, IRQ_Flag, "base_irq_nest");
   --  XXX this is a bit messy, it is an inheritance of OSKit that should be
   --  changed in a more portable way.
   --  This flag is used to know if we are executing an interrupt handler
   --  and also to know if scheduler should be called at the end of
   --  interrupt handler
   --  lower bit set to 1 => in interrupt handler
   --  higher bit set to 0 => call scheduler at the end of the handler

   ----------------------------------------
   -- Dependencies from Kernel.Scheduler --
   ----------------------------------------

   --  XXX this shouldn't be here. It should be in in a kernel package HAL
   --  independent

   procedure Scheduler_Start_Interrupt;
   pragma Import (C, Scheduler_Start_Interrupt, "scheduler__start_interrupt");

   procedure Scheduler_End_Interrupt;
   pragma Import (C, Scheduler_End_Interrupt, "scheduler__end_interrupt");

   procedure Do_Scheduling;
   pragma Import (C, Do_Scheduling, "do_scheduling");

   function XM_Mask_IRQ (HWIRQ : Unsigned_32; EXTIRQ : Unsigned_32) return Integer;
   pragma Import (C, XM_Mask_IRQ, "XM_set_irqmask");

   function XM_Unmask_IRQ (HWIRQ : Unsigned_32; EXTIRQ : Unsigned_32) return Integer;
   pragma Import (C, XM_Unmask_IRQ, "XM_clear_irqmask");

   ------------------------
   -- Global IRQ handler --
   ------------------------

   --  It is called from 'marte_extirqhandler.c'

   procedure ExtIrqHandlerAda (State : Trap_State);
   pragma Export (C, ExtIrqHandlerAda, "ExtIrqHandlerAda");
   pragma Export_Procedure (ExtIrqHandlerAda, "ExtIrqHandlerAda",
      Mechanism => (State => Value));

   procedure ExtIrqHandlerAda (State : Trap_State) is
      function XM_Write_Console (Str : System.Address; Length: Integer)
           return Integer;
      --  --  int XM_write_console(char *buffer, int length);
      pragma Import (C, XM_Write_Console, "XM_write_console");
      Str : String := " -INTR0- ";
      Ret : Integer;
      Num : String := State.IRQ_Num'Img;
   begin
      -- Ret := XM_Write_Console (Str'Address, 9);
      -- Ret:=XM_Write_Console (Num'Address, Num'Length);
      --  Inform scheduler a handler is in execution

      Scheduler_Start_Interrupt;
      IRQ_Flag := IRQ_Flag + 1;

      --  Execute user interrupt handler

      User_Interrupt_Handlers (State.IRQ_Num).all (State'Unrestricted_Access);

      --  Re-Unmask interrupt

      Hardware_Interrupt_Controller_Enable_Interrupt (State.IRQ_Num);


      -- The interrupt has finished

      IRQ_Flag := IRQ_Flag - 1;

      --  Call MaRTE scheduler?

      if IRQ_Flag = 0 then
         Do_Scheduling;
         --  IRQ_Flag := Outside_Of_Interrupts  done en do_scheduling

      else
         Scheduler_End_Interrupt;
      end if;

   end ExtIrqHandlerAda;

   ----------------------------------
   -- Install_HW_Interrupt_Handler --
   ----------------------------------

   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler) is

      function Install_Handler (Num_IRQ    : HW_Interrupt) return Integer;
      pragma Import (C, Install_Handler, "install_handler");

      function XM_Write_Console (Str : System.Address; Length: Integer)
           return Integer;
      --  --  int XM_write_console(char *buffer, int length);
      pragma Import (C, XM_Write_Console, "XM_write_console");
      Ret : Integer;

   begin
      pragma Assert (Initialized);
      User_Interrupt_Handlers (Int_Num) := Handler;
	  Ret := Install_Handler (Int_Num);
      pragma Assert (Ret = XM_OK);
   end Install_HW_Interrupt_Handler;
   pragma Inline (Install_HW_Interrupt_Handler);

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   --  Not just an import of "XM_disable_irqs" because symbol
   --  marte__hal__disable_interrupts must exists to be used from out of the
   --  MaRTE kernel

   procedure Disable_Interrupts is
      procedure XM_Disable_IRQs;
      pragma Import (C, XM_Disable_IRQs, "XM_x86_clear_if");
      --pragma Import (C, XM_Disable_IRQs, "HwCli");
      Ret : Integer;
   begin
      XM_Disable_IRQs;
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   --  Not just an import of "XM_enable_irqs" because symbol
   --  marte__hal__enable_interrupts must exists to be used from outside of
   --  the MaRTE kernel

   procedure Enable_Interrupts is
      procedure XM_Enable_IRQs;
      pragma Import (C, XM_Enable_IRQs, "XM_x86_set_if");
      --pragma Import (C, XM_Enable_IRQs, "HwSti");
      Ret : Integer;
   begin
      XM_Enable_IRQs;
   end Enable_Interrupts;

   ----------------------------------------------------
   -- Hardware_Interrupt_Controller_Enable_Interrupt --
   ----------------------------------------------------

   procedure Hardware_Interrupt_Controller_Enable_Interrupt
     (Int : in HW_Interrupt) is

      function XM_Write_Console (Str : System.Address; Length: Integer)
           return Integer;
      --  --  int XM_write_console(char *buffer, int length);
      pragma Import (C, XM_Write_Console, "XM_write_console");

      procedure Unmask (Num_IRQ    : HW_Interrupt);
      pragma Import (C, Unmask, "unmask");

      STR : String := "Unmask";
      Num : String := Int'Img;
      --  xm_s32_t XM_unmask_irq (xm_u32_t irq)

      Ret : Integer;

   begin

      --Ret:=XM_Write_Console (STR'Address, 6);
      --Ret:=XM_Write_Console (Num'Address, Num'Length);
      Unmask (Int);
      --pragma Assert (Ret = XM_OK);
   end Hardware_Interrupt_Controller_Enable_Interrupt;

   -----------------------------------------------------
   -- Hardware_Interrupt_Controller_Disable_Interrupt --
   -----------------------------------------------------

   procedure Hardware_Interrupt_Controller_Disable_Interrupt
     (Int : in HW_Interrupt) is
      --  xm_s32_t XM_mask_irq (xm_u32_t irq)
      procedure Mask (Num_IRQ    : HW_Interrupt);
      pragma Import (C, Mask, "mask");

      function XM_Write_Console (Str : System.Address; Length: Integer)
           return Integer;
      --  --  int XM_write_console(char *buffer, int length);
      pragma Import (C, XM_Write_Console, "XM_write_console");

      STR : String := "Mask ";
      Num : String := Int'Img;
      Ret : Integer;

   begin
      --Ret:=XM_Write_Console (STR'Address, 5);
      --Ret:=XM_Write_Console (Num'Address, Num'Length);
      Mask (Int);
      --pragma Assert (Ret = XM_OK);
   end Hardware_Interrupt_Controller_Disable_Interrupt;

   ------------------------------------------
   -- Enable_Hardware_Interrupt_Controller --
   ------------------------------------------

   procedure Enable_Hardware_Interrupt_Controller is
   begin
      null;
   end Enable_Hardware_Interrupt_Controller;
   pragma Inline (Enable_Hardware_Interrupt_Controller);

   ----------------------------
   -- Are_Interrupts_Enabled --
   ----------------------------

   function Are_Interrupts_Enabled return Boolean is

      --  xm_s32_t XM_are_irqs_enabled(void)
      function Are_IRQs_Enabled return Integer;
      pragma Import (C, Are_IRQs_Enabled, "are_irqs_enabled");

   begin 
      return Are_IRQs_Enabled = 1;
   end Are_Interrupts_Enabled;
   pragma Inline (Are_Interrupts_Enabled);

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
      
      --  nothing has to be done in this architecture to re-enable the
      --  hardware interrupt timer
   end End_Of_Timer_Interrupt;

   ---------------------------------------------------------------------------
   -- Save and Restore the Flags Register ------------------------------------
   ---------------------------------------------------------------------------

   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer) is
   begin
      Disable_Interrupts;
   end Save_Flags_And_Disable_Interrupts;

   function Save_Flags return Integer is
   begin
      return 0;
   end Save_Flags;

   procedure Restore_Flags (EFlags : in Integer) is
   begin
      Enable_Interrupts;
   end Restore_Flags;

--   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer)
--     renames Processor_Registers.Save_Flags_And_Disable_Interrupts;
--   pragma Inline_Always (Save_Flags_And_Disable_Interrupts);

--   function Save_Flags return Integer
--     renames Processor_Registers.Save_Flags;
--   pragma Inline_Always (Save_Flags);

--   procedure Restore_Flags (EFlags : in Integer)
--     renames Processor_Registers.Restore_Flags;
   --  Exported to C  with name "restore_flags"
--   pragma Inline_Always (Restore_Flags);

   ---------------------------------------------------------------------------
   -- Bit Operations ---------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Bit_Set (Bit_Field : in out Unsigned_32;
                      Bit : in Unsigned_32) is
   begin
      Asm ("bts %2, %0", Unsigned_32'Asm_Output ("=r", Bit_Field),
           (Unsigned_32'Asm_Input ("0", Bit_Field),
            Unsigned_32'Asm_Input ("r", Bit)), "", True);
   end Bit_Set;

   procedure Bit_Reset (Bit_Field : in out Unsigned_32;
                        Bit : in Unsigned_32) is
   begin
      Asm ("btr %2, %0", Unsigned_32'Asm_Output ("=r", Bit_Field),
           (Unsigned_32'Asm_Input ("0", Bit_Field),
            Unsigned_32'Asm_Input ("r", Bit)), "", True);
   end Bit_Reset;

   procedure Bit_Scan_Reverse (Bit_Field : in Unsigned_32;
                               Bit : out Unsigned_32) is
   begin
      Asm ("bsrl %1, %0;" &
           "jnz 1f;"      &
           "movl $-1,%0;" &
           "1:",
           Unsigned_32'Asm_Output ("=d", Bit),
           Unsigned_32'Asm_Input ("d", Bit_Field),
           "", True);
   end Bit_Scan_Reverse;

   procedure Bit_Scan_Forward (Bit_Field : in Unsigned_32;
                               Bit : out Unsigned_32) is
   begin
      Asm ("bsfl %1, %0;" &
           "jnz 1f;"      &
           "movl $-1,%0;" &
           "1:",
           Unsigned_32'Asm_Output ("=d", Bit),
           Unsigned_32'Asm_Input ("d", Bit_Field),
           "", True);
   end Bit_Scan_Forward;

   --------------------------------------------------------------------------
   -- Time and Timers -------------------------------------------------------
   --------------------------------------------------------------------------

   HWT_HZ : constant := 1_000_000;
   --  'HWT_HZ' is the number of 'HWTime' units per second.
   --  Type 'HWTime' (xmTime_t) counts microseconds.

   function To_HWT_NS is new Unchecked_Conversion (Duration, HWTime);
   function To_Duration is new Unchecked_Conversion (HWTime, Duration);
   Dur_VS_HWTime : constant := 10#1#E9 / HWT_HZ;

   ------------------------
   -- HWTime_To_Duration --
   ------------------------

   function HWTime_To_Duration (Th : in HWTime) return Duration is
   begin
      --  XXX no overflow check

      return To_Duration (Th * Dur_VS_HWTime);
   end HWTime_To_Duration;
   pragma Inline (HWTime_To_Duration);

   ------------------------
   -- Duration_To_HWTime --
   ------------------------

   function Duration_To_HWTime (D : in Duration) return HWTime is
   begin
      return To_HWT_NS (D) / Dur_VS_HWTime;
   end Duration_To_HWTime;
   pragma Inline (Duration_To_HWTime);

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
      --  Assume a 100MHz processor (only used to get the value for
      --  Time_Suspension_Minimum)

      return 100_000_000;
   end CPU_Frequency;

   ---------------------------
   -- Import of XM_get_time --
   ---------------------------

   function XM_Get_Time (Clock_Id : Natural; Time : access HWTime) return Integer;
   pragma Import(C, XM_Get_Time, "XM_get_time");
   --  xm_s32_t XM_get_time (xm_u32_t clockId, *xmTime_t time)

   ----------------
   -- Get_HWTime --
   ----------------

   function Get_HWTime return HWTime is

      Time : aliased HWTime;
      Ret : Integer;

   begin
      pragma Assert (Initialized);

      Ret := XM_Get_Time (XM_HW_CLOCK, Time'access);
      pragma Assert (Ret = XM_OK);

      return Time;
   end Get_HWTime;

   ----------------------------
   -- Import of XM_set_timer --
   ----------------------------

   function XM_Set_Timer (Clock_Id : Natural;
                          Abs_Time : HWTime;
                          Interval : HWTime) return Integer;
   pragma Import(C, XM_Set_Timer, "XM_set_timer");
   --  xm_s32_t XM_set_timer (xm_u32_t clockId,
   --                         xmTime_t absTime,
   --                         xmTime_t interval);

   -------------------
   -- Program_Timer --
   -------------------

   procedure Program_Timer (Timer           : in  HW_Timers;
                            Interval        : in  HWTime;
                            Next_Activation : out HWTime) is

      Abs_Time : aliased HWTime;
      Ret : Integer;
      function XM_Write_Console (Str : System.Address; Length: Integer)
           return Integer;
      --  --  int XM_write_console(char *buffer, int length);
      pragma Import (C, XM_Write_Console, "XM_write_console");

   begin
      --  XXX it is quite inefficient the conversion between
      --  absolute->interval-absolute

      pragma Assert (Initialized);

      --  Get current time

      Ret := XM_Get_Time (XM_HW_CLOCK, Abs_Time'access);
      pragma Assert (Ret = XM_OK);

      --  Get absolute programming time

      Abs_Time := Abs_Time + Interval;

      --  Program XtratuM timer

      Ret := XM_Set_Timer (XM_HW_CLOCK, Abs_Time, Interval => 0);
      pragma Assert (Ret = XM_OK);

      --  Set Next_Activation

      Next_Activation := Abs_Time;
   end Program_Timer;

   ---------------------------------
   -- Safe_Longest_Timer_Interval --
   ---------------------------------

   function Safe_Longest_Timer_Interval return HWTime is
   begin
      pragma Assert (Initialized);

      return HWTime'Last;  --  No limit in timer programming interval
   end Safe_Longest_Timer_Interval;

   ------------------------------------
   -- Compulsory_Timer_Reprogramming --
   ------------------------------------

   function Compulsory_Timer_Reprogramming return Boolean is
   begin
      return False;
   end Compulsory_Timer_Reprogramming;

   ----------------------------
   -- RTC_HWTime_Since_Epoch --
   ----------------------------

   function RTC_HWTime_Since_Epoch return HWTime is
   begin
      return Duration_To_HWTime
        (Duration (RTC.RTC_Time_To_Seconds_Since_Epoch));
   end RTC_HWTime_Since_Epoch;

   ---------------------------------------------------------------------------
   -- Context Swich ----------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address)
     renames Processor_Registers.Context_Switch;
   pragma Inline (Context_Switch);

   procedure Change_To_Context (New_Task : in System.Address)
     renames Processor_Registers.Change_To_Context;
   pragma Inline (Change_To_Context);

   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address)
     renames Processor_Registers.Change_Return_Address_Of_Preempted_Task;

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

   -- Para x86 serán:
--   function Main_Task_Stack_Top_Limit return System.Address is
   --  pragma Import (C, Top_Main_Task_Stack, "base_stack_start");
   --  Defined in 'hwi/boot/base_stack.S' for architecture x86
--   begin
--      return Top_Main_Task_Stack'Address;
--   end Main_Task_Stack_Top_Limit;

--   function Main_Task_Stack_Base return System.Address is
   --  pragma Import (C, Base_Main_Task_Stack, "base_stack_end");
   --  Defined in 'hwi/boot/base_stack.S' for architecture x86
--   begin
--      return Base_Main_Task_Stack'Address;
--   end Main_Task_Stack_Base;

   -------------------------
   -- Finish_Hardware_Use --
   -------------------------

   procedure Finish_Hardware_Use is
   begin
      null;
   end Finish_Hardware_Use;

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize is
   begin
      pragma Assert (not Initialized);

      pragma Assert (Duration'Small = 1.0/1e9);
      --  We rely that Duration is an exact count of nanoseconds.

      --  XXX Could read TSC frequency?

      Initialized := True;
   end Initialize;


end MaRTE.HAL;
