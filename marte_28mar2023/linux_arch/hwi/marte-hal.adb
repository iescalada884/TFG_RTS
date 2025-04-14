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
--  File 'hardware_interface.adb'                By MAR and
--                                                  Miguel Ángel Masmano Tello.
--
--  Linux Version.
--
--  This package defines the interface between the hardware dependent
--  and hardware independent parts of the kernel.
--
--  ----------------------------------------------------------------------
--  The idea of running MaRTE OS as a Linux process is due to Miguel Angel
--  Masmano     Tello     (Universidad    Politecnica     de     Valencia)
--  <mimastel@doctor.upv.es>.
--  He  is  also  the  author  of   most  of  the  code  involved  in  the
--  implementation of the hardware interface for this architecture.
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
with System.Machine_Code; use System.Machine_Code;
with Unchecked_Conversion;

with MaRTE.HAL.Processor_Registers;
with MaRTE.HAL.Interrupt_Tables;

package body MaRTE.HAL is

   Initialized : Boolean := False;

   use type MaRTE.Integer_Types.Int;

   --------------------------------------------------------------------------
   -- Interrupts ------------------------------------------------------------
   --------------------------------------------------------------------------

   ---------------------
   -- Timer_Interrupt --
   ---------------------
   --  Interrupt used by system hardware timer
   function Timer_Interrupt return HW_Interrupt is
   begin
      return HAL.Interrupt_Tables.Linux_Signals_Get_Timer_Signal;
   end Timer_Interrupt;

   ----------------------------------
   -- Default_HW_Interrupt_Handler --
   ----------------------------------
   procedure Default_HW_Interrupt_Handler_C (State : in Trap_State_Ac);
   pragma Import (C, Default_HW_Interrupt_Handler_C,
                  "base_irq_default_handler");
   procedure Default_HW_Interrupt_Handler (State : in Trap_State_Ac) is
   begin
      Default_HW_Interrupt_Handler_C (State);
   end Default_HW_Interrupt_Handler;

   ----------------------------------
   -- Install_HW_Interrupt_Handler --
   ----------------------------------
   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler) is

      procedure Interrupts_Install_Signal_Handler
        (Signo   : HW_Interrupt;
         Handler : HW_Interrupt_Handler);
      pragma Import (C, Interrupts_Install_Signal_Handler,
                       "interrupts_install_signal_handler");
      --  defined in 'interrupts.c'
   begin
      Interrupts_Install_Signal_Handler (Int_Num, Handler);
   end Install_HW_Interrupt_Handler;

   ------------------------
   -- Disable_Interrupts --
   ------------------------
   procedure Disable_Interrupts is
      procedure Linux_Signals_Mask_Used_Signals;
      pragma Import (C, Linux_Signals_Mask_Used_Signals,
                       "linux_signals_mask_used_signals");
      --  Defined in 'boot/linux_signals.c'
   begin
      Linux_Signals_Mask_Used_Signals;
   end Disable_Interrupts;

   ------------------------
   --  Enable_Interrupts --
   ------------------------
   procedure Enable_Interrupts is
      procedure Linux_Signals_Unmask_All_Signals;
      pragma Import (C, Linux_Signals_Unmask_All_Signals,
                    "linux_signals_unmask_all_signals");
      --  Defined in 'boot/linux_signals.c'
   begin
      Linux_Signals_Unmask_All_Signals;
   end Enable_Interrupts;

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
      function Linux_Signals_Is_Timer_Signal_Masked return Int;
      pragma Import (C, Linux_Signals_Is_Timer_Signal_Masked,
                       "linux_signals_is_timer_signal_masked");
   begin
      return Linux_Signals_Is_Timer_Signal_Masked = 0;
   end Are_Interrupts_Enabled;

   ------------------------------
   -- Get_Current_HW_Interrupt --
   ------------------------------

   function Get_Current_HW_Interrupt (State : in Trap_State_Ac)
                                     return HW_Interrupt is
   begin
      return HW_Interrupt (State.ERR);
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

   procedure Save_Flags_And_Disable_Interrupts (Not_Used : out Integer) is
      procedure Linux_Signals_Save_Mask_And_Mask_Signals
        (Not_Used : out Integer);
      pragma Import (C, Linux_Signals_Save_Mask_And_Mask_Signals,
                       "linux_signals_save_mask_and_mask_signals");
   begin
      Linux_Signals_Save_Mask_And_Mask_Signals (Not_Used);
   end Save_Flags_And_Disable_Interrupts;

   function Save_Flags return Integer is
   begin
      return 0;
   end Save_Flags;
   procedure Restore_Flags (Not_Used : in Integer) is
      procedure Linux_Signals_Restore_Mask (Not_Used : in Integer);
      pragma Import (C, Linux_Signals_Restore_Mask,
                       "linux_signals_restore_mask");
   begin
      Linux_Signals_Restore_Mask (Not_Used);
   end Restore_Flags;

   --------------------------------------------------------------------------
   -- Stack Pointer Register ------------------------------------------------
   --------------------------------------------------------------------------
   function Get_Stack_Pointer_Register return Unsigned_32 is
   begin
      return Processor_Registers.Get_ESP_Register;
   end Get_Stack_Pointer_Register;
   pragma Inline (Get_Stack_Pointer_Register);

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

   function HW_Timer_Get_HWTime return HWTime;
   pragma Import (C, HW_Timer_Get_HWTime,
                    "hw_timer_get_hwtime");  --  'boot/hw_timer.c'

   ---------------------------------
   -- Safe_Longest_Timer_Interval --
   ---------------------------------
   function Safe_Longest_Timer_Interval return HWTime is
   begin
      --  Timer can be programed of an interval arbitrarily long
      return HWTime'Last;
   end Safe_Longest_Timer_Interval;

   --  'HWT_HZ' is the number of 'HWTime' units per second.
   HWT_HZ : HWTime;
   pragma Import (C, HWT_HZ, "hw_timer_hwtime_Hz"); -- 'boot/hw_timer.c'

   function To_HWT_NS is new Unchecked_Conversion (Duration, HWTime);
   function To_Duration is new Unchecked_Conversion (HWTime, Duration);
   NS_Per_S : constant HWTime := 10#1#E9;

   ------------------------
   -- HWTime_To_Duration --
   ------------------------
   function HWTime_To_Duration (Th : in HWTime) return Duration is
      S : HWTime; -- := Th;
   begin
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
      S : HWTime;  --  := To_HWT_NS (D);
   begin
      S := To_HWT_NS (D) / NS_Per_S;
      return S * HWT_HZ +
             ((To_HWT_NS (D) - S * NS_Per_S) * HWT_HZ) / NS_Per_S;
   end Duration_To_HWTime;
   pragma Inline (Duration_To_HWTime);

   -------------------
   -- Program_Timer --
   -------------------
   --
   --  Updates the absolute hardware time ('Total_Time'). Should be called
   --  with interrupts disabled.
   procedure Program_Timer (Timer           : in  HW_Timers;
                            Interval        : in  HWTime;
                            Next_Activation : out HWTime) is

      procedure HW_Timer_Program_Timer (Interval : in HWTime);
      pragma Import (C, HW_Timer_Program_Timer, "hw_timer_program_timer");

   begin
      HW_Timer_Program_Timer (Interval);
      --  Gets next activation
      Next_Activation := HW_Timer_Get_HWTime + Interval;
   end Program_Timer;

   ---------------------
   -- Get_HWTime_Slow --
   ---------------------
   --
   --
   function Get_HWTime_Slow return HWTime renames Get_HWTime;

   ----------------
   -- Get_HWTime --
   ----------------
   --
   --
   function Get_HWTime return HWTime is
   begin
      return HW_Timer_Get_HWTime;
   end Get_HWTime;

   ---------------------------
   -- Get_HWClock_Frequency --
   ---------------------------
   function Get_HWClock_Frequency return HWTime is
   begin
      return HWT_HZ;
   end Get_HWClock_Frequency;

   -------------------
   -- CPU_Frequency --
   -------------------
   function CPU_Frequency return HWTime is
   begin
      return Get_HWClock_Frequency;
   end CPU_Frequency;

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
      function Linux_Time (T : in System.Address) return Natural;
      pragma Import (C, Linux_Time, "linux_time");
      --  Defined in 'boot/linux_syscalls/lib/syscall_time.c'
   begin
      return HWTime (Linux_Time (System.Null_Address)) * Get_HWClock_Frequency;
   end RTC_HWTime_Since_Epoch;

   -------------------
   -- Context Swich --
   -------------------
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

   -------------------------
   -- Finish_Hardware_Use --
   -------------------------
   procedure Finish_Hardware_Use is
   begin
      null;
   end Finish_Hardware_Use;

   ----------------
   -- Inicialize --
   ----------------
   procedure Initialize is

      procedure HW_Timer_Init (Hwtime_Hz : in Integer);
      pragma Import (C, HW_Timer_Init,
                       "hw_timer_init");  -- Defined in 'boot/hw_timer.c'
   begin
      pragma Assert (Duration'Small = 1.0/1e9);
      --  We rely that Duration is an exact count of nanoseconds.

      --  gives value to HWT_HZ
      HW_Timer_Init (0); --  Initialize HW_Timer

      Interrupt_Tables.Inicialize;
   end Initialize;

end MaRTE.HAL;
