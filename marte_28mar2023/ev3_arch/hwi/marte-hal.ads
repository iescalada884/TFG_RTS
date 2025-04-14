------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                   'H a r d w a r e _ I n t e r f a c e'
--
--                                  Spec
--
--
--  File 'hardware_interface.ads'                                   By MAR.
--
--
--  This package is the border between the hardware dependent and hardware
--  independent parts of the kernel.
--
--  STM32F Version.
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
with Interfaces; use Interfaces;
with MaRTE.Integer_Types;
with Interfaces.C;
with MaRTE.Configuration_Parameters; -- to use MaRTE_Architecture

package MaRTE.HAL is

   pragma Preelaborate;

   package CP renames Configuration_Parameters;

   use type CP.Supported_Architectures;

   ------------------------
   -- MaRTE Architecture --
   ------------------------

   --  These constants are exported to be used from outside MaRTE kernel

   ARCHITECTURE : constant CP.Supported_Architectures :=
     CP.MaRTE_Architecture'First;
   for ARCHITECTURE'Size use Interfaces.C.Int'Size;
   pragma Export (C, ARCHITECTURE, "hal__architecture");

   X86_ARCH : constant CP.Supported_Architectures := CP.ARCH_X86;
   for X86_ARCH'Size use Interfaces.C.Int'Size;
   pragma Export (C, X86_ARCH, "hal__x86_arch");

   LINUX_ARCH : constant CP.Supported_Architectures := CP.ARCH_LINUX;
   for LINUX_ARCH'Size use Interfaces.C.Int'Size;
   pragma Export (C, LINUX_ARCH, "hal__linux_arch");

   LINUX_LIB_ARCH : constant CP.Supported_Architectures := CP.ARCH_LINUX_LIB;
   for LINUX_LIB_ARCH'Size use Interfaces.C.Int'Size;
   pragma Export (C, LINUX_LIB_ARCH, "hal__linux_lib_arch");

   XTRATUM_ARCH : constant CP.Supported_Architectures := CP.ARCH_XTRATUM;
   for XTRATUM_ARCH'Size use Interfaces.C.Int'Size;
   pragma Export (C, XTRATUM_ARCH, "hal__xtratum_arch");

   RPI_ARCH : constant CP.Supported_Architectures := CP.ARCH_RPI;
   for RPI_ARCH'Size use Interfaces.C.Int'Size;
   pragma Export (C, RPI_ARCH, "hal__rpi_arch");
   
   ARCH_GNAT_BB_ARM : constant CP.Supported_Architectures := 
     CP.ARCH_GNAT_BB_ARM;
   for ARCH_GNAT_BB_ARM'Size use Interfaces.C.Int'Size;
   pragma Export (C, ARCH_GNAT_BB_ARM, "hal__gnat_bb_arm_arch");
   
   ARCH_STM32F : constant CP.Supported_Architectures := 
     CP.ARCH_STM32F;
   for ARCH_STM32F'Size use Interfaces.C.Int'Size;
   pragma Export (C, ARCH_STM32F, "hal__stm32f_arch");
   
   ARCH_EV3 : constant CP.Supported_Architectures := 
     CP.ARCH_EV3;
   for ARCH_EV3'Size use Interfaces.C.Int'Size;
   pragma Export (C, ARCH_EV3, "hal__ev3_arch");

   ----------------------------------------------------------------------------
   -- Interrupts --------------------------------------------------------------
   ----------------------------------------------------------------------------
   type HW_Interrupt is range 0 .. 32;  -- XXX
   
   PendSV_Vector : constant HW_Interrupt := 14;  -- XXX
   Sys_Tick_Vector : constant HW_Interrupt := 15;  -- XXX

   --  Interrupt used by system hardware timer
   function Timer_Interrupt return HW_Interrupt;

   type Trap_State is record --  PC
      IRQ_Num : HW_Interrupt;
   end record;
   pragma Convention (C, Trap_State);
   type Trap_State_Ac is access Trap_State;

   type HW_Interrupt_Handler is access
     procedure (State : in Trap_State_Ac);
   pragma Convention (C, HW_Interrupt_Handler);

   procedure Default_HW_Interrupt_Handler (State : in Trap_State_Ac);
--   pragma Convention (C, Default_HW_Interrupt_Handler); -- Necesario para ARM?
   --  Called for interrupts the user has not installed a handler

   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler);
   --  Install a handler for the interrupt

   procedure Disable_Interrupts;
   pragma Export (C, Disable_Interrupts, "marte__hal__disable_interrupts");
   pragma Inline (Disable_Interrupts);

   procedure Enable_Interrupts;
   pragma Export (C, Enable_Interrupts, "marte__hal__enable_interrupts");
   pragma Inline (Enable_Interrupts);

   procedure Hardware_Interrupt_Controller_Enable_Interrupt
     (Int : in HW_Interrupt);

   procedure Hardware_Interrupt_Controller_Disable_Interrupt
     (Int : in HW_Interrupt);

   procedure Enable_Hardware_Interrupt_Controller;
   pragma Inline (Enable_Hardware_Interrupt_Controller);
   --  Enable the hardware interrupt controller to generate interrupts for
   --  the Processor

   function Are_Interrupts_Enabled return Boolean;
   pragma Inline (Are_Interrupts_Enabled);

   function Get_Current_HW_Interrupt (State : in Trap_State_Ac)
                                    return HW_Interrupt;
   pragma Inline_Always (Get_Current_HW_Interrupt);
   --  To be used inside interrupt handlers in order to know what interrupt
   --  are handling

   procedure End_Of_Timer_Interrupt;
   --  Re-enable the hardware timer. To be called to finish a timer interrupt
   --  when the final part of the interrupt handler in not going to be executed
   --  because a long jump in the user's interrupt handler.

   ---------------------------------------------------------------------------
   -- Save and Restore the Flags Register ------------------------------------
   ---------------------------------------------------------------------------

   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer);
   pragma Inline_Always (Save_Flags_And_Disable_Interrupts);

   function Save_Flags return Integer;
   pragma Inline_Always (Save_Flags);

   procedure Restore_Flags (EFlags : in Integer);
   --  Exported to C  with name "restore_flags"
   pragma Inline_Always (Restore_Flags);

   ----------------------------------------------------------------------------
   -- Stack Pointer Register --------------------------------------------------
   ----------------------------------------------------------------------------

   function Get_Stack_Pointer_Register return Integer_Types.Unsigned_32;
   pragma Import (C, Get_Stack_Pointer_Register, "returnSP");
   --  Defined in 'context_switch.s'
   pragma Inline (Get_Stack_Pointer_Register);

   ---------------------------------------------------------------------------
   -- Bit Operations ---------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Bit_Set (Bit_Field : in out Integer_Types.Unsigned_32;
                      Bit : in Integer_Types.Unsigned_32);
   pragma Inline_Always (Bit_Set);
   --  Sets to 1 the bit in 'Bit_Field' in the position pointed by 'Bit'
   --  ('Bit' in range 0 .. 31)

   procedure Bit_Reset (Bit_Field : in out Integer_Types.Unsigned_32;
                        Bit : in Integer_Types.Unsigned_32);
   pragma Inline_Always (Bit_Reset);
   --  Sets to 0 the bit in 'Bit_Field' in the position pointed by 'Bit'
   --  ('Bit' in range 0 .. 31)

   procedure Bit_Scan_Forward (Bit_Field : in Integer_Types.Unsigned_32;
                               Bit       : out Integer_Types.Unsigned_32);
   pragma Import(C,Bit_Scan_Forward,"bit_scan_forward");
   pragma Inline_Always (Bit_Scan_Forward);
   --  Returns in 'Bit' the position of the LEST SIGNIFICANT bit set in
   --  'Bit_Field' ('Bit' in range 0 .. 31)
   --  'Bit_Field' can't be 0!!

   procedure Bit_Scan_Reverse (Bit_Field : in Integer_Types.Unsigned_32;
                               Bit       : out Integer_Types.Unsigned_32);
   pragma Import(C,Bit_Scan_Reverse,"bit_scan_reverse");
   --  pragma Inline_Always (Bit_Scan_Reverse);
   --  Returns in 'Bit' the position of the MOST SIGNIFICANT bit set in
   --  'Bit_Field' ('Bit' in range 0 .. 31)
   --  'Bit_Field' can't be 0!!

   --------------------------------------------------------------------------
   -- Time and Timers -------------------------------------------------------
   --------------------------------------------------------------------------

   Last_HW_Timer : constant Integer_32 := 0; -- PC

   type HW_Timers is new Integer_32 range 0 .. Last_HW_Timer;
   for HW_Timers'Size use 32;

   HW_Timer_0 : constant HW_Timers := 0;

   type HWTime is new Unsigned_64;

   function Get_HWTime_Slow return HWTime;
   --  Disable and enable interrupts when using the PIC for timers and time
   --  measure purposes. Equivalent to Get_HWTime when using TSC.
   pragma Inline (Get_HWTime_Slow);

   function Get_HWTime return HWTime;
   --  Assumes interrupts disabled when using the PIC for timers and time
   --  measure purposes.
   pragma Inline (Get_HWTime);

   function Safe_Longest_Timer_Interval return HWTime;
   --  The longest interval that can be used to program the timer.

   function HWTime_To_Duration (Th : in HWTime) return Duration;
   pragma Inline (HWTime_To_Duration);

   function Duration_To_HWTime (D : in Duration)  return HWTime;
   pragma Inline (Duration_To_HWTime);

   function Get_HWClock_Frequency return HWTime;  --  Ticks per second
   pragma Inline (Get_HWClock_Frequency);

   function CPU_Frequency return HWTime; --  Ticks per second

   procedure Program_Timer (Timer           : in HW_Timers;
                            Interval        : in HWTime;
                            Next_Activation : out HWTime);
   pragma Inline (Program_Timer);

   Compulsory_Timer_Reprogramming : constant Boolean := False;
   --  In this architecture we use a ticker timer (TODO: use an alarm timer).

   ---------------------
   -- Real Time Clock --
   ---------------------
   RTC_Available : constant Boolean := False;
   function RTC_HWTime_Since_Epoch return HWTime;

   ---------------------------------------------------------------------------
   -- Context Switch ---------------------------------------------------------
   ---------------------------------------------------------------------------

   subtype Context_Switch_In_Interrupt_Handler is Boolean range True .. True;
   --  In this architecture, context switch is always performed in an 
   --  interrupt handler (PendSV)
   
   type Arch_Specific_Context_T is private;
   --  Based on System.BB.CPU_Primitives.Context_Buffer

   type Context_Id is range 0 .. 10;  -- XXX
   --  Type used for accessing to the different elements in the context buffer
   
   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address);
   pragma Inline (Context_Switch);
   --  For this architecture Old_Task and New_Task point to the
   --  Arch_Specific_Context field of the TCB.
   
   function Is_Context_Switch_Pending return Boolean;
   --  Whether the context switch handler (PendSV) is pending or not.
   --  In other architectures, this function always return false since the
   --  CS is never pending at hardware level.

   procedure Change_To_Context (New_Task : in System.Address);
   pragma Inline (Change_To_Context);

   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address);

   procedure Init_Task_Context (Context_Ac : access Arch_Specific_Context_T;
                                Task_Wrapper_Address : System.Address;
                                Task_Body_Address : System.Address;
                                Stack_Pointer   : Integer_Types.Unsigned_32);

   ---------------------------------------------------------------------------
   -- Main Stack Limits ------------------------------------------------------
   ---------------------------------------------------------------------------

   Main_Task_Stack_Limits_Available : constant Boolean := True;

   function Main_Task_Stack_Top_Limit return System.Address;

   function Main_Task_Stack_Base return System.Address;

   -------------------------
   -- Finish_Hardware_Use --
   -------------------------

   procedure Finish_Hardware_Use;
   --  Invoked while finishing the application to leave the hardware used
   --  by MaRTE OS in a known and stable status. Called from 'our_exit' in
   --  'base_console_init.c'.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

private
   Context_Buffer_Size : constant :=
     20 * System.Word_Size;  -- XXX
   --  Size calculated taken into account that the components are 32-bit, and
   --  that we want then aligned on 64-bit boundaries.

   type Arch_Specific_Context_T is array (Context_Id) of System.Address;
   for Arch_Specific_Context_T'Size use Context_Buffer_Size;
   for Arch_Specific_Context_T'Alignment use 8;
   --  Contains 9 registers R4 .. R11, PSP and an extra reserved world.
   --  They are all the registers that the thread needs to save
   --  within its thread descriptor. Using double word boundaries allows us
   --  to use double word loads and stores safely in the context switch.

end MaRTE.HAL;
