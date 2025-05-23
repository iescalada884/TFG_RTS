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
--  XtratuM Version.
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
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
   for ARCHITECTURE'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, ARCHITECTURE, "hal__architecture");

   X86_ARCH : constant CP.Supported_Architectures := CP.ARCH_X86;
   for X86_ARCH'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, X86_ARCH, "hal__x86_arch");

   LINUX_ARCH : constant CP.Supported_Architectures := CP.ARCH_LINUX;
   for LINUX_ARCH'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, LINUX_ARCH, "hal__linux_arch");

   LINUX_LIB_ARCH : constant CP.Supported_Architectures := CP.ARCH_LINUX_LIB;
   for LINUX_LIB_ARCH'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, LINUX_LIB_ARCH, "hal__linux_lib_arch");

   XTRATUM_ARCH : constant CP.Supported_Architectures := CP.ARCH_XTRATUM;
   for XTRATUM_ARCH'Size use MaRTE.Integer_Types.Int'Size;
   pragma Export (C, XTRATUM_ARCH, "hal__xtratum_arch");

   ----------------------------------------------------------------------------
   -- Interrupts --------------------------------------------------------------
   ----------------------------------------------------------------------------

   type HW_Interrupt is new Integer_32 range 0 .. 256;--32;
   for HW_Interrupt'Size use 32;

   type XM_Interrupt is new Integer_32 range 0 .. 288; -- 256 + 32;
   for XM_Interrupt'Size use 32;

   --------------------
   --  XtratuM IRQs  --
   --------------------

   XM_VT_EXT_FIRST : constant := 0;
   XM_VT_EXT_LAST  : constant := 31;
   XM_VT_EXT_MAX   : constant := 32;
   XAL_XMEXT_TRAP  : constant := 224;

   XM_VT_EXT_HW_TIMER       : constant := 0 + XM_VT_EXT_FIRST;
   XM_VT_EXT_EXEC_TIMER     : constant := 1 + XM_VT_EXT_FIRST;
   XM_VT_EXT_WATCHDOG_TIMER : constant := 2 + XM_VT_EXT_FIRST;
   XM_VT_EXT_SHUTDOWN       : constant := 3 + XM_VT_EXT_FIRST;
   XM_VT_EXT_SAMPLING_PORT  : constant := 4 + XM_VT_EXT_FIRST;
   XM_VT_EXT_QUEUING_PORT   : constant := 5 + XM_VT_EXT_FIRST;

   XM_VT_EXT_CYCLIC_SLOT_START : constant := 8 + XM_VT_EXT_FIRST;
   XM_VT_EXT_MEM_PROTECT       : constant := 16 + XM_VT_EXT_FIRST;
   --  Values obtained from <xm_inc/guest.h>

   function Xtratum_Interrupt (XM_Trap : HW_Interrupt) return XM_Interrupt;
   --  Ext interrupt used by XtratuM

   function Timer_Interrupt return HW_Interrupt;
   --  Interrupt used by system hardware timer

   type Trap_State is record --  XtratuM
      IRQ_Num : HW_Interrupt;
   end record;
   pragma Convention (C, Trap_State);
   type Trap_State_Ac is access Trap_State;

   type HW_Interrupt_Handler is access
     procedure (State : in Trap_State_Ac);
   pragma Convention (C, HW_Interrupt_Handler);

   procedure Default_HW_Interrupt_Handler (State : in Trap_State_Ac);
   pragma Convention (C, Default_HW_Interrupt_Handler);
   --  Called for interrupts the user has not installed a handler

   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler);
   --  Install a handler for the interrupt

   procedure Disable_Interrupts;
   pragma Inline_Always (Disable_Interrupts);
   --  Not just an import of "XM_disable_irqs" because symbol
   --  marte__hal__disable_interrupts must exists to be used from outside of
   --  the MaRTE kernel

   procedure Enable_Interrupts;
   pragma Inline_Always (Enable_Interrupts);
   --  Not just an import of "XM_enable_irqs" because symbol
   --  marte__hal__disable_interrupts must exists to be used from outside of
   --  the MaRTE kernel

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

   function Get_Stack_Pointer_Register return Unsigned_32;
   pragma Import (C, Get_Stack_Pointer_Register, "get_esp_register");
   --  Defined in 'processor_registers_c.c'
   pragma Inline (Get_Stack_Pointer_Register);

   ---------------------------------------------------------------------------
   -- Bit Operations ---------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Bit_Set (Bit_Field : in out Unsigned_32; Bit : in Unsigned_32);
   pragma Inline_Always (Bit_Set);
   --  Sets to 1 the bit in 'Bit_Field' in the position pointed by 'Bit'
   --  ('Bit' in range 0 .. 31)

   procedure Bit_Reset (Bit_Field : in out Unsigned_32; Bit : in Unsigned_32);
   pragma Inline_Always (Bit_Reset);
   --  Sets to 0 the bit in 'Bit_Field' in the position pointed by 'Bit'
   --  ('Bit' in range 0 .. 31)

   procedure Bit_Scan_Forward (Bit_Field : in Unsigned_32;
                               Bit       : out Unsigned_32);
   pragma Inline_Always (Bit_Scan_Forward);
   --  Returns in 'Bit' the position of the LEST SIGNIFICANT bit set in
   --  'Bit_Field' ('Bit' in range 0 .. 31)
   --  'Bit_Field' can't be 0!!

   procedure Bit_Scan_Reverse (Bit_Field : in Unsigned_32;
                               Bit       : out Unsigned_32);
   pragma Inline_Always (Bit_Scan_Reverse);
   --  Returns in 'Bit' the position of the MOST SIGNIFICANT bit set in
   --  'Bit_Field' ('Bit' in range 0 .. 31)
   --  'Bit_Field' can't be 0!!

   --------------------------------------------------------------------------
   -- Time and Timers -------------------------------------------------------
   --------------------------------------------------------------------------

   Last_HW_Timer : constant Integer_32 := 0;  --  XtratuM: not used

   type HW_Timers is new Integer_32 range 0 .. Last_HW_Timer;
   for HW_Timers'Size use 32;

   HW_Timer_0 : constant HW_Timers := 0;  --  XtratuM: not used

   type HWTime is new Unsigned_64;
   --  typedef xm_s64_t xmTime_t

   function HWTime_To_Duration (Th : in HWTime) return Duration;
   pragma Inline (HWTime_To_Duration);

   function Duration_To_HWTime (D : in Duration)  return HWTime;
   pragma Inline (Duration_To_HWTime);

   function Get_HWClock_Frequency return HWTime;  --  Ticks per second
   pragma Inline (Get_HWClock_Frequency);

   function CPU_Frequency return HWTime; --  Ticks per second

   function Get_HWTime return HWTime;
   pragma Inline (Get_HWTime);

   function Get_HWTime_Slow return HWTime renames Get_HWTime;
   pragma Inline (Get_HWTime_Slow);

   procedure Program_Timer (Timer           : in HW_Timers;
                            Interval        : in HWTime;
                            Next_Activation : out HWTime);
   pragma Inline (Program_Timer);

   function Safe_Longest_Timer_Interval return HWTime;
   --  The longest interval that can be used to program the timer.
   --  In XtratuM is infinite

   function Compulsory_Timer_Reprogramming return Boolean;
   --  Returns true if the timer should always be reprogrammed even if
   --  there isn't any event. This could happen, for example, when the
   --  timer interrupts are also used for the system clock.

   ---------------------
   -- Real Time Clock --
   ---------------------

   RTC_Available : constant Boolean := False;
   function RTC_HWTime_Since_Epoch return HWTime;

   ---------------------------------------------------------------------------
   -- Context Switch ---------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address);
   pragma Inline (Context_Switch);
      
   function Is_Context_Switch_Pending return Boolean is (False);
   --  In this architecture CS is never pending at hardware level.


   procedure Change_To_Context (New_Task : in System.Address);
   pragma Inline (Change_To_Context);

   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address);

   ---------------------------------------------------------------------------
   -- Main Stack Limits ------------------------------------------------------
   ---------------------------------------------------------------------------

   Main_Task_Stack_Limits_Available : constant Boolean := False;

   function Main_Task_Stack_Top_Limit return System.Address;

   function Main_Task_Stack_Base return System.Address;

   -------------------------
   -- Finish_Hardware_Use --
   -------------------------

   procedure Finish_Hardware_Use;
   --  Invoked while finishing the application to leave the hardware used
   --  by MaRTE OS in a known and stable status.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

end MaRTE.HAL;
