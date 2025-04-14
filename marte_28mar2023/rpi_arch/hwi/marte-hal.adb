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
--  RPi Version.
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

with MaRTE.HAL.Interrupts_Registers;

with System.Machine_Code; use System.Machine_Code;
with Interfaces;

package body MaRTE.HAL is

   Initialized : Boolean := False;


   -- If No_Exception_Handlers is defined
--   procedure Last_Chance_Handler (Source_Location : System.Address;
--                                  Line : Integer) is
--   begin
--      null;
--   end Last_Chance_Handler;


   ------------------
   -- Current time --
   ------------------

   Current_Time : HWTime := 0;
   pragma Volatile (Current_Time);
   --  Count of timer ticks since the reset of the computer.

   --------------------------------------------------------------------------
   -- Interrupts ------------------------------------------------------------
   --------------------------------------------------------------------------

   ---------------------
   -- Timer_Interrupt --
   ---------------------

   function Timer_Interrupt return HW_Interrupt is
   begin
      return TIMER_1;
   end Timer_Interrupt;

   ----------------------------------
   -- Default_HW_Interrupt_Handler --
   ----------------------------------

   procedure Default_HW_Interrupt_Handler (State : in Trap_State_Ac) is
      procedure C_HDMI_Console_PutString (Str : in String;Long : in Integer);
      pragma Import (C, C_HDMI_Console_PutString, "hdmi_console_putString");
   begin
        C_HDMI_Console_PutString ("Unexpected_Interrupt",20);
   end Default_HW_Interrupt_Handler;

   ----------------------------------
   -- Install_HW_Interrupt_Handler --
   ----------------------------------

   procedure Install_HW_Interrupt_Handler
     (Int_Num  : in HW_Interrupt;
      Handler  : in HW_Interrupt_Handler) is
      procedure C_Put_Handler (Int_Type : in Integer;
                               Int_Num  : in HW_Interrupt;
                               Handler  : in HW_Interrupt_Handler);
      pragma Import (C, C_Put_Handler, "put_handler");
      procedure C_HDMI_Console_PutString (Str : in String;Long : in Integer);
      pragma Import (C, C_HDMI_Console_PutString, "hdmi_console_putString");
   begin
    case Integer(Int_Num) is
       when 0..31 => C_Put_Handler(0,Int_Num,Handler);
       when 32..63 => C_Put_Handler(1,Int_Num mod 32,Handler);
       when 64..71 => C_Put_Handler(2,Int_Num mod 32,Handler);
       when others => null;
    end case;
   end Install_HW_Interrupt_Handler;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsid i",Volatile => True);
   end Disable_Interrupts;


   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsie i",Volatile => True);
   end Enable_Interrupts;


   ----------------------------------------------------
   -- Hardware_Interrupt_Controller_Enable_Interrupt --
   ----------------------------------------------------

   procedure Hardware_Interrupt_Controller_Enable_Interrupt
     (Int : in HW_Interrupt) is
      use MaRTE.HAL.Interrupts_Registers;
      Reg : Interfaces.Unsigned_32 := 1;
   begin
      case Integer(Int) is
         when 0..31 => VIC_BASE(Enable1) :=
              Register_32(Interfaces.Shift_Left(Reg, Natural(Int)));
         when 32..63 => VIC_BASE(Enable2) :=
              Register_32(Interfaces.Shift_Left(Reg, Natural(Int Mod 32)));
         when 64..71 => VIC_BASE(Enable_Basic) :=
              Register_32(Interfaces.Shift_Left(Reg, Natural(Int Mod 32)));
         when others => null;
      end case;
   end Hardware_Interrupt_Controller_Enable_Interrupt;

   -----------------------------------------------------
   -- Hardware_Interrupt_Controller_Disable_Interrupt --
   -----------------------------------------------------

   procedure Hardware_Interrupt_Controller_Disable_Interrupt
     (Int : in HW_Interrupt) is
      use MaRTE.HAL.Interrupts_Registers;
      Reg : Interfaces.Unsigned_32 := 1;
   begin
      case Integer(Int) is
         when 0..31 => VIC_BASE(Disable1) :=
              Register_32(Interfaces.Shift_Left(Reg, Natural(Int)));
         when 32..63 => VIC_BASE(Disable2) :=
              Register_32(Interfaces.Shift_Left(Reg, Natural(Int Mod 32)));
         when 64..71 => VIC_BASE(Disable_Basic) :=
              Register_32(Interfaces.Shift_Left(Reg, Natural(Int Mod 32)));
         when others => null;
      end case;
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
      function C_irqStatus return Integer;
      pragma Import (C, C_irqStatus, "irqStatus");
      Aux : Integer;
   begin
      Aux := C_irqStatus;
      if Aux = 0 then
         return True;
      else
         return False;
      end if;
   end Are_Interrupts_Enabled;


   ------------------------------
   -- Get_Current_HW_Interrupt --
   ------------------------------

   function Get_Current_HW_Interrupt (State : in Trap_State_Ac)
                                     return HW_Interrupt is
   begin
      return State.IRQ_Num;
      --return 1;
   end Get_Current_HW_Interrupt;

   ----------------------------
   -- End_Of_Timer_Interrupt --
   ----------------------------

   procedure End_Of_Timer_Interrupt is
   begin
      if Timer_Interrupt = TIMER_1 then
         MaRTE.HAL.Interrupts_Registers.ST_Base(0):=16#0000_0002#;
      else
         MaRTE.HAL.Interrupts_Registers.ST_Base(0):=16#0000_0008#; -- TIMER_3
      end if;

   end End_Of_Timer_Interrupt;

   ---------------------------------------------------------------------------
   -- Save and Restore the Flags Register ------------------------------------
   ---------------------------------------------------------------------------

   procedure Save_Flags_And_Disable_Interrupts (EFlags : out Integer) is
   begin
      EFlags := Save_Flags;
      Disable_Interrupts;
   end Save_Flags_And_Disable_Interrupts;

   function Save_Flags return Integer is
      Aux : Boolean;
   begin
      Aux := Are_Interrupts_Enabled;
      if Aux = True then
         return 1;
      else
         return 0;
      end if;
   end Save_Flags;


   procedure Restore_Flags (EFlags : in Integer) is
   begin
      if EFlags = 0 then
         Disable_Interrupts;
      else
         Enable_Interrupts;
      end if;
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
      Aux : HWTime;
      Flag : Integer;
   begin
      Save_Flags_And_Disable_Interrupts(Flag);
      Aux := Get_HWTime;
      Restore_Flags(Flag);
      return Aux;
   end Get_HWTime_Slow;

   ----------------
   -- Get_HWTime --
   ----------------

   function Get_HWTime return HWTime is
      Aux : HWTime;
      use MaRTE.HAL.Interrupts_Registers;
   begin
      Aux := HWTime(Interfaces.Shift_Left(Interfaces.Unsigned_64(ST_BASE(2)),32));
      Aux := Aux + HWTime(ST_BASE(1));
      return Aux;
   end Get_HWTime;


   ---------------------------------
   -- Safe_Longest_Timer_Interval --
   ---------------------------------

   function Safe_Longest_Timer_Interval return HWTime is
   begin
      return (2**32)-100;
   end Safe_Longest_Timer_Interval;

   function To_HWT_NS is new Unchecked_Conversion (Duration, HWTime);
   function To_Duration is new Unchecked_Conversion (HWTime, Duration);
   NS_Per_S : constant HWTime := 10#1#E9;
   HWT_HZ : constant HWTime := 1_000_000;


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
      return 700_000_000;
   end CPU_Frequency;

   -------------------
   -- Program_Timer --
   -------------------

   procedure Program_Timer (Timer           : in HW_Timers;
                            Interval        : in HWTime;
                            Next_Activation : out HWTime) is
      use MaRTE.HAL.Interrupts_Registers;
      type Two_32_T is record
         Low : Register_32;
         High : Register_32;
      end record;
      for Two_32_T'Size use 64;
      Reference : Two_32_T;
      Next : Two_32_T;
      Inter : Two_32_T;
      for Inter'Address use Interval'Address;
      for Next'Address use Next_Activation'Address;
      --Now : HWTime;
   begin
      Reference.Low := ST_BASE(Counter_Lower);
      Reference.High := ST_BASE(Counter_Higher);
      --Being sure it's a coherence data
      while ST_BASE(1) < Reference.Low loop
         Reference.Low := ST_BASE(Counter_Lower);
         Reference.High := ST_BASE(Counter_Higher);
      end loop;
      Next.Low := Reference.Low + Inter.Low;
      Next.High := Reference.High + Inter.High; --Inter.High allways 0
      if Next.Low < Inter.Low then
         Next.High := Next.High+1;
      end if;
      if Timer_Interrupt = TIMER_1 then
         ST_BASE(Compare1) := Next.Low;
      else
         ST_BASE(Compare3) := Next.Low;
      end if;
   end Program_Timer;

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
      return 0;
   end RTC_HWTime_Since_Epoch;

--     Running_Thread : System.Address;
--     pragma Volatile (Running_Thread);
--     pragma Export (Asm, Running_Thread, "running_thread");
--
--     First_Thread : System.Address;
--     pragma Volatile (First_Thread);
--     pragma Export (Asm, First_Thread, "first_thread");

   ---------------------------------------------------------------------------
   -- Context Swich ----------------------------------------------------------
   ---------------------------------------------------------------------------
   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address) is

      procedure C_Context_Switch(Old_Task : in System.Address;
                                 New_Task : in System.Address);
      pragma Import (C, C_Context_Switch, "context_switch");
      --        function C_returnSP return MaRTE.Integer_Types.Unsigned_32;
--        pragma Import (C, C_returnSP, "returnSP");
--        use System.Machine_Code;
--        Aux : System.Address;
      --Aux2 : System.Address;
   begin
--        Running_Thread := Old_Task;
--        First_Thread := New_Task;
      C_Context_Switch(Old_Task,New_Task);

--        -- Store registers, return address and status register(old)
--        Asm ("push {r0-r12};" &
--               "push {lr};" &
--               "mrs r2,cpsr;" &
--               "push {r2}",
--             No_Output_Operands, No_Input_Operands, "", True);
--        -- Store stack pointer (old)
--        Asm ("mov %0,sp",
--             System.Address'Asm_Output("=r",Aux), No_Input_Operands,
--             "", True);
--        --Old_Task := Aux;
--
--        -- Load stack pointer (new)
--  --        Asm ("move sp,%0",
--  --             No_Output_Operands, System.Address'Asm_Input("r",Aux2),
--  --             "",True);
--        -- Here is where the context switch is actually performed
--
--        -- Load registers and return address (new)
--        Asm ("pop {lr};" &
--             "pop {r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12}",
--             No_Output_Operands, No_Input_Operands, "", True);
   end Context_Switch;
   pragma Inline (Context_Switch);

   procedure Init_Task_Context (Context_Ac : access Arch_Specific_Context_T;
                                Task_Wrapper_Address : System.Address;
                                Task_Body_Address : System.Address;
                                Stack_Pointer   : Unsigned_32) is
   begin
      pragma Assert (False);
      null;
      --  XXX Not used just now, but stack initialization should be performed by
      --  this procedure in all the architectures to avoid dependency with the
      --  hardware in MaRTE.Kernel.Tasks_Operations.Initialize_TCBs.
   end Init_Task_Context;

   -----------------------
   -- Change_To_Context --
   -----------------------

   procedure Change_To_Context (New_Task : in System.Address) is
      procedure C_Change_To_Context(New_Task : in System.Address);
      pragma Import (C, C_Change_To_Context, "change_to_context");
   begin
      c_change_to_context(New_Task);
         -- Load stack pointer (new)
--        Asm ("move sp,%0",
--             No_Output_Operands, System.Address'Asm_Input("r",Aux2),
--             "",True);
      -- Here is where the context switch is actually performed

      -- Load registers and return address (new)
--        Asm ("pop {r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12};" &
--             "pop {lr};",
--             No_Output_Operands, No_Input_Operands, "", True);
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
      C_Change_LR(Top_Of_Stack, New_Ret_Address);
   end Change_Return_Address_Of_Preempted_Task;


   -------------------------------
   -- Main_Task_Stack_Top_Limit --
   -------------------------------

   function Main_Task_Stack_Top_Limit return System.Address is
      Ret : System.Address;
      pragma Import (C, Ret, "_sp_init");
   begin
      return Ret'Address;
   end Main_Task_Stack_Top_Limit;

   function Main_Task_Stack_Base return System.Address is
      Ret : System.Address;
      pragma Import (C, Ret, "_sp_end");
   begin
      return Ret'Address;
   end Main_Task_Stack_Base;

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
      MaRTE.HAL.Interrupts_Registers.ST_Base(0):=16#0000_0002#; --  TIMER_1
      MaRTE.HAL.Interrupts_Registers.ST_Base(0):=16#0000_0008#; --  TIMER_3
      Initialized := True;
   end Initialize;

end MaRTE.HAL;
