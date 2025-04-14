------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'P r o c e s s o r _ R e g i s t e r s'
--
--                                  Body
--
--  File 'processor_registers.adb'               By MAR and
--                                                  Miguel Ángel Masmano Tello.
--
--  x86 registers.
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
--  ----------------------------------------------------------------------
--  The idea of running MaRTE OS as a Linux process is due to Miguel Angel
--  Masmano     Tello     (Universidad    Politecnica     de     Valencia)
--  <mimastel@doctor.upv.es>.
--  He  is  also  the  author  of   most  of  the  code  involved  in  the
--  implementation of the hardware interface for this architecture.
--
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;

with System.Machine_Code; use System.Machine_Code;

--  To use 'Address' and its operations
with System; use System;

with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package body MaRTE.HAL.Processor_Registers is

   --------------------------------------------------------------------------
   -- Context Switch related stuff ------------------------------------------
   --------------------------------------------------------------------------

   type Rec_Fpu_State is array (1 .. 27) of Unsigned_32;

   type Preempted_Task_Stack is record
      Return_Address     : System.Address;
      --  The address of label "1:" inside 'Context_Switch'

      Fpu_State          : Rec_Fpu_State;

      Ebx                : Unsigned_32;
      Ecx                : Unsigned_32;
      Edx                : Unsigned_32;
      Esi                : Unsigned_32;
      Edi                : Unsigned_32;
      Ebp                : Unsigned_32;
      Eax                : Unsigned_32;

      Old_Ebp            : System.Address;

      Context_Switch_Ret : System.Address;
      --  The address where the task will return when finish either
      --  'Context_Switch' (if 'Context_Switch' isn't inlined) or the frame
      --  from which 'Context_Switch' has been called (if 'Context_Switch'
      --  is inlined).
      Old_Task           : System.Address;
      New_Task           : System.Address;
   end record;
   type Preempted_Task_Stack_Ac is access Preempted_Task_Stack;

   function To_Preempted_Task_Stack_Ac is
     new Ada.Unchecked_Conversion (Unsigned_32, Preempted_Task_Stack_Ac);

   ---------------------------------------------
   -- Change_Return_Address_Of_Preempted_Task --
   ---------------------------------------------
   --
   --  This procedure changes the return address of a preempted task by
   --  changing the return address stored in the stack.
   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address) is
      Stack : Preempted_Task_Stack_Ac :=
        To_Preempted_Task_Stack_Ac (Top_Of_Stack);
   begin
      Stack.Return_Address := New_Ret_Address;
   end Change_Return_Address_Of_Preempted_Task;

   --------------------
   -- Context_Switch --
   --------------------
   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address) is
   begin
      --  Store registers (old)
      Asm ("pushl %%eax;" &
           "pushl %%ebp;" &
           "pushl %%edi;" &
           "pushl %%esi;" &
           "pushl %%edx;" &
           "pushl %%ecx;" &
           "pushl %%ebx",
           No_Output_Operands, No_Input_Operands, "", True);

      --  Store FPU status (old)
      Asm ("sub $108,%%esp;" &
           "fsave (%%esp)",
           No_Output_Operands, No_Input_Operands, "", True);

      Asm ("pushl $1f;" & -- Store return address (old)
           "movl %%esp, (%%eax)",  -- Store stack pointer (old)
           No_Output_Operands, Address'Asm_Input ("a", Old_Task),
           "", True);

      Asm ("movl (%%ecx), %%esp", -- load stack pointer (new)
           No_Output_Operands, Address'Asm_Input ("c", New_Task),
           "", True);

      --  Here is where the context switch is actually performed
      Asm ("ret",
           No_Output_Operands, No_Input_Operands, "", True);

      Asm ("1: frstor (%%esp);" & -- Tasks return label
           "add $108,%%esp ",
           No_Output_Operands, No_Input_Operands, "", True);

      --  Restore registers (New)
      Asm ("popl %%ebx;" &
           "popl %%ecx;" &
           "popl %%edx;" &
           "popl %%esi;" &
           "popl %%edi;" &
           "popl %%ebp;" &
           "popl %%eax",
           No_Output_Operands, No_Input_Operands, "", True);
   end Context_Switch;
   pragma Inline (Context_Switch);

   -----------------------
   -- Change_To_Context --
   -----------------------
   procedure Change_To_Context (New_Task : in System.Address) is
   begin

      Asm ("movl (%%ecx), %%esp", -- load stack pointer (new)
           No_Output_Operands, Address'Asm_Input ("c", New_Task),
           "", True);

      --  Here is where the context switch is actually performed
      Asm ("ret",
           No_Output_Operands, No_Input_Operands, "", True);

      Asm ("1: frstor (%%esp);" & -- Tasks return label
           "add $108,%%esp ",
           No_Output_Operands, No_Input_Operands, "", True);

      --  Restore registers (New)
      Asm ("popl %%ebx;" &
           "popl %%ecx;" &
           "popl %%edx;" &
           "popl %%esi;" &
           "popl %%edi;" &
           "popl %%ebp;" &
           "popl %%eax",
           No_Output_Operands, No_Input_Operands, "", True);
   end Change_To_Context;
   pragma Inline (Change_To_Context);

end MaRTE.HAL.Processor_Registers;
