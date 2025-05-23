------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                       (MaRTE/ARM Version)                         --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  pragma Restrictions (No_Exception_Registration);
--  Disable exception name registration. This capability is not used because
--  it is only required by exception stream attributes which are not supported
--  in this run time.

--  pragma Profile (GNAT_Extended_Ravenscar);
--  This is a Ravenscar run time

--  pragma Restrictions (No_Task_At_Interrupt_Priority);
--  On Cortex-M, it is not possible to have tasks at Interrupt_Priority, as
--  the context switch is triggered by the Pend_SV interrupt, which is at
--  lowest priority.

package System is
   pragma Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable; see RM 13.7.1(15). In Ada
   --  2005, this is Pure in any case (AI-362).

   pragma No_Elaboration_Code_All;
   --  Allow the use of that restriction in units that WITH this unit

   type Name is (SYSTEM_NAME_GNAT);
   System_Name : constant Name := SYSTEM_NAME_GNAT;

   --  System-Dependent Named Numbers

   Min_Int               : constant := Long_Long_Integer'First;
   Max_Int               : constant := Long_Long_Integer'Last;

   Max_Binary_Modulus    : constant := 2 ** Long_Long_Integer'Size;
   Max_Nonbinary_Modulus : constant := 2 ** Integer'Size - 1;

   Max_Base_Digits       : constant := Long_Long_Float'Digits;
   Max_Digits            : constant := Long_Long_Float'Digits;

   Max_Mantissa          : constant := 63;
   Fine_Delta            : constant := 2.0 ** (-Max_Mantissa);

   Tick                  : constant := 0.000_001;

   --  Storage-related Declarations

   type Address is private;
   pragma Preelaborable_Initialization (Address);
   Null_Address : constant Address;

   Storage_Unit : constant := 8;
   Word_Size    : constant := 32;
   Memory_Size  : constant := 2 ** 32;

   --  Address comparison

   function "<"  (Left, Right : Address) return Boolean;
   function "<=" (Left, Right : Address) return Boolean;
   function ">"  (Left, Right : Address) return Boolean;
   function ">=" (Left, Right : Address) return Boolean;
   function "="  (Left, Right : Address) return Boolean;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "=");

   --  Other System-Dependent Declarations

   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order :=
                         Bit_Order'Val (Standard'Default_Bit_Order);
   pragma Warnings (Off, Default_Bit_Order); -- kill constant condition warning

   --  Priority-related Declarations (RM D.1)

   --  0 .. 98 corresponds to the system priority range 1 .. 99.
   --
   --  If the scheduling policy is SCHED_FIFO or SCHED_RR the runtime makes use
   --  of the entire range provided by the system.
   --
   --  If the scheduling policy is SCHED_OTHER the only valid system priority
   --  is 1 and other values are simply ignored.

   Max_Priority           : constant Positive := 97;
   Max_Interrupt_Priority : constant Positive := 98;

   subtype Any_Priority       is Integer      range  0 .. 98;
   subtype Priority           is Any_Priority range  0 .. 97;
   subtype Interrupt_Priority is Any_Priority range 98 .. 98;

   Default_Priority : constant Priority := 48;

private

   type Address is mod Memory_Size;
   Null_Address : constant Address := 0;

   --------------------------------------
   -- System Implementation Parameters --
   --------------------------------------

   --  These parameters provide information about the target that is used
   --  by the compiler. They are in the private part of System, where they
   --  can be accessed using the special circuitry in the Targparm unit
   --  whose source should be consulted for more detailed descriptions
   --  of the individual switch values.

   Atomic_Sync_Default       : constant Boolean := False;  --  MaRTE ARM
   Backend_Divide_Checks     : constant Boolean := False;
   Backend_Overflow_Checks   : constant Boolean := True;
   Command_Line_Args         : constant Boolean := False;  --  MaRTE ARM
   Configurable_Run_Time     : constant Boolean := True;  --  MaRTE ARM
   Denorm                    : constant Boolean := False;  --  MaRTE ARM
   Duration_32_Bits          : constant Boolean := False;
   Exit_Status_Supported     : constant Boolean := False; --  MaRTE ARM
   Fractional_Fixed_Ops      : constant Boolean := False;
   Frontend_Layout           : constant Boolean := False;
   Machine_Overflows         : constant Boolean := False;
   Machine_Rounds            : constant Boolean := True;
   Preallocated_Stacks       : constant Boolean := True;  --  MaRTE ARM
   Signed_Zeros              : constant Boolean := True;
   Stack_Check_Default       : constant Boolean := False;
   Stack_Check_Probes        : constant Boolean := False;  --  MaRTE ARM
   Stack_Check_Limits        : constant Boolean := False;
   Support_Aggregates        : constant Boolean := True;
--   Support_Atomic_Primitives : constant Boolean := True;  --  MaRTE ARM
   Support_Composite_Assign  : constant Boolean := True;
   Support_Composite_Compare : constant Boolean := True;
   Support_Long_Shifts       : constant Boolean := True;
   Always_Compatible_Rep     : constant Boolean := True;  --  MaRTE ARM
   Suppress_Standard_Library : constant Boolean := False;
   Use_Ada_Main_Program_Name : constant Boolean := False;
   Frontend_Exceptions       : constant Boolean := False;
   ZCX_By_Default            : constant Boolean := True;  --  MaRTE ARM

end System;
