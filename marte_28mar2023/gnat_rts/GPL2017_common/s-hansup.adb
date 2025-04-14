------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              S Y S T E M . H A N D L E R S _ S U P P O R T               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2005-2008, Free Software Foundation, Inc.        --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
--
--
--
--
--
--
--
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This is a MaRTE OS specific package

with System.Tasking;
with System.Task_Primitives.Operations;
with Ada.Unchecked_Conversion;

package body System.Handlers_Support is

   use type Interfaces.C.int;

   TIMED_HANDLER_TIMED_EVENT_SIZE_IN_BYTES_MaRTE : Integer;
   pragma Import (C, TIMED_HANDLER_TIMED_EVENT_SIZE_IN_BYTES_MaRTE,
                  "TIMED_HANDLER_TIMED_EVENT_SIZE_IN_BYTES");

   -------------------------------------------
   -- Interface with MaRTE "timed handlers" --
   -------------------------------------------

   TIMER_ABSTIME  : constant Interfaces.C.int;
   pragma Import (C, TIMER_ABSTIME, "marte__kernel__timers__absolute_timer");

   --  MaRTE_Timed_Handler_Destroy

   function MaRTE_Timed_Handler_Destroy (TH : access MaRTE_Timed_Handler_T)
                                         return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Destroy,
                  "marte_timed_handler_destroy");

   --  MaRTE_Timed_Handler_Enable

   function MaRTE_Timed_Handler_Enable (TH : access MaRTE_Timed_Handler_T)
                                        return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Enable, "marte_timed_handler_enable");

   ----------------------
   -- To_OSI_Thread_Id --
   ----------------------

   function To_OSI_Thread_Id
     (Ada_Task_Id : Ada.Task_Identification.Task_Id)
      return OSI.Thread_Id
   is
      function To_Task_Id is new Ada.Unchecked_Conversion
        (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);

   begin
      return
        System.Task_Primitives.Operations.Get_Thread_Id
          (To_Task_Id (Ada_Task_Id));
   end To_OSI_Thread_Id;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Event : in out Base_Timing_Event) is
      Ret : Interfaces.C.int;
   begin
      pragma Assert (not Event.Based_On_Group_Clock);

      Ret := MaRTE_Timed_Handler_Destroy (Event.Timed_Handler'Access);
      pragma Assert (Ret = 0);
   end Finalize;

   -------------------
   --  Set_Handler  --
   -------------------

   procedure Set_Handler (Event    : in out Base_Timing_Event'Class;
                          T        : Duration;
                          Relative : Boolean;
                          Clear    : Boolean) is
      Ret : Interfaces.C.int;
      Options : Interfaces.C.int := 0;
      Ts  : aliased System.OS_Interface.timespec;
   begin
      pragma Assert (TIMED_HANDLER_TIMED_EVENT_SIZE_IN_BYTES =
                       TIMED_HANDLER_TIMED_EVENT_SIZE_IN_BYTES_MaRTE);

      --  ??? how to check the PO ceiling ?

      --  Disable event

      Ret := MaRTE_Timed_Handler_Disable (Event.Timed_Handler'Access);
      pragma Assert (Ret = 0);

      if Clear then
         --  Event remains disabled

         return;
      end if;

      --  Set the MaRTE "timed event" for the desired time

      if not Relative then
         Options := TIMER_ABSTIME;
      end if;
      Ts := System.OS_Interface.To_Timespec (T);
      Ret := MaRTE_Timed_Handler_Set (Event.Timed_Handler'Access,
                                      Options,
                                      Ts'Access);
      pragma Assert (Ret = 0);

      --  re-enable event

      Ret := MaRTE_Timed_Handler_Enable (Event.Timed_Handler'Access);
      pragma Assert (Ret = 0);
   end Set_Handler;

end System.Handlers_Support;
