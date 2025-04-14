------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                              'K e r n e l'
--
--                                  Body
--
--
--  File 'kernel.adb'                                                  By MAR.
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
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;

with MaRTE.Kernel.Debug;
with MaRTE.Debug_Messages;
with MaRTE.Direct_IO;
with MaRTE.Spy;

package body MaRTE.Kernel is

   package DBG renames MaRTE.Kernel.Debug;

   procedure Debug_Message (Op : String);

   use type HAL.HWTime;

   Mx_Kernel_Nesting_Level : constant := 4;
   --  Maximum spected nesting level. Something strange happens if
   --  'Kernel_Nesting_Level' goes over this value.

   --------------------------
   -- Kernel Nesting Level --
   --------------------------
   Kernel_Nesting_Level : Natural := 0;
   --  Kernel nesting level count for the running task. 0 when the
   --  running task is not executing inside MaRTE OS kernel. It can
   --  have a value greater than 1 since calls to MaRTE OS can be
   --  nested in some (few) cases.
   --
   --  Used to avoid disabling interrupts twice when entering to
   --  nested critic sections or re-enabling interrupts too early when
   --  leaving internal nested calls to the kernel.
   --
   --  Initialized to 0 in 'Kernel.Scheduler'.

   function Get_Kernel_Nesting_Level return Natural is
   begin
      return Kernel_Nesting_Level;
   end Get_Kernel_Nesting_Level;

   procedure Set_Kernel_Nesting_Level (New_Value : Natural) is
   begin
      pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled));

      Kernel_Nesting_Level := New_Value;

      pragma Debug (Debug_Message ("Set_KNL="));
      pragma Debug (MaRTE.Spy.Send_Event_If_Initialized
                    ("KS", 0, Integer (Kernel_Nesting_Level)));
   end Set_Kernel_Nesting_Level;

   --------------------------
   -- Enter_Critic_Section --
   --------------------------
   procedure Enter_Critic_Section (Flags : out Integer) is
   begin
      pragma Debug
        (DBG.Assert (Kernel_Nesting_Level < Mx_Kernel_Nesting_Level));

      if Kernel_Nesting_Level = 0 then
         pragma Debug (Debug_Message ("EK&DI"));
         --  Entering to kernel
         HAL.Save_Flags_And_Disable_Interrupts (Flags);
      else
         pragma Debug (Debug_Message ("EK"));
         null;
      end if;
      Kernel_Nesting_Level := Kernel_Nesting_Level + 1;

      pragma Debug (MaRTE.Spy.Send_Event_If_Initialized
                    ("KE", 0, Integer (Kernel_Nesting_Level)));
   end Enter_Critic_Section;
   pragma Inline (Enter_Critic_Section);
   -----------------------------------------
   -- Enter_Critic_Section_From_Interrupt --
   -----------------------------------------
   --  To be called from inside a kernel interrupt handler. Doesn't
   --  touch the EFlags register, only sets to one the
   --  'Kernel_Nesting_Level' counter.
   procedure Enter_Critic_Section_From_Interrupt is
   begin
      pragma Debug (Debug_Message ("EKI"));
      pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled));
      pragma Debug (DBG.Assert (Kernel_Nesting_Level = 0));
      Kernel_Nesting_Level := 1;

      pragma Debug (MaRTE.Spy.Send_Event (HAL.Get_HWTime, "KE",
         1, Integer (Kernel_Nesting_Level)));
   end Enter_Critic_Section_From_Interrupt;
   pragma Inline (Enter_Critic_Section_From_Interrupt);

   --------------------------
   -- Leave_Critic_Section --
   --------------------------
   procedure Leave_Critic_Section (Flags : in Integer) is
   begin
      pragma Debug
        (DBG.Assert (Kernel_Nesting_Level <= Mx_Kernel_Nesting_Level));
      Kernel_Nesting_Level := Kernel_Nesting_Level - 1;

      pragma Debug (MaRTE.Spy.Send_Event_If_Initialized
                    ("KL", 0, Integer (Kernel_Nesting_Level)));

      if Kernel_Nesting_Level = 0 then
         pragma Debug (Debug_Message ("LK&EI"));
         --  Leaving kernel
         HAL.Restore_Flags (Flags);
      else
         pragma Debug (Debug_Message ("LK"));
         null;
      end if;
   end Leave_Critic_Section;
   pragma Inline (Leave_Critic_Section);

   -----------------------------------------
   -- Leave_Critic_Section_From_Interrupt --
   -----------------------------------------
   --  To be called from inside a kernel interrupt handler. Doesn't
   --  touch the EFlags register, only sets to zero the
   --  'Kernel_Nesting_Level' counter.
   procedure Leave_Critic_Section_From_Interrupt is
   begin
      pragma Debug (Debug_Message ("LKI"));
      pragma Debug (DBG.Assert (not HAL.Are_Interrupts_Enabled));
      pragma Debug (DBG.Assert (Kernel_Nesting_Level = 1));
      Kernel_Nesting_Level := 0;

      pragma Debug (MaRTE.Spy.Send_Event (HAL.Get_HWTime, "KL",
         1, Integer (Kernel_Nesting_Level)));
   end Leave_Critic_Section_From_Interrupt;
   pragma Inline (Leave_Critic_Section_From_Interrupt);

   ---------
   -- ">" --
   ---------
   function ">" (Left, Right : in CPU_Time_Timed_Events_Lists.Element_Ac)
                 return Boolean is
   begin
      return Left.CPU_Time < Right.CPU_Time;
   end ">";

   ---------
   -- ">" --
   ---------
   function ">" (Left, Right : in Tasks_Lists.Element_Ac) return Boolean is
   begin
      if not CP.Use_EDF_Scheduling_Policy'First
        and then not CP.Use_Application_Defined_Scheduling
      then
         --  Urgency is not used, priority is the only criterion to compare
         --  tasks

         return Left.Active_Prio > Right.Active_Prio;

      else
         --  Urgency is used. Task are ordered first by priority, then by
         --  urgency and preemption level

         return Left.Active_Prio > Right.Active_Prio
           or else
             (Left.Active_Prio = Right.Active_Prio
              and then
                (Left.Active_Urgency > Right.Active_Urgency
                 and then
                   (Mutexes_Lists.Is_Empty (Right.Mutexes_Owned)
                    or else
                    Left.Active_Preemption_Level
                    > Right.Active_Preemption_Level)));
      end if;
   end ">";

   ---------------------
   -- Get_Active_Prio --
   ---------------------
   function Get_Active_Prio (T : in Tasks_Lists.Element_Ac)
                             return Task_Priority is
   begin
      return T.Active_Prio;
   end Get_Active_Prio;

   -----------------
   --  CPU_TE_OK  --
   -----------------
   function CPU_TE_OK (CPU_TE_Ac : CPU_Time_Timed_Event_Ac) return Boolean is
      function To_Clock_Id is new Ada.Unchecked_Conversion (TCB_Ac, Clock_Id);
      function To_Clock_Id is new Ada.Unchecked_Conversion (Task_Set_Base_Ac,
                                                            Clock_Id);
      use type CPU_Time_Timed_Event_Ac;
   begin
      return CPU_TE_Ac /= null

      --  Base clock isn't a "standard" clock
        and then CPU_TE_Ac.Base_Clock /= MaRTE.POSIX_Constants.CLOCK_MONOTONIC
        and then CPU_TE_Ac.Base_Clock /= MaRTE.POSIX_Constants.CLOCK_REALTIME

      --  Expiration_Time /= 0 in event NOT based on group clock
        and then not (CPU_TE_Ac.Group_Expiration_Time /= 0
                      and not CPU_TE_Ac.Is_Based_On_Group_Clock)

      --  Task_Where_Queued /= clock in event NOT based on group clock
        and then not (not CPU_TE_Ac.Is_Based_On_Group_Clock
                      and then CPU_TE_Ac.Task_Where_Queued /= null
                      and then To_Clock_Id (CPU_TE_Ac.Task_Where_Queued) /=
                        CPU_TE_Ac.Base_Clock)

      --  Task_Where_Queued.Set /= clock in event based on group clock
        and then not (CPU_TE_Ac.Is_Based_On_Group_Clock
                      and then CPU_TE_Ac.Task_Where_Queued /= null
                      and then CPU_TE_Ac.Base_Clock /=
                        To_Clock_Id (CPU_TE_Ac.Task_Where_Queued.Set));

   end CPU_TE_OK;

   -------------
   -- Task_OK --
   -------------
   function Task_OK (T : Task_Id) return Boolean is
   begin
      return Tasks_Lists."/=" (T, null) and then T.Magic = ACTIVE;
   end Task_OK;

   function Task_Terminated (T : Task_Id) return Boolean is
   begin
      return Tasks_Lists."/=" (T, null) and then T.Magic = TERMINATED;
   end Task_Terminated;

   function Task_OK (T           : Task_Id;
                     Extra_Magic : Magic_Check) return Boolean is
   begin
      return Tasks_Lists."/=" (T, null) and then
        (T.Magic = ACTIVE or T.Magic = Extra_Magic);
   end Task_OK;

   function Task_OK (T : TCB) return Boolean is
   begin
      return T.Magic = ACTIVE;
   end Task_OK;

   function Task_OK (T : TCB_Ac) return Boolean is
   begin
      return T.Magic = ACTIVE;
   end Task_OK;

   ----------------------------------------------------------------------------
   -- Task Containers ">" -----------------------------------------------------
   ----------------------------------------------------------------------------
   function ">" (Left, Right : in Task_Container_Ac) return Boolean is
   begin
      return Left.T.Active_Prio > Right.T.Active_Prio;
   end ">";

   -----------
   -- Debug --
   -----------

   procedure Debug_Message (Op : String) is
   begin
      if Debug_Messages.Enter_Kernel_Messages'First then
         Direct_IO.Put ("  | ");
         Direct_IO.Put (Op);
         Direct_IO.Put ("nl:");
         Direct_IO.Put (Kernel_Nesting_Level);
      end if;
   end Debug_Message;

end MaRTE.Kernel;
