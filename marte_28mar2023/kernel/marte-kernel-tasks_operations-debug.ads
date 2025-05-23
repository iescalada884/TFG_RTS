------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--        'K e r n e l . T a s k s _ O p e r a t i o n s . D e b u g'
--
--                                 Spec
--
--
--  File 'k-to-debug.ads'                                              By MAR.
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
with MaRTE.HAL;
with MaRTE.Integer_Types;
with MaRTE.Timespec;

package MaRTE.Kernel.Tasks_Operations.Debug is

   pragma Elaborate_Body;

   package K renames MaRTE.Kernel;

   --------------
   -- Suspend1 --
   --------------
   procedure Suspend1 (T          : in Task_Id;
                       Until_Time : in MaRTE.HAL.HWTime);

   -----------------
   -- Create_Task --
   -----------------
   procedure Create_Task1 (Id : in Integer; Attr : in Pthread_Attr_T);

   --------------------------
   -- Initialize_Main_Task --
   --------------------------
   procedure Initialize_Main_Task1 (Prio : in Task_Priority;
                                    Sched_Policy : in Scheduling_Policies);

   --------------------------
   -- Initialize_Idle_Task --
   --------------------------
   procedure Initialize_Idle_Task1 (Id : in Integer);

   -----------------
   -- Remove_Task --
   -----------------
   procedure Remove_Task1 (T : in Task_Id);

   -----------------
   -- Setdeadline --
   -----------------

   procedure Setdeadline (T         : Task_Id;
                          Deadline  : Timespec.Timespec_Ac;
                          Immediate : Integer_Types.Int);

end MaRTE.Kernel.Tasks_Operations.Debug;
