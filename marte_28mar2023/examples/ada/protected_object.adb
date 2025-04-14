------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                    'P r o t e c t e d _ O b j e c t'
--
--                               Ada Program
--
--
--  File 'protected_object.adb'                                        By MAR.
--
--  Basic test of the Ceiling Locking protocol in protected objects:
--
--  A high priority task (T_High) displays periodically a message and
--  suspends. A low priority task (T_Low) is active all the time and
--  periodically executes a protected operation. T_High's messages
--  stop while T_Low executes the protected operation since the PO's
--  ceiling is above T_High's priority.
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
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy (Ceiling_Locking);

with System;
with Ada.Real_Time; use Ada.Real_Time;
with Text_IO; use Text_IO;

with Execution_Load;

procedure Protected_Object is

   pragma Priority (20);

   ----------
   --  PO  --
   ----------
   protected PO is
      pragma Priority (10);
      procedure Proc1;
   end PO;

   protected body PO is

      procedure Proc1 is
      begin
         Put ("Low prio task enters protected procedure...");
         Execution_Load.Eat (2.0);
         Put_Line (" leaves");
      end Proc1;

   end PO;

   ---------------------------
   --  Task type T_Uses_PO  --
   ---------------------------
   task type T_Uses_PO (Prio : System.Priority) is
      pragma Priority (Prio);
   end T_Uses_PO;

   task body T_Uses_PO is
   begin
      loop
         Execution_Load.Eat (2.0);
         PO.Proc1;
      end loop;
   end T_Uses_PO;

   --------------------------
   --  Task type T_Period  --
   --------------------------
   task type T_Period (Prio : System.Priority) is
      pragma Priority (Prio);
   end T_Period;

   task body T_Period is
      Next_Activation : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      loop
         Next_Activation := Next_Activation + Ada.Real_Time.To_Time_Span (0.5);
         Put_Line ("High prio Task");
         delay until Next_Activation;
      end loop;
   end T_Period;

   T_Low : T_Uses_PO (Prio   => 5);
   T_High : T_Period (Prio   => 8);

begin
   null;
end Protected_Object;

