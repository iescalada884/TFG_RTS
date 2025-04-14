------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                     'U s e s _ R o u n d _ R o b i n'
--
--                               Ada Program
--
--
--  File 'uses_round_robin.adb'                                       By MAR.
--
--
--  Uses the package 'Round_Robin_Scheduling' to create 3 Ada tasks to be
--  scheduled under the Round-Robin scheduling policy.
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
with MaRTE_OS;
with Round_Robin_Scheduling;

with Execution_Load;

with Ada.Exceptions;
with Text_IO; use Text_IO;
with System;

procedure Uses_Round_Robin is

   package My_Round_Robin is
     new Round_Robin_Scheduling (Max_Round_Robin_Priority => 8,
                                 Default_Quantum => 0.3);

   --------------------
   -- type 'RR_Task' --
   --------------------
   task type RR_Task (Id : Character) is
      pragma Priority (5);
   end RR_Task;

   task body RR_Task is
   begin
      --  Changes policy to SCHED_APP
      My_Round_Robin.Become_Round_Robin_Task;
      --  Does "useful" work
      for I in 1..14 loop
         Put (" " & Id);
         Execution_Load.Eat (0.05);
      end loop;
      Put (" " & Id & " ends.");
      My_Round_Robin.Terminate_Execution;
   exception
      when Excep_Event:others =>
         Put ("Excep. in RR_Task (" & Id & "):");
         Put (Ada.Exceptions.Exception_Name (Excep_Event));
         Put (" " & Ada.Exceptions.Exception_Message (Excep_Event));
   end RR_Task;

   RR_Task_1 : RR_Task ('A');
   RR_Task_2 : RR_Task ('B');
   RR_Task_3 : RR_Task ('C');

begin
   null;
end Uses_Round_Robin;





