--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                            'T e s t _ A T C'
--
--                               Ada Program
--
--
--  File 'test_atc.adb'                                               By MAR.
--
--  Test the ATC.
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

pragma Task_Dispatching_Policy (FIFO_Within_Priorities);

with Calendar; use Calendar;

with MaRTE_OS;
with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Execution_Load;
with Reports;


--------------------------
-- Procedure 'Test_ATC' --
--------------------------
procedure Test_ATC is

   pragma Priority (0);

   package Sec_IO is new Fixed_IO (Calendar.Day_Duration); use Sec_IO;


   First_Test, Second_Test : Boolean := False;
   Initial_Time : Time;

   Delay_Time : constant Duration := 0.75;
   Eat_Time   : constant Duration := 1.0;
   Epsilon    : constant Duration := 0.2;
   End_Delay    : Duration;
   End_Eat      : Duration;
   Start_Select : Duration;

   ------------
   -- Task_2 --
   ------------
   --
   --  Disturbing Task
   task Task_2  is
      pragma Priority (17);
   end Task_2;

   task body Task_2 is
   begin
      Reports.Init;
      for I in 1 .. 4 loop
         --  Put("T2"); New_Line;
         delay Delay_Time/3;
      end loop;
   end Task_2;

   ------------
   -- Task_1 --
   ------------
   task Task_1  is
      pragma Priority (16);
   end Task_1;

   task body Task_1 is
   begin
      Reports.Init;
      Initial_Time := Clock;
      for I in 1 .. 2 loop

         New_Line;
         Put ("- Test "); Put (I, 2);
         Put ("  delay:"); Put (Duration (I) * Delay_Time);
         Put ("  Abortable code: " & Duration'Image (Eat_Time)); New_Line;
         Start_Select := Clock - Initial_Time;
         --  Put ("  Entering select at:");
         --  Put (Duration'Image (Clock - Initial_Time)); New_Line;

         select

            delay Duration (I) * Delay_Time;
            End_Delay := Clock - Initial_Time;
            Put ("  Abortable code started at:" &
                 Duration'Image (Start_Select));
            New_Line;
            if I = 1 then
               First_Test := True;
               Put ("  Delay finished at:" & Duration'Image (End_Delay));
               New_Line;
               Reports.Assert (abs ((End_Delay - Start_Select) -
                                    (Duration (I) * Delay_Time)) < Epsilon);
            else
               First_Test := False;
               Put ("  ERROR Delay finished at:" & Duration'Image (End_Delay));
               Reports.Assert (False);
            end if;

         then abort
            Execution_Load.Eat (Eat_Time);
            End_Eat := Clock - Initial_Time;
            Put ("  Abortable code started at:" &
                 Duration'Image (Start_Select));
            New_Line;
            if I = 2 then
               Second_Test := True;
               Put ("  Abortable code finishing at:" &
                    Duration'Image (End_Eat));
               New_Line;
               Reports.Assert (abs ((End_Eat - Start_Select) -
                                    Eat_Time) < Epsilon);
            else
               Second_Test := False;
               Put ("  ERROR Abortable code finishing at:" &
                    Duration'Image (End_Eat));
               Reports.Assert (False);
            end if;

         end select;

         Put ("  Leaving select at:");
         Put (Duration'Image (Clock - Initial_Time)); New_Line;

      end loop;
   end Task_1;

begin
   Reports.Init;
   New_Line;
   Put ("Tests Result: ");
   if First_Test and Second_Test then
      Reports.Test_OK;
   else
      Put ("ATC didn't work properly.");
   end if;
   New_Line (2);

end Test_ATC;
