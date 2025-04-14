------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                            'A T C _ T e s t'
--
--                               Ada Program
--
--
--  File 'atc_test.adb'                                               By MAR.
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

with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Execution_Load;


--------------------------
-- Procedure 'ATC_Test' --
--------------------------
procedure ATC_Test is

   pragma Priority (0);

   package Sec_IO is new Fixed_IO (Calendar.Day_Duration); use Sec_IO;


   First_Test, Second_Test : Boolean := False;
   Initial_Time : Time;

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
      for I in 1 .. 4 loop
         --  Put("T2"); New_Line;
         delay 2.0;
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
      Initial_Time := Clock;
      for I in 1 .. 2 loop

         New_Line;
         Put ("- Test "); Put (I, 2);
         Put ("  delay:"); Put (Duration (I) * 4.0);
         Put ("  Abortable code: 6.0"); New_Line;
         Put ("  Entering select at:");
         Put (Integer (Clock - Initial_Time)); New_Line;

         select

            delay Duration (I) * 4.0;
            if I = 1 then
               First_Test := True;
               Put ("  Delay finished at:");
               Put (Integer (Clock - Initial_Time)); New_Line;
            else
               First_Test := False;
               Put ("  ERROR Delay finished at:");
               Put (Integer (Clock - Initial_Time)); New_Line;
            end if;

         then abort
            Put ("  Starting abortable code at:");
            Put (Integer (Clock - Initial_Time)); New_Line;
            Execution_Load.Eat (6.0);
            if I = 2 then
               Second_Test := True;
               Put ("  Abortable code finishing at:");
               Put (Integer (Clock - Initial_Time)); New_Line;
            else
               Second_Test := False;
               Put ("  ERROR Abortable code finishing at:");
               Put (Integer (Clock - Initial_Time)); New_Line;
            end if;

         end select;

         Put ("  Leaving select at:");
         Put (Integer (Clock - Initial_Time)); New_Line;

      end loop;
   end Task_1;

begin

   New_Line;
   Put ("Tests Result: ");
   if First_Test and Second_Test then
      Put ("ATC OK.");
   else
      Put ("ATC didn't work properly.");
   end if;
   New_Line (2);

end ATC_Test;
