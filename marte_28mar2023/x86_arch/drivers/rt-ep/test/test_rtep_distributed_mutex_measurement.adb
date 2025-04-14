----------------------------------------------------------------------------
-------------------------         RT-EP             ------------------------
----------------------------------------------------------------------------
--                       Copyright (C) 2003-2005                          --
--                   Universidad de Cantabria, SPAIN                      --
--                      http://www.ctr.unican.es/                         --
--                                                                        --
-- This program is free software; you can redistribute it and/or          --
-- modify it under the terms of the GNU General Public                    --
-- License as published by the Free Software Foundation; either           --
-- version 2 of the License, or (at your option) any later version.       --
--                                                                        --
-- This program is distributed in the hope that it will be useful,        --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      --
-- General Public License for more details.                               --
--                                                                        --
-- You should have received a copy of the GNU General Public              --
-- License along with this program; if not, write to the                  --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,           --
-- Boston, MA 02111-1307, USA.                                            --
--                                                                        --
-- As a special exception, if other files instantiate generics from       --
-- this unit, or you link this unit with other files to produce an        --
-- executable, this  unit  does not  by itself cause  the resulting       --
-- executable to be covered by the GNU General Public License. This       --
-- exception does not however invalidate any other reasons why the        --
-- executable file  might be covered by the  GNU Public License.          --
----------------------------------------------------------------------------
--                                                     RT-EP/Ada95 {V1.0}
--
--         't e s t _ r t e p _ m e s s a g e _ m a s t e r  .  a d b'
--
--                                     Ada
--
--
--  File 'test_rtep_message_master.adb'                      By Sangorrin
--
--  This is just a simple test for the rtep subsystem made by Chema.
--  A MASTER sends a message ("Hello World!!") to a SLAVE (test...slave.adb)
--  You can compile them (test_rtep_message_slave.adb and
--  test_rtep_message_master.adb) with 'mgnatmake' (change then the name of
--  'mprogram' so it is not overwritten).
--  If you want to, you can test them with the QEMU x86 emulator:
--  a) SLAVE
--  qemu -hda slave.img -net nic,macaddr=00:50:56:22:22:23,model=rtl8139 -net
--    socket,mcast=230.0.0.1:1234 &
--  b) OTHER_SLAVE
--  qemu -hda other_slave.img -net nic,macaddr=00:50:56:22:22:24,model=rtl8139 -net
--    socket,mcast=230.0.0.1:1234
--  c) MASTER (must be booted the last!!)
--  qemu -hda master.img -net nic,macaddr=00:50:56:22:22:22,model=rtl8139 -net
--    socket,mcast=230.0.0.1:1234
-----------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;

with RTEP;
with RTEP.Protocol;
with RtEp.Protocol.Core;
with RTEP.Protocol.Servers;

with Var_Strings;
with Time_Measure;

procedure test_rtep_distributed_mutex_measurement is

   package RP  renames RTEP.Protocol;
   package RPS renames RTEP.Protocol.Servers;

   M : RTEP.Distributed_Mutex_Id := RTEP.Distributed_Mutex_Id'First;
   Time_Mutex_Lock_ID   : Time_Measure.Measure_ID;
   Time_Mutex_Unlock_ID : Time_Measure.Measure_ID;

   procedure Master_Job is
      Master_Prio : RTEP.Priority := 15;
   begin
      Put_Line ("MASTER");
      loop
         delay 1.0;
         Put ("LOCKING ...  ");
         Time_Measure.Set_Time_Mark (Time_Mutex_Lock_ID, Time_Measure.First);
            RP.Lock_Distributed_Mutex (M, Master_Prio);
         Time_Measure.Set_Time_Mark (Time_Mutex_Lock_ID, Time_Measure.Last);
         Put_Line ("Mutex FOR ME!!!");
         delay 1.0;
         Time_Measure.Set_Time_Mark (Time_Mutex_Unlock_ID, Time_Measure.First);
         RP.Unlock_Distributed_Mutex (M, Master_Prio);
         Time_Measure.Set_Time_Mark (Time_Mutex_Unlock_ID, Time_Measure.Last);
         Put_Line ("Mutex RELEASED!!!");
         delay 2.0;
      end loop;
   end Master_Job;

   procedure Slave_Job is
      Slave_Prio : RTEP.Priority := 3;
   begin
      Put_Line ("SLAVE");
--       loop
--          delay 0.5;
--          Put ("LOCKING ...  ");
--          RP.Lock_Distributed_Mutex (M, Slave_Prio);
--          Put_Line ("Mutex FOR ME!!!");
--          delay 3.0;
--          RP.Unlock_Distributed_Mutex (M, Slave_Prio);
--          Put_Line ("Mutex RELEASED!!!");
--          delay 1.5;
--       end loop;
   end Slave_Job;

begin
   --  Init_Main_Task;
   RTEP.Protocol.Core.Init_Comm;
   --  Init time measure IDs
   Time_Mutex_Lock_ID := Time_Measure.Init_Time_Measure
      (Var_Strings.To_Var_String ("TEST:Mutex_Lock"),
       Time_Measure.ABS_Time_Clock);
   Time_Mutex_Unlock_ID := Time_Measure.Init_Time_Measure
      (Var_Strings.To_Var_String ("TEST:Mutex_Unlock"),
       Time_Measure.ABS_Time_Clock);

   RP.Init_Distributed_Mutex (M);
   case RTEP.Protocol.Get_Station_ID is
      when 1 =>
         Master_Job;
      when 2 =>
         Slave_Job;
      when others =>
         Put_Line ("ERROR check your ring configuration");
   end case;

end test_rtep_distributed_mutex_measurement;
