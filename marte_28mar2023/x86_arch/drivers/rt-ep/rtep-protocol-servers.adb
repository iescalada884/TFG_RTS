----------------------------------------------------------------------------
--------------------             RT-EP              ------------------------
----------------------------------------------------------------------------
--                         Copyright (C) 2003-2005
--                     Universidad de Cantabria, SPAIN
--                        http://www.ctr.unican.es/
--
--    This program is free software; you can redistribute it and/or
--    modify it under the terms of the GNU General Public
--    License as published by the Free Software Foundation; either
--    version 2 of the License, or (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--    General Public License for more details.
--
--    You should have received a copy of the GNU General Public
--    License along with this program; if not, write to the
--    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
--    Boston, MA 02111-1307, USA.
--
--    As a special exception, if other files instantiate generics from
--    this unit, or you link this unit with other files to produce an
--    executable, this  unit  does not  by itself cause  the resulting
--    executable to be covered by the GNU General Public License. This
--    exception does not however invalidate any other reasons why the
--    executable file  might be covered by the  GNU Public License.
----------------------------------------------------------------------------
--  This package contains the implementation of the sporadic server algorithm
--  which is used to control the delivery of rt-ep packets when they have
--  been sent using a server.
--
--  Each sporadic server has an associated queue to store packets. This
--  queue is in the RTEP.Protocol.Buffers module.
--
--  In this module we store the information (Budget, Period, ..) of each of
--  the servers and we execute the sporadic server algorithm. There is a
--  replenishment queue for each server and a global priority queue to sort
--  the active servers (the ones with packets pending to be sent) according
--  to their priorities. The sporadic server algorithm consists on controling
--  the consumed budget and replenishments and the priority of the server
--  by using the following rules:
--
--  1.- When a packet is enqueued by the user, a timestamp is set
--  (see Send_Info) and the function Packet_Enqueued is called. In that
--  function, if the server was inactive it gets active and is put in
--  the global priority queue. The priority will background if the server
--  didn't have budget. TODO: put the timestamp in the last time?
--
--  2.- When a packet is sent (because it wins the arbitration phase) the
--  function Packet_Sent is called
--  (see RTEP.Protocol.Buffers.Server_Tx_Queues_Extract_Packet).
--  Then, the budget is decreased in one packet (except if it was sent in
--  background) and a replenishment operation of 1 packet is programmed
--  for the moment: (timestamp when it was enqueued + Period of the server)
--  TODO: if it was enqueued when capacity=0 the replenishment time should
--  be time when C>0 + Period?
--
--  For the replenishment operations we don't use any timer. When the
--  main task wants to know the server with highest priority, we execute
--  all the pending replenishment operations that were supposed to happen
--  before that time (see Get_Max_Priority_Server).
--
--  Sporadic Servers:
--       Aperiodic Task Scheduling for Real-Time Systems (1990)
--       Brinkley Sprunt
--       http://citeseer.ist.psu.edu/306166.html
--
----------------------------------------------------------------------------
with MaRTE.Kernel.Mutexes; pragma Elaborate_All (MaRTE.Kernel.Mutexes);
with RTEP.Protocol.Buffers;
with RTEP_Debug_Functions; pragma Elaborate_All (RTEP_Debug_Functions);
use RTEP_Debug_Functions;

package body RTEP.Protocol.Servers is

   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;
   package Km renames MaRTE.Kernel.Mutexes;

   Background_Priority: constant RTEP.Priority := RTEP.Priority'First;

   ------------------
   -- Servers_Init --
   ------------------
   --  This package was a protected object and now it is a monitor using a mutex
   Servers_Mutex_Ref : Km.Mutex_Descriptor;

   procedure Servers_Init is
      Attr      : aliased Km.Attributes;
   begin
      -- Mutex initialization
      Servers_Mutex_Ref := new Km.Mutex;
      Check_NZ (Km.Pthread_Mutexattr_Init (Attr'access));
      Check_NZ (Km.Pthread_Mutexattr_Setprotocol
         (Attr'access, Km.Highest_Ceiling_Priority));
      Check_NZ (Km.Pthread_Mutexattr_Setprioceiling
         (Attr'access, RTEP.RTEP_Task_Prio));
      Check_NZ (Km.Pthread_Mutex_Init (Servers_Mutex_Ref, Attr'access));
      Check_NZ (Km.Pthread_Mutexattr_Destroy (Attr'access));
   exception
      when others =>
         ERROR ("Exception in Init_Servers_Monitor");
   end Servers_Init;

   --------------------
   --  Create_Server --
   --------------------

   procedure Create_Server
      (Max_Allocated_Budget : in  Network_Budget;
       Server_Period        : in  Ada.Real_Time.Time_Span;
       Server_Priority      : in  RTEP.Priority;
       Id                   : out Server_Id)
   is
      Info : Server_Info;
   begin
      DEBUG ("Create_Server, Prio"&RTEP.Priority'Image (Server_Priority),
             Enable_DEBUG_Servers'First);
      Info.Max_Allocated_Budget := Max_Allocated_Budget;
      Info.Server_Period := Server_Period;
      Info.Priority := Server_Priority;
      Info.Current_Budget := Max_Allocated_Budget;
      Info.Packets_Pending := 0;
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Servers_Table_Pkg.Add (Info, Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
      DEBUG ("Create_Server, Server_Id"&Integer'Image (Integer(Id)),
         Enable_DEBUG_Servers'First);
   exception
      when others =>
         ERROR ("Exception in Create_Server");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   end Create_Server;

   --------------------
   --  Update_Server --
   --------------------

   procedure Update_Server
      (Max_Allocated_Budget : in  Network_Budget;
       Server_Period        : in  Ada.Real_Time.Time_Span;
       Server_Priority      : in  RTEP.Priority;
       Id                   : in Server_Id)
   is
      Info : Server_Info;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Info := Servers_Table_Pkg.Item (Id, Servers_Table);

      Info.Max_Allocated_Budget := Max_Allocated_Budget;
      Info.Server_Period := Server_Period;
      Info.Priority := Server_Priority;

      Servers_Table_Pkg.Update (Info, Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   exception
      when others =>
         ERROR ("Exception in Update_Server");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   end Update_Server;

   -------------------
   -- Delete_Server --
   -------------------

   procedure Delete_Server (Id : in Server_Id) is
      Prio  : RTEP.Priority;
      Found : Boolean;
      Rep_Time : Ada.Real_Time.Time;
      Tmp_Id : Server_Id := Id;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      --  delete server from prio queue of servers (if it is there)
      Servers_Priority_Queue_Pkg.Dequeue_Middle
         (Tmp_Id, Prio, Servers_Priority_Queue, Found);
      --  Init the associated buffer
      Buffers.Server_Tx_Queues_Init (Tmp_Id);
      --  delete first replenishment
      First_Repl_Times_Pkg.Dequeue_Middle
         (Tmp_Id, Rep_Time, First_Repl_Times, Found);
      --  delete replenishment queue of server
      Repl_Queues_Pkg.Init (Servers_Repl_Queues (Tmp_Id));
      --  delete server info
      Servers_Table_Pkg.Delete (Tmp_Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   exception
      when others =>
         ERROR ("Exception in Delete_Server");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   end Delete_Server;

   -------------------
   --  Set_Priority --
   -------------------

   procedure Set_Priority
      (Id              : in Server_Id;
       Server_Priority : in RTEP.Priority)
   is
      Info : Server_Info;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Info := Servers_Table_Pkg.Item (Id, Servers_Table);
      Info.Priority := Server_Priority;
      Servers_Table_Pkg.Update (Info, Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   exception
      when others =>
         ERROR ("Exception in Set_Priority");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   end Set_Priority;

   --------------------------------
   --  Set_Max_Budget_And_Period --
   --------------------------------

   procedure Set_Max_Budget_And_Period
      (Id                   : in Server_Id;
       Max_Allocated_Budget : in Network_Budget;
       Server_Period        : in Ada.Real_Time.Time_Span)
   is
      Info : Server_Info;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Info := Servers_Table_Pkg.Item (Id, Servers_Table);
      Info.Max_Allocated_Budget := Max_Allocated_Budget;
      Info.Server_Period := Server_Period;
      Info.Current_Budget := Max_Allocated_Budget;
      Servers_Table_Pkg.Update (Info, Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   exception
      when others =>
         ERROR ("Exception in Set_Max_Budget_And_Period");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   end Set_Max_Budget_And_Period;

   ----------------------
   --  Get_Server_Info --
   ----------------------

   procedure Get_Server_Info
      (Id                   : in Server_Id;
       Max_Allocated_Budget : out Network_Budget;
       Server_Period        : out Ada.Real_Time.Time_Span;
       Server_Priority      : out RTEP.Priority)
   is
      Info : Server_Info;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Info := Servers_Table_Pkg.Item (Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
      Max_Allocated_Budget := Info.Max_Allocated_Budget;
      Server_Period        := Info.Server_Period;
      Server_Priority      := Info.Priority;
   exception
      when Servers_Table_Pkg.Inexistent =>
         ERROR ("Exception in Get_Server_Info");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
         raise;
   end Get_Server_Info;

   ------------------------
   -- Get_Current_Budget --
   ------------------------

   function Get_Current_Budget (Id : in Server_Id) return Network_Budget
   is
      Info : Server_Info;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Info := Servers_Table_Pkg.Item (Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
      return Info.Current_Budget;
   exception
      when others =>
         ERROR ("Exception in Get_Current_Budget");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
         return Info.Current_Budget;
   end Get_Current_Budget;

   -------------------------
   -- Get_Packets_Pending --
   -------------------------

   function Get_Packets_Pending (Id : in Server_Id) return Network_Budget
   is
      Info : Server_Info;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Info := Servers_Table_Pkg.Item (Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
      return Info.Packets_Pending;
   exception
      when others =>
         ERROR ("Exception in Get_Packets_Pending");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
         return Info.Packets_Pending;
   end Get_Packets_Pending;

   ------------------------------
   --  Get_Max_Priority_Server --
   ------------------------------
   --  Execute pending replenishments and get the server with the highest
   --  priority. If Are_Active_Servers is FALSE there are not active servers
   --  so the other values are not valid.

   procedure Get_Max_Priority_Server
      (Id                  : out Server_Id;
       Server_Priority     : out RTEP.Priority;
       Are_Active_Servers  : out Boolean)
   is
      Tmp_Id     : Server_Id;
      Info       : Server_Info;
      Op         : Repl_Operation;
      Now        : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      At_Time    : Ada.Real_Time.Time;
      Old_Budget : Network_Budget;
   begin
      DEBUG ("Entering Get_Max_Priority_Server", Enable_DEBUG_Servers'First);
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      -- Iterate for all server earliest replenishment times
      while not First_Repl_Times_Pkg.Empty (First_Repl_Times) loop
         First_Repl_Times_Pkg.Read_First(Tmp_Id, At_Time, First_Repl_Times);
         exit when At_Time > Now;
         First_Repl_Times_Pkg.Dequeue(Tmp_Id, At_Time, First_Repl_Times);
         Info := Servers_Table_Pkg.Item (Tmp_Id, Servers_Table);
         Old_Budget := Info.Current_Budget;
         -- Iterate for all the server's replenishment operations
         while not Repl_Queues_Pkg.Empty (Servers_Repl_Queues (Tmp_Id)) loop
            Repl_Queues_Pkg.Read_First (Op, Servers_Repl_Queues (Tmp_Id));
            if Op.At_Time > Now then
               -- Enqueue the server back in the queue of earliest
               -- replenishment times
               First_Repl_Times_Pkg.Enqueue(Tmp_Id,Op.At_Time,First_Repl_Times);
               exit;
            else
               Repl_Queues_Pkg.Dequeue (Op, Servers_Repl_Queues(Tmp_Id));
               Info.Current_Budget := Info.Current_Budget + Op.Amount;
            end if;
         end loop;
         --  Sanity check
         if Info.Current_Budget > Info.Max_Allocated_Budget then
            ERROR ("Get_Max_Priority_Server, current budget > max_budget");
            raise Constraint_Error; --  Something went wrong!!
         end if;
         --  Update the server's Info (Current_Budget)
         Servers_Table_Pkg.Update (Info, Tmp_Id, Servers_Table);
         -- if budget was zero and is now larger than zero,
         -- and if the server is active, raise priority
         if (Old_Budget = 0) and then
            (Info.Current_Budget > 0) and then
            (Info.Packets_Pending > 0) then
            Servers_Priority_Queue_Pkg.Set_Prio
               (Tmp_Id, Info.Priority, Servers_Priority_Queue);
         end if;
      end loop;

      --Obtain highest priority server
      if Servers_Priority_Queue_Pkg.Empty (Servers_Priority_Queue) then
         DEBUG (" --> NO active servers", Enable_DEBUG_Servers'First);
         Are_Active_Servers := False;
      else
         DEBUG (" --> YES active servers", Enable_DEBUG_Servers'First);
         Are_Active_Servers := True;
         Servers_Priority_Queue_Pkg.Read_First
            (Id, Server_Priority, Servers_Priority_Queue);
      end if;
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   exception
      when others =>
         ERROR ("Exception in Get_Max_Priority_Server");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   end Get_Max_Priority_Server;

   ----------------------
   --  Packet_Enqueued --
   ----------------------
   --  Tell the Server that a new Packet has been enqueued to it

   procedure Packet_Enqueued (Id : in Server_Id)
   is
      Prio : RTEP.Priority;
      Info : Server_Info;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Info := Servers_Table_Pkg.Item (Id, Servers_Table);
      -- if there were no packets, add the server to the priority queue
      if Info.Packets_Pending = 0 then
         if Info.Current_Budget = 0 then
            Prio := Background_Priority;
         else
            Prio := Info.Priority;
         end if;
         Servers_Priority_Queue_Pkg.Enqueue (Id, Prio, Servers_Priority_Queue);
      end if;
      -- Packet accounting
      Info.Packets_Pending := Info.Packets_Pending + 1;
      --  Update the info of the server
      Servers_Table_Pkg.Update (Info, Id, Servers_Table);
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   exception
      when others =>
         ERROR ("Exception in Packet_Enqueued");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   end Packet_Enqueued;

   ------------------
   --  Packet_Sent --
   ------------------
   --  Tell the Server that one of its Packets has been dequeued and sent
   --  so its budget can be consumed and a new replenishment operation can
   --  be programmed.

   procedure Packet_Sent
      (Id         : in Server_Id;
       Timestamp  : in Ada.Real_Time.Time)
   is
      Repl_Time : Ada.Real_Time.Time;
      Op : Repl_Operation;
      Op_In_Queue : Repl_Operation;
      S_Id : Server_Id := Id;
      Prio : RTEP.Priority;
      Info : Server_Info;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Servers_Mutex_Ref));
      Info := Servers_Table_Pkg.Item (Id, Servers_Table);
      Repl_Time := Timestamp + Info.Server_Period;
      --  Op := (Amount  => Network_Budget'Min (Amount, Info.Current_Budget),
      --         At_Time => Repl_Time);
      Op := (Amount  => Network_Budget'Min (1, Info.Current_Budget),
             At_Time => Repl_Time);
      --  Packet accounting
      --  if Amount > Info.Packets_Pending then
      --     raise Constraint_Error; -- attempt to consume too many packets
      --  end if;
      --  Packet_Count(Id):=Packet_Count(Id)-Amount;
      Info.Packets_Pending := Info.Packets_Pending - 1;

      -- if no packets left, delete the server from the priority queue
      if Info.Packets_Pending = 0 then
         Servers_Priority_Queue_Pkg.Dequeue_Middle
            (S_Id, Prio, Servers_Priority_Queue);
      end if;

      -- Program a replenishment
      if Repl_Queues_Pkg.Empty (Servers_Repl_Queues (Id)) then
         Repl_Queues_Pkg.Enqueue(Op, Servers_Repl_Queues(Id));
         First_Repl_Times_Pkg.Enqueue (Id, Repl_Time, First_Repl_Times);
      else
         Repl_Queues_Pkg.Read_Last (Op_In_Queue, Servers_Repl_Queues(Id));
         --  If we already have a replenishment operation for the same time,
         --  we just add the new amount to it instead of programming a new one
         if Op_In_Queue.At_Time = Repl_Time then
            Op_In_Queue.Amount := Op_In_Queue.Amount + Op.Amount;
            Repl_Queues_Pkg.Update_Last(Op_In_Queue, Servers_Repl_Queues(Id));
         else
            --  If the Replenishment Queue of the server is full we update its
            --  last element with the new time and the sum of the amounts
            if Repl_Queues_Pkg.Full(Servers_Repl_Queues(Id)) then
               Op_In_Queue.Amount := Op_In_Queue.Amount + Op.Amount;
               Op_In_Queue.At_Time := Repl_Time;
               Repl_Queues_Pkg.Update_Last
                  (Op_In_Queue, Servers_Repl_Queues(Id));
            else
               Repl_Queues_Pkg.Enqueue (Op, Servers_Repl_Queues(Id));
            end if;
         end if;
      end if;
      --  Budget accounting
      Info.Current_Budget := Info.Current_Budget - Op.Amount;
      --  Update the info of the server
      Servers_Table_Pkg.Update (Info, Id, Servers_Table);
      -- If budget is zero and server is active, lower its priority
      if (Info.Current_Budget = 0) and then (Info.Packets_Pending > 0) then
         Servers_Priority_Queue_Pkg.Set_Prio
            (Id, Background_Priority, Servers_Priority_Queue);
      end if;
      Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   exception
      when others =>
         ERROR ("Exception in Packet_Sent");
         Check_NZ (Km.Pthread_Mutex_Unlock (Servers_Mutex_Ref));
   end Packet_Sent;

end RTEP.Protocol.Servers;
