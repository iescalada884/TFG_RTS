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
with Ada.Real_Time;

with Generic_Table_Exceptions;
with Generic_Table; pragma Elaborate_All (Generic_Table);
with Priority_Queues; pragma Elaborate_All (Priority_Queues);
with Queues; pragma Elaborate_All (Queues);

package RTEP.Protocol.Servers is

   INEXISTENT : exception renames Generic_Table_Exceptions.Inexistent;
   NO_SPACE   : exception renames Generic_Table_Exceptions.No_Space;
   UNEXPECTED_ERROR : exception;

   ------------------
   -- Servers_Init --
   ------------------
   procedure Servers_Init;

   --------------------
   --  Create_Server --
   --------------------
   --  Creates a new sporadic server with the specified attributes.
   --  Exceptions: NO_SPACE if there is no space for the new server

   procedure Create_Server
      (Max_Allocated_Budget : in  Network_Budget;
       Server_Period        : in  Ada.Real_Time.Time_Span;
       Server_Priority      : in  RTEP.Priority;
       Id                   : out Server_Id);

   --------------------
   --  Update_Server --
   --------------------
   --  Updates an existing sporadic server with the specified attributes.
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   procedure Update_Server
      (Max_Allocated_Budget : in  Network_Budget;
       Server_Period        : in  Ada.Real_Time.Time_Span;
       Server_Priority      : in  RTEP.Priority;
       Id                   : in Server_Id);

   --------------------
   --  Delete_Server --
   --------------------
   --  Delete the server identified by Id
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   procedure Delete_Server (Id : in Server_Id);

   -------------------
   --  Set_Priority --
   -------------------
   --  Update the Priority of the server identified by Id
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   procedure Set_Priority
      (Id              : in Server_Id;
       Server_Priority : in RTEP.Priority);

   --------------------------------
   --  Set_Max_Budget_And_Period --
   --------------------------------
   --  Update the Maximum Budget and Period of the server identified by Id.
   --  The Current Budget of the server is replenished to the maximum budget.
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   procedure Set_Max_Budget_And_Period
      (Id                   : in Server_Id;
       Max_Allocated_Budget : in Network_Budget;
       Server_Period        : in Ada.Real_Time.Time_Span);

   ----------------------
   --  Get_Server_Info --
   ----------------------
   --  Get the information associated to the server identified by Id
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   procedure Get_Server_Info
      (Id                   : in Server_Id;
       Max_Allocated_Budget : out Network_Budget;
       Server_Period        : out Ada.Real_Time.Time_Span;
       Server_Priority      : out RTEP.Priority);

   ------------------------
   -- Get_Current_Budget --
   ------------------------
   --  Get the current budget of the server identified by Id
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   function Get_Current_Budget (Id : in Server_Id) return Network_Budget;

   -------------------------
   -- Get_Packets_Pending --
   -------------------------
   --  Get the packets pending to be sent of the server identified by Id
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   function Get_Packets_Pending (Id : in Server_Id) return Network_Budget;

   ------------------------------
   --  Get_Max_Priority_Server --
   ------------------------------
   --  Execute pending replenishments and get the server with the highest
   --  priority. If Are_Active_Servers is FALSE there are not active servers
   --  so the other values are not valid.

   procedure Get_Max_Priority_Server
      (Id                  : out Server_Id;
       Server_Priority     : out RTEP.Priority;
       Are_Active_Servers  : out Boolean);

   ----------------------
   --  Packet_Enqueued --
   ----------------------
   --  Tell the Server that a new Packet has been enqueued to it
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   procedure Packet_Enqueued (Id : in Server_Id);

   ------------------
   --  Packet_Sent --
   ------------------
   --  Tell the Server that one of its Packets has been dequeued and sent
   --  so its budget can be consumed and a new replenishment operation can
   --  be programmed.
   --  Exceptions: INEXISTENT if Id does not refere to a valid server

   procedure Packet_Sent
      (Id         : in Server_Id;
       Timestamp  : in Ada.Real_Time.Time);

private

   --------------------------
   --  Servers Information --
   --------------------------
   --  Information associated to of each Server
   type Server_Info is record
      Max_Allocated_Budget : Network_Budget;
      Server_Period        : Ada.Real_Time.Time_Span;
      Priority             : RTEP.Priority;
      Current_Budget       : Network_Budget;
      Packets_Pending      : Network_Budget;
   end record;
   --  Table where we store the servers information
   package Servers_Table_Pkg is new Generic_Table (Server_Id, Server_Info);
   Servers_Table : Servers_Table_Pkg.Element_Table;

   -----------------------------
   --  Servers Priority Queue --
   -----------------------------
   --  Priority queue where we insert/extract the Active servers (the ones that
   --  have Packets Pending) ordered by their active priority so we can get the
   --  active server with the highest priority
   package Servers_Priority_Queue_Pkg is new Priority_Queues
     (Size     => RTEP.Number_Of_Servers,
      Element  => Server_Id,
      Priority => RTEP.Priority,
      ">"      => RTEP.">",
      "="      => RTEP."=");
   Servers_Priority_Queue : Servers_Priority_Queue_Pkg.Queue;

   -------------------------------
   --  Replenishment Operations --
   -------------------------------
   --  A replenishment operation as defined in Sporadic Servers bibliography
   type Repl_Operation is record
      Amount  : Network_Budget;
      At_Time : Ada.Real_Time.Time;
   end record;
   --  Each server has a queue with Replenishment Operations
   package Repl_Queues_Pkg is new Queues
      (RTEP.Max_RTEP_Packet_At_A_Time, Repl_Operation);
   type Servers_Repl_Queue_Array is array (Server_Id) of Repl_Queues_Pkg.Queue;
   Servers_Repl_Queues : Servers_Repl_Queue_Array;
   --  We keep a priority queue of the first replenishment time for each server
   --  where the priority is the replenishment TIME!
   package First_Repl_Times_Pkg is new Priority_Queues
     (Size     => RTEP.Number_Of_Servers,
      Element  => Server_Id,
      Priority => Ada.Real_Time.Time,
      ">"      => Ada.Real_Time."<",
      "="      => RTEP."=");
   First_Repl_Times : First_Repl_Times_Pkg.Queue;

end RTEP.Protocol.Servers;
