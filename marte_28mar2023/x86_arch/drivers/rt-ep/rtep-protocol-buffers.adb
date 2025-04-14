--  ------------------------------------------------------------------------
--  ---------------------           RT-EP           ------------------------
--  ------------------------------------------------------------------------
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
--  ------------------------------------------------------------------------
--                                                     RT-EP /Ada95 {V1.1}
--
--             'r t e p - p r o t o c o l - c h a n n e l s . a d b'
--
--                                     Ada
--
--
--  File 'rtep-protocol-channels.adb'                              By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  RT-EP Channel handling. These functions will be used to store info in a
--  channel
--
--
--
--
-----------------------------------------------------------------------------

with RTEP.Protocol.Packet;
with RTEP.Protocol.Servers;
with Priority_FIFO_Queues; pragma Elaborate_All (Priority_FIFO_Queues);

--  with Queues; pragma Elaborate_All (Queues);
with Queues_With_Rejection_Policy;
pragma Elaborate_All (Queues_With_Rejection_Policy);

package body RTEP.Protocol.Buffers is

   -----------------------
   -- 0) Initialization --
   -----------------------
   procedure Buffers_Init is
   begin
      for Id in Server_ID'Range loop
         Server_Tx_Queues_Init (Id);
      end loop;
   end Buffers_Init;

   -----------------------
   -- 1) CHANNEL QUEUES --
   -----------------------
   --  There is one transmission channel plus several reception channels (we
   --  have also the queues for the servers. We use a generic ADT (abstract
   --  data type) that implemments priority queues with a FIFO behaviour.
   --  Access to the queues is protected through the use of a monitor.
   subtype Index is Integer range 0 .. RTEP.Number_Of_Channels;
   Tx_Channel : constant Index := 0; --  Channel 0 is reserved for transmission

   type FIFO_Index is mod RTEP.Max_Queued_Element_Same_Priority;
   package Prio_FIFO_Queues_Pkg is new Priority_FIFO_Queues
     (FIFO_Index => FIFO_Index,
      Element  => Packet.RTEP_Packet_Ac,
      Priority => RTEP.Priority,
      ">"      => RTEP.">");

   type Channel_Queues is array (Index) of Prio_FIFO_Queues_Pkg.Prio_Queue;

   -------------------------
   -- Queues_Monitor SPEC --
   -------------------------

   protected Queues_Monitor is

      pragma Priority (RTEP.RTEP_Task_Prio);

      procedure Insert
        (RTEP_Channel : in Index;
         Packet_Ac : in Packet.RTEP_Packet_Ac;
         Prio : in Priority);

      entry Extract
        (RTEP_Channel : in Index;
         Packet_Ac : out Packet.RTEP_Packet_Ac;
         Prio : out Priority;
         Blocking : in Boolean);

      procedure Read_First
        (RTEP_Channel : in Index;
         Packet_Ac : out Packet.RTEP_Packet_Ac;
         Prio : out Priority);

      procedure Set_Rejection_Policy
         (RTEP_Channel : in Index;
          Policy       : in Prio_FIFO_Queues_Pkg.Rejection_Policy);

   private
      entry Blocking_Extract (Index)
        (RTEP_Channel : in Index;
         Packet_Ac : out Packet.RTEP_Packet_Ac;
         Prio : out Priority;
         Blocking : in Boolean);

      The_Channel_Queues : Channel_Queues;
   end Queues_Monitor;

   -------------------------
   -- Queues_Monitor BODY --
   -------------------------

   protected body Queues_Monitor is

      procedure Insert
        (RTEP_Channel : in Index;
         Packet_Ac    : in Packet.RTEP_Packet_Ac;
         Prio         : in Priority) is
      begin
         Prio_FIFO_Queues_Pkg.Enqueue
            (The_Channel_Queues (RTEP_Channel), Packet_Ac, Prio);
      end Insert;

      entry Extract
        (RTEP_Channel : in Index;
         Packet_Ac    : out Packet.RTEP_Packet_Ac;
         Prio         : out Priority;
         Blocking     : in Boolean) when True is
      begin
         if Prio_FIFO_Queues_Pkg.Empty (The_Channel_Queues (RTEP_Channel)) then
            --  If the queue is Empty we have to wait only if Blocking = True.
            if  Blocking then
               requeue Blocking_Extract (RTEP_Channel);
            else
               Packet_Ac := null;
            end if;
         else
            Prio_FIFO_Queues_Pkg.Dequeue
               (The_Channel_Queues (RTEP_Channel), Packet_Ac, Prio);
         end if;
      end Extract;

      entry Blocking_Extract (for I in Index)
        (RTEP_Channel : in Index;
         Packet_Ac    : out Packet.RTEP_Packet_Ac;
         Prio         : out Priority;
         Blocking     : in Boolean)
      when not Prio_FIFO_Queues_Pkg.Empty (The_Channel_Queues (I)) is
      begin
         if Blocking then
            Prio_FIFO_Queues_Pkg.Dequeue
               (The_Channel_Queues (RTEP_Channel), Packet_Ac, Prio);
         end if;
      end Blocking_Extract;

      procedure Read_First
        (RTEP_Channel : in Index;
         Packet_Ac    : out Packet.RTEP_Packet_Ac;
         Prio         : out Priority) is
      begin
         if Prio_FIFO_Queues_Pkg.Empty (The_Channel_Queues (RTEP_Channel)) then
            Packet_Ac := Null;
         else
            Prio_FIFO_Queues_Pkg.Read_First
               (The_Channel_Queues (RTEP_Channel), Packet_Ac, Prio);
         end if;
      end Read_First;

      procedure Set_Rejection_Policy
         (RTEP_Channel : in Index;
          Policy       : in Prio_FIFO_Queues_Pkg.Rejection_Policy)
      is
      begin
         Prio_FIFO_Queues_Pkg.Set_Rejection_Policy
            (The_Channel_Queues (RTEP_Channel), Policy);
      end Set_Rejection_Policy;

   end Queues_Monitor;

   ------------------
   -- 2) Rx QUEUES --
   ------------------
   --  Rx_Queues_Insert_Packet
   procedure Rx_Queues_Insert_Packet
     (RTEP_Channel   : in Channel;
      Packet_Ac : in Packet.RTEP_Packet_Ac;
      Prio           : in Priority) is
   begin
      Queues_Monitor.Insert
         (Index (RTEP_Channel), Packet_Ac, Prio);
   exception
      when Prio_FIFO_Queues_Pkg.Priority_Storage_Full |
           Prio_FIFO_Queues_Pkg.Full_Queue =>
         raise Full_Of_Packets;
   end Rx_Queues_Insert_Packet;

   --  Rx_Queues_Extract_Packet
   procedure Rx_Queues_Extract_Packet
     (RTEP_Channel   : in Channel;
      Packet_Ac : out Packet.RTEP_Packet_Ac;
      Prio           : out Priority;
      Blocking       : in Boolean := True) is
   begin
      Queues_Monitor.Extract
         (Index (RTEP_Channel), Packet_Ac, Prio, Blocking);
   exception
      when Prio_FIFO_Queues_Pkg.Empty_Queue =>
         raise Empty;
   end Rx_Queues_Extract_Packet;

   --  Rx_Queues_Read_Packet
   procedure Rx_Queues_Read_Packet
     (RTEP_Channel   : in Channel;
      Packet_Ac : out Packet.RTEP_Packet_Ac;
      Prio           : out Priority) is
   begin
      Queues_Monitor.Read_First
         (Index (RTEP_Channel), Packet_Ac, Prio);
   end Rx_Queues_Read_Packet;

   --  Rx_Queues_Set_Rejection_Policy
   procedure Rx_Queues_Set_Rejection_Policy
           (RTEP_Channel   : in Channel;
            Policy         : in Rejection_Policy)
   is
   begin
      case Policy is
         when DISCARD_OLDEST =>
            Queues_Monitor.Set_Rejection_Policy
               (Index (RTEP_Channel), Prio_FIFO_Queues_Pkg.DISCARD_OLDEST);
         when DISCARD_NEWEST =>
            Queues_Monitor.Set_Rejection_Policy
               (Index (RTEP_Channel), Prio_FIFO_Queues_Pkg.DISCARD_NEWEST);
      end case;
   end Rx_Queues_Set_Rejection_Policy;

   --------------------------------
   -- 3) Fixed Priority Tx QUEUE --
   --------------------------------
   --  FP_Tx_Queue_Insert_Packet
   procedure FP_Tx_Queue_Insert_Packet
     (Packet_Ac : in Packet.RTEP_Packet_Ac;
      Prio      : in Priority) is
   begin
      Queues_Monitor.Insert
         (Tx_Channel, Packet_Ac, Prio);
   exception
      when Prio_FIFO_Queues_Pkg.Priority_Storage_Full |
           Prio_FIFO_Queues_Pkg.Full_Queue =>
         raise Full_Of_Packets;
   end FP_Tx_Queue_Insert_Packet;

   --  FP_Tx_Queue_Extract_Packet
   procedure FP_Tx_Queue_Extract_Packet
     (Packet_Ac : out Packet.RTEP_Packet_Ac;
      Prio      : out Priority;
      Blocking  : in Boolean := True) is
   begin
      Queues_Monitor.Extract
         (Tx_Channel, Packet_Ac, Prio, Blocking);
   exception
      when Prio_FIFO_Queues_Pkg.Empty_Queue =>
         raise Empty;
   end FP_Tx_Queue_Extract_Packet;

   --  FP_Tx_Queue_Read_Packet
   procedure FP_Tx_Queue_Read_Packet
     (Packet_Ac : out Packet.RTEP_Packet_Ac;
      Prio      : out Priority) is
   begin
      Queues_Monitor.Read_First
         (Tx_Channel, Packet_Ac, Prio);
   end FP_Tx_Queue_Read_Packet;

   --  FP_Tx_Queue_Set_Rejection_Policy
   procedure FP_Tx_Queue_Set_Rejection_Policy
           (Policy : in Rejection_Policy) is
   begin
      case Policy is
         when DISCARD_OLDEST =>
            Queues_Monitor.Set_Rejection_Policy
               (Tx_Channel, Prio_FIFO_Queues_Pkg.DISCARD_OLDEST);
         when DISCARD_NEWEST =>
            Queues_Monitor.Set_Rejection_Policy
               (Tx_Channel, Prio_FIFO_Queues_Pkg.DISCARD_NEWEST);
      end case;
   end FP_Tx_Queue_Set_Rejection_Policy;

   -------------------------
   -- 4) Server Tx QUEUES --
   -------------------------
   --  There is a normal FIFO Queue for each Server.
   --  We use a monitor to protect the access to these Queues.

   package Server_Tx_Queues_Pkg is new Queues_With_Rejection_Policy
     (Max_Queued_Element_Same_Priority, Packet.RTEP_Packet_Ac);

   type Server_Tx_Queues_Type is
      array (Server_ID) of Server_Tx_Queues_Pkg.Queue;

   -----------------------------------
   -- Server_Tx_Queues_Monitor SPEC --
   -----------------------------------

   protected Server_Tx_Queues_Monitor is
      pragma Priority (RTEP.RTEP_Task_Prio);

      procedure Insert
        (Id        : in Server_Id;
         Packet_Ac : in Packet.RTEP_Packet_Ac);

      entry Extract
        (Id        : in Server_Id;
         Packet_Ac : out Packet.RTEP_Packet_Ac;
         Blocking  : in Boolean := True);

      procedure Set_Rejection_Policy
         (Id     : in Server_Id;
          Policy : in Server_Tx_Queues_Pkg.Rejection_Policy);

      procedure Init
         (Id       : in Server_Id);

   private
      entry Blocking_Extract (Server_Id)
        (Id        : in Server_Id;
         Packet_Ac : out Packet.RTEP_Packet_Ac;
         Blocking  : in Boolean := True);

      Server_Tx_Queues : Server_Tx_Queues_Type;
   end Server_Tx_Queues_Monitor;

   -----------------------------------
   -- Server_Tx_Queues_Monitor BODY --
   -----------------------------------

   protected body Server_Tx_Queues_Monitor is
      procedure Insert
        (Id        : in Server_ID;
         Packet_Ac : in Packet.RTEP_Packet_Ac) is
      begin
         Server_Tx_Queues_Pkg.Enqueue (Packet_Ac, Server_Tx_Queues (Id));
      end Insert;

      entry Extract
        (Id        : in Server_Id;
         Packet_Ac : out Packet.RTEP_Packet_Ac;
         Blocking  : in Boolean := True) when True is
      begin
         if Server_Tx_Queues_Pkg.Empty (Server_Tx_Queues(Id)) then
            --  If the queue is Empty we have to wait only if Blocking = True.
            if Blocking then
               requeue Blocking_Extract (Id);
            else
               Packet_Ac := null;
            end if;
         else
            Server_Tx_Queues_Pkg.Dequeue (Packet_Ac, Server_Tx_Queues(Id));
         end if;
      end Extract;

      entry Blocking_Extract (for I in Server_Id)
        (Id          : in Server_Id;
         Packet_Ac   : out Packet.RTEP_Packet_Ac;
         Blocking    : in Boolean := True)
         when not Server_Tx_Queues_Pkg.Empty(Server_Tx_Queues(I)) is
      begin
         if Blocking then
            Server_Tx_Queues_Pkg.Dequeue (Packet_Ac, Server_Tx_Queues(Id));
         end if;
      end Blocking_Extract;

      procedure Set_Rejection_Policy
         (Id     : in Server_Id;
          Policy : in Server_Tx_Queues_Pkg.Rejection_Policy)
      is
      begin
         Server_Tx_Queues_Pkg.Set_Rejection_Policy
            (Policy, Server_Tx_Queues(Id));
      end Set_Rejection_Policy;

      procedure Init
         (Id : in Server_Id) is
      begin
         Server_Tx_Queues_Pkg.Init (Server_Tx_Queues(Id));
      end Init;
   end Server_Tx_Queues_Monitor;

   --------------------------
   -- Server_Insert_Packet --
   --------------------------

   procedure Server_Tx_Queues_Insert_Packet
     (Packet_Ac : in Packet.RTEP_Packet_Ac;
      Id        : in Server_Id) is
   begin
      Server_Tx_Queues_Monitor.Insert (Id, Packet_Ac);
      RTEP.Protocol.Servers.Packet_Enqueued (Id);
   exception
      when Server_Tx_Queues_Pkg.Full_Queue =>
         raise Full_Of_Packets;
   end Server_Tx_Queues_Insert_Packet;

   ---------------------------
   -- Server_Extract_Packet --
   ---------------------------

   procedure Server_Tx_Queues_Extract_Packet
     (Packet_Ac : out Packet.RTEP_Packet_Ac;
      Id        : in Server_Id;
      Blocking  : in Boolean := True) is
   begin
      Server_Tx_Queues_Monitor.Extract
         (Id, Packet_Ac, Blocking);
      RTEP.Protocol.Servers.Packet_Sent
         (Id, Packet.Get_Timestamp (Packet_Ac.all));
   exception
      when  Server_Tx_Queues_Pkg.Empty_Queue =>
         raise Empty;
   end Server_Tx_Queues_Extract_Packet;

   ---------------------------
   -- Server_Tx_Queues_Init --
   ---------------------------

   procedure Server_Tx_Queues_Init (Id  : in Server_Id) is
   begin
      Server_Tx_Queues_Monitor.Init (Id);
   end Server_Tx_Queues_Init;

   ---------------------------
   --  Set_Rejection_Policy --
   ---------------------------

   procedure Server_Tx_Queues_Set_Rejection_Policy
      (Id     : in Server_Id;
       Policy : in Rejection_Policy)
   is
   begin
      case Policy is
         when DISCARD_OLDEST =>
            Server_Tx_Queues_Monitor.Set_Rejection_Policy
               (Id, Server_Tx_Queues_Pkg.DISCARD_OLDEST);
         when DISCARD_NEWEST =>
            Server_Tx_Queues_Monitor.Set_Rejection_Policy
               (Id, Server_Tx_Queues_Pkg.DISCARD_NEWEST);
      end case;
   end Server_Tx_Queues_Set_Rejection_Policy;

end RTEP.Protocol.Buffers;
