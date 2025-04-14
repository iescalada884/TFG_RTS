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
--            'r t e p - p r o t o c o l - c h a n n e l s . a d s'
--
--                                     Ada
--
--
--  File 'rtep-protocol-channels.ads'                              By Chema.
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

private package RTEP.Protocol.Buffers is

   -------------------------------------
   --  INDEX
   --   0) Initialization
   --   1) Exceptions
   --   2) Rx QUEUES
   --   3) General Tx QUEUE
   --   4) Server Tx QUEUES
   -------------------------------------

   -----------------------
   -- 0) Initialization --
   -----------------------
   procedure Buffers_Init;

   -------------------
   -- 1) Exceptions --
   -------------------
   Full_Of_Packets : exception; --  Queue is full (with policy DISCARD_NEWEST)
   Empty           : exception; --  Queue is empty

   ------------------
   -- 2) Rx QUEUES --
   ------------------
   --  Insert a packet in a receiving channel queue
   --  Exceptions: Channel_Full_Of_Packets
   procedure Rx_Queues_Insert_Packet
     (RTEP_Channel : in Channel;
      Packet_Ac    : in Packet.RTEP_Packet_Ac;
      Prio         : in Priority);

   --  Extract a packet from a receiving channel queue
   --  The boolean Blocking will determine if the Extract Operation is
   --  Blocking (wait till receive a packet) or not.
   --  Exceptions: Empty_Channel
   procedure Rx_Queues_Extract_Packet
     (RTEP_Channel : in Channel;
      Packet_Ac    : out Packet.RTEP_Packet_Ac;
      Prio         : out Priority;
      Blocking     : in Boolean := True);

   --  Read a packet from a receiving channel without extracting it. If no
   --  Packet is found a null pointer is returned throught Packet_Ac
   procedure Rx_Queues_Read_Packet
     (RTEP_Channel : in Channel;
      Packet_Ac    : out Packet.RTEP_Packet_Ac;
      Prio         : out Priority);

   --  Set the rejection policy for a reception channel
   procedure Rx_Queues_Set_Rejection_Policy
           (RTEP_Channel   : in Channel;
            Policy         : in Rejection_Policy);

   pragma Export (C, Rx_Queues_Set_Rejection_Policy,
                     "rtep_rx_queues_set_rejection_policy");

   --------------------------------
   -- 3) Fixed Priority Tx QUEUE --
   --------------------------------
   --  Insert a packet in the general transmission queue
   procedure FP_Tx_Queue_Insert_Packet
     (Packet_Ac : in Packet.RTEP_Packet_Ac;
      Prio      : in Priority);

   --  Extract a packet from the general transmission queue
   procedure FP_Tx_Queue_Extract_Packet
     (Packet_Ac : out Packet.RTEP_Packet_Ac;
      Prio      : out Priority;
      Blocking  : in Boolean := True);

   --  Read a Packet from the general transmission queue
   procedure FP_Tx_Queue_Read_Packet
     (Packet_Ac : out Packet.RTEP_Packet_Ac;
      Prio      : out Priority);

   --  Set the rejection policy for the FP transmission channel
   procedure FP_Tx_Queue_Set_Rejection_Policy
     (Policy    : in Rejection_Policy);

   pragma Export (C, FP_Tx_Queue_Set_Rejection_Policy,
                     "rtep_fp_tx_queue_set_rejection_policy");

   -------------------------
   -- 4) Server Tx QUEUES --
   -------------------------
   --  Init the tx queue of a server
   procedure Server_Tx_Queues_Init
     (Id  : in Server_Id);

   --  Insert a packet in the transmission queue of a specific server
   procedure Server_Tx_Queues_Insert_Packet
     (Packet_Ac : in Packet.RTEP_Packet_Ac;
      Id        : in Server_Id);

   --  Extract a packet from the transmission queue of a specific server
   procedure Server_Tx_Queues_Extract_Packet
     (Packet_Ac : out Packet.RTEP_Packet_Ac;
      Id        : in Server_Id;
      Blocking  : in Boolean := True);

   --  Set Rejection_Policy of the Queue associated to a server Tx Queue
   procedure Server_Tx_Queues_Set_Rejection_Policy
      (Id     : in Server_Id;
       Policy : in Rejection_Policy);

   pragma Export (C, Server_Tx_Queues_Set_Rejection_Policy,
                     "rtep_server_tx_queues_set_rejection_policy");

end RTEP.Protocol.Buffers;
