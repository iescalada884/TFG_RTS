--  --------------------------------------------------------------------------
--  -----------------------           RT-EP           ------------------------
--  --------------------------------------------------------------------------
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
--                             'r t e p . a d s'
--
--                                    Ada
--
--
--  File 'rtep.ads'                                      By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  RTEP contains the basic types and the configuration.
--  Here, you may adjust the protocol parameters
--
-------------------------------------------------------------------------------
with Interfaces;
with Ada.Streams;
with Persistent_Signals; pragma Elaborate_All (Persistent_Signals);
with MaRTE_Semaphores;

package RTEP is

   use type Interfaces.Unsigned_16;
   package MS renames MaRTE_Semaphores;

   -------------------------------------
   --  INDEX
   --   1) RTEP Enable/Disable Flags
   --   2) RTEP Tunable Constants
   --   3) RTEP Exceptions
   --   4) RTEP Types
   -------------------------------------

   ----------------------------------
   -- 1) RTEP Enable/Disable Flags --
   ----------------------------------
   --  Enable Delays in Recv/Send a Token to reduce overhead
   subtype Enable_RTEP_Delay is Boolean range True .. True;
   --  Enable Sporadic Servers Support
   subtype Enable_Servers is Boolean range True .. True;
   --  Enable Distributed Mutexes Support
   subtype Enable_Mutexes is Boolean range True .. True;
   --  DEBUG Show Initialization messages
   subtype Enable_DEBUG_Init is Boolean range False .. False;
   --  DEBUG Show states of the main Task
   subtype Enable_DEBUG_States is Boolean range False .. False;
   --  DEBUG Show timeout messages
   subtype Enable_DEBUG_Timers is Boolean range False .. False;
   --  DEBUG Show timeout task messages
   subtype Enable_DEBUG_Timeouts is Boolean range False .. False;
   --  DEBUG Show messages when a station fails
   subtype Enable_DEBUG_Failure is Boolean range True .. True;
   --  DEBUG Show calls to persistent signal functions
   subtype Enable_DEBUG_Sync is Boolean range False .. False;
   --  DEBUG Show multicast related messages
   subtype Enable_DEBUG_Multicast is Boolean range False .. False;
   --  DEBUG Show mutexes related messages
   subtype Enable_DEBUG_Mutexes is Boolean range False .. False;
   --  DEBUG Show C interface related messages
   subtype Enable_DEBUG_C_Interface is Boolean range False .. False;
   --  DEBUG Show Sporadic Servers related messages
   subtype Enable_DEBUG_Servers is Boolean range False .. False;
   --  DEBUG Show RTEP_BWRES messages
   subtype Enable_DEBUG_RTEP_BWRES is Boolean range False .. False;

   -------------------------------
   -- 2) RTEP Tunable Constants --
   -------------------------------
   --  Priority of the RTEP internal communication task (and related ceilings)
   RTEP_Task_Prio : constant := 90;

   --  Maximum number of user bytes per packet
   Max_RTEP_MTU : constant := 1490;

   --  Maximum number of user multicast bytes per token packet
   Multicast_MTU : constant := 26;

   --  Number of reception channels
   Number_Of_Channels : constant := 10;

   --  Number of Sporadic Servers
   Number_Of_Servers : constant := 60;

   --  Maximum number of packets that can be queued with the same priority
   Max_Queued_Element_Same_Priority : constant := 50;

   --  Maximum number of simultaneous pending packets
   Max_RTEP_Packet_At_A_Time : constant Integer :=
      Max_Queued_Element_Same_Priority * Number_Of_Channels;

   --  The number of configured stations.
   Number_Of_Stations : constant := 6;

   --  The number of configured multicast addresses.
   Number_Of_Multicast_Addresses : constant := 2;

   --  The number of available Distributed Mutexes
   Number_Of_Dist_Mutexes : constant := 2;

   --  Identifier for the RTEP protocol
   RTEP_Protocol_Number : constant := 16#A000#; -- 16#A000# for wireshark

   --  Ethernet Device name
   Device_Name : constant String := "/dev/eth0";

   --  Max_retries for packet retransmission, before excluding a station:
   RTEP_Error_Max_Retries : constant := 10000;

   --  Timeout to determine that a packet has been lost
   RTEP_Communication_Timeout : constant Duration := 2.0; -- 0.000_8;

   --  Timeout for the master node to restart the initialization
   RTEP_Comunication_Initialitation_Timeout : constant Duration := 2.0;

   --  Delay between receiving and sending a token to reduce overhead
   RTEP_Delay : constant Duration := 0.000_1;

   -- The packet transmission time relates the budget with the time
   -- for the network utilization calculations (nanoseconds)
   Packet_Tx_Time : constant := 1_000_000;

   ------------------------
   -- 3) RTEP Exceptions --
   ------------------------
   Station_Not_Valid : exception; --  If the station is no longer in the ring
   Station_Not_Found : exception; --  If the station isn't in the logical ring
   Invalid_Channel : exception; --  If the channel is not available
   Info_Length_Overflow : exception; --  If we send more than Max_RTEP_MTU
   Creation_Error : exception; --  If not being able creating the queues
   Unexpected_Error : exception; --  If an unknown error has occurred
   Initialization_Error : exception; --  If an error initializing the protocol

   -------------------
   -- 4) RTEP Types --
   -------------------
   --  The Station Identifier within the protocol.
   type Station_ID is new Interfaces.Unsigned_16
      range 1 .. (Number_Of_Stations + Number_Of_Multicast_Addresses);

   --  The station position in the ring
   subtype Position is Integer
      range 1 .. (Number_Of_Stations + Number_Of_Multicast_Addresses);

   --  The priority of the messages
   type Priority is range 1 .. 2**8-1;
   for Priority'Size use 8;

   --  Identifier of a reception channel
   type Channel is new Interfaces.Unsigned_16 range 1 .. Number_Of_Channels;

   -- Identifier of an Sporadic Server
   type Server_Id is private;

   --  Network_Budget is measured in Number of packets of maximum size
   type Network_Budget is range 0 .. 2**16-1;
   for Network_Budget'Size use 16;

   --  Identifier of a distributed mutex
   type Distributed_Mutex_Id is new Interfaces.Unsigned_16
      range 1 .. Number_Of_Dist_Mutexes;

   --  Rejection_Policy when a queue is full
   type Rejection_Policy is
      (DISCARD_OLDEST,  --  overwrite the oldest data in the Queue
       DISCARD_NEWEST); --  no overwrite

   pragma Convention (C, Rejection_Policy);
   for Rejection_Policy'Size use 8;
   for Rejection_Policy use (DISCARD_OLDEST => 0, DISCARD_NEWEST => 1);

private

   type Server_Id is range 1 .. Number_Of_Servers;
   for Server_Id'Size use 16;

   --------------------------------
   --  Synchronization Variables --
   --------------------------------
   package PSS is new Persistent_Signals (Ceiling => RTEP.RTEP_Task_Prio);
   type Pers_Signals_Array is array (Server_Id) of PSS.Persistent_Signal_Ref;
   Can_I_Send, Is_Result_Available : Pers_Signals_Array;

   --------------------------
   --  Distributed Mutexes --
   --------------------------
   type Distributed_Mutex_Action is (Lock, Unlock, None);

   type Distributed_Mutex is record
      Locked      : Boolean := False;
      Holder      : Station_ID;
      Prio        : Priority;
      Action      : Distributed_Mutex_Action := None;
      Sem         : aliased MS.Semaphore;
      Group       : Station_ID;
   end record;
   pragma Pack (Distributed_Mutex);

   The_Dist_Mutexes : array (Distributed_Mutex_Id'Range) of Distributed_Mutex;

end RTEP;
