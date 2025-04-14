with RTEP; --  for Priority, Server_Id, Number_Of_Servers, Number_Of_Stations
with Ada.Real_Time;
with MaRTE.Timespec; --  for Timespec
pragma Elaborate_All (MaRTE.Timespec);
with MaRTE.Integer_Types;

package RTEP_Bandwith_Reservation is

   package BIT renames MaRTE.Integer_Types;

   --  Maximum number of accepted contracts for a network (global value)
   Max_Contracts : constant := RTEP.Number_Of_Servers*RTEP.Number_Of_Stations;
   --  Maximum number of pending negotiation requests at a time
   Mx_Neg_Requests : constant := 10;
   --  Distributed Mutex used for negotiation
   Negotiation_Mutex   : constant RTEP.Distributed_Mutex_Id := 1;
   --  The priority at which lock and unlock are performed
   Negotiation_Mutex_Prio  : constant RTEP.Priority := 5;
   --  Channel through where negotiation messages are sent
   Negotiation_Channel : constant RTEP.Channel := 1;
   --  Initial priority for the network server for negotiation messages
   Negotiation_Msg_Prio  : constant := 18;
   --  Initial period for the network server for negotiation messages
   Negotiation_Msg_Period  : constant Duration := 1.0;
   --  Internal Negotiation TASK priority
   Negotiation_Task_Prio : constant := 80;
   --  Internal Negotiation Results TASK priority (higher than the previous one)
   Negotiation_Results_Task_Prio : constant := Negotiation_Task_Prio + 1;
   --  Maximum priority of tasks involved (to put ceiling priority of mutex)
   Mx_Negotiation_Prio : constant := Negotiation_Results_Task_Prio;

   type RTEP_BWRES_Contract is private;
   type RTEP_BWRES_Virtual_Resource_Id is private;

   ----------
   -- Init --
   ----------

   function Init return BIT.Int;

   pragma Export (C, Init, "rtep_bwres_init");
   pragma Inline (Init);

   -----------------
   -- Create_Vres --
   -----------------

   function Create_Vres return RTEP_BWRES_Virtual_Resource_Id;

   ----------------------
   -- Set_Basic_Params --
   ----------------------

   function Set_Basic_Params
      (Contract   : access RTEP_BWRES_Contract; --  in out
       Budget_Min : in RTEP.Network_Budget;
       Period_Max : in MaRTE.Timespec.Timespec;
       Deadline   : in MaRTE.Timespec.Timespec) return BIT.Int;

   pragma Export (C, Set_Basic_Params,"rtep_bwres_contract_set_basic_params");
   pragma Inline (Set_Basic_Params);

   ------------------
   -- Set_Priority --
   ------------------

   function Set_Priority
      (Contract : access RTEP_BWRES_Contract; --  in out
       Prio     : in RTEP.Priority) return BIT.Int;

   pragma Export (C, Set_Priority,"rtep_bwres_set_priority");
   pragma Inline (Set_Priority);

   ------------------------
   -- Negotiate_Contract --
   ------------------------

   function Negotiate_Contract
      (Contract   : in RTEP_BWRES_Contract;
       Vres       : in RTEP_BWRES_Virtual_Resource_Id)
       return BIT.Int;

   pragma Export (C, Negotiate_Contract,"rtep_bwres_contract_negotiate");
   pragma Inline (Negotiate_Contract);

   ---------------------
   -- Cancel_Contract --
   ---------------------

   function Cancel_Contract
     (Vres : in RTEP_BWRES_Virtual_Resource_Id) return BIT.Int;

   pragma Export (C, Cancel_Contract, "rtep_bwres_vres_destroy");
   pragma Inline (Cancel_Contract);

   -------------------
   -- Get_Server_Id --
   -------------------

   function Get_Server_Id
      (Vres : in RTEP_BWRES_Virtual_Resource_Id;
       S_Id : access RTEP.Server_Id) --  out
       return BIT.Int;

   pragma Export (C, Get_Server_Id,"rtep_bwres_get_server_id");
   pragma Inline (Get_Server_Id);

   -------------------------------
   -- Renegotiate_Contract_Sync --
   -------------------------------

   function Renegotiate_Contract_Sync
      (Vres     : in RTEP_BWRES_Virtual_Resource_Id;
       Contract : in  RTEP_BWRES_Contract)
       return BIT.Int;

   pragma Export
      (C, Renegotiate_Contract_Sync, "rtep_bwres_contract_renegotiate_sync");
   pragma Inline (Renegotiate_Contract_Sync);

   --------------------------------
   -- Renegotiate_Contract_Async --
   --------------------------------

--    function Renegotiate_Contract_Async
--       (Vres     : in RTEP_BWRES_Virtual_Resource_Id;
--        Contract : in  RTEP_BWRES_Contract)
--        return BIT.Int;
--
--    pragma Export
--       (C, Renegotiate_Contract_Async, "rtep_bwres_contract_renegotiate_async");
--    pragma Inline (Renegotiate_Contract_Async);

   ------------------------------
   -- Get_Renegotiation_Status --
   ------------------------------

--    function Get_Renegotiation_Status
--       (Vres     : in RTEP_BWRES_Virtual_Resource_Id;
--        Status   : access RTEP_BWRES_Renegotiation_Status)
--        return BIT.Int;
--
--    pragma Export
--       (C, Get_Renegotiation_Status, "rtep_bwres_get_renegotiation_status");
--    pragma Inline (Get_Renegotiation_Status);

   ------------------------------------
   -- Renegotiate_Negotiation_Period --
   ------------------------------------

   function Renegotiate_Negotiation_Period
      (Period_Max : in MaRTE.Timespec.Timespec)
       return BIT.Int;

   pragma Export (C, Renegotiate_Negotiation_Period,
      "rtep_bwres_renegotiate_negotiation_period");
   pragma Inline (Renegotiate_Negotiation_Period);

   ----------------------------
   -- Get_Negotiation_Period --
   ----------------------------

   function Get_Negotiation_Period
      (Period_Max : access MaRTE.Timespec.Timespec)
       return BIT.Int;

   pragma Export (C, Get_Negotiation_Period,
      "rtep_bwres_get_negotiation_period");
   pragma Inline (Get_Negotiation_Period);

private

   ---------------
   --  CONTRACT --
   ---------------

   type RTEP_BWRES_Contract is record
      Period_Max : MaRTE.Timespec.Timespec;
      Deadline   : MaRTE.Timespec.Timespec;
      Budget_Min : RTEP.Network_Budget;
      Priority   : RTEP.Priority;
   end record;
   pragma Pack (RTEP_BWRES_Contract);
   RTEP_BWRES_Contract_Size : constant := 2 + 8 + 8 + 1;
   for RTEP_BWRES_Contract'Size use RTEP_BWRES_Contract_Size * 8;

   type RTEP_BWRES_Contract_Id is range 1 .. Max_Contracts;
   for RTEP_BWRES_Contract_Id'Size use 16;

   -----------------------
   --  Virtual Resource --
   -----------------------

   type RTEP_BWRES_Virtual_Resource is record
      Contract_Id : RTEP_BWRES_Contract_Id;
      Server      : RTEP.Server_Id;
   end record;

   pragma Convention (C, RTEP_BWRES_Virtual_Resource);
   pragma Pack (RTEP_BWRES_Virtual_Resource);
   for RTEP_BWRES_Virtual_Resource'Size use (2 + 2)*8;

   type RTEP_BWRES_Virtual_Resource_Id is access RTEP_BWRES_Virtual_Resource;

end RTEP_Bandwith_Reservation;