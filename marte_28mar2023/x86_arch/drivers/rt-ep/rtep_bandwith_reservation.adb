with Ada.Streams;
with Ada.Real_Time;
with Ada.Numerics.Elementary_Functions; --  for Umax
use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Interfaces;
with Ada.Unchecked_Conversion;

with RTEP;
with RTEP.Protocol; pragma Elaborate_All (RTEP.Protocol);
with RTEP.Protocol.Core;
with RTEP.Protocol.Servers; pragma Elaborate_All (RTEP.Protocol.Servers);
with RTEP_Debug_Functions; pragma Elaborate_All (RTEP_Debug_Functions);
use RTEP_Debug_Functions;
with Generic_Table; pragma Elaborate_All (Generic_Table);
with Time_Measure;
with Var_Strings;
with Commands_Queue; pragma Elaborate_All (Commands_Queue);

package body RTEP_Bandwith_Reservation is

   use type MaRTE.Integer_Types.Int;

   package RP renames RTEP.Protocol;
   package RPS renames RTEP.Protocol.Servers;

   package Accepted_Contracts_Pkg is new Generic_Table
      (RTEP_BWRES_Contract_Id, RTEP_BWRES_Contract);

   Accepted_Contracts : Accepted_Contracts_Pkg.Element_Table;
   Negotiation_Server_Id : RTEP.Server_Id;
   U_Total : Float := 0.0;
   N_Total : Natural := 0;

   type Negotiation_Op_Code is (Add_Contract, Delete_Contract, Update_Contract);
   for Negotiation_Op_Code'Size use 8;
   for Negotiation_Op_Code use
      (Add_Contract => 0, Delete_Contract => 1, Update_Contract => 2);

   type Negotiation_Result is record
      Op : Negotiation_Op_Code;
      Id : RTEP_BWRES_Contract_Id;
      Contract : RTEP_BWRES_Contract;
   end record;
   pragma Pack (Negotiation_Result);
   Negotiation_Result_Size : constant := 1 + 2 + RTEP_BWRES_Contract_Size;
   for Negotiation_Result'Size use Negotiation_Result_Size*8;

   type Negotiation_Result_Stream is new
      Ada.Streams.Stream_Element_Array (1 .. Negotiation_Result_Size);

   type Negotiation_Result_Stream_Ac is access all Negotiation_Result_Stream;
   type Negotiation_Result_Ac is access all Negotiation_Result;

   function Stream_To_Negotiation_Result is new Ada.Unchecked_Conversion
      (Negotiation_Result_Stream_Ac, Negotiation_Result_Ac);

   function Negotiation_Result_To_Stream is new Ada.Unchecked_Conversion
      (Negotiation_Result_Ac, Negotiation_Result_Stream_Ac);

   function Server_Id_To_Unsigned_16 is new Ada.Unchecked_Conversion
      (RTEP.Server_Id, Interfaces.Unsigned_16);

   ---------------------------------
   --  THE (RE)NEGOTIATIONS QUEUE --
   ---------------------------------
   --  This is the queue for the commands sent by users to the
   --  negotiation task daemon. Since we are going to access concurrently we
   --  need to protect the queue. The Queue is a priority one so the commands
   --  are served in priority order.
   -------------------------------------------------------------------
   type Negotiation_Op is (Negotiate, Cancel, Renegotiate);

   type Negotiation_Command is record
      Op       : Negotiation_Op;
      Contract : RTEP_BWRES_Contract;
      Vres     : RTEP_BWRES_Virtual_Resource_Id;
   end record;

   package Negotiations_Queue is new Commands_Queue
      (Size            => Mx_Neg_Requests,
       Generic_Command => Negotiation_Command,
       Prio            => Mx_Negotiation_Prio);
   use type Negotiations_Queue.Command_Status;

   ----------
   -- Init --
   ----------

   Time_Negotiate_ID : Time_Measure.Measure_ID;
   Time_Cancel_ID    : Time_Measure.Measure_ID;

   function Init return BIT.Int is
   begin
      --  Init Main Task
      RP.Core.Init_Comm;
      --  Init time measure
      Time_Negotiate_ID := Time_Measure.Init_Time_Measure
         (Var_Strings.To_Var_String ("RTEP_BWRES:Negotiate_Contract"),
            Time_Measure.ABS_Time_Clock);
      Time_Cancel_ID := Time_Measure.Init_Time_Measure
         (Var_Strings.To_Var_String ("TEST:Cancel_Contract"),
            Time_Measure.ABS_Time_Clock);
      --  Create a Server for performing the negotations
      RPS.Create_Server
         (Max_Allocated_Budget => 1,
          Server_Period => Ada.Real_Time.To_Time_Span (Negotiation_Msg_Period),
          Server_Priority      => Negotiation_Msg_Prio,
          Id                   => Negotiation_Server_Id);
      --  Init the distributed mutex
      RP.Init_Distributed_Mutex (Negotiation_Mutex);
      return 0;
   exception
      when The_Error : others =>
         ERROR ("Init Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Init;

   -----------------
   -- Create_Vres --
   -----------------

   function Create_Vres return RTEP_BWRES_Virtual_Resource_Id is
   begin
      return new RTEP_BWRES_Virtual_Resource;
   end Create_Vres;

   --------------------------
   -- Set_Basic_Parameters --
   --------------------------

   function Set_Basic_Params
      (Contract   : access RTEP_BWRES_Contract;
       Budget_Min : in RTEP.Network_Budget;
       Period_Max : in MaRTE.Timespec.Timespec;
       Deadline   : in MaRTE.Timespec.Timespec) return BIT.Int is
   begin
      Contract.Budget_Min := Budget_Min;
      Contract.Period_Max := Period_Max;
      Contract.Deadline   := Deadline;
      DEBUG ("Budget_Min: "
         &RTEP.Network_Budget'Image (Contract.Budget_Min),
         RTEP.Enable_DEBUG_C_Interface'First);
      DEBUG ("Period_Max: "
         &Duration'Image (MaRTE.Timespec.To_Duration (Contract.Period_Max)),
         RTEP.Enable_DEBUG_C_Interface'First);
      return 0;
   exception
      when The_Error : others =>
         ERROR ("Set_Basic_Params Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Set_Basic_Params;

   ------------------
   -- Set_Priority --
   ------------------

   function Set_Priority
      (Contract : access RTEP_BWRES_Contract; --  in out
       Prio     : in RTEP.Priority) return BIT.Int is
   begin
      Contract.Priority := Prio;
      DEBUG ("Priority: "&RTEP.Priority'Image (Contract.Priority),
         RTEP.Enable_DEBUG_C_Interface'First);
      return 0;
   exception
      when The_Error : others =>
         ERROR ("Set_Priority Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Set_Priority;

   --------------------------
   -- UTILIZATION Formulas --
   --------------------------
   --  TODO: ADD OVERHEAD!! (IN F.I.R.S.T. OVERHEAD WAS INCLUDED IN Tx_TIME)
   --  Utilization
   function Utilization (B : RTEP.Network_Budget;
                         T : Duration) return Float is
   begin
      return (Float(B)*Float(RTEP.Packet_Tx_Time)/1.0E9)/
         Float(T);
   end Utilization;

   --  Maximum utilization value for N tasks/servers
   function Umax (N : Natural) return Float is
   begin
      return Float(N)*(2.0**(1.0/Float(N))-1.0);
   end Umax;

   -------------------
   -- Get_Server_Id --
   -------------------

   function Get_Server_Id
      (Vres : in RTEP_BWRES_Virtual_Resource_Id;
       S_Id : access RTEP.Server_Id) --  out
       return BIT.Int is
   begin
      S_Id.all := Vres.Server;
      return 0;
   exception
      when The_Error : others =>
         ERROR ("Get_Server_Id Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Get_Server_Id;

   ------------------------------
   -- NEGOTIATION RESULTS TASK --
   ------------------------------

   task Negotiation_Results_Task is
      pragma Priority (Negotiation_Results_Task_Prio);
   end Negotiation_Results_Task;

   task body Negotiation_Results_Task is
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Priority : RTEP.Priority;
      Neg_Result_Ac : Negotiation_Result_Ac;
      C_Id : RTEP_BWRES_Contract_Id;
      Old_Contract : RTEP_BWRES_Contract;
   begin
      loop
         --  Wait for new contracts
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
            Channel_ID        => Negotiation_Channel,
            Data              => Rx_Info,
            Last              => Rx_Info_Last,
            Data_Priority     => Rx_Priority);

         --  Convert STREAM to the negotiation message
         Neg_Result_Ac := Stream_To_Negotiation_Result
            (Negotiation_Result_Stream
               (Rx_Info (1 .. Rx_Info_Last))'Unrestricted_Access);

         DEBUG ("RESULT "
            &Negotiation_Op_Code'Image (Neg_Result_Ac.Op),
            RTEP.Enable_DEBUG_RTEP_BWRES'First);

         case Neg_Result_Ac.Op is
            when Add_Contract =>
               --  Update the Accepted_Contracts table. Note that we don't
               --  need to keep a Virtual Resource for the contract because
               --  it's external.
               Accepted_Contracts_Pkg.Add
                  (Neg_Result_Ac.Contract, C_Id, Accepted_Contracts);

               if Neg_Result_Ac.Id /= Neg_Result_Ac.Id then
                  DEBUG ("ERROR, server_id inconsistent", True);
                  exit;
               end if;

               --  Update the Total Utilization value and the number of Servers
               U_Total := U_Total + Utilization
                  (Neg_Result_Ac.Contract.Budget_Min,
                   MaRTE.Timespec.To_Duration
                     (Neg_Result_Ac.Contract.Period_Max));
               N_Total := N_Total + 1;

               DEBUG ("RESULT: U_total="&Float'Image (U_Total)&
                  " N="&Natural'Image(N_Total)&
                  " U_i="&Float'Image(Utilization
                                         (Neg_Result_Ac.Contract.Budget_Min,
                                          MaRTE.Timespec.To_Duration
                                          (Neg_Result_Ac.Contract.Period_Max)))&
                  "U_max="&Float'Image (Umax(N_Total)),
                  RTEP.Enable_DEBUG_RTEP_BWRES'First);

               if U_Total > 1.0 then
                  DEBUG ("ERROR, utilization inconsistency", True);
                  exit;
               end if;
               DEBUG ("RESULT CONTRACT: "&
                  "C="&RTEP.Network_Budget'Image
                     (Neg_Result_Ac.Contract.Budget_Min)&
                  "T="&Duration'Image
                     (MaRTE.Timespec.To_Duration
                        (Neg_Result_Ac.Contract.Period_Max))&
                  "D="&Duration'Image
                     (MaRTE.Timespec.To_Duration
                        (Neg_Result_Ac.Contract.Deadline))&
                  "P="&RTEP.Priority'Image (Neg_Result_Ac.Contract.Priority),
                     RTEP.Enable_DEBUG_RTEP_BWRES'First);
               DEBUG ("RESULT From: "&RTEP.Station_ID'Image (Source_Station_ID)&
                  "  Contract_Id: "&RTEP_BWRES_Contract_Id'Image(C_Id),
                  RTEP.Enable_DEBUG_RTEP_BWRES'First);
            when Delete_Contract =>
               --  Get contract we are deleting
               Old_Contract := Accepted_Contracts_Pkg.Item
                  (Neg_Result_Ac.Id, Accepted_Contracts);
               --  Delete local contract copy
               Accepted_Contracts_Pkg.Delete
                  (Neg_Result_Ac.Id, Accepted_Contracts);
               --  Update the Total Utilization value and the number of Servers
               U_Total := U_Total - Utilization
                  (Old_Contract.Budget_Min,
                   MaRTE.Timespec.To_Duration (Old_Contract.Period_Max));
               N_Total := N_Total - 1;

               DEBUG ("RESULT From: "&
                  RTEP.Station_ID'Image (Source_Station_ID)&
                  " Deleted contract="&
                  RTEP_BWRES_Contract_Id'Image (Neg_Result_Ac.Id)&
                  " U="&Float'Image (U_Total)&
                  " N="&Natural'Image(N_Total),
                  RTEP.Enable_DEBUG_RTEP_BWRES'First);

         when Update_Contract =>
               --  Get contract we are updating
               Old_Contract := Accepted_Contracts_Pkg.Item
                  (Neg_Result_Ac.Id, Accepted_Contracts);
               --  Update local contract copy
               Accepted_Contracts_Pkg.Update
                  (Neg_Result_Ac.Contract,
                   Neg_Result_Ac.Id,
                   Accepted_Contracts);
               --  Update the Total Utilization value and the number of Servers
               U_Total := U_Total
                  - Utilization
                     (Old_Contract.Budget_Min,
                     MaRTE.Timespec.To_Duration (Old_Contract.Period_Max))
                  + Utilization
                     (Neg_Result_Ac.Contract.Budget_Min,
                     MaRTE.Timespec.To_Duration
                     (Neg_Result_Ac.Contract.Period_Max));

               DEBUG ("RESULT From: "&
                  RTEP.Station_ID'Image (Source_Station_ID)&
                  " Updated contract="&
                  RTEP_BWRES_Contract_Id'Image (Neg_Result_Ac.Id)&
                  " U="&Float'Image (U_Total)&
                  " N="&Natural'Image(N_Total),
                  RTEP.Enable_DEBUG_RTEP_BWRES'First);
         end case;
      end loop;
   exception
      when The_Error : others =>
         ERROR ("Negotiation_Results_Task Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
   end Negotiation_Results_Task;

   ----------------------
   -- NEGOTIATION TASK --
   ----------------------

   task Negotiation_Task is
      pragma Priority (Negotiation_Task_Prio);
   end Negotiation_Task;

   task body Negotiation_Task is
      S_Id : RTEP.Server_Id;
      U_New : Float;
      Neg_Result : aliased Negotiation_Result;
      Cmd   : Negotiations_Queue.Command;
      Neg_Cmd : Negotiation_Command;
      Accepted : Boolean;
      Old_Contract : RTEP_BWRES_Contract;
   begin
      loop
         --  Block for a Command
         Negotiations_Queue.Dequeue (Cmd, Neg_Cmd);
         case Neg_Cmd.Op is
            when Negotiate =>
               --  Lock the negotiation mutex
               RP.Lock_Distributed_Mutex
                  (Negotiation_Mutex, Negotiation_Mutex_Prio);
               --  Negotiate the contract
               --  TODO: arbitrary deadlines not yet supported!!!
               U_New := U_Total + Utilization
                  (Neg_Cmd.Contract.Budget_Min,
                  MaRTE.Timespec.To_Duration (Neg_Cmd.Contract.Period_Max));
               Accepted := (U_New <= Umax (N_Total + 1));

               DEBUG("Negotiation_Task NEG:"&
                      " U_new="&Float'Image (U_New)&
                      " N="&Natural'Image(N_Total + 1)&
                      " U_max="&Float'Image (Umax(N_Total + 1))&
                      " Accepted="&Boolean'Image (Accepted),
                      RTEP.Enable_DEBUG_RTEP_BWRES'First);

               DEBUG("Negotiation_Task NEG:"&
                     " Bmin="&RTEP.Network_Budget'Image
                                              (Neg_Cmd.Contract.Budget_Min)&
                     " Tmax="&Duration'Image(MaRTE.Timespec.To_Duration
                                              (Neg_Cmd.Contract.Period_Max))&
                     " U_server:"&Float'Image
                                (Utilization
                                     (Neg_Cmd.Contract.Budget_Min,
                                      MaRTE.Timespec.To_Duration
                                              (Neg_Cmd.Contract.Period_Max))),
                      RTEP.Enable_DEBUG_RTEP_BWRES'First);

               if not Accepted then
                  RP.Unlock_Distributed_Mutex
                     (Negotiation_Mutex, Negotiation_Mutex_Prio);
                  Negotiations_Queue.Set_Status
                     (Cmd, Negotiations_Queue.Error);
               else
                  --  Create a rtep sporadic server
                  RPS.Create_Server
                     (Max_Allocated_Budget => Neg_Cmd.Contract.Budget_Min,
                      Server_Period         => Ada.Real_Time.To_Time_Span
                      (MaRTE.Timespec.To_Duration (Neg_Cmd.Contract.Period_Max)),
                      Server_Priority       => Neg_Cmd.Contract.Priority,
                      Id                    => S_Id);

                  --  Create and initialize the virtual resource
                  Neg_Cmd.Vres.Server := S_Id;

                  --  Add it to the table of accepted contracts
                  Accepted_Contracts_Pkg.Add
                     (Neg_Cmd.Contract,
                      Neg_Cmd.Vres.Contract_Id,
                      Accepted_Contracts);

                  --  Update the utilization values
                  U_Total := U_New;
                  N_Total := N_Total + 1;

                  --  Spread out the results of the negotiation
                  Neg_Result.Op       := Add_Contract;
                  Neg_Result.Id       := Neg_Cmd.Vres.Contract_Id;
                  Neg_Result.Contract := Neg_Cmd.Contract;

                  RP.Send_Info
                  (Destination_Station_ID =>
                        RP.Get_Station_ID_By_Name ("broadcast"),
                     Channel_ID             => Negotiation_Channel,
                     Data => Ada.Streams.Stream_Element_Array
                        (Negotiation_Result_To_Stream
                           (Neg_Result'Unrestricted_Access).all),
                     Id                     => Negotiation_Server_Id,
                     Blocking               => True);

                  --  Unlock the distributed Mutex
                  RP.Unlock_Distributed_Mutex
                     (Negotiation_Mutex, Negotiation_Mutex_Prio);
                  Negotiations_Queue.Set_Status
                     (Cmd, Negotiations_Queue.Workdone);
               end if;
            when Renegotiate =>
               --  Lock the negotiation mutex
               RP.Lock_Distributed_Mutex
                  (Negotiation_Mutex, Negotiation_Mutex_Prio);
               --  Calculate the new utilization values
               --  TODO: arbitrary deadlines not yet supported!!!
               Old_Contract := Accepted_Contracts_Pkg.Item
                  (Neg_Cmd.Vres.Contract_Id, Accepted_Contracts);

               U_New := U_Total
                  - Utilization
                     (Old_Contract.Budget_Min,
                     MaRTE.Timespec.To_Duration (Old_Contract.Period_Max))
                  + Utilization
                     (Neg_Cmd.Contract.Budget_Min,
                     MaRTE.Timespec.To_Duration (Neg_Cmd.Contract.Period_Max));

               Accepted := U_New <= Umax (N_Total);

               if not Accepted then
                  RP.Unlock_Distributed_Mutex
                     (Negotiation_Mutex, Negotiation_Mutex_Prio);
                  Negotiations_Queue.Set_Status
                     (Cmd, Negotiations_Queue.Error);
               else
                  --  Update the rtep sporadic server
                  RPS.Update_Server
                     (Max_Allocated_Budget  => Neg_Cmd.Contract.Budget_Min,
                      Server_Period         => Ada.Real_Time.To_Time_Span
                      (MaRTE.Timespec.To_Duration (Neg_Cmd.Contract.Period_Max)),
                      Server_Priority       => Neg_Cmd.Contract.Priority,
                      Id                    => Neg_Cmd.Vres.Server);

                  --  Update the table of accepted contracts
                  Accepted_Contracts_Pkg.Update
                     (Neg_Cmd.Contract,
                      Neg_Cmd.Vres.Contract_Id,
                      Accepted_Contracts);

                  --  Update the utilization values
                  U_Total := U_New;

                  DEBUG
                     ("Negotiation_Task RENEG: U="&Float'Image (U_Total)&
                      " N="&Natural'Image(N_Total),
                      RTEP.Enable_DEBUG_RTEP_BWRES'First);

                  --  Spread out the results of the renegotiation
                  Neg_Result.Op       := Update_Contract;
                  Neg_Result.Id       := Neg_Cmd.Vres.Contract_Id;
                  Neg_Result.Contract := Neg_Cmd.Contract;

                  RP.Send_Info
                     (Destination_Station_ID =>
                        RP.Get_Station_ID_By_Name ("broadcast"),
                     Channel_ID              => Negotiation_Channel,
                     Data => Ada.Streams.Stream_Element_Array
                        (Negotiation_Result_To_Stream
                           (Neg_Result'Unrestricted_Access).all),
                     Id                      => Negotiation_Server_Id,
                     Blocking                => True);

                  --  Unlock the distributed Mutex
                  RP.Unlock_Distributed_Mutex
                     (Negotiation_Mutex, Negotiation_Mutex_Prio);
                  Negotiations_Queue.Set_Status
                     (Cmd, Negotiations_Queue.Workdone);
               end if;
            when Cancel =>
               --  Lock the negotiation mutex
               RP.Lock_Distributed_Mutex
                  (Negotiation_Mutex, Negotiation_Mutex_Prio);
               --  Delete sporadic server
               RPS.Delete_Server (Neg_Cmd.Vres.Server);
               --  Update utilization values
               Old_Contract := Accepted_Contracts_Pkg.Item
                  (Neg_Cmd.Vres.Contract_Id, Accepted_Contracts);
               U_Total := U_Total - Utilization
                  (Old_Contract.Budget_Min,
                   MaRTE.Timespec.To_Duration (Old_Contract.Period_Max));
               N_Total := N_Total - 1;
               DEBUG ("Negotiation_Task CANCEL: U="&Float'Image (U_Total)&
                  " N="&Natural'Image(N_Total)&
                  " S_Id deleted="&Interfaces.Unsigned_16'Image
                     (Server_Id_To_Unsigned_16 (Neg_Cmd.Vres.Server)),
                  RTEP.Enable_DEBUG_RTEP_BWRES'First);
               --  Delete local contract copy
               Accepted_Contracts_Pkg.Delete
                  (Neg_Cmd.Vres.Contract_Id, Accepted_Contracts);
               --  Spread results
               Neg_Result.Op := Delete_Contract;
               Neg_Result.Id := Neg_Cmd.Vres.Contract_Id;

               RP.Send_Info
               (Destination_Station_ID =>
                  RP.Get_Station_ID_By_Name ("broadcast"),
                  Channel_ID             => Negotiation_Channel,
                  Data => Ada.Streams.Stream_Element_Array
                     (Negotiation_Result_To_Stream
                        (Neg_Result'Unrestricted_Access).all),
                  Id                     => Negotiation_Server_Id,
                  Blocking               => True);
               --  Unlock the distributed Mutex
               RP.Unlock_Distributed_Mutex
                  (Negotiation_Mutex, Negotiation_Mutex_Prio);
               Negotiations_Queue.Set_Status
                  (Cmd, Negotiations_Queue.Workdone);
         end case;
      end loop; -- end BIG LOOP
   exception
      when The_Error : others =>
         ERROR ("Negotiation_Task Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
   end Negotiation_Task;

   ------------------------
   -- Negotiate_Contract --
   ------------------------

   function Negotiate_Contract
      (Contract   : in  RTEP_BWRES_Contract;
       Vres       : in RTEP_BWRES_Virtual_Resource_Id) --  out
       return BIT.Int
   is
      Neg_Cmd : Negotiation_Command;
      Cmd : Negotiations_Queue.Command;
      Stat : Negotiations_Queue.Command_Status;
   begin
      DEBUG ("C >> Negotiate_Contract, Tmax: "
         &Duration'Image (MaRTE.Timespec.To_Duration (Contract.Period_Max))&
         " Bmin: "&RTEP.Network_Budget'Image (Contract.Budget_Min),
         RTEP.Enable_DEBUG_C_Interface'First);

      Time_Measure.Set_Time_Mark (Time_Negotiate_ID, Time_Measure.First);

      --  Create the command for the negotation task
      Neg_Cmd.Op := Negotiate;
      Neg_Cmd.Contract := Contract;
      Neg_Cmd.Vres := Vres;
      --  Enqueue the command
      Negotiations_Queue.Enqueue (Neg_Cmd, Cmd);
      --  Wait for its execution
      Stat := Negotiations_Queue.Wait (Cmd);
      if Stat /= Negotiations_Queue.Workdone then
         Time_Measure.Set_Time_Mark (Time_Negotiate_ID, Time_Measure.Last);
         return -1;
      end if;
      Negotiations_Queue.Set_Status (Cmd, Negotiations_Queue.Not_In_Use);

      Time_Measure.Set_Time_Mark (Time_Negotiate_ID, Time_Measure.Last);

      DEBUG ("Contract accepted ID: "&
         RTEP_BWRES_Contract_Id'Image (Vres.Contract_Id)
         &"Server ID: "&Interfaces.Unsigned_16'Image
         (Server_Id_To_Unsigned_16 (Vres.Server)),
         RTEP.Enable_DEBUG_RTEP_BWRES'First);
      return 0;
   exception
      when The_Error : others =>
         ERROR ("Negotiate_Contract Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Negotiate_Contract;

   -------------------------------
   -- Renegotiate_Contract_Sync --
   -------------------------------

   function Renegotiate_Contract_Sync
      (Vres     : in RTEP_BWRES_Virtual_Resource_Id;
       Contract : in RTEP_BWRES_Contract) return BIT.Int
   is
      Neg_Cmd : Negotiation_Command;
      Cmd : Negotiations_Queue.Command;
      Stat : Negotiations_Queue.Command_Status;
   begin
      DEBUG ("Renegotiate_Contract, Contract ID: "&
         RTEP_BWRES_Contract_Id'Image (Vres.Contract_Id)
         &"Server ID: "&Interfaces.Unsigned_16'Image
         (Server_Id_To_Unsigned_16 (Vres.Server))
         &"Period_Max: "&Duration'Image
         (MaRTE.Timespec.To_Duration (Contract.Period_Max)),
         RTEP.Enable_DEBUG_RTEP_BWRES'First);
      --  Create the command for the negotation task
      Neg_Cmd.Op := Renegotiate;
      Neg_Cmd.Contract := Contract;
      Neg_Cmd.Vres := Vres;
      --  Enqueue the command
      Negotiations_Queue.Enqueue (Neg_Cmd, Cmd);
      --  Wait for its execution
      Stat := Negotiations_Queue.Wait (Cmd);
      if Stat /= Negotiations_Queue.Workdone then
         return -1;
      end if;
      Negotiations_Queue.Set_Status (Cmd, Negotiations_Queue.Not_In_Use);
      DEBUG ("Renegotiation accepted", RTEP.Enable_DEBUG_RTEP_BWRES'First);
      return 0;
   exception
      when The_Error : others =>
         ERROR ("Renegotiate_Contract_Sync Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Renegotiate_Contract_Sync;

   ---------------------
   -- Cancel_Contract --
   ---------------------

   function Cancel_Contract
     (Vres : in RTEP_BWRES_Virtual_Resource_Id) return BIT.Int
   is
      Neg_Cmd : Negotiation_Command;
      Cmd : Negotiations_Queue.Command;
      Stat : Negotiations_Queue.Command_Status;
   begin

      Time_Measure.Set_Time_Mark (Time_Cancel_ID, Time_Measure.First);

      --  Create the command for the negotation task
      Neg_Cmd.Op := Cancel;
      Neg_Cmd.Vres := Vres;

      DEBUG ("Cancel_Contract ID: "&
         RTEP_BWRES_Contract_Id'Image (Vres.Contract_Id)
         &"Server ID: "&Interfaces.Unsigned_16'Image
         (Server_Id_To_Unsigned_16 (Vres.Server)), True);
         -- RTEP.Enable_DEBUG_RTEP_BWRES'First);

      --  Enqueue the command
      Negotiations_Queue.Enqueue (Neg_Cmd, Cmd);
      --  Wait for its execution
      Stat := Negotiations_Queue.Wait (Cmd);
      if Stat /= Negotiations_Queue.Workdone then
         Time_Measure.Set_Time_Mark (Time_Cancel_ID, Time_Measure.Last);
         return -1;
      end if;
      Negotiations_Queue.Set_Status (Cmd, Negotiations_Queue.Not_In_Use);

      Time_Measure.Set_Time_Mark (Time_Cancel_ID, Time_Measure.Last);

      return 0;
   exception
      when The_Error : others =>
         ERROR ("Cancel_Contract Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Cancel_Contract;

   ------------------------------------
   -- Renegotiate_Negotiation_Period --
   ------------------------------------

   function Renegotiate_Negotiation_Period
      (Period_Max : in MaRTE.Timespec.Timespec) return BIT.Int
   is
   begin
      -- TODO: check schedulability analysis!!
      RPS.Set_Max_Budget_And_Period
         (Id                   => Negotiation_Server_Id,
          Max_Allocated_Budget => 1,
          Server_Period        => Ada.Real_Time.To_Time_Span
            (MaRTE.Timespec.To_Duration (Period_Max)));
      return 0;
   exception
      when The_Error : others =>
         ERROR ("Renegotiate_Negotiation_Period Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Renegotiate_Negotiation_Period;

   ----------------------------
   -- Get_Negotiation_Period --
   ----------------------------

   function Get_Negotiation_Period
      (Period_Max : access MaRTE.Timespec.Timespec) -- out
       return BIT.Int
   is
      Max_Allocated_Budget : RTEP.Network_Budget;
      Server_Period        : Ada.Real_Time.Time_Span;
      Server_Priority      : RTEP.Priority;
   begin
      RPS.Get_Server_Info
         (Id                   => Negotiation_Server_Id,
          Max_Allocated_Budget => Max_Allocated_Budget,
          Server_Period        => Server_Period,
          Server_Priority      => Server_Priority);
      Period_Max.all := MaRTE.Timespec.To_Timespec
         (Ada.Real_Time.To_Duration (Server_Period));
      return 0;
   exception
      when The_Error : others =>
         ERROR ("Renegotiate_Negotiation_Period Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         return -1;
   end Get_Negotiation_Period;

end RTEP_Bandwith_Reservation;
