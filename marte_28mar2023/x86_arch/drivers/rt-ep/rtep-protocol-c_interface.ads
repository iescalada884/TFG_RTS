with MaRTE.Integer_Types;

package RTEP.Protocol.C_Interface  is

   type Buffer is array (1 .. MaRTE.Integer_Types.Size_T'Last)
      of MaRTE.Integer_Types.Unsigned_8;
   pragma Pack (Buffer);

   type Buffer_Ac is access all Buffer;

   procedure Send_Info_C
     (Destination_Station_ID : in Station_ID;
      Channel_ID             : in Channel;
      Data                   : in Buffer_Ac;
      Size                   : in MaRTE.Integer_Types.Size_T;
      Data_Priority          : in Priority);
   pragma Export (C, Send_Info_C, "rtep_send_info");
   pragma Inline (Send_Info_C);

   procedure Send_Info_Server_C
     (Destination_Station_ID : in Station_ID;
      Channel_ID             : in Channel;
      Data                   : in Buffer_Ac;
      Size                   : in MaRTE.Integer_Types.Size_T;
      Id                     : in Server_ID;
      Blocking               : in Boolean);
   pragma Export (C, Send_Info_Server_C, "rtep_server_send_info");
   pragma Inline (Send_Info_Server_C);

   procedure Recv_Info_C
     (Source_Station_ID : out Station_ID;
      Channel_ID        : in Channel;
      Data              : in Buffer_Ac;
      Size              : in MaRTE.Integer_Types.Size_T;
      Last              : out MaRTE.Integer_Types.Size_T;
      Data_Priority     : out Priority);
   pragma Export (C, Recv_Info_C, "rtep_recv_info");
   pragma Inline (Recv_Info_C);

   function Get_Station_ID_By_Name_C
      (Station_Name : in Buffer_Ac;
       Length       : in MaRTE.Integer_Types.Size_T) return Station_ID;
   pragma Export (C, Get_Station_ID_By_Name_C, "rtep_get_station_id_by_name");
   pragma Inline (Get_Station_ID_By_Name_C);

   function Create_Server_C
      (Max_Allocated_Budget : in  Network_Budget;
       Period_Sec           : in  MaRTE.Integer_Types.Int;
       Period_Nsec          : in  MaRTE.Integer_Types.Int;
       Server_Priority      : in  RTEP.Priority;
       Id                   : access Server_Id)
      return MaRTE.Integer_Types.Int;
   pragma Export (C, Create_Server_C, "rtep_server_create");
   pragma Inline (Create_Server_C);

   function Init_Distributed_Mutex_C
      (M : in Distributed_Mutex_Id) return MaRTE.Integer_Types.Int;
   pragma Export (C, Init_Distributed_Mutex_C, "rtep_init_distributed_mutex");
   pragma Inline (Init_Distributed_Mutex_C);

   function Lock_Distributed_Mutex_C
      (M    : in Distributed_Mutex_Id;
       Prio : in RTEP.Priority) return MaRTE.Integer_Types.Int;
   pragma Export (C, Lock_Distributed_Mutex_C, "rtep_lock_distributed_mutex");
   pragma Inline (Lock_Distributed_Mutex_C);

   function Unlock_Distributed_Mutex_C
      (M    : in Distributed_Mutex_Id;
       Prio : in RTEP.Priority) return MaRTE.Integer_Types.Int;
   pragma Export (C, Unlock_Distributed_Mutex_C, "rtep_unlock_distributed_mutex");
   pragma Inline (Unlock_Distributed_Mutex_C);

   function Finalize_Distributed_Mutex_C
      (M : in Distributed_Mutex_Id) return MaRTE.Integer_Types.Int;
   pragma Export
      (C, Finalize_Distributed_Mutex_C, "rtep_finalize_distributed_mutex");
   pragma Inline (Finalize_Distributed_Mutex_C);

   function Valid_Multicast_Id_C (Id : Station_ID)
      return MaRTE.Integer_Types.Int;
   pragma Export (C, Valid_Multicast_Id_C, "rtep_valid_multicast_id");
   pragma Inline (Valid_Multicast_Id_C);

end RTEP.Protocol.C_Interface;
