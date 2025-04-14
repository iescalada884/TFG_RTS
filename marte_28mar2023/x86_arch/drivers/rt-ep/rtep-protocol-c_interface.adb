with MaRTE.Integer_Types;
with Ada.Real_Time;
with Ada.Streams; use Ada.Streams;

with RTEP.Protocol.Servers;
with RTEP.Protocol.Stations;
with RTEP_Debug_Functions; use RTEP_Debug_Functions;

package body RTEP.Protocol.C_Interface  is

   use type MaRTE.Integer_Types.Int; -- Mario

   procedure Send_Info_C
     (Destination_Station_ID : in Station_ID;
      Channel_ID             : in Channel;
      Data                   : in Buffer_Ac;
      Size                   : in MaRTE.Integer_Types.Size_T;
      Data_Priority          : in Priority)
   is
      subtype Stream_Data is
         Stream_Element_Array (1 .. Stream_Element_Offset (Size));
      type Stream_Data_Ac is access all Stream_Data;
      function To_Stream_Data_Ac is
         new Ada.Unchecked_Conversion (Buffer_Ac, Stream_Data_Ac);
   begin
      DEBUG ("C >> Send_Info_C "&
             Integer'Image(Integer(To_Stream_Data_Ac(Data).all'Last))&
             " bytes, to "&Station_ID'Image (Destination_Station_ID)&
             " channel "&Channel'Image(Channel_ID)&
             " prio "&Priority'Image(Data_Priority),
             Enable_DEBUG_C_Interface'First);
      Send_Info (Destination_Station_ID,
                 Channel_ID,
                 To_Stream_Data_Ac (Data).all,
                 Data_Priority);
   end Send_Info_C;

   procedure Send_Info_Server_C
     (Destination_Station_ID : in Station_ID;
      Channel_ID             : in Channel;
      Data                   : in Buffer_Ac;
      Size                   : in MaRTE.Integer_Types.Size_T;
      Id                     : in Server_ID;
      Blocking               : in Boolean)
   is
      subtype Stream_Data is
         Stream_Element_Array (1 .. Stream_Element_Offset (Size));
      type Stream_Data_Ac is access all Stream_Data;
      function To_Stream_Data_Ac is
         new Ada.Unchecked_Conversion (Buffer_Ac, Stream_Data_Ac);
   begin
      DEBUG ("C >> Send_Info_Server_C "&
             Integer'Image(Integer(To_Stream_Data_Ac(Data).all'Last))&
             " bytes, to "&Station_ID'Image (Destination_Station_ID)&
             " channel "&Channel'Image(Channel_ID)&
             " server "&Integer'Image (Integer(Id)),
             Enable_DEBUG_C_Interface'First);
      Send_Info
        (Destination_Station_ID,
         Channel_ID,
         To_Stream_Data_Ac (Data).all,
         Id,
         Blocking);
   end Send_Info_Server_C;

   procedure Recv_Info_C
     (Source_Station_ID : out Station_ID;
      Channel_ID        : in Channel;
      Data              : in Buffer_Ac;
      Size              : in MaRTE.Integer_Types.Size_T;
      Last              : out MaRTE.Integer_Types.Size_T;
      Data_Priority     : out Priority)
   is
      subtype Stream_Data is
         Stream_Element_Array (1 .. Stream_Element_Offset (Size));
      type Stream_Data_Ac is access all Stream_Data;
      function To_Stream_Data_Ac is
         new Ada.Unchecked_Conversion (Buffer_Ac, Stream_Data_Ac);
      Stream_Last : Stream_Element_Offset;
   begin
      DEBUG ("C >> blocking to receive on channel "&Channel'Image(Channel_ID),
             Enable_DEBUG_C_Interface'First);
      Recv_Info
         (Source_Station_ID,
          Channel_ID,
          To_Stream_Data_Ac (Data).all,
          Stream_Last,
          Data_Priority);
      Last := MaRTE.Integer_Types.Size_T (Stream_Last);
      DEBUG ("C >> received "&MaRTE.Integer_Types.Size_T'Image(Last)&
             " bytes, from "&Station_ID'Image (Source_Station_ID)&
             " channel "&Channel'Image(Channel_ID),
             Enable_DEBUG_C_Interface'First);
   end Recv_Info_C;

   function Get_Station_ID_By_Name_C
      (Station_Name : in Buffer_Ac;
       Length       : in MaRTE.Integer_Types.Size_T) return Station_ID
   is
      subtype String_Station_Name is String (1 .. Integer (Length));
      type String_Station_Name_Ac is access all String_Station_Name;
      function To_String_Station_Name_Ac is
         new Ada.Unchecked_Conversion (Buffer_Ac, String_Station_Name_Ac);
   begin
      return Get_Station_ID_By_Name
         (To_String_Station_Name_Ac (Station_Name).all);
   end Get_Station_ID_By_Name_C;

   function Create_Server_C
      (Max_Allocated_Budget : in  Network_Budget;
       Period_Sec           : in  MaRTE.Integer_Types.Int;
       Period_Nsec          : in  MaRTE.Integer_Types.Int;
       Server_Priority      : in  RTEP.Priority;
       Id                   : access Server_Id)
      return MaRTE.Integer_Types.Int
   is
      Server_Period : Duration :=
         Duration (Period_Sec) + Duration (Period_Nsec) / 10#1#E9;
   begin
      RTEP.Protocol.Servers.Create_Server
         (Max_Allocated_Budget,
          Ada.Real_Time.To_Time_Span (Server_Period),
          Server_Priority,
          Id.all);
      DEBUG ("C >> Max_Allocated_Budget: "
         &Network_Budget'Image (Max_Allocated_Budget),
         Enable_DEBUG_C_Interface'First);
      DEBUG ("timespec1: "
         &MaRTE.Integer_Types.Int'Image (Period_Sec),
         Enable_DEBUG_C_Interface'First);
      DEBUG ("timespec2: "
         &MaRTE.Integer_Types.Int'Image (Period_Nsec),
         Enable_DEBUG_C_Interface'First);
      DEBUG ("Server_Period: "
         &Duration'Image
            (Duration (Period_Sec) + Duration (Period_Nsec) / 10#1#E9),
            Enable_DEBUG_C_Interface'First);
      DEBUG ("Server_Priority: "&RTEP.Priority'Image (Server_Priority),
         Enable_DEBUG_C_Interface'First);
      DEBUG ("Server: "&Server_ID'Image (Id.all),
             Enable_DEBUG_C_Interface'First);
      return 0;
   exception
      when others =>
         ERROR ("Create_Server_C");
         return -1;
   end Create_Server_C;

   function Init_Distributed_Mutex_C
      (M : in Distributed_Mutex_Id) return MaRTE.Integer_Types.Int is
   begin
      RTEP.Protocol.Init_Distributed_Mutex (M);
      return 0;
   exception
      when others =>
         return -1;
   end Init_Distributed_Mutex_C;

   function Lock_Distributed_Mutex_C
      (M    : in Distributed_Mutex_Id;
       Prio : in RTEP.Priority) return MaRTE.Integer_Types.Int is
   begin
      RTEP.Protocol.Lock_Distributed_Mutex (M, Prio);
      return 0;
   exception
      when others =>
         ERROR ("Lock Distributed Mutex C Interface");
         return -1;
   end Lock_Distributed_Mutex_C;

   function Unlock_Distributed_Mutex_C
      (M    : in Distributed_Mutex_Id;
       Prio : in RTEP.Priority) return MaRTE.Integer_Types.Int is
   begin
      RTEP.Protocol.Unlock_Distributed_Mutex (M, Prio);
      return 0;
   exception
      when others =>
         ERROR ("Unlock Distributed Mutex C Interface");
         return -1;
   end Unlock_Distributed_Mutex_C;

   function Finalize_Distributed_Mutex_C
      (M : in Distributed_Mutex_Id) return MaRTE.Integer_Types.Int is
   begin
      RTEP.Protocol.Finalize_Distributed_Mutex (M);
      return 0;
   exception
      when others =>
         ERROR ("Finalize Distributed Mutex C Interface");
         return -1;
   end Finalize_Distributed_Mutex_C;

   function Valid_Multicast_Id_C (Id : Station_ID)
      return MaRTE.Integer_Types.Int
   is
      Valid : Boolean;
   begin
      Valid := RTEP.Protocol.Stations.Valid_Multicast_Id (Id);
      if Valid then
         return 1;
      else
         return 0;
      end if;
   exception
      when others =>
         return 0;
   end Valid_Multicast_Id_C;

end RTEP.Protocol.C_Interface;
