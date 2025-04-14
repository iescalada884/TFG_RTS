with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Ada.Streams;

with RTEP;
with RTEP.Protocol;
with RTEP.Protocol.Servers;
with RtEp.Protocol.Core;
with RTEP_Streams;

procedure test_rtep_server_messages is

   package RP  renames RTEP.Protocol;
   package RPS renames RTEP.Protocol.Servers;

   pragma Priority (10);

   procedure Job_1 is
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Priority : RTEP.Priority;
   begin
      Put_Line ("Job_1");
      loop
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 5,
             Data              => Rx_Info,
             Last              => Rx_Info_Last,
             Data_Priority     => Rx_Priority);
         Put ("RECEIV: "&RTEP_Streams.To_String (Rx_Info (1 .. Rx_Info_Last)));
         Put ("  FROM: "&RTEP.Station_ID'Image (Source_Station_ID));
         Put_Line ("  WITH PRIO: "&RTEP.Priority'Image (Rx_Priority));
      end loop;
   end Job_1;

   procedure Job_2 is
      Low_Prio : RTEP.Priority := 4;
      High_Prio : RTEP.Priority := 15;
      S_Id : RTEP.Server_Id;
      C : RTEP.Network_Budget := 2; -- packets
      T : Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (10.0); -- secs
   begin
      Put_Line ("Job_2");
      RPS.Create_Server
         (Max_Allocated_Budget => C,
          Server_Period        => T,
          Server_Priority      => High_Prio,
          Id                   => S_Id);
      loop
         Put_Line ("Send!");
         --  Server Msg
         RP.Send_Info
            (Destination_Station_ID => 3,
               Channel_ID           => 5,
               Data => RTEP_Streams.To_Stream_Element_Array
                        ("Server_Msg_Number_1"),
               Id   => S_Id);
         --  Server Msg
         RP.Send_Info
            (Destination_Station_ID => 3,
               Channel_ID           => 5,
               Data => RTEP_Streams.To_Stream_Element_Array
                        ("Server_Msg_Number_2"),
               Id   => S_Id);
         --  Server Broadcast Msg
         RP.Send_Info
            (Destination_Station_ID   => RP.Get_Station_ID_By_Name("broadcast"),
             Channel_ID               => 5,
             Data => RTEP_Streams.To_Stream_Element_Array ("Master_Broadcast"),
             Id   => S_Id);
         --  Low Prio Msg
         RP.Send_Info
            (Destination_Station_ID   => 3,
             Channel_ID               => 5,
             Data => RTEP_Streams.To_Stream_Element_Array ("Low_Prio_Msg"),
             Data_Priority            => Low_Prio);
         delay 6.0;
      end loop;
      RPS.Delete_Server (S_Id);
   end Job_2;

   procedure Job_3 is
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Priority : RTEP.Priority;
   begin
      Put_Line ("Job_3");
      loop
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 5,
             Data              => Rx_Info,
             Last              => Rx_Info_Last,
             Data_Priority     => Rx_Priority);
         Put ("RECEIV: "&RTEP_Streams.To_String (Rx_Info (1 .. Rx_Info_Last)));
         Put ("  FROM: "&RTEP.Station_ID'Image (Source_Station_ID));
         Put_Line ("  WITH PRIO: "&RTEP.Priority'Image (Rx_Priority));
      end loop;
   end Job_3;

begin
  RP.Core.Init_Comm;  -- Init_Main_Task;
  case RP.Get_Station_ID is
    when 1 =>
       Job_1;
    when 2 =>
       Job_2;
    when 3 =>
       Job_3;
    when others =>
      Put_Line ("ERROR check your ring configuration");
  end case;
end test_rtep_server_messages;
