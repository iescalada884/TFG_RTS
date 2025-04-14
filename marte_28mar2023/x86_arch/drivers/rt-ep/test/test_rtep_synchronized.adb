with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Ada.Streams;

with RTEP;
with RTEP.Protocol;
with RTEP.Protocol.Servers;
with RtEp.Protocol.Core;
with RTEP_Streams;

procedure test_rtep_synchronized is

   package RP  renames RTEP.Protocol;
   package RPS renames RTEP.Protocol.Servers;

   Low_Prio  : RTEP.Priority := 4;
   High_Prio : RTEP.Priority := 15;

   procedure Job_1 is
   begin
      Put_Line ("Job_1");
      loop
         delay 5.0;
      end loop;
   end Job_1;

   procedure Job_2 is
      S_Id : RTEP.Server_Id;
      C : RTEP.Network_Budget := 2; -- packets
      T : Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (10.0); -- secs
   begin
      Put_Line ("Job_2");
      RPS.Create_Server
         (Max_Allocated_Budget => C,
          Server_Period        => T,
          Server_Priority      => Low_Prio,
          Id                   => S_Id);
      loop
         Put_Line ("Send 5 High prio messages non blocking");
         for i in 1 .. 5 loop
            RP.Send_Info
              (Destination_Station_ID   => 3,
               Channel_ID               => 10,
               Data => RTEP_Streams.To_Stream_Element_Array ("Non_Sync_Msg"),
               Data_Priority            => High_Prio);
         end loop;
         Put_Line ("Send 1 Low prio message blocking");
         RP.Send_Info
            (Destination_Station_ID => 3,
             Channel_ID             => 10,
             Data => RTEP_Streams.To_Stream_Element_Array ("Sync_Msg"),
             Id   => S_Id,
             Blocking => True);
         Put_Line ("Sync_Msg sent!");
         delay 10.0;
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
         --  Wait in channel 5
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 10,
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
end test_rtep_synchronized;
