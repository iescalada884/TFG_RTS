with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Ada.Streams;

with RTEP;
with RTEP.Protocol;
with RTEP.Protocol.Servers;
with RtEp.Protocol.Core;
with RTEP_Streams;

procedure test_rtep_messages is

   package RP  renames RTEP.Protocol;

   procedure Job_1 is
   begin
      Put_Line ("Job_1");
      loop
         RP.Send_Info
            (Destination_Station_ID   => 2,
             Channel_ID               => 10,
             Data => RTEP_Streams.To_Stream_Element_Array ("Master_Msg"),
             Data_Priority            => 7);
         RP.Send_Info
            (Destination_Station_ID   => RP.Get_Station_ID_By_Name("broadcast"),
             Channel_ID               => 5,
             Data => RTEP_Streams.To_Stream_Element_Array ("Master_Broadcast"),
             Data_Priority            => 9);
         RP.Send_Info
            (Destination_Station_ID   => RP.Get_Station_ID_By_Name("slaves"),
             Channel_ID               => 6,
             Data => RTEP_Streams.To_Stream_Element_Array ("Master_to_Slaves"),
             Data_Priority            => 15);
         delay 5.0;
      end loop;
   end Job_1;

   procedure Job_2 is
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Priority : RTEP.Priority;
   begin
      Put_Line ("Job_2");
      loop
         --  Wait in channel 10
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 10,
             Data              => Rx_Info,
             Last              => Rx_Info_Last,
             Data_Priority     => Rx_Priority);
         Put ("RECEIV: "&RTEP_Streams.To_String (Rx_Info (1 .. Rx_Info_Last)));
         Put ("  FROM: "&RTEP.Station_ID'Image (Source_Station_ID));
         Put_Line ("  WITH PRIO: "&RTEP.Priority'Image (Rx_Priority));
         --  Wait in channel 5
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
             Channel_ID        => 5,
             Data              => Rx_Info,
             Last              => Rx_Info_Last,
             Data_Priority     => Rx_Priority);
         Put ("RECEIV: "&RTEP_Streams.To_String (Rx_Info (1 .. Rx_Info_Last)));
         Put ("  FROM: "&RTEP.Station_ID'Image (Source_Station_ID));
         Put_Line ("  WITH PRIO: "&RTEP.Priority'Image (Rx_Priority));
         --  Wait in channel 6
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 6,
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
end test_rtep_messages;
