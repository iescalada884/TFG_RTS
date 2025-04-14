with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Ada.Streams;

with RTEP;
with RTEP.Protocol;
with RTEP.Protocol.Servers;
with RtEp.Protocol.Core;
with RTEP_Streams;
with Time_Measure;
with Var_Strings;

with Debug_Marte; use Debug_Marte; -- For Debugging


procedure test_rtep_server_messages_measurement is

   pragma Priority (5);

   package RP  renames RTEP.Protocol;
   package RPS renames RTEP.Protocol.Servers;

   procedure Job_1 is
      --  Receiving variables
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Priority : RTEP.Priority;
      --  Sending variables
      High_Prio : RTEP.Priority := 15;
      S_Id : RTEP.Server_Id;
      C : RTEP.Network_Budget := 1; -- packets
      T : Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (0.5); -- secs
   begin
      Put_Line ("Job_1");
      RPS.Create_Server
         (Max_Allocated_Budget => C,
          Server_Period        => T,
          Server_Priority      => High_Prio,
          Id                   => S_Id);
      loop
         --  Wait MSG
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 5,
             Data              => Rx_Info,
             Last              => Rx_Info_Last,
             Data_Priority     => Rx_Priority);
         --  Send ANSWER
         RP.Send_Info
            (Destination_Station_ID => Source_Station_ID,
             Channel_ID             => 5,
             Data                   => Rx_Info (1 .. Rx_Info_Last),
             Id                     => S_Id);
      end loop;
   end Job_1;

   procedure Job_2 is
      --  Receiving variables
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Priority : RTEP.Priority;
      --  Sending variables
      High_Prio : RTEP.Priority := 15;
      S_Id : RTEP.Server_Id;
      C : RTEP.Network_Budget := 1; -- packets
      T : Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (0.1); -- secs
      Go_And_Return_ID  : Time_Measure.Measure_ID;
   begin
      Put_Line ("Job_2");

      Go_And_Return_ID :=
        Time_Measure.Init_Time_Measure
        (Var_Strings.To_Var_String ("Test_Server_Msgs:Go_Return_Time"),
         Time_Measure.ABS_Time_Clock);

      RPS.Create_Server
         (Max_Allocated_Budget => C,
          Server_Period        => T,
          Server_Priority      => High_Prio,
          Id                   => S_Id);

      for I in 1 .. 100000 loop
         Put_Line ("Send Msg "&Integer'Image (I));
         Time_Measure.Set_Time_Mark (Go_And_Return_ID, Time_Measure.First);
         --  Send Msg
         RP.Send_Info
            (Destination_Station_ID => 1,
             Channel_ID             => 5,
             Data => RTEP_Streams.To_Stream_Element_Array
               ("Msg "&Integer'Image (I)),
             Id   => S_Id);
         --  Wait answer
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 5,
             Data              => Rx_Info,
             Last              => Rx_Info_Last,
             Data_Priority     => Rx_Priority);
         Time_Measure.Set_Time_Mark (Go_And_Return_ID, Time_Measure.Last);
         Put ("RECEIV: "&RTEP_Streams.To_String (Rx_Info (1 .. Rx_Info_Last)));
         Put ("  FROM: "&RTEP.Station_ID'Image (Source_Station_ID));
         Put_Line ("  WITH PRIO: "&RTEP.Priority'Image (Rx_Priority));
         delay 0.5;
      end loop;
      RPS.Delete_Server (S_Id);
   end Job_2;

begin
  RP.Core.Init_Comm;  -- Init_Main_Task;

--    Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
--    Debug_Marte.Set_Break_Point_Here;

  case RP.Get_Station_ID is
    when 1 =>
       Job_1;
    when 2 =>
       Job_2;
    when others =>
      Put_Line ("ERROR check your ring configuration");
  end case;
end test_rtep_server_messages_measurement;
