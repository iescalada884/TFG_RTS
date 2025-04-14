with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Ada.Streams;
with Ada.Exceptions;

with RTEP;
with RTEP.Protocol;
with RTEP.Protocol.Core;
with RTEP_Streams;
with Time_Measure;
with Var_Strings;

procedure test_rtep_messages_2stations_measurement is

   package RP  renames RTEP.Protocol;

   pragma Priority (2);

   procedure Job_1 is
      I : Integer := 0;
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Priority : RTEP.Priority;
      Go_And_Return_ID  : Time_Measure.Measure_ID;
   begin
      Put_Line ("Job_1: Sender");

      delay 5.0;

      Go_And_Return_ID :=
        Time_Measure.Init_Time_Measure
        (Var_Strings.To_Var_String ("Test_FP_Msgs:Go_Return_Time"),
         Time_Measure.ABS_Time_Clock);

      loop
         Time_Measure.Set_Time_Mark (Go_And_Return_ID, Time_Measure.First);

         RP.Send_Info
            (Destination_Station_ID   => 2,
             Channel_ID               => 10,
             Data => RTEP_Streams.To_Stream_Element_Array
               ("Master_Msg "&Integer'Image(I)),
             Data_Priority            => 7);
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 10,
             Data              => Rx_Info,
             Last              => Rx_Info_Last,
             Data_Priority     => Rx_Priority);

         Time_Measure.Set_Time_Mark (Go_And_Return_ID, Time_Measure.Last);

         Put_Line ("RECEIV: "&RTEP_Streams.To_String (Rx_Info (1 .. Rx_Info_Last)));
         I := I + 1;
         delay 1.5;
      end loop;
   end Job_1;

   procedure Job_2 is
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Priority : RTEP.Priority;
   begin
      Put_Line ("Job_2: Receiver");
      loop
         --  Wait in channel 10
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
             Channel_ID        => 10,
             Data              => Rx_Info,
             Last              => Rx_Info_Last,
             Data_Priority     => Rx_Priority);
         RP.Send_Info
            (Destination_Station_ID   => Source_Station_ID,
             Channel_ID               => 10,
             Data                     => Rx_Info (1 .. Rx_Info_Last),
             Data_Priority            => 7);
      end loop;
   end Job_2;

begin
  RP.Core.Init_Comm;  -- Init_Main_Task;
  case RP.Get_Station_ID is
    when 1 =>
       Job_1;
    when 2 =>
       Job_2;
    when others =>
      Put_Line ("ERROR check your ring configuration");
  end case;
exception
   when E : others =>
      Put_Line ("ERROR test_rtep_messages_2stations");
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end test_rtep_messages_2stations_measurement;
