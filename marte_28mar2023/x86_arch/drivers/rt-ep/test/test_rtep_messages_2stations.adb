with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Ada.Streams;
with Ada.Exceptions;

with RTEP;
with RTEP.Protocol;
with RTEP.Protocol.Core;
with RTEP_Streams;

procedure test_rtep_messages_2stations is

   package RP  renames RTEP.Protocol;

   pragma Priority (2);

   procedure Job_1 is
      I : Integer := 0;
   begin
      Put_Line ("Job_1: Sender");
      loop
         RP.Send_Info
            (Destination_Station_ID   => 2,
             Channel_ID               => 10,
             Data => RTEP_Streams.To_Stream_Element_Array
               ("Master_Msg "&Integer'Image(I)),
             Data_Priority            => 7);
         I := I + 1;
         delay 0.1;
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
         Put ("RECEIV: "&RTEP_Streams.To_String (Rx_Info (1 .. Rx_Info_Last)));
         Put ("  FROM: "&RTEP.Station_ID'Image (Source_Station_ID));
         Put_Line ("  WITH PRIO: "&RTEP.Priority'Image (Rx_Priority));
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
      Put_Line ("This station has no job");
      delay 1000.0;
  end case;
exception
   when E : others =>
      Put_Line ("ERROR test_rtep_messages_2stations");
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end test_rtep_messages_2stations;
