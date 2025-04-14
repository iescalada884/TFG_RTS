with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Ada.Streams;
with Ada.Exceptions;

with RTEP;
with RTEP.Protocol;
with RTEP.Protocol.Servers;
with RtEp.Protocol.Core;
with RTEP_Streams;

procedure test_rtep_initialization is

   package RP  renames RTEP.Protocol;

   Source_Station_ID : RTEP.Station_ID;
   Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
   Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
   Rx_Priority : RTEP.Priority;

begin
   --  Enqueue a msg before initialization
--    RP.Send_Info
--       (Destination_Station_ID   => RP.Get_Station_ID_By_Name("broadcast"),
--        Channel_ID               => 5,
--        Data => RTEP_Streams.To_Stream_Element_Array ("enqueued msg"),
--        Data_Priority            => 9);
--    delay 5.0;
   --  Init_Main_Task
   Put_Line ("Calling init comm!!");
   RP.Core.Init_Comm;
   Put_Line ("Initialized!!");
   --  Wait in channel 5
--    RP.Recv_Info
--       (Source_Station_ID => Source_Station_ID,
--          Channel_ID        => 5,
--          Data              => Rx_Info,
--          Last              => Rx_Info_Last,
--          Data_Priority     => Rx_Priority);
--    Put ("RECEIV: "&RTEP_Streams.To_String (Rx_Info (1 .. Rx_Info_Last)));
--    Put ("  FROM: "&RTEP.Station_ID'Image (Source_Station_ID));
--    Put_Line ("  WITH PRIO: "&RTEP.Priority'Image (Rx_Priority));

   delay 30.0;

exception
   when E : others =>
      Put_Line ("ERROR test_rtep_initialization");
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end test_rtep_initialization;
