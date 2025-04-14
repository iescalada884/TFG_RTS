with MaRTE.Integer_Types;

with Ada.Real_Time;
with Ada.Text_Io;
with Ada.Streams;
with Ada.Exceptions;

with RTEP;
with RTEP.Protocol;
with RTEP_Streams;
with RTEP_Bandwith_Reservation;
with MaRTE.Timespec;

with RTEP.Protocol.Servers;
with Interfaces;
with Ada.Unchecked_Conversion;

procedure Test_RTEP_BWRES_Renegotiate is

   package RP renames RTEP.Protocol;
   package RPS renames RTEP.Protocol.Servers;
   package BW renames RTEP_Bandwith_Reservation;

   use type MaRTE.Integer_Types.Int;

   pragma Priority (BW.Negotiation_Task_Prio - 1);

   function Server_Id_To_Unsigned_16 is
      new Ada.Unchecked_Conversion (RTEP.Server_Id, Interfaces.Unsigned_16);

   function Unsigned_16_To_Server_Id is
      new Ada.Unchecked_Conversion (Interfaces.Unsigned_16, RTEP.Server_Id);

   procedure Show_Server_Info (Id : in RTEP.Server_Id) is
       Max_Allocated_Budget : RTEP.Network_Budget;
       Server_Period        : Ada.Real_Time.Time_Span;
       Server_Priority      : RTEP.Priority;
       Server_Unchecked     : Interfaces.Unsigned_16 :=
          Server_Id_To_Unsigned_16 (Id);
   begin
      RPS.Get_Server_Info
         (Id, Max_Allocated_Budget, Server_Period, Server_Priority);
      Ada.Text_Io.Put_Line
            ("Server: "&Interfaces.Unsigned_16'Image(Server_Unchecked)&" ->"&
            " C="&RTEP.Network_Budget'Image(Max_Allocated_Budget)&
            -- " T="&Float'Image(Float(Duration(Server_Period)))&
            " P="&RTEP.Priority'Image(Server_Priority));
   exception
      when RPS.Inexistent =>
         Ada.Text_Io.Put_Line
            ("Server: "&Interfaces.Unsigned_16'Image(Server_Unchecked)&" ---");
   end Show_Server_Info;

   procedure Show_All is
   begin
      for I in Interfaces.Unsigned_16 range 1 .. 4 loop
         Show_Server_Info (Unsigned_16_To_Server_Id (I));
      end loop;
      Ada.Text_Io.Put_Line("-------------------");
   end Show_All;

   Unexpected_Error : exception;

   procedure Check_NZ (Result : MaRTE.Integer_Types.Int) is
   begin
      if Result /= 0 then
         Ada.Text_Io.Put_Line ("test_RTEP_BWRES error check_nz");
         raise Unexpected_Error;
      end if;
   end Check_NZ;
   pragma Inline (Check_NZ);

   Chan  : RTEP.Channel := 5;

   task rx is
      pragma Priority (BW.Negotiation_Task_Prio - 2);
   end rx;

   task body rx is
      Source_Station_ID : RTEP.Station_ID;
      Rx_Info : Ada.Streams.Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
      Rx_Info_Last: Ada.Streams.Stream_Element_Offset;
      Rx_Prio : RTEP.Priority;
   begin
      loop
         RP.Recv_Info
            (Source_Station_ID => Source_Station_ID,
            Channel_ID        => Chan,
            Data              => Rx_Info,
            Last              => Rx_Info_Last,
            Data_Priority     => Rx_Prio);
         Ada.Text_Io.Put ("RECEIV: "&RTEP_Streams.To_String
            (Rx_Info (1 .. Rx_Info_Last)));
         Ada.Text_Io.Put ("  FROM: "&RTEP.Station_ID'Image (Source_Station_ID));
         Ada.Text_Io.Put_Line ("  WITH PRIO: "&RTEP.Priority'Image (Rx_Prio));
      end loop;
   end rx;

   Contract   : aliased BW.RTEP_BWRES_Contract;
   Budget_Min : RTEP.Network_Budget := 2; -- packets
   Period_Max : Duration := 2.0;
   Deadline   : Duration := Period_Max;
   Prio       : RTEP.Priority := 8;
   Vres_Id    : BW.RTEP_BWRES_Virtual_Resource_Id;
   Accepted   : MaRTE.Integer_Types.Int;
   S_Id       : aliased RTEP.Server_Id;
   Dest_Id    : RTEP.Station_ID := RP.Get_Station_ID_By_Name("broadcast");
   Next_Time  : Ada.Real_Time.Time := Ada.Real_Time.Clock;
begin

   Check_NZ (BW.Init);

   Ada.Text_Io.Put_Line ("I am "&RTEP.Station_ID'Image (RP.Get_Station_ID));

   Check_NZ (BW.Set_Basic_Params
      (Contract'Access,
       Budget_Min,
       MaRTE.Timespec.To_Timespec (Period_Max),
       MaRTE.Timespec.To_Timespec (Deadline)));

   Check_NZ (BW.Set_Priority (Contract'Access, Prio));

   Vres_Id := BW.Create_Vres;

   delay 2.0;

   Accepted := BW.Negotiate_Contract (Contract, Vres_Id);

   if Accepted /= 0 then
      Ada.Text_Io.Put_Line ("Contract Not Accepted");
      return;
   end if;

   -- Ada.Text_Io.Put_Line ("Contract Accepted");
   Check_NZ (BW.Get_Server_Id (Vres_Id, S_Id'Access));

   Show_All;

   for I in 1 .. 10 loop
      Ada.Text_Io.Put_Line ("Sending info...");
      RP.Send_Info
      (Destination_Station_ID  => Dest_Id,
         Channel_ID              => Chan,
         Data => RTEP_Streams.To_Stream_Element_Array
            ("Packet "&Integer'Image (I)&" Part 1"),
         Id                      => S_Id,
         Blocking                => False);
      RP.Send_Info
      (Destination_Station_ID  => Dest_Id,
         Channel_ID              => Chan,
         Data => RTEP_Streams.To_Stream_Element_Array
            ("Packet "&Integer'Image (I)&" Part 2"),
         Id                      => S_Id,
         Blocking                => False);
      Next_Time := Ada.Real_Time."+"
         (Next_Time, Ada.Real_Time.To_Time_Span (Period_Max));
      delay until Next_Time;
   end loop;

   --  renegotiate contract
   Show_All;
   Ada.Text_Io.Put_Line ("Renegotiating CONTRACT...");

   Check_NZ (BW.Set_Basic_Params
      (Contract'Access,
       4,
       MaRTE.Timespec.To_Timespec (Period_Max+4.0),
       MaRTE.Timespec.To_Timespec (Deadline)));

   Accepted := BW.Renegotiate_Contract_Sync (Vres_Id, Contract);
   if Accepted /= 0 then
      Ada.Text_Io.Put_Line ("Renegotiation Not Accepted");
   else
      Ada.Text_Io.Put_Line ("Renegotiation Accepted");
   end if;
   Show_All;

   for I in 1 .. 10 loop
      Ada.Text_Io.Put_Line ("Sending info...");
      RP.Send_Info
      (Destination_Station_ID  => Dest_Id,
         Channel_ID              => Chan,
         Data => RTEP_Streams.To_Stream_Element_Array
            ("Packet "&Integer'Image (I)&" Part 1"),
         Id                      => S_Id,
         Blocking                => False);
      RP.Send_Info
      (Destination_Station_ID  => Dest_Id,
         Channel_ID              => Chan,
         Data => RTEP_Streams.To_Stream_Element_Array
            ("Packet "&Integer'Image (I)&" Part 2"),
         Id                      => S_Id,
         Blocking                => False);
      RP.Send_Info
      (Destination_Station_ID  => Dest_Id,
         Channel_ID              => Chan,
         Data => RTEP_Streams.To_Stream_Element_Array
            ("Packet "&Integer'Image (I)&" Part 3"),
         Id                      => S_Id,
         Blocking                => False);
      RP.Send_Info
      (Destination_Station_ID  => Dest_Id,
         Channel_ID              => Chan,
         Data => RTEP_Streams.To_Stream_Element_Array
            ("Packet "&Integer'Image (I)&" Part 4"),
         Id                      => S_Id,
         Blocking                => False);

      Next_Time := Ada.Real_Time."+"
         (Next_Time, Ada.Real_Time.To_Time_Span (Period_Max+4.0));
      delay until Next_Time;
   end loop;

   Ada.Text_IO.Put_Line("End of Test_RTEP_BWRES_Renegotiate");

exception
   when The_Error : others =>
      Ada.Text_IO.Put_Line("Unknown error:");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      raise;
end Test_RTEP_BWRES_Renegotiate;
