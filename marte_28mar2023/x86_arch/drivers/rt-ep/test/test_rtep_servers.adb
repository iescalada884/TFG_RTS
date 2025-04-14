with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Real_Time;        use Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Interfaces;

with RTEP;
with RTEP.Protocol.Servers;

procedure Test_RTEP_Servers is

   package RPS renames RTEP.Protocol.Servers;

   procedure Pause is
      H : Character;
   begin
      Put("Debug: Pause...");
      Get_Immediate(H);
      New_Line;
   end Pause;

   function Server_Id_To_Unsigned_16 is
      new Ada.Unchecked_Conversion (RTEP.Server_Id, Interfaces.Unsigned_16);

   Servers_Ids : array (Integer range 1 .. 6) of RTEP.Server_Id;

   procedure Show_Server_Info (Id : in RTEP.Server_Id) is
       Max_Allocated_Budget : RTEP.Network_Budget;
       Server_Period        : Ada.Real_Time.Time_Span;
       Server_Priority      : RTEP.Priority;
       Server_Unchecked     : Interfaces.Unsigned_16 :=
          Server_Id_To_Unsigned_16 (Id);
   begin
      RPS.Get_Server_Info
         (Id, Max_Allocated_Budget, Server_Period, Server_Priority);
      Put_Line("Server: "&Interfaces.Unsigned_16'Image(Server_Unchecked)&" ->"&
               " C="&RTEP.Network_Budget'Image(Max_Allocated_Budget)&
               " T="&Float'Image(Float(To_Duration(Server_Period)))&
               " P="&RTEP.Priority'Image(Server_Priority));
   exception
      when RPS.Inexistent =>
         Put_Line
            ("Server: "&Interfaces.Unsigned_16'Image(Server_Unchecked)&" ---");
   end Show_Server_Info;

   procedure Show_All is
   begin
      for I in Servers_Ids'Range loop
         Show_Server_Info (Servers_Ids (I));
      end loop;
      Put_Line("-------------------");
   end Show_All;

   procedure Show_Budget_And_Pending (Id : in RTEP.Server_Id) is
      Server_Unchecked : Interfaces.Unsigned_16 := Server_Id_To_Unsigned_16(Id);
   begin
      Put_Line("Server:"&Interfaces.Unsigned_16'Image(Server_Unchecked)&
               " Budget = "&
               RTEP.Network_Budget'Image (RPS.Get_Current_Budget (Id))&
               " Packets Pending = "&
               RTEP.Network_Budget'Image (RPS.Get_Packets_Pending (Id)));
   end Show_Budget_And_Pending;

   procedure Show_Highest_Prio_Server is
      High_Prio_Server   : RTEP.Server_Id;
      Highest_Prio       : RTEP.Priority;
      Are_Active_Servers : Boolean;
      Server_Unchecked : Interfaces.Unsigned_16;
   begin
      RPS.Get_Max_Priority_Server
         (High_Prio_Server, Highest_Prio, Are_Active_Servers);
      if Are_Active_Servers then
         Server_Unchecked := Server_Id_To_Unsigned_16(High_Prio_Server);
         Put_Line ("High_Prio_Server:"&
            Interfaces.Unsigned_16'Image(Server_Unchecked));
         Put_Line ("Highest_Prio: "&RTEP.Priority'Image(Highest_Prio));
      else
         Put_Line ("No Active Servers");
      end if;
   end Show_Highest_Prio_Server;

   procedure Enqueue_Packets (Id     : in RTEP.Server_Id;
                              Amount : in Integer)
   is
   begin
      for I in 1 .. Amount loop
         RPS.Packet_Enqueued (Id);
      end loop;
      Put_Line ("Enqueued: "&Integer'Image (Amount));
   end Enqueue_Packets;

   procedure Send_Packets (Id     : in RTEP.Server_Id;
                           Amount : in Integer)
   is
   begin
      for I in 1 .. Amount loop
         RPS.Packet_Sent (Id, Clock);
      end loop;
      Put_Line ("Sent: "&Integer'Image (Amount));
   end Send_Packets;

   Max_Budget : RTEP.Network_Budget;

   use type RTEP.Network_Budget;

begin
   Put_Line ("Add six elements");
   Max_Budget := 1;
   for I in 1 .. 6 loop
      RPS.Create_Server
         (Max_Allocated_Budget => Max_Budget,
          Server_Period        => Microseconds(200_000),
          Server_Priority      => 7,
          Id                   => Servers_Ids (I));
      Max_Budget := Max_Budget + 1;
   end loop;
   Show_All;
   Pause;

   Put_Line ("Delete server number 4");
   RPS.Delete_Server (Servers_Ids (4));
   Show_All;
   Pause;

   Show_Budget_And_Pending (Servers_Ids (3));
   Enqueue_Packets (Servers_Ids(3), 4);
   Show_Budget_And_Pending (Servers_Ids (3));
   Send_Packets (Servers_Ids(3), 2);
   Show_Budget_And_Pending (Servers_Ids (3));
   delay(0.01);

   Show_Highest_Prio_Server;

   Show_Budget_And_Pending (Servers_Ids (3));
   Enqueue_Packets (Servers_Ids(3), 4);
   Show_Budget_And_Pending (Servers_Ids (3));
   Send_Packets (Servers_Ids(3), 2);
   Show_Budget_And_Pending (Servers_Ids (3));
   delay(0.01);

   Show_Highest_Prio_Server;

   Show_Budget_And_Pending (Servers_Ids (3));
   Enqueue_Packets (Servers_Ids(3), 4);
   Show_Budget_And_Pending (Servers_Ids (3));
   Send_Packets (Servers_Ids(3), 2);
   Show_Budget_And_Pending (Servers_Ids (3));
   delay(0.01);

   for I in 1 .. 2 loop
      Show_Highest_Prio_Server;

      Show_Budget_And_Pending (Servers_Ids (3));
      Enqueue_Packets (Servers_Ids(3), 4);
      Show_Budget_And_Pending (Servers_Ids (3));
      Send_Packets (Servers_Ids(3), 2);
      Show_Budget_And_Pending (Servers_Ids (3));
      delay(1.0);
   end loop;

end Test_RTEP_Servers;
