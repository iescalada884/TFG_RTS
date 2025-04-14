with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Tipos_Can_Bus;
with Can_Bus;

procedure Test_Can_Times_Tx is

   Next_Activation : Ada.Real_Time.Time;
   Period          : Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (5.000);

   Key             : String (1 .. 5);
   KeySize         : Natural := 5;

   The_Handle  : Tipos_Can_Bus.Handle;	-- Manejador de la interfaz abierta.
   CAN_Network : constant Natural := 0;	-- Identificador del puerto can a utilizar.

   -- Tasa de kbps por defecto a la que funcionara el bus CAN.
   Baud : constant Tipos_Can_Bus.Baudrate := Tipos_Can_Bus.b1000;

   The_Message           : Tipos_Can_Bus.Can_Message;
   Id                    : Tipos_Can_Bus.Can_Id := 16#601#;

begin

   Put_Line ("** Test CAN BUS times from Ada **");

   --  Heading
   The_Message.Id     := Id;
   The_Message.Length := 8;
   --  Data
   The_Message.Message (1) := 1;
   The_Message.Message (2) := 2;
   The_Message.Message (3) := 3;
   The_Message.Message (4) := 4;
   The_Message.Message (5) := 5;
   The_Message.Message (6) := 6;
   The_Message.Message (7) := 7;
   The_Message.Message (8) := 8;

   --  Open device file and get file descriptor
   Put_Line ("** Open CAN device: Press a key to continue **");
   The_Handle := Can_Bus.Open (Network => CAN_Network, The_Baudrate => Baud);
   Ada.Text_IO.Get_Line (Key, KeySize);

   Next_Activation := Clock;

   loop
      delay until Next_Activation;
      --  Write on device file
      Can_Bus.Send (The_Handle, The_Message);
      Next_Activation := Next_Activation + Period;
   end loop;

end;
