with Ada.Text_IO; use Ada.Text_IO;

with Tipos_Can_Bus;
with Can_Bus;

procedure Test_Can_Times_Rx is

   The_Handle  : Tipos_Can_Bus.Handle;	-- Manejador de la interfaz abierta.
   CAN_Network : constant Natural := 0;	-- Identificador del puerto can a utilizar.

   -- Tasa de kbps por defecto a la que funcionara el bus CAN.
   Baud : constant Tipos_Can_Bus.Baudrate := Tipos_Can_Bus.b1000;

begin
   Put_Line ("** Test CAN BUS times from Ada (RX) **");
   The_Handle := Can_Bus.Open (Network => CAN_Network, The_Baudrate => Baud);

   --  Wait for data (default receive hook)
   delay 1000.0;

end Test_Can_Times_Rx;
