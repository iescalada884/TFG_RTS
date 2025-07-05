--
-- Ejemplo de uso de un LED en la placa STM32F407G-DISC1.
--
-- Este ejemplo enciende y apaga el LED azul, ubicado entre los dos botones de la placa.
-- Utiliza el paquete `LEDs` para el control del hardware
-- y emplea una espera activa (busy-wait) para el retardo.
--

with LEDs;

procedure Blink_LED is

   procedure My_Delay (Iterations : Natural) is
   begin
      for I in 1 .. Iterations loop
         -- Ciclo vacío: espera activa
         null;
      end loop;
   end My_Delay;
   The_Delay : constant Natural := 10_000_000;

begin

   loop

      LEDs.On(LEDs.Blue);         -- Encender LED
      My_Delay (The_Delay);

      LEDs.Off(LEDs.Blue);        -- Apagar LED
      My_Delay (The_Delay);

   end loop;
end Blink_LED;
