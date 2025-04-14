with MaRTE_OS;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.HAL.IO; use MaRTE.HAL.IO;
with Text_IO;
procedure Prueba is
begin
   Text_IO.Put_Line ("Prog 2");
   --    : se almacena en Dir IO $43 el c�digo $B6
   MaRTE.HAL.IO.Outb(16#43#,2#10_11_011_0#);
   --    : se establece el bit 0 del registro $61.
   MaRTE.HAL.IO.Outb(16#61#,
                     (MaRTE.HAL.IO.Inb(16#61#) or 2#0000_0001#) and
                     2#1111_1101#);
   -- Instancia la tarea PlayController.

   MaRTE.HAL.IO.Outb(16#42#, 0);
   MaRTE.HAL.IO.Outb(16#42#, 16#20#);
   MaRTE.HAL.IO.Outb(16#61#, 16#FF#);
   loop
      delay 1.0;
      Text_IO.Put_Line ("Prog 1");
   end loop;
end Prueba;
