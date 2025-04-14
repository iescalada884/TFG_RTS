with MaRTE_OS;
with Vga_Marte;
with Panel;

procedure Test_Panel_Init is
begin
   Vga_Marte.Vga_Rectangle_Fill((0,0),(639,479),1);
   Panel.Puntero.Inicializa ;

   loop
      null;
      delay 30.0;
   end loop;
end Test_Panel_Init;