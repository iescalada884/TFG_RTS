with MaRTE_OS;
with Vga_Marte;
with Panel;
with Interfaces.C.Strings;

procedure Test_Panel_Numeric_Display is
   D_Ref : Panel.Display_Numerico_Ref;
   N     : Panel.Tipo_Numero := 0;
begin
   Vga_Marte.Vga_Rectangle_Fill((0,0),(639,479),1);
   Panel.Puntero.Inicializa ;

   D_Ref := new Panel.Display_Numerico;

   Panel.Configurar
     (Elemento                => D_Ref,
      Esquina_Superior_Izda   => (50, 50),
      Color_De_Marco          => Panel.Negro,
      Etiqueta                => Interfaces.C.Strings.New_String("Numbers"),
      Posicion_Etiqueta       => (50, 100),
      Color_Etiqueta          => Panel.Negro,
      Color_Fondo_Etiqueta    => Panel.Blanco,
      Identificador           => 1,
      Cifra_Inicial           => N,
      Color_Display           => Panel.Negro,
      Color_Fondo_Display     => Panel.Verde_Claro,
      Esta_Encendido          => True,
      Numero_De_Digitos       => 2);

   Panel.Dibujar (D_Ref.all);

   loop
      delay 1.0;
      Panel.Poner_Cifra (D_Ref, N);
      N := N + 1;
   end loop;

end Test_Panel_Numeric_Display;
