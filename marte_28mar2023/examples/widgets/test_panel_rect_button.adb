with MaRTE_OS;
with Vga_Marte;
with Panel;
with Interfaces.C.Strings;

procedure Test_Panel_Rect_Button is
   B_Ref : Panel.Boton_Rectangular_Ref;

   Pressed_Text : constant Panel.Tipo_Texto :=
                                 Interfaces.C.Strings.New_String("Pressed");
   Released_Text : constant Panel.Tipo_Texto :=
                                 Interfaces.C.Strings.New_String("Released");

   procedure My_Button_Callback_Pressed (Codigo: Panel.Tipo_Id) is
   begin
      Vga_Marte.Vga_Text(Pressed_Text, (200, 100), Panel.Negro, Panel.Rojo);
   end My_Button_Callback_Pressed;

   procedure My_Button_Callback_Released (Codigo: Panel.Tipo_Id) is
   begin
      Vga_Marte.Vga_Text(Released_Text, (300, 100), Panel.Negro, Panel.Rojo);
   end My_Button_Callback_Released;

begin
   Vga_Marte.Vga_Rectangle_Fill((0,0),(639,479),1);
   Panel.Puntero.Inicializa;

   B_Ref := new Panel.Boton_Rectangular;

   Panel.Configurar
     (Elemento                 => B_Ref,
      Esquina_Superior_Izda    => (50, 50),
      Esquina_Inferior_Dcha    => (100, 100),
      Color_De_Marco           => Panel.Negro,
      Etiqueta                 => Panel.Texto_Vacio,
      Posicion_Etiqueta        => (100, 100),
      Color_Etiqueta           => Panel.Negro,
      Color_Fondo_Etiqueta     => Panel.Blanco,
      Identificador            => 1,
      Color_Elemento           => Panel.Verde,
      Color_Elemento_Seleccion => Panel.Verde_Claro,
      Color_Elemento_Presion   => Panel.Verde,
      Esta_Enclavado           => False,
      Tiene_Luz                => True,
      Procedimiento_Presion    => My_Button_Callback_Pressed'Unrestricted_Access,
      Procedimiento_Liberacion => My_Button_Callback_Released'Unrestricted_Access);

   Panel.Dibujar (B_Ref.all);

   loop
      null;
      delay 2.0;
   end loop;

end Test_Panel_Rect_Button;
