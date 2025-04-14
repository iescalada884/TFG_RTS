with MaRTE_OS;
with Vga_Marte;
with Panel;

procedure Test_Panel_Lamp is
   L_Ref : Panel.Luz_Ref;
begin
   Vga_Marte.Vga_Rectangle_Fill((0,0),(639,479),1);
   Panel.Puntero.Inicializa ;

   L_Ref := new Panel.Luz;
   Panel.Configurar (Elemento                 => L_Ref,
                     Esquina_Superior_Izda    => (50, 50),
                     Esquina_Inferior_Dcha    => (100, 100),
                     Color_De_Marco           => Panel.Negro,
                     Etiqueta                 => Panel.Texto_Vacio,
                     Posicion_Etiqueta        => (75, 80),
                     Color_Etiqueta           => Panel.Negro,
                     Color_Fondo_Etiqueta     => Panel.Blanco,
                     Identificador            => 1,
                     Color_Elemento           => Panel.Verde,
                     Color_Elemento_Seleccion => Panel.Verde_Claro);

   Panel.Dibujar (L_Ref.all);

   loop
      Panel.Seleccionar(L_Ref);
      delay 1.0;
      Panel.Anular_Seleccion (L_Ref);
      delay 1.0;
   end loop;

end Test_Panel_Lamp;