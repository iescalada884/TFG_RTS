with MaRTE_OS;
with POSIX_IO;
pragma Elaborate_All (POSIX_IO);

package body Panel is

   -----------------------------------------
   -- Cuerpo del Objeto Protegido Puntero --
   -----------------------------------------
   protected body Puntero is
      procedure Inicializa is
      begin
         Puntero := True;
      end Inicializa;

      function Esta_Inicializado return Boolean is
      begin
         return Puntero;
      end Esta_Inicializado;
   end Puntero;

   -------------------------------------------------
   -- Cuerpo del objeto protegido Matriz_Pantalla --
   -------------------------------------------------
   protected body Matriz_Pantalla is
      procedure Inicializa is
      begin
         for Fila in Pantalla'Range(1) loop
            for Columna in Pantalla'Range(2) loop
               Pantalla(Fila,Columna) := null;
            end loop;
         end loop;
      end Inicializa;

      function Celda_Ya_Asignada(F: in Fila; C: in Columna) return Boolean is
      begin
         return Pantalla(F,C) /= null;
      end Celda_Ya_Asignada;

      procedure Asigna(Ref: Boton_Ref;F: in Fila; C: in Columna) is
      begin
         Pantalla(F,C)  := Ref;
      end Asigna;

      procedure Elimina(F: in Fila;C: in Columna) is
      begin
         Pantalla(F,C)  := null;
      end Elimina;

      function Obtener_Celda(P: in Punto) return Boton_Ref is
         F: Fila;
         C: Columna;
      begin
         F := (P.Y)/10;
         c := (P.X)/10;
         return Pantalla(F,C);
      end Obtener_Celda;

   end Matriz_Pantalla;

   ------------------------------------------------------------------------
   -- Funciones Auxiliares para la gesti�n del puntero y eventos de rat�n.
   ------------------------------------------------------------------------
   procedure Copia_Zona (M: in out Matrix; P_Ini : in Punto) is
      Fila_Inicial      : Natural := M'First(1);
      Columna_Inicial   : Natural := M'First(2);
   begin
      for Fila in M'Range(1) loop
         for Columna in M'Range(2) loop
            M(Fila,Columna) :=
              Vga_Getpixel(Interfaces.C.Int(P_Ini.X) + Interfaces.C.Int (Columna - Columna_Inicial),
                           Interfaces.C.Int(P_Ini.Y) + Interfaces.C.Int (Fila - Fila_Inicial));
         end loop;
      end loop;
   end Copia_Zona;

   procedure Pega_Zona (M: in out Matrix; P_Ini : in Punto) is
      Fila_Inicial      : Natural := M'First(1);
      Columna_Inicial   : Natural := M'First(2);
   begin
      for Fila in M'Range(1) loop
         for Columna in M'Range(2) loop
            Vga_Pixel
              ((P_Ini.X +  Interfaces.C.Unsigned(Columna - Columna_Inicial),
                P_Ini.Y +  Interfaces.C.Unsigned(Fila - Fila_Inicial)),
               Interfaces.C.Unsigned(M(Fila,Columna)));
         end loop;
      end loop;
   end Pega_Zona;

   procedure Situa_Puntero_Normal(P_Director: in Punto) is
   begin
      -- Interior del puntero (pixels negros)
      for I in 2..8 loop
         Vga_Pixel((P_Director.X+1,P_Director.Y+Unsigned(I)),0);
      end loop;
      for I in 3..7 loop
         Vga_Pixel((P_Director.X+2,P_Director.Y+Unsigned(I)),0);
      end loop;
      for I in 4..6 loop
         Vga_Pixel((P_Director.X+3,P_Director.Y+Unsigned(I)),0);
      end loop;
      for I in 5..6 loop
         Vga_Pixel((P_Director.X+4,P_Director.Y+Unsigned(I)),0);
      end loop;
      Vga_Pixel((P_Director.X+5,P_Director.Y+6),0);
      -- Borde del puntero (l�neas blancas)
      Vga_Line(P_Director,(P_Director.X,P_Director.Y+10),15);
      Vga_Line(P_Director,(P_Director.X+7,P_Director.Y+7),15);
      Vga_Line((P_Director.X,P_Director.Y+10),(P_Director.X+3,P_Director.Y+7),15);
      Vga_Line((P_Director.X+7,P_Director.Y+7),(P_Director.X+3,P_Director.Y+7),15);
      -- Sombra del puntero (l�neas grises)
      Vga_Line((P_Director.X+1,P_Director.Y),(P_Director.X+8,P_Director.Y+7),8);
      Vga_Line((P_Director.X+2,P_Director.Y),(P_Director.X+9,P_Director.Y+7),8);
      Vga_Line((P_Director.X+1,P_Director.Y+10),(P_Director.X+3,P_Director.Y+8),8);
      Vga_Line((P_Director.X+2,P_Director.Y+10),(P_Director.X+4,P_Director.Y+8),8);
      Vga_Line((P_Director.X+3,P_Director.Y+8),(P_Director.X+9,P_Director.Y+8),8);
   end Situa_Puntero_Normal;

   procedure Situa_Puntero_Seleccion(P_Director: in Punto) is
   begin
      --Interior del puntero
      Vga_Line((P_Director.X+4,P_Director.Y+1),(P_Director.X+4,P_Director.Y+7),0);
      Vga_Line((P_Director.X+1,P_Director.Y+4),(P_Director.X+7,P_Director.Y+4),0);
      -- Borde del puntero
      Vga_Line((P_Director.X+3,P_Director.Y),(P_Director.X+5,P_Director.Y),15);
      Vga_Line((P_Director.X+3,P_Director.Y+1),(P_Director.X+3,P_Director.Y+3),15);
      Vga_Line((P_Director.X+2,P_Director.Y+3),(P_Director.X,P_Director.Y+3),15);
      Vga_Line((P_Director.X,P_Director.Y+4),(P_Director.X,P_Director.Y+5),15);
      Vga_Line((P_Director.X+1,P_Director.Y+5),(P_Director.X+3,P_Director.Y+5),15);
      Vga_Line((P_Director.X+3,P_Director.Y+6),(P_Director.X+3,P_Director.Y+8),15);
      Vga_Line((P_Director.X+4,P_Director.Y+8),(P_Director.X+5,P_Director.Y+8),15);
      Vga_Line((P_Director.X+5,P_Director.Y+7),(P_Director.X+5,P_Director.Y+5),15);
      Vga_Line((P_Director.X+6,P_Director.Y+5),(P_Director.X+8,P_Director.Y+5),15);
      Vga_Line((P_Director.X+8,P_Director.Y+4),(P_Director.X+8,P_Director.Y+3),15);
      Vga_Line((P_Director.X+7,P_Director.Y+3),(P_Director.X+5,P_Director.Y+3),15);
      Vga_Line((P_Director.X+5,P_Director.Y+2),(P_Director.X+5,P_Director.Y+1),15);
      -- Sombra del puntero
      Vga_Line((P_Director.X+6,P_Director.Y),(P_Director.X+6,P_Director.Y+2),8);
      Vga_Line((P_Director.X+9,P_Director.Y+3),(P_Director.X+9,P_Director.Y+5),8);
      Vga_Line((P_Director.X+6,P_Director.Y+6),(P_Director.X+6,P_Director.Y+7),8);

   end Situa_Puntero_Seleccion;

   procedure Dibuja_Puntero
     (M: in out Matrix; P: in Punto; P_Ant: in Punto; Refresca: in Boolean) is
      Nueva_Ref,Ultima_Ref : Boton_Ref;
   begin
      Nueva_Ref := Matriz_Pantalla.Obtener_Celda(P);
      Ultima_Ref := Matriz_Pantalla.Obtener_Celda(P_Ant);
      if P /= P_Ant or else Refresca then  -- Ha habido cambio de posici�n
                                           -- O bien se han seleccionado elementos
                                           -- y es necesario refrescar el puntero.
         Pega_Zona(M,P_Ant);

         if Ultima_Ref /= Nueva_Ref then  -- Entrada a Bot�n o salida de Bot�n
            if Ultima_Ref = null then   -- Entrada a Bot�n
               if Esta_Apuntado(Nueva_Ref.all,P) then
--                    Seleccionar(Nueva_Ref.all);
                  Copia_Zona(M,P);
                  Situa_Puntero_Seleccion(P);
               else
                  --                 Dibujar(Nueva_Ref.all);
                  Copia_Zona(M,P);
                  Situa_Puntero_Normal(P);
               end if;
            elsif Nueva_Ref = null then         -- Salida de Bot�n
--                 if Esta_Seleccionado(Ultima_Ref.all) then
--                    Anular_Seleccion(Ultima_Ref.all);
--                 end if;
               if Esta_Enclavado(Ultima_Ref.all) then
                  if Esta_Medio_Presionado(Ultima_Ref.all) then
                     Ultima_Ref.Medio_Presionado := False;
                  end if;
               elsif Esta_Presionado(Ultima_Ref.all) then
                  Liberar(Ultima_Ref.all);
               end if;
               Copia_Zona(M,P);
               Situa_Puntero_Normal(P);
            else  -- En este caso hemos ido de un bot�n a otro
               --Salida del Bot�n anterior
--                 if Esta_Seleccionado(Ultima_Ref.all) then
--                    Anular_Seleccion(Ultima_Ref.all);
--                 end if;
               if Esta_Enclavado(Ultima_Ref.all) then
                  if Esta_Medio_Presionado(Ultima_Ref.all) then
                     Ultima_Ref.Medio_Presionado := False;
                  end if;
               elsif Esta_Presionado(Ultima_Ref.all) then
                  Liberar(Ultima_Ref.all);
               end if;
               --Entrada en el Bot�n nuevo
               if Esta_Apuntado(Nueva_Ref.all,P) then
--                    Seleccionar(Nueva_Ref.all);
                  Copia_Zona(M,P);
                  Situa_Puntero_Seleccion(P);
               else
--                    Dibujar(Nueva_Ref.all);
                  Copia_Zona(M,P);
                  Situa_Puntero_Normal(P);
               end if;
            end if;
         elsif Ultima_Ref = null then  -- Entonces Nueva_Ref tambi�n es null
            Copia_Zona(M,P);
            Situa_Puntero_Normal(P);
         else -- En este caso estamos dentro del mismo bot�n.
            if Esta_Seleccionado(Nueva_Ref.all) then
               if Esta_Apuntado(Nueva_Ref.all,P) then
                  Copia_Zona(M,P);
                  Situa_Puntero_Seleccion(P);
               else
--                    Anular_Seleccion(Nueva_Ref.all);
                  if Esta_Enclavado(Ultima_Ref.all) then
                     if Esta_Medio_Presionado(Ultima_Ref.all) then
                        Ultima_Ref.Medio_Presionado := False;
                     end if;
                  elsif Esta_Presionado(Ultima_Ref.all) then
                     Liberar(Ultima_Ref.all);
                  end if;
                  Copia_Zona(M,P);
                  Situa_Puntero_Normal(P);
               end if;
            else
               if Esta_Apuntado(Nueva_Ref.all,P) then
--                    Seleccionar(Nueva_Ref.all);
                  Copia_Zona(M,P);
                  Situa_Puntero_Seleccion(P);
               else
                  Copia_Zona(M,P);
                  Situa_Puntero_Normal(P);
               end if;
            end if;
         end if;
      else -- No ha habido cambio de posici�n
           null; -- Este else sobra
      end if;
   end Dibuja_Puntero;

   procedure Gestiona_Liberacion (P: in Punto;
                                  Invalidado: in out Boolean;
                                  M: in out Matrix) is
      Boton : Boton_Ref;
   begin
      if Invalidado then
         Invalidado := False;
      end if;
      Boton := Matriz_Pantalla.Obtener_Celda(P);
      if Boton /= null and then Esta_Apuntado(Boton.all,P) then
         if not Esta_Enclavado(Boton.all) and then
           Esta_Presionado(Boton.all) then
            Pega_Zona(M,P);
            Liberar(Boton.all);
            Copia_Zona(M,P);
            --AMC
            Situa_Puntero_Seleccion(P);
            --AMC
         elsif Esta_Enclavado(Boton.all) then
            if Esta_Presionado(Boton.all) and then
              Esta_Medio_Presionado(Boton.all) then
               Pega_Zona(M,P);
               Liberar(Boton.all);
               Copia_Zona(M,P);
               Boton.Medio_Presionado := False;
               -- AMC
               Situa_Puntero_Seleccion(P);
               -- AMC
            elsif not Esta_Presionado(Boton.all) and then
            Esta_Medio_Presionado(Boton.all) then
               Pega_Zona(M,P);
               Presionar(Boton.all);
               Copia_Zona(M,P);
               Boton.Medio_Presionado := False;
               -- AMC
               Situa_Puntero_Seleccion(P);
               -- AMC
            end if;
         end if;

      end if;
   end Gestiona_Liberacion;

   procedure Gestiona_Presion (P: in Punto;
                               Invalidado: in out Boolean;
                               M: in out Matrix) is
      Boton : Boton_Ref;
   begin
      if not Invalidado then
         -- Miraremos si es necesario Invalidar
         -- cualquier posterior presi�n
         Boton := Matriz_Pantalla.Obtener_Celda(P);
         if Boton = null or else not Esta_Apuntado(Boton.all,P) then
            -- Se ha presionado un bot�n del rat�n
            -- fuera de un bot�n de pantalla
            Invalidado := True;
         else
            -- En este caso se ha presionado un bot�n apuntado
            if not Esta_Enclavado(Boton.all) then
               -- Ya se puede presionar (tanto si lo estaba como si no)
               if Esta_Presionado(Boton.all) then
                  null;
               else
                  Pega_Zona(M,P);
                  Presionar(Boton.all);
                  Copia_Zona(M,P);
                  Situa_Puntero_Seleccion(P);
               end if;
            else

               Boton.Medio_Presionado := True;

               -- A�n no se presiona (ni se libera)
--                 if not Esta_Presionado(Boton.all) then
--                    Boton.Medio_Presionado := True;
--                    -- A�n no se presiona (ni se libera)
--                 else
--                    -- En este caso el Bot�n est� presionado
--                    Boton.Medio_Presionado := False;
--                    -- Ponemos el bot�n a estado 'Medio_Liberado'
--                 end if;

            end if;
         end if;
      end if;

   end Gestiona_Presion;

   --------------------------------------------------------------------
   -- Procedimiento asignado por defecto a cada bot�n (no hace nada) --
   --------------------------------------------------------------------
   procedure Procedimiento_Nulo(Codigo: Tipo_Id) is
   begin
      null;
   end Procedimiento_Nulo;

   -----------------------------------------------------------
   -- Operaciones de Elemento_Panel (padre de la jerarqu�a) -- COMPLETO
   -----------------------------------------------------------
   procedure Configurar
     (Elemento                  : in Elemento_Panel_Ref;
      Esquina_Superior_Izda     : in Punto;
      Esquina_Inferior_Dcha     : in Punto;
      Color_De_Marco            : in Color      := Negro;
      Etiqueta                  : in Tipo_Texto := Texto_Vacio;
      Posicion_Etiqueta         : in Punto;
      Color_Etiqueta            : in Color      := Negro;
      Color_Fondo_Etiqueta      : in Color      := Color_Fondo_Panel;
      Identificador             : in Tipo_Id)
   is
   begin
      -- Comprobacion de errores
      if Elemento = null then
         raise No_Existe_Elemento;
      end if;
      if (Esquina_Inferior_Dcha.X <
            Minima_Dimension + Esquina_Superior_Izda.X) or else
        (Esquina_Inferior_Dcha.Y <
           Minima_Dimension + Esquina_Superior_Izda.Y) or else
        (Esquina_Inferior_Dcha.X > Maxima_X) or else
        (Esquina_Inferior_Dcha.Y > Maxima_Y) then
         raise Error_De_Coordenadas;
      end if;

      -- Asignacion de valores a los campos del elemento
      Elemento.Esquina_Sup_Izda := Esquina_Superior_Izda;
      Elemento.Esquina_Inf_Dcha := Esquina_Inferior_Dcha;
      Elemento.Color_Marco      := Color_De_Marco;
      Elemento.Texto            := Etiqueta;
      Elemento.Posicion_Texto   := Posicion_Etiqueta;
      Elemento.Color_Texto      := Color_Etiqueta;
      Elemento.Color_Fondo_Texto:= Color_Fondo_Etiqueta;
      Elemento.Id               := Identificador;
   end Configurar;

   procedure Dibujar(E: in Elemento_Panel) is
   begin
      Vga_Rectangle_Fill
        (E.Esquina_Sup_Izda,E.Esquina_Inf_Dcha,E.Color_Marco);
      Vga_Text
        (E.Texto,E.Posicion_Texto,E.Color_Texto,E.Color_Fondo_Texto);
   end Dibujar;

   procedure Borrar (E: in Elemento_Panel) is
   begin
      Vga_Rectangle_Fill
        (E.Esquina_Sup_Izda,E.Esquina_Inf_Dcha,Color_Fondo_Panel);
      Vga_Rectangle_Fill
        (E.Posicion_Texto,
         (E.Posicion_Texto.X + 8 * Unsigned(Strlen(E.Texto)),
          E.Posicion_Texto.Y + 8),
         Color_Fondo_Panel);
   end Borrar;

   function  Identificar(E: in  Elemento_Panel'Class) return Tipo_Id is
   begin
      return E.Id;
   end Identificar;

   -------------------------------------
   -- Operaciones de Display_Numerico -- INCOMPLETO
   -------------------------------------
   procedure Configurar
     (Elemento                  : in Display_Numerico_Ref;
      Esquina_Superior_Izda     : in Punto;
--        Esquina_Inferior_Dcha         : in Punto;
      Color_De_Marco            : in Color      := Negro;
      Etiqueta                  : in Tipo_Texto := Texto_Vacio;
      Posicion_Etiqueta         : in Punto;
      Color_Etiqueta            : in Color      := Negro;
      Color_Fondo_Etiqueta      : in Color      := Color_Fondo_Panel;
      Identificador             : in Tipo_Id;
      Cifra_Inicial             : in Tipo_Numero:= 0;
      Color_Display             : in Color      := Negro;
      Color_Fondo_Display       : in Color      := Verde_Claro;
      Esta_Encendido            : in Boolean    := False;
      Numero_De_Digitos         : in Tipo_N_Digitos)
   is
      Esq_Inf_Derecha_Auto: Punto;
   begin
      Esq_Inf_Derecha_Auto :=
        (Esquina_Superior_Izda.X + Interfaces.C.Unsigned(28 * Numero_De_Digitos),
         Esquina_Superior_Izda.Y + 40);
      -- Llamamos a la operacion del padre
      Configurar(Elemento_Panel_Ref(Elemento),
                 Esquina_Superior_Izda,Esq_Inf_Derecha_Auto,
                 Color_De_Marco,Etiqueta,Posicion_Etiqueta,
                 Color_Etiqueta,Color_Fondo_Etiqueta,
                 Identificador);
      -- Completamos atributos
      Elemento.Cifra := Cifra_Inicial;
      Elemento.Color_Cifra := Color_Display;
      Elemento.Color_Fondo_Cifra := Color_Fondo_Display;
      Elemento.Encendido := Esta_Encendido;
      Elemento.Numero_Digitos := Numero_De_Digitos;
   end Configurar;

   procedure Dibujar(D: in Display_Numerico) is
   begin
      -- LLamamos a la operacion del padre (Elemento_Panel)
      Dibujar(Elemento_Panel(D));
      -- Dibujaremos el resto de los elementos del display
      if D.Encendido then
         -- Dibujo del Fondo del Display
         Vga_Rectangle_Fill
           ((D.Esquina_Sup_Izda.X+4,D.Esquina_Sup_Izda.Y+4),
            (D.Esquina_Inf_Dcha.X-4,D.Esquina_Inf_Dcha.Y-4),
            D.Color_Fondo_Cifra);
         --Ahora se dibujar�a la cifra correspondiente a D.Cifra
         Dibuja_Numero(D.Cifra,D.Numero_Digitos,D.Color_Cifra,
                       (D.Esquina_Sup_Izda.X + 8,D.Esquina_Sup_Izda.Y + 8));
      else
         -- En este caso el Display est� apagado
         null;
      end if;
   end Dibujar;

   procedure Poner_Cifra (D: in Display_Numerico_Ref; C: in Tipo_Numero) is
   begin
      if C >= 10**(D.Numero_Digitos) then
         raise Demasiados_Digitos;
      end if;

      if D.Encendido and D.Cifra = C then
         null;
      else
         D.Encendido := True;
         D.Cifra := C;
         Cola_Ordenes.Protegida.Inserta(Elemento_Panel_Ref(D));
      end if;
   exception
         when Demasiados_Digitos => null; --Elegir la acci�n a tomar
   end Poner_Cifra;

   procedure Apagar (D: in Display_Numerico_Ref) is
   begin
      if not D.Encendido then
         null;
      else
         D.Encendido := False;
         Cola_Ordenes.Protegida.Inserta(Elemento_Panel_Ref(D));
      end if;
   end Apagar;

   procedure Dibuja_Segmento (N_Segmento: in Tipo_Segmento;
                              C: in Color;
                              P: in Punto) is
   begin
      case N_Segmento is
         when 1 =>
            Vga_Line(P,(P.X+8,P.Y),C);
            Vga_Line((P.X+1,P.Y+1),(P.X+7,P.Y+1),C);
            Vga_Line((P.X+2,P.Y+2),(P.X+6,P.Y+2),C);
         when 2 =>
            Vga_Line(P,(P.X,P.Y+10),C);
            Vga_Line((P.X+1,P.Y+1),(P.X+1,P.Y+11),C);
            Vga_Line((P.X+2,P.Y+2),(P.X+2,P.Y+9),C);
         when 3 =>
            Vga_Line(P,(P.X,P.Y+10),C);
            Vga_Line((P.X-1,P.Y+1),(P.X-1,P.Y+11),C);
            Vga_Line((P.X-2,P.Y+2),(P.X-2,P.Y+9),C);
         when 4 =>
            Vga_Line(P,(P.X+6,P.Y),C);
            Vga_Line((P.X-1,P.Y+1),(P.X+7,P.Y+1),C);
            Vga_Line((P.X,P.Y+2),(P.X+6,P.Y+2),C);
         when 5 =>
            Vga_Line(P,(P.X,P.Y+10),C);
            Vga_Line((P.X-1,P.Y+1),(P.X-1,P.Y+11),C);
            Vga_Line((P.X+1,P.Y+2),(P.X+1,P.Y+9),C);
         when 6 =>
            Vga_Line(P,(P.X,P.Y+10),C);
            Vga_Line((P.X-1,P.Y+2),(P.X-1,P.Y+9),C);
            Vga_Line((P.X+1,P.Y+1),(P.X+1,P.Y+11),C);
         when 7 =>
            Vga_Line(P,(P.X+4,P.Y),C);
            Vga_Line((P.X-1,P.Y+1),(P.X+5,P.Y+1),C);
            Vga_Line((P.X-2,P.Y+2),(P.X+6,P.Y+2),C);
      end case;
   end Dibuja_Segmento;

   procedure Dibuja_Digito(D: in Tipo_Digito;
                           C: Color;
                           P: in Punto) is
   begin
      case D is
         when 0 =>
            Dibuja_Segmento(1,C,(P.X+2,P.Y));
            Dibuja_Segmento(2,C,P);
            Dibuja_Segmento(3,C,(P.X+12,P.Y));
            Dibuja_Segmento(5,C,(P.X+1,P.Y+13));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
            Dibuja_Segmento(7,C,(P.X+4,P.Y+22));
         when 1 =>
            Dibuja_Segmento(3,C,(P.X+12,P.Y));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
         when 2 =>
            Dibuja_Segmento(1,C,(P.X+2,P.Y));
            Dibuja_Segmento(3,C,(P.X+12,P.Y));
            Dibuja_Segmento(4,C,(P.X+3,P.Y+11));
            Dibuja_Segmento(5,C,(P.X+1,P.Y+13));
            Dibuja_Segmento(7,C,(P.X+4,P.Y+22));
         when 3=>
            Dibuja_Segmento(1,C,(P.X+2,P.Y));
            Dibuja_Segmento(3,C,(P.X+12,P.Y));
            Dibuja_Segmento(4,C,(P.X+3,P.Y+11));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
            Dibuja_Segmento(7,C,(P.X+4,P.Y+22));
         when 4 =>
            Dibuja_Segmento(2,C,P);
            Dibuja_Segmento(4,C,(P.X+3,P.Y+11));
            Dibuja_Segmento(3,C,(P.X+12,P.Y));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
         when 5 =>
            Dibuja_Segmento(1,C,(P.X+2,P.Y));
            Dibuja_Segmento(2,C,P);
            Dibuja_Segmento(4,C,(P.X+3,P.Y+11));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
            Dibuja_Segmento(7,C,(P.X+4,P.Y+22));
         when 6 =>
            Dibuja_Segmento(1,C,(P.X+2,P.Y));
            Dibuja_Segmento(2,C,P);
            Dibuja_Segmento(4,C,(P.X+3,P.Y+11));
            Dibuja_Segmento(5,C,(P.X+1,P.Y+13));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
            Dibuja_Segmento(7,C,(P.X+4,P.Y+22));
         when 7 =>
            Dibuja_Segmento(1,C,(P.X+2,P.Y));
            Dibuja_Segmento(3,C,(P.X+12,P.Y));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
         when 8 =>
            Dibuja_Segmento(1,C,(P.X+2,P.Y));
            Dibuja_Segmento(2,C,P);
            Dibuja_Segmento(3,C,(P.X+12,P.Y));
            Dibuja_Segmento(4,C,(P.X+3,P.Y+11));
            Dibuja_Segmento(5,C,(P.X+1,P.Y+13));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
            Dibuja_Segmento(7,C,(P.X+4,P.Y+22));
         when 9 =>
            Dibuja_Segmento(1,C,(P.X+2,P.Y));
            Dibuja_Segmento(2,C,P);
            Dibuja_Segmento(3,C,(P.X+12,P.Y));
            Dibuja_Segmento(4,C,(P.X+3,P.Y+11));
            Dibuja_Segmento(6,C,(P.X+11,P.Y+13));
            Dibuja_Segmento(7,C,(P.X+4,P.Y+22));
      end case;
   end Dibuja_Digito;

   procedure Dibuja_Numero(N: in Tipo_Numero;
                           N_Digitos: in Tipo_N_Digitos;
                           C: Color;
                           P: in Punto) is
      type Lista_Numeros is array (1..N_Digitos) of Tipo_Digito;
      Lista: Lista_Numeros;
      Temp: Tipo_Numero;
   begin
      for I in 1..N_Digitos loop
         Temp := N / (10**(I-1));
         Lista(I) := Temp mod 10;
         Dibuja_Digito(Lista(I),C,(P.X+Interfaces.C.Unsigned(28*(N_Digitos-I)),P.Y));
         if N_Digitos > 1 and then I < N_Digitos then
            Vga_Circle
              ((P.X+Interfaces.C.Unsigned(20*(N_Digitos-I)),P.Y+8),1,C);
            Vga_Circle
              ((P.X+Interfaces.C.Unsigned(20*(N_Digitos-I)),P.Y+16),1,C);
         end if;
      end loop;
   end Dibuja_Numero;

   -------------------------------------------------
   -- Operaciones de Elemento_Con_Luz (abstracto) -- COMPLETO
   -------------------------------------------------
   procedure Configurar
     (Elemento                  : in Elemento_Con_Luz_Ref;
      Esquina_Superior_Izda     : in Punto;
      Esquina_Inferior_Dcha     : in Punto;
      Color_De_Marco            : in Color      := Negro;
      Etiqueta                  : in Tipo_Texto := Texto_Vacio;
      Posicion_Etiqueta         : in Punto;
      Color_Etiqueta            : in Color      := Negro;
      Color_Fondo_Etiqueta      : in Color      := Color_Fondo_Panel;
      Identificador             : in Tipo_Id;
      Color_Elemento            : in Color      := Verde;
      Color_Elemento_Seleccion  : in Color      := Verde_Claro)
   is
   begin
      -- Llamamos a la operacion del padre
      Configurar(Elemento_Panel_Ref(Elemento),
                 Esquina_Superior_Izda,Esquina_Inferior_Dcha,
                 Color_De_Marco,
                 Etiqueta,Posicion_Etiqueta,Color_Etiqueta,Color_Fondo_Etiqueta,
                 Identificador);
      -- Completamos atributos
      Elemento.Color_Normal := Color_Elemento;
      Elemento.Color_Seleccion := Color_Elemento_Seleccion;
   end Configurar;

   function  Esta_Seleccionado (E_Luz: Elemento_Con_Luz'Class) return Boolean is
   begin
      return E_Luz.Seleccionado;
   end Esta_Seleccionado;

   procedure Seleccionar (E_Luz: in Elemento_Con_Luz_Ref) is
   begin
      if E_Luz.Seleccionado then
         null;
      else
         E_Luz.Seleccionado := True;
         Cola_Ordenes.Protegida.Inserta(Elemento_Panel_Ref(E_Luz));
      end if;
   end Seleccionar;

   procedure Anular_Seleccion(E_Luz: in Elemento_Con_Luz_Ref) is
   begin
      if not E_Luz.Seleccionado then
         null;
      else
         E_Luz.Seleccionado := False;
         Cola_Ordenes.Protegida.Inserta(Elemento_Panel_Ref(E_Luz));
      end if;
   end Anular_Seleccion;

   ---AMC{
   procedure Cambia_Luz
     (E_Luz                     : in out Elemento_Con_Luz;
      Color_Elemento            : in Color;
      Color_Elemento_Seleccion  : in Color) is
   begin
      E_Luz.Color_Normal := Color_Elemento;
      E_Luz.Color_Seleccion := Color_Elemento_Seleccion;
   end Cambia_Luz;
   ---AMC}

   ------------------------
   -- Operaciones de Luz -- COMPLETO
   ------------------------

   procedure Configurar
     (Elemento                  : in Luz_Ref;
      Esquina_Superior_Izda     : in Punto;
      Esquina_Inferior_Dcha     : in Punto;
      Color_De_Marco            : in Color      := Negro;
      Etiqueta                  : in Tipo_Texto := Texto_Vacio;
      Posicion_Etiqueta         : in Punto;
      Color_Etiqueta            : in Color      := Negro;
      Color_Fondo_Etiqueta      : in Color      := Color_Fondo_Panel;
      Identificador             : in Tipo_Id;
      Color_Elemento            : in Color      := Verde;
      Color_Elemento_Seleccion  : in Color      := Verde_Claro) is
   begin
      Configurar(Elemento_Con_Luz_Ref(Elemento),
                 Esquina_Superior_Izda,Esquina_Inferior_Dcha,
                 Color_De_Marco,Etiqueta,Posicion_Etiqueta,Color_Etiqueta,
                 Color_Fondo_Etiqueta,Identificador,Color_Elemento,
                 Color_Elemento_Seleccion);
   end Configurar;

   procedure Dibujar(L: in Luz) is
   begin
      Dibujar(Elemento_Panel(L));
      if L.Seleccionado then
         Vga_Rectangle_Fill
           ((L.Esquina_Sup_Izda.X+4,L.Esquina_Sup_Izda.Y+4),
            (L.Esquina_Inf_Dcha.X-4,L.Esquina_Inf_Dcha.Y-4),
            L.Color_Seleccion);
      else
         Vga_Rectangle_Fill
           ((L.Esquina_Sup_Izda.X+4,L.Esquina_Sup_Izda.Y+4),
            (L.Esquina_Inf_Dcha.X-4,L.Esquina_Inf_Dcha.Y-4),
            L.Color_Normal);
      end if;
   end Dibujar;

   procedure Seleccionar(L: in Luz_Ref) is
   begin
      Seleccionar(Elemento_Con_Luz_Ref(L));
   end Seleccionar;

   procedure Anular_Seleccion (L: in Luz_Ref) is
   begin
      Anular_Seleccion(Elemento_Con_Luz_Ref(L));
   end Anular_Seleccion;

   --------------------------------------
   -- Operaciones de Boton (abstracto) -- COMPLETO
   --------------------------------------
   procedure Configurar
     (Elemento                  : in Boton_Ref;
      Esquina_Superior_Izda     : in Punto;
      Esquina_Inferior_Dcha     : in Punto;
      Color_De_Marco            : in Color      := Negro;
      Etiqueta                  : in Tipo_Texto := Texto_Vacio;
      Posicion_Etiqueta         : in Punto;
      Color_Etiqueta            : in Color      := Negro;
      Color_Fondo_Etiqueta      : in Color      := Color_Fondo_Panel;
      Identificador             : in Tipo_Id;
      Color_Elemento            : in Color      := Verde;
      Color_Elemento_Seleccion  : in Color      := Verde_Claro;
      Color_Elemento_Presion    : in Color      := Verde;
      Esta_Enclavado            : in Boolean    := True;
      Tiene_Luz                 : in Boolean    := False;
      Procedimiento_Presion     : in Presion_Ref        := Procedimiento_Nulo'Access;
      Procedimiento_Liberacion  : in Liberacion_Ref     := Procedimiento_Nulo'Access)
   is
      Fila_Min,Fila_Max         : Fila;
      Columna_Min,Columna_Max   : Columna;
   begin
      -- Llamamos a la operacion del padre
      Configurar(Elemento_Con_Luz_Ref(Elemento),
                 Esquina_Superior_Izda,Esquina_Inferior_Dcha,
                 Color_De_Marco,
                 Etiqueta,Posicion_Etiqueta,Color_Etiqueta,Color_Fondo_Etiqueta,
                 Identificador,Color_Elemento,Color_Elemento_Seleccion);

      -- Completamos atributos
      Elemento.Color_Presion    := Color_Elemento_Presion;
      Elemento.Enclavado        := Esta_Enclavado;
      Elemento.Iluminacion_Auto := Tiene_Luz;
      Elemento.Presiona         := Procedimiento_Presion;
      Elemento.Libera           := Procedimiento_Liberacion;

      -- Calculo de las celdas de pantalla ocupadas por el boton
      Fila_Min          := (Esquina_Superior_Izda.Y)/10;
      Columna_Min       := (Esquina_Superior_Izda.X)/10;
      Fila_Max          := (Esquina_Inferior_Dcha.Y)/10;
      Columna_Max       := (Esquina_Inferior_Dcha.X)/10;

      -- Comprobacion de que las celdas de pantalla estan libres
      for La_Fila in Fila_Min..Fila_Max loop
         for La_Columna in Columna_Min..Columna_Max loop
            if Matriz_Pantalla.Celda_Ya_Asignada(La_Fila,La_Columna) then
               raise Posicion_Ocupada;
            end if;
         end loop;
      end loop;

      -- Asignacion de punteros a Boton en la matriz de la pantalla
      for La_Fila in Fila_Min..Fila_Max loop
         for La_Columna in Columna_Min..Columna_Max loop
            Matriz_Pantalla.Asigna(Elemento,La_Fila,La_Columna);
         end loop;
      end loop;
   end Configurar;

   procedure Borrar(B: in Boton) is
      Fila_Min,Fila_Max         : Fila;
      Columna_Min,Columna_Max   : Columna;
   begin
      -- Llamamos a la operacion del padre
      Borrar(Elemento_Panel(B));

      -- Calculo de las celdas de pantalla ocupadas por el boton
      Fila_Min          := (B.Esquina_Sup_Izda.Y)/10;
      Columna_Min       := (B.Esquina_Sup_Izda.X)/10;
      Fila_Max          := (B.Esquina_Inf_Dcha.Y)/10;
      Columna_Max       := (B.Esquina_Inf_Dcha.X)/10;

      -- Eliminacion de punteros a Boton en la matriz de la pantalla
      for La_Fila in Fila_Min..Fila_Max loop
         for La_Columna in Columna_Min..Columna_Max loop
            Matriz_Pantalla.Elimina(La_Fila,La_Columna);
         end loop;
      end loop;
   end Borrar;

   function Esta_Presionado (B: in Boton'Class) return Boolean is
   begin
      return B.Presionado;
   end Esta_Presionado;

   function Esta_Medio_Presionado (B: in Boton'Class) return Boolean is
   begin
      return B.Medio_Presionado;
   end Esta_Medio_Presionado;

   function Esta_Enclavado  (B: in Boton'Class) return Boolean is
   begin
      return B.Enclavado;
   end Esta_Enclavado;

   procedure Presionar(B: in out Boton'Class) is
   begin
      B.Presionado := True;
      B.Presiona(B.Id);
      -- o: B.Presiona(Identificar(B)).all
      Dibujar(B);
   end Presionar;

   procedure Liberar(B: in out Boton'Class) is
   begin
      B.Presionado := False;
      B.Libera(B.Id);
      -- o: B.Libera(Identificar(B)).all
      Dibujar(B);
   end Liberar;

   -----------------------------------
   -- Operaciones de Boton Circular -- COMPLETO
   -----------------------------------
   procedure Configurar
     (Elemento                  : in Boton_Circular_Ref;
      Esquina_Superior_Izda     : in Punto;
      Esquina_Inferior_Dcha     : in Punto;
      Color_De_Marco            : in Color      := Negro;
      Etiqueta                  : in Tipo_Texto := Texto_Vacio;
      Posicion_Etiqueta         : in Punto;
      Color_Etiqueta            : in Color      := Negro;
      Color_Fondo_Etiqueta      : in Color      := Color_Fondo_Panel;
      Identificador             : in Tipo_Id;
      Color_Elemento            : in Color      := Verde;
      Color_Elemento_Seleccion  : in Color      := Verde_Claro;
      Color_Elemento_Presion    : in Color      := Verde;
      Esta_Enclavado            : in Boolean    := True;
      Tiene_Luz                 : in Boolean    := False;
      Procedimiento_Presion     : in Presion_Ref        := Procedimiento_Nulo'Access;
      Procedimiento_Liberacion  : in Liberacion_Ref     := Procedimiento_Nulo'Access) is
   begin
      -- Llamamos a la operacion del padre
      Configurar(Boton_Ref(Elemento),
                 Esquina_Superior_Izda,Esquina_Inferior_Dcha,
                 Color_De_Marco,Etiqueta,Posicion_Etiqueta,
                 Color_Etiqueta,Color_Fondo_Etiqueta,
                 Identificador,Color_Elemento,Color_Elemento_Seleccion,
                 Color_Elemento_Presion,Esta_Enclavado,Tiene_Luz,
                 Procedimiento_Presion,Procedimiento_Liberacion);
   end Configurar;

   procedure Dibujar(B: in Boton_Circular) is
      Centro    : Punto;
      Radio     : Interfaces.C.Unsigned;
      Ancho,Alto: Interfaces.C.Unsigned;
   begin
      Dibujar(Elemento_Panel(B));
      -- Calculo del Centro y el Radio del Boton
      Centro.X := (B.Esquina_Sup_Izda.X
                   + B.Esquina_Inf_Dcha.X) / 2;
      Centro.Y := (B.Esquina_Sup_Izda.Y
                   + B.Esquina_Inf_Dcha.Y) / 2;
      -- Determinacion del alto y el ancho del Boton
      Ancho := B.Esquina_Inf_Dcha.X - B.Esquina_Sup_Izda.X;
      Alto  := B.Esquina_Inf_Dcha.Y - B.Esquina_Sup_Izda.Y;
      -- Calculamos el Radio en conformidad a la dimension minima
      if Ancho <= Alto then
         Radio := (Ancho / 2) - 4;
      else
         Radio := (Alto / 2) - 4;
      end if;
      -- Dibujo del Boton Circular
      -- Caso 1: Boton Presionado y Seleccionado o bien
      --        Boton Presionado y Luminoso
      if (B.Presionado and then B.Seleccionado) or else
        (B.Presionado and then B.Iluminacion_Auto) then
         Vga_Circle(Centro,Radio,B.Color_Seleccion);
         Vga_Circle(Centro,Radio-6,B.Color_Presion);
         Vga_Circumference(Centro,Radio-6,Negro);
         -- Caso 2: Boton Presionado pero No Seleccionado y No Luminoso
      elsif B.Presionado then
         Vga_Circumference(Centro,Radio,B.Color_Normal);
         Vga_Circle(Centro,Radio-6,B.Color_Presion);
         Vga_Circumference(Centro,Radio-6,Negro);
         -- Caso 3: Boton Seleccionado pero No Presionado
      elsif B.Seleccionado then
         Vga_Circle(Centro,Radio,B.Color_Seleccion);
         -- Caso 4: Boton No Presionado y No Seleccionado
      else
         Vga_Circle(Centro,Radio,B.Color_Normal);
      end if;

--        if B.Presionado then
--
--              Vga_Circle(Centro,Radio,B.Color_Seleccion);
--              Vga_Circle(Centro,Radio-4,B.Color_Presion);
--              Vga_Circumference(Centro,Radio-4,Negro);
--           end if;
--        elsif B.Seleccionado then
--           Vga_Circle(Centro,Radio,B.Color_Seleccion);
--        else
--           Vga_Circle(Centro,Radio,B.Color_Normal);
--        end if;
   end Dibujar;

   procedure Seleccionar(B: in Boton_Circular_Ref) is
   begin
      Seleccionar(Elemento_Con_Luz_Ref(B));
   end Seleccionar;

   procedure Anular_Seleccion (B: in Boton_Circular_Ref) is
   begin
      Anular_Seleccion(Elemento_Con_Luz_Ref(B));
   end Anular_Seleccion;

   function Esta_Apuntado(B: in Boton_Circular; P: in Punto) return Boolean is
      Centro    : Punto;
      Radio     : Interfaces.C.Unsigned;
      Ancho,Alto: Interfaces.C.Unsigned;
   begin
      -- Calculo del Centro y el Radio del Boton
      Centro.X := (B.Esquina_Sup_Izda.X
                   + B.Esquina_Inf_Dcha.X) / 2;
      Centro.Y := (B.Esquina_Sup_Izda.Y
                   + B.Esquina_Inf_Dcha.Y) / 2;
      -- Determinacion del alto y el ancho del Boton
      Ancho := B.Esquina_Inf_Dcha.X - B.Esquina_Sup_Izda.X;
      Alto  := B.Esquina_Inf_Dcha.Y - B.Esquina_Sup_Izda.Y;
      -- Calculamos el Radio en conformidad a la dimension minima
      if Ancho <= Alto then
         Radio := (Ancho / 2) - 4;
      else
         Radio := (Alto / 2) - 4;
      end if;
      return (((P.X - Centro.X)**2 + (P.Y - Centro.Y)**2)) < Radio**2;
   end Esta_Apuntado;

   --------------------------------------
   -- Operaciones de Boton Rectangular -- COMPLETO
   --------------------------------------
   procedure Configurar
     (Elemento                  : in Boton_Rectangular_Ref;
      Esquina_Superior_Izda     : in Punto;
      Esquina_Inferior_Dcha     : in Punto;
      Color_De_Marco            : in Color      := Negro;
      Etiqueta                  : in Tipo_Texto := Texto_Vacio;
      Posicion_Etiqueta         : in Punto;
      Color_Etiqueta            : in Color      := Negro;
      Color_Fondo_Etiqueta      : in Color      := Color_Fondo_Panel;
      Identificador             : in Tipo_Id;
      Color_Elemento            : in Color      := Verde;
      Color_Elemento_Seleccion  : in Color      := Verde_Claro;
      Color_Elemento_Presion    : in Color      := Verde;
      Esta_Enclavado            : in Boolean    := True;
      Tiene_Luz                 : in Boolean    := False;
      Procedimiento_Presion     : in Presion_Ref        := Procedimiento_Nulo'Access;
      Procedimiento_Liberacion  : in Liberacion_Ref     := Procedimiento_Nulo'Access) is
   begin
      -- Llamamos a la operacion del padre
      Configurar(Boton_Ref(Elemento),
                 Esquina_Superior_Izda,Esquina_Inferior_Dcha,
                 Color_De_Marco,Etiqueta,Posicion_Etiqueta,
                 Color_Etiqueta,Color_Fondo_Etiqueta,
                 Identificador,Color_Elemento,Color_Elemento_Seleccion,
                 Color_Elemento_Presion,Esta_Enclavado,Tiene_Luz,
                 Procedimiento_Presion,Procedimiento_Liberacion);
   end Configurar;

   procedure Dibujar(B: in Boton_Rectangular) is
   begin
      Dibujar(Elemento_Panel(B));
      -- Dibujo del Boton Rectangular
      -- Caso 1: Boton Presionado y Seleccionado o bien
      --        Boton Presionado y Luminoso
      if (B.Presionado and then B.Seleccionado) or else
        (B.Presionado and then B.Iluminacion_Auto) then
         Vga_Rectangle_Fill
           ((B.Esquina_Sup_Izda.X + 4,B.Esquina_Sup_Izda.Y + 4),
            (B.Esquina_Inf_Dcha.X - 4,B.Esquina_Inf_Dcha.Y - 4),
            B.Color_Seleccion);
         Vga_Rectangle_Fill
           ((B.Esquina_Sup_Izda.X + 10,B.Esquina_Sup_Izda.Y + 10),
            (B.Esquina_Inf_Dcha.X - 10,B.Esquina_Inf_Dcha.Y - 10),
            B.Color_Presion);
         Vga_Rectangle
           ((B.Esquina_Sup_Izda.X + 10,B.Esquina_Sup_Izda.Y + 10),
            (B.Esquina_Inf_Dcha.X - 10,B.Esquina_Inf_Dcha.Y - 10),
            Negro);
         -- Caso 2: Boton Presionado pero No Seleccionado y No Luminoso
      elsif B.Presionado then
       Vga_Rectangle
           ((B.Esquina_Sup_Izda.X + 4,B.Esquina_Sup_Izda.Y + 4),
            (B.Esquina_Inf_Dcha.X - 4,B.Esquina_Inf_Dcha.Y - 4),
            B.Color_Normal);
         Vga_Rectangle_Fill
           ((B.Esquina_Sup_Izda.X + 10,B.Esquina_Sup_Izda.Y + 10),
            (B.Esquina_Inf_Dcha.X - 10,B.Esquina_Inf_Dcha.Y - 10),
            B.Color_Presion);
         Vga_Rectangle
           ((B.Esquina_Sup_Izda.X + 10,B.Esquina_Sup_Izda.Y + 10),
            (B.Esquina_Inf_Dcha.X - 10,B.Esquina_Inf_Dcha.Y - 10),
            Negro);
         -- Caso 3: Boton Seleccionado pero No Presionado
      elsif B.Seleccionado then
         Vga_Rectangle_Fill
           ((B.Esquina_Sup_Izda.X + 4,B.Esquina_Sup_Izda.Y + 4),
            (B.Esquina_Inf_Dcha.X - 4,B.Esquina_Inf_Dcha.Y - 4),
            B.Color_Seleccion);
         -- Caso 4: Boton No Presionado y No Seleccionado
      else
         Vga_Rectangle_Fill
           ((B.Esquina_Sup_Izda.X + 4,B.Esquina_Sup_Izda.Y + 4),
            (B.Esquina_Inf_Dcha.X - 4,B.Esquina_Inf_Dcha.Y - 4),
            B.Color_Normal);
      end if;

--        if B.Presionado then
--           Vga_Rectangle_Fill
--             ((B.Esquina_Sup_Izda.X + 4,B.Esquina_Sup_Izda.Y + 4),
--              (B.Esquina_Inf_Dcha.X - 4,B.Esquina_Inf_Dcha.Y - 4),
--              B.Color_Seleccion);
--           Vga_Rectangle_Fill
--             ((B.Esquina_Sup_Izda.X + 8,B.Esquina_Sup_Izda.Y + 8),
--              (B.Esquina_Inf_Dcha.X - 8,B.Esquina_Inf_Dcha.Y - 8),
--              B.Color_Presion);
--           Vga_Rectangle
--             ((B.Esquina_Sup_Izda.X + 8,B.Esquina_Sup_Izda.Y + 8),
--              (B.Esquina_Inf_Dcha.X - 8,B.Esquina_Inf_Dcha.Y - 8),
--              Negro);
--        elsif B.Seleccionado then
--           Vga_Rectangle_Fill
--             ((B.Esquina_Sup_Izda.X + 4,B.Esquina_Sup_Izda.Y + 4),
--              (B.Esquina_Inf_Dcha.X - 4,B.Esquina_Inf_Dcha.Y - 4),
--              B.Color_Seleccion);
--        else
--           Vga_Rectangle_Fill
--             ((B.Esquina_Sup_Izda.X + 4,B.Esquina_Sup_Izda.Y + 4),
--              (B.Esquina_Inf_Dcha.X - 4,B.Esquina_Inf_Dcha.Y - 4),
--              B.Color_Normal);
--        end if;
   end Dibujar;

   procedure Seleccionar(B: in Boton_Rectangular_Ref) is
   begin
      Seleccionar(Elemento_Con_Luz_Ref(B));
   end Seleccionar;

   procedure Anular_Seleccion (B: in Boton_Rectangular_Ref) is
   begin
      Anular_Seleccion(Elemento_Con_Luz_Ref(B));
   end Anular_Seleccion;

   function Esta_Apuntado(B: in Boton_Rectangular; P: in Punto) return Boolean is
   begin
      return (P.X > B.Esquina_Sup_Izda.X + 4) and then
        (P.X < B.Esquina_Inf_Dcha.X - 4) and then
        (P.Y > B.Esquina_Sup_Izda.Y + 4) and then
        (P.Y < B.Esquina_Inf_Dcha.Y - 4) ;
   end Esta_Apuntado;

   task body Gestiona_Pantalla is
      pragma linker_options ("-ldrivers -lsvga -lm");
      Ret_Error         : Interfaces.C.Int;
      Fd                : POSIX_IO.File_Descriptor :=0;
      Data              : PS2_Mouse.Mouse_Event;
      Raton_Aux         : PS2_Mouse.PS2_Mouse;
      P_Act, P_Ant      : Punto;
      Matriz            : Matrix(1..11,1..10);
      Presion_Invalidada: Boolean := True;
      Byte_De_Control   : PS2_Mouse.Control_Byte;
      Periodo_Tarea     : constant Duration := 0.010;
      Periodo           : constant Duration := 0.5;
      Proximo_Periodo   : Time := Clock;
      Orden_Ref         : Elemento_Panel_Ref;
      Refresca_Pantalla : Boolean;
      use type PS2_Mouse.PS2_Mouse_Buttons;

      procedure Read is new POSIX_IO.Generic_Read (PS2_Mouse.Mouse_Event);
      procedure Ioctl is new POSIX_IO.Generic_Ioctl
        (PS2_Mouse.Ioctl_Command,PS2_Mouse.Ioctl_Data);
   begin

      Fd := POSIX_IO.Open ("/dev/ps2mouse", POSIX_IO.Read_Only);
      Raton_Aux.Command := PS2_Mouse.Set_Scaling_2_1;
      Ioctl(Fd,PS2_Mouse.PS2_Command,Raton_Aux);

      Ioctl(Fd,PS2_Mouse.Reset_Blocking_Mode,Raton_Aux);

      Ioctl(Fd,PS2_Mouse.Get_Mouse_Position,Raton_Aux);

      P_Act.X := Interfaces.C.Unsigned(3*Raton_Aux.X_Pos+320);
      P_Act.Y := Interfaces.C.Unsigned(240-3*Raton_Aux.Y_Pos);
      P_Ant := P_Act;

--        Copia_Zona(Matriz,P_Act);
--        Situa_Puntero_Normal(P_Act);
--        delay(2.0);

      while not Puntero.Esta_Inicializado loop
         Proximo_Periodo := Proximo_Periodo + Periodo;
         delay until Proximo_Periodo;
      end loop;
      Copia_Zona(Matriz,P_Act);
      --        Situa_Puntero_Normal(P_Act);

--        -- Prueba de dibujo de numero de displays
--        for I in reverse 0..9 loop
--           Vga_Rectangle_Fill((447,82),(475,122),Rojo);
--           Vga_Rectangle((447,82),(475,122),Negro);
--           Vga_Rectangle_Fill((451,86),(471,118),Negro);
--           Dibuja_Digito(I,Blanco,(455,90));
--           delay(1.0);
--        end loop;
--        -----------------------------------------

      Proximo_Periodo := Clock;
      loop
         Proximo_Periodo := Proximo_Periodo + Periodo_Tarea;
         delay until Proximo_Periodo;

         if not Cola_Ordenes.Protegida.Esta_Vacia then
            Refresca_Pantalla := True;
            Pega_Zona(Matriz,P_Act);
         end if;

         while not Cola_Ordenes.Protegida.Esta_Vacia loop
            Cola_Ordenes.Protegida.Extrae(Orden_Ref);
            Dibujar(Orden_Ref.all);
         end loop;

         if Refresca_Pantalla then
            Copia_Zona(Matriz,P_Act);
         end if;

         Ioctl(Fd,PS2_Mouse.Get_Mouse_Position,Raton_Aux);
         if Raton_Aux.X_Pos <= -104 then
            Raton_Aux.X_Pos := -104;
            Ioctl(Fd,PS2_Mouse.Set_Mouse_Position,Raton_Aux);
         elsif Raton_Aux.X_Pos >= 104 then
            Raton_Aux.X_Pos := 104;
            Ioctl(Fd,PS2_Mouse.Set_Mouse_Position,Raton_Aux);
         end if;
         if Raton_Aux.Y_Pos <= -77 then
            Raton_Aux.Y_Pos := -77;
            Ioctl(Fd,PS2_Mouse.Set_Mouse_Position,Raton_Aux);
         elsif Raton_Aux.Y_Pos >= 77 then
            Raton_Aux.Y_Pos := 77;
            Ioctl(Fd,PS2_Mouse.Set_Mouse_Position,Raton_Aux);
         end if;

         P_Act.X := Interfaces.C.Unsigned(3*Raton_Aux.X_Pos+320);
         P_Act.Y := Interfaces.C.Unsigned(240-3*Raton_Aux.Y_Pos);

--           if P_Ant /= P_Act then -- Ha habido movimiento
--              Dibuja_Puntero(Matriz,P_Act,P_Ant);
--           end if;

         Dibuja_Puntero(Matriz,P_Act,P_Ant,Refresca_Pantalla);

         -- Actualizamos P_Ant para el proximo ciclo
         P_Ant := P_Act;

         -- Se obtiene un evento de rat�n y se comprueba el estado de
         -- los botones
         Read(Fd,Data);
         Byte_De_Control := PS2_Mouse.Unsigned_8_To_Control_Byte(Data.Mouse_Byte(1));

         -- Usamos s�lo el bot�n izqdo.

         if (Byte_De_Control.Buttons mod 2)=0 then
            -- Detecci�n de bot�n izquierdo liberado
            Gestiona_Liberacion(P_Act,Presion_Invalidada,Matriz);
         else
            -- Deteccion de bot�n izquierdo presionado
            Gestiona_Presion(P_Act,Presion_Invalidada,Matriz);
         end if;

         Refresca_Pantalla := False;

      end loop;

   exception
      when Error: others =>
         Ret_Error := Vga_Marte.Vga_Setmode(TEXT);
         Put_Line("Se ha elevado " & Exception_Name(Error));
         Put_Line("Mensaje      : " & Exception_Message(Error));
         Put_Line("Informacion  : " & Exception_Information(Error));
   end Gestiona_Pantalla;

   Error_Inicio_Modo_Grafico : exception;

begin
   Matriz_Pantalla.Inicializa;
   if Init_Vga(G640x480x16,VGA,16#8901#) /= 0 then
      raise Error_Inicio_Modo_Grafico;
   end if;
end Panel;
