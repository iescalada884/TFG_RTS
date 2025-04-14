with MaRTE_OS;
with POSIX_IO;
pragma Elaborate_All (POSIX_IO);
with Vga_Marte; use Vga_Marte;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with PS2_Mouse; use PS2_Mouse;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Calendar; use Ada.Calendar;
with Cola; pragma Elaborate_All(Cola);

package Panel is

   ------------------------
   -- Types and subtypes --
   ------------------------
   subtype Punto is Vga_Marte.Point_T;
   type Tipo_Id is new Natural;
   type Presion_Ref is access procedure (Codigo: Tipo_Id);
   type Liberacion_Ref is access procedure (Codigo: Tipo_Id);
   subtype Tipo_Texto is Interfaces.C.Strings.Chars_Ptr;
   subtype Tipo_Numero is Natural; -- Tipo para el Display Numerico
   subtype Tipo_N_Digitos is Positive; -- Tipo para el Display Numerico
   subtype Color is Interfaces.C.Unsigned;

   ---------------
   -- Constants --
   ---------------
   -- Ancho y largo minimo (en pixels) del marco de un elemento del panel
   Minima_Dimension     : constant Interfaces.C.Unsigned := 16;

   Texto_Vacio  : constant Tipo_Texto := Interfaces.C.Strings.New_String("");

   Amarillo     : constant Color := 14;
   Azul         : constant Color := 1;
   Azul_Hielo   : constant Color := 9;
   Azul_Claro   : constant Color := 11;
   Blanco       : constant Color := 15;
   Cyan         : constant Color := 3;
   Gris         : constant Color := 8;
   Gris_Claro   : constant Color := 7;
   Magenta      : constant Color := 5;
   Marron       : constant Color := 6;
   Negro        : constant Color := 0;
   Rojo         : constant Color := 4;
   Rojo_Claro   : constant Color := 12;
   Rosa         : constant Color := 13;
   Verde        : constant Color := 2;
   Verde_Claro  : constant Color := 10;

   Color_Fondo_Panel    : constant Color := Gris_Claro;

   ----------------
   -- Exceptions --
   ----------------
   -- Elemento aun no creado
   No_Existe_Elemento   : exception;
   -- Coordenadas fuera de limites
   Error_De_Coordenadas : exception;
   -- Posicion ocupada en la matriz de pantalla
   Posicion_Ocupada     : exception;
   -- Numero a dibujar en el display demasiado grande
   Demasiados_Digitos   : exception;

   ------------------------------------
   -- Elemento del panel (abstracto) --
   ------------------------------------
   type Elemento_Panel is abstract tagged private;
   type Elemento_Panel_Ref is access all Elemento_Panel'Class;
   procedure Configurar
     (Elemento                  : in Elemento_Panel_Ref;
      Esquina_Superior_Izda     : in Punto;
      Esquina_Inferior_Dcha     : in Punto;
      Color_De_Marco            : in Color      := Negro;
      Etiqueta                  : in Tipo_Texto := Texto_Vacio;
      Posicion_Etiqueta         : in Punto;
      Color_Etiqueta            : in Color      := Negro;
      Color_Fondo_Etiqueta      : in Color      := Color_Fondo_Panel;
      Identificador             : in Tipo_Id);
   procedure Dibujar    (E: in  Elemento_Panel);
   procedure Borrar     (E: in  Elemento_Panel);
   function  Identificar(E: in  Elemento_Panel'Class) return Tipo_Id;

   ----------------------
   -- Display Numerico --
   ----------------------
   type Display_Numerico is new Elemento_Panel with private;
   type Display_Numerico_Ref is access all Display_Numerico'Class;
   procedure Configurar
     (Elemento                  : in Display_Numerico_Ref;
      Esquina_Superior_Izda     : in Punto;
      --  Esquina_Inferior_Dcha         : in Punto;
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
      Numero_De_Digitos         : in Tipo_N_Digitos);

   procedure Dibujar(D: in Display_Numerico);
   -- El procedimiento Borrar se hereda de
   -- la clase padre Elemento_Panel y no se ampl�a
   procedure Poner_Cifra (D: in Display_Numerico_Ref; C: in Tipo_Numero);
   procedure Apagar (D: in Display_Numerico_Ref);

   -----------------------------------
   -- Elemento con Luz (abstracto) ---
   -----------------------------------
   type Elemento_Con_Luz is abstract new Elemento_Panel with private;
   type Elemento_Con_Luz_Ref is access all Elemento_Con_Luz'Class;

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
      Color_Elemento_Seleccion  : in Color      := Verde_Claro);

   procedure       Seleccionar(E_Luz: in Elemento_Con_Luz_Ref);
   procedure Anular_Seleccion (E_Luz: in Elemento_Con_Luz_Ref);

   procedure Cambia_Luz
     (E_Luz                     : in out Elemento_Con_Luz;
      Color_Elemento            : in Color;
      Color_Elemento_Seleccion  : in Color);

   function  Esta_Seleccionado(E_Luz: in Elemento_Con_Luz'Class) return Boolean;

   ---------
   -- Luz --
   ---------
   -- Configurar se hereda directamente de Elemento_Con_Luz y no se amplia.
   type Luz is new Elemento_Con_Luz with private;
   type Luz_Ref is access all Luz'Class;
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
      Color_Elemento_Seleccion  : in Color      := Verde_Claro);
   procedure Dibujar (L: in Luz);
   procedure Seleccionar(L: in Luz_Ref);
   procedure Anular_Seleccion (L: in Luz_Ref);

   -----------------------
   -- Boton (abstracto) --
   -----------------------
   type Boton is abstract new Elemento_Con_Luz with private;
   type Boton_Ref is access all Boton'Class;

   -- Procedimiento_Nulo: procedimiento asignado por defecto
   -- a cada bot�n (no hace nada)
   procedure Procedimiento_Nulo(Codigo: Tipo_Id);

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
      Procedimiento_Presion     : in Presion_Ref    := Procedimiento_Nulo'Access;
      Procedimiento_Liberacion  : in Liberacion_Ref := Procedimiento_Nulo'Access);

   procedure Borrar(B: in Boton);

   function Esta_Presionado (B: in Boton'Class) return Boolean;

   --  Los subprogramas abstractos deben ser visibles {RM 3.9.3(10)}
   function Esta_Apuntado
     (B: in Boton; P: in Punto)
      return Boolean is abstract;

   --------------------
   -- Boton Circular --
   --------------------
   type Boton_Circular is new Boton with private;
   type Boton_Circular_Ref is access all Boton_Circular'Class;
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
      Procedimiento_Liberacion  : in Liberacion_Ref     := Procedimiento_Nulo'Access);

   procedure Dibujar (B: in Boton_Circular);
   procedure Seleccionar(B: in Boton_Circular_Ref);
   procedure Anular_Seleccion (B: in Boton_Circular_Ref);

   -----------------------
   -- Boton Rectangular --
   -----------------------
   type Boton_Rectangular is new Boton with private;
   type Boton_Rectangular_Ref is access all Boton_Rectangular'Class;
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
      Procedimiento_Presion     : in Presion_Ref    := Procedimiento_Nulo'Access;
      Procedimiento_Liberacion  : in Liberacion_Ref := Procedimiento_Nulo'Access);

   procedure Dibujar (B: in Boton_Rectangular);
   procedure Seleccionar(B: in Boton_Rectangular_Ref);
   procedure Anular_Seleccion (B: in Boton_Rectangular_Ref);

   -------------------------------
   -- Inicializaci�n de puntero --
   -------------------------------
   protected Puntero is
      procedure Inicializa;
      function Esta_Inicializado return Boolean;
   private
      Puntero: Boolean:= False;
   end Puntero;

private
   ----------------------------------------------------------
   -- Cola de �rdenes de Dibujo de los elementos del panel --
   ----------------------------------------------------------
   Tamano_Cola_Ordenes : constant := 256;
   package Cola_Ordenes is new Cola(Elemento_Panel_Ref,Tamano_Cola_Ordenes);
   -- P.ej. para insertar un elemento:
   -- Cola_Ordenes.Protegida.Inserta (<Elemento>)

   ------------------------------------------------
   -- Elemento del Panel (padre de la jerarqu�a) --
   ------------------------------------------------
   type Elemento_Panel is abstract tagged record
      Esquina_Sup_Izda  : Punto;
      Esquina_Inf_Dcha  : Punto;
      Color_Marco       : Color;
      Texto             : Interfaces.C.Strings.Chars_Ptr;
      Posicion_Texto    : Punto;
      Color_Texto       : Color;
      Color_Fondo_Texto : Color;
      Id                : Tipo_Id;
   end record;

   ----------------------
   -- Display Numerico --
   ----------------------
   type Display_Numerico is new Elemento_Panel with record
      Cifra             : Tipo_Numero; -- es de tipo 'Natural'
      Color_Cifra       : Color;
      Color_Fondo_Cifra : Color;
      Encendido         : Boolean := False;
      Numero_Digitos    : Tipo_N_Digitos;
   end record;

   ----------------------------------------------------------------------
   -- Subprogramas privados de la clase Display,
   -- que ser�n utilizados por los procedimientos de dibujo del display
   subtype Tipo_Segmento is Integer range 1..7;
   subtype Tipo_Digito is Integer range 0..9;
   procedure Dibuja_Segmento(N_Segmento: in Tipo_Segmento;
                             C: in Color;
                             P: in Punto);
   procedure Dibuja_Digito(D: in Tipo_Digito;
                           C: Color;
                           P: in Punto);
   procedure Dibuja_Numero(N: in Tipo_Numero;
                           N_Digitos: in Tipo_N_Digitos;
                           C: Color;
                           P: in Punto);

   -----------------------------------
   -- Elemento Luminoso (abstracto) --
   -----------------------------------
   type Elemento_Con_Luz is abstract new Elemento_Panel with record
      Color_Normal      : Color;
      Color_Seleccion   : Color;
      Seleccionado      : Boolean := False;
   end record;

   ---------
   -- Luz --
   ---------
   type Luz is new Elemento_Con_Luz with null record;

   -----------
   -- Boton --
   -----------
   type Boton is abstract new Elemento_Con_Luz with record
      Color_Presion     : Color;
      Presionado        : Boolean := False;
      Medio_Presionado  : Boolean := False;
      Enclavado         : Boolean;
      Iluminacion_Auto  : Boolean;
      Presiona          : Presion_Ref;
      Libera            : Liberacion_Ref;
   end record;
   ----------------------------------------------------------------------
   -- Subprogramas privados de la clase Boton,
   -- que ser�n utilizados por la tarea que usa el driver del rat�n

   function Esta_Medio_Presionado (B: in Boton'Class) return Boolean;
   function Esta_Enclavado  (B: in Boton'Class) return Boolean;
   procedure Presionar(B: in out Boton'Class);
   procedure Liberar(B: in out Boton'Class);

   type Boton_Circular is new Boton with null record;

   function Esta_Apuntado
     (B: in Boton_Circular; P: in Punto)
      return Boolean;

   type Boton_Rectangular is new Boton with null record;

   function Esta_Apuntado
     (B: in Boton_Rectangular; P: in Punto)
      return Boolean;

   --------------------------------
   -- Tratamiento de la pantalla --
   --------------------------------
   subtype Coordenada is Interfaces.C.Unsigned;

   Maxima_X     : constant Coordenada := 639;
   Maxima_Y     : constant Coordenada := 479;

   subtype Fila is Coordenada range 0..47;
   subtype Columna is Coordenada range 0..63;

   type Matriz_De_Pantalla is array (Fila,Columna) of Boton_Ref;

   protected Matriz_Pantalla is
      pragma priority(22);
      procedure Inicializa;

      function Celda_Ya_Asignada(F: in Fila; C: in Columna) return Boolean;

      procedure Asigna(Ref: Boton_Ref;F: in Fila; C: in Columna);

      procedure Elimina(F: in Fila; C: in Columna);

      function Obtener_Celda(P: in Punto) return Boton_Ref;
   private
      Pantalla : Matriz_De_Pantalla;
   end Matriz_Pantalla;

   -- Funciones Auxiliares para la gesti�n del puntero.

   type Matrix is array
     (Natural range <>,
      Natural range <>) of Interfaces.C.Int;

   procedure Copia_Zona (M: in out Matrix; P_Ini : in Punto);
   procedure Pega_Zona  (M: in out Matrix; P_Ini : in Punto);
   procedure Situa_Puntero_Normal   (P_Director: in Punto);
   procedure Situa_Puntero_Seleccion(P_Director: in Punto);

   -- Colocaci�n del puntero y gesti�n de eventos de movimiento del rat�n
   procedure Dibuja_Puntero
     (M: in out Matrix; P: in Punto; P_Ant: in Punto; Refresca: in Boolean);
   -- Gestion de la liberaci�n de bot�n izqdo.
   procedure Gestiona_Liberacion (P: in Punto;
                                  Invalidado : in out Boolean;
                                  M: in out Matrix);
   -- Gestion de la presi�n del bot�n izqdo.
   procedure Gestiona_Presion (P: in Punto;
                               Invalidado : in out Boolean;
                               M: in out Matrix);

   -------------------------------------
   -- TAREA DE GESTI�N DE LA PANTALLA --
   -------------------------------------
   task Gestiona_Pantalla is
        pragma priority(80);
   end Gestiona_Pantalla;

end Panel;
