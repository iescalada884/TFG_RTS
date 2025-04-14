package Configuracion is
  
   package IO_Digital is

      Num_Canales_Entrada : constant := 48; -- DAVID cambiado valor anterior 48 -- 100 en dataseet
      Num_Canales_Salida  : constant := 48; -- DAVID cambiado valor anterior 48 -- 100 en datasheet

      type Valor_Digital  is range 0..1;

      type Estado_Digital is (ASERTADO,NO_ASERTADO);
      -- El valor Asertado corresponde a los valores:
      --       Estado_Interruptor'Encendido
      --       Activar_Desactivar'Activar
      -- El valor No_Asertado corresponde a los valores contrarios

      type Canal_Entrada is range 0..Num_Canales_Entrada-1;
      type Canal_Salida  is range 0..Num_Canales_Salida-1;

      function Tiempo_Muestreo_IO_Dig return Tiempo;

      function Valor_Inicial_Salida_Digital (Canal : in Canal_Salida)
         return Valor_Digital;

      function Valor (Est : Estado_Digital;
         Canal : Canal_Salida ) return Valor_Digital;

      function Estado (Val : Valor_Digital;
         Canal : Canal_Entrada ) return Estado_Digital;

   end IO_Digital;

   package IO_Analogica is

      Ch_Analog_Out : constant := 1; -- Canal utilizado en el armario
      
      Num_Conv_Entrada : constant := 16;
      Num_Conv_Salida  : constant := 16;  -- DAVID cambiado antiguo valor 8

      type Canal_Entrada is range 0..Num_Conv_Entrada-1;
      type Canal_Salida  is range 0..Num_Conv_Salida-1;

      type Unidades_Convertidor is range -2**31..2**31-1; -- DAVID cambiado antiguo valor 15
      ----------
      -- Consignas de velocidad de los servos --.*DML* -- Tipo declarado en brazo.ads; lo declaro aqui para test_eje6
      type Consignas_Servos is array
        (Configuracion.Numero_Ejes_Controlables) of
        Configuracion.IO_Analogica.Unidades_Convertidor;
      ----------
      -- Existe otro Consignas_Servos en Brazo.Actuadores
      type IOA_Consignas_Servos is array (Numero_Ejes_Controlables)
      of Unidades_Convertidor;

      function Tiempo_Muestreo_IO_Ana return Tiempo;

      function Max_Entrada_Analogica return Unidades_Convertidor;

      function Min_Entrada_Analogica return Unidades_Convertidor;

      function Max_Salida_Analogica return Unidades_Convertidor;

      function Min_Salida_Analogica return Unidades_Convertidor;

      function Max_Consigna_Velocidad return IOA_Consignas_Servos;

      function Valor_Inicial_Salida_Analogica (Canal : in Canal_Salida)
         return Unidades_Convertidor;

      function Limite_Sup_Salida_Analogica (Canal : in Canal_Salida)
         return Unidades_Convertidor;

      function Limite_Inf_Salida_Analogica (Canal : in Canal_Salida)
         return Unidades_Convertidor;

      function Unidad_Usuario
        (U_Convertidor : in Unidades_Convertidor;
         Canal: in Canal_Entrada) return Parametro_Real;

      function Unidad_Convertidor
        (U_Usuario : in Parametro_Real;
         Canal: in Canal_Salida) return Unidades_Convertidor;

   end IO_Analogica;

end Configuracion;
