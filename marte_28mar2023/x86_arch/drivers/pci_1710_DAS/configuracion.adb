--with Debug_Marte;                       use Debug_Marte;
package body Configuracion is

   -----------------------------------------------------------------
   -- SE INSTANCIA UN PAQUETE DE FUNCIONES MATEMATICAS
   -----------------------------------------------------------------
   package ang_funciones is new Ada.Numerics.Generic_Elementary_Functions(Angulo);
   use ang_funciones;

   My_exception : exception;

    -------------------------------------------------------------------------------
   package body IO_Digital is
      ---
      function Tiempo_Muestreo_IO_Dig return Tiempo is
      begin
         return V_Tiempo_Muestreo_IO_Dig;
      end Tiempo_Muestreo_IO_Dig;
      ---
      function Valor_Inicial_Salida_Digital (Canal : in Canal_Salida)
                                             return Valor_Digital is
      begin
         return V_Valor_Inicial_Salida_Digital(canal);
      end Valor_Inicial_Salida_Digital;
      ---
      function Valor (Est : Estado_Digital;
                      Canal : Canal_Salida )  return Valor_Digital is
      begin
         if Est = ASERTADO then
            return V_Valor(Canal);
         else
            if V_Valor(Canal)= 0 then
               return 1;
            else
               return 0;
            end if;
         end if;
         --         return V_Valor(Canal);
      end Valor;
      ---
      function Estado (Val : Valor_Digital;
                       Canal : Canal_Entrada ) return Estado_Digital is
      begin
         if Val = 1 then
            return V_Estado(Canal);
         else
            if V_Estado(Canal)= NO_ASERTADO then
               return ASERTADO;
            else
               return NO_ASERTADO;
            end if;
         end if;
         --         return V_Estado(Canal);
      end Estado;
      ---
   end IO_Digital;

   -------------------------------------------------------------------------------
   package body IO_Analogica is
      ---
      function Tiempo_Muestreo_IO_Ana return Tiempo is
      begin
         return V_Tiempo_Muestreo_IO_Ana;
      end Tiempo_Muestreo_IO_Ana;
      ---
      function Max_Entrada_Analogica return Unidades_Convertidor is
      begin
         return V_Max_Entrada_Analogica;
      end Max_Entrada_Analogica;
      ---
      function Min_Entrada_Analogica return Unidades_Convertidor is
      begin
         return V_Min_Entrada_Analogica;
      end Min_Entrada_Analogica;
      ---
      function Max_Salida_Analogica  return Unidades_Convertidor is
      begin
         return V_Max_Salida_Analogica;
      end Max_Salida_Analogica;
      ---
      function Min_Salida_Analogica  return Unidades_Convertidor is
      begin
         return V_Min_Salida_Analogica;
      end Min_Salida_Analogica;
      ---
      function Max_Consigna_Velocidad return IOA_Consignas_Servos is
      begin
         return V_Max_Consigna_Velocidad;
      end Max_Consigna_Velocidad;
      ---
      function Valor_Inicial_Salida_Analogica (Canal : in Canal_Salida)
                                                 return IO_Analogica.Unidades_Convertidor is
      begin
         return V_Val_Ini_Sal_Analogica(Canal);
      end Valor_Inicial_Salida_Analogica;
      ---
      function Limite_Sup_Salida_Analogica (Canal : in Canal_Salida)
                                            return Unidades_Convertidor is
      begin
         return V_Lim_Sup_Sal_Analogica(Canal);
      end Limite_Sup_Salida_Analogica;
      ---
      function Limite_Inf_Salida_Analogica (Canal : in Canal_Salida)
                                            return Unidades_Convertidor is
      begin
         return V_Lim_Inf_Sal_Analogica(Canal);
      end Limite_Inf_Salida_Analogica;
      ---
      function Unidad_Usuario (U_Convertidor : in Unidades_Convertidor;
                               Canal: in Canal_Entrada) return Parametro_Real is
      begin
         return (Parametro_Real(U_Convertidor)*V_Ganancia_Conv_Ent(Canal)+
                   V_Offset_Conv_Ent(Canal));
      end Unidad_Usuario;
      ---
      function Unidad_Convertidor (U_Usuario : in Parametro_Real;
                                   Canal: in Canal_Salida) return Unidades_Convertidor is
      begin
         return (Unidades_Convertidor((U_Usuario - V_Offset_Conv_Sal(Canal)) /
                   V_Ganancia_Conv_Sal(Canal)));
      end Unidad_Convertidor;

   end IO_Analogica;

   -------------------------------------------------------------------------------

end Configuracion;
