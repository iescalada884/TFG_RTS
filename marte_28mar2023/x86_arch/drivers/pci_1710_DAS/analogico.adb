---------------------------------------------------------------------
--
-- Identificador:       ANALOGICO
--
-- Tipo de componente:  PAQUETE ADA (Cuerpo)
--
-- Modulo:              Analogico.adb
--
-- Programador:         Sergio Martin
--
-- Fecha:               03/09/2008
--
-- Funciones que realiza:
--   Paquete con el manejo de las tarjetas análogicas
--   pci_1753 y pci_6713
--
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Modificaciones:
--    1) Fecha:
--       Programador:
--       Descripcion:
---------------------------------------------------------------------
with Analogico_1710;      --  Lectura
--with Analogico_6713;      --  Escritura es manual

with Ada.Text_IO;

with Configuracion;               use Configuracion;

package body Analogico is

   use IO_Analogica;

   -----------------------
   -- Lee_Canal_Entrada --
   -----------------------
   function Lee_Canal_Entrada
     (Canal: in IO_Analogica.Canal_Entrada)
     return IO_Analogica.Unidades_Convertidor is
   begin
      return Analogico_1710.Lee_Canal_Entrada(Canal);
   end Lee_Canal_Entrada;

   ----------------------
   -- Pon_Canal_Salida --   DUMMY
   ----------------------
   procedure Pon_Canal_Salida
     (Canal: in IO_Analogica.Canal_Salida;
      Valor: in IO_Analogica.Unidades_Convertidor) is
   begin
      --      Analogico_6713.Pon_Canal_Salida(Canal, Valor);
      null;
   end Pon_Canal_Salida;

   ------------
   -- Cierra --   DUMMY
   ------------
   procedure Cierra is
   begin
      null;
   end Cierra;

   --------------------------------
   -- Estado_Instalacion_Tarjeta --   DUMMY
   --------------------------------
   -- Este procedimiento devuelve informacion para conocer el estado de
   -- instalacion de la tarjeta. En "error" retorna el error que se produjo
   -- durante la instalacion:
   --   - TARJETA_BIEN_INSTALADA: ningun error.
   --   - NO_TARJETA_EN_DIREC: la tarjeta no esta en la direccion esperada.
   --   - NO_AREA_SWSM: aunque esta en la direccion esperada no hay un area de
   --     swsm que "cubra" dicha direccion.
   -- En "direc" se retorna la direccion en la que esta o deberia estar la
   -- tarjeta.
   procedure Estado_Instalacion_Tarjeta (Error: out Error_Instalacion;
         Direc:out Integer) is
      Err : Integer;
   begin
      Err:= 0;
      Direc:= Integer(0);
      case (Err) is
         when  0 =>
            Error:= Tarjeta_Bien_Instalada;
         when  1 =>
            Error:= No_Tarjeta_En_Direccion;
         when  2 =>
            Error:= No_Area_Swsm;
         when others =>
            raise Error_Drv_Analogico;
            -- Normalmente sera -1. error en la llamada a ioctl
      end case;
   end Estado_Instalacion_Tarjeta;

end Analogico;

