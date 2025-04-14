---------------------------------------------------------------------
--
-- Identificador:       ANALOGICO_1710
--
-- Tipo de componente:  PAQUETE ADA (Especificacion)
--
-- Modulo:              Analogico_1710.ads
--
-- Programador:         Sergin Martin
--
-- Fecha:               03/08/2008
--
-- Funciones que realiza:
--   Paquete con el manejo de la tarjeta pci_1710.
--   Lectura analógica 
--
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Modificaciones:
--    1) Fecha:
--       Programador:
--       Descripcion:
---------------------------------------------------------------------

with Configuracion;       use Configuracion;

package Analogico_1710 is
   package IOA renames Configuracion.IO_Analogica;

   --  Elevada como consecuencia de un error en la llamada del driver.
   Error_Drv_Analogico : exception;

   --  Se eleva cuando la consigna que se va escribir en un canal 
   --  (tipo IO_Analogica.Unidades_Convertidor) se sale del rango 
   --  permitido por la tarjeta (-2048..2047 en complemento a 2). 
   --  El valor es escrito de todas formas pero recortado.
   Consigna_Fuera_De_Rango : exception;

   --  Errores posibles en la instalacion de la tarjeta
   type Error_Instalacion is
     (Tarjeta_Bien_Instalada,
      No_Tarjeta_En_Direccion,
      No_Area_Swsm);

   -- DUMMY --
   valores : array (IOA.Canal_Entrada) of IOA.Unidades_Convertidor;

   ----------------------------------------------------------------------------
   --  Lee un canal de entrada de modo individual. 
   --  EXCEPCIONES :
   --      - Error_Drv_Analogico: Es elevada como consecuencia de un
   --      error en la llamada a la funcion Ioctl del driver.
   ----------------------------------------------------------------------------
   function Lee_Canal_Entrada
     (Canal: IO_Analogica.Canal_Entrada)
     return IO_Analogica.Unidades_Convertidor;

   ----------------------------------------------------------------------------
   --  Cierra el fichero de dispositivo. 
   --     #### DUMMY ####
   ----------------------------------------------------------------------------
   procedure Cierra;

   ----------------------------------------------------------------------------
   --  Este procedimiento devuelve informacion para conocer el
   --  estado de instalacion de la tarjeta.
   --  EXCEPCIONES :
   --      - Error_Drv_Analogico: Es elevada como consecuencia de un
   --      error en la llamada a la funcion Ioctl del driver.
   --     #### DUMMY ####
   ----------------------------------------------------------------------------
   procedure Estado_Instalacion_Tarjeta
     (Error: out Error_Instalacion;
      Direc: out Integer);

end Analogico_1710;

