---------------------------------------------------------------------
--
-- Identificador:       ANALOGICO
--
-- Tipo de componente:  PAQUETE ADA (Especificacion)
--
-- Modulo:              Analogico.ads
--
-- Programador:         Sergio Martin
--
-- Fecha:              03/09/2008
--
-- Funciones que realiza:
--   Paquete con el manejo de las tarjetas análogicas
--   pci_1753 y pci_6713
--
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Modificaciones:
--    1) Fecha: 11/02/2015
--       Programador: DGV
--       Descripcion: Omision del controlador de pci_6713
---------------------------------------------------------------------

with Configuracion;                    use Configuracion;

package Analogico is

   --  Elevada como consecuencia de un error en la llamada del driver.
   Error_Drv_Analogico : exception;

   --  Se eleva cuando la consigna que se va escribir en un canal
   --  (tipo IO_Analogica.Unidades_Convertidor) se sale del rango
   --  permitido por la tarjeta (-2048..2047 en complemento a 2).
   --  El valor es escrito de todas formas pero recortado.
   Consigna_Fuera_De_Rango : exception;

   -- Errores posibles en la instalacion de la tarjeta
   type Error_Instalacion is
     (Tarjeta_Bien_Instalada,
      No_Tarjeta_En_Direccion,
      No_Area_Swsm);

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
   --  Pone el canal de salida indicado al voltaje requerido, medido
   --  en unidades del convertidor).
   --  EXCEPCIONES :
   --      - Error_Drv_Analogico: Es elevada como consecuencia de un
   --      error en la llamada a la funcion Ioctl del driver.
   --      - Consigna_Fuera_De_Rango: En el caso de que el valor indicado
   --      se salga del rango valido.
   ----------------------------------------------------------------------------
   procedure Pon_Canal_Salida
     (Canal: in IO_Analogica.Canal_Salida;
      Valor: in IO_Analogica.Unidades_Convertidor);

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

end Analogico;

