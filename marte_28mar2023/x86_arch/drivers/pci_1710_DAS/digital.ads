---------------------------------------------------------------------
--
-- Identificador:       DIGITAL
--
-- Tipo de componente:  PAQUETE ADA (Especificacion)
--
-- Modulo:              Digital.ads
--
-- Programador:         Sergio Martin
--
-- Fecha:               03/09/2008
--
-- Funciones que realiza:
--   Paquete que encapsula el manejo de la tarjeta Pci_1753
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Modificaciones:
--    1) Fecha:
--       Programador:
--       Descripcion:
---------------------------------------------------------------------

with Configuracion;        use  Configuracion;

package Digital is

   -- Se elevada como consecuencia de un error
   --      en la llamada del driver.
   -- Tambien se eleva en el caso de producirse un
   --      error de escritura (Pon_Canal) o lectura (Lee_Canal) al
   --      acceder a los registros del converitidor.
   Error_Drv_Digital : exception;

   type Error_Instalacion is
     (Tarjeta_Bien_Instalada,
      No_Tarjeta_En_Direc,
      No_Area_Swsm);

   type Tarjeta is range 0..1;

   -----------------------------------------------------------------------------
   -- Pon_Canal
   -- Pone el canal de salida indicado por "bit" al valor indicado por "valor".
   -----------------------------------------------------------------------------
   procedure Pon_Canal
     (Canal : Io_Digital.Canal_Salida;
      Valor : Io_Digital.Valor_Digital);

   -----------------------------------------------------------------------------
   -- Lee_Canal
   -- Retorna el valor logico leido en el canal de entrada indicado por "bit".
   -----------------------------------------------------------------------------
   function Lee_Canal
     (Canal : Io_Digital.Canal_Entrada)
      return Io_Digital.Valor_Digital;

   -----------------------------------------------------------------------------
   -- Cierra
   -- Cierra el fichero de dispositivo. Debe usarse cuando se ha acabado de
   -- usar el paquete (el fichero se abre al instanciarse el paquete).
   -----------------------------------------------------------------------------
   procedure Cierra;

   -----------------------------------------------------------------------------
   -- Estado_Instalacion_Tarjeta
   -- Este procedimiento devuelve informacion para conocer el estado de
   -- instalacion de la tarjeta indicada por "t". En "error" retorna el error
   -- que se produjo durante la instalacion:
   --   * TARJETA_BIEN_INSTALADA: ningun error.
   --   * NO_TARJETA_EN_DIREC: la tarjeta no esta en la direccion esperada.
   --   * NO_AREA_SWSM: aunque esta en la direccion esperada no hay un area de
   --     swsm que "cubra" dicha direccion.
   -- En "dir" se retorna la direccion en la que esta o deberia estar la
   -- tarjeta.
   -----------------------------------------------------------------------------
   procedure Estado_Instalacion_Tarjeta
     (T : Tarjeta;
      Error : out Error_Instalacion;
      Dir : out Integer);

end Digital;

