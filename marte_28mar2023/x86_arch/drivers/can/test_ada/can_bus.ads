with Tipos_Can_Bus;
--with Tipos_Interfaz_CAN_Advantech;

--------------------------------------------------------------------------------
-- Can_Bus
-- <summary>Paquete que permite el manejo de una interfaz CAN para envio y
-- recepcion de datos.</summary>
-- <parameter name="@autor">Pablo Gutierrez Peon - Universidad de Cantabria
-- </parameter>
-- <parameter name="@fecha">23/12/13</parameter>
--------------------------------------------------------------------------------
package Can_Bus is

   -----------------------------------------------------------------------------
   -- Open
   -- <summary>Permite abrir una interfaz CAN.</summary>
   -- <parameter name="Network">Numero del puerto CAN a abrir (Debe estar
   --  configurado en MaRTE).</parameter>
   -- <parameter name="The_Baudrate">Tasa de bits de transmision de los datos
   -- por el bus CAN en kbps. Por defecto 500 kbps.</parameter>
   -- <parameter name="Txtimeout">Tiempo de espera en milisegundos hasta dar por
   -- fallida una transmision de datos. Por defecto 3000 milisegundos.
   -- </parameter>
   -- <parameter name="Rxtimeout">Tiempo de espera en milisegundos hasta dar por
   -- fallida una recepcion de datos. Por defecto 3000 milisegundos.</parameter>
   -- <parameter name="@return">Manejador del puerto que permite controlar el
   -- dispositivo abierto.</parameter>
   -- <exception>Tipos_Can_Bus.Open_Port_Error</exception>
   -- <exception>Tipos_Can_Bus.Enter_Reset_Mode_Error</exception>
   -- <exception>Tipos_Can_Bus.Set_Bit_Rate_Error</exception>
   -- <exception>Tipos_Can_Bus.Set_Timeout_Error</exception>
   -- <exception>Tipos_Can_Bus.Enter_Work_Mode_Error</exception>
   -- <exception>Tipos_Can_Bus.Unknown_Error</exception>
   -----------------------------------------------------------------------------
   function Open (Network : in Tipos_Can_Bus.Port;
                  The_Baudrate : in Tipos_Can_Bus.Baudrate := Tipos_Can_Bus.b500;
                  Txtimeout : in Tipos_Can_Bus.Timeout := 3000;
                  Rxtimeout : in Tipos_Can_Bus.Timeout := 3000)
                  return Tipos_Can_Bus.Handle;

   -----------------------------------------------------------------------------
   -- Close
   -- <summary>Permite cerrar una interfaz CAN abierta.</summary>
   -- <parameter name="The_Handle">Manejador de la interfaz CAN que se desea
   -- cerrar.</parameter>
   -- <exception>Tipos_Can_Bus.Close_Net_Error</exception>
   -----------------------------------------------------------------------------
   procedure Close (The_Handle : in Tipos_Can_Bus.Handle);

   -----------------------------------------------------------------------------
   -- Send
   -- <summary>Permite enviar un mensaje a traves de la interfaz CAN.</summary>
   -- <parameter name="The_Handle">Manejador del puerto CAN abierto sobre el que
   -- se envian los datos.</parameter>
   -- <parameter name="Message">Mensaje CAN que se quiere transmitir.
   -- </parameter>
   -- <exception>Tipos_Can_Bus.Message_Not_Transmitted</exception>
   -- <exception>Tipos_Can_Bus.Write_Timeout_Error</exception>
   -- <exception>Tipos_Can_Bus.IO_Pending_Error</exception>
   -- <exception>Tipos_Can_Bus.Invalid_Parameter_Error</exception>
   -- <exception>Tipos_Can_Bus.Operation_Aborted_Error</exception>
   -- <exception>Tipos_Can_Bus.Gen_Failure_Error</exception>
   -- <exception>Tipos_Can_Bus.Unknown_Error</exception>
   -----------------------------------------------------------------------------
   procedure Send (The_Handle : in Tipos_Can_Bus.Handle;
                   Message : in Tipos_Can_Bus.Can_Message);

   -----------------------------------------------------------------------------
   -- Receive
   -- <summary>Permite recibir un mensaje a traves de la interfaz CAN.</summary>
   -- <parameter name="The_Handle">Manejador del puerto CAN abierto a traves
   -- del cual se reciben los datos.</parameter>
   -- <parameter name="Message">Mensaje CAN recibido.</parameter>
   -- <exception>Tipos_Can_Bus.Identifier_Error</exception>
   -- <exception>Tipos_Can_Bus.Number_Of_Messages_Error</exception>
   -- <exception>Tipos_Can_Bus.Read_Timeout_Error</exception>
   -- <exception>Tipos_Can_Bus.Unknown_Error</exception>
   -----------------------------------------------------------------------------
   procedure Receive (The_Handle : in Tipos_Can_Bus.Handle;
                      Message : out Tipos_Can_Bus.Can_Message);

end Can_Bus;
