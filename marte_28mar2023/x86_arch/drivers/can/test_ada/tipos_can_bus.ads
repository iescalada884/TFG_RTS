--with Tipos_Interfaz_CAN_Advantech;
with System;

--------------------------------------------------------------------------------
-- Tipos_Can_Bus
-- <summary>Paquete para la definicion de los tipos y excepciones manejadas
-- en la operacion sobre el bus CAN.</summary>
-- <parameter name="@autor">Pablo Gutierrez Peon - Universidad de Cantabria
-- </parameter>
-- <parameter name="@fecha">23/12/13</parameter>
--------------------------------------------------------------------------------
package Tipos_Can_Bus is

   -- Manejador para operar sobre CAN.
   type Handle is new System.Address; --Tipos_Interfaz_CAN_Advantech.C_VoidPtr;

   -- Identificador del puerto CAN de la tarjeta.
   subtype Port is Natural range 0..1;

   -- Tasas de transmision en kbps admitidas por el servo.
   type Baudrate is (b1000, b500, b250, b125);

   -- Timeouts de espera.
   subtype Timeout is Natural range 0 .. 65535;

   -- Identificador en los mensajes CAN (COB+ID).
   subtype Can_Id is Natural range 0 .. 16#7FF#;

   -- Longitud del mensaje.
   subtype Message_Length is Natural range 0 .. 8;

   -- Indicar si es peticion RTR.
   subtype Remote_Request is Boolean;

   -- Longitud del mensaje.
   subtype Byte_Length is Natural range 0 .. 255;

   -- Datos transmitidos en un mensaje.
   type Chain is array (1 .. 8) of Byte_Length;

   -- Mensaje CAN.
   type Can_Message is record
      Id      : Can_Id; -- COB+ID
      Length  : Message_Length;
      RTR     : Remote_Request := False;
      Message : Chain;
   end record;

   -- Error al abrir el puerto CAN.
   Open_Port_Error : exception;

   -- Error al entrar en el modo reset del driver CAN necesario para realizar
   -- la comunicacion.
   Enter_Reset_Mode_Error : exception;

   -- Error al establecer la tasa de bits (probablemente no sea una de las
   -- soportadas por el dispositivo).
   Set_Bit_Rate_Error : exception;

   -- Error al establecer el timeout de envio y recepcion.
   Set_Timeout_Error : exception;

   -- Error al entrar en el modo de trabajo del driver CAN necesario para
   -- hacer realizar la comunicacion.
   Enter_Work_Mode_Error : exception;

   -- Error indefinido.
   Unknown_Error : exception;

   -- Error al cerrar la interfaz de red.
   Close_Net_Error : exception;

   -- Error generado cuando el mensaje no ha podido ser transmitido.
   Message_Not_Transmitted : exception;

   -- Error al expirar el tiempo de envio del mensaje.
   Write_Timeout_Error : exception;

   -- Error por entrada/salida pendiente.
   IO_Pending_Error : exception;

   -- Error por parametro invalido en llamada a funcion.
   Invalid_Parameter_Error : exception;

   -- Error por operacion de envio abortada.
   Operation_Aborted_Error : exception;

   -- Error por fallo generico en el envio.
   Gen_Failure_Error : exception;

   -- Error por el identificador CAN recibido.
   Identifier_Error : exception;

   -- Error generado cuando el numero de mensajes recibidos no se corresponde
   -- con el esperado.
   Number_Of_Messages_Error : exception;

   -- Error al expirar el tiempo de recepcion del mensaje.
   Read_Timeout_Error : exception;

end Tipos_Can_Bus;
