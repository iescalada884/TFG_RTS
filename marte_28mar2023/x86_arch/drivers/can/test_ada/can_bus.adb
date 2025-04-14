--with Funciones_Interfaz_CAN_Advantech;
with Drivers_MaRTE;
with Can_Driver_Import;

with Interfaces.C.Strings;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System;

-------------
-- Can_Bus --
-------------
package body Can_Bus is

   package TIO renames Ada.Text_IO;

   type CAN_IOCTL_CMD is (CAN_IOCTL_SET_FILTERS, -- set filters (struct ioctl_filters_t)
                          CAN_IOCTL_ABORT_FRAME, -- abort frame
                          CAN_IOCTL_SET_RX_HOOK, -- set hook "a frame was received"
                          CAN_IOCTL_SET_TX_HOOK, -- set hook "a frame was transmited"
                          CAN_IOCTL_SET_AB_HOOK); -- set hook "a frame was aborted"

   for CAN_IOCTL_CMD use (CAN_IOCTL_SET_FILTERS => 2#0000_0001#,
                          CAN_IOCTL_ABORT_FRAME => 2#0000_0010#,
                          CAN_IOCTL_SET_RX_HOOK => 2#0000_0100#,
                          CAN_IOCTL_SET_TX_HOOK => 2#0000_1000#,
                          CAN_IOCTL_SET_AB_HOOK => 2#0001_0000#);

   type CAN_Data_Type is array (1..8) of Interfaces.Unsigned_8;
   type CAN_Frame is
      record
         Is_Extended_Format : Interfaces.C.int;
         Is_RTR : Interfaces.C.int;
         Id : Interfaces.Unsigned_32;
         DLC : Interfaces.Unsigned_8;
         Data : CAN_Data_Type;
         Poll_Pos : Interfaces.C.int;
      end record;
   type Can_Frame_Ac is access all CAN_Frame;

   function Can_Frame_To_Buffer is
     new Ada.Unchecked_Conversion (Can_Frame_Ac, Drivers_Marte.Buffer_Ac);
   function Address_To_Buffer is
     new Ada.Unchecked_Conversion (System.Address, Drivers_Marte.Buffer_Ac);


   -- #### PROTECTED ####
   Full_Pool : exception;
   POOL_CAPACITY : constant Natural := 10;
   type Data_Pool is array (0..POOL_CAPACITY-1) of Tipos_Can_Bus.Can_Message;

   protected Receiver_Pool is
      procedure Write (Dato :  in Tipos_Can_Bus.Can_Message);
      entry Read  (Dato : out Tipos_Can_Bus.Can_Message);
   private
      Recv_Pool : Data_Pool;
      Recv_Pool_Write_I : Natural := 0;
      Recv_Pool_Read_I  : Natural := 0;
      Recv_Pool_Count   : Natural := 0;
   end Receiver_Pool;

   protected body Receiver_Pool is
      procedure Write (Dato :  in Tipos_Can_Bus.Can_Message) is
      begin
         if Recv_Pool_Count >= POOL_CAPACITY then
            raise Full_Pool;
         end if;
         Recv_Pool (Recv_Pool_Write_I) := Dato;
         Recv_Pool_Write_I := (Recv_Pool_Write_I +1) mod POOL_CAPACITY;
         Recv_Pool_Count   := Recv_Pool_Count   +1;
      exception
         when e:others =>
            TIO.Put_Line ("RP.write " & Ada.Exceptions.Exception_Name(e) &" "&
                            Ada.Exceptions.Exception_Message(e) &" "&
                            Ada.Exceptions.Exception_Information(e));
      end Write;

      entry Read (Dato : out Tipos_Can_Bus.Can_Message)
        when Recv_Pool_Count > 0 is
      begin
         Dato := Recv_Pool (Recv_Pool_Read_I);
         Recv_Pool_Read_I := (Recv_Pool_Read_I +1) mod POOL_CAPACITY;
         Recv_Pool_Count  := Recv_Pool_Count  -1;
      exception
         when e:others =>
            TIO.Put_Line ("RP.read " & Ada.Exceptions.Exception_Name(e) &" "&
                            Ada.Exceptions.Exception_Message(e) &" "&
                            Ada.Exceptions.Exception_Information(e));
      end Read;
   end Receiver_Pool;

   function CAN_Receiver_Hook (Chip : Drivers_MaRTE.Buffer_Ac;
                               Frame : CAN_Frame_Ac)
                               return Interfaces.C.int is
      function Free_Pool (F: CAN_Frame_Ac) return Interfaces.C.int;
      Pragma Import (C, Free_Pool, "can_framespool_free");
      ret : Interfaces.C.int;
      Message : Tipos_Can_Bus.Can_Message;
   begin
      null;
      Message.Id     := Integer (Frame.Id);
      Message.Length := Integer (Frame.DLC);
      if Interfaces.C."="(Frame.Is_RTR, 0) then
         Message.RTR  := False;
      else
         Message.RTR := True;
      end if;

      for i in 1..Frame.DLC loop
         Message.Message(Integer(i)) := Integer (Frame.Data(Integer(i)));
      end loop;

      begin
         Receiver_Pool.Write (Message);
         TIO.Put_Line ("=Frame= " & Frame.Id'Img &"."& Frame.DLC'Img
                       & "(Data: "& Frame.Data(1)'Img
                       &" "& Frame.Data(2)'Img
                       &" "& Frame.Data(3)'Img
                       &" "& Frame.Data(4)'Img
                       &") "& Frame.Is_Extended_Format'Img
                      );
      exception
         when Full_Pool =>
            TIO.Put_Line ("Pool lleno");
         when e:others =>
            TIO.Put_Line ("hook " & Ada.Exceptions.Exception_Name(e) &" "&
                            Ada.Exceptions.Exception_Message(e) &" "&
                            Ada.Exceptions.Exception_Information(e));
      end;

      ret := Free_Pool (Frame);
      return 0;

   exception
      when e:others =>
         TIO.Put_Line ("Hook " & Ada.Exceptions.Exception_Name(e) &" "&
                         Ada.Exceptions.Exception_Message(e) &" "&
                         Ada.Exceptions.Exception_Information(e));
         return Interfaces.C.int (-1);
   end CAN_Receiver_Hook;


   ----------
   -- Open --
   ----------
   function Open (Network : in Tipos_Can_Bus.Port;
                  The_Baudrate : in Tipos_Can_Bus.Baudrate := Tipos_Can_Bus.b500;
                  Txtimeout : in Tipos_Can_Bus.Timeout := 3000;
                  Rxtimeout : in Tipos_Can_Bus.Timeout := 3000)
                  return Tipos_Can_Bus.Handle is

      --------------------------------------------------------------------------
      -- Test_Baudrate
      -- <summary>Devuelve el valor numerico de la tasa de bits que se este
      -- manejando.</summary>
      -- <parameter name="@return">Valor numerico de la tasa de bits.
      -- </parameter>
      --------------------------------------------------------------------------
      function Test_Baudrate return Interfaces.C.unsigned is
      begin
         case The_Baudrate is
            when Tipos_Can_Bus.b1000 =>
               return 1000;
            when Tipos_Can_Bus.b500 =>
               return 500;
            when Tipos_Can_Bus.b250 =>
               return 250;
            when Tipos_Can_Bus.b125 =>
               return 125;
         end case;
      end Test_Baudrate;

      use type Interfaces.C.int;
      Err : Interfaces.C.int;
      Valor_Manejador : aliased Integer := 0;
      Manejador_Dispositivo : aliased System.Address := Valor_Manejador'Address;
   begin

      Err := Can_Driver_Import.Ioctl (Drivers_MaRTE.File_Descriptor (Network),
                        4,
                        Address_To_Buffer (CAN_Receiver_Hook'Address));

      -- Network should be zero.
      Err := Can_Driver_Import.Open (Drivers_MaRTE.File_Descriptor (Network),
                                     Drivers_MaRTE.READ_WRITE);

--        Err := Funciones_Interfaz_CAN_Advantech.
--          Abrir (Nombre_Puerto         => Interfaces.C.To_C ("can" &
--                 Ada.Strings.Fixed.Trim (Network'Img, Ada.Strings.Left), True),
--                 Sincrono              => 0,
--                 Tasa_Bits             => Test_Baudrate,
--                 Timeout_Lectura       => Tipos_Interfaz_CAN_Advantech.C_ULong (Txtimeout),
--                 Timeout_Escritura     => Tipos_Interfaz_CAN_Advantech.C_ULong (Rxtimeout),
--                 Manejador_Dispositivo => Manejador_Dispositivo'Unchecked_Access);
      if Err /= 0 then
         if Err = -1 then
            raise Tipos_Can_Bus.Open_Port_Error;
         elsif Err = -2 then
            raise Tipos_Can_Bus.Enter_Reset_Mode_Error;
         elsif Err = -3 then
            raise Tipos_Can_Bus.Set_Bit_Rate_Error;
         elsif Err = -4 then
            raise Tipos_Can_Bus.Set_Timeout_Error;
         elsif Err = -5 then
            raise Tipos_Can_Bus.Enter_Work_Mode_Error;
         else
            raise Tipos_Can_Bus.Unknown_Error;
         end if;
      end if;
      return Tipos_Can_Bus.Handle (Manejador_Dispositivo);
   end Open;

   -----------
   -- Close --
   -----------
   procedure Close (The_Handle : in Tipos_Can_Bus.Handle) is
      use type Interfaces.C.int;
      Err : Interfaces.C.int;
      Network : Tipos_Can_Bus.Port := 0;
   begin
      null;

      Err := Can_Driver_Import.Close (Drivers_MaRTE.File_Descriptor (Network));
--        Err := Funciones_Interfaz_CAN_Advantech.Cerrar
--          (Manejador_Dispositivo => Tipos_Interfaz_CAN_Advantech.C_VoidPtr(The_Handle));
      if Err /= 0 then
         raise Tipos_Can_Bus.Close_Net_Error;
      end if;
   end Close ;

   ----------
   -- Send --
   ----------
   procedure Send (The_Handle : in Tipos_Can_Bus.Handle;
                   Message : in Tipos_Can_Bus.Can_Message) is
      use type Interfaces.C.int;
      Err : Interfaces.C.int;
      Network : Tipos_Can_Bus.Port := 0;

      Frame   : CAN_Frame_Ac := new CAN_Frame;
      Buff_Ac : Drivers_MaRTE.Buffer_Ac;
      Buff_L  : Drivers_MaRTE.Buffer_Length;
   begin
      null;
      for I in 1 .. Message.Length loop
         Frame.Data (i) := Interfaces.Unsigned_8 (Message.Message (I));
--           Datos_Enviar (Interfaces.C.size_t(I)) :=
--             Interfaces.C.To_C (Character'Val (Message.Message (I)));
      end loop;
      Frame.Is_Extended_Format := Interfaces.C.int (0);
      Frame.Id := Interfaces.Unsigned_32 (Message.Id);
      Frame.DLC := Interfaces.Unsigned_8 (Message.Length);
      case Message.RTR is
         when True  => Frame.Is_RTR := Interfaces.C.int (1);
         when False => Frame.Is_RTR := Interfaces.C.int (0);
      end case;

      Buff_Ac := Can_Frame_To_Buffer (Frame);
      Buff_L  := Drivers_MaRTE.Buffer_Length (CAN_Frame'Size / 8);
      Err := Can_Driver_Import.Write (Drivers_MaRTE.File_Descriptor (Network),
                                      Buff_Ac,
                                      Buff_L);

--        Err := Funciones_Interfaz_CAN_Advantech.
--          Enviar_Mensaje (Manejador_Dispositivo => Tipos_Interfaz_CAN_Advantech.C_VoidPtr (The_Handle),
--                          Cob_Id                => Interfaces.C.unsigned_long (Message.Id),
--                          Datos                 => Datos_Enviar,
--                          Longitud_Datos        => Interfaces.C.short (Message.Length));

      if Err /= 0 then
         if Err = -1 then
            raise Tipos_Can_Bus.Message_Not_Transmitted;
         elsif Err = -2 then
            raise Tipos_Can_Bus.Write_Timeout_Error;
         elsif Err = -3 then
            raise Tipos_Can_Bus.IO_Pending_Error;
         elsif Err = -4 then
            raise Tipos_Can_Bus.Invalid_Parameter_Error;
         elsif Err = -5 then
            raise Tipos_Can_Bus.Operation_Aborted_Error;
         elsif Err = -6 then
            raise Tipos_Can_Bus.Gen_Failure_Error;
         elsif Err = -7 then
            raise Tipos_Can_Bus.Unknown_Error;
         elsif Err > 0 then
            null; -- OK -> cantidad de Bytes enviados
         else
            raise Tipos_Can_Bus.Unknown_Error;
         end if;
      end if;
   exception
      when e:others =>
         TIO.Put_Line ("Send " & Ada.Exceptions.Exception_Name(e) &" "&
                         Ada.Exceptions.Exception_Message(e) &" "&
                         Ada.Exceptions.Exception_Information(e));
   end Send;

   -------------
   -- Receive --
   -------------
   procedure Receive (The_Handle : in Tipos_Can_Bus.Handle;
                      Message : out Tipos_Can_Bus.Can_Message) is
      use type Interfaces.C.int;
      Err : Interfaces.C.int := 0;

   begin
--        TIO.Put_Line ("Receive...");
      null;

      Receiver_Pool.Read (Message);
--        TIO.Put_Line ("Received." & Message.Length'Img);

--        -- Inicializa para que el compilador no se queje.
--        for I in 1 .. 8 loop
--           Datos_Recibidos (Interfaces.C.size_t(I)) := Interfaces.C.To_C (Character'Val (16#00#));
--        end loop;

--        -- Recibe el mensaje CAN.
--        Err := Funciones_Interfaz_CAN_Advantech.
--          Recibir_Mensaje (Manejador_Dispositivo => Tipos_Interfaz_CAN_Advantech.C_VoidPtr (The_Handle),
--                           Cob_Id                => Id_Ptr,
--                           Datos                 => Datos_Recibidos,
--                           Longitud_Datos        => Longitud_Datos_Ptr);
      if Err /= 0 then
         if Err = -1 then
            raise Tipos_Can_Bus.Identifier_Error;
         elsif Err = -2 then
            raise Tipos_Can_Bus.Number_Of_Messages_Error;
         elsif Err = -3 then
            raise Tipos_Can_Bus.Read_Timeout_Error;
         elsif Err = -4 then
            raise Tipos_Can_Bus.Unknown_Error;
         else
            raise Tipos_Can_Bus.Unknown_Error;
         end if;
      end if;

--        -- Conforma el mensaje CAN con los datos recibidos.
--        Mensaje_Recibido.Id := Integer (Id_Ptr.all);
--        Mensaje_Recibido.Length := Integer (Longitud_Datos_Ptr.all);
--        for I in 1 .. Mensaje_Recibido.Length loop
--           Mensaje_Recibido.Message (I) := Character'Pos (Interfaces.C.To_Ada(Datos_Recibidos(Interfaces.C.size_t (I))));
--        end loop;
--
--        -- Devuelve el mensaje CAN.
--        Message := Mensaje_Recibido;
   exception
      when e:others =>
         TIO.Put_Line ("Receive " & Ada.Exceptions.Exception_Name(e) &" "&
                         Ada.Exceptions.Exception_Message(e) &" "&
                         Ada.Exceptions.Exception_Information(e));
   end Receive;

end Can_Bus;
