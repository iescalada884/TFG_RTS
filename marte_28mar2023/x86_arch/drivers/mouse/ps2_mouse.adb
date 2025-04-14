------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V1.58  Sep 2006
--
--                            'P S / 2   M o u s e'
--
--                                   Body
--
--
--  File 'ps2_mouse.adb'                                               By AMC.
--
--  -- Information::
--  -- Body for the PS/2 Mouse Driver
--  ---------------------------------------------------------------------------
--   Copyright (C) 2003   Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@ctr.unican.es
--                      Michael Gonz�lez Harbour      mgh@ctr.unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------
with MaRTE.Direct_IO;

package body PS2_Mouse is
   package KC renames MaRTE.Direct_IO;
   use type Int;
   ---------------------------
   -- Mouse Errors Handling --
   ---------------------------

   package body Mouse_Errors is

      NO_ERROR_CONSTANT: constant := 0;

      use type MaRTE.Integer_Types.Unsigned_8;

      Error: MaRTE.Integer_Types.Unsigned_8;

      procedure Set_Correct is
      begin
         Error:= NO_ERROR_CONSTANT;
      end Set_Correct;


      procedure Add_Code (The_Code: in Code) is
      begin
         Error := Error or Code'Enum_Rep(The_Code);
      end Add_Code;

      procedure Delete_Code (The_Code: in Code) is
      begin
         Error := Error and (not Code'Enum_Rep(The_Code));
      end Delete_Code;

      procedure Set_Code (The_Code: in Code) is
      begin
         Error := Code'Enum_Rep(The_Code);
      end Set_Code;

      function Included_Code (The_Code: in Code) return boolean is
      begin
         if (Error and Code'Enum_Rep(The_Code)) /= 0 then
            return true;
         else
            return false;
         end if;
      end Included_Code;

      function No_Error return boolean is
      begin
         return Error = 0;
      end No_Error;

   begin

      Set_Correct;

   end Mouse_Errors;


   -----------------------
   -- Create Semaphores --
   -----------------------

   function Create_Mouse_Semaphores return Int is
   begin
      if MaRTE_Semaphores.Initialize (Sem	=> Synchro_Sem'Access,
                                      Pshared 	=> 0,
                                      Value   	=> 0) /= 0 then
         return -1;
      end if;

      return 0;
   end Create_Mouse_Semaphores;


   ----------------------
   -- Initialize Mouse --
   ----------------------

   function Initialize_Mouse return Int is
      -- This Line only for using function Shift_Right to show driver version
      use MaRTE.Integer_Types;
      ------------------------------
      use type MaRTE.Integer_Types.Unsigned_8;
      Mouse_Aux		: PS2_Mouse;

   begin
      -- Remember that it's not necessary to initialize the queue because it
      -- is done in the package body of FIFO_Queue
      -- Recordar que no es necesario inicializar la cola, ya que es una
      -- maquina de estados abstracta y se inicializa en el paquete FIFO_Queue
      Mouse := new PS2_Mouse; -- Mouse Object Creation

      KC.New_Line;
      KC.Put
        ("      Mouse Driver v. "
         & MaRTE.Integer_Types.Unsigned_32'Image(Shift_Right(MOUSE_DRV_VER,16) and 16#ff#) & "."
         & MaRTE.Integer_Types.Unsigned_32'Image(Shift_Right(MOUSE_DRV_VER,8)  and 16#ff#) & "."
         & MaRTE.Integer_Types.Unsigned_32'Image(MOUSE_DRV_VER and 16#ff#));
      KC.New_Line;
      KC.Put("      (c) 2005 - http://marte.unican.es");
      KC.New_Line;

      -- Enable the Interface with the mouse sending the command
      -- "Enable Mouse Interface" to the Control Register (I/O Addr: 0x64)
      -- Ahora activaremos la interfaz con el mouse escribiendo
      -- el comando $A8 (Enable Mouse Interface) en el Control Register (0x64)
      Poll_Mouse_IBF(N_Retries 	=> Max_Retries,
                     Time_Delay	=> 0.0,
                     Wait	=> False);
      Outb(Control_Register,Enable_Mouse_Interface);

      --------------------
      -- Reset the Mouse
      --------------------
      Mouse_Aux.Command := Reset;
      Mouse_Send_Command(Mouse_Aux, Max_Retries, Wait=>False);

      if Mouse_Aux.Receive_Buffer(1) = Ret_BAT'Enum_Rep then
         KC.Put("      BAT Received (OK): ");
      else
         KC.Put("      BAT Received (FAIL!): ");
      end if;
      KC.Put(Mouse_Aux.Receive_Buffer(1),16);KC.New_Line;
      KC.Put("      ID Received : ");
      KC.Put(Mouse_Aux.Receive_Buffer(2),16);
      case Mouse_Aux.Receive_Buffer(2) is
         when Ret_PS2_ID'Enum_Rep =>
            Mouse.Vendor := To_Unbounded_String("Generic");
            Mouse.Name   := To_Unbounded_String("PS/2 Mouse");
         when others =>
            null;  -- Other mouse types may return a value different from 16#00#
      end case;
      KC.New_Line;

      -- Enabling Data Reporting
      Mouse_Aux.Command := Enable_Data_Reporting;
      Mouse_Send_Command (Mouse_Aux, Max_Retries, Wait=>False);

      -- Testing some mouse commands : only for debugging

      ----------------------
      -- Setting Resolution
      ----------------------
--        Mouse_Aux.Command := Set_Resolution;
--        Mouse_Aux.Param_To_Send := 0;
--        Mouse_Send_Command(Mouse_Aux, Max_Retries, Wait=>False);

      -------------------------------------
      -- Sending an erroneous Sample Rate
      -------------------------------------
--        Mouse_Aux.Command := Set_Sample_Rate;
--        Mouse_Aux.Param_To_Send:= 120;
--        Mouse_Send_Command (Mouse_Aux, Max_Retries, Wait=>False);

      -------------------------------------
      -- Sending a Status Request Command
      -------------------------------------
      Mouse_Aux.Command := Status_Request;
      Mouse_Send_Command (Mouse_Aux, Max_Retries, Wait=>False);
      KC.Put("      Status Request: ");
      for I in Packet_Bytes'Range loop
         KC.Put(Mouse_Aux.Receive_Buffer(I));
         KC.Put(" ");
      end loop;


      -- THIS CODE NO MORE USED.
      -- It was only used for developing the driver.
      -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
      -- Antes de implementar el procedure Mouse_Send_Command            --
      -- los comandos se enviaban actuando directamente sobre el 8042:   --
      -- En estas l�neas se prueban las respuestas del rat�n a           --
      -- diversas �rdenes PS/2                                           --
      -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --

      --------------------------------------
      -- Testing "Enable Data Reporting"  --
      --------------------------------------

--        Outb(Control_Register, Write_Mouse_Device);
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.IBF = Cleared;
--        end loop;
--        Outb(Input_Buffer,Byte(Standard_Command'Enum_Rep(Enable_Data_Reporting)));
--        KC.New_Line;
--        KC.Put("Comando Enable Data Reporting Enviado ");
--        KC.New_Line;
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.OBF = Set
--             and PS2_Mouse_Status.MOBF = Set;
--        end loop;
--        Data := Inb(Output_Buffer);
--        KC.Put("Recibido = ");KC.Put(Inb(Output_Buffer));
--        KC.New_Line;

      ------------------------------
      -- Testing "Status Request" --
      ------------------------------

--        Outb(Control_Register, Write_Mouse_Device);
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.IBF = Cleared;
--        end loop;
--        Outb(Input_Buffer,Byte(Standard_Command'Enum_Rep(Status_Request)));
--        KC.New_Line;
--        KC.Put("Comando Status Request Enviado ");
--        KC.New_Line;
--        for I in 1..4 loop
--           loop
--              PS2_Mouse_Status :=
--                Unsigned_8_To_Reg(Inb(Status_Register));
--              exit when PS2_Mouse_Status.OBF = Set
--                and PS2_Mouse_Status.MOBF = Set;
--           end loop;
--           Data := Inb(Output_Buffer);
--           KC.Put("Recibido = ");
--           KC.Put(Inb(Output_Buffer));
--           KC.New_Line;
--        end loop;

      ----------------------
      -- Testing "Resend" --
      ----------------------

--        Outb(Control_Register, Write_Mouse_Device);
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.IBF = Cleared;
--        end loop;
--        Outb(Input_Buffer,Byte(Standard_Command'Enum_Rep(Resend)));
--        KC.New_Line;
--        KC.Put("Comando Resend Enviado ");
--        KC.New_Line;
--        for I in 1..3 loop
--           loop
--              PS2_Mouse_Status :=
--                Unsigned_8_To_Reg(Inb(Status_Register));
--              exit when PS2_Mouse_Status.OBF = Set
--                and PS2_Mouse_Status.MOBF = Set;
--           end loop;
--           Data := Inb(Output_Buffer);
--           KC.Put("Recibido = ");
--           KC.Put(Inb(Output_Buffer));
--           KC.New_Line;
--        end loop;

      -------------------------------
      -- Testing "Set Sample Rate" --
      -------------------------------

--        Outb(Control_Register, Write_Mouse_Device);
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.IBF = Cleared;
--        end loop;
--        Outb(Input_Buffer,Byte(Standard_Command'Enum_Rep(Set_Sample_Rate)));
--        KC.New_Line;
--        KC.Put("Comando Set_Sample_Rate enviado ");
--        KC.New_Line;
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.OBF = Set
--             and PS2_Mouse_Status.MOBF = Set;
--        end loop;
--        KC.Put("Recibido = ");
--        KC.Put(Inb(Output_Buffer));
--        KC.New_Line;
--
--        Outb(Control_Register, Write_Mouse_Device);
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.IBF = Cleared;
--        end loop;
--        Outb(Input_Buffer,Byte(10));
--        KC.New_Line;
--        KC.Put("Tasa de muestreo 10 enviada ");
--        KC.New_Line;
--
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.OBF = Set
--             and PS2_Mouse_Status.MOBF = Set;
--        end loop;
--        Data := Inb(Output_Buffer);
--        KC.Put("Recibido = ");KC.Put(Inb(Output_Buffer));
--        KC.New_Line;
--

      ------------------------------
      -- Testing "Set Resolution" --
      ------------------------------

--        Outb(Control_Register, Write_Mouse_Device);
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.IBF = Cleared;
--        end loop;
--        Outb(Input_Buffer,Byte(Standard_Command'Enum_Rep(Set_Resolution)));
--        KC.New_Line;
--        KC.Put("Comando Set_Resolution enviado ");
--        KC.New_Line;
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.OBF = Set
--             and PS2_Mouse_Status.MOBF = Set;
--        end loop;
--        Data := Inb(Output_Buffer);
--        KC.Put("Recibido = ");KC.Put(Inb(Output_Buffer));
--        KC.New_Line;
--
--
--        Outb(Control_Register, Write_Mouse_Device);
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.IBF = Cleared;
--        end loop;
--        Outb(Input_Buffer,Byte(3));
--        KC.New_Line;
--        KC.Put("Resolucion 8 counts/mm enviada ");
--        KC.New_Line;
--        loop
--           PS2_Mouse_Status :=
--             Unsigned_8_To_Reg(Inb(Status_Register));
--           exit when PS2_Mouse_Status.OBF = Set
--             and PS2_Mouse_Status.MOBF = Set;
--        end loop;
--        Data := Inb(Output_Buffer);
--        KC.Put("Recibido = ");KC.Put(Inb(Output_Buffer));
--        KC.New_Line;


      --------------------------------
      --  Testing "Set Scaling 2:1" --
      --------------------------------

      --        Outb(Control_Register, Write_Mouse_Device);
      --        loop
      --           PS2_Mouse_Status :=
      --             Unsigned_8_To_Reg(Inb(Status_Register));
      --           exit when PS2_Mouse_Status.IBF = Cleared;
      --        end loop;
      --        Outb(Input_Buffer,Byte(Standard_Command'Enum_Rep(Set_Scaling_2_1)));
      --        KC.New_Line;
      --        KC.Put("Comando Set_Scaling_2_1 enviado ");
      --        KC.New_Line;
      --
      --        loop
      --           PS2_Mouse_Status :=
      --             Unsigned_8_To_Reg(Inb(Status_Register));
      --           exit when PS2_Mouse_Status.OBF = Set
      --             and PS2_Mouse_Status.MOBF = Set;
      --        end loop;
      --        KC.Put("Recibido = ");
      --        KC.Put(Inb(Output_Buffer));
      --        KC.New_Line;


      return 1;

   end Initialize_Mouse;


   -- PS2_Mouse_Packet_Process captura los datos del puerto, los introduce en
   -- la cola del rat�n y actualiza el objeto Mouse. Debe utilizarse con
   -- las interrupciones desactivadas => Esto se cumple porque
   -- PS2_Mouse_Packet_Process es llamada por la ISR.

   procedure PS2_Mouse_Packet_Process is
      PS2_Mouse_Status	: Status_Reg;
      --AMC{
      --        Mouse_Packet	: Mouse_Event;
      --AMC}
      Mouse_Packet	: Packet_Bytes;
      Ctrl_Byte		: Control_Byte;
      Delta_X		: Integer:=0;
      Delta_Y		: Integer:=0;
      New_Buttons	: PS2_Mouse_Buttons:=0;

      use type Mouse_Queue.Error;
      Error_Queue	: Mouse_Queue.Error;

   begin
      --AMC{
      --  En una versi�n anterior del driver, el timestamp se a�ad�a
      --  en el momento de la interrupci�n (en este procedimiento),
      --  y se guardaba en la cola asociado al resto de bytes del rat�n
      --  (la cola gen�rica se instanciaba con el tipo Mouse_Event).
      --  Actualmente el timestamp es a�adido por el m�todo Read cuando
      --  es llamado desde la aplicaci�n, con lo que la ISR es m�s r�pida
      --  y la cola gen�rica se instancia con el tipo Packet_Bytes (ver el ads).

      --  Mouse_Packet.Timestamp := Get_HWTime;
      --AMC}
      for I in 1..PS2_Packet_Size loop

         loop
            PS2_Mouse_Status :=
              Unsigned_8_To_Reg(Inb(Status_Register));
            exit when (PS2_Mouse_Status.OBF = Set and then PS2_Mouse_Status.MOBF = Set)
              or else (PS2_Mouse_Status.TO = Set);

         end loop;

         if PS2_Mouse_Status.TO = Set  then
            Flush_Pending_Input_Bytes
              (Max_Retries,(PS2_Packet_Size - I + 1));
            Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Timeout);
         end if;

         if PS2_Mouse_Status.PERR = Set then
            Flush_Pending_Input_Bytes
              (Max_Retries,(PS2_Packet_Size - I));
            Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Parity_Error);
         end if;
         --AMC{
         --  Mouse_Packet.Mouse_Byte(I) := Inb(Output_Buffer);
         --AMC}
         Mouse_Packet(I) := Inb(Output_Buffer);

      end loop;
      	--AMC{
	--  KC.Put("Procesando el paquete recogido, lo introduzco en la cola");
	--  KC.New_Line;
       --AMC}

      -----------------------------------------
      -- Insert the mouse event in the queue --
      -----------------------------------------

      if Mouse_Errors.No_Error then
         Mouse_Queue.Insert_In_Queue(Mouse_Packet, Error_Queue);
         if Error_Queue = Mouse_Queue.No_Room then
            Mouse_Errors.Add_Code(Mouse_Errors.Queue_Full);
         end if;
      end if;


      -- Now we process the Mouse Packet in PS/2 standard protocol
      -- and update global variables

      -- AMC{
      --   Ctrl_Byte   := Unsigned_8_To_Control_Byte
      --         (Mouse_Packet.Mouse_Byte(1));
      -- AMC}
      Ctrl_Byte   := Unsigned_8_To_Control_Byte(Mouse_Packet(1));

--        New_Buttons := Ctrl_Byte.Buttons;

      if Ctrl_Byte.X_Sign then -- Negative Increment in X
--           Delta_X := Integer(Mouse_Packet.Mouse_Byte(2)) - 256;
         Delta_X := Integer(Mouse_Packet(2)) - 256;
      else			-- Positive Increment in X
--           Delta_X := Integer(Mouse_Packet.Mouse_Byte(2));
         Delta_X := Integer(Mouse_Packet(2));
      end if;

      if Ctrl_Byte.Y_Sign then  -- Negative Increment in Y
--           Delta_Y := Integer(Mouse_Packet.Mouse_Byte(3)) - 256;
         Delta_Y := Integer(Mouse_Packet(3)) - 256;
      else			-- Positive Increment in Y
--           Delta_Y := Integer(Mouse_Packet.Mouse_Byte(3));
         Delta_Y := Integer(Mouse_Packet(3));
      end if;

      --------------------------------------------------------------
      -- Mouse Object Updating
      --------------------------------------------------------------
      Mouse.Buttons 	:= Ctrl_Byte.Buttons;
      Mouse.X_Pos	:= Mouse.X_Pos + Delta_X;
      Mouse.Y_Pos	:= Mouse.Y_Pos + Delta_Y;

      if Mouse.X_Pos < -Mouse.Max_X then
         Mouse.X_Pos := -Mouse.Max_X;
      elsif Mouse.X_Pos > Mouse.Max_X then
         Mouse.X_Pos :=  Mouse.Max_X;
      end if;
      if Mouse.Y_Pos < -Mouse.Max_Y then
         Mouse.Y_Pos := -Mouse.Max_Y;
      elsif Mouse.Y_Pos > Mouse.Max_Y then
         Mouse.Y_Pos :=  Mouse.Max_Y;
      end if;

--        ---- PRUEBA DE ERRORES ----
--        Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Bad_Parameter);
-------------------------------------------------------------------

   end PS2_Mouse_Packet_Process;


   procedure Poll_Mouse_IBF
     (N_Retries 	: in  	Retries_T    := Poll_Retries_Constant;
      Time_Delay	: in  	Time_Delay_T := Poll_Delay_Constant;
      Wait		: in	Boolean	     := True)
   is
      PS2_Mouse_Status	: Status_Reg;
      Retries		: Retries_T := N_Retries;
   begin

      while Retries > 0 loop
         PS2_Mouse_Status :=
           Unsigned_8_To_Reg(Inb(Status_Register));
         exit when PS2_Mouse_Status.IBF = Cleared
           or else PS2_Mouse_Status.TO  = Set;
         Retries := Retries - 1;
         if Wait then
            delay(Time_Delay);
         end if;
      end loop;

      if PS2_Mouse_Status.TO = Set then
         Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Timeout);

      elsif Retries = 0 then
        Mouse_Errors.Add_Code(Mouse_Errors.Poll_Timeout);

      end if;

   end Poll_Mouse_IBF;


   procedure Poll_Mouse_OBF
     (N_Retries		: in	Retries_T	:= Poll_Retries_Constant;
      Time_Delay	: in 	Time_Delay_T	:= Poll_Delay_Constant;
      Wait		: in	Boolean		:= True)
   is
      PS2_Mouse_Status	: Status_Reg;
      Retries		: Retries_T := N_Retries;
   begin

      while Retries > 0 loop
         PS2_Mouse_Status :=
           Unsigned_8_To_Reg(Inb(Status_Register));
         exit when (PS2_Mouse_Status.OBF = Set and PS2_Mouse_Status.MOBF = Set)
           or else PS2_Mouse_Status.TO  = Set;
         Retries := Retries - 1;
         if Wait then
            delay(Time_Delay);
         end if;
      end loop;

      if PS2_Mouse_Status.TO = Set then
         Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Timeout);

      elsif PS2_Mouse_Status.PERR = Set then
         Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Parity_Error);

      elsif Retries = 0 then
         Mouse_Errors.Add_Code(Mouse_Errors.Poll_Timeout);

      end if;

   end Poll_Mouse_OBF;


   procedure PS2_Mouse_Send_Byte
     (N_Retries		: in	Retries_T	:= Poll_Retries_Constant;
      Time_Delay	: in 	Time_Delay_T	:= Poll_Delay_Constant;
      Wait		: in	Boolean		:= True;
      Byte_To_Send	: in    Byte) is
   begin
      Poll_Mouse_IBF(N_Retries, Time_Delay, Wait);
      Outb(Control_Register, Write_Mouse_Device);
      Poll_Mouse_IBF(N_Retries, Time_Delay, Wait);
      Outb(Input_Buffer, Byte_To_Send);
      Poll_Mouse_OBF(N_Retries, Time_Delay, Wait);
   end PS2_Mouse_Send_Byte;


   procedure Flush_Pending_Input_Bytes (N_Retries: Retries_T := Max_Retries;
                                        N_Bytes  : Natural )  is
      PS2_Mouse_Status	: Status_Reg;
      Time		: Retries_T	:= N_Retries;
      Bytes		: Natural	:= 0;
      Flush		: MaRTE.Integer_Types.Unsigned_8; -- Used for flushing mouse input bytes
   begin

      while Time > 0 loop
         PS2_Mouse_Status :=
           Unsigned_8_To_Reg(Inb(Status_Register));
         if PS2_Mouse_Status.OBF = Set and PS2_Mouse_Status.MOBF = Set then
            Flush	:= Inb(Output_Buffer);
            Bytes	:= Bytes + 1;
         end if;
         exit when Bytes = N_Bytes;

         Time := Time - 1;
      end loop;

      if Time = 0 then
         Mouse_Errors.Add_Code(Mouse_Errors.Poll_Timeout);

      end if;
   end Flush_Pending_Input_Bytes;


   ---------------------------
   -- Send Command to Mouse --
   ---------------------------
   procedure Mouse_Send_Command (New_Mouse : in out PS2_Mouse;
                                 N_Retries : in Retries_T    := Poll_Retries_Constant;
                                 Time_Delay: in Time_Delay_T := Poll_Delay_Constant;
                                 Wait	   : in Boolean	     := True)
   is
      Retries		: Integer	:= 0;
      Processor_Flags	: Integer;
      Data		: MaRTE.Integer_Types.Unsigned_8;
      Param_Resp	: MaRTE.Integer_Types.Unsigned_8;
      Reactivate	: Boolean 	:= False;
   begin
      -- Lo primero ser� salvar el estado del procesador e
      -- inhabilitar las interrupciones
      Save_Flags_And_Disable_Interrupts (Processor_Flags);

      -- Ahora desactivaremos la interfaz con el teclado escibiendo
      -- el comando $AD (Disable Keyboard Interface)
      -- en el Control Register (0x64)
      Poll_Mouse_IBF(N_Retries 	=> Max_Retries,
                     Time_Delay	=> 0.0,
                     Wait	=> False);
      Outb(Control_Register,Disable_Keyboard_Interface);

      -- Colocamos el comando enviado en el campo correspondiente
      Mouse.Command := New_Mouse.Command;

      if Mouse.Command = Disable_Data_Reporting and then
        Mouse.Data_Reporting_On then
         PS2_Mouse_Send_Byte
           (N_Retries 	=> Max_Retries,
            Time_Delay	=> 0.0,
            Wait		=> False,
            Byte_To_Send
            => Standard_Command'Enum_Rep(Disable_Data_Reporting));
         Data := Inb(Output_Buffer);
         case Data is
            when 16#FA# =>
               Mouse.Command_Ack := Ret_ACK;
               Mouse.Data_Reporting_On := False;
            when  others =>
               KC.Put ("Error disabling data reporting");KC.New_Line;
               Mouse.Command_Ack := Ret_NAK;
               Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
         end case;

      elsif Mouse.Command = Enable_Data_Reporting and then
        (not Mouse.Data_Reporting_On) then
         PS2_Mouse_Send_Byte
           (N_Retries 	=> Max_Retries,
            Time_Delay	=> 0.0,
            Wait		=> False,
            Byte_To_Send
            => Standard_Command'Enum_Rep(Enable_Data_Reporting));
         Data := Inb(Output_Buffer);
         case Data is
            when 16#FA# =>
               Mouse.Command_Ack := Ret_ACK;
               Mouse.Data_Reporting_On := True;
            when 16#FE# =>
               KC.Put ("Error enabling data reporting");KC.New_Line;
               Mouse.Command_Ack := Ret_NAK;
               Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
            when others =>
               KC.Put ("Error enabling data reporting");KC.New_Line;
               Mouse.Command_Ack := Ret_ERR;
               Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
         end case;
      elsif
      -- In this case the command is not sent, as it is not necessary
        (Mouse.Command = Enable_Data_Reporting
         and Mouse.Data_Reporting_On)  or else
        (Mouse.Command = Disable_Data_Reporting
         and not Mouse.Data_Reporting_On) then
         KC.Put("Taking no action..."); KC.New_Line;

      elsif Mouse.Command = Status_Request then
         PS2_Mouse_Send_Byte
           (N_Retries,Time_Delay,Wait,
            Byte_To_Send =>
              Standard_Command'Enum_Rep(Mouse.Command));
         Data := Inb(Output_Buffer);
         case Data is
            when 16#FA# =>
               Mouse.Command_Ack := Ret_ACK;
            when  16#FE# =>
               Mouse.Command_Ack := Ret_NAK;
               Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
            when others =>
               Mouse.Command_Ack := Ret_ERR;
               Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
         end case;
         if Mouse.Command_Ack = Ret_ACK then
            for I in 1..3 loop
               Poll_Mouse_OBF(N_Retries,Time_Delay,Wait);
               Mouse.Receive_Buffer(I) := Inb(Output_Buffer);
            end loop;
            New_Mouse.Receive_Buffer(1..3) := Mouse.Receive_Buffer(1..3);
         end if;


      else
         --  If the device is in Stream mode (the default) and has been
         --  enabled with an Enable ($F4) command, then the host should
         --  disable the device with a Disable ($F5) command before
         --  sending any other command. However, if the host does send a
         --  command during enabled Stream mode, the device abandons any data
         --  packet or previous command response that was being transmitted
         --  at command time; the device will not send any more
         --  data packets until the response to the new command is finished.
         if Mouse.Data_Reporting_On and then
           (Mouse.Current_Mode = Stream_Mode or else
              Mouse.Command = Set_Stream_Mode) then
            --------------------------------------------------
            -- Later we'll have to re-enable Data Reporting --
            -- Except for the "Reset" command case          --
            --------------------------------------------------
            if Mouse.Command /= Reset then
               Reactivate := True;
            end if;
            --------------------------------------------------
            -- Disable Data Reporting before sending the command
            --------------------------------------------------
            PS2_Mouse_Send_Byte(N_Retries 	=> Max_Retries,
                                Time_Delay	=> 0.0,
                                Wait		=> False,
                                Byte_To_Send
                                => Standard_Command'Enum_Rep(Disable_Data_Reporting));
            Data := Inb(Output_Buffer);
            case Data is
               when 16#FA# =>
                  null;
               when  16#FE# =>
                  KC.Put ("Error disabling data reporting");KC.New_Line;
                  Mouse.Command_Ack := Ret_NAK;
                  Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
               when others =>
                  KC.Put ("Error disabling data reporting");KC.New_Line;
                  Mouse.Command_Ack := Ret_ERR;
                  Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
            end case;

         end if;

         -- Now we send the Command to the Mouse
         PS2_Mouse_Send_Byte
           (N_Retries,Time_Delay,Wait,
            Byte_To_Send =>
              Standard_Command'Enum_Rep(Mouse.Command));

         if Mouse_Errors.No_Error then

            -- if everything is OK, we store the ACK
            Data := Inb(Output_Buffer);

            case Data is
            when 16#FA# =>
               Mouse.Command_Ack := Ret_ACK;
            when  16#FE# =>
               Mouse.Command_Ack := Ret_NAK;
               Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
            when others =>
               Mouse.Command_Ack := Ret_ERR;
               Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Command_Error);
            end case;

            --              KC.New_Line;
            --              KC.Put("Este es el caso ");
            --              KC.Put(Standard_Command'Image(Mouse.Command));
            --              KC.New_Line;

            if Mouse.Command_Ack = Ret_ACK then

               case Mouse.Command is
               when SET_SCALING_1_1 | SET_SCALING_2_1 =>

                  null;
                  -- Commands not implemented yet
                  --                 when RESET_WRAP_MODE | SET_WRAP_MODE =>
                  --                    null;
                  --                 when RESEND =>
                  --                    null;
               when SET_STREAM_MODE =>

                  Mouse.Current_Mode := Stream_Mode;

               when SET_REMOTE_MODE =>

                  Mouse.Current_Mode := Remote_Mode;

               when SET_DEFAULTS =>

                  Mouse.Current_Mode		:= Stream_Mode;
                  Mouse.Data_Reporting_On	:= False;

               when SET_RESOLUTION | SET_SAMPLE_RATE =>

                  Mouse.Param_To_Send := New_Mouse.Param_To_Send;
                  PS2_Mouse_Send_Byte(N_Retries,Time_Delay,Wait,
                                      Byte_To_Send => Mouse.Param_To_Send);

                  -- Store the Ack
                  Data := Inb(Output_Buffer);

                  case Data is
                  when 16#FA# =>
                     Mouse.Param_Ack := Ret_ACK;
                  when  16#FE# =>
                     -- In this case the parameter is re-sent
                     PS2_Mouse_Send_Byte(N_Retries,Time_Delay,Wait,
                                         Byte_To_Send => Mouse.Param_To_Send);
                     KC.Put("Parameter re-sent");
                     KC.New_Line;
                     Param_Resp := Inb(Output_Buffer);
                     case Param_Resp is
                        when 16#FA# =>
                           Mouse.Param_Ack := Ret_ACK;
                        when  16#FE# =>
                           Mouse.Param_Ack := Ret_NAK;
                           Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Bad_Parameter);

                        when others =>
                           Mouse.Param_Ack := Ret_ERR;
                           Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Bad_Parameter);

                     end case;
                     KC.Put("Recibido = ");
                     KC.Put(Data,16);
                     KC.Put(" (");
                     KC.Put(Response'Image(Mouse.Param_Ack));
                     KC.Put(") ");
                     KC.New_Line;
                  when others =>
                     Mouse.Param_Ack := Ret_ERR;
                     Mouse_Errors.Add_Code(Mouse_Errors.Mouse_Bad_Parameter);

                  end case;

               when READ_DATA =>

                  for I in 1..PS2_Packet_Size loop
                     Poll_Mouse_OBF(N_Retries,Time_Delay,Wait);
                     Mouse.Receive_Buffer(I) := Inb(Output_Buffer);

                     KC.Put("Byte " & Integer'Image(I) &": ");
                     KC.Put(Mouse.Receive_Buffer(I));
                     KC.New_Line;
                  end loop;
                  New_Mouse.Receive_Buffer(1..PS2_Packet_Size)
                    := Mouse.Receive_Buffer(1..PS2_Packet_Size);

               when GET_DEVICE_ID =>

                  for I in Packet_Bytes'Range loop
                     Mouse.Receive_Buffer(I) 	:= 0;
                     New_Mouse.Receive_Buffer(I):= 0;
                  end loop;
                  Poll_Mouse_OBF(N_Retries,Time_Delay,Wait);
                  Mouse.Receive_Buffer(1) 	:= Inb(Output_Buffer);
                  New_Mouse.Receive_Buffer(1)	:= Mouse.Receive_Buffer(1);

               when RESET =>

                  for I in Packet_Bytes'Range loop
                     Mouse.Receive_Buffer(I) 	:= 0;
                     New_Mouse.Receive_Buffer(I):= 0;
                  end loop;

                  Poll_Mouse_OBF(N_Retries 	=> Max_Retries,
                                 Time_Delay	=> 0.0,
                                 Wait		=> False);
                  Data := Inb(Output_Buffer);
                  Mouse.Receive_Buffer(1) 	:= Data;
                  New_Mouse.Receive_Buffer(1)	:= Data;

                  --                    KC.Put("Primer byte recibido (BAT): ");
                  --                    KC.Put(Mouse.Receive_Buffer(1));
                  --                    KC.New_Line;

                  Poll_Mouse_OBF(N_Retries 	=> Max_Retries,
                                 Time_Delay	=> 0.0,
                                 Wait		=> False);
                  Data := Inb(Output_Buffer);
                  Mouse.Receive_Buffer(2) 	:= Data;
                  New_Mouse.Receive_Buffer(2) 	:= Data;

                  --                    KC.Put("Segundo byte recibido (ID): ");
                  --                    KC.Put(Mouse.Receive_Buffer(2));
                  --                    KC.New_Line;

                  Mouse.Current_Mode		:= Stream_Mode;
                  New_Mouse.Current_Mode	:= Stream_Mode;
                  Mouse.Data_Reporting_On 	:= False;
                  New_Mouse.Data_Reporting_On	:= False;
               when others =>
                  null;

               end case;
            end if;
         end if;

      end if;

      ----------------------------------------------------
      -- 'Hardware-Dealing Errors' Detection & Handling --
      ----------------------------------------------------

      if Mouse_Errors.Included_Code(Mouse_Errors.Poll_Timeout) then
         KC.New_Line;
         KC.Put
           ("ps2_mouse.adb.Mouse_Send_Command: bad data from KBC, poll timeout");
         KC.New_Line;
      end if;

      if Mouse_Errors.Included_Code(Mouse_Errors.Mouse_Timeout) then
         KC.New_Line;
         KC.Put
           ("ps2_mouse.adb.Mouse_Send_Command: bad data from KBC, mouse timeout");
         KC.New_Line;
      end if;

      if Mouse_Errors.Included_Code(Mouse_Errors.Mouse_Parity_Error) then
         KC.New_Line;
         KC.Put
           ("ps2_mouse.adb.Mouse_Send_Command: bad data from KBC, mouse parity error");
         KC.New_Line;
      end if;

      if Mouse_Errors.Included_Code(Mouse_Errors.Mouse_Command_Error) then
         KC.New_Line;
         KC.Put
           ("ps2_mouse.adb.Mouse_Send_Command: bad data from KBC.");
         KC.Put("Error sending command ");
         KC.Put(Standard_Command'Image(Mouse.Command));
         KC.New_Line;
         KC.Put("Data received from KBC: ");
         KC.Put(Data,16);
         KC.New_Line;
      end if;

      if Mouse_Errors.Included_Code(Mouse_Errors.Mouse_Bad_Parameter) then
         KC.New_Line;
         KC.Put
           ("ps2_mouse.adb.Mouse_Send_Command: bad parameter sent to mouse. Try again");
         KC.New_Line;
         KC.Put("Valid parameters are :");
         KC.New_Line;
         KC.Put("Sample rates : 10, 20, 40, 80, 100 and 200 samples/sec");
         KC.New_Line;
         KC.Put("Resolutions  : 0, 1, 2, 3 (=> 1, 2, 4 and 8 counts/mm)");
         KC.New_Line;
         KC.Put("Command sent: ");KC.Put(Standard_Command'Image(Mouse.Command));
         KC.Put(". Parameter sent: ");
         KC.Put(Mouse.Param_To_Send);
         KC.New_Line;
         Mouse_Errors.Set_Correct;
      end if;

      if Reactivate then
         -- Re-Enabling Data Reporting in the Mouse
         -- after sending the command

         PS2_Mouse_Send_Byte(N_Retries 	=> Max_Retries,
                             Time_Delay	=> 0.0,
                             Wait	=> False,
                             Byte_To_Send
                             => Standard_Command'Enum_Rep(Enable_Data_Reporting));

         Data := Inb(Output_Buffer);
         case Data is
            when 16#FA# =>
               null;
            when  others =>
               KC.Put ("Error re-enabling data reporting");
               KC.New_Line;
               Mouse.Command_Ack := Ret_ERR;

         end case;

      end if;

      -- Copiamos los campos que nos interesan del estado actual del raton,
      -- para poder comprobar los campos de Acknowledge y/o los bytes recibidos

      New_Mouse.Command_Ack := Mouse.Command_Ack;
      New_Mouse.Param_Ack   := Mouse.Param_Ack;

      -- Ahora activaremos de nuevo la interfaz con el teclado escibiendo
      -- el comando $AE (Enable Keyboard Interface)
      -- en el Control Register (0x64)
      Poll_Mouse_IBF(N_Retries 	=> Max_Retries,
                     Time_Delay	=> 0.0,
                     Wait	=> False);
      Outb(Control_Register,Enable_Keyboard_Interface);


      -- Por �ltimo recuperamos el estado del procesador y habilitamos
      -- de nuevo las interrupciones
     Restore_Flags(Processor_Flags);


   end Mouse_Send_Command;



end PS2_Mouse;
