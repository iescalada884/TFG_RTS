------------------------------------------------------------------------------
----------------------        M a R T E     O S        -----------------------
------------------------------------------------------------------------------
--                                                             V1.58  Sep 2006
--
--                   'P S 2 _ M o u s e _ F u n c t i o n s'
--
--                                  Body
--
--
--  File 'ps2_mouse-functions.adb'                                     By AMC.
--
--  Driver methods for the PC PS/2 Mouse
--
--  ----------------------------------------------------------------------
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
with MaRTE_Hardware_Interrupts;
with MaRTE_Semaphores;
with MaRTE.HAL.IO;
with MaRTE.HAL;
with MaRTE.Integer_Types;
with System;
with Ada.Unchecked_Conversion;
with PS2_Mouse; use PS2_Mouse;

package body PS2_Mouse.Functions is

   package HW_INTS renames MaRTE_Hardware_Interrupts;
   package HWI     renames MaRTE.HAL;
   package KC	   renames MaRTE.Direct_IO;
   use type Int;

   ----------------------------------------------------------------------------
   -- Mouse Interrupt Handler -------------------------------------------------
   ----------------------------------------------------------------------------

   function PS2_Mouse_Interrupt_Handler (Area : in System.Address;
                                         Intr : in HW_INTS.Hardware_Interrupt)
                                         return HW_INTS.Handler_Return_Code;
   function PS2_Mouse_Interrupt_Handler (Area : in System.Address;
                                         Intr : in HW_INTS.Hardware_Interrupt)
                                         return HW_INTS.Handler_Return_Code is
      Sem_Ret : Int;
   begin

      case Mouse.Protocol is
         when PS2_Standard =>
            PS2_Mouse_Packet_Process;
         when others =>
            null;  -- Call here other mouse packet processing protocols
      end case;

      -- In both Blocking and Non-Blocking Mode,
      -- the synchro semaphore is posted.

      -- But if there's an error processing mouse packet the
      -- synchronization semaphore will not be posted.

      if Mouse_Errors.No_Error then
         Sem_Ret:= MaRTE_Semaphores.Post(Synchro_Sem'Access);
      else
         -- Print on screen the error code(s)
         for E in Mouse_Errors.Code loop
            if Mouse_Errors.Included_Code(E) then
               KC.Put(" | "); KC.Put(Mouse_Errors.Code'Image(E));
            end if;
         end loop;
         KC.New_Line;
         -- < and choose the action to take for every error >
         -- < v.g. if Error = Queue_Full>
         -- 	< Mouse_Queue.Initialize_Queue>;

         -- Set Mouse_Error.Code to 'Correct'
         Mouse_Errors.Set_Correct;
      end if;

      return HW_INTS.POSIX_INTR_HANDLED_DO_NOT_NOTIFY;
   end PS2_Mouse_Interrupt_Handler;


   ----------------------------------------------------------------------------
   -- Driver Functions --------------------------------------------------------
   ----------------------------------------------------------------------------

   ------------
   -- Create --
   ------------
   function Create return Int is

   begin

      --  Create Semaphores
      if Create_Mouse_Semaphores /= 0 then
         return -1;
      end if;

      --  Configure Mouse
      if Initialize_Mouse /= 1 then
         return -1;
      end if;


         --  Install handler
      if HW_INTS.Associate (Intr      => HW_INTS.RESERVED3_INTERRUPT,
                            Handler   => PS2_Mouse_Interrupt_Handler'Access,
                            Area      => System.Null_Address,
                            Area_Size => 0) /= 0 then
         return -1;
      end if;

      --  Enable Mouse Interrupt
      if HW_INTS.Unlock (HW_INTS.RESERVED3_INTERRUPT) /= 0 then
         return -1;
      end if;

      return 0;
   end Create;


   ----------
   -- Read --
   ----------
   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Ssize_T is
      Packet            : Mouse_Event;
      Count             : Buffer_Length := 1;
      Time_Array        : Time_Byte_Array;
      Packets_To_Read   : Buffer_Length;
      Packet_Counter    : Buffer_Length := 1;
      Flags             : Integer;
      use type Buffer_Length;

      use type Mouse_Queue.Error;
      Error_Queue	: Mouse_Queue.Error;

      -- AMC Variables empleadas para la prueba del trywait
      Ret   : Int;
      use type MaRTE_Semaphores.Semaphore_Value;
      Value : aliased MaRTE_Semaphores.Semaphore_Value;
      -- AMC
   begin
      Packets_To_Read := Bytes / ((MaRTE.HAL.HWTime'Size / 8) +
                                   PS2_Packet_Size);
      -- Every read will retrieve at least one complete mouse packet.
      --AMC{ Este c�digo confirma el n�mero de Bytes que espera leer la
      --     llamada al sistema Generic_Read de POSIX
--        KC.Put("Soy la rutina Read. Se leeran ");
--        KC.Put(Integer(Bytes));
--        KC.Put(" Bytes");
--        KC.New_Line;
      --} AMC
      if Packets_To_Read < 1 then
         return -1;
      end if;

      While Packet_Counter <= Packets_To_Read loop

         MaRTE.HAL.Save_Flags_And_Disable_Interrupts(Flags);
         if Mouse.Blocking_Mode then

            -- When waiting at Synchro_Sem, interrupts
            -- are automatically enabled
            if MaRTE_Semaphores.Wait (Synchro_Sem'Access) /= 0 then
               KC.Put("ERROR EN EL SEMAFORO");
               return -1;
            end if;
         else -- Nonblocking Mode
--                -- AMC Prueba del trywait (valor del sem�foro antes)
--              Ret := MaRTE_Semaphores.Getvalue (Synchro_Sem'Access,Value'Access);
--              KC.Put(MaRTE_Semaphores.Semaphore_Value'Image(Value)); KC.New_Line;
--              -- AMC
            if MaRTE_Semaphores.Trywait(Synchro_Sem'Access) /= 0 then
               Ret := MaRTE_Semaphores.Getvalue (Synchro_Sem'Access,Value'Access);
               -- Esto consigue que no se eleve la excepci�n POSIX.POSIX_ERROR
               -- En Value tendremos el valor del sem�foro.

               -- La cola estar� vac�a. Trataremos esta situaci�n despu�s
               -- (Aunque tb puede tratarse aqu�)
	       -- return Ssize_T (Bytes);
	       -- return -1; -- Esto elevar� END_ERROR en la interfaz POSIX

            end if;
         end if;

--           -- AMC Prueba del trywait (valor del sem�foro despu�s)
--           Ret := MaRTE_Semaphores.Getvalue (Synchro_Sem'Access,Value'Access);
--           KC.Put(MaRTE_Semaphores.Semaphore_Value'Image(Value)); KC.New_Line;
--           -- AMC

         -------------------------------
         --  Mouse Packet available ? --
         -------------------------------

         Mouse_Queue.Extract_From_Queue(Packet.Mouse_Byte, Error_Queue);

         if Error_Queue = Mouse_Queue.No_Elements then
            -- This error can only occur if Mouse.Blocking_Mode = False;
            -- Handle this error as you please.
            -- <actions>:
	    -- Mouse_Errors.Add_Code(Mouse_Errors.Queue_Empty);
            MaRTE.HAL.Restore_Flags(Flags);
            return Ssize_T (Bytes);
         else

            Packet.Timestamp := Get_HWTime;
            MaRTE.HAL.Restore_Flags(Flags);

            Time_Array := To_Time_Byte_Array(Packet.Timestamp);

            for J in 1..Integer(MaRTE.HAL.HWTime'Size / 8) loop
               Buffer_Ptr.all (Count) := Time_Array(J);
               Count := Count + 1;
            end loop;

            for J in 1..PS2_Packet_Size loop
               Buffer_Ptr.all (Count) := MaRTE.Integer_Types.Unsigned_8
                                                      (Packet.Mouse_Byte(J));
               Count := Count + 1;
            end loop;

            Packet_Counter := Packet_Counter + 1;

         end if;

      end loop;

      -- return Ssize_T (Count - 1);
      -- Problema con la excepci�n End_Error y el tama�o de dato solicitado
      -- por el procedimiento Generic_Read
      return Ssize_T (Bytes);

   end Read;


   -----------
   -- Ioctl --
   -----------
   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac)  return Int is


      type Ioctl_Data_Ac is access all Ioctl_Data;
      function Buffer_Ac_To_Ioctl_Data_Ac is new
        Ada.Unchecked_Conversion (Buffer_Ac, Ioctl_Data_Ac);

      Data_Ac	: Ioctl_Data_Ac:= Buffer_Ac_To_Ioctl_Data_Ac (Ioctl_Data_Ptr);
      Flags	: Integer;


   begin
      --------------------------------------------------------------------
      HWI.Save_Flags_And_Disable_Interrupts(Flags);
      case Request is

         when Ioctl_Command'Enum_Rep(PS2_Command) =>
            Mouse_Send_Command(Data_Ac.all, Max_Retries, Wait=>False);

         when Ioctl_Command'Enum_Rep(Set_Blocking_Mode) =>
            Mouse.Blocking_Mode := True;

         when Ioctl_Command'Enum_Rep(Reset_Blocking_Mode) =>
            Mouse.Blocking_Mode := False;

         when Ioctl_Command'Enum_Rep(Enable_Mouse) =>
            Poll_Mouse_IBF(Max_Retries, Wait => False);
            MaRTE.HAL.IO.Outb(Control_Register, Enable_Mouse_Interface);

         when Ioctl_Command'Enum_Rep(Disable_Mouse) =>
            Poll_Mouse_IBF(Max_Retries, Wait => False);
            MaRTE.HAL.IO.Outb(Control_Register, Disable_Mouse_Interface);

         when Ioctl_Command'Enum_Rep(Get_Button_Status) =>
            Data_Ac.Buttons := Mouse.Buttons;

         when Ioctl_Command'Enum_Rep(Get_Mouse_Position) =>
            Data_Ac.X_Pos := Mouse.X_Pos;
            Data_Ac.Y_Pos := Mouse.Y_Pos;

         when Ioctl_Command'Enum_Rep(Set_Mouse_Position) =>
            if Data_Ac.X_Pos > Mouse.Max_X then
               Mouse.X_Pos := Mouse.Max_X;
            elsif Data_Ac.X_Pos < -Mouse.Max_X then
               Mouse.X_Pos := -Mouse.Max_X;
            else
               Mouse.X_Pos := Data_Ac.X_Pos;
            end if;

            if Data_Ac.Y_Pos > Mouse.Max_Y then
               Mouse.Y_Pos := Mouse.Max_Y;
            elsif Data_Ac.Y_Pos < -Mouse.Max_Y then
               Mouse.Y_Pos := -Mouse.Max_Y;
            else
               Mouse.Y_Pos := Data_Ac.Y_Pos;
            end if;

         when Ioctl_Command'Enum_Rep(Get_Mouse_Protocol) =>
            Data_Ac.Protocol := Mouse.Protocol;

         when Ioctl_Command'Enum_Rep(Set_Mouse_Protocol) =>
            Mouse.Protocol := Data_Ac.Protocol;

         when Ioctl_Command'Enum_Rep(Get_Device_Name) =>
            Data_Ac.Name := Mouse.Name;

         when others => null;


      end case;
      HWI.Restore_Flags(Flags);
      -------------------------------------------------------------------

      return 0;

   end Ioctl;


end PS2_Mouse.Functions;
