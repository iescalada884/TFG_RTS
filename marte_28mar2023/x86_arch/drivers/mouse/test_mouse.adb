------------------------------------------------------------------------------
--  ------------------        M a R T E     O S        -------------------  --
------------------------------------------------------------------------------
--                                                             V1.58  Sep 2006
--
--                          'T e s t _  M o u s e'
--
--                                 Body
--
--
--  File 'test_mouse.adb'                                           By AMC.
--
------------------------------------------------------------------------------
with MaRTE_OS;
with PS2_Mouse; use PS2_Mouse;
with POSIX_IO;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Debug_Marte; use Debug_Marte; -- For Debugging
with MaRTE.HAL; use MaRTE.HAL;

procedure Test_Mouse is

   Fd : POSIX_IO.File_Descriptor := 0;

   Data: PS2_Mouse.Mouse_Event;
   Max : Integer := 0;

   procedure Read is new POSIX_IO.Generic_Read (PS2_Mouse.Mouse_Event);
   procedure Ioctl is new POSIX_IO.Generic_Ioctl
     (PS2_Mouse.Ioctl_Command,PS2_Mouse.Ioctl_Data);

   Raton_Aux : PS2_Mouse.PS2_Mouse;

begin
--     --  For Debugging
--     Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
--     Debug_Marte.Set_Break_Point_Here;

   Put_Line("Comienza la aplicacion");
   Fd := POSIX_IO.Open ("/dev/ps2mouse", POSIX_IO.Read_Only);

   Ada.Text_IO.Put("Valor de Fd = ");
   Put(Integer(Fd));
   New_Line;
   Raton_Aux.Command := Set_Sample_Rate;
   Raton_Aux.Param_To_Send := 10;
   Ioctl(Fd,PS2_Command,Raton_Aux);
   Raton_Aux.Param_To_Send := 3;
   Raton_Aux.Command := Set_Resolution;
   Ioctl(Fd,PS2_Command,Raton_Aux);
   Raton_Aux.Command := Set_Scaling_1_1;
   Ioctl(Fd,PS2_Command,Raton_Aux);

   loop
      begin
         --  Read from device file
         loop
            Read (Fd, Data);
            Put_Line ("Application: read '" & "'(" & Integer'Image(Data'Size/8)
                      & " bytes) from the device file");
            Put("Paquete leido: ");
            Put(Duration'Image(HWTime_To_Duration(Data.Timestamp)));
            for I in 1..3 loop
               Put("  ");
               Put(Integer(Data.Mouse_Byte(I)),4);
            end loop;
            New_Line;
            Ioctl(Fd,Get_Mouse_Position,Raton_Aux);
            Put_Line("Posicion (x,y): "
                     & Integer'Image(Raton_Aux.X_Pos) & ","
                     & Integer'Image(Raton_Aux.Y_Pos));
            Put_Line("-----------------------------------------------------");
         end loop;

         --  Close device file
         --  POSIX_IO.Close (Fd);

      exception
         when Excep_Event:others =>
            Put ("Exception:");
            Put (Ada.Exceptions.Exception_Name (Excep_Event));
            Put_Line("-----------------------------------------------------");
            Put("Paquete leido: ");
            Put(Duration'Image(HWTime_To_Duration(Data.Timestamp)));
            for I in 1..3 loop
               Put("  ");
               Put(Integer(Data.Mouse_Byte(I)),4);
            end loop;
            New_Line;
            Ioctl(Fd,Get_Mouse_Position,Raton_Aux);
            Put_Line("Posicion (x,y): "
                     & Integer'Image(Raton_Aux.X_Pos) & ","
                     & Integer'Image(Raton_Aux.Y_Pos));
            Put_Line("-----------------------------------------------------");
      end;
   end loop;
end Test_Mouse;
