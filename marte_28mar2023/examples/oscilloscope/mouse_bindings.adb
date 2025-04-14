with PS2_Mouse;
with POSIX_IO;
pragma Elaborate_All (POSIX_IO);

package body Mouse_Bindings is

   Fd : POSIX_IO.File_Descriptor := 0;
   Max : Integer := 0;
   Raton_Aux : PS2_Mouse.PS2_Mouse;

   procedure Read is new POSIX_IO.Generic_Read (PS2_Mouse.Mouse_Event);
   procedure Ioctl is new POSIX_IO.Generic_Ioctl
      (PS2_Mouse.Ioctl_Command, PS2_Mouse.Ioctl_Data);

   procedure Mouse_Init is
   begin
        Fd := POSIX_IO.Open ("/dev/ps2mouse", POSIX_IO.Read_Only);

        Raton_Aux.Param_To_Send := 10;
        Raton_Aux.Command := PS2_Mouse.Set_Sample_Rate;
        Ioctl(Fd, PS2_Mouse.PS2_Command, Raton_Aux);

        Raton_Aux.Param_To_Send := 1;
        Raton_Aux.Command := PS2_Mouse.Set_Resolution;
        Ioctl(Fd, PS2_Mouse.PS2_Command, Raton_Aux);

        Raton_Aux.Command := PS2_Mouse.Set_Scaling_1_1;
        Ioctl(Fd, PS2_Mouse.PS2_Command, Raton_Aux);

        Ioctl(Fd, PS2_Mouse.Reset_Blocking_Mode, Raton_Aux);
   end Mouse_Init;

   function Mouse_Get_Y return Interfaces.C.Int is
   begin
      Ioctl(Fd, PS2_Mouse.Get_Mouse_Position, Raton_Aux);

      if Raton_Aux.Y_Pos <= 0 then
         Raton_Aux.Y_Pos := 0;
         Ioctl(Fd, PS2_Mouse.Set_Mouse_Position, Raton_Aux);
      elsif Raton_Aux.Y_Pos >= 100 then
         Raton_Aux.Y_Pos := 100;
         Ioctl(Fd, PS2_Mouse.Set_Mouse_Position, Raton_Aux);
      end if;

      return Interfaces.C.Int (Raton_Aux.Y_Pos);
   end Mouse_Get_Y;

end Mouse_Bindings;
