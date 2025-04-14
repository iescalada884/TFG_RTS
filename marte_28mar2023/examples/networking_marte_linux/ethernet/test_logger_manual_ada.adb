with Text_IO;
with Logger_Ada;
with Interfaces.C;
with Ada.Exceptions;

procedure Test_Logger_Manual_Ada is
   use type Interfaces.C.Int;

   procedure Pause is
      H : Character;
   begin
      Text_IO.Put(" ...");
      Text_IO.Get_Immediate(H);
   end Pause;

   Dev : constant Logger_Ada.Log_Device := Logger_Ada.Log_Ethernet;
   Fd  : Text_IO.File_Type;
   Ret_Error : exception;
begin

   Text_IO.Open (Fd, Text_IO.Out_File, "/dev/membuff");

   if Logger_Ada.Logger_Init (Dev) /= 0 then
      raise Ret_Error;
   end if;

   loop
      for I in 1 .. 4 loop
         Text_IO.Put_Line (Fd, "line "&Integer'Image (I));
      end loop;

      Text_IO.Put ("execute manual log, press Enter"); Pause;

      if Logger_Ada.Logger_Manual_Call < 0 then
         raise Ret_Error;
      end if;
   end loop;

exception
   when The_Error : others =>
      Text_IO.Close (Fd);
      Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      raise;
end Test_Logger_Manual_Ada;
