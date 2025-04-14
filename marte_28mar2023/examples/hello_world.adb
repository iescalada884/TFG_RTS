------------------------------------------------------------------------------
--  ------------------        M a R T E     O S        -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                          'h e l l o   w o r l d'
--
--                                   Body
--
--
--  File 'hello_world.adb'                                             By MAR.
--
------------------------------------------------------------------------------


with Text_IO;
--  with Debug_Marte; use Debug_Marte; -- For Debugging


procedure Hello_World is

begin
   --  For Debugging
   --  Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
   --  Debug_Marte.Set_Break_Point_Here;

   Text_IO.New_Line;
   Text_IO.Put ("Hello, I'm an Ada program running on MaRTE OS.");
   Text_IO.New_Line (2);

end Hello_World;
