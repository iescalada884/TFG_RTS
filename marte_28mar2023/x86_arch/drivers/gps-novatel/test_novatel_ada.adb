with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Novatel_Import; use Novatel_Import;

procedure Test_Novatel_Ada is
   use type C.Int;
   key: Character;
   port: C.char_array := "/dev/ttyS0";
begin

   put("Empieza la inicializacion del gps. Pulsa enter ...");
   Get_Immediate (key);
   initNovatel(port); -- Modificarlo para pasarle PUERTO, BAUD (como el laser)
   Get_Immediate (key);

   if (SetupNovatel < 0) then Put("error en setup");end if;

   Get_Immediate (key);

   for I in 1 .. 100 loop
      if leerSerie > 0 then
         Put_line("ERORR: leyendo de la unidad de GPS");
      end if;
      leeGps; -- Modificar para que no saque informacion,
              -- sino que almacene la lectura variables ..
   end loop;

   ShutdownNovatel;

end Test_Novatel_Ada;
