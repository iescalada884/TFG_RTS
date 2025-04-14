with Ada.Unchecked_Conversion;
with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Interfaces.C;
with Laser_Sick_LMS200_Import; use Laser_Sick_LMS200_Import;

procedure Test_Laser_Sick_LMS200 is

   package C renames Interfaces.C;
   use type C.Char_array;
   use type C.Unsigned_char;
   use type C.Int;

   Port        : C.Char_array := "/dev/ttyS0";
   Key         : Character;
   Datalen     : C.Int;
   Count       : Integer := 0;
   Count_max   : Integer := 100;
   Range_mode  : C.Int := RANGE_100;
   Res_mode    : C.Int := RES_1_DEG;
   Unit_mode   : C.Int := MMMODE;
   Baud_sel    : C.Int := BAUD_9600; --  BAUD_38400 BAUD_19200 BAUD_9600

begin

   Put("Empieza La Inicializacion: (ESPERA A QUE SE PONGA EL LED VERDE SOLO!");
   Get_Immediate (Key);

   ConnectToLMS(Range_mode, Res_mode, Unit_mode, Port, Baud_sel);
   Put_line("Fin De Inicializacion");
   Count_max := 100;

   while (Count < Count_max) loop
      Datalen := ReadLMSValues;
      if (Datalen /= 0) then
         for I in 0 .. Datalen loop
            Put(Integer(I));
            Put(":");
            Put(Integer(Laserazo(I)));
            Put(" ");
            if (I mod 5) = 0 then
               New_line;
            end if;
         end loop;
         Count:=Count + 1;
      end if;
   end loop;

   Vaciarlaser;
   Put_line("Vaciado El Laser");
   StopLMS;
   Put_line("Paramos El Laser");
   ResetLMS;
   Put_line("Reseteamos El Laser");

end Test_Laser_Sick_LMS200;
