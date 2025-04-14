with Ada.Text_IO;
with MaRTE.Integer_Types;               use MaRTE.Integer_Types; 
   use type MaRTE.Integer_Types.Int;
with MaRTE.HAL;                         use MaRTE.HAL;  -- HWTime
with MaRTE.HAL.TSC;
with Execution_Load_Loop;               use Execution_Load_Loop;  -- Eat

with MaRTE.Clock_Modulation;
   use type MaRTE.Clock_Modulation.CM_Dutycyclelevels;


procedure test04 is
   ret : Int;
   tsc1, tsc2, full_tsc : HWTime;
   res : Float;
begin
   -- Check if supported
   ret := MaRTE.Clock_Modulation.is_Supported;
   if ret = 0 then
      Ada.Text_IO.Put ("Error Not Supported ");
      Ada.Text_IO.Put (Int'Image (ret) );
      Ada.Text_IO.New_Line;
   else
      Ada.Text_IO.Put ("Supported ");
      Ada.Text_IO.Put (Int'Image (ret) );
      Ada.Text_IO.New_Line;
   end if;

   -- Calibrate frequencies values
   ret := MaRTE.Clock_Modulation.calibrate;
   if ret /= 0 then
      Ada.Text_IO.Put ("Error calibrate");
      Ada.Text_IO.New_Line;
   end if;

   -- loop changing dutycycle
   for item in MaRTE.Clock_Modulation.CM_Dutycyclelevels loop
      ret := MaRTE.Clock_Modulation.set_Dutycyclelevel (item);
      if ret /= 0 then
         Ada.Text_IO.Put ("Error set");
         Ada.Text_IO.New_Line;
      end if;

      tsc1 := MaRTE.HAL.TSC.Read_TSC;
      Eat (Duration (0.1));
      tsc2 := MaRTE.HAL.TSC.Read_TSC;

      if item = MaRTE.Clock_Modulation.CLOCK_MODULATION_DUTYCYCLE_100 then
        full_tsc := tsc2 - tsc1;
      end if;

      -- Get actual dutycycle level
      Ada.Text_IO.Put (MaRTE.Clock_Modulation.CM_Dutycyclelevels'Image
               (MaRTE.Clock_Modulation.get_dutycyclelevel));
      Ada.Text_IO.Put ("-");
      -- Get calibrated dutycycle from the dutycycle level
      Ada.Text_IO.Put (Float'Image (MaRTE.Clock_Modulation.get_dutycycle (item)));
      tsc2 := tsc2 - tsc1;
      Ada.Text_IO.Put (" @");
      -- dutycycle calculated from this example
      res := Float(full_tsc) / Float(tsc2);
      Ada.Text_IO.Put (Float'Image (res));
      Ada.Text_IO.New_Line;


   end loop;

end test04;