with MaRTE.Direct_IO;

package body RTEP_Debug_Functions is

   --------------------------------
   -- MESSAGE auxiliar functions --
   --------------------------------

   procedure DEBUG (Str     : in String;
                    Enabled : in Boolean) is
   begin
      if Enabled then
         MaRTE.Direct_IO.Put ("[DEBUG]: " & Str);
         MaRTE.Direct_IO.New_Line;
      end if;
   end DEBUG;
   pragma Inline (DEBUG);

   procedure ERROR (Str : in String) is
   begin
      MaRTE.Direct_IO.Put ("[ERROR]: " & Str);
      MaRTE.Direct_IO.New_Line;
   end ERROR;
   pragma Inline (ERROR);

   procedure WARNING (Str : in String) is
   begin
      MaRTE.Direct_IO.Put ("[WARNING]: " & Str);
      MaRTE.Direct_IO.New_Line;
   end WARNING;
   pragma Inline (WARNING);

   -------------------------------
   -- RESULT Checking functions --
   -------------------------------

   procedure Check_NZ (Result : MaRTE.Integer_Types.Int) is
      use type MaRTE.Integer_Types.Int;
   begin
      if Result /= 0 then
         ERROR ("Check_NZ failed, Err="&MaRTE.Integer_Types.Int'Image(Result));
         raise Check_NZ_ERROR;
      end if;
   end Check_NZ;
   pragma Inline (Check_NZ);

   ----------------------------------------------
   -- TIME MEASURE functions and configuration --
   ----------------------------------------------

   procedure Time_Measures_init is
   begin
      --  TIME_MEASURE : Initalization
      if Time_Measure.Enable_Measures'First then
        -- DEBUG ("INIT the measures...", Time_Measure.Enable_Measures'First);
        Idle_State_ID := Time_Measure.Init_Time_Measure
          (Var_Strings.To_Var_String ("RT-EP_Core:Idle_State"));
        Send_Initial_Token_State_ID :=
          Time_Measure.Init_Time_Measure
            (Var_Strings.To_Var_String ("RT-EP_Core:Send_Initial_Token"));
        Check_Token_State_ID :=
          Time_Measure.Init_Time_Measure
            (Var_Strings.To_Var_String ("RT-EP_Core:Check_Token"));
        Send_Permission_State_ID :=
          Time_Measure.Init_Time_Measure
            (Var_Strings.To_Var_String ("RT-EP_Core:Send_Permission"));
        Send_Token_State_ID :=
          Time_Measure.Init_Time_Measure
            (Var_Strings.To_Var_String ("RT-EP_Core:Send_Token"));
        Send_Info_State_ID :=
          Time_Measure.Init_Time_Measure
            (Var_Strings.To_Var_String ("RT-EP_Core:Send_Info"));
        Recv_Info_State_ID :=
          Time_Measure.Init_Time_Measure
             (Var_Strings.To_Var_String ("RT-EP_Core:Recv_Info"));
        Token_Passing_Time_ID :=
          Time_Measure.Init_Time_Measure
            (Var_Strings.To_Var_String ("RT-EP_Core:Token_Passing_Time"),
             Time_Measure.ABS_Time_Clock);
        -- DEBUG ("INIT the measures OK!", Time_Measure.Enable_Measures'First);
      end if;
   end Time_Measures_init;

   procedure BEGIN_TIME_MEASURE (Id : in Time_Measure.Measure_ID) is
   begin
      if Time_Measure.Enable_Measures'First then
         Time_Measure.Set_Time_Mark (Id, Time_Measure.First);
      end if;
   end BEGIN_TIME_MEASURE;
   pragma Inline (BEGIN_TIME_MEASURE);

   procedure END_TIME_MEASURE (Id : in Time_Measure.Measure_ID) is
   begin
      if Time_Measure.Enable_Measures'First then
         Time_Measure.Set_Time_Mark (Id, Time_Measure.Last);
      end if;
   end END_TIME_MEASURE;
   pragma Inline (END_TIME_MEASURE);

end RTEP_Debug_Functions;
