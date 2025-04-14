
with Var_Strings; pragma Elaborate_All (Var_Strings);
with Time_Measure; pragma Elaborate_All (Time_Measure);
with MaRTE.Integer_Types;

package RTEP_Debug_Functions is

   --------------------------------
   -- MESSAGE auxiliar functions --
   --------------------------------

   procedure DEBUG (Str     : in String;
                    Enabled : in Boolean);

   procedure ERROR (Str : in String);

   procedure WARNING (Str : in String);

   -------------------------------
   -- RESULT Checking functions --
   -------------------------------

   Check_NZ_ERROR : exception;

   procedure Check_NZ (Result : MaRTE.Integer_Types.Int); -- Mario
   --  procedure Check_NZ (Result : Integer);

   ----------------------------------------------
   -- TIME MEASURE functions and configuration --
   ----------------------------------------------

   Idle_State_ID               : Time_Measure.Measure_ID;
   Send_Initial_Token_State_ID : Time_Measure.Measure_ID;
   Check_Token_State_ID        : Time_Measure.Measure_ID;
   Send_Permission_State_ID    : Time_Measure.Measure_ID;
   Send_Token_State_ID         : Time_Measure.Measure_ID;
   Send_Info_State_ID          : Time_Measure.Measure_ID;
   Recv_Info_State_ID          : Time_Measure.Measure_ID;
   Token_Passing_Time_ID       : Time_Measure.Measure_ID;

   procedure Time_Measures_init;
   procedure BEGIN_TIME_MEASURE (Id : in Time_Measure.Measure_ID);
   procedure END_TIME_MEASURE (Id : in Time_Measure.Measure_ID);

end RTEP_Debug_Functions;
