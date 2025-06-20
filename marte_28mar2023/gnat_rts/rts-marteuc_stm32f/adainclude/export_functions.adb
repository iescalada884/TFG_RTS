with System.Task_Primitives.Operations;

procedure Export_Functions is
   procedure Local_Initialize renames System.Task_Primitives.Operations.Initialize;
   pragma Export (C, Initialize, "system__task_primitives__operations__initialize");
begin
   null;
end Export_Functions;
