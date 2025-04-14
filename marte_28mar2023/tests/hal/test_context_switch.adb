--  Test not for: x86
------------------------------------------------------------------------------
--  ------------------        M a R T E     O S        -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'T e s t _ C o n t e x t _ S w i t c h'
--
--                                   Body
--
--
--  File 'test_context_switch.adb'                                  By MAR.
--
--  Check context switch and stacks.
--
--  A stack is created and set to execute a procedure
--  (Other_Task_Body). A context switch is performed to that stack and
--  then back to the main task.
--
--  In x86 the test doesn't work because
--  Console_Switcher.Serial_Console_Init calls ioctl what involves
--  using SCHD.Self that is not initialized. The test can be run in a
--  manual way copying in this directory the file
--  'reports-init.adb.linux' with name 'reports-init.adb'.
--
------------------------------------------------------------------------------

with Reports;
with System;
--  with Ada.Unchecked_Conversion;
with MaRTE.Kernel;
with MaRTE.HAL;
with MaRTE.Stacks_Management;
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with MaRTE.Integer_Types;
--  with Debug_Marte; use Debug_Marte; -- For Debugging

procedure Test_Context_Switch is

   package K renames MaRTE.Kernel;
   package HAL renames MaRTE.HAL;
   package STACKS renames MaRTE.Stacks_Management;
   use type STACKS.Stack_Id;

   Other_Stack_Size : constant := 4 * 1024; -- Bytes
   Other_Stack_Id : STACKS.Stack_Id;
   Other_Stack_Top : MaRTE.Integer_Types.Unsigned_32;
   Main_Stack_Top : MaRTE.Integer_Types.Unsigned_32 := 0;

   Other_Task_Has_Executed : Boolean := False;
   pragma Volatile (Other_Task_Has_Executed);

   --  function Task_Body_Function_To_Address is new Ada.Unchecked_Conversion
   --    (K.Task_Body_Function, System.Address);

   -----------------------
   -- Wrapper procedure --
   -----------------------

   --  Similar to Tasks_Operations.Internals.Task_Wrapper

   procedure Task_Wrapper (Task_Body : K.Task_Body_Function);

   procedure Task_Wrapper (Task_Body : K.Task_Body_Function) is
      Ret_Val : System.Address;
      pragma Unreferenced (Ret_Val);
   begin
      Put ("In task wrapper"); New_Line;

      Ret_Val := Task_Body.all (System.Null_Address);

      Reports.Assert (False); --  This line should never be reached
   end Task_Wrapper;

   ---------------------
   -- Other_Task_Body --
   ---------------------

   function Other_Task_Body (Arg : System.Address) return System.Address;

   function Other_Task_Body (Arg : System.Address) return System.Address is
      pragma Unreferenced (Arg);
   begin
      Put ("In other task"); New_Line;
      Other_Task_Has_Executed := True;

      --  go back to the main task

      HAL.Context_Switch (Old_Task => Other_Stack_Top'Address,
                          New_Task => Main_Stack_Top'Address);

      return System.Null_Address;
   end Other_Task_Body;

begin
   --  For Debugging
   --  Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
   --  Debug_Marte.Set_Break_Point_Here;
   Reports.Init;

   --  Initialize packages

   Put ("Initialize..."); New_Line;
   MaRTE.Direct_IO.End_Of_Kernel_Initialization;
   --  Here because in linux_lib Direct_IO doesn't print anything util
   --  this procedure is called

   HAL.Initialize;
   STACKS.Initialize_Pool;

   --  Get other task's stack

   Other_Stack_Id := STACKS.Request_Stack (Other_Stack_Size);
   Reports.Assert (Other_Stack_Id /= STACKS.Null_Stack_Id);

   --  Configure the task stack for the first activation
   --
   --  Stack_Top -> --  --  --  --  --  --  --  --  --  --  --  --  --
   --                         Address of 'Task_Wrapper'
   --   4 bytes     (This will be used as return addres in the 'ret'
   --                     instruction of 'HAL.Context_Switch')
   --               --  --  --  --  --  --  --  --  --  --  --  --  --
   --                                      0
   --   4 bytes      (Fake return address for 'Task_Wrapper', it can
   --                   be 0 because 'Task_Wrapper' never returns)
   --               --  --  --  --  --  --  --  --  --  --  --  --  --
   --                            Address of 'Body'
   --   4 bytes            (Parameter of 'Task_Wrapper')
   --
   --  Stack_Base-> --  --  --  --  --  --  --  --  --  --  --  --  --

   STACKS.Write_In_Stack (Addr => Task_Wrapper'Address,
                          P    => 3,
                          S    => Other_Stack_Id);
   STACKS.Write_In_Stack (Int => 0,
                          P   => 2,
                          S   => Other_Stack_Id);
   STACKS.Write_In_Stack (Addr => Other_Task_Body'Address,
--         Task_Body_Function_To_Address (Other_Task_Body'Unrestricted_Access),
                          P    => 1,
                          S    => Other_Stack_Id);
   Other_Stack_Top :=
     STACKS.DWord_In_Stack_Address (P => 3, S => Other_Stack_Id);

   --  swicth to the other task

   Put ("About to switch context to the other task..."); New_Line;
   HAL.Context_Switch (Old_Task => Main_Stack_Top'Address,
                       New_Task => Other_Stack_Top'Address);

   --  When here is because the other task has executed

   Reports.Assert (Other_Task_Has_Executed);
   Put ("Back in main task"); New_Line;

   Reports.Test_OK;

end Test_Context_Switch;
