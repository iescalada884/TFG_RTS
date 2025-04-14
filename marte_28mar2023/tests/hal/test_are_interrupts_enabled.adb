--  Test for:
------------------------------------------------------------------------------
--  ------------------        M a R T E     O S        -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--          'T e s t _ A r e_ I n t e r r u p t s _ E n a b l e d'
--
--                                   Body
--
--
--  File 'test_are_interrupts_enabled.adb'                            By MAR.
--
--  Check enable/disable interrupts and Are_Interrupts_Enabled function
--
--  XXX work in progress, it doesn't work
--
--  In x86 the test doesn't work because
--  Console_Switcher.Serial_Console_Init calls ioctl what involves
--  using SCHD.Self that is not initialized. The test can be run in a
--  manual way copying in this directory the file
--  'reports-init.adb.linux' with name 'reports-init.adb'.
--
------------------------------------------------------------------------------

with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with Reports;
with MaRTE.HAL;
--  with Debug_Marte; use Debug_Marte; -- For Debugging

procedure Test_Are_Interrupts_Enabled is

   package HAL renames MaRTE.HAL;

begin
   --  For Debugging
   --  Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
   --  Debug_Marte.Set_Break_Point_Here;
   Reports.Init;

   Put ("Test_Are_Interrupts_Enabled"); New_Line;

   --  Reports.Assert (not HAL.Are_Interrupts_Enabled);

   HAL.Enable_Interrupts;
   Reports.Assert (HAL.Are_Interrupts_Enabled);

   HAL.Disable_Interrupts;
   Reports.Assert (not HAL.Are_Interrupts_Enabled);

   HAL.Enable_Interrupts;
   Reports.Assert (HAL.Are_Interrupts_Enabled);

   HAL.Disable_Interrupts;
   Reports.Assert (not HAL.Are_Interrupts_Enabled);

   Reports.Test_OK;

end Test_Are_Interrupts_Enabled;
