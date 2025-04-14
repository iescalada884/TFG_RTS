--  Test for:
------------------------------------------------------------------------------
--  ------------------        M a R T E     O S        -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                 'T e s t _ T i m e r _ I n t e r r u p t'
--
--                                   Body
--
--
--  File 'test_timer_interrupt.adb'                                  By MAR.
--
--  Check enable/disable interrupts and timer interrupt:
--
--  Check the timer handler is actually called after programming the
--  timer but only when interrupts are enabled.
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

procedure Test_Timer_Interrupt is

   package HAL renames MaRTE.HAL;
   use type HAL.HWTime;

   -----------------------------------------------
   -- Data shared by handler and main procedure --
   -----------------------------------------------

   After_Interrupts_Enable : Boolean := False;
   pragma Volatile (After_Interrupts_Enable);

   Timer_Handler_Called : Boolean := False;
   pragma Volatile (Timer_Handler_Called);

   ------------------------------
   --  Timer interrupt handler --
   ------------------------------

   procedure Timer_Handler (State : HAL.Trap_State_Ac);
   pragma Convention (C, Timer_Handler);

   procedure Timer_Handler (State : HAL.Trap_State_Ac) is
      pragma Unreferenced (State);
   begin
      Reports.Assert (not HAL.Are_Interrupts_Enabled);
      Reports.Assert (After_Interrupts_Enable);

      Put ("Handler"); New_Line;
      Timer_Handler_Called := True;
   end Timer_Handler;

   Timer_Interval : constant HAL.HWTime := HAL.Duration_To_HWTime (1.0);
   Next_Activation : HAL.HWTime;

begin
   --  For Debugging
   --  Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
   --  Debug_Marte.Set_Break_Point_Here;
   Reports.Init;

   Reports.Assert (not HAL.Are_Interrupts_Enabled);

   --  Initialize Hardware

   HAL.Initialize;
   Reports.Assert (not HAL.Are_Interrupts_Enabled);

   --  Install timer handler

   HAL.Disable_Interrupts;
   HAL.Install_HW_Interrupt_Handler (HAL.Timer_Interrupt,
                                     Timer_Handler'Unrestricted_Access);

   --  Program timer and wait some time (Timer handler should not be called
   --  until interrupts are enabled)

   HAL.Program_Timer (HAL.HW_Timer_0, Timer_Interval, Next_Activation);
   while HAL.Get_HWTime < Next_Activation + Timer_Interval loop
      --  Wait for and interval approximately equal to (Timer_Interval * 2)
      null;
   end loop;
   Reports.Assert (not Timer_Handler_Called);

   --  Enable interrupts

   Put ("Enable interrupts"); New_Line;
   After_Interrupts_Enable := True;
   HAL.Enable_Interrupts;

   --  Interrupt should be pending and it's handler should be executed
   --  just after enabling interrupts

   Reports.Assert (Timer_Handler_Called);
   Reports.Assert (HAL.Are_Interrupts_Enabled);

   Reports.Test_OK;

end Test_Timer_Interrupt;
