--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--           'T e s t _ E D F _ P O _ F a i r b a i r n _ B u r n s'
--
--                               Ada Program
--
--
--  File 'test_edf_po_fairbairn_burns.adb'
--
--  Scenario 6b from paper: Mark Louis Fairbairn and Alan Burns,
--  "Implementing and Validating EDF Preemption-Level Resource
--  Control", Ada-Europe, 2012.
--
--  Three tasks, TA and TB shares a PO:
--    TA: offset:0   D:100  PL:2
--    TB: offset:2   D:60   PL:3
--    TC: offset:6   D:58   PL:4
--    PO_A: PL:3
--
--  Task(TA,0,100) {
--    1
--    PO_A {
--           7
--    }
--    2
--  }
--
--  Task(TB,2,60) {
--    1
--    PO_A {
--           7
--    }
--    2
--  }
--
--  Task(TC,6,58) {
--    10
--  }
--
--  This scenario behaves incorrectly in versions of MaRTE OS previous
--  to 05-06-2012:
--     TA =---PO---.
--        ^                                              Deadline: 100
--     TB   .......=
--          ^       ^block                               Deadline:  62 (60+2)
--     TC       ....=
--              ^                                        Deadline:  64 (58+6)
--        012345678901234567890123456789
--        0         1         2
--
------------------------------------------------------------------------------
pragma Task_Dispatching_Policy(EDF_Across_Priorities);
pragma Locking_Policy(Ceiling_Locking);

with Ada.Text_IO, Ada.Real_Time, Ada.Dispatching.EDF,
  Ada.Execution_Time, Ada.Dynamic_Priorities;
use Ada.Real_Time, Ada.Dispatching.EDF,
  Ada.Execution_Time, Ada.Dynamic_Priorities;

with Reports;

procedure Test_EDF_PO_Fairbairn_Burns is

   Counter : Natural := 0;
   pragma Volatile (Counter);

   StartTime: Time := Ada.Real_Time.Clock + Seconds(1);

   protected Logger is
      Pragma Priority(4); -- Mario
      procedure Identify(Name : in String;
                         Step : in Natural;
                         Counter : in out Natural);
   end Logger;

   protected body Logger is
      procedure Identify(Name : in String;
                         Step : in Natural;
                         Counter : in out Natural) is
         TextTime: String :=
           Integer'Image(Integer(To_Duration(Ada.Real_Time.Clock
                                               - StartTime)*10));
         TextDeadline: String := "Deadline: "
           & Integer'Image(Integer(To_Duration(Get_Deadline - StartTime)*100));
         TextPriority: String := "Priority: "
           & Integer'Image(Ada.Dynamic_Priorities.Get_Priority);
      begin
         Ada.Text_IO.Put_Line(TextTime & " " & Name & " " & TextDeadline
                                & " " & TextPriority);
         --Ada.Text_IO.Put_Line(TextTime & " " & Name);
         Reports.Assert (Step = Counter);
         Counter := Counter + 1;
      end Identify;
   end Logger;

   procedure BurnTime(WaitTime: in Integer) is
      SC : Seconds_Count;
      TS : Time_Span;
      TT : Time;
      S : Ada.Execution_Time.CPU_Time;
   begin
      S := Ada.Execution_Time.Clock;
      Ada.Execution_Time.Split(S,SC,TS);
      TT := Ada.Real_Time.Time_Of(SC,TS);
      TT := TT + Milliseconds(WaitTime*100);
      Ada.Real_Time.Split(TT,SC,TS);
      S := Ada.Execution_Time.Time_Of(SC, TS);
      while Ada.Execution_Time.Clock < S loop
         null;
      end loop;
   end BurnTime;
   pragma inline(BurnTime);

   task TB is
      Pragma Priority(3);
   end TB;

   task TC is
      Pragma Priority(4);
   end TC;

   task TA is
      Pragma Priority(2);
   end TA;

   protected A is
      Pragma Priority(3);
      procedure P10;
      procedure P3;
   end A;

   task body TB is
      Next_Release: Time;
      StartDelay: Time_Span := Milliseconds(2*100);
      Rel_Deadline : Time_Span := Milliseconds(60*100);
   begin
      Reports.Init;
      Ada.Text_IO.Put_Line(" Task TB initialised");
      Next_Release := StartTime + StartDelay;
      Delay_Until_and_Set_Deadline(Next_Release, Rel_Deadline);

      Logger.Identify("in  TB_0", 4, Counter);
      BurnTime(1);
      Logger.Identify("in  TB_1", 5, Counter);
      A.P10;
      Logger.Identify("out TB_0", 8, Counter);
      BurnTime(2);
      Logger.Identify("out TB_1", 9, Counter);
   end TB;

   task body TC is
      Next_Release: Time;
      StartDelay: Time_Span := Milliseconds(6*100);
      Rel_Deadline : Time_Span := Milliseconds(58*100);
   begin
      Reports.Init;
      Ada.Text_IO.Put_Line(" Task TC initialised");
      Next_Release := StartTime + StartDelay;
      Delay_Until_and_Set_Deadline(Next_Release, Rel_Deadline);

      Logger.Identify("in  TC", 10, Counter);
      BurnTime(10);
      Logger.Identify("out TC", 11, Counter);
   end TC;

   task body TA is
      Next_Release: Time;
      StartDelay: Time_Span := Milliseconds(0*100);
      Rel_Deadline : Time_Span := Milliseconds(100*100);
   begin
      Reports.Init;
      Ada.Text_IO.Put_Line(" Task TA initialised");
      Next_Release := StartTime + StartDelay;
      Delay_Until_and_Set_Deadline(Next_Release, Rel_Deadline);

      Logger.Identify("in  TA_0", 0, Counter);
      BurnTime(1);
      Logger.Identify("in  TA_1", 1, Counter);
      A.P3;
      Logger.Identify("out TA_0", 12, Counter);
      BurnTime(2);
      Logger.Identify("out TA_1", 13, Counter);

      Reports.Assert (Counter = 14);
      Reports.Test_OK;
   end TA;

   protected body A is
      procedure P10 is
      begin
         Logger.Identify("in  TB A P10", 6, Counter);
         BurnTime(7);
         Logger.Identify("out TB A P10", 7, Counter);
      end P10;
      procedure P3 is
      begin
         Logger.Identify("in  TA A P3", 2, Counter);
         BurnTime(7);
         Logger.Identify("out TA A P3", 3, Counter);
      end P3;
   end A;

begin
   null;
end Test_EDF_PO_Fairbairn_Burns;
