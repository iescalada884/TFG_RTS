--  Test for all architectures

pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy(Ceiling_Locking);
pragma Queuing_Policy(Priority_Queuing);

with MaRTE_OS;

with Ada.Task_Attributes;
with Ada.Text_IO; use Ada.Text_IO;
with Reports;

procedure Test_Task_Attributes is
   pragma Priority (12);

   package Natural_Attr is new Ada.Task_Attributes (Natural, 0);

   task type With_Attributes is
      pragma Priority (10);
   end With_Attributes;

   task body With_Attributes is
      A : Natural := 5;
   begin
      Reports.Init;
      A := Natural_Attr.Value;
      Reports.Assert (A=0);
      Put_Line ("Attr: " & Natural'Image (A));
      delay 0.05;
      A := Natural_Attr.Value;
      Reports.Assert (A=1);
      Put_Line ("Attr: " & Natural'Image (A));
      delay 0.05;
   end With_Attributes;

   T1 : With_Attributes;

   A : Natural := 7;

begin
   Reports.Init;
   A := Natural_Attr.Value (T1'Identity);
   Reports.Assert (A=0);
   delay 0.03;
   Put_Line ("Main set Attr");
   Natural_Attr.Set_Value (1, T1'Identity);
   delay 0.03;
   Put_Line ("Main leaves");
   A := Natural_Attr.Value (T1'Identity);
   Reports.Assert (A=1);
   delay 0.02;
   Reports.Test_OK;
end Test_Task_Attributes;
