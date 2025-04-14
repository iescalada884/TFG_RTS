--  Test for all architectures
with MaRTE_OS;
with Reports;
with MaRTE.Direct_IO;

procedure Test_Assert_Fail is
begin
   Reports.Init;
   MaRTE.Direct_IO.Put ("Intentional assert fail");
   MaRTE.Direct_IO.New_Line;
   Reports.Assert (False);
end Test_Assert_Fail;
