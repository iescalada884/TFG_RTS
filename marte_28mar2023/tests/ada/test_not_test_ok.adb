--  Test for all architectures
with MaRTE_OS;
with Reports;

procedure Test_Not_Test_OK is
begin
   Reports.Init;
   Reports.Assert (True);
   --  Don't call to Reports.Test_OK
end Test_Not_Test_OK;
