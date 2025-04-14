--  Test for: x86
with Text_IO; use Text_IO;
with POSIX;
with POSIX_Signals;
with POSIX_Condition_Variables;
with POSIX_Mutexes;
with POSIX_Timers;
with POSIX_Semaphores;
with POSIX_IO;
with POSIX_Files;
with POSIX_Hardware_Interrupts;
-- with POSIX_Application_Scheduling;

with Reports;

procedure Test_POSIX5_Compilation is
begin
   Reports.Init;
   Put_Line ("Test POSIX5 compilation");
   Reports.Test_OK;
end Test_POSIX5_Compilation;
