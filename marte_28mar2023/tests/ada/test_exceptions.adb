--  Test for all architectures
with MaRTE_OS;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Reports;

procedure Test_Exceptions is
   E : exception;

   procedure Q is

   begin
      Put_Line ("raise E");
      raise E;
   end Q;

   procedure P is
   begin
      Q;
      Reports.Assert (False);
   exception
      when E =>
         Put_Line ("caught E in P");
         raise Program_Error;
      when X : others =>
         Put_Line ("caught " & Exception_Name (X) & "P");
         Reports.Assert (False);
   end P;

begin
   Reports.Init;
   P;
   Reports.Assert (False);
exception
   when E =>
      Put_Line ("caught E in main procedure");
      Reports.Assert (False);
   when X : others =>
      Put_Line ("caught " & Exception_Name (X)
                & " in main procedure");
      Reports.Test_OK;
end Test_Exceptions;
