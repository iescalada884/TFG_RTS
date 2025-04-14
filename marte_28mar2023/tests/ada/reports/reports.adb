with MaRTE.Direct_IO;
pragma Warnings (Off);
with MaRTE.Configuration_Parameters;
use MaRTE.Configuration_Parameters; -- for ARCH_X86
pragma Warnings (On);

package body Reports is

   Is_Initialized : Boolean := False;

   procedure Init is separate;

   procedure Assert
     (Condition : Boolean;
      Msg       : String := GNAT.Source_Info.Source_Location) is
   begin
      Init;
      if not Condition then
         MaRTE.Direct_IO.Put_Error ("ASSERT FAILED ");
         MaRTE.Direct_IO.Put_Error (Msg, Fatal => True);
         --  abort;
      end if;
   end Assert;

   procedure Test_OK is
   begin
      Init;
      MaRTE.Direct_IO.Put ("Test OK");
      MaRTE.Direct_IO.New_Line;
   end Test_OK;

end Reports;
