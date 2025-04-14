with Ada.Text_Io;
with Persistent_Signals;
with MaRTE.Integer_Types;

procedure test_persistent_signals is

   use type MaRTE.Integer_Types.Int;

   pragma Priority (10 + 1);

   package PSS is new Persistent_Signals (Ceiling => 10);

   Unexpected_Error : exception;

   procedure Check_NZ (Result : MaRTE.Integer_Types.Int) is
   begin
      if Result /= 0 then
         raise Unexpected_Error;
      end if;
   end Check_NZ;

   PS_Ref : PSS.Persistent_Signal_Ref;
   Initialized : Boolean := False;

   task producer is
      pragma Priority (10);
   end producer;

   task body producer is
   begin
      loop
         exit when Initialized;
      end loop;
      loop
         delay 3.0;
         Ada.Text_Io.Put_Line ("sent signal!");
         Check_NZ (PSS.Signal (PS_Ref));
      end loop;
   exception
      when Unexpected_Error =>
         Ada.Text_Io.Put_Line ("exception in producer");
   end producer;

   task consumer is
      pragma Priority (10);
   end consumer;

   task body consumer is
   begin
      loop
         exit when Initialized;
      end loop;
      loop
         Check_NZ (PSS.Wait (PS_Ref));
         Ada.Text_Io.Put_Line ("received signal!");
         delay 1.0;
      end loop;
   exception
      when Unexpected_Error =>
         Ada.Text_Io.Put_Line ("exception in producer");
   end consumer;

begin
   PSS.Init (PS_Ref, False);
   Initialized := True;
end test_persistent_signals;




