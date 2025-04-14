with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions;

with Commands_Queue;

procedure Test_Commands_Queue is

   pragma Priority (6);

   type Operation is (Add, Substract);

   type Command_Type is record
      Op : Operation;
      A  : Integer;
      B  : Integer;
   end record;

   package My_Commands_Queue is new Commands_Queue
      (Size => 7,
       Generic_Command => Command_Type,
       Prio => 30);
   use type My_Commands_Queue.Command_Status;

   task Executor is
      pragma Priority (4);
   end Executor;

   task body Executor is
      Cmd   : My_Commands_Queue.Command;
      G_Cmd : Command_Type;
   begin
      loop
         My_Commands_Queue.Dequeue (Cmd, G_Cmd);
         delay 1.0;
         case G_Cmd.Op is
            when Add =>
               Put_Line (Integer'Image (G_Cmd.A)&" + "&Integer'Image (G_Cmd.B)
                  &" = "&Integer'Image (G_Cmd.A + G_Cmd.B));
            when Substract =>
               Put_Line (Integer'Image (G_Cmd.A)&" - "&Integer'Image (G_Cmd.B)
                  &" = "&Integer'Image (G_Cmd.A - G_Cmd.B));
         end case;
         My_Commands_Queue.Set_Status (Cmd, My_Commands_Queue.Workdone);
      end loop;
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line("Unknown error:");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
         raise;
   end Executor;

   procedure Pause is
      H : Character;
   begin
      Put(" Press..");
      Get_Immediate(H);
      New_Line;
   end Pause;

   procedure Message (
         Str : String) is
   begin
      Put(Str);
      Pause;
   end Message;

   G_Cmd : Command_Type;
   Cmd : My_Commands_Queue.Command;
   Stat : My_Commands_Queue.Command_Status;

   G_Cmd_Array : array (1 .. 7) of Command_Type;
   Cmd_Array : array (1 .. 7) of My_Commands_Queue.Command;

begin

   Put_Line ("Tests for Commands_Queue:");
   Put_Line ("The Test consists on enqueuing commands (add and substract)");
   Put_Line ("that another task has to execute and print out.");
   Pause;

   Message ("TEST 1: Enqueue 3 additions sequentially and BLOCK");
   for I in 1 .. 3 loop
      Put_Line ("Enqueuing operations");
      G_Cmd := (Add, I, 10);
      My_Commands_Queue.Enqueue (G_Cmd, Cmd);
      Stat := My_Commands_Queue.Wait (Cmd);
      if Stat /= My_Commands_Queue.Workdone then
         Put_Line ("Something went wrong");
      end if;
      My_Commands_Queue.Set_Status (Cmd, My_Commands_Queue.Not_In_Use);
   end loop;

   New_Line;
   Message ("TEST 2: Enqueue 3 substractions sequentially and POLL");
   for I in 1 .. 3 loop
      Put_Line ("Enqueuing operations");
      G_Cmd := (Substract, 10, I);
      My_Commands_Queue.Enqueue (G_Cmd, Cmd);
      loop
         Stat := My_Commands_Queue.Get_Status (Cmd);
         case Stat is
            when My_Commands_Queue.Workdone =>
               exit;
            when My_Commands_Queue.Enqueued =>
               Put_Line (".");
               delay 0.4;
            when Others =>
               Put_Line ("Something went wrong");
               exit;
         end case;
      end loop;
      My_Commands_Queue.Set_Status (Cmd, My_Commands_Queue.Not_In_Use);
   end loop;

   New_Line;
   Message ("TEST 3: Enqueue "&Integer'Image(G_Cmd_Array'Length)&
      " operations at a time and BLOCK");
   Put_Line ("Enqueuing several operations");
   for I in 1 .. 3 loop
      G_Cmd_Array (I) := (Add, 10, I);
   end loop;

   for I in 4 .. 7 loop
      G_Cmd_Array (I) := (Substract, 10, I);
   end loop;

   for I in 1 .. 7 loop
      G_Cmd_Array (I) := (Substract, 10, I);
      My_Commands_Queue.Enqueue (G_Cmd_Array (I), Cmd_Array (I));
   end loop;

   for I in 1 .. 7 loop
      Stat := My_Commands_Queue.Wait (Cmd_Array(I));
      if Stat /= My_Commands_Queue.Workdone then
         Put_Line ("Something went wrong");
      end if;
      My_Commands_Queue.Set_Status (Cmd_Array(I), My_Commands_Queue.Not_In_Use);
   end loop;

exception
   when The_Error : others =>
      Ada.Text_IO.Put_Line("Unknown error:");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      raise;
end Test_Commands_Queue;
