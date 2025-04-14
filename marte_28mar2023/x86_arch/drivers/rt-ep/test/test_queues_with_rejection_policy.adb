with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions;
with Queues_With_Rejection_Policy;

procedure Test_Queues_With_Rejection_Policy is

   procedure Pause is
      H : Character;
   begin
      Put(" Press..");
      Get_Immediate(H);
      New_Line;
   end Pause;

   package Queues_Pkg is new Queues_With_Rejection_Policy
      (Queue_Size  => 5,
       Element     => Integer);
   use Queues_Pkg;

   Q : Queue;
   E : Integer;

begin

   Put_Line ("Tests for Queues:");
   Pause;

   Init (Q);

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));

   for I in Integer range 1 .. 5 loop
      Enqueue (I, Q);
      Put_Line ("Enqueue "&Integer'Image (I));
   end loop;

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));
   Pause;

   begin
         Put_Line ("Enqueue "&Integer'Image (6));
         Enqueue (6, Q);
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
   end;

   Pause;

   Set_Rejection_Policy (DISCARD_OLDEST, Q);

   begin
         Put_Line ("Enqueue "&Integer'Image (7));
         Enqueue (7, Q);
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
   end;

   Pause;

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));
   Pause;

   Read_First (E, Q);
   Put_Line ("Read_First "&Integer'Image (E));

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));
   Pause;

   for I in 1 .. 5 loop
      Dequeue (E, Q);
      Put_Line ("Dequeue "&Integer'Image (E));
   end loop;

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));

exception
   when The_Error : others =>
      Ada.Text_IO.Put_Line("Unknown error:");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      raise;
end Test_Queues_With_Rejection_Policy;
