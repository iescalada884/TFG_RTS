with Ada.Text_Io; use Ada.Text_Io;
with Ada.Exceptions;
with Priority_FIFO_Queues;

procedure Test_Priority_FIFO_Queues is

   procedure Pause is
      H : Character;
   begin
      Put(" Press..");
      Get_Immediate(H);
      New_Line;
   end Pause;

   Max_Elem_Same_Prio : constant := 3;

   type FIFO_Index is mod Max_Elem_Same_Prio;
   subtype Priority is Integer range 1 .. 5;

   package Prio_FIFO_Queues_Pkg is new Priority_FIFO_Queues
      (FIFO_Index  => FIFO_Index,
       Element     => Integer,
       Priority    => Priority,
       ">"         => ">");
   use Prio_FIFO_Queues_Pkg;

   Q : Prio_Queue;
   E : Integer;
   P : Priority;

begin

   Put_Line ("Tests for Priority_FIFO_Queues:");
   Pause;

   Set_Rejection_Policy (Q, DISCARD_OLDEST);

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));

   for J in Integer range 1 .. Max_Elem_Same_Prio loop
      for I in Integer range 1 .. 5 loop
         Enqueue (Q, I, I);
         Put_Line ("Enqueue "&Integer'Image (I)&" with prio "&Integer'Image (I));
      end loop;
   end loop;

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));
   Pause;

   begin
         Put_Line ("Enqueue "&Integer'Image (7)&" with prio "&Integer'Image (1));
         Enqueue (Q, 7, 1);
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
   end;

   Pause;

   begin
         Put_Line ("Enqueue "&Integer'Image (6)&" with prio "&Integer'Image (1));
         Enqueue (Q, 6, 1);
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
   end;

   Pause;

   begin
         Put_Line ("Enqueue "&Integer'Image (4)&" with prio "&Integer'Image (3));
         Enqueue (Q, 4, 3);
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
   end;

   Pause;

   begin
         Set_Rejection_Policy (Q, DISCARD_NEWEST);
         Put_Line ("Enqueue "&Integer'Image (45)&" with prio "&Integer'Image (3));
         Enqueue (Q, 45, 3);
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
   end;

   Pause;

   begin
         Set_Rejection_Policy (Q, DISCARD_OLDEST);
         Put_Line ("Enqueue "&Integer'Image (43)&" with prio "&Integer'Image (3));
         Enqueue (Q, 43, 3);
   exception
      when The_Error : others =>
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
   end;

   Pause;

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));
   Pause;

   Read_First (Q, E, P);
   Put_Line ("Read_First "&Integer'Image (E)&", prio "&Integer'Image (P));

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));
   Pause;

   for I in 1 .. Max_Elem_Same_Prio*Priority'Last loop
      Dequeue (Q, E, P);
      Put_Line ("Dequeue "&Integer'Image (E)&", prio "&Integer'Image (P));
   end loop;

   Put_Line ("Empty? "&Boolean'Image (Empty (Q)));
   Put_Line ("Full? "&Boolean'Image (Full (Q)));

exception
   when The_Error : others =>
      Ada.Text_IO.Put_Line("Unknown error:");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      raise;
end Test_Priority_FIFO_Queues;
