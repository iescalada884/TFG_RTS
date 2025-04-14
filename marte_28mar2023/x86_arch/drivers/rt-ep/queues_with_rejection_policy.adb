with Ada.Text_Io;

package body Queues_With_Rejection_Policy is

   procedure Increment (Index : in out Index_Type) is
   begin
      Index := (Index + 1) mod Queue_Size;
   end;

   procedure Init (Q : in out Queue) is
   begin
      Q.Head   := 0;
      Q.Tail   := 0;
      Q.Count  := 0;
      Q.Policy := DISCARD_NEWEST;
   end Init;

   function Empty (Q : in Queue) return Boolean is
   begin
      return Q.Count = 0;
   end Empty;

   function Full (Q : in Queue) return Boolean is
   begin
      return Q.Count = Queue_Size;
   end Full;

   procedure Set_Rejection_Policy (Policy : in Rejection_Policy;
                                   Q      : in out Queue)
   is
   begin
      Q.Policy := Policy;
   end Set_Rejection_Policy;

   procedure Enqueue (E : in Element; Q : in out Queue) is
   begin
      if Full (Q) then
         case Q.Policy is
            when DISCARD_NEWEST => raise Full_Queue;
            when DISCARD_OLDEST =>
                  if Enable_DEBUG_Overwrite'Last then
                     Ada.Text_Io.Put_Line ("WARNING: overwriting element");
                  end if;
                  Q.Contents (Q.Tail) := E;
                  Increment (Q.Head);
                  Increment (Q.Tail);
         end case;
      else
            Q.Contents (Q.Tail) := E;
            Increment (Q.Tail);
            Q.Count := Q.Count + 1;
      end if;
   end Enqueue;

   procedure Dequeue (E : out Element; Q : in out Queue) is
   begin
      if Empty (Q) then
         raise Empty_Queue;
      end if;
      E := Q.Contents (Q.Head);
      Increment (Q.Head);
      Q.Count := Q.Count - 1;
   end Dequeue;

   procedure Read_First (E : out Element; Q : in Queue) is
   begin
      if Empty (Q) then
         raise Empty_Queue;
      end if;
      E := Q.Contents (Q.Head);
   end Read_First;

   function Number_Of_Elements (Q : in Queue) return Natural is
   begin
      return Q.Count;
   end Number_Of_Elements;

end Queues_With_Rejection_Policy;
