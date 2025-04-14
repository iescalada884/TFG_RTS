-------------------------------------------------------------------------------
--                        PACKAGE BODY QUEUE
--                        ==================
--
--  The implementation of the queue is with a circular array
--
--  This implementation has the following characteristics:
--
--      Insertion : O(1)
--      Extraction : O(1)
--      Read first : O(1)
-------------------------------------------------------------------------------

package body Queues is

   function Increment (An_Index : in Index; By : in Index:=1) return Index is
   begin
      return (An_Index+By) mod Queue_Size;
   end;

   function Empty (The_Queue : in Queue) return Boolean is
   begin
      return Increment(The_Queue.Tail)=The_Queue.Head;
   end Empty;


   function Full (The_Queue : in Queue) return Boolean is
   begin
      return Increment(The_Queue.Tail,2)=The_Queue.Head;
   end Full;


   procedure Init
     (The_Queue  : in out Queue)
   is
   begin
      The_Queue.Head :=0;
      The_Queue.Tail :=Queue_Size-1;
   end Init;

   procedure Enqueue
     (The_Element  : in Element;
      In_The_Queue : in out Queue)
   is
   begin
      if Full(In_The_Queue) then
         raise Full_Queue;
      else
         In_The_Queue.Tail:=Increment(In_The_Queue.Tail);
         In_The_Queue.Contents(In_The_Queue.Tail):=The_Element;
      end if;
   end Enqueue;

   procedure Dequeue
     (An_Element     : out Element;
      From_The_Queue : in out Queue)
   is
   begin
      if Empty(From_The_Queue) then
         raise Empty_Queue;
      else
         An_Element:=From_The_Queue.Contents(From_The_Queue.Head);
         From_The_Queue.Head:=Increment(From_The_Queue.Head);
      end if;
   end Dequeue;

   procedure Read_First
     (An_Element: out Element;
      From_The_Queue : in Queue)
   is
   begin
      if Empty(From_The_Queue) then
         raise Empty_Queue;
      else
         An_Element:=From_The_Queue.Contents(From_The_Queue.Head);
      end if;
   end Read_First;

   procedure Read_Last
     (An_Element     : out Element;
      From_The_Queue : in Queue)
   is
   begin
      if Empty(From_The_Queue) then
         raise Empty_Queue;
      else
         An_Element:=From_The_Queue.Contents(From_The_Queue.Tail);
      end if;
   end Read_Last;


   procedure Update_Last
     (New_Element    : in Element;
      From_The_Queue : in out Queue)
   is
   begin
      if Empty(From_The_Queue) then
         raise Empty_Queue;
      else
         From_The_Queue.Contents(From_The_Queue.Tail):=New_Element;
      end if;
   end Update_Last;

   function Number_Of_Elements
     (In_The_Queue : in Queue)
     return Natural
   is
   begin
      return (Queue_Size+In_The_Queue.Tail+1-
              In_The_Queue.Head) mod Queue_Size;
   end Number_Of_Elements;

end Queues;
