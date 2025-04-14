-----------------------------------------------------------------------
--                      PACKAGE BODY PRIORITY_QUEUE
--                      ===========================
--
--  The implementation of the priority queue is a heapform heap
--  [W. AMSBURY, "Data Structures. From Arrays to Priority Queues",
--  Wadsworth, 1985.
--
--  This implementation has the following characteristics:
--
--      Insertion : O(log n)
--      Highest_priority extraction : O(log n)
--      Lowest_priority extraction : O(n)
--      Read highest priority : O(1)
--      Deletion : O(n) (Change is Deletion + Insertion = O(n)).
-----------------------------------------------------------------------
-- Date: 17/3/2005
-------------------------------------------------------------------------------

with Queue_Exceptions;

package body Priority_Queues is

   ------------
   -- UpHeap --
   ------------

   procedure UpHeap (N : in Positive;
                     Q : in out Queue) is

      P : Positive;
      K : Positive := N;
      Temp : Cell;

   begin
      while K >= 2 loop
         P:= K/2;
         if Q.Q(K).Pri > Q.Q(P).Pri then
            Temp:=Q.Q(K);
            Q.Q(K):=Q.Q(P);
            Q.Q(P):=Temp;
            K:=P;
         else
            exit;
         end if;
      end loop;
   end UpHeap;

   procedure Init(Q : in out Queue) is
   begin
      Q.Length:=0;
   end Init;

   -----------
   -- Empty --
   -----------

   function Empty (Q : Queue) return Boolean is

   begin
      return Q.Length=0;
   end Empty;

   ------------
   -- Length --
   ------------

   function Length (Q : Queue) return Natural is

   begin
      return Q.Length;
   end Length;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (E : in Element;
                      P : in Priority;
                      Q : in out Queue) is
   begin
      if Q.Length=Size then
         raise Queue_Exceptions.Full_Queue;
      end if;
      Q.Length:=Q.Length+1;
      Q.Q(Q.Length):=(E,P);
      UpHeap(Q.Length,Q);
   end Enqueue;


   --------------
   -- DownHeap --
   --------------

   procedure DownHeap (K : in Positive;
                       N : in Natural;
                       Q : in out Queue) is

      J : Integer:=2*K;
      Half : Positive:=K;
      Temp : Cell:= Q.Q(K);

   begin
      while J<=N loop
         if (J<N) and then (Q.Q(J+1).Pri>Q.Q(J).Pri) then
            J:=J+1;
         end if;
         if not (Q.Q(J).Pri > Temp.Pri) then
            Q.Q(Half):=Temp;
            return;
         end if;
         Q.Q(Half):=Q.Q(J);
         Half:=J;
         J:=2*J;
      end loop;
      Q.Q(Half):=Temp;
   end DownHeap;


   -------------
   -- Dequeue --
   -------------

   procedure Dequeue (E : out Element;
                      P : out Priority;
                      Q : in out Queue) is

   begin
      if Q.Length=0 then
         raise Queue_Exceptions.Empty_Queue;
      end if;
      E:=Q.Q(1).E;
      P:=Q.Q(1).Pri;
      Q.Q(1):=Q.Q(Q.Length);
      Q.Length:=Q.Length-1;
      DownHeap(1,Q.Length,Q);
   end Dequeue;

   --------------------
   -- Dequeue_Middle --
   --------------------

   procedure Dequeue_Middle (E : in out Element;
                             P : out Priority;
                             Q : in out Queue;
                             Found : out Boolean) is

      I : Integer :=0;
      Parent : Integer;

   begin
      Found:=False;
      while I<Q.Length loop
         I:=I+1;
         if E=Q.Q(I).E then
            Found:=True;
            E:=Q.Q(I).E;
            P:=Q.Q(I).Pri;
            Q.Q(I):=Q.Q(Q.Length);
            Q.Length:=Q.Length-1;
            Parent:=I/2;
            if Parent=0 or else Q.Q(Parent).Pri > Q.Q(I).Pri then
               DownHeap(I,Q.Length,Q);
            else
               Upheap(I,Q);
            end if;
            exit;
         end if;
      end loop;
   end Dequeue_Middle;

   --------------------
   -- Dequeue_Middle --
   --------------------

   procedure Dequeue_Middle (E : in out Element;
                             P : out Priority;
                             Q : in out Queue) is

      I : Integer :=0;
      Parent : Integer;
      Found:Boolean:=False;

   begin
      while I<Q.Length loop
         I:=I+1;
         if E=Q.Q(I).E then
            Found:=True;
            E:=Q.Q(I).E;
            P:=Q.Q(I).Pri;
            Q.Q(I):=Q.Q(Q.Length);
            Q.Length:=Q.Length-1;
            Parent:=I/2;
            if Parent=0 or else Q.Q(Parent).Pri > Q.Q(I).Pri then
               DownHeap(I,Q.Length,Q);
            else
               Upheap(I,Q);
            end if;
            exit;
         end if;
      end loop;
      if not Found then
         raise Queue_Exceptions.Not_Found;
      end if;
   end Dequeue_Middle;


   ----------------
   -- Read_First --
   ----------------

   procedure Read_First (E : out Element;
                         P : out Priority;
                         Q : in Queue) is

   begin
      if Q.Length=0 then
         raise Queue_Exceptions.Empty_Queue;
      else
         E:=Q.Q(1).E;
         P:=Q.Q(1).Pri;
      end if;
   end Read_First;

   --------------
   -- Set_Prio --
   --------------

   procedure Set_Prio (E : in Element;
                       P : in Priority;
                       Q : in out Queue) is

      Elem : Element;
      Parent : Integer;
   begin
      for I in 1..Q.Length loop
         if E=Q.Q(I).E then
            if P=Q.Q(I).Pri then
               return;
            else
               Elem:=Q.Q(I).E;
               Q.Q(I):=Q.Q(Q.Length);
               Q.Length:=Q.Length-1;
               Parent:=I/2;
               if Parent=0 or else Q.Q(Parent).Pri > Q.Q(I).Pri then
                  DownHeap(I,Q.Length,Q);
               else
                  Upheap(I,Q);
               end if;
               DownHeap(I,Q.Length,Q);
               Enqueue(Elem,P,Q);
               return;
            end if;
         end if;
      end loop;
      raise Queue_Exceptions.Not_Found;
   end Set_Prio;

end Priority_Queues;
