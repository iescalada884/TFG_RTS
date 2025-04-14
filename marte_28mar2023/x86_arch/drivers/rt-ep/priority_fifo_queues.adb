----------------------------------------------------------------------------
--  ---------------------           RT-EP           ------------------------
----------------------------------------------------------------------------
--                         Copyright (C) 2003-2005
--                     Universidad de Cantabria, SPAIN
--                        http://www.ctr.unican.es/
--
--    This program is free software; you can redistribute it and/or
--    modify it under the terms of the GNU General Public
--    License as published by the Free Software Foundation; either
--    version 2 of the License, or (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--    General Public License for more details.
--
--    You should have received a copy of the GNU General Public
--    License along with this program; if not, write to the
--    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
--    Boston, MA 02111-1307, USA.
--
--    As a special exception, if other files instantiate generics from
--    this unit, or you link this unit with other files to produce an
--    executable, this  unit  does not  by itself cause  the resulting
--    executable to be covered by the GNU General Public License. This
--    exception does not however invalidate any other reasons why the
--    executable file  might be covered by the  GNU Public License.
--  ------------------------------------------------------------------------
--                                                     RT-EP /Ada95 {V1.1}
--
--  'r t e p p r o t o c o c h a n n e l s p r i o r i t y q u e u e . a d b'
--
--                                     Ada
--
--
--  File 'rtepprotocochannelspriorityqueue.adb'                    By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  Priority FIFO queues. All the code except from the FIFO behaviour is
--  borrowed from Michael Gonzalez Harbour implementation (see headers below)
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
--
--
--
-----------------------------------------------------------------------------
with Ada.Text_Io;

package body Priority_FIFO_Queues is

   procedure UpHeap (Q : in out Prio_Queue;
                     N : in Prio_Index) is
      P : Prio_Index;
      K : Prio_Index := N;
      Temp : Cell;
   begin
      while K >= 2 loop
         P := K / 2;
         if Q.Queue (K).Pri > Q.Queue (P).Pri then
            Temp := Q.Queue (K);
            Q.Queue (K) := Q.Queue (P);
            Q.Queue (P) := Temp;
            K := P;
         else
            exit;
         end if;
      end loop;
   end UpHeap;

   procedure DownHeap (Q : in out Prio_Queue;
                       K : in Prio_Index;
                       N : in Queue_Size_Type) is
      J : Integer := 2*K;
      Half : Prio_Index := K;
      Temp : constant Cell := Q.Queue (K);
   begin
      while J <= N loop
         if (J < N) and then (Q.Queue (J + 1).Pri > Q.Queue (J).Pri) then
            J := J + 1;
         end if;
         if not (Q.Queue (J).Pri > Temp.Pri) then
            Q.Queue (Half) := Temp;
            return;
         end if;
         Q.Queue (Half) := Q.Queue (J);
         Half := J;
         J := 2 * J;
      end loop;
      Q.Queue (Half) := Temp;
   end DownHeap;

   --------------------------
   -- Set_Rejection_Policy --
   --------------------------

   procedure Set_Rejection_Policy (Q      : in out Prio_Queue;
                                   Policy : in Rejection_Policy)
   is
   begin
      Q.Policy := Policy;
   end Set_Rejection_Policy;

   -----------
   -- Empty --
   -----------

   function Empty (Q : in Prio_Queue) return Boolean is
   begin
      return Q.Total_Length = 0;
   end Empty;

   ----------
   -- Full --
   ----------

   function Full (Q : in Prio_Queue) return Boolean is
   begin
      return Q.Total_Length >= Queued_Elements'Last;
   end Full;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (Q : in out Prio_Queue;
                      E : in Element;
                      P : in Priority) is
      P_Index : constant Prio_Index := Prio_Index (P);
   begin

      if Q.Total_Length >= Queued_Elements'Last then
         case Q.Policy is
            when DISCARD_NEWEST => raise Full_Queue;
            when DISCARD_OLDEST => null;
         end case;
      end if;

      if Q.FIFO (P_Index).Dirty then
         --  In this case we have to enqueue in the stack.
         if (Q.FIFO (P_Index).Last + 1) = Q.FIFO (P_Index).First then
            case Q.Policy is
               when DISCARD_NEWEST => raise Priority_Storage_Full;
               when DISCARD_OLDEST =>
                  if Enable_DEBUG_OverWritten'Last then
                     Ada.Text_Io.Put_Line ("WARNING: overwriting element");
                  end if;
                  Q.FIFO (P_Index).First := Q.FIFO (P_Index).First + 1;
                  Q.Total_Length := Q.Total_Length - 1;
            end case;
         end if;
         Q.FIFO (P_Index).C (Q.FIFO (P_Index).Last) := (E, P);
         Q.FIFO (P_Index).Last := Q.FIFO (P_Index).Last + 1;
         Q.FIFO (P_Index).Any_Element := True;
      else
         --  If there isn't any element of that priority, we enqueue it.
         Q.Queue_Length := Q.Queue_Length + 1;
         Q.Queue (Q.Queue_Length) := (E, P);
         UpHeap (Q, Prio_Index (Q.Queue_Length));
         Q.FIFO (P_Index).Dirty := True;
      end if;

      Q.Total_Length := Q.Total_Length + 1;

   end Enqueue;

   -------------
   -- Dequeue --
   -------------

   procedure Dequeue (Q : in out Prio_Queue;
                      E : out Element;
                      P : out Priority) is
      P_Index : Prio_Index;
   begin
      if Q.Total_Length = 0 then
         raise Empty_Queue;
      end if;

      E := Q.Queue (1).E;
      P := Q.Queue (1).Pri;

      P_Index := Prio_Index (Q.Queue (1).Pri);

      if Q.FIFO (P_Index).Any_Element then
         Q.Queue (1) := Q.FIFO (P_Index).C (Q.FIFO (P_Index).First);
         Q.FIFO (P_Index).First := Q.FIFO (P_Index).First + 1;

         if Q.FIFO (P_Index).First = Q.FIFO (P_Index).Last then
            Q.FIFO (P_Index).Any_Element := False;
         end if;
      else

         Q.FIFO (P_Index).Dirty := False;
         Q.Queue (1) := Q.Queue (Q.Queue_Length);
         Q.Queue_Length := Q.Queue_Length - 1;
         DownHeap (Q, 1, Q.Queue_Length);
      end if;

      Q.Total_Length := Q.Total_Length - 1;

   end Dequeue;

   ----------------
   -- Read_First --
   ----------------

   procedure Read_First (Q : in out Prio_Queue;
                         E : out Element;
                         P : out Priority) is

   begin
      if Q.Total_Length = 0 then
         raise Empty_Queue;
      else
         E := Q.Queue (1).E;
         P := Q.Queue (1).Pri;
      end if;
   end Read_First;

end Priority_FIFO_Queues;
