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
--  'r t e p p r o t o c o c h a n n e l s p r i o r i t y q u e u e . a d s'
--
--                                     Ada
--
--
--  File 'rtepprotocochannelspriorityqueue.ads'                    By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  Priority FIFO queues. All the code except from the FIFO behaviour is
--  borrowed from Michael Gonzalez Harbour implementation (see headers below)
--
--
--
--
-----------------------------------------------------------------------------

-------------------------------------------------------------------------------
--                            PACKAGE PRIORITY_QUEUE
--                            ======================
--
--  This package defines a priority queue object and the operations that can be
--  performed on this object:
--
--     Empty and Full queue functions.
--     Enqueue : insert in the queue, in priority order.
--     Dequeue : extract the highest priority element from the queue. No
--               particular order is imposed on equal priority elements.
--     Dequeue_Middle : extract the queue element that matches the supplied
--                      argument.
--     Dequeue_Last : extract the lowest priority element from the queue. No
--                    particular order is imposed on equal priority elements.
--     Read_First : return the value of the highest priority element in the
--                  queue, without extracting it from the queue.
--
--  It is written as a generic package, in which the generic parameters are:
--
--     Size : Maximum number of elements in the queue
--     Element : type of the queue elements
--     Priority : type of the priority used for ordering the queue elements
--     ">" : Function used to order the priorities.
--     "=" : Used by dequeue_middle to compare the queue elements with
--           the element supplied by the caller.
-------------------------------------------------------------------------------
generic
   type FIFO_Index is mod <>; -- type FIFO_Index is mod Max_Elem_Same_Prio
   type Element is private;
   type Priority is range <>;
   with function ">" (Left,Right : in Priority) return Boolean;
package Priority_FIFO_Queues is

   Priority_Storage_Full   : exception;
   Empty_Queue             : exception;
   Full_Queue              : exception;

   subtype Enable_DEBUG_OverWritten is Boolean range True .. True;

   type Rejection_Policy is
      (DISCARD_OLDEST,  --  overwrite the oldest data in the FIFO Queue
       DISCARD_NEWEST); --  no overwrite

   type Prio_Queue is private;

   --------------------------
   -- Set_Rejection_Policy --
   --------------------------
   --  sets the rejection policy. By default it is set to DISCARD_NEWEST

   procedure Set_Rejection_Policy (Q      : in out Prio_Queue;
                                   Policy : in Rejection_Policy);

   -----------
   -- Empty --
   -----------
   --  returns true it the queue is empty

   function Empty (Q : in Prio_Queue) return Boolean;

   ----------
   -- Full --
   -----------
   --  returns true it the queue is COMPLETELY full. Note that it can be full
   --  for enqueuing elements with a certain priority and still returning False.

   function Full (Q : in Prio_Queue) return Boolean;

   -------------
   -- Enqueue --
   -------------
   --  Enqueues an element with a priority in the priority queue. If the
   --  rejection policy is set to DISCARD_NEWEST it may raise Full_Queue
   --  (when the whole queue is full) or Priority_Storage_Full
   --  (when the fifo queue associated to a priority is full). If the
   --  rejection policy is set to DISCARD_OLDEST, no exception will be
   --  raised and the oldest element will be overwritten
   --  (set Enable_DEBUG_OverWritten to show a warning when that occurs).

   procedure Enqueue (Q : in out Prio_Queue;
                      E : in Element;
                      P : in Priority);

   -------------
   -- Dequeue --
   -------------
   --  Dequeues the element with highest priority.
   --  If the Queue is Empty raises Empty_Queue

   procedure Dequeue (Q : in out Prio_Queue;
                      E : out Element;
                      P : out Priority);

   -------------
   -- Dequeue --
   -------------
   --  Reads (without extracting) the element with highest priority.
   --  If the Queue is Empty raises Empty_Queue
   procedure Read_First (Q : in out Prio_Queue;
                         E : out Element;
                         P : out Priority);

private

   ----------
   -- Cell --
   ----------
   --  a leave in the tree: element + priority

   type Cell is record
      E   : Element;
      Pri : Priority;
   end record;

   ---------------
   -- FIFO_Cell --
   ---------------
   --  a FIFO Queue, there will be one per priority

   type FIFO_Cell_Array is array (FIFO_Index) of Cell;

   type FIFO_Cell is record
      C : FIFO_Cell_Array;
      First : FIFO_Index := 0;
      Last : FIFO_Index := 0;
      --  The dirty state becomes true when there is an element in
      --  the heap within this priority. And false when there isn't.
      Dirty : Boolean := False;
      --  True if there are any elements in the stack. False if threr isn't
      Any_Element : Boolean := False;
   end record;

   ----------------
   -- Prio_Queue --
   ----------------
   --  Queue: the priority tree. It is represented as an array 1 .. Mx_Prio
   --  where each leave has the (Element and the Priority.)
   --
   --  FIFO: when we enqueue an element with a prio that is already assigned
   --  in the tree (queue) we have to put it in the FIFO queue. FIFO is an
   --  array 1 .. Mx_Prio where each element is a FIFO queue (=FIFO_Cell)
   --
   --  Queue_Length: the number of elements of the tree (Queue)
   --
   --  Total_Length: the total number of elements (tree + FIFO queues)
   --
   --  Rejection_Policy: indicates what the queue is full and
   --  a new message arrives

   Priority_Range : constant Integer := Integer (Priority'Last);
   subtype Prio_Index is Integer range 1 .. Priority_Range;

   type Queue_Cell_Array is array (Prio_Index) of Cell;

   type FIFO_FIFO_Cell_Array is array (Prio_Index) of FIFO_Cell;

   subtype Queue_Size_Type is Integer range 0 .. Priority_Range;

   Max_Elem_Same_Prio : constant Integer := Integer (FIFO_Index'Last) + 1;

   subtype Queued_Elements is Integer range
      0 .. (Max_Elem_Same_Prio*Priority_Range);

   type Prio_Queue is record
      Queue        : Queue_Cell_Array;
      FIFO         : FIFO_FIFO_Cell_Array;
      Queue_Length : Queue_Size_Type := 0;
      Total_Length : Queued_Elements := 0;
      Policy       : Rejection_Policy := DISCARD_NEWEST;
   end record;

end Priority_FIFO_Queues;
