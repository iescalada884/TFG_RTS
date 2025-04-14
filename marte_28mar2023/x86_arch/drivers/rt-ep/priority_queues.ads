-----------------------------------------------------------------------
--                            PACKAGE PRIORITY_QUEUE
--                            ======================
--
-- This package defines a priority queue type and the operations that
-- can be performed on this type:
--
--     Empty and Full queue functions.
--     Enqueue : insert in the queue, in priority order.
--     Dequeue : extract the highest priority element from the queue.
--               No particular order is imposed on equal priority
--               elements.
--     Dequeue_Middle : extract the queue element that matches the
--                      supplied argument.
--     Read_First : return the value of the highest priority element
--                  in the queue, without extracting it from the queue.
--     Set_Prio : change the priority of the element that
--                matches the supplied argument
--     Update : change the element that matches the supplied argument
--              (possibly comparing just the relevant fields) to the full
--              supplied value
--
-- This version contains a fixed-size queue
--
-- It is written as a generic package, in which the generic parameters
-- are:
--
--     Element : type of the queue elements
--     Priority : type of the priority used for ordering the queue
--                elements
--     ">" : Function used to order the priorities.
--     "=" : Used by dequeue_middle to compare the queue elements with
--           the element supplied by the caller.
-------------------------------------------------------------------------------
-- Date: 17/3/2005
-------------------------------------------------------------------------------

generic
   Size : Integer;
   type Element is private;
   type Priority is private;
   with function ">" (Left,Right : in Priority) return BOOLEAN;
   with function "=" (Left,Right : in Element) return Boolean;

package Priority_Queues is

   type Queue is private;

   procedure Init
     (Q   : in out Queue);

   function Empty
     (Q : Queue)
     return Boolean;

   procedure Enqueue
     (E : in Element;
      P : in Priority;
      Q : in out Queue);

   procedure Dequeue
     (E : out Element;
      P : out Priority;
      Q : in out Queue);
   -- may raise Queue_Exceptions.Empty

   procedure Set_Prio
     (E : in Element;
      P : in Priority;
      Q : in out Queue);
   -- may raise Queue_Exceptions.Not_Found

   procedure Dequeue_Middle
     (E : in out Element;
      P : out Priority;
      Q : in out Queue;
      Found : out Boolean);

   procedure Dequeue_Middle
     (E : in out Element;
      P : out Priority;
      Q : in out Queue);
   -- may raise Queue_Exceptions.Not_Found

   procedure Read_First
     (E : out Element;
      P : out Priority;
      Q : in Queue);
   -- may raise Queue_Exceptions.Empty

private

   type Cell is record
      E : Element;
      Pri : Priority;
   end record;

   type Storing_Space is array (1..Size) of Cell;

   type Queue is record
      Length : Integer range 0..Size:=0;
      Q : Storing_Space;
   end record;

end Priority_Queues;
