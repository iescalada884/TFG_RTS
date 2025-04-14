-------------------------------------------------------------------------------
--  This package implements a FIFO queue where you can set the rejection
--  policy. That is, when the Queue is FULL and a new element is enqueued
--  you can say either that you want to reject it, DISCARD_NEWEST, or that
--  you want it to overwrite the oldest element in the Queue, DISCARD_OLDEST.
--
--  Daniel Sangorrin,  daniel.sangorrin@gmail.com
-------------------------------------------------------------------------------
generic
   Queue_Size : in Positive;
   type Element is private;

package Queues_With_Rejection_Policy is

   subtype Enable_DEBUG_Overwrite is Boolean range True .. True;

   type Queue is private;

   type Rejection_Policy is
      (DISCARD_OLDEST,  --  overwrite the oldest data in the FIFO Queue
       DISCARD_NEWEST); --  no overwrite

   Empty_Queue, Full_Queue : exception;

   procedure Init (Q : in out Queue);

   function Empty (Q : in Queue) return Boolean;

   function Full (Q : in Queue) return Boolean;

   procedure Enqueue (E : in Element; Q : in out Queue);

   procedure Dequeue (E : out Element; Q : in out Queue);

   procedure Read_First (E : out Element; Q : in Queue);

   function Number_Of_Elements (Q : in Queue) return Natural;

   procedure Set_Rejection_Policy (Policy : in Rejection_Policy;
                                   Q      : in out Queue);

private

   subtype Index_Type is Integer range 0 .. Queue_Size - 1;

   subtype Count_Type is Natural range 0 .. Queue_Size;

   type Contents_Type is array (Index_Type) of Element;

   type Queue is record
      Contents : Contents_Type;
      Head     : Index_Type := 0;
      Tail     : Index_Type := 0;
      Count    : Count_Type := 0;
      Policy   : Rejection_Policy := DISCARD_NEWEST;
   end record;

end Queues_With_Rejection_Policy;
