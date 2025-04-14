generic
   type Element is private;
   Max :  Integer := 128;
package FIFO_Queue is

   type Error is (Correct, No_Elements, No_Room);

   procedure Initialize_Queue;
   pragma Inline (Initialize_Queue);

   procedure Insert_In_Queue
     (The_Element : in  Element;
      Error_Code  : out Error);
   pragma Inline (Insert_In_Queue);

   procedure Extract_From_Queue
     (The_Element : out Element;
      Error_Code : out Error);
   pragma Inline (Extract_From_Queue);

   function Queue_Is_Empty
      return Boolean;
   pragma Inline (Queue_Is_Empty);

end FIFO_Queue;
