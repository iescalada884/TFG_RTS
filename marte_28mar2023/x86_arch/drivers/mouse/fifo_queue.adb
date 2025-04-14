package body FIFO_Queue is

   subtype Index is Integer range 0..Max-1;
   type Elements_Type is array (Index) of Element;
   type Queue is record
      Start,Finish : Index;
      Elements: Elements_Type;
   end record;

   The_Queue : Queue;

   function Increment(X : in Index; N : in Index:=1)
                       return Index is
   begin
      return (X+N) mod Max;
   end Increment;

   procedure Initialize_Queue is
   begin
      The_Queue.Start	:= 0;
      The_Queue.Finish	:= Max - 1;
   end Initialize_Queue;


   procedure Insert_In_Queue(The_Element : in  Element;
                             Error_Code: out Error) is
   begin
      if Increment(The_Queue.Finish,2) = The_Queue.Start
      then
         Error_Code := No_Room;
      else
         The_Queue.Finish 			:= Increment(The_Queue.Finish);
         The_Queue.Elements(The_Queue.Finish) 	:= The_Element;
         Error_Code := Correct;
      end if;
   end Insert_In_Queue;


   procedure Extract_From_Queue (The_Element : out Element;
                                 Error_Code: out Error) is
   begin
      if Queue_Is_Empty then
         Error_Code := No_Elements;
      else
         The_Element	:= The_Queue.Elements(The_Queue.Start);
         The_Queue.Start:= Increment(The_Queue.Start);
         Error_Code := Correct;
      end if;
   end Extract_From_Queue;

   function Queue_Is_Empty return Boolean is
   begin
      return Increment(The_Queue.Finish) = The_Queue.Start;
   end Queue_Is_Empty;

begin
   Initialize_Queue;

end FIFO_Queue;
