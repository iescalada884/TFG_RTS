with Bricks, Wall, Calendar;
with Ada.Exceptions;
with Text_IO;

package body Arrival is

   Initial_Delay : constant := 0.6;
   Delay_Time    : Duration;

   type Unsigned is range 0..2**16;

   use Calendar;
   Seed : Unsigned := Unsigned(FLOAT(Seconds(Clock))/10.0); -- in range

   function Cheap_Random return Integer is
   begin
      Seed := (Seed * 25173 + 13849) mod 2**16;
      return Integer(Seed mod 2**15);
   end Cheap_Random;


   task body Manager is
      Done  : Boolean;
   begin
      Outer : loop
         accept Start;
         Wall.Style_Next := Wall.Styles(Cheap_Random mod Wall.Styles'LAST + 1);
         Middle : loop
            Wall.Style_Now := Wall.Style_Next;
            Wall.Style_Next := Wall.Styles(Cheap_Random mod Wall.Styles'LAST + 1);
            Wall.Next;
            select
               accept Tick;
            or
               accept Stop;
               exit Middle;
            or
               delay Delay_Time;
            end select;
            Bricks.Move.Put(X     => 5,
                            Y     => 2,
                            Brick => Wall.Pick(Wall.Style_Now),
                            Done  => Done);
            if Done then
               accept Stop;
               exit Middle;
            end if;
            for Y in Wall.Height'First + 1 .. Wall.Height'Last loop
               declare
                  Ok : Boolean;
               begin
                  select
                     accept Tick;
                  or
                     accept Stop;
                     exit Middle;
                  or
                     delay Delay_Time;
                  end select;
                  Bricks.Move.Drop(Ok);
                  if not Ok then
                     exit;
                  end if;
               end;
            end loop;
            Wall.Erase_Lines;
         end loop Middle;
      end loop Outer;
   exception
      when E : others =>
         Text_IO.Put ("Manager error: ");
         Text_IO.Put (Ada.Exceptions.Exception_Name (E));
         Text_IO.Put_Line (" " & Ada.Exceptions.Exception_Message (E));
   end Manager;

   task body Timer is
   begin
      Outer : loop
         Delay_Time := Initial_Delay;
         accept Start;
         Main : loop
            select
               accept Stop;
               exit Main;
            or
               delay Delay_Time;
            end select;
            select
               Manager.Tick;
            else
               null;
            end select;
         end loop Main;
      end loop Outer;
   exception
      when E : others =>
         Text_IO.Put ("Timer error: ");
         Text_IO.Put (Ada.Exceptions.Exception_Name (E));
         Text_IO.Put_Line (" " & Ada.Exceptions.Exception_Message (E));
   end Timer;


   task body Speeder is
   begin
      Delay_Time := Initial_Delay;
      Outer : loop
         accept Start;
         Middle : loop
            for I in 1 .. 100 loop
               select
                  accept Stop;
                  exit Middle;
               or
                  delay Delay_Time;
               end select;
            end loop;
            Delay_Time := Delay_Time*9/10;
         end loop Middle;
      end loop Outer;
   exception
      when E : others =>
         Text_IO.Put ("Speeder error: ");
         Text_IO.Put (Ada.Exceptions.Exception_Name (E));
         Text_IO.Put_Line (" " & Ada.Exceptions.Exception_Message (E));
   end Speeder;

end Arrival;

