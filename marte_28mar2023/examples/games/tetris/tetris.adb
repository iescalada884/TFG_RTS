with Screen, Bricks, Wall, Arrival, Text_IO;
with Console_Management;
--  with Debug_Marte;

procedure Tetris is

   pragma Priority(4);

   Ch : Character;
   Ok : Boolean;
   Available : Boolean;

begin
   --  Debug_Marte.Init_Serial_Communication_With_Gdb (Debug_Marte.SERIAL_PORT_1);
   --  Debug_Marte.Set_Break_Point_Here;
   Console_Management.Set_Raw_Mode;
   --  Console_Management.Reset_Blocking_Mode; -- deberia estar en Console_Management
   Console_Management.Disable_Echo;

   --  {<<MAR}
   Text_IO.New_Line (4);
   Console_Management.Set_Text_Color(Console_Management.Red);
   Text_IO.Put_Line ("                            T e t r i s");
   Text_IO.Put_Line ("                           -------------");
   Text_IO.New_Line;
   Text_IO.Put_Line ("                Originaly written by Michael Feldman");
   Text_IO.Put_Line ("           The George Washington University - July, 1995 ");
   Text_IO.Put_Line ("                 Adapted to MaRTE OS by Pedro Espeso");
   Text_IO.New_Line (5);
   Text_IO.Put ("Press any key to start playing Tetris...");
   loop
      Text_IO.Get_immediate (Ch, Available);
      exit when Available;
      delay 0.5;
   end loop;
   --  {MAR>>}

   Console_Management.Set_Text_Color(Console_Management.White);

   loop
      Screen.ClearScreen;

      Wall.Initialize;
      Screen.MoveCursor((Column => 10, Row => 3));
      Console_Management.Set_Text_Color(Console_Management.Red);
      Text_IO.Put_Line( " TETRIS Ada " );
      Console_Management.Set_Text_Color(Console_Management.White);
      Screen.MoveCursor((Column => 1, Row => 5));
      Text_IO.Put_Line( "a=drop o=left q=spin p=right");

      Bricks.Move.Start;
      Arrival.Manager.Start;
      Arrival.Timer.Start;
      Arrival.Speeder.Start;


      Outer : loop
         loop
            Text_IO.Get_immediate (Ch, Available);
            exit when Available;
            --  Text_IO.Get(Ch);
            --  exit when integer(Character'Pos(Ch))/=0;
            delay 0.1;
         end loop;

         exit Outer when Bricks.Finished;
         case Ch is
            when 'a'|'2' =>
               loop
                  select
                     Bricks.Move.Drop(Ok);
                  else
                     Ok := TRUE; -- Keep dropping the brick
                  end select;
                  exit when not Ok;
               end loop;
               delay 1.0;
            when 'o'|'4' =>
               select
                  Bricks.Move.Left;
               else
                  null;
               end select;
            when 'q' | ' ' | '5' | '8'=>
               select
                  Bricks.Move.Rotation;
               else
                  null;
               end select;
            when 'p' | '6'  =>
               select
                  Bricks.Move.Right;
               else
                  null;
               end select;
            when others =>
               null;
         end case;
      end loop Outer;

      Arrival.Speeder.Stop;
      Arrival.Timer.Stop;
      Bricks.Move.Stop;
      Arrival.Manager.Stop;

      exit when Ch = 'N' or Ch = 'n';

   end loop;

   Screen.ClearScreen;
   abort Bricks.Move;
   abort Arrival.Timer;
   abort Arrival.Manager;
   abort Arrival.Speeder;

end Tetris;
