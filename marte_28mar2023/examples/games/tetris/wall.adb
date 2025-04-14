with Screen, Text_IO;
with Console_Management;

package body Wall is

   subtype Width_Inner is Width range
      Width'Succ(Width'First) .. Width'Pred(Width'Last);
   subtype Height_Inner is Height range
      Height'First .. Height'Pred(Height'Last);

   Pick_Array : array(Styles) of Brick_Type :=
    (((-1, 1), (0, 1), (1, 1), (2, 1)), -- Long
     ((0, 0), (0, 1), (0, 2), (1, 2)),
     ((0, 0), (1, 0), (0, 1), (0, 2)),
     ((0, 0), (0, 1), (1, 0), (1, 1)),  -- Square
     ((0, 0), (0, 1), (1, 1), (1, 2)),
     ((1, 0), (1, 1), (0, 1), (0, 2)),
     ((1, 0), (0, 1), (1, 1), (1, 2)));

   Colors : array(Styles) of Console_Management.BkColors :=
     (Console_Management.Blue,
      Console_Management.Green,
      Console_Management.Cyan,
      Console_Management.Red,
      Console_Management.Magenta,
      Console_Management.Brown,
      Console_Management.LightGray);


   type Quad is
      record
         Ocup:BOOLEAN;
         Bt:Styles;
      end record;

   subtype Erase_State is Integer range 1..4;

   type Line is array(Width_Inner) of Quad;
   type Wall_Type is array(Height_Inner) of Line;
   Tetris_Wall : Wall_Type;
   Lines_Cont : integer := 0;

   procedure Paint_Brick( Style: in Styles ) is
   begin
      Console_Management.Set_Text_Background_Color(Colors(Style));
      Text_IO.Put_Line("  ");
      Console_Management.Set_Text_Background_Color(Console_Management.Black);
   end Paint_Brick;

   procedure Paint_Erase( S : in Erase_State ; St: in Styles) is
   begin
      Console_Management.Set_Text_Background_Color(Colors(St));
      Console_Management.Set_Text_Color(Console_Management.Black);
      case S is
         when 4 => Text_IO.Put_Line(Character'Val(178)&Character'Val(178));
         when 3 => Text_IO.Put_Line(Character'Val(177)&Character'Val(177));
         when 2 => Text_IO.Put_Line(Character'Val(176)&Character'Val(176));
         when 1 => Text_IO.Put_Line("  ");
      end case;
      Console_Management.Set_Text_Color(Console_Management.White);
      Console_Management.Set_Text_Background_Color(Console_Management.Black);
   end Paint_Erase;


   procedure Write_Lines is
   begin
      Screen.MoveCursor((Column => 8, Row => 21));
      Text_IO.Put_Line("Total Lines : " & Integer'Image(Lines_Cont));
   end Write_Lines;


   function Pick ( Style : in Styles ) return Brick_Type is
   begin
      return Pick_Array ( Style );
   end Pick;

   procedure Put_Wall is
      Tetris_Piece : BOOLEAN;
   begin
      for Y in Height_Inner loop
         for X in Width_Inner loop
            Tetris_Piece := Tetris_Wall(Y)(X).Ocup;
            Screen.MoveCursor((Column => 30 + X*2, Row => 2 + Y));
            if Tetris_Piece then
               Paint_Brick(Tetris_Wall(Y)(X).Bt);
            else
               Text_IO.Put_Line("  ");
            end if;
         end loop;
      end loop;
   end Put_Wall;

   procedure Initialize is
   begin
      for I in Height_Inner loop
         for J in  Width_Inner loop
            Tetris_Wall(I)(J).Ocup := FALSE;
            Tetris_Wall(I)(J).Bt := 1;
         end loop;
      end loop;
      Console_Management.Set_Text_Color(Console_Management.Yellow);
      for Y in Height loop
         for X in Width loop
            Screen.MoveCursor((Column => 30 + X*2, Row => 2 + Y));
            if not (Y in Height_Inner'First .. Height_Inner'Last and
               X in Width_Inner'First .. Width_Inner'Last) then
               Text_IO.Put_Line("##"); --Character'Val(219)&Character'Val(219));
            end if;
         end loop;
      end loop;
      Console_Management.Set_Text_Color(Console_Management.White);
      Lines_Cont := 0;
      Write_Lines;
   end Initialize;

   procedure Next is
      Brick : Brick_Type := Pick (Style_Next);
      Brick_Old : Brick_Type := Pick (Style_Now);
      Bx, By : integer;
   begin
      for I in Brick_Old'range loop
         Bx := Brick_Old(I).X;
         By := Brick_Old(I).Y;
         Screen.MoveCursor((Column => 12 + Bx*2, Row => 10 + By));
         Text_IO.Put_Line("  ");
      end loop;
      for I in Brick'range loop
         Bx := Brick(I).X;
         By := Brick(I).Y;
         Screen.MoveCursor((Column => 12 + Bx*2, Row => 10 + By));
         Paint_Brick(Style_Next);
      end loop;
      Screen.MoveCursor((Column => 9, Row => 13));
      Text_IO.Put_Line("Next Brick");
   end Next;

   procedure Put(Brick : in Brick_Type;
                 X     : in Width;
                 Y     : in Height) is
      Bx, By : Natural;
   begin
      for I in Brick'range loop
         Bx := X + Brick(I).X;
         By := Y + Brick(I).Y;
         Screen.MoveCursor((Column => 30 + Bx*2, Row => 2 + By));
         Paint_Brick(Style_Now);
      end loop;
   end Put;

   procedure Erase(Brick : in Brick_Type;
                   X     : in Width;
                   Y     : in Height) is
      Bx, By : Natural;
   begin
      for I in Brick'range loop
         Bx := X + Brick(I).X;
         By := Y + Brick(I).Y;
         Screen.MoveCursor((Column => 30 + Bx*2, Row => 2 + By));
         Text_IO.Put_Line("  ");
      end loop;
   end Erase;

   procedure Place(Brick : in Brick_Type;
                   X     : in Width;
                   Y     : in Height) is
      Bx, By : Natural;
   begin
      for I in Brick'range loop
         Bx := X + Brick(I).X;
         By := Y + Brick(I).Y;
         Tetris_Wall(By)(Bx).Ocup := TRUE;
         Tetris_Wall(By)(Bx).Bt := Style_Now;
      end loop;
   end Place;

   function Examine(Brick : in Brick_Type;
                    X     : in Width;
                    Y     : in Height) return Boolean is
      Bx, By : Natural;
   begin
      for I in Brick'range loop
         Bx := X + Brick(I).X;
         By := Y + Brick(I).Y;
         if not (Bx in Width_Inner) or else not (By in Height_Inner) then
            return False;
         elsif Tetris_Wall(By)(Bx).Ocup then
            return False;
         end if;
      end loop;
      return True;
   end Examine;

   procedure Erase_Lines is
      Line_No     : array(Height_Inner) of Height_Inner;
      No_Of_Lines : Natural := Height_Inner'First - 1;
   begin
      for Y in Height_Inner loop
         for X in Width_Inner loop
            exit when not Tetris_Wall(Y)(X).Ocup;
            if X = Width_Inner'Last then
               No_Of_Lines := No_Of_Lines + 1;
               Lines_Cont := Lines_Cont +1;
               Line_No(No_Of_Lines) := Y;
            end if;
         end loop;
      end loop;
      if Height_Inner'First <= No_Of_Lines then
         for S in Erase_State loop
            for I in Height_Inner'First .. No_Of_Lines loop
               for X in Width_Inner loop
                  Screen.MoveCursor((Column => 30 + X*2, Row => 2 + Line_No(I)));
                  Paint_Erase(S,Tetris_Wall(Line_No(I))(X).Bt);
               end loop;
            end loop;
            delay 0.3;
         end loop;
         for I in Height_Inner'First .. No_Of_Lines loop
             Tetris_Wall(Height_Inner'First + 1 .. Line_No(I)) :=
             Tetris_Wall(Height_Inner'First .. Line_No(I) - 1);
         end loop;
         Put_Wall;
      end if;
      Write_Lines;
   end Erase_Lines;

end Wall;
