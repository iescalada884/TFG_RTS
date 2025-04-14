with MaRTE_OS;
with Ada_Music;
with Text_IO;
     use Ada_Music;
procedure Play_Christmas is
   c:Character;
begin
   Open;
   Play(Christmas);
   loop
      text_IO.Get(c);
      case c is
         when 'p'|'P' => play(Christmas);
         when 'r'|'R' => repeat_play(Christmas);
         when 'c'|'C' => cancel;
         when 'f'|'F' => exit;
         when others => null;
      end case;
   end loop;
   Ada_Music.Close;
end Play_Christmas;