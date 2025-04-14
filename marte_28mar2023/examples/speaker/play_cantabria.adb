with MaRTE_OS;
with Ada_Music;
     use Ada_Music;
with Text_IO;
procedure Play_Cantabria is
c:Character;
begin
   Open;
   Play(Cantabria);
   loop
      text_IO.Get(c);
      case c is
         when 'p'|'P' => play(Cantabria);
         when 'r'|'R' => repeat_play(Cantabria);
         when 'c'|'C' => cancel;
         when 'f'|'F' => exit;
         when others => null;
      end case;
   end loop;
   Ada_Music.Close;
end Play_Cantabria;
