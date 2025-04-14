--  with Text_IO;
with Console_Management;
--  with Basic_Integer_Types;
--  with Keyboard;
--  with Keyboard_Functions;

package body Screen is

  procedure Beep is
  begin
    null;--Text_IO.Put (Item => ASCII.BEL);
  end Beep;

  procedure ClearScreen is
  begin
    Console_Management.Clear_Screen;
  end ClearScreen;

  procedure MoveCursor (To: in Position) is
  begin
     Console_Management.Set_Cursor(
                                   Console_Management.Rows(To.Row),
                                   Console_Management.Columns(To.Column));
  end MoveCursor;

end Screen;
