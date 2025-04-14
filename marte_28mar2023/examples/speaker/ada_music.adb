-----------------------------------------------------------------------
--                            Ada_Music                              --
--                                                                   --
--                       Copyright (C) 2001                          --
--                 Universidad de Cantabria, SPAIN                   --
--                                                                   --
-- Authors: Jose Maria Drake              drakej@unican.es           --
--                                                                   --
-- This program is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This program is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this program; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-----------------------------------------------------------------------
-- with Text_IO;               --  ***  MarteOS NO MANEJA FICHEROS  ***
with Containers_exceptions;
with MaRTE.Integer_Types;
   use type MaRTE.Integer_Types.Unsigned_8;
with MaRTE.HAL.IO;

package body Ada_Music is

   --    Retorna la frecuencia en Hz que corresponde a la nota.
   function Frequency(theNote: Scale) return Float is
   begin
      return NoteFreq(theNote);
   end Frequency;



   --    Retorna la duraci�n en segundos del tiempo base.
   function Basic_Time(t:Tempo) return Float is
   begin
      return TempoTime(t);
   end Basic_Time;

   --    Retorna el factor por el que hay que multiplicar el tiempo
   --    base para obtener la duraci�n de la nota.
   function Times_Tempo(d: Note_Duration) return Float is
   begin
      return TimeDuration(d);
   end Times_Tempo;


   --    Establece la unidad b�sica de tiempo que va a ser considerada
   --    para las nuevas notas que se introduzcan en la melod�a a
   --    partir de ese instante.
   procedure Set_Tempo(op: in out Opus; t: Tempo) is
   begin
      op.currentTempo:= t;
   end Set_Tempo;

   --    Establece el modo de percusi�n con que debe interpretarse la
   --    melod�a.
   procedure Set_PercussionMode(op: in out Opus;
                         m: Percussion_Mode)is
   begin
      op.currentPercussionMode:= m;
   end Set_PercussionMode;

   --    A�ade una nueva nota a la melod�a (nota de la escala y dura-
   --    ci�n de la misma relativa al tempo con que se est� interpre-
   --    tando).
   procedure Add_Note(op: in out Opus;
                       n: Scale;
                       d: Note_Duration)is
      tempNote:Note;
   begin
      tempNote.theNote:=n;
      tempNote.theDuration:=d;
      tempNote.theTempo:= op.currentTempo;
      CompassList.Add(tempNote,op.compass);
   end Add_Note;

--  *** La Versi�n de MarteOS NO MANEJA FICHEROS ***
--   --    Retorna un puntero a una instancia Opus en la que se ha cargado
--   --    la melod�a que estaba almacenada en el fichero cuyo pathname se
--   --    indica.
--   procedure Load(op: in out Opus; path:String) is
--      theFile:Text_IO.File_Type;
--      subtype Note_N_Range is Integer range 0..15;
--      package Note_IO is new Text_IO.Integer_IO(Note_N_Range);
--      rNote:Note_N_Range;
--      subtype Duration_N_Range is Integer range 0..5;
--      package Duration_IO is new Text_IO.Integer_IO(Duration_N_Range);
--      rDuration:Duration_N_Range ;
--      subtype Tempo_N_Range is Integer range 0..7;
--      package Tempo_IO is new Text_IO.Integer_IO(Tempo_N_Range);
--      rTempo: Tempo_N_Range;

--   begin
--      -- Lee la informaci�n del fichero.
--      Text_IO.Create(theFile,Text_IO.In_File,path);
--      while not Text_IO.End_Of_File(theFile) loop
--         Note_IO.Get(theFile,rNote);
--         Duration_IO.Get(theFile,rDuration);
--         Tempo_IO.Get(theFile,rTempo);
--         Add_Note(op,
--                  Scale'Val(rNote),
--                  Note_Duration'Val(rDuration),
--                  Tempo'Val(rTempo));
--      end loop;
--   end Load;

--  ***   MarteOS NO MANEJA FICHEROS   ***
--   --    Almacena en un fichero la melod�a.
--   procedure Save(op: Opus; path: String)is
--      theFile:Text_IO.File_Type;
--      index: compassList.Index;
--      tempNote: Note;
--      subtype Note_N_Range is Integer range 0..15;
--      package Note_IO is new Text_IO.Integer_IO(Note_N_Range);
--      subtype Duration_N_Range is Integer range 0..5;
--      package Duration_IO is new Text_IO.Integer_IO(Duration_N_Range);
--      subtype Tempo_N_Range is Integer range 0..7;
--      package Tempo_IO is new Text_IO.Integer_IO(Tempo_N_Range);
--   begin
--      Text_IO.Create(theFile,Text_IO.Out_File,path);
--      CompassList.Rewind(op.compass, index);
--      loop
--         begin
--            CompassList.Get_Next_Item(tempNote,op.compass,index);
--            Note_IO.put(theFile,Scale'pos(tempNote.theNote));
--            text_IO.put(theFile," ");
--            Duration_IO.put(theFile,Note_Duration'pos(
--                                               tempNote.theDuration));
--            text_IO.put(theFile," ");
--            Tempo_IO.put(theFile,Tempo'Pos(tempNote.theTempo));
--            text_IO.New_Line(theFile);
--         exception
--            when Containers_exceptions.No_More_Items => exit;
--         end;
--      end loop;
--      Text_IO.Close(theFile);
--   end Save;



   -- Class Music Driver
   --    Clase instanciada con la funcionalidad b�sica de Music_Driver.
   --    Permite ejecutar una melod�a sobre el altavoz de un PC, as�
   --    mismo permite codificar la medol�a a partir de su pentagrama.

--   type Byte is mod 2**8;
--   type Address is new Natural;
   type Word is mod 2**16;

   -- Timer Register Address.
   CNTR_REG: constant MaRTE.HAL.IO.IO_Port:= 16#43#;
   CNT2_REG: constant MaRTE.HAL.IO.IO_Port:= 16#42#;
   HBLT_REG: constant MaRTE.HAL.IO.IO_Port:= 16#61#;
   CLCKFREQ: constant Float  := 1193180.0;



   task body Controller is
      noteIndex: CompassList.Index;
      currentNote: Note;


      procedure StartNote(n:Scale) is
         w:Word;
      begin
        if n /= Pause
        then
           w:=WORD(Float'Rounding(CLCKFREQ/NoteFreq(n)));
           MaRTE.HAL.IO.Outb(CNT2_REG,
                   MaRTE.Integer_Types.Unsigned_8(w and  16#FF#));
           MaRTE.HAL.IO.Outb(CNT2_REG,
                   MaRTE.Integer_Types.Unsigned_8(w / 256));
           MaRTE.HAL.IO.Outb(HBLT_REG,MaRTE.HAL.IO.Inb(HBLT_REG) or 16#03#);
        else
           MaRTE.HAL.IO.Outb(HBLT_REG,MaRTE.HAL.IO.Inb(HBLT_REG) and 16#FC#);
        end if;
      end StartNote;

      procedure StopSound is
      begin
         MaRTE.HAL.IO.Outb(HBLT_REG,MaRTE.HAL.IO.Inb(HBLT_REG) AND 16#FC#);
      end StopSound;

      FinalizeTask: exception;

   begin
      loop
         select
            accept Play_One;
            if theOpus /= null
            then
               CompassList.Rewind(theOpus.compass,noteIndex);
               loop
               begin
                  CompassList.Get_Next_Item(currentNote,
                                            theOpus.compass,noteIndex);
                  StartNote(currentNote.theNote);
                  select
                     accept Cancel;
                     StopSound;
                     exit;
                  or
                     accept Finalize;
                     StopSound;
                     raise FinalizeTask;
                  or
                     delay Duration(0.8*
                                 TimeDuration(currentNote.theDuration)*
                                 TempoTime(currentNote.theTempo));
                     StopSound;
                     delay Duration(0.2*
                                 TimeDuration(currentNote.theDuration)*
                                 TempoTime(currentNote.theTempo));
                  end select;
               exception
                  when Containers_exceptions.No_More_Items =>
                     exit;
               end;
               end loop;
            end if;
         or
            accept Play_Many;
            if theOpus/= null
            then
               CompassList.Rewind(theOpus.compass,noteIndex);
               loop
               begin
                  CompassList.Get_Next_Item(currentNote,
                                            theOpus.compass,
                                            noteIndex);
                  StartNote(currentNote.theNote);
                  select
                     accept Cancel;
                     StopSound;
                     exit;
                  or
                     accept Finalize;
                     StopSound;
                     raise FinalizeTask;
                  or
                     delay Duration(0.8*
                                 TimeDuration(currentNote.theDuration)*
                                 TempoTime(currentNote.theTempo));
                     StopSound;
                    delay Duration(0.2*
                                 TimeDuration(currentNote.theDuration)*
                                 TempoTime(currentNote.theTempo));
                  end select;
               exception
                  when Containers_exceptions.No_More_Items =>
                     CompassList.Rewind(theOpus.compass,noteIndex);
               end;
               end loop;
            end if;

         or
            accept Cancel;
         or
            accept Finalize;
            raise FinalizeTask;
         end select;
         end loop;
   exception
      when FinalizeTask => null;
   end Controller;

   --    Inicializa el Driver: Inicializa la estructuras de datos y
   --    inicializa el hardware del PC. Debe ser invocada antes que
   --    cualquier otro procedimiento del driver.
   procedure Open is
   begin
      -- Inicializa las variables internas.
      theOpus:=Null;
      -- Se inicializa el hardware.
      -- Se establece el registro de control del Timer 8254.
      --    : se almacena en Dir IO $43 el c�digo $B6
      MaRTE.HAL.IO.Outb(CNTR_REG,2#10_11_011_0#);
      --    : se establece el bit 0 del registro $61.
      MaRTE.HAL.IO.Outb(HBLT_REG,
                (MaRTE.HAL.IO.Inb(HBLT_REG) or 2#0000_0001#) and
                2#1111_1101#);
      -- Instancia la tarea PlayController.
      PlayController:=New Controller;
   end Open;

   --    Libera definitivamente los recursos utilizados por el driver.
   procedure Close is
   begin
      -- Se establece el registro de control del Timer 8254.
      --    : se establece el bit 0 del registro $61.
      MaRTE.HAL.IO.Outb(HBLT_REG,
               MaRTE.HAL.IO.Inb(HBLT_REG) and 2#1111_1100#);
      -- Concluye la tarea PlayController.
      PlayController.Finalize;
   end Close;

   --    Procedimiento no bloqueante que ejecuta la melod�a que se
   --    pasa como argumento una sola vez.
   procedure Play(op: Opus_link)is
   begin
      PlayController.Cancel;
      theOpus:=op;
      PlayController.Play_one;
   end Play;

   --    Clase que representa una melodia ejecutable por el driver,
   --    as� como las operaciones que pueden ejecutarse sobre ella.
   procedure Repeat_Play(op: Opus_Link)is
   begin
      PlayController.Cancel;
      theOpus:=op;
      PlayController.Play_many;
   end Repeat_Play;

   --    Cancela la ejecuci�n de la melod�a que se est� ejecutando.
   procedure Cancel is
   begin
      PlayController.Cancel;
   end Cancel;

begin
-- Melod�as predefinidas

   --******************************************************************
   -- Christmas
   Christmas:=new Opus;
   Set_Tempo(Christmas.all,Adagio);
   Set_PercussionMode(Christmas.all,Pulsed);
   Add_Note(Christmas.all, Mi_h,  Crotchet);
   Add_Note(Christmas.all, Mi_h,  Crotchet);
   Add_Note(Christmas.all, Mi_h,  Minim);
   Add_Note(Christmas.all, Mi_h,  Crotchet);
   Add_Note(Christmas.all, Mi_h,  Crotchet);
   Add_Note(Christmas.all, Mi_h,  Minim);
   Add_Note(Christmas.all, Mi_h,  Crotchet);
   Add_Note(Christmas.all, Sol_h, Crotchet);
   Add_Note(Christmas.all, Do_h,  Crotchet);
   Add_Note(Christmas.all, Re_h,  Crotchet);
   Add_Note(Christmas.all, Mi_h,  Minim_p);
   Add_Note(Christmas.all, Fa_h,  Crotchet);
   Add_Note(Christmas.all, Fa_h,  Crotchet);
   Add_Note(Christmas.all, Fa_h,  Crotchet_p);
   Add_Note(Christmas.all, Fa_h,  Quaver);
   Add_Note(Christmas.all, Fa_h,  Crotchet);
   Add_Note(Christmas.all, Mi_h,  Crotchet);
   Add_Note(Christmas.all, Mi_h,  Crotchet_p);
   Add_Note(Christmas.all, Mi_h,  Quaver);
   Add_Note(Christmas.all, Mi_h,  Crotchet);
   Add_Note(Christmas.all, Re_h,  Crotchet);
   Add_Note(Christmas.all, Re_h,  Crotchet);
   Add_Note(Christmas.all, Mi_h,  Crotchet);
   Add_Note(Christmas.all, Re_h,  Minim_p);
   --******************************************************************
   -- Cantabria
   Cantabria:=new Opus;
   Set_Tempo(Cantabria.all,Adagio);
   Set_PercussionMode(Cantabria.all,Pulsed);
   Add_Note(Cantabria.all, La_l,  Quaver);

   Add_Note(Cantabria.all, Re_h,  Crotchet);
   Add_Note(Cantabria.all, Re_h,  Quaver);
   Add_Note(Cantabria.all, Do_h,  Quaver);
   Add_Note(Cantabria.all, Re_h,  Crotchet);
   Add_Note(Cantabria.all, Re_h,  Quaver);
   Add_Note(Cantabria.all, Do_h,  Quaver);

   Add_Note(Cantabria.all, La_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Fa_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, La_l,  Quaver);

   Add_Note(Cantabria.all, Re_h,  Crotchet);
   Add_Note(Cantabria.all, Re_h,  Quaver);
   Add_Note(Cantabria.all, Do_h,  Quaver);
   Add_Note(Cantabria.all, Re_h,  Crotchet);
   Add_Note(Cantabria.all, Re_h,  Quaver);
   Add_Note(Cantabria.all, Do_h,  Quaver);

   Add_Note(Cantabria.all, La_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, Sol_l,  Quaver);
   Add_Note(Cantabria.all, Fa_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, La_l,  Quaver);

   Add_Note(Cantabria.all, Mi_l,  Crotchet);
   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Fa_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, La_l,  Quaver);

   Add_Note(Cantabria.all, Si_l,  Quaver);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Si_l,  Quaver);
   Add_Note(Cantabria.all, La_l,  Crotchet_P);
   Add_Note(Cantabria.all, La_l,  Quaver);

   Add_Note(Cantabria.all, Re_h,  Crotchet);
   Add_Note(Cantabria.all, Re_h,  Quaver);
   Add_Note(Cantabria.all, Do_h,  Quaver);
   Add_Note(Cantabria.all, Re_h,  Crotchet);
   Add_Note(Cantabria.all, Re_h,  Quaver);
   Add_Note(Cantabria.all, Do_h,  Quaver);

   Add_Note(Cantabria.all, La_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Fa_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, La_l,  Quaver);

   Add_Note(Cantabria.all, Re_h,  Crotchet);
   Add_Note(Cantabria.all, Re_h,  Quaver);
   Add_Note(Cantabria.all, Do_h,  Quaver);
   Add_Note(Cantabria.all, Re_h,  Crotchet);
   Add_Note(Cantabria.all, Re_h,  Quaver);
   Add_Note(Cantabria.all, Do_h,  Quaver);

   Add_Note(Cantabria.all, La_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Fa_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, La_l,  Quaver);

   Add_Note(Cantabria.all, Mi_l,  Crotchet);
   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Fa_l,  Crotchet);
   Add_Note(Cantabria.all, La_l,  Quaver);
   Add_Note(Cantabria.all, La_l,  Quaver);

   Add_Note(Cantabria.all, Sol_l, Quaver);
   Add_Note(Cantabria.all, Fa_l,  Quaver);
   Add_Note(Cantabria.all, Mi_l,  Quaver);
   Add_Note(Cantabria.all, Fa_l,  Quaver);
   Add_Note(Cantabria.all, Re_l,  Minim);
   --******************************************************************
end Ada_Music;
