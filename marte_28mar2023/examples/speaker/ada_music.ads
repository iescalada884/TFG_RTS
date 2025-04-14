-----------------------------------------------------------------------
--                             Ada_Music                             --
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
with Indexed_List_Generic;
pragma Elaborate_All (Indexed_List_Generic);

package Ada_Music is

--*********************************************************************
-- Component: Ada_Music.                                              *
--                                                                    *
-- Permite interpretar una melod�a sobre el altavoz de un PC y compo- *
-- ner melod�as a partir de la informaci�n habitual de un pentagrama. *
--*********************************************************************

   -- Class Scale
   --    Notas correspondientes a dos octavas de la escala principal.
   type Scale is (pause,
              do_l,re_l,mi_l,fa_l,sol_l,la_l,si_l,
              do_h,re_h,mi_h,fa_h,sol_h,la_h,si_h,
              do_hh);

   -- Class Tempo
   --    Establece el tiempo base de metr�nomo con que se ejecuta una
   --    nota.

   type Tempo is (Largo,    Larghetto, Adagio, Andante,
                  Moderato, Allegro,   Presto, Prestissimo);


   -- Class Note_Duration
   --    N�mero o fracci�n del tiempo b�sico establecido por el tempo
   --    que una nota est� interpretandose. Corresponde a los diferen-
   --    tes s�mbolos con que se representa en el pentagrama.
   type Note_Duration is (Quaver, Crotchet, Crotchet_P, Minim,
                          Minim_P, Semibreve);

   -- Class Percussion_Mode
   --    Modo de percusi�n de la nota, esto es si las notas se suceden
   --    de forma continua (como en un organo) o si se  est� separadas
   --    por un tiempo de pausa (como en un piano)
   type Percussion_Mode is (
          Continous, -- Las notas se suceden sin interrupci�n entre
                     -- ellas como ocurren en un organo.
          Pulsed     -- Modo pulsado(como un piano). La nota suena du-
                     -- rante el 90% inicial del tiempo y queda supri-
                     -- mida (silencio) durante el �ltimo 10% de su
                     -- durecci�n.
          );

   -- Class Opus
   --    Clase que representa una melodia ejecutable por el driver, as�
   --    como las operaciones que pueden ejecutarse sobre ella.
   type Opus is private;
   type Opus_Link is access Opus;

   --    Establece la unidad b�sica de tiempo que va a ser considerada
   --    paralas nuevas notas que se introduzcan en la melod�a a partir
   --    de ese instante.
   procedure Set_Tempo(op: in out Opus; t: Tempo);

   --    Establece el modo de percusi�n con que debe interpretarse la
   --    melod�a.
   procedure Set_PercussionMode(op: in out Opus;
                         m: Percussion_Mode);

   --    A�ade una nueva nota a la melod�a (nota de la escala y dura-
   --    ci�n de la misma relativa al tempo con que se est� interpre-
   --    tando).
   procedure Add_Note(op: in out Opus;
                       n: Scale;
                       d:Note_Duration);

   --    *** Versi�n de MarteOS.                             ***
   --    *** MarteOS NO TIENE CAPACIDAD DE MANEJAR FICHEROS  ***
   --    A�ade a la melod�a op la melod�a que estaba almacenada en el
   --    fichero cuyo pathname se indica.
--   procedure Load(op: in out Opus; path: String);

   --    Almacena en un fichero la melod�a.
--   procedure Save(op: Opus; path: String);

   -- Class Music Driver
   --    Clase instanciada con la funcionalidad b�sica de Music_Driver.
   --    Permite ejecutar una melod�a sobre el altavoz de un PC, as�
   --    mismo permite codificar la medol�a a partir de su pentagrama.

   --    Inicializa el Driver: Inicializa la estructuras de datos e
   --    inicializa el hardware del PC. Debe ser invocada antes que
   --    cualquier otro procedimiento del driver.
   procedure Open;

   --    Libera los recursos utilizados por el driver.
   procedure Close;

   --    Procedimiento no bloqueante que ejecuta la melod�a que se pasa
   --    como argumento una sola vez.
   procedure Play(op: Opus_Link);

   --    Clase que representa una melodia ejecutable por el driver, as�
   --    como las operaciones que pueden ejecutarse sobre ella.
   procedure Repeat_Play(op: Opus_Link);

   --    Cancela la ejecuci�n de la melod�a que se est� ejecutando.
   procedure Cancel;

   --******************************************************************
   -- Melod�as predefinidas.
   Christmas: Opus_Link;
   Cantabria: Opus_Link;
   --******************************************************************

private
   --    Valores de frecuencia en Hz de las notas definidas en Scale.
   NoteFreq: constant array (Scale) of Float:=
             (0.0,
              262.0, 289.0, 319.0, 353.0, 389.0, 430.0, 475.0,
              524.0, 579.0, 639.0, 705.0, 779.0, 860.0, 949.0,
             1048.0);

   --    Retorna la frecuencia en Hz que corresponde a la nota.
   function Frequency(theNote: Scale) return Float;

   -- Valores de tiempo base de metr�nomo.
   TempoTime: constant array (Tempo) of Float:=
             ( 60.0/50.0,  60.0/62.0,  60.0/68.0, 60.0/100.0,
              60.0/115.0, 60.0/140.0, 60.0/180.0, 60.0/204.0);

   --   Retorna la duraci�n en segundos que corresponde al tiempo base.
   function Basic_Time(t:Tempo) return Float;

   --    Valores que corresponden a los tipos de notas.
   TimeDuration: constant array (Note_Duration) of Float:=
             ( 0.5,  1.0,  1.5, 2.0,3.0, 4.0);

   --    Retorna el factor por el que hay que multiplicar el tiempo
   --    base para obtener la duraci�n de la nota.
   function Times_Tempo(d: Note_Duration) return Float;

   task type Controller is
      entry Play_One;
      entry Play_Many;
      entry Cancel;
      entry Finalize;
   end Controller;
   type ControllerLink is access Controller;

   playController: ControllerLink;

   --    Variable tipo access que apunta a la melod�a que actualmente
   --    est� en ejecuci�n.
   theOpus: Opus_Link;

   --    N�mero o fracci�n del tiempo b�sico establecido por el tempo
   --    que una nota est� interpretandose.
   type Note is record
      theNote: Scale;                -- Nota que se interpreta.
      theDuration: Note_Duration;    -- Duraci�n de la nota.
      theTempo: Tempo;               -- Tempo en que fu� pulsada.
   end record;


   package CompassList is new Indexed_List_Generic (Element => Note,
                                                         "=" => "=");

   --    Clase que representa una melodia ejecutable por el driver,
   --    as� como las operaciones que pueden ejecutarse sobre ella.
   type Opus is record
         -- Tempo que debe utilizarse para establecer la duraci�n de
         -- las nuevas notas que se introduzcan en la melod�a.
      currentTempo: Tempo:= Adagio;
         -- Modo de percusi�n con que se interpreta la melod�a.
      currentPercussionMode: Percussion_Mode:= Pulsed;
         --  Cola de notas que constituyen la melod�a.
      compass: CompassList.List;
   end record;

end Ada_Music;

