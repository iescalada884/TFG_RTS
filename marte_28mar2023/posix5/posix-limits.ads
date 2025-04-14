------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                        'P O S I X . L i m i t s'
--
--                                  Spec
--
--
--  File 'posix-limits.ads'                                            By MAR.
--
--
--  Package 'POSIX_Limits' as defined in IEEE Std 1003.5b-1996.
--
--  MaRTe OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------
package POSIX.Limits is

   ------------------------------
   --  Portable System Limits  --
   ------------------------------

   Portable_Argument_List_Maximum : Natural
      renames POSIX.Portable_Argument_List_Maximum;
   Portable_Asynchronous_IO_Maximum :
      constant Natural := 1;
   Portable_Child_Processes_Maximum : Natural
      renames POSIX.Portable_Child_Processes_Maximum;
   Portable_Clock_Resolution_Minimum :
      constant := 20_000_000;
   Portable_Filename_Maximum : Natural
      renames POSIX.Portable_Filename_Limit_Maximum;
   Portable_Groups_Maximum : Natural
      renames POSIX.Portable_Groups_Maximum;
   Portable_Input_Line_Maximum : POSIX.IO_Count
      renames POSIX.Portable_Input_Line_Limit_Maximum;
   Portable_Input_Queue_Maximum : POSIX.IO_Count
      renames POSIX.Portable_Input_Queue_Limit_Maximum;
   Portable_Links_Maximum : Natural
      renames POSIX.Portable_Link_Limit_Maximum;
   Portable_List_IO_Maximum :
      constant Natural := 2;
   Portable_Message_Priority_Maximum :
      constant Natural := 32;
   Portable_Open_Files_Maximum : Natural
      renames POSIX.Portable_Open_Files_Maximum;
   Portable_Open_Message_Queues_Maximum :
      constant Natural := 8;
   Portable_Pathname_Maximum : Natural
      renames POSIX.Portable_Pathname_Limit_Maximum;
   Portable_Pipe_Length_Maximum : POSIX.IO_Count
      renames POSIX.Portable_Pipe_Limit_Maximum;
   Portable_Queued_Signals_Maximum :
      constant Natural := 32;
   Portable_Realtime_Signals_Maximum :
      constant Natural := 8;
   Portable_Semaphores_Maximum :
      constant Natural := 256;
   Portable_Semaphores_Value_Maximum :
      constant Natural := 32767;
   Portable_Streams_Maximum : Natural
      renames POSIX.Portable_Stream_Maximum;
   Portable_Timer_Overruns_Maximum :
      constant Natural := 32;
   Portable_Timers_Maximum :
      constant Natural := 32;
   Portable_Time_Zone_String_Maximum : Natural
      renames POSIX.Portable_Time_Zone_String_Maximum;

   ---------------------------
   --  Configurable Limits  --
   ---------------------------

   subtype Argument_List_Maxima is
      POSIX.Argument_List_Maxima;
   subtype Asynchronous_IO_Maxima is Natural range
      1 .. Natural'Last;
   subtype Asynchronous_IO_Priority_Delta_Maxima is Natural range
      0 .. Natural'Last;
   subtype Child_Processes_Maxima is
      POSIX.Child_Processes_Maxima;
   subtype Filename_Maxima is
      POSIX.Filename_Limit_Maxima;
   subtype Groups_Maxima is
      POSIX.Groups_Maxima;
   subtype Input_Line_Maxima is
      POSIX.Input_Line_Limit_Maxima;
   subtype Input_Queue_Maxima is
      POSIX.Input_Queue_Limit_Maxima;
   subtype Links_Maxima is
      POSIX.Link_Limit_Maxima;
   subtype List_IO_Maxima is Natural range
      2 .. Natural'Last;
   subtype Message_Priority_Maxima is Natural range
      32 .. Natural'Last;
   subtype Open_Message_Queues_Maxima is Natural range
      8 .. Natural'Last;
   subtype Open_Files_Maxima is
      POSIX.Open_Files_Maxima;
   subtype Page_Size_Range  is Natural range 4096 .. 4096;
   subtype Pathname_Maxima is
      POSIX.Pathname_Limit_Maxima;
   subtype Pipe_Length_Maxima is
      POSIX.Pipe_Limit_Maxima;
   subtype Queued_Signals_Maxima is Natural range
      32 .. Natural'Last;
   subtype Realtime_Signals_Maxima is Natural range
      8 .. Natural'Last;
   subtype Semaphores_Maxima is Natural range
      256 .. Natural'Last;
   subtype Semaphores_Value_Maxima is Natural range
      2147483647 .. 2147483647;
   subtype Streams_Maxima is
      POSIX.Stream_Maxima;
   subtype Timer_Overruns_Maxima is Natural range
      32 .. Natural'Last;
   subtype Timers_Maxima is Natural range
      32 .. Natural'Last;
   subtype Time_Zone_String_Maxima is
      POSIX.Time_Zone_String_Maxima;

--    subtype File_Descriptor_Set_Maxima is Natural range
--       1024 .. 1024;
--    subtype Socket_Buffer_Maxima is IO_Count range
--       512 .. IO_Count'Last;
--    subtype Socket_IO_Vector_Maxima is Natural range
--       1024 .. 1024;
--    subtype Socket_Connection_Maxima is Natural range
--       1 .. Natural'Last;
--    subtype XTI_IO_Vector_Maxima is Natural range
--       16 .. Natural'Last;
end POSIX.Limits;
