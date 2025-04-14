------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                       'P O S I X . O p t i o n s'
--
--                                  Spec
--
--
--  File 'posix-options.ads'                                           By MAR.
--
--
--  Package 'POSIX_Options' as defined in IEEE Std 1003.5b-1996.
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
package POSIX.Options is

   subtype Asynchronous_IO_Support is Boolean range
     False .. False;

   subtype Change_Owner_Restriction is
     POSIX.Change_Owner_Restriction;

   subtype Filename_Truncation is
     POSIX.Filename_Truncation;

   subtype File_Synchronization_Support is Boolean range
     True .. True;

   subtype Job_Control_Support is
     POSIX.Job_Control_Support;

   subtype Memory_Mapped_Files_Support is Boolean range
     False .. False;

   subtype Memory_Locking_Support is Boolean range
     True .. True;

   subtype Memory_Range_Locking_Support is Boolean range
     True .. True;

   subtype Memory_Protection_Support is Boolean range
     False .. False;

   subtype Message_Queues_Support is Boolean range
     False .. False;   -- Non implemented in MaRTe OS

   subtype Mutex_Priority_Ceiling_Support is Boolean range
     True .. True;

   subtype Mutex_Priority_Inheritance_Support is Boolean range
     True .. True;

   subtype Mutexes_Support is Boolean range
     True .. True;

   subtype Prioritized_IO_Support is Boolean range
     False .. False;

   subtype Priority_Process_Scheduling_Support is Boolean range
     False .. False;

   subtype Priority_Task_Scheduling_Support is Boolean range
     True .. True;

   subtype Process_Shared_Support is Boolean range
     True .. True;

   subtype Realtime_Signals_Support is Boolean range
     True .. True;

   subtype Saved_IDs_Support is
     POSIX.Saved_IDs_Support;

   subtype Semaphores_Support is Boolean range
     False .. False;  -- Non implemented in MaRTe OS (Yet)

   subtype Shared_Memory_Objects_Support is Boolean range
     True .. True;

   subtype Signal_Entries_Support is Boolean range
     False .. False;

   subtype Synchronized_IO_Support is Boolean range
     True .. True;

   subtype Timers_Support is Boolean range
     True .. True;

end POSIX.Options;
