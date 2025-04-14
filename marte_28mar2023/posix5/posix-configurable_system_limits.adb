------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--      'P O S I X . C o n f i g u r a b l e _ S y s t e m _ L i m i t s'
--
--                                  Body
--
--
--  File 'posix-configurable_system_limits.adb'                        By MAR.
--
--
--  Package 'POSIX_Configurable_System_Limits' as defined in IEEE Std
--  1003.5b-1996.
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
with POSIX, POSIX_Limits, POSIX_Options;

package body POSIX.Configurable_System_Limits is

   package PO renames POSIX_Options;
   package PL renames POSIX_Limits;

   -- Get configurable system options
   function Asynchronous_IO_Is_Supported
     return PO.Asynchronous_IO_Support is
   begin
      return PO.Asynchronous_IO_Support'First;
   end Asynchronous_IO_Is_Supported;

   function File_Synchronization_Is_Supported
     return PO.File_Synchronization_Support is
   begin
      return PO.File_Synchronization_Support'First;
   end File_Synchronization_Is_Supported;

   function Job_Control_Supported
     return Job_Control_Support is
   begin
      return Job_Control_Support'First;
   end Job_Control_Supported;

   function Memory_Mapped_Files_Are_Supported
     return PO.Memory_Mapped_Files_Support is
   begin
      return PO.Memory_Mapped_Files_Support'First;
   end Memory_Mapped_Files_Are_Supported;

   function Memory_Locking_Is_Supported
     return PO.Memory_Locking_Support is
   begin
      return PO.Memory_Locking_Support'First;
   end Memory_Locking_Is_Supported;

   function Memory_Range_Locking_Is_Supported
     return PO.Memory_Range_Locking_Support is
   begin
      return PO.Memory_Range_Locking_Support'First;
   end Memory_Range_Locking_Is_Supported;

   function Memory_Protection_Is_Supported
      return PO.Memory_Protection_Support is
   begin
      return PO.Memory_Protection_Support'First;
   end Memory_Protection_Is_Supported;

   function Message_Queues_Are_Supported
      return PO.Message_Queues_Support is
   begin
      return PO.Message_Queues_Support'First;
   end Message_Queues_Are_Supported;

   function Mutex_Priority_Ceiling_Is_Supported
      return PO.Mutex_Priority_Ceiling_Support is
   begin
      return PO.Mutex_Priority_Ceiling_Support'First;
   end Mutex_Priority_Ceiling_Is_Supported;

   function Mutex_Priority_Inheritance_Is_Supported
      return PO.Mutex_Priority_Inheritance_Support is
   begin
      return PO.Mutex_Priority_Inheritance_Support'First;
   end Mutex_Priority_Inheritance_Is_Supported;

   function Mutexes_Are_Supported
      return PO.Mutexes_Support is
   begin
      return PO.Mutexes_Support'First;
   end Mutexes_Are_Supported;

   function Prioritized_IO_Is_Supported
      return PO.Prioritized_IO_Support is
   begin
      return PO.Prioritized_IO_Support'First;
   end Prioritized_IO_Is_Supported;

   function Priority_Process_Scheduling_Is_Supported
      return PO.Priority_Process_Scheduling_Support is
   begin
      return PO.Priority_Process_Scheduling_Support'First;
   end Priority_Process_Scheduling_Is_Supported;

   function Priority_Task_Scheduling_Is_Supported
      return PO.Priority_Task_Scheduling_Support is
   begin
      return PO.Priority_Task_Scheduling_Support'First;
   end Priority_Task_Scheduling_Is_Supported;

   function Realtime_Signals_Are_Supported
      return PO.Realtime_Signals_Support is
   begin
      return PO.Realtime_Signals_Support'First;
   end Realtime_Signals_Are_Supported;

   function Saved_IDs_Supported
      return PO.Saved_IDs_Support is
   begin
      return PO.Saved_IDs_Support'First;
   end Saved_IDs_Supported;

   function Semaphores_Are_Supported
      return PO.Semaphores_Support is
   begin
      return PO.Semaphores_Support'First;
   end Semaphores_Are_Supported;

   function Shared_Memory_Objects_Are_Supported
      return PO.Shared_Memory_Objects_Support is
   begin
      return PO.Shared_Memory_Objects_Support'First;
   end Shared_Memory_Objects_Are_Supported;

   function Process_Shared_Is_Supported
      return PO.Process_Shared_Support is
   begin
      return PO.Process_Shared_Support'First;
   end Process_Shared_Is_Supported;

   function Synchronized_IO_Is_Supported
      return PO.Synchronized_IO_Support is
   begin
      return PO.Synchronized_IO_Support'First;
   end Synchronized_IO_Is_Supported;

   function Timers_Are_Supported
      return PO.Timers_Support is
   begin
      return PO.Timers_Support'First;
   end Timers_Are_Supported;

   function System_POSIX_Version
      return POSIX_Version is
   begin
      return POSIX.POSIX_Version;
   end System_POSIX_Version;

   function System_POSIX_Ada_Version
      return POSIX_Version is
   begin
      return POSIX_Ada_Version;
   end System_POSIX_Ada_Version;

   -- Get configurable system limits
   function Argument_List_Maximum
     return PL.Argument_List_Maxima is
   begin
      return PL.Argument_List_Maxima'First;
   end Argument_List_Maximum;

   function Asynchronous_IO_Maximum
      return PL.Asynchronous_IO_Maxima is
   begin
      return PL.Asynchronous_IO_Maxima'First;
   end Asynchronous_IO_Maximum;

   function Asynchronous_IO_Priority_Delta_Maximum
      return PL.Asynchronous_IO_Priority_Delta_Maxima is
   begin
      return PL.Asynchronous_IO_Priority_Delta_Maxima'First;
   end Asynchronous_IO_Priority_Delta_Maximum;

   function Child_Processes_Maximum
      return PL.Child_Processes_Maxima is
   begin
      return PL.Child_Processes_Maxima'First;
   end Child_Processes_Maximum;

   function Groups_Maximum
      return PL.Groups_Maxima is
   begin
      return PL.Groups_Maxima'First;
   end Groups_Maximum;

   function List_IO_Maximum
      return PL.List_IO_Maxima is
   begin
      return PL.List_IO_Maxima'First;
   end List_IO_Maximum;

   function Open_Message_Queues_Maximum
      return PL.Open_Message_Queues_Maxima is
   begin
      return PL.Open_Message_Queues_Maxima'First;
   end Open_Message_Queues_Maximum;

   function Message_Priority_Maximum
      return PL.Message_Priority_Maxima is
   begin
      return PL.Message_Priority_Maxima'First;
   end Message_Priority_Maximum;

   function Open_Files_Maximum
      return PL.Open_Files_Maxima is
   begin
      return PL.Open_Files_Maxima'First;
   end Open_Files_Maximum;

   function Page_Size
      return PL.Page_Size_Range is
   begin
      return PL.Page_Size_Range'First;
   end Page_Size;

   function Queued_Signals_Maximum
      return PL.Queued_Signals_Maxima is
   begin
      return PL.Queued_Signals_Maxima'First;
   end Queued_Signals_Maximum;

   function Realtime_Signals_Maximum
      return PL.Realtime_Signals_Maxima is
   begin
      return PL.Realtime_Signals_Maxima'First;
   end Realtime_Signals_Maximum;

   function Semaphores_Maximum
      return PL.Semaphores_Maxima is
   begin
      return PL.Semaphores_Maxima'First;
   end Semaphores_Maximum;

   function Semaphores_Value_Maximum
      return PL.Semaphores_Value_Maxima is
   begin
      return PL.Semaphores_Value_Maxima'First;
   end Semaphores_Value_Maximum;

   function Stream_Maximum
      return PL.Streams_Maxima is
   begin
      return PL.Streams_Maxima'First;
   end Stream_Maximum;

   function Timers_Maximum
     return PL.Timers_Maxima is
   begin
      return PL.Timers_Maxima'First;
   end Timers_Maximum;

   function Timer_Overruns_Maximum
     return PL.Timer_Overruns_Maxima is
   begin
      return PL.Timer_Overruns_Maxima'First;
   end Timer_Overruns_Maximum;

   function Time_Zone_String_Maximum
     return PL.Time_Zone_String_Maxima is
   begin
      return PL.Time_Zone_String_Maxima'First;
   end Time_Zone_String_Maximum;

end POSIX.Configurable_System_Limits;
