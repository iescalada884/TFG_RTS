------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--      'P O S I X . C o n f i g u r a b l e _ S y s t e m _ L i m i t s'
--
--                                  Spec
--
--
--  File 'posix-configurable_system_limits.ads'                        By MAR.
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

package POSIX.Configurable_System_Limits is

   function Asynchronous_IO_Is_Supported
     return POSIX_Options.Asynchronous_IO_Support;
   function File_Synchronization_Is_Supported
     return POSIX_Options.File_Synchronization_Support;
   function Job_Control_Supported                          -- obsolescent
     return POSIX.Job_Control_Support;                     -- obsolescent
   function Job_Control_Is_Supported
     return POSIX_Options.Job_Control_Support
     renames Job_Control_Supported;
   function Memory_Mapped_Files_Are_Supported
     return POSIX_Options.Memory_Mapped_Files_Support;
   function Memory_Locking_Is_Supported
     return POSIX_Options.Memory_Locking_Support;
   function Memory_Range_Locking_Is_Supported
     return POSIX_Options.Memory_Range_Locking_Support;
   function Memory_Protection_Is_Supported
     return POSIX_Options.Memory_Protection_Support;
   function Message_Queues_Are_Supported
     return POSIX_Options.Message_Queues_Support;
   function Mutex_Priority_Ceiling_Is_Supported
     return POSIX_Options.Mutex_Priority_Ceiling_Support;
   function Mutex_Priority_Inheritance_Is_Supported
     return POSIX_Options.Mutex_Priority_Inheritance_Support;
   function Mutexes_Are_Supported
     return POSIX_Options.Mutexes_Support;
   function Prioritized_IO_Is_Supported
     return POSIX_Options.Prioritized_IO_Support;
   function Process_Shared_Is_Supported
     return POSIX_Options.Process_Shared_Support;
   function Priority_Process_Scheduling_Is_Supported
     return POSIX_Options.Priority_Process_Scheduling_Support;
   function Priority_Task_Scheduling_Is_Supported
     return POSIX_Options.Priority_Task_Scheduling_Support;
   function Realtime_Signals_Are_Supported
     return POSIX_Options.Realtime_Signals_Support;
   function Saved_IDs_Supported                        --  obsolescent
    return POSIX.Saved_IDs_Support;                    --  obsolescent
   function Saved_IDs_Are_Supported
     return POSIX_Options.Saved_IDs_Support
     renames Saved_IDs_Supported;
   function Semaphores_Are_Supported
     return POSIX_Options.Semaphores_Support;
   function Shared_Memory_Objects_Are_Supported
     return POSIX_Options.Shared_Memory_Objects_Support;
   function Synchronized_IO_Is_Supported
     return POSIX_Options.Synchronized_IO_Support;
   function Timers_Are_Supported
     return POSIX_Options.Timers_Support;

   type POSIX_Version is new Integer;
   function System_POSIX_Version
     return POSIX_Version;
   function System_POSIX_Ada_Version
     return POSIX_Version;
   function Argument_List_Maximum
     return POSIX_Limits.Argument_List_Maxima;
   function Asynchronous_IO_Maximum
     return POSIX_Limits.Asynchronous_IO_Maxima;
   function Asynchronous_IO_Priority_Delta_Maximum
     return POSIX_Limits.Asynchronous_IO_Priority_Delta_Maxima;
   function Child_Processes_Maximum
     return POSIX_Limits.Child_Processes_Maxima;
   function Groups_Maximum
     return POSIX_Limits.Groups_Maxima;
   function List_IO_Maximum
     return POSIX_Limits.List_IO_Maxima;
   function Open_Message_Queues_Maximum
     return POSIX_Limits.Open_Message_Queues_Maxima;
   function Message_Priority_Maximum
     return POSIX_Limits.Message_Priority_Maxima;
   function Open_Files_Maximum
     return POSIX_Limits.Open_Files_Maxima;
   function Page_Size
     return POSIX_Limits.Page_Size_Range;
   function Queued_Signals_Maximum
     return POSIX_Limits.Queued_Signals_Maxima;
   function Realtime_Signals_Maximum
     return POSIX_Limits.Realtime_Signals_Maxima;
   function Semaphores_Maximum
     return POSIX_Limits.Semaphores_Maxima;
   function Semaphores_Value_Maximum
     return POSIX_Limits.Semaphores_Value_Maxima;
   function Stream_Maximum                            -- obsolescent
     return POSIX.Stream_Maxima;                      -- obsolescent
   function Streams_Maximum
     return POSIX_Limits.Streams_Maxima
     renames Stream_Maximum;
   function Timers_Maximum
     return POSIX_Limits.Timers_Maxima;
   function Timer_Overruns_Maximum
     return POSIX_Limits.Timer_Overruns_Maxima;
   function Time_Zone_String_Maximum
     return POSIX_Limits.Time_Zone_String_Maxima;

   --  POSIX.5c/D4 extensions

--    function Internet_Datagram_Is_Supported
--      return POSIX_Options.Internet_Datagram_Support;
--    function Internet_Protocol_Is_Supported
--      return POSIX_Options.Internet_Protocol_Support;
--    function Internet_Stream_Is_Supported
--      return POSIX_Options.Internet_Stream_Support;
--    function ISO_OSI_Protocol_Is_Supported
--      return POSIX_Options.ISO_OSI_Protocol_Support;
--    function Network_Management_Is_Supported
--      return POSIX_Options.Network_Management_Support;
--    function OSI_Connectionless_Is_Supported
--      return POSIX_Options.OSI_Connectionless_Support;
--    function OSI_Connection_Is_Supported
--      return POSIX_Options.OSI_Connection_Support;
--    function OSI_Minimal_Is_Supported
--      return POSIX_Options.OSI_Minimal_Support;
--    function Poll_Is_Supported
--      return POSIX_Options.Poll_Support;
--    function Select_Is_Supported
--      return POSIX_Options.Select_Support;
--    function Sockets_DNI_Is_Supported
--      return POSIX_Options.Sockets_DNI_Support;
--    function Socket_IO_Vector_Maximum
--      return POSIX_Limits.Socket_IO_Vector_Maxima;
--    function XTI_DNI_Is_Supported
--      return POSIX_Options.XTI_DNI_Support;

end POSIX.Configurable_System_Limits;
