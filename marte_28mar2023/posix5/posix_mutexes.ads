------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                        'P O S I X _ M u t e x e s'
--
--                                  Spec
--
--
--
--  File 'posix_mutexes.ads'                                           By MAR.
--
--
--  Implementation of the package 'Posix_Mutexes' as defined in IEEE
--  Std 1003.5b-1996 (POSIX Ada bindings).
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
with MaRTE.Kernel.Mutexes;
use MaRTE.Kernel;
with Ada.Task_Identification;

package POSIX_Mutexes is

   --  ==========  --
   --   WARNINGS   --
   --  ==========  --

   --  This package is for mixed-language programming, in which
   --  an Ada task needs to synchronize with a C thread.

   --  Do NOT use POSIX mutexes to synchronize between Ada tasks.
   --  Instead, use Ada protected objects.
   --  Protected objects are implemented using mutexes.
   --  The difference is that they are safer.
   --  In particular protected operations are abort-deferred,
   --  and have cleanup code to ensure mutexes are always released,
   --  even if a protected operation completes abnormally due to an exception.
   --  If you use one of these "raw" mutexes, you risk undefined
   --  behavior if you violate any of the POSIX.1c rules about mutexes,
   --  or if you attempt to abort (including ATC) a task that is performing
   --  a mutex or CV operation.

   subtype Mutex is MaRTE.Kernel.Mutexes.Mutex;

   subtype Mutex_Descriptor is MaRTE.Kernel.Mutexes.Mutex_Descriptor;

   subtype Attributes is MaRTE.Kernel.Mutexes.Attributes;

   subtype AppSched_Parameters is MaRTE.Kernel.Mutexes.Mutex_AppSched_Param_T;

   procedure Initialize (Attr : in out Attributes);

   procedure Finalize (Attr : in out Attributes);

   function Get_Process_Shared (Attr : Attributes)
                                return Boolean;

   procedure Set_Process_Shared
     (Attr      : in out Attributes;
      Is_Shared : in Boolean := False);

   subtype Ceiling_Priority is Mutexes.Ceiling_Priority;

   subtype Locking_Policy   is Mutexes.Locking_Policy;

   NO_PRIORITY_INHERITANCE  : constant Locking_Policy :=
     Mutexes.NO_PRIORITY_INHERITANCE;
   HIGHEST_BLOCKED_TASK     : constant Locking_Policy :=
     Mutexes.HIGHEST_BLOCKED_TASK;
   HIGHEST_CEILING_PRIORITY : constant Locking_Policy :=
     Mutexes.HIGHEST_CEILING_PRIORITY;
   APPSCHED_PROTOCOL        : constant Locking_Policy :=
     Mutexes.APPSCHED_PROTOCOL;

   procedure Set_Locking_Policy
     (Attr    : in out Attributes;
      Locking : in Locking_Policy);
   function Get_Locking_Policy
     (Attr : Attributes)
      return Locking_Policy;

   procedure Set_Ceiling_Priority
     (Attr        : in out Attributes;
      New_Ceiling : in     Ceiling_Priority);
   function Get_Ceiling_Priority (Attr : Attributes)
     return Ceiling_Priority;

   procedure Set_App_Scheduler
     (Attr      : in out Attributes;
      Scheduler : in     Ada.Task_Identification.Task_Id);
   function Get_App_Scheduler (Attr : in Attributes)
                               return Ada.Task_Identification.Task_Id;

   procedure Initialize (M    : in out Mutex;
                         Attr : in     Attributes);

   procedure Initialize (M : in out Mutex);

   function Descriptor_Of (M : Mutex) return Mutex_Descriptor;

   procedure Finalize (M : in out Mutex);

   procedure Set_Ceiling_Priority
     (M           : in  Mutex_Descriptor;
      New_Ceiling : in  Ceiling_Priority;
      Old_Ceiling : out Ceiling_Priority);
   function Get_Ceiling_Priority (M : Mutex_Descriptor)
                                  return Ceiling_Priority;

   function Get_App_Scheduler (M : in Mutex_Descriptor)
                               return Ada.Task_Identification.Task_Id;

   procedure Lock (M : in Mutex_Descriptor);

   function Try_Lock (M : Mutex_Descriptor) return Boolean;

   procedure Unlock (M : in Mutex_Descriptor);

   generic
      type Parameters is private;
   package Application_Defined_Parameters is
      procedure Set_Parameters (Attr  : in out Attributes;
                                Param : in     Parameters);
      procedure Get_Parameters (Attr  : in  Attributes;
                                Param : out Parameters);

      procedure Set_Parameters (M     : in Mutex_Descriptor;
                                Param : in Parameters);
      procedure Get_Parameters (M     : in  Mutex_Descriptor;
                                Param : out Parameters);
   end Application_Defined_Parameters;


   generic
      type Attribute is private;
   package Mutex_Attributes is
      type Attribute_Handle is access all Attribute;

      function Get (M : Mutex_Descriptor) return Attribute_Handle;

      procedure Set (Val : in Attribute_Handle; M : in Mutex_Descriptor);
   end Mutex_Attributes;

end POSIX_Mutexes;
