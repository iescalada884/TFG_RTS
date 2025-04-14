------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                        'P O S I X _ M u t e x e s'
--
--                                  Body
--
--
--
--  File 'posix_mutexes.adb'                                           By MAR.
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
with Ada.Unchecked_Conversion;
with System;

with POSIX;
with POSIX.Implementation;

with MaRTE.Kernel.Mutexes.Internals;
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package body POSIX_Mutexes is

   package PI renames POSIX.Implementation;
   package TOI renames MaRTE.Kernel.Tasks_Operations.Internals;

   -----------------------
   -- Initialize (Attr) --
   -----------------------
   procedure Initialize (Attr : in out Attributes) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutexattr_Init (Attr'Unrestricted_Access));
   end Initialize;

   ---------------------
   -- Finalize (Attr) --
   ---------------------
   procedure Finalize (Attr : in out Attributes) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutexattr_Destroy (Attr'Unrestricted_Access));
   end Finalize;

   ------------------------
   -- Get_Process_Shared --
   ------------------------
   function Get_Process_Shared (Attr : Attributes) return Boolean is
   begin
      return True;
   end Get_Process_Shared;

   ------------------------
   -- Set_Process_Shared --
   ------------------------
   procedure Set_Process_Shared (Attr      : in out Attributes;
                                 Is_Shared : in Boolean := False) is
   begin
      null;
   end Set_Process_Shared;

   ------------------------
   -- Set_Locking_Policy --
   ------------------------
   procedure Set_Locking_Policy (Attr    : in out Attributes;
                                 Locking : in Locking_Policy) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutexattr_Setprotocol (Attr'Unrestricted_Access,
                                                Locking));
   end Set_Locking_Policy;

   ------------------------
   -- Get_Locking_Policy --
   ------------------------
   function Get_Locking_Policy (Attr : Attributes) return Locking_Policy is
      Policy : aliased MaRTE.Integer_Types.Int;
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutexattr_Getprotocol (Attr'Unrestricted_Access,
                                                Policy'Access));
      return Locking_Policy (Policy);
   end Get_Locking_Policy;

   --------------------------
   -- Set_Ceiling_Priority --
   --------------------------
   procedure Set_Ceiling_Priority (Attr        : in out Attributes;
                                   New_Ceiling : in Ceiling_Priority) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutexattr_Setprioceiling (Attr'Unrestricted_Access,
                                                   New_Ceiling));
   end Set_Ceiling_Priority;

   --------------------------
   -- Get_Ceiling_Priority --
   --------------------------
   function Get_Ceiling_Priority (Attr : Attributes)
                                  return Ceiling_Priority is
      Ceiling : aliased MaRTE.Integer_Types.Int;
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutexattr_Getprioceiling (Attr'Unrestricted_Access,
                                                   Ceiling'Access));
      return Ceiling_Priority (Ceiling);
   end Get_Ceiling_Priority;

   -----------------------
   -- Set_App_Scheduler --
   -----------------------
   procedure Set_App_Scheduler
     (Attr      : in out Attributes;
      Scheduler : in     Ada.Task_Identification.Task_Id) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutexattr_Setappscheduler
         (Attr'Unrestricted_Access, PI.To_Kernel_Task_Id (Scheduler)));
   end Set_App_Scheduler;

   -----------------------
   -- Get_App_Scheduler --
   -----------------------
   function Get_App_Scheduler (Attr : in Attributes)
                               return Ada.Task_Identification.Task_Id is
      Scheduler : aliased MaRTE.Kernel.Task_Id;
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutexattr_Getappscheduler (Attr'Unrestricted_Access,
                                                    Scheduler'Access));
      return PI.To_Ada_Task_Id (Scheduler);
   end Get_App_Scheduler;

   ------------------------
   -- Initialize (Mutex) --
   ------------------------
   procedure Initialize (M    : in out Mutex;
                         Attr : in Attributes) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutex_Init (M'Unrestricted_Access,
                                     Attr'Unrestricted_Access));
   end Initialize;

   ------------------------
   -- Initialize (Mutex) --
   ------------------------
   procedure Initialize (M : in out Mutex) is
      Attr : aliased Mutexes.Attributes := Mutexes.Attr_Default;
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutex_Init (M'Unrestricted_Access,
                                     Attr'Access));
   end Initialize;

   -------------------
   -- Descriptor_Of --
   -------------------
   function Descriptor_Of (M : Mutex) return Mutex_Descriptor is
      use type Mutex_Descriptor;
      Md : Mutex_Descriptor;
   begin
      Md := Mutexes.Descriptor_Of (M);

      if Md = null then
         PI.Raise_POSIX_Error (Invalid_Argument);
      end if;

      return Md;
   end Descriptor_Of;

   --------------
   -- Finalize --
   --------------
   procedure Finalize (M : in out Mutex) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutex_Destroy (M'Unrestricted_Access));
   end Finalize;

   --------------------------
   -- Set_Ceiling_Priority --
   --------------------------
   procedure Set_Ceiling_Priority (M           : in Mutex_Descriptor;
                                   New_Ceiling : in Ceiling_Priority;
                                   Old_Ceiling : out Ceiling_Priority) is
      Result : aliased Int;
   begin
      PI.Check_NZ (Mutexes.Pthread_Mutex_Setprioceiling
        (M, Int (New_Ceiling), Result'Unchecked_Access));
      Old_Ceiling := Ceiling_Priority (Result);
   end Set_Ceiling_Priority;

   --------------------------
   -- Get_Ceiling_Priority --
   --------------------------
   function Get_Ceiling_Priority (M : Mutex_Descriptor)
                                 return Ceiling_Priority is
      Result : aliased Int;
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutex_Getprioceiling (M, Result'Unchecked_Access));
      return Ceiling_Priority (Result);
   end Get_Ceiling_Priority;

   -----------------------
   -- Get_App_Scheduler --
   -----------------------
   function Get_App_Scheduler (M : in Mutex_Descriptor)
                               return Ada.Task_Identification.Task_Id is
      Scheduler : aliased Task_Id;
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutex_Getappscheduler (M, Scheduler'Access));
      return PI.To_Ada_Task_Id (Scheduler);
   end Get_App_Scheduler;

   ----------
   -- Lock --
   ----------
   procedure Lock (M : in Mutex_Descriptor) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutex_Lock (M));
   end Lock;

   --------------
   -- Try_Lock --
   --------------
   function Try_Lock (M : Mutex_Descriptor) return Boolean is
      Ret : MaRTE.Integer_Types.Int;
   begin
      Ret := Mutexes.Pthread_Mutex_Trylock (M);
      case Ret is
         when 0 =>
           null;
         when Resource_Busy =>
           return False;
         when others =>
           PI.Raise_POSIX_Error (Error_Code (Ret));
      end case;
      return True;
   end Try_Lock;

   ------------
   -- Unlock --
   ------------
   procedure Unlock (M : in Mutex_Descriptor) is
   begin
      PI.Check_NZ
        (Mutexes.Pthread_Mutex_Unlock (M));
   end Unlock;

   ------------------------------------
   -- Application_Defined_Parameters --
   ------------------------------------
   package body Application_Defined_Parameters is

      function To_Mutex_AppSched_Param_Ac is
        new Ada.Unchecked_Conversion (System.Address,
                                      Mutexes.Mutex_AppSched_Param_Ac);

      function To_Mutex_AppSched_Param_Size_T is
        new Ada.Unchecked_Conversion (Integer,
                                      Mutexes.Mutex_AppSched_Param_Size_T);

      type Parameters_Ac is access all Parameters;
      function To_Mutex_AppSched_Param_Ac is
        new Ada.Unchecked_Conversion (Parameters_Ac,
                                      Mutexes.Mutex_AppSched_Param_Ac);

      --------------------
      -- Set_Parameters --
      --------------------
      procedure Set_Parameters (Attr  : in out  Attributes;
                                Param : in      Parameters) is
      begin
         PI.Check_NZ
           (Mutexes.Pthread_Mutexattr_SetappSchedparam
                       (Attr'Unrestricted_Access,
                        To_Mutex_AppSched_Param_Ac (Param'Unrestricted_Access),
                        To_Mutex_AppSched_Param_Size_T (Param'Size / 8)));
      end Set_Parameters;

      --------------------
      -- Get_Parameters --
      --------------------
      procedure Get_Parameters (Attr  : in  Attributes;
                                Param : out Parameters) is
         Kernel_Param      : aliased Mutexes.Mutex_AppSched_Param_T;
         Kernel_Param_Size : aliased Mutexes.Mutex_AppSched_Param_Size_T;
         use type Mutexes.Mutex_AppSched_Param_Size_T;
      begin
         PI.Check_NZ (Mutexes.Pthread_Mutexattr_GetappSchedparam
                                                (Attr'Unrestricted_Access,
                                                 Kernel_Param'Unchecked_Access,
                                                 Kernel_Param_Size'Access));
         if Integer (Kernel_Param_Size) /= Parameters'Size / 8 then
            PI.Raise_POSIX_Error (POSIX.Invalid_Argument);
         end if;

         To_Mutex_AppSched_Param_Ac (Param'Address) (0 .. Kernel_Param_Size) :=
           Kernel_Param (0 .. Kernel_Param_Size);
      end Get_Parameters;

      --------------------
      -- Set_Parameters --
      --------------------
      procedure Set_Parameters (M     : in Mutex_Descriptor;
                                Param : in Parameters) is
      begin
         PI.Check_NZ
           (Mutexes.Pthread_Mutex_SetappSchedparam
                       (M,
                        To_Mutex_AppSched_Param_Ac (Param'Unrestricted_Access),
                        To_Mutex_AppSched_Param_Size_T (Param'Size / 8)));
      end Set_Parameters;

      --------------------
      -- Get_Parameters --
      --------------------
      procedure Get_Parameters (M     : in  Mutex_Descriptor;
                                Param : out Parameters) is
         Kernel_Param      : aliased Mutexes.Mutex_AppSched_Param_T;
         Kernel_Param_Size : aliased Mutexes.Mutex_AppSched_Param_Size_T;
         use type Mutexes.Mutex_AppSched_Param_Size_T;
      begin
         PI.Check_NZ (Mutexes.Pthread_Mutex_GetappSchedparam
                                                (M,
                                                 Kernel_Param'Unchecked_Access,
                                                 Kernel_Param_Size'Access));
         if Kernel_Param_Size /= Parameters'Size / 8 then
            PI.Raise_POSIX_Error (POSIX.Invalid_Argument);
         end if;

         To_Mutex_AppSched_Param_Ac (Param'Address) (0 .. Kernel_Param_Size) :=
           Kernel_Param (0 .. Kernel_Param_Size);
      end Get_Parameters;

   end Application_Defined_Parameters;

   ----------------------
   -- Mutex Attributes --
   ----------------------
   package body Mutex_Attributes is

      function To_Address is
        new Ada.Unchecked_Conversion (Attribute_Handle, System.Address);
      function To_Attribute_Handle is
        new Ada.Unchecked_Conversion (System.Address, Attribute_Handle);

      ---------
      -- Get --
      ---------
      function Get (M : Mutex_Descriptor) return Attribute_Handle is
         Add : aliased System.Address;
      begin
         PI.Check_NZ
           (Mutexes.Posix_Appsched_Mutex_Getspecific (M, Add'Access));
         return To_Attribute_Handle (Add);
      end Get;

      ---------
      -- Set --
      ---------
      procedure Set (Val : in Attribute_Handle;
                     M   : in Mutex_Descriptor) is
      begin
         PI.Check_NZ
           (Mutexes.Posix_Appsched_Mutex_Setspecific (M, To_Address (Val)));
      end Set;

   end Mutex_Attributes;

end POSIX_Mutexes;
