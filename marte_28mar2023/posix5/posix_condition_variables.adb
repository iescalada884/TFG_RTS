------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--             'P O S I X _ C o n d i t i o n _ V a r i a b l e s'
--
--                                  Body
--
--
--  File 'posix_condition_variables.adb'                               By MAR.
--
--
--  Implementation of the package 'Posix_Condition_Variables' as
--  defined in IEEE Std 1003.5b-1996 (POSIX Ada bindings).
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

with POSIX;
with POSIX.Implementation;

with MaRTE.HAL;
with MaRTE.Kernel.Condition_Variables.Internals;

package body POSIX_Condition_Variables is

   package PI  renames POSIX.Implementation;
   package HWI renames MaRTE.HAL;
   package CV  renames MaRTE.Kernel.Condition_Variables;

   -----------------------
   -- Initialize (Attr) --
   -----------------------
   procedure Initialize (Attr : in out Attributes) is
   begin
      PI.Check_NZ
        (CV.Pthread_Condattr_Init (Attr'Unrestricted_Access));
   end Initialize;

   ---------------------
   -- Finalize (Attr) --
   ---------------------
   procedure Finalize (Attr : in out Attributes) is
   begin
      PI.Check_NZ
        (CV.Pthread_Condattr_Destroy (Attr'Unrestricted_Access));
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

   ---------------------
   -- Initialize (CV) --
   ---------------------
   procedure Initialize (Cond : in out Condition;
                         Attr : in     Attributes) is
   begin
      PI.Check_NZ
        (CV.Pthread_Cond_Init (Cond'Unrestricted_Access,
                               Attr'Unrestricted_Access));
   end Initialize;

   ---------------------
   -- Initialize (CV) --
   ---------------------
   procedure Initialize (Cond : in out Condition) is
      Attr : aliased CV.Attributes := CV.Default_Attributes;
   begin
      PI.Check_NZ
        (CV.Pthread_Cond_Init (Cond'Unrestricted_Access, Attr'Access));
   end Initialize;

   -------------------
   -- Descriptor_Of --
   -------------------
   function Descriptor_Of (Cond : Condition) return Condition_Descriptor is
      use type Condition_Descriptor;
      CVd : Condition_Descriptor;
   begin
      CVd := Condition_Variables.Descriptor_Of (Cond);

      if CVd = null then
         PI.Raise_POSIX_Error (Invalid_Argument);
      end if;

      return CVd;
   end Descriptor_Of;
   pragma Inline (Descriptor_Of);

   -------------------
   -- Finalize (CV) --
   -------------------
   procedure Finalize (Cond : in out Condition) is
   begin
      PI.Check_NZ
        (CV.Pthread_Cond_Destroy (Cond'Unrestricted_Access));
   end Finalize;

   ------------
   -- Signal --
   ------------
   procedure Signal (Cond : in Condition_Descriptor) is
   begin
      PI.Check_NZ
        (CV.Pthread_Cond_Signal (Cond));
   end Signal;

   ---------------
   -- Broadcast --
   ---------------
   procedure Broadcast (Cond : in Condition_Descriptor) is
   begin
      PI.Check_NZ
        (CV.Pthread_Cond_Broadcast (Cond));
   end Broadcast;

   ----------
   -- Wait --
   ----------
   procedure Wait (Cond : in Condition_Descriptor;
                   M    : in POSIX_Mutexes.Mutex_Descriptor) is
   begin
      PI.Check_NZ
        (CV.Pthread_Cond_Wait (Cond, M));
   end Wait;

   ----------------
   -- Timed_Wait --
   ----------------
   procedure Timed_Wait (Cond    : Condition_Descriptor;
                         M       : POSIX_Mutexes.Mutex_Descriptor;
                         Timeout : Duration) is
   begin
      PI.Check_NZ
        (CV.Timed_Wait_HWTime (Cond, M, HWI.Duration_To_HWTime (Timeout)));
   end Timed_Wait;

end POSIX_Condition_Variables;





