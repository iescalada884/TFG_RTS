------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                     'P O S I X _ S e m a p h o r e s'
--
--                                  Body
--
--
--
--  File 'posix_semaphores.adb'                                        By MAR.
--
--
--  Implementation of the package 'Posix_Semaphores' as defined in
--  IEEE Std 1003.5b-1996 (POSIX Ada bindings).
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
with POSIX.Implementation;
with MaRTE.Kernel.Semaphores.Operations;
with MaRTE.Integer_Types;
with MaRTE.Timespec;
with MaRTE.HAL;
with MaRTE.Kernel.Tasks_Operations.Internals;

package body POSIX_Semaphores is

   package PI       renames POSIX.Implementation;
   package MSEM     renames MaRTE.Kernel.Semaphores;
   package MSEMOP   renames MaRTE.Kernel.Semaphores.Operations;
   package HWI      renames MaRTE.HAL;
   package TOI      renames MaRTE.Kernel.Tasks_Operations.Internals;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (Sem       : in out Semaphore;
                         Value     : in     Natural;
                         Is_Shared : in     Boolean := False) is
   begin
      PI.Check_NNeg
        (MSEMOP.Sem_Init (Sem     => Sem'Unrestricted_Access,
                          Pshared => 1, --  Irrelevant in MaRTE
                          Value   => MSEM.Semaphore_Value (Value)));
   end Initialize;

   -------------------
   -- Descriptor_Of --
   -------------------
   function Descriptor_Of (Sem : Semaphore) return Semaphore_Descriptor is
   begin
      return Sem'Unrestricted_Access;
   end Descriptor_Of;

   --------------
   -- Finalize --
   --------------
   procedure Finalize (Sem : in out Semaphore) is
   begin
      PI.Check_NNeg (MSEMOP.Sem_Destroy (Sem'Unrestricted_Access));
   end Finalize;

   ----------
   -- Wait --
   ----------
   procedure Wait
     (Sem            : in Semaphore_Descriptor;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      PI.Check_NNeg (MSEMOP.Sem_Wait (Sem));
   end Wait;

   ----------------
   -- Timed_Wait --
   ----------------
   Valid_Timespec : MaRTE.Timespec.Timespec := (0, 0);
   procedure Timed_Wait
     (Sem            : in Semaphore_Descriptor;
      Abs_Timeout    : in POSIX.Timespec;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      PI.Check_NNeg
        (MSEMOP.Sem_Timedwait_HWTime (Sem,
                                      HWI.Duration_To_HWTime (Abs_Timeout),
                                      Valid_Timespec));
   end Timed_Wait;

   --------------
   -- Try_Wait --
   --------------
   function Try_Wait (Sem : Semaphore_Descriptor) return Boolean is
      Ret : MaRTE.Integer_Types.Int;
      use type MaRTE.Integer_Types.Int;
   begin
      if MSEMOP.Sem_Trywait (Sem) = -1 then
         if (TOI.Get_Last_POSIX_Error_Code =
             MaRTE.Kernel.RESOURCE_TEMPORARILY_UNAVAILABLE) then
            --  Semaphore cannot be taken
            return False;
         else
            --  Any other error
            PI.Raise_POSIX_Error_On_Error;
         end if;
      end if;
      return True; -- Semaphore has been locked
   end Try_Wait;

   ----------
   -- Post --
   ----------
   procedure Post (Sem : in Semaphore_Descriptor) is
   begin
      PI.Check_NNeg (MSEMOP.Sem_Post (Sem));
   end Post;

   ---------------
   -- Get_Value --
   ---------------
   function Get_Value (Sem : Semaphore_Descriptor) return Integer is
      Val : aliased MSEM.Semaphore_Value;
   begin
      PI.Check_NNeg (MSEMOP.Sem_Getvalue (Sem, Val'Access));
      return Integer (Val);
   end Get_Value;

end POSIX_Semaphores;
