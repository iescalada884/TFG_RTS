------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                     'P O S I X _ S e m a p h o r e s'
--
--                                  Spec
--
--
--
--  File 'posix_semaphores.ads'                                        By MAR.
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
with MaRTE.Kernel.Semaphores;
with POSIX;

package POSIX_Semaphores is

   subtype Semaphore is MaRTE.Kernel.Semaphores.Semaphore;
   subtype Semaphore_Descriptor is MaRTE.Kernel.Semaphores.Semaphore_Ac;

   procedure Initialize
     (Sem       : in out Semaphore;
      Value     : in     Natural;
      Is_Shared : in     Boolean := False);

   function Descriptor_Of (Sem : Semaphore) return Semaphore_Descriptor;

   procedure Finalize (Sem : in out Semaphore);

   procedure Wait
     (Sem            : in Semaphore_Descriptor;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals);

   function Try_Wait (Sem : Semaphore_Descriptor) return Boolean;

   procedure Timed_Wait
     (Sem            : in Semaphore_Descriptor;
      Abs_Timeout    : in POSIX.Timespec;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals);
   --  Not included yet in the POSIX Ada bindings

   procedure Post (Sem : in Semaphore_Descriptor);

   function Get_Value (Sem : Semaphore_Descriptor) return Integer;

end POSIX_Semaphores;
