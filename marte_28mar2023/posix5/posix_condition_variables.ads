------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--             'P O S I X _ C o n d i t i o n _ V a r i a b l e s'
--
--                                  Spec
--
--
--  File 'posix_condition_variables.ads'                               By MAR.
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
with POSIX_Mutexes;

with MaRTE.Kernel.Condition_Variables;
use MaRTE.Kernel;

package POSIX_Condition_Variables is

   --  ==========  --
   --   WARNINGS   --
   --  ==========  --

   --  This package is for mixed-language programming, in which
   --  an Ada task needs to synchronize with a C thread.

   --  Do NOT use POSIX CVs to synchronize between Ada tasks.
   --  Instead, use Ada protected objects.

   --  If you use one of these "raw" CVs, you risk undefined
   --  behavior if you violate any of the POSIX.1c rules about CVs,
   --  or if you attempt to abort (including ATC) a task that is performing
   --  a mutex or CV operation.

   subtype Condition is Condition_Variables.Condition;
   subtype Condition_Descriptor is Condition_Variables.Condition_Descriptor;

   subtype Attributes is Condition_Variables.Attributes;
   procedure Initialize (Attr : in out Attributes);
   procedure Finalize (Attr : in out Attributes);

   function Get_Process_Shared (Attr : Attributes) return Boolean;
   procedure Set_Process_Shared
     (Attr      : in out Attributes;
      Is_Shared : in Boolean := False);

   procedure Initialize
     (Cond : in out Condition;
      Attr : in Attributes);
   procedure Initialize (Cond : in out Condition);
   function Descriptor_Of (Cond : Condition) return Condition_Descriptor;
   procedure Finalize (Cond : in out Condition);

   procedure Signal (Cond : in Condition_Descriptor);
   procedure Broadcast (Cond : in Condition_Descriptor);

   procedure Wait
     (Cond : in Condition_Descriptor;
      M    : in POSIX_Mutexes.Mutex_Descriptor);
   procedure Timed_Wait
     (Cond    : Condition_Descriptor;
      M       : POSIX_Mutexes.Mutex_Descriptor;
      Timeout : Duration);

end POSIX_Condition_Variables;
