------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                'S i m p l e _ D y n a m i c _ M e m o r y'
--
--                                  Spec
--
--
--  File 'simple_dynamic_memory.ads'                                   By MAR.
--
--
--  Simple dynamic memory allocation algorithm: allows allocation of
--  memory blocks with no deallocation. It is useful for applications
--  that do not use dynamic memory at all or use it only at
--  initialization time.
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
with System;
with Marte.Integer_Types; use Marte.Integer_Types;

package Simple_Dynamic_Memory is

   function Malloc (Size : Unsigned_32) return Unsigned_32;

   function Calloc (Nmemb, Size : Unsigned_32) return Unsigned_32;

   --  No deallocation!!. Free doesn't do anything
   procedure Free (Ptr : Unsigned_32);

   --  XXX It does not work yet
   function Realloc (Ptr, Size : Unsigned_32) return Unsigned_32;

   procedure Initialize (Pool_Start_Address : in System.Address);

end Simple_Dynamic_Memory;
