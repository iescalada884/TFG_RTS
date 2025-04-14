------------------------------------------------------------------------------
-- -------------------         M a R T E   O S         -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                 'S i m p l e _ D y n a m i c _ M e m o r y'
--
--                                  Body
--
--
--  File 'simple_dynamic_memory.adb'                                  By MAR.
--
--
--  Simple 'malloc' related functions. 'free' does nothing, 'realloc'
--  doesn't works yet. This implementation is useful for applications
--  that does not use dynamic memory at all or uses it only at
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
with MaRTE.Direct_IO;
with MaRTE.Configuration_Parameters;

package body Simple_Dynamic_Memory is


   function Address_To_Unsigned_32 (Add : in System.Address)
                                    return Unsigned_32;
   pragma Import (C, Address_To_Unsigned_32, "convert_4_bytes_long_data");


   Bytes_In_Pool : constant Unsigned_32 :=
     MaRTE.Configuration_Parameters.Dynamic_Memory_Pool_Size_In_Bytes;
   Bytes_Assigned : Unsigned_32;
   Base_Memory_Pool : Unsigned_32;

   ------------
   -- Malloc --
   ------------
   function Malloc (Size : Unsigned_32) return Unsigned_32 is
      First_Byte : Unsigned_32 := Bytes_Assigned + Base_Memory_Pool;
   begin
      if Bytes_Assigned + Size <= Bytes_In_Pool then
         Bytes_Assigned := Bytes_Assigned + Size;
         return First_Byte;
      else
         MaRTE.Direct_IO.Put ("Memory pool exhausted. Bytes free: ");
         MaRTE.Direct_IO.Put (Integer (Bytes_In_Pool - Bytes_Assigned));
         MaRTE.Direct_IO.Error ("Memory pool exhausted!! ");
         return 0;
      end if;
   end Malloc;

   ------------
   -- Calloc --
   ------------
   function Calloc (Nmemb, Size : Unsigned_32) return Unsigned_32 is
      First_Byte : Unsigned_32 := Bytes_Assigned + Base_Memory_Pool;
   begin
      if Bytes_Assigned + Size * Nmemb  <= Bytes_In_Pool then
         Bytes_Assigned := Bytes_Assigned + Size * Nmemb;
         return First_Byte;
      else
         MaRTE.Direct_IO.Put ("Memory pool exhausted. Bytes free: ");
         MaRTE.Direct_IO.Put (Integer (Bytes_In_Pool - Bytes_Assigned));
         MaRTE.Direct_IO.Error ("Memory pool exhausted!! ");
         return 0;
      end if;
   end Calloc;

   ----------
   -- Free --
   ----------
   procedure Free (Ptr : Unsigned_32) is
   begin
      null;
   end Free;

   -------------
   -- Realloc --
   -------------
   function Realloc (Ptr, Size : Unsigned_32) return Unsigned_32 is
   begin
      MaRTE.Direct_IO.Error ("Realloc: Non-implemented procedure !!");
      return 0;
   end Realloc;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (Pool_Start_Address : in System.Address) is
   begin
      Bytes_Assigned := 0;
      Base_Memory_Pool := Address_To_Unsigned_32 (Pool_Start_Address);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("Dynamic Memory Pool Initialized (");
      MaRTE.Direct_IO.Put (Integer (Bytes_In_Pool));
      MaRTE.Direct_IO.Put ( " bytes: ");
      MaRTE.Direct_IO.Put (Integer (Base_Memory_Pool), 16);
      MaRTE.Direct_IO.Put (" to ");
      MaRTE.Direct_IO.Put (Integer (Base_Memory_Pool + Bytes_In_Pool), 16);
      MaRTE.Direct_IO.Put (")");
      MaRTE.Direct_IO.New_Line;
   end Initialize;

   Dummy : Integer; --  It is necessary this package always has elaboration
   --  because the elaboration procedure is called from 'init_dynamic_memory.c'
begin
   Dummy := 1;
end Simple_Dynamic_Memory;
