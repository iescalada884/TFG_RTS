------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--       'D y n a m i c _ B u f f e r _ D r i v e r . F u n c t i o n s'
--
--                                    Body
--
--
--  File 'dynamic_buffer_driver-functions.adb'                 By Fguerreira
--
--
--  A dynamic buffer with counters, so that sequentials read/write can be done
--     (Functions)
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

with Drivers_MaRTE; use Drivers_MaRTE;
with MaRTE.Integer_Types;
with Ada.Unchecked_Conversion;

package body Dynamic_Buffer_Driver.Functions is

   use type Drivers_MaRTE.Buffer_Length;
   use type Drivers_MaRTE.Int;

   type Dynamic_Buffer is array
     (Drivers_MaRTE.Buffer_Length range <>) of MaRTE.Integer_Types.Unsigned_8;

   type Dynamic_Buffer_Ac is access all Dynamic_Buffer;

   Buffer_Driver_Length : aliased Drivers_MaRTE.Buffer_Length := 0;
   Buffer_Driver_Ac : Dynamic_Buffer_Ac;

   Read_Counter, Write_Counter : Drivers_MaRTE.Buffer_Length := 1;

   ------------
   -- Create --
   ------------

   function Create return Int is
   begin
      return 0;
   end Create;

   ------------
   -- Remove --
   ------------

   function Remove return Int is
   begin
      return 0;
   end Remove;

   ----------
   -- Open --
   ----------

   function Open (Fd   : in File_Descriptor;
                  Mode : in File_Access_Mode) return Int is
   begin
      Read_Counter  := 1;
      Write_Counter := 1;
      return 0;
   end Open;

   -----------
   -- Close --
   -----------

   function Close (Fd : in File_Descriptor) return Int is
   begin
      Read_Counter  := 1;
      Write_Counter := 1;
      return 0;
   end Close;

   ----------
   -- Read --
   ----------

   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Int
   is
      Begin_Read : Integer;
   begin
      if Read_Counter > Write_Counter then
         return 0;
      end if;

      Begin_Read   := Integer (Read_Counter);
      Read_Counter := Read_Counter + Bytes;

      if Read_Counter > Write_Counter then
         Read_Counter := Write_Counter;
      end if;

      for I in Begin_Read .. Integer (Read_Counter) - 1 loop
         Buffer_Ptr.all (Buffer_Length (I - (Begin_Read - 1))) :=
           Buffer_Driver_Ac.all (Buffer_Length (I));
      end loop;

      return Int (Integer (Read_Counter) - Begin_Read);
   end Read;

   -----------
   -- Write --
   -----------

   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Int
   is
      Begin_Write : Integer;
   begin
      if Write_Counter > Buffer_Driver_Length then
         return 0;
      end if;

      Begin_Write   := Integer (Write_Counter);
      Write_Counter := Write_Counter + Bytes;
      if Write_Counter > Buffer_Driver_Length then
         Write_Counter := Buffer_Driver_Length + 1;
      end if;

      for I in Begin_Write .. Integer (Write_Counter) - 1 loop
         Buffer_Driver_Ac.all (Buffer_Length (I)) :=
           Buffer_Ptr.all (Buffer_Length (I - (Begin_Write - 1)));
      end loop;

      return Int (Integer (Write_Counter) - Begin_Write);
   end Write;

   -----------
   -- Ioctl --
   -----------

   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac) return Int
   is
      type Ioctl_Data_Ac is access all Ioctl_Data;
      function Buffer_Ac_To_Ioctl_Data_Ac is new
        Ada.Unchecked_Conversion (Buffer_Ac, Ioctl_Data_Ac);
      Data_Ac : Ioctl_Data_Ac;
   begin
      Data_Ac := Buffer_Ac_To_Ioctl_Data_Ac (Ioctl_Data_Ptr);
      case Ioctl_Options'Val (Request) is
         when Set_Buffer_Length =>
            Buffer_Driver_Length := Buffer_Length (Data_Ac.all);
            Buffer_Driver_Ac := new Dynamic_Buffer (1 .. Buffer_Driver_Length);
            Read_Counter  := 1;
            Write_Counter := 1;
         when Reset_Both_Counters =>
            Read_Counter  := 1;
            Write_Counter := 1;
         when Reset_Read_Counter =>
            Read_Counter := 1;
         when Reset_Write_Counter =>
            Write_Counter := 1;
         when others =>
            return -1;
      end case;
      return 0;
   end Ioctl;

end Dynamic_Buffer_Driver.Functions;
