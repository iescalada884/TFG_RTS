------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                                       i 2 c
--
--                                    spec
--
-- File 'i2c.ads'                                             By Sangorrin
--
-- Main types used across the i2c subsystem
--
-- I2C Specification:
-- http://www.semiconductors.philips.com/acrobat/literature/9398/39340011.pdf
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2004   Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http:--marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael González Harbour      mgh@unican.es
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
with MaRTE.Integer_Types;
use MaRTE.Integer_Types; -- for Unsigned_8/16/32

package I2c is

   -------------------------------------------------------------------
   -- 1) CONSTANTS
   -------------------------------------------------------------------
   --  a) I2C_Mx_Prio
   -------------------------------------------------------------------
   --  Sets how many priority levels can exist. These priorities are
   --  used by the I2C daemon to serve the I2C commands accordingly.
   --  Note that they are not task priorities but command ones.
   -------------------------------------------------------------------
   I2c_Mx_Prio : constant := 256;
   -------------------------------------------------------------------
   --  b) I2C_Mx_Number_Of_Buffers
   -------------------------------------------------------------------
   I2c_Mx_Number_Of_Buffers : constant := 20;
   -------------------------------------------------------------------
   --  c) INTERNAL PRIORITIES (READ!!)
   -------------------------------------------------------------------
   --  Sets the priority of the internal task I2C_Daemon and the
   --  internal protected types.
   --  I2C clients should make sure that their priorities are lower
   --  or equal to the priorities of the internal protected types.
   -------------------------------------------------------------------
   I2c_Daemon_Prio         : constant := 10;
   I2c_Operation_List_Prio : constant := 11;
   I2c_Commands_List_Prio  : constant := 11;
   I2c_Buffer_List_Prio    : constant := 11;

   -------------------------------------------------------------------
   -- 2) DATA TYPES
   -------------------------------------------------------------------
   -- a) I2C_Address
   -------------------------------------------------------------------
   -- This is typically the address of the I2C slave or a Control Word.
   -- This driver is not available for 10 bit addresses yet but we use
   -- a 16 bit type so it could be implemmented in a future work.
   -- The normal I2C slave address is 7 bits long (the write or read
   -- bit is added inside when necessary)
   -------------------------------------------------------------------
   subtype I2c_Address is Unsigned_16;
   -------------------------------------------------------------------
   -- b) I2C_Priority
   -------------------------------------------------------------------
   -- This attribute defines the order in which the I2C commands will
   -- be served by the I2C daemons. The higher the number the higher
   -- the priority.
   -------------------------------------------------------------------
   type I2c_Priority is new Positive range 1 .. I2c_Mx_Prio;
   -------------------------------------------------------------------
   -- c) I2C_Data, I2C_Data_Count, I2C_Data_Ref
   -------------------------------------------------------------------
   subtype I2c_Data_Count is Integer;
   subtype Byte_Type is Unsigned_8;
   type I2c_Data is array (I2c_Data_Count range <>) of aliased Byte_Type;
   type I2c_Data_Ref is access I2c_Data;
   type I2c_Buffer is private;
   Null_I2c_Buffer : constant I2c_Buffer;
   -------------------------------------------------------------------
   -- d) I2C_Operation_ID, I2C_Operation_Status
   -------------------------------------------------------------------
   -- You can send a command to the I2C Daemon, continue with your work,
   -- and poll the status of your operation (or get blocked).
   -------------------------------------------------------------------
   I2c_Operation_Id_Mx : constant := 10;
   type I2c_Operation_Id is range 1 .. I2c_Operation_Id_Mx;
   type I2c_Operation_Status is
         (Not_In_Use,
          Workdone,
          Waiting,
          I2c_Error);
   -------------------------------------------------------------------
   -- e) I2C_Adapter_ID
   -------------------------------------------------------------------
   -- Each I2C_Adapter_ID represent a physical I2C bus. The adapters
   -- are attached to a table in i2c.adapters_table.ads
   -------------------------------------------------------------------
   type I2c_Adapter_Id is
         (Elite,
          Parport,
          Pcm3718);
   -------------------------------------------------------------------
   -- f) I2C_Flags
   -------------------------------------------------------------------
   -- The I2C_Flags allow us to add special functionality in the future
   -------------------------------------------------------------------
   type I2c_Flags is private;
   -------------------------------------------------------------------
   -- g) I2C_Msg_List
   -------------------------------------------------------------------
   -- A list of Messages (for Writing or Reading from an I2C device)
   -- for telling the I2C daemon to execute a desired operation.
   -------------------------------------------------------------------
   type I2c_Msg is private;
   type I2c_Msg_List is private;

private

   type I2c_Flags is new Unsigned_16;
   I2c_Default_Flags  : constant I2c_Flags := 16#00#;
   I2c_M_Ten          : constant I2c_Flags := 16#10#;
   I2c_M_Nostart      : constant I2c_Flags := 16#4000#;
   I2c_M_Rev_Dir_Addr : constant I2c_Flags := 16#2000#;
   I2c_M_Ignore_Nak   : constant I2c_Flags := 16#1000#;
   I2c_M_No_Rd_Ack    : constant I2c_Flags := 16#0800#;
   I2c_M_Rd           : constant I2c_Flags := 16#01#;   -- Read/Write flag

   type I2c_Buffer is new Unsigned_32 range 0 .. I2c_Mx_Number_Of_Buffers;
   for I2c_Buffer'Size use 32;
   Null_I2c_Buffer : constant I2c_Buffer := 0;

   type I2c_Msg is
      record
         Addr   : I2c_Address;
         Flags  : I2c_Flags;
         Count  : I2c_Data_Count;
         Buffer : I2c_Data_Ref;
      end record;

   type I2c_Msg_List_Array is array (Natural range <>) of I2c_Msg;
   type I2c_Msg_List is access I2c_Msg_List_Array;

   -- C Compatibility Stuff
   for I2c_Priority'Size use 16;
   for I2c_Adapter_Id'Size use 16;
   for I2c_Adapter_Id use (
      Elite   => 1,
      Parport => 2,
      Pcm3718 => 3);

   for I2c_Operation_Id'Size use 16;

   for I2c_Operation_Status'Size use 8;
   for I2c_Operation_Status use (
      Not_In_Use => 1,
      Workdone   => 2,
      Waiting    => 3,
      I2c_Error  => 4);

end I2c;

