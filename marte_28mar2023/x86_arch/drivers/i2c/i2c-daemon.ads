------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                              i 2 c - d a e m o n
--
--                                    spec
--
-- File 'i2c-daemon.ads'                                  By Sangorrin
--
-- This is the i2c daemon. Drivers for i2c clients should interact with this
-- daemon through the corresponding interface by sending him commands and
-- waiting to receive responses.
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
with Protected_Priority_Queues;
pragma Elaborate_All(Protected_Priority_Queues); -- Commands
with MaRTE.Kernel.Mutexes;

package I2C.Daemon is

   private
      -------------------------------------------------------------------
      -- NOTE:
      -------------------------------------------------------------------
      -- We declare this data and functions here so they are visibles in
      -- the child packages.
      -------------------------------------------------------------------

      -------------------------------------------------------------------
      -- 1) THE COMMANDS QUEUE
      -------------------------------------------------------------------
      -- This is the queue for the commands sent by I2C drivers to the
      -- I2C daemon. Since we are going to access concurrently we need
      -- to protect the queue. The Queue is a priority one so the commands
      -- are served in priority order.
      -------------------------------------------------------------------
      type I2C_Command is record
         Adap_ID           : I2C_Adapter_ID;
         Operation_ID     : I2C_Operation_ID;
         Msg_List    : I2C_Msg_List;
      end record;
      type I2C_Command_Ref is access I2C_Command;

      package CQ is new Protected_Priority_Queues
                            (I2C_Command_Ref, I2C_Priority,">");
      use CQ;

      The_Commands_Queue : CQ.Queue;

      -------------------------------------------------------------------
      -- 2) Operation FUNCTIONS
      -------------------------------------------------------------------
      -- Operation variables are used by I2C drivers to check the status of
      -- their operations. You can also get blocked until the work is done
      -- by calling the 'Wait' funcion.
      -------------------------------------------------------------------
      procedure Set_Status (ID    : in I2C_Operation_ID;
                            Stat         : in I2C_Operation_Status);

      function Get_Status (ID : in I2C_Operation_ID) return I2C_Operation_Status;

      function Wait (Op : in I2C_Operation_ID) return I2C_Operation_Status;

      -------------------------------------------------------------------
      -- 3) BUFFERS
      -------------------------------------------------------------------
      type I2C_Buffer_Cell is record
         In_Use : Boolean;
         Data    : I2C_Data_Ref;
         Flags   : I2C_Flags;
      end record;

      type I2C_Buffer_List is array
        (I2C_Buffer range 1 .. I2C_Buffer'Last) of I2C_Buffer_Cell;

      type I2C_Protected_Buffer_List is record
         Mutex_Ref : MaRTE.Kernel.Mutexes.Mutex_Descriptor;
         List   : I2C_Buffer_List;
      end record;

      procedure Init (Prio : in MaRTE.Kernel.Mutexes.Ceiling_Priority;
                      List : in out I2C_Protected_Buffer_List);

      procedure Create_Recv_Buff (Num_Bytes : in I2C_Data_Count;
                                  Buffer : out I2C_Buffer;
                                  List : in out I2C_Protected_Buffer_List);

      procedure Create_Send_Buff (Data : in I2C_Data;
                                  Buffer : out I2C_Buffer;
                                  List : in out I2C_Protected_Buffer_List);

      procedure Free (Buffer : in I2C_Buffer;
                      List : in out I2C_Protected_Buffer_List);

      function Get_Buffer_Cell (Buffer : in I2C_Buffer;
                                List : in I2C_Protected_Buffer_List)
                               return I2C_Buffer_Cell;

      The_Buffer_List : I2C_Protected_Buffer_List;

end I2C.Daemon;

