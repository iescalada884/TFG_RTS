------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--   Copyright (C) 2003-2005   Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
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
--
--                   'e t h e r n e t _ d r i v e r . a d s'
--
--                                     Ada
--
--
--  File 'ethernet_driver.ads'                                        By Chema.
--                                                          Jose Maria Martinez
--                                                            <chema@gmx.net>
--  Import declarations of eth_ioctl.h
--
-----------------------------------------------------------------------------

with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package Ethernet_Driver is

   --  ---------------------------
   --  Ethernet 'Ioctl' Options
   --  ---------------------------

   --  Commands explanation.

   --   Eth_Hardware_Address : Will write in Eth_Addr_Ioctl_Arg the Physical
   --                          MAC address of the current Ethernet card.
   --                          (the size of MAC addr is 48 bits.)

   --  Eth_Blocking_Read : Will turn the read operation blocking. read will not
   --                      return till any data is received.

   --  Eth_Non_Blocking_Read : Will turn the read operation as non blocking.
   --                          The read call will return with or without (in
   --                          case not any) data

   --  Set_Protocol_Filter : Will set the receiving protocol filter. By default
   --                        the driver only receives RT_EP packets. You can
   --                        configure to receive only packets defined in
   --                        Eth_Proto_Arg (unsigned short *)
   --                        (IP, NETBIOS, etc..
   --                        <see include/drivers/if_ether.h> for protocol
   --                        numbers). If set to 0, the driver will receive all
   --                        protocol packets on the net.

   --  Get_Protocol_Filter : Will write in Eth_Proto_Arg (unsigned short) the
   --                        current protocol filter.
   --                        If protocol 0, the protocol filter is disabled

   type Ioctl_Cmd_No_Args is
     (Eth_Blocking_Read,
      Eth_Non_Blocking_Read);
   for Ioctl_Cmd_No_Args use
      (Eth_Blocking_Read     => 16#0501#,
       Eth_Non_Blocking_Read => 16#0502#);

   type Ioctl_Cmd_Addr is
     (Eth_Hardware_Address);
   for Ioctl_Cmd_Addr use
      (Eth_Hardware_Address => 16#0500#);

   type Ioctl_Cmd_Protocol is
     (Set_Protocol_Filter,
      Get_Protocol_Filter);
   for Ioctl_Cmd_Protocol use
      (Set_Protocol_Filter => 16#0503#,
       Get_Protocol_Filter => 16#0504#);

   --  ------------------------
   --  Ethernet 'Ioctl' Data
   --  ------------------------

   type Eth_Addr_Ioctl_Arg is array (1 .. 6) of MaRTE.Integer_Types.Unsigned_8;

   subtype Eth_Proto_Arg is MaRTE.Integer_Types.Unsigned_16;

   --   type Eth_Ioctl_Null is private;


end Ethernet_Driver;
