------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                            i 2 c . a d a p t e r s
--
--                                    spec
--
-- File 'i2c-adapters.ads'                                    By Sangorrin
--
--
-- An adapter representes a physical i2c bus, and how to send/receive bytes
-- from it. To create a new adapter you will have to create a child and
-- override the Master_Xfer function.
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

package I2c.Adapters is

   type I2c_Adapter is abstract tagged
      record
         Id : I2c_Adapter_Id;
      end record;
   type I2c_Adapter_Ref is access all I2c_Adapter'Class;

   function Get_Id (
         Adap : I2c_Adapter)
      return I2c_Adapter_Id;
      
   function Master_Xfer (
         Adap     : I2c_Adapter;
         Msg_List : I2c_Msg_List)
     return Int is
   abstract;

end I2c.Adapters;

