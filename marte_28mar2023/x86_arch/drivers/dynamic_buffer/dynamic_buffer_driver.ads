------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                 'D y n a m i c _ B u f f e r _ D r i v e r'
--
--                                    Spec
--
--
--  File 'dynamic_buffer_driver.ads'                            By Fguerreira
--
--
--  A dynamic buffer with counters, so that sequentials read/write can be done
--     (IOCTL options)
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
with Drivers_MaRTE;

package Dynamic_Buffer_Driver is

   -------------------------------------
   --  Dynamic Buffer 'Ioctl' Options --
   -------------------------------------

   type Ioctl_Options is
      (Set_Buffer_Length,      -- Ioctl_Options => 0
       Reset_Both_Counters,    -- Ioctl_Options => 1
       Reset_Read_Counter,     -- Ioctl_Options => 2
       Reset_Write_Counter);   -- Ioctl_Options => 3

   for Ioctl_Options use
     (Set_Buffer_Length   => 0,
      Reset_Both_Counters => 1,
      Reset_Read_Counter  => 2,
      Reset_Write_Counter => 3);

   for Ioctl_Options'Size use Integer'Size;

   ----------------------------------
   --  Dynamic Buffer 'Ioctl' Data --
   ----------------------------------

   type Ioctl_Data is new Drivers_MaRTE.Buffer_Length;

end Dynamic_Buffer_Driver;
