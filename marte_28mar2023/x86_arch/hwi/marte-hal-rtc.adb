------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                                 'R T C'
--
--                                   Body
--
--
--  File 'rtc.adb'                                                     By Mar.
--
--
--  PC Real Time Clock management.
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

package body MaRTE.HAL.RTC is

   procedure Get_RTC_Time_C (T : access RTC_Time);
   pragma Import (C, Get_RTC_Time_C, "get_rtc_time");

   ------------------
   -- Get_RTC_Time --
   ------------------
   function Get_RTC_Time return RTC_Time is
      T : aliased RTC_Time;
   begin
      Get_RTC_Time_C (T'Access);
      return T;
   end Get_RTC_Time;

end MaRTE.HAL.RTC;
