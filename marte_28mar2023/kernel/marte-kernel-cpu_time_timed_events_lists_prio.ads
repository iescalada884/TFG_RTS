------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                             'K e r n e l .
--        C P U _ T i m e _ T i m e d _ E v e n t s _ L i s t s _ P r i o'
--
--                                  Body
--
--
--  File 'k-cpu_time_timed_events_lists_prio.ads'                      By Mar.
--
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
with MaRTE.SLL.Prio;
with MaRTE.HAL;
pragma Elaborate_All ( MaRTE.SLL.Prio );

package Kernel.CPU_Time_Timed_Events_Lists_Prio is new

   package K renames MaRTE.Kernel;
  Kernel.CPU_Time_Timed_Events_Lists.Prio ((MaRTE.HAL).HWTime,
                                           Kernel.">",
                                           Kernel.Get_CPU_Time);
