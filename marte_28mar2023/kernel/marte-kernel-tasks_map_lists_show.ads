------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--          'K e r n e l . T a s k s _ M a p _ L i s t s _ S h o w'
--
--                                  Spec
--
--
--  File 'k-tasks_map_lists_show.ads'                                  By Mar.
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
with MaRTE.Kernel.Debug;
with MaRTE.SLL.Map.Show;
pragma Elaborate_All (MaRTE.SLL.Map.Show);
with MaRTE.Direct_IO;


package MaRTE.Kernel.Tasks_Map_Lists_Show is
   new MaRTE.Kernel.Tasks_Map_Lists.Show
     (MaRTE.Kernel.Debug.Short_Show_Task_On_Console,
      MaRTE.Direct_IO.Put,
      MaRTE.Direct_IO.Put);
