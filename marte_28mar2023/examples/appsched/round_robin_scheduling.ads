------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                 'R o u n d _ R o b i n _ S c h e d u l i n g'
--
--                                   Spec
--
--
--  File 'round_robin_scheduling.ads'                                  By MAR.
--
--
--  Package that encapsulates the operations to create Ada Round Robin
--  tasks. In the body of this package the Round Robin scheduler task is
--  created. Uses the Application-Defined Scheduling Interface implemented
--  in MaRTE OS.
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
with System;

generic
   Default_Quantum : Duration;
   Max_Round_Robin_Priority : System.Priority;
package Round_Robin_Scheduling is

   -----------------------------
   -- Become_Round_Robin_Task --
   -----------------------------
   procedure Become_Round_Robin_Task
     (Quantum : in Duration := Default_Quantum);

   -------------------------
   -- Terminate_Execution --
   -------------------------
   procedure Terminate_Execution;

end Round_Robin_Scheduling;
