--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--   'T e s t _ T i m i n g _ E v e n t s _ T i m e _ F i r s t _ L a s t'
--
--                               Ada Program
--
--
--  File 'test_timing_eventss_time_first_last.adb'                                   By MAR.
--
--  Test the Timing Events when programmed to expire to Time_Last (possible
--  overflow) and Time_First (negative value).
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
with Text_IO;
with Ada.Real_Time;
with Ada.Real_Time.Timing_Events;

with Timing_Events_Time_First_Last_PO;
with Reports;

procedure Test_Timing_Events_Time_First_Last is

   TE : Ada.Real_Time.Timing_Events.Timing_Event;

begin
   Reports.Init;
   Text_IO.Put_Line ("Test_Timing_Events_Time_First_Last");

   --  Set handler for Time_Last

   Ada.Real_Time.Timing_Events.Set_Handler
     (TE,
      Ada.Real_Time.Time_Last,
      Timing_Events_Time_First_Last_PO.PO.Handler_Never'access);

   --  Set handler for Time_First

   Ada.Real_Time.Timing_Events.Set_Handler
     (TE,
      Ada.Real_Time.Time_First,
      Timing_Events_Time_First_Last_PO.PO.Handler_Always'access);

   Reports.Assert
     (Timing_Events_Time_First_Last_PO.PO.Is_Handlers_Execution_OK);

   Reports.Test_OK;

end Test_Timing_Events_Time_First_Last;
