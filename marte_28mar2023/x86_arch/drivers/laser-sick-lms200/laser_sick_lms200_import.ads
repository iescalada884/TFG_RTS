------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.59B   070502
--
--                      'l a s e r _ i m p o r t'
--
--                                   Spec
--
--
--  File 'laser_import.ads'                               By F.J.Feijoo
--                                             University of Zaragoza (UNIZAR)
--
--
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2007, Universidad de Cantabria, SPAIN
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
-- with Drivers_MaRTE;  use Drivers_MaRTE;
with System, Ada.Unchecked_Conversion;
-- with Ring_Buffers;
with Interfaces.C;

package Laser_Sick_LMS200_Import is

   package C renames Interfaces.C;
   use type C.char_array;

   MAXNDATA    : constant C.Size_t := 801; -- (de 0 a 801)
   MAXVALORES  : constant C.Size_t := 400; -- (de 0 a 401)
   --velocidades posibles de conexion
   BAUD_500000 : constant C.Int := 512;
   BAUD_38400  : constant C.Int := 256;
   BAUD_19200  : constant C.Int := 128;
   BAUD_9600   : constant C.Int := 64;
   --modos del laser
   RANGE_100   : constant C.Int := 32;
   RANGE_180   : constant C.Int := 16;
   --resolucion del laser
   RES_1_DEG   : constant C.Int := 8;
   RES_0_5_DEG : constant C.Int := 4;
   RES_0_25_DEG : constant C.Int := 2;
   -- en mm o en cm
   MMMODE : constant C.Int := 1;
   CMMODE : constant C.Int := 0;

   procedure Showdata(len: C.Int; buf: C.char_array);
   pragma Import (C, showdata, "showdata");

   procedure ShowLaser(len: C.Int; buf: C.char_array);
   pragma Import (C, ShowLaser, "showLaser");

   procedure Chkstatus(chk: C.char);
   pragma Import (C, Chkstatus, "chkstatus");

   procedure ResetLMS;
   pragma Import (C, ResetLMS, "resetLMS");

   procedure StopLMS;
   pragma Import (C, StopLMS, "stopLMS");

   procedure ConnectToLMS(range_mode, res_mode, unit_mode: C.Int;
                          port: C.char_array;
                          baud_sel:C.Int);
   pragma Import (C, ConnectToLMS, "connectToLMS");

   function ReadLMSdata(buf: C.char_array) return C.Int;
   pragma Import (C, ReadLMSdata, "readLMSdata");

   function GetCountLaser return C.Int;
   pragma Import (C, GetCountLaser, "GetCountLaser");

   function GetScanResLaser return C.Double;
   pragma Import (C, GetScanResLaser, "GetScanResLaser");

   function GetRangeResLaser return C.Double;
   pragma Import (C, GetRangeResLaser, "GetRangeResLaser");

   function ReadLMSValues return C.Int;
   pragma Import (C, ReadLMSValues, "readLMSValues");

   function ReadLMSValuesDemand return C.Int;
   pragma Import (C, ReadLMSValuesDemand, "readLMSValuesDemand");

   function Laserazo (i: C.Int) return C.Int;
   pragma Import (C, laserazo, "laserazo");

   procedure LockLaser;
   pragma Import (C, LockLaser, "lockLaser");

   procedure UnlockLaser;
   pragma Import (C, UnlockLaser, "unlockLaser");

   function GetStatus return C.Int;
   pragma Import (C, GetStatus, "getStatus");

   procedure VaciarLaser;
   pragma Import (C, VaciarLaser, "vaciarLaser");

end Laser_Sick_LMS200_Import;
