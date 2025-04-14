------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.59B   070502
--
--                      'n o v a t e l _ i m p o r t'
--
--                                   Spec
--
--
--  File 'novatel_import.ads'                               By F.J.Feijoo
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
with Interfaces.C;

package novatel_import is

    package C renames Interfaces.C;

    procedure initNovatel(port: C.char_array);
    pragma Import (C, initNovatel, "initNovatel");

    function SetupNovatel return C.Int;
    pragma Import (C, SetupNovatel, "SetupNovatel");

    function leerSerie return C.Int;
    pragma Import (C, leerSerie, "leerSerie");

    procedure leeGps;
    pragma Import (C, leeGps, "leeGps");

    function gps_X return Float;
    pragma Import (C, gps_X, "gps_X");

    function gps_Y return Float;
    pragma Import (C, gps_Y, "gps_Y");

    procedure ShutdownNovatel;
    pragma Import (C, ShutdownNovatel, "ShutdownNovatel");

end novatel_import;