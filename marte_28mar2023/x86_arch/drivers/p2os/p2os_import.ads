------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.59B   070502
--
--                      'p 2 o s _ i m p o r t'
--
--                                   Spec
--
--
--  File 'p2os_import.ads'                               By F.J.Feijoo
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
with System, Ada.Unchecked_Conversion;
with Interfaces.C;

package p2os_import is

    package C renames Interfaces.C;
    use type C.char_array;

    MAXNDATA: constant Interfaces.C.Size_t := 801; -- (de 0 a 801)
    CMMODE : constant Integer := 0;

--  procedure showdata(len: C.Int; buf: C.char_array);
--  pragma Import (C, showdata, "showdata");

    function initP2os (port: C.char_array) return C.Int;
    pragma Import (C,initP2os, "initP2os" );

    procedure PrintP2OS_SIP;
    pragma Import (C, PrintP2OS_SIP, "PrintP2OS_SIP");

    procedure SendPulse;
    pragma Import (C, SendPulse, "SendPulse");

    function p2osShutdown return C.Int;
    pragma Import (C,p2osShutdown, "p2osShutdown" );

    --  toggle motors on/off
    procedure p2osMotorOn;
    pragma Import (C, p2osMotorOn, "p2osMotorOn");

    procedure p2osMotorOff;
    pragma Import (C, p2osMotorOff, "p2osMotorOff");

    --  toggle sonar on/off
    procedure p2osSonarOn;
    pragma Import (C, p2osSonarOn, "p2osSonarOn");

    procedure p2osSonarOff;
    pragma Import (C, p2osSonarOff, "p2osSonarOff");

    -- Actualiza los valors del sippacket.
    -- Esta función debe ser periodica (todo el rato en un bucle)
    procedure p2osGetValues;
    pragma Import (C, p2osGetValues, "p2osGetValues");

    -- Funciones para el acceso al paquete SIP desde ADA
    function p2osGetLwstall return C.Int;
    pragma Import (C,p2osGetLwstall, "p2osGetLwstall" );

    function p2osGetRwstall return C.Int;
    pragma Import (C,p2osGetRwstall, "p2osGetRwstall" );

    function p2osGetStatus return C.unsigned_char;
    pragma Import (C,p2osGetStatus, "p2osGetStatus" );

    function p2osGetBattery return C.unsigned_char;
    pragma Import (C,p2osGetBattery, "p2osGetBattery" );

    function p2osGetSonarreadings return C.unsigned_char;
    pragma Import (C,p2osGetSonarreadings, "p2osGetSonarreadings" );

    function p2osGetAnalog return C.unsigned_char;
    pragma Import (C,p2osGetAnalog, "p2osGetAnalog" );

    function p2osGetDigin return C.unsigned_char;
    pragma Import (C,p2osGetDigin, "p2osGetDigin" );

    function p2osGetDigout return C.unsigned_char;
    pragma Import (C,p2osGetDigout, "p2osGetDigout" );

    function p2osGetPtu return C.unsigned_short;
    pragma Import (C,p2osGetPtu, "p2osGetPtu" );

    function p2osGetCompass return C.unsigned_short;
    pragma Import (C,p2osGetCompass, "p2osGetCompass" );

    function p2osGetTimer return C.unsigned_short;
    pragma Import (C,p2osGetTimer, "p2osGetTimer" );

    function p2osGetRawxpos return C.unsigned_short;
    pragma Import (C,p2osGetRawxpos, "p2osGetRawxpos" );

    function p2osGetRawypos return C.unsigned_short;
    pragma Import (C,p2osGetRawypos, "p2osGetRawypos" );

    function p2osGetFrontbumpers return C.unsigned_short;
    pragma Import (C,p2osGetFrontbumpers, "p2osGetFrontbumpers" );

    function p2osGetRearbumpers return C.unsigned_short;
    pragma Import (C,p2osGetRearbumpers, "p2osGetRearbumpers" );

    function p2osGetLvel return C.short;
    pragma Import (C,p2osGetLvel, "p2osGetLvel" );

    function p2osGetRvel return C.short;
    pragma Import (C,p2osGetRvel, "p2osGetRvel" );

    function p2osGetControl return C.short;
    pragma Import (C,p2osGetControl, "p2osGetControl" );

    function p2osGetSonar(I: C.Int) return C.unsigned_short;
    pragma Import (C, p2osGetSonar, "p2osGetSonar");

    function p2osGetXpos return float;
    pragma Import (C,p2osGetXpos, "p2osGetXpos" );

    function p2osGetYpos return float;
    pragma Import (C,p2osGetYpos, "p2osGetYpos" );

    function p2osGetAngle return float;
    pragma Import (C,p2osGetAngle, "p2osGetAngle" );

    function p2osGetXSpeed return float;
    pragma Import (C,p2osGetXSpeed, "p2osGetXSpeed" );

    function p2osGetYawSpeed return float;
    pragma Import (C,p2osGetYawSpeed, "p2osGetYawSpeed" );

    function p2osGetX_offset return C.Int;
    pragma Import (C,p2osGetX_offset, "p2osGetX_offset" );

    function p2osGetY_offset return C.Int;
    pragma Import (C,p2osGetY_offset, "p2osGetY_offset" );

    function p2osGetAngle_offset return C.Int;
    pragma Import (C,p2osGetAngle_offset, "p2osGetAngle_offset" );

    procedure SetSpeed(trans: float; rot: float);
    pragma Import (C, SetSpeed, "SetSpeed");

    procedure lockP2os;
    pragma Import (C,lockP2os, "lockP2os" );

    procedure unlockP2os;
    pragma Import (C,unlockP2os, "unlockP2os" );

end p2os_import;