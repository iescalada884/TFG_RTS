------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                       'P O S I X _ P r o f i l e s'
--
--                                  Spec
--
--
--  File 'posix_profiles.ads'                                           By MAR.
--
--
--  Package 'POSIX_Profiles' as defined in IEEE Std 1003.13-1998
--  (Annex B).
--
--  MaRTe OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
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

package Posix_Profiles is

   -- Profile Options
   subtype Realtime_Minimal    is Boolean range TRUE .. TRUE;
   subtype Realtime_Controller is Boolean range FALSE .. FALSE;
   subtype Realtime_Dedicated  is Boolean range FALSE .. FALSE;
   subtype Realtime_Multi      is Boolean range FALSE .. FALSE;

   -- Language Development Options
   subtype Realtime_Lang_c89   is Boolean range TRUE .. TRUE;
   subtype Realtime_Lang_Ada95 is Boolean range TRUE .. TRUE;
   subtype Realtime_Lang_F77   is Boolean range FALSE .. FALSE;

end Posix_Profiles;
