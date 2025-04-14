------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'T e x t _ C o n s o l e _ I o c t l'
--
--                                 Spec
--
--
--  File 'text_console_ioctl.ads'                                     By MAR.
--
--  'Ioctl' commands for the text console.
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
-------------------------------------------------------------------------------
with MaRTE.Integer_Types;

package Text_Console_Ioctl is

   ------------------
   --  Ioctl Data  --
   ------------------
   --  Any change here should be also reflected in
   --  'include/drivers/text_console.h'
   type Ioctl_Data is record
      Position  : MaRTE.Integer_Types.Unsigned;
      Text_Attr : MaRTE.Integer_Types.Unsigned_8;
      Char      : Character;
   end record;
   pragma Convention (C, Ioctl_Data);

   ---------------------
   --  Ioctl commads  --
   ---------------------
   SET_CURSOR_POS         : constant := 200;
   ACTIVATE_SCROLL_MODE   : constant := 201;
   DEACTIVATE_SCROLL_MODE : constant := 202;
   SET_TEXT_ATTR          : constant := 203;
   GET_CHAR_IN_SCREEN_POS : constant := 204;
   GET_ATTR_OF_SCREEN_POS : constant := 205;
   CLRSCR                 : constant := 206;
   GET_CURSOR_POS         : constant := 207;

end Text_Console_Ioctl;
