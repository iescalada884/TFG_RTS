----------------------------------------------------------------------------
----------------------        M a R T E   O S          ---------------------
----------------------------------------------------------------------------
--                                                         V2.0 2019-05-24
--
--                            'P O S I X - F I L E S'
--
--                                  Spec
--
--
--  File 'posix-files.ads'                               By Sangorrin
--
--
--  Package 'POSIX_Files' as defined in IEEE Std 1003.5b-1996.
--
--  MaRTE OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
--
--  This package calls directly the operations defined in
--  'Kernel.File_System', but taking into account the necessary
--  treatment of the signals reserved for Gnat run time use.
--
--  This file is based on the Florist implementation
--  (http://www.cs.fsu.edu/~baker/florist.html)
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

with POSIX;
-- with POSIX,
--      POSIX.C,
--      POSIX.Permissions,
--      POSIX.Process_Identification,
--      POSIX.Calendar;
package POSIX.Files is

   --  Operations to Create Files in the File System

   --    procedure Create_Directory
   --      (Pathname   : in POSIX.Pathname;
   --       Permission : in POSIX.Permissions.Permission_Set);
   --    procedure Create_FIFO
   --      (Pathname   : in POSIX.Pathname;
   --       Permission : in POSIX.Permissions.Permission_Set);

   --  Operations to remove files from the File System

   procedure Unlink (Pathname : in POSIX.Pathname);
   --    procedure Remove_Directory (Pathname : in POSIX.Pathname);

   --  Predicates on files in the File System

   --    function Is_Block_Special_File (Pathname : POSIX.Pathname)
   --       return Boolean;
   --    function Is_Character_Special_File (Pathname : POSIX.Pathname)
   --       return Boolean;
   --    function Is_Directory (Pathname : POSIX.Pathname) return Boolean;
   --    function Is_FIFO (Pathname : POSIX.Pathname) return Boolean;
   --    --  Is_Symbolic_Link is not in the IEEE standard
   --    function Is_Symbolic_Link (Pathname : POSIX.Pathname) return Boolean;
   --    --  .... Change POSIX.5?
   --    --  Why is this not called Is_Regular_File?  Add renaming decl?
   --    function Is_File (Pathname : POSIX.Pathname) return Boolean;
   --    --  Is_Socket is from POSIX.5c [D2]
   --    function Is_Socket (Pathname : POSIX.Pathname) return Boolean;
   --
   --    --  Operations to modify File Pathnames
   --
   --    procedure Link
   --      (Old_Pathname : in POSIX.Pathname;
   --       New_Pathname : in POSIX.Pathname);
   --    procedure Rename
   --      (Old_Pathname : in POSIX.Pathname;
   --       New_Pathname : in POSIX.Pathname);
   --
   --    --  Iterating over files within a directory
   --
   --    type Directory_Entry is limited private;
   --    function Filename_Of (D_Entry : Directory_Entry) return POSIX.Filename;
   --    pragma Inline (Filename_Of);
   --    generic
   --    with procedure Action
   --      (D_Entry : in Directory_Entry;
   --       Quit    : in out Boolean);
   --    procedure For_Every_Directory_Entry
   --       (Pathname : in POSIX.Pathname);
   --
   --    --  Operations to Update File Status Information
   --
   --    procedure Change_Owner_And_Group
   --      (Pathname : in POSIX.Pathname;
   --       Owner    : in POSIX.Process_Identification.User_ID;
   --       Group    : in POSIX.Process_Identification.Group_ID);
   --    procedure Change_Permissions
   --      (Pathname   : in POSIX.Pathname;
   --       Permission : in POSIX.Permissions.Permission_Set);
   --    procedure Set_File_Times
   --      (Pathname          : in POSIX.Pathname;
   --       Access_Time       : in POSIX.Calendar.POSIX_Time;
   --       Modification_Time : in POSIX.Calendar.POSIX_Time);
   --    procedure Set_File_Times (Pathname : in POSIX.Pathname);
   --
   --    --  Operations to Determine File Accessibility
   --
   --    type Access_Mode is (Read_Ok, Write_Ok, Execute_Ok);
   --    type Access_Mode_Set is array (Access_Mode) of Boolean;
   --    function Is_Accessible
   --      (Pathname     : POSIX.Pathname;
   --       Access_Modes : Access_Mode_Set) return Boolean;
   --    function Accessibility
   --      (Pathname     : POSIX.Pathname;
   --       Access_Modes : Access_Mode_Set) return POSIX.Error_Code;
   --    function Is_File_Present (Pathname : POSIX.Pathname)
   --       return Boolean;
   --    function Existence (Pathname : POSIX.Pathname)
   --       return POSIX.Error_Code;
   --
   -- private
   --    type Directory_Entry is new POSIX.C.dirent_ptr;
end POSIX.Files;
