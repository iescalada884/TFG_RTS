------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                             S Y S T E M . I N I T                        --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--          Copyright (C) 2003-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a bare board implementation of this package

with System.BB.CPU_Primitives;
with System.Tasking;

package body System.Init is

   ------------------------
   --  Local procedures  --
   ------------------------

   --  These procedures are called by the binder.

   procedure Initialize;
   pragma Export (C, Initialize, "__gnat_initialize");

   procedure Finalize;
   pragma Export (C, Finalize, "__gnat_finalize");

   E_Exeptions_Elab_Spec : Short_Integer;
   pragma Import (Ada, E_Exeptions_Elab_Spec, "ada__exceptions_E");

   E_Soft_tLinnks : Short_Integer;
   pragma Import (Ada, E_Soft_tLinnks, "system__soft_links_E");

   Sysrem_os_lib : Short_Integer;
   pragma Import (Ada, Sysrem_os_lib, "system__os_lib_E");
   System_Exception_Table_Elab_Spec : Short_Integer;
   pragma Import (Ada, System_Exception_Table_Elab_Spec,
   "system__exception_table_E");
   Ada_Containers_Elab_Spec      : Short_Integer;
   pragma Import (Ada, Ada_Containers_Elab_Spec, "ada__containers_E");
   Ada_IO_Exceptions_Elab_Spec   : Short_Integer;
   pragma Import (Ada, Ada_IO_Exceptions_Elab_Spec, "ada__io_exceptions_E");
   Ada_Numerics_Elab_Spec        : Short_Integer;
   pragma Import (Ada, Ada_Numerics_Elab_Spec, "ada__numerics_E");
   Ada_Strings_Elab_Spec         : Short_Integer;
   pragma Import (Ada, Ada_Strings_Elab_Spec, "ada__strings_E");
   Ada_Strings_Maps_Elab_Spec    : Short_Integer;
   pragma Import (Ada, Ada_Strings_Maps_Elab_Spec,
   "ada__strings__maps_E");
   Ada_Strings_Maps_Constants_Elab_Spec : Short_Integer;
   pragma Import (Ada, Ada_Strings_Maps_Constants_Elab_Spec,
   "ada__strings__maps__constants_E");
   Ada_Strings_UTF_Encoding_Elab_Spec : Short_Integer;
   pragma Import (Ada, Ada_Strings_UTF_Encoding_Elab_Spec,
   "ada__strings__utf_encoding_E");
   Ada_Tags_Elab_Spec            : Short_Integer;
   pragma Import (Ada, Ada_Tags_Elab_Spec, "ada__tags_E");
   Ada_Strings_Text_Buffers_Elab_Spec : Short_Integer;
   pragma Import (Ada, Ada_Strings_Text_Buffers_Elab_Spec,
   "ada__strings__text_buffers_E");
   GNAT_Elab_Spec                : Short_Integer;
   pragma Import (Ada, GNAT_Elab_Spec, "gnat_E");
   Interfaces_C_Elab_Spec        : Short_Integer;
   pragma Import (Ada, Interfaces_C_Elab_Spec, "interfaces__c_E");
   System_Exceptions_Elab_Spec   : Short_Integer;
   pragma Import (Ada, System_Exceptions_Elab_Spec, "system__exceptions_E");
   System_BB_Timing_Events_Elab_Spec : Short_Integer;
   pragma Import (Ada, System_BB_Timing_Events_Elab_Spec,
   "system__bb__timing_events_E");
   System_Object_Reader_Elab_Spec : Short_Integer;
   pragma Import (Ada, System_Object_Reader_Elab_Spec,
   "system__object_reader_E");
   System_Dwarf_Lines_Elab_Spec  : Short_Integer;
   pragma Import (Ada, System_Dwarf_Lines_Elab_Spec, "system__dwarf_lines_E");
   System_Traceback_Symbolic_Elab_Spec : Short_Integer;
   pragma Import (Ada, System_Traceback_Symbolic_Elab_Spec,
   "system__traceback__symbolic_E");

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler is
   begin
      BB.CPU_Primitives.Install_Error_Handlers;
   end Install_Handler;

   ------------------------
   -- Runtime_Initialize --
   ------------------------

   procedure Runtime_Initialize (Install_Handler : Integer) is

   begin
      --  Ensure that the tasking run time is initialized when using this run
      --  time. This initialization is required by the support for exceptions
      --  (which uses thread local storage). The initialization routine has the
      --  required machinery to prevent multiple calls to Initialize.

      System.Tasking.Initialize;

      if Install_Handler = 1 then
         --  Init.Install_Handler;
         E_Exeptions_Elab_Spec := 1; -- avoid check
      end if;

      --  Avoid Elab_Spec errors
      E_Exeptions_Elab_Spec := 1;
      E_Soft_tLinnks := 1;
      Sysrem_os_lib := 1;
      System_Exception_Table_Elab_Spec := 1;
      Ada_Containers_Elab_Spec := 1;
      Ada_IO_Exceptions_Elab_Spec := 1;
      Ada_Numerics_Elab_Spec := 1;
      Ada_Strings_Elab_Spec := 1;
      Ada_Strings_Maps_Elab_Spec := 1;
      Ada_Strings_Maps_Constants_Elab_Spec := 1;
      Ada_Strings_UTF_Encoding_Elab_Spec := 1;
      Ada_Tags_Elab_Spec := 1;
      Ada_Strings_Text_Buffers_Elab_Spec := 1;
      GNAT_Elab_Spec := 1;
      Interfaces_C_Elab_Spec := 1;
      System_Exceptions_Elab_Spec := 1;
      System_BB_Timing_Events_Elab_Spec := 1;
      System_Object_Reader_Elab_Spec := 1;
      System_Dwarf_Lines_Elab_Spec := 1;
      System_Traceback_Symbolic_Elab_Spec := 1;

   end Runtime_Initialize;

   ----------------------
   -- Runtime_Finalize --
   ----------------------

   procedure Runtime_Finalize is
   begin
      null;
   end Runtime_Finalize;

end System.Init;
