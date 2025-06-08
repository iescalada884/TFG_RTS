package Unimplemented_Functions is

   --  File Operations
   procedure Gnat_Open_Read;
   procedure Gnat_Open_Append;
   procedure Gnat_Open_RW;
   procedure Gnat_Open_Create;
   procedure Gnat_Open_New;
   procedure Gnat_Open_New_Temp;
   procedure Gnat_File_Time_Fd;
   procedure Gnat_File_Time_Name;
   procedure Gnat_Rename;
   procedure Gnat_Is_Absolute_Path;
   procedure Gnat_Is_Directory;
   procedure Gnat_Is_Read_Accessible_File;
   procedure Gnat_Is_Readable_File;
   procedure Gnat_Is_Executable_File;
   procedure Gnat_Is_Regular_File;
   procedure Gnat_Is_Symbolic_Link;
   procedure Gnat_Is_Write_Accessible_File;
   procedure Gnat_Is_Writable_File;
   procedure Gnat_Create_Output_File_New;
   procedure Gnat_Copy_Attribs;
   procedure Gnat_Locate_Regular_File;

   --  System Operations
   procedure Gnat_Kill;
   procedure Gnat_KillProcessTree;
   procedure Gnat_Set_Close_On_Exec;
   procedure Gnat_Set_Executable;
   procedure Gnat_Set_File_Time_Name;
   procedure Gnat_Set_Non_Readable;
   procedure Gnat_Set_Non_Writable;
   procedure Gnat_Set_Readable;
   procedure Gnat_Set_Writable;
   procedure Gnat_Setenv;
   procedure Gnat_Portable_Wait;
   procedure Gnat_Portable_No_Block_Wait;
   procedure Gnat_Portable_No_Block_Spawn;
   procedure Gnat_Portable_Spawn;

   --  GNAT Runtime
   procedure Gnat_Get_Debuggable_Suffix_Ptr;
   procedure Gnat_Get_Executable_Suffix_Ptr;
   procedure Gnat_Get_Object_Suffix_Ptr;
   procedure Gnat_Target_Debuggable_Extension;
   procedure Gnat_Target_Executable_Extension;
   procedure Gnat_Target_Object_Extension;
   procedure Gnat_Getenv;
   procedure Gnat_To_Gm_Time;
   procedure Gnat_To_Os_Time;

   --  Debugging and Utilities
   procedure DlAddr;
   procedure R_Debug;
   procedure Gnat_LenArg;
   procedure Gnat_FillArg;
   procedure Gnat_UnhandledTerminate;
   procedure Put_Char;
   procedure Put_Char_Stderr;
   procedure Put_Int;
   procedure Put_Int_Stderr;
   procedure Gnat_Dup;
   procedure Gnat_Dup2;
   procedure Gnat_Readlink;
   procedure Gnat_Max_Path_Len;
   procedure Gnat_Dir_Separator;
   procedure Gnat_Argument_Needs_Quote;

   --  File Operations
   pragma Export (C, Gnat_Open_Read, "__gnat_open_read");
   pragma Export (C, Gnat_Open_Append, "__gnat_open_append");
   pragma Export (C, Gnat_Open_RW, "__gnat_open_rw");
   pragma Export (C, Gnat_Open_Create, "__gnat_open_create");
   pragma Export (C, Gnat_Open_New, "__gnat_open_new");
   pragma Export (C, Gnat_Open_New_Temp, "__gnat_open_new_temp");
   pragma Export (C, Gnat_File_Time_Fd, "__gnat_file_time_fd");
   pragma Export (C, Gnat_File_Time_Name, "__gnat_file_time_name");
   pragma Export (C, Gnat_Rename, "__gnat_rename");
   pragma Export (C, Gnat_Is_Absolute_Path, "__gnat_is_absolute_path");
   pragma Export (C, Gnat_Is_Directory, "__gnat_is_directory");

   pragma Export (C, Gnat_Is_Read_Accessible_File,
                  "__gnat_is_read_accessible_file");

   pragma Export (C, Gnat_Is_Readable_File, "__gnat_is_readable_file");
   pragma Export (C, Gnat_Is_Executable_File, "__gnat_is_executable_file");
   pragma Export (C, Gnat_Is_Regular_File, "__gnat_is_regular_file");
   pragma Export (C, Gnat_Is_Symbolic_Link, "__gnat_is_symbolic_link");

   pragma Export (C,
                  Gnat_Is_Write_Accessible_File,
                  "__gnat_is_write_accessible_file");

   pragma Export (C, Gnat_Is_Writable_File, "__gnat_is_writable_file");
   pragma Export (C, Gnat_Create_Output_File_New,
                  "__gnat_create_output_file_new");
   pragma Export (C, Gnat_Copy_Attribs, "__gnat_copy_attribs");
   pragma Export (C, Gnat_Locate_Regular_File, "__gnat_locate_regular_file");

   --  System Operations
   pragma Export (C, Gnat_Kill, "__gnat_kill");
   pragma Export (C, Gnat_KillProcessTree, "__gnat_killprocesstree");
   pragma Export (C, Gnat_Set_Close_On_Exec, "__gnat_set_close_on_exec");
   pragma Export (C, Gnat_Set_Executable, "__gnat_set_executable");
   pragma Export (C, Gnat_Set_File_Time_Name, "__gnat_set_file_time_name");
   pragma Export (C, Gnat_Set_Non_Readable, "__gnat_set_non_readable");
   pragma Export (C, Gnat_Set_Non_Writable, "__gnat_set_non_writable");
   pragma Export (C, Gnat_Set_Readable, "__gnat_set_readable");
   pragma Export (C, Gnat_Set_Writable, "__gnat_set_writable");
   pragma Export (C, Gnat_Setenv, "__gnat_setenv");
   pragma Export (C, Gnat_Portable_Wait, "__gnat_portable_wait");
   pragma Export (C, Gnat_Portable_No_Block_Wait,
                  "__gnat_portable_no_block_wait");
   pragma Export (C, Gnat_Portable_No_Block_Spawn,
                  "__gnat_portable_no_block_spawn");
   pragma Export (C, Gnat_Portable_Spawn, "__gnat_portable_spawn");

   --  GNAT Runtime
   pragma Export (C, Gnat_Get_Debuggable_Suffix_Ptr,
                  "__gnat_get_debuggable_suffix_ptr");

   pragma Export (C, Gnat_Get_Executable_Suffix_Ptr,
                  "__gnat_get_executable_suffix_ptr");

   pragma Export (C, Gnat_Get_Object_Suffix_Ptr,
                  "__gnat_get_object_suffix_ptr");

   pragma Export (C, Gnat_Target_Debuggable_Extension,
                  "__gnat_target_debuggable_extension");

   pragma Export (C, Gnat_Target_Executable_Extension,
                  "__gnat_target_executable_extension");

   pragma Export (C, Gnat_Target_Object_Extension,
                  "__gnat_target_object_extension");

   pragma Export (C, Gnat_Getenv, "__gnat_getenv");
   pragma Export (C, Gnat_To_Gm_Time, "__gnat_to_gm_time");
   pragma Export (C, Gnat_To_Os_Time, "__gnat_to_os_time");

   --  Debugging and Utilities
   pragma Export (C, DlAddr, "dladdr");
   pragma Export (C, R_Debug, "_r_debug");
   pragma Export (C, Gnat_LenArg, "__gnat_len_arg");
   pragma Export (C, Gnat_FillArg, "__gnat_fill_arg");
   pragma Export (C, Gnat_UnhandledTerminate, "__gnat_unhandled_terminate");
   pragma Export (C, Put_Char, "put_char");
   pragma Export (C, Put_Char_Stderr, "put_char_stderr");
   pragma Export (C, Put_Int, "put_int");
   pragma Export (C, Put_Int_Stderr, "put_int_stderr");
   pragma Export (C, Gnat_Dup, "__gnat_dup");
   pragma Export (C, Gnat_Dup2, "__gnat_dup2");
   pragma Export (C, Gnat_Readlink, "__gnat_readlink");
   pragma Export (C, Gnat_Max_Path_Len, "__gnat_max_path_len");
   pragma Export (C, Gnat_Dir_Separator, "__gnat_dir_separator");
   pragma Export (C, Gnat_Argument_Needs_Quote, "__gnat_argument_needs_quote");

end Unimplemented_Functions;