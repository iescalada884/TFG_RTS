package Unimplemented_Functions is

   --  ========================================================================
   --  LOW-LEVEL SYSTEM CALLS (POSIX-like interface)
   --  ========================================================================
   procedure Close;
   procedure Fstat;
   procedure Isatty;
   procedure Lseek;
   procedure Read;
   procedure Write;

   --  ========================================================================
   --  FILE OPERATIONS - Basic File Management
   --  ========================================================================

   --  File Opening and Creation
   procedure Gnat_Fopen;
   procedure Gnat_Freopen;
   procedure Gnat_Open_Append;
   procedure Gnat_Open_Create;
   procedure Gnat_Open_New;
   procedure Gnat_Open_New_Temp;
   procedure Gnat_Open_Read;
   procedure Gnat_Open_RW;

   --  File Attributes and Properties
   procedure Gnat_Copy_Attribs;
   procedure Gnat_File_Exists;
   procedure Gnat_File_Time_Fd;
   procedure Gnat_File_Time_Name;
   procedure Gnat_Fileno;
   procedure Gnat_Is_Executable_File;
   procedure Gnat_Is_Fifo;
   procedure Gnat_Is_Read_Accessible_File;
   procedure Gnat_Is_Readable_File;
   procedure Gnat_Is_Regular_File;
   procedure Gnat_Is_Regular_File_Fd;
   procedure Gnat_Is_Symbolic_Link;
   procedure Gnat_Is_Write_Accessible_File;
   procedure Gnat_Is_Writable_File;

   --  File Operations
   procedure Gnat_Create_Output_File;
   procedure Gnat_Create_Output_File_New;
   procedure Gnat_Rename;
   procedure Gnat_Unlink;

   --  File Modes and I/O Control
   procedure Gnat_Set_Binary_Mode;
   procedure Gnat_Set_Text_Mode;
   procedure Gnat_Text_Translation_Required;

   --  ========================================================================
   --  FILE ERROR HANDLING AND STATUS
   --  ========================================================================
   procedure Gnat_Feof;
   procedure Gnat_Ferror;
   procedure Gnat_Is_File_Not_Found_Error;

   --  ========================================================================
   --  FILE CONSTANTS AND PARAMETERS
   --  ========================================================================
   procedure Gnat_Constant_Eof;
   procedure Gnat_Constant_Iofbf;
   procedure Gnat_Constant_Iolbf;
   procedure Gnat_Constant_Ionbf;
   procedure Gnat_Constant_Seek_End;
   procedure Gnat_Constant_Stderr;
   procedure Gnat_Constant_Stdin;
   procedure Gnat_Constant_Stdout;

   --  ========================================================================
   --  PATH AND DIRECTORY OPERATIONS
   --  ========================================================================
   procedure Gnat_Dir_Separator;
   procedure Gnat_Full_Name;
   procedure Gnat_Get_Current_Dir;
   procedure Gnat_Get_File_Names_Case_Sensitive;
   procedure Gnat_Is_Absolute_Path;
   procedure Gnat_Is_Directory;
   procedure Gnat_Locate_Regular_File;
   procedure Gnat_Max_Path_Len;
   procedure Gnat_Tmp_Name;

   --  ========================================================================
   --  INPUT/OUTPUT OPERATIONS
   --  ========================================================================
   procedure Getc_Immediate;
   procedure Getc_Immediate_Nowait;
   procedure Gnat_Getc_Immediate;
   procedure Gnat_Getc_Immediate_Nowait;

   --  ========================================================================
   --  PROCESS AND SYSTEM CONTROL
   --  ========================================================================

   --  Process Management
   procedure Gnat_Kill;
   procedure Gnat_KillProcessTree;
   procedure Gnat_Os_Exit;
   procedure Gnat_Portable_No_Block_Spawn;
   procedure Gnat_Portable_No_Block_Wait;
   procedure Gnat_Portable_Spawn;
   procedure Gnat_Portable_Wait;

   --  File Permissions and Security
   procedure Gnat_Set_Close_On_Exec;
   procedure Gnat_Set_Executable;
   procedure Gnat_Set_File_Time_Name;
   procedure Gnat_Set_Non_Readable;
   procedure Gnat_Set_Non_Writable;
   procedure Gnat_Set_Readable;
   procedure Gnat_Set_Writable;

   --  ========================================================================
   --  ENVIRONMENT AND RUNTIME VARIABLES
   --  ========================================================================
   procedure Gnat_Argc;
   procedure Gnat_Argv;
   procedure Gnat_Envp;
   procedure Gnat_Exit_Status;
   procedure Gnat_Getenv;
   procedure Gnat_Setenv;

   --  ========================================================================
   --  TIME AND DATE OPERATIONS
   --  ========================================================================
   procedure Gnat_Current_Time_String;
   procedure Gnat_To_Gm_Time;
   procedure Gnat_To_Os_Time;
   procedure Ada_Calendar_UTC_Time_Offset;
   procedure Ada_Calendar_Elabb;
   procedure GL_Leap_Seconds_Support;
   procedure Gnat_Localtime_Tzoff;

   --  ========================================================================
   --  ERROR HANDLING AND DIAGNOSTICS
   --  ========================================================================
   procedure Get_Errno;
   procedure Gnat_Get_Errno;

   --  ========================================================================
   --  UTILITY FUNCTIONS
   --  ========================================================================
   procedure Gnat_Argument_Needs_Quote;
   procedure Gnat_Decode;
   procedure Gnat_Dup;
   procedure Gnat_Dup2;
   procedure Gnat_FillArg;
   procedure Gnat_LenArg;
   procedure Gnat_Readlink;

   --  ========================================================================
   --  GNAT COMPILER SUPPORT
   --  ========================================================================
   procedure Gnat_Get_Debuggable_Suffix_Ptr;
   procedure Gnat_Get_Executable_Suffix_Ptr;
   procedure Gnat_Get_Object_Suffix_Ptr;
   procedure Gnat_Runtime_Finalize;
   procedure Gnat_Runtime_Initialize;
   procedure Gnat_Target_Debuggable_Extension;
   procedure Gnat_Target_Executable_Extension;
   procedure Gnat_Target_Object_Extension;

   --  System initialization and finalization
   procedure Gnat_Initialize;
   procedure Gnat_Finalize;

   --  ========================================================================
   --  DEBUG AND DEVELOPMENT SUPPORT
   --  ========================================================================
   procedure DlAddr;
   procedure Gnat_Bkpt_Trap;
   procedure Gnat_UnhandledTerminate;
   procedure Put_Char;
   procedure Put_Char_Stderr;
   procedure Put_Int;
   procedure Put_Int_Stderr;
   procedure R_Debug;

   --  ========================================================================
   --  ADA RUNTIME GLOBALS (GL_*)
   --  ========================================================================

   --  ARM and Hardware Specific
   procedure Ada_Setup_PLL;
   procedure GL_Main_CPU;

   --  Interrupt Management
   procedure GL_Interrupt_States;
   procedure GL_Num_Interrupt_States;
   procedure GL_Unreserve_All_Interrupts;

   --  Task and Scheduling Policies
   procedure GL_Default_Stack_Size;
   procedure GL_Detect_Blocking;
   procedure GL_Locking_Policy;
   procedure GL_Num_Specific_Dispatching;
   procedure GL_Priority_Specific_Dispatching;
   procedure GL_Queuing_Policy;
   procedure GL_Task_Dispatching_Policy;
   procedure GL_Time_Slice_Val;

   --  Character Encoding and I/O
   procedure GL_WC_Encoding;
   procedure GL_Xdr_Stream;

   --  ========================================================================
   --  PRAGMA EXPORTS - C Interface Declarations
   --  ========================================================================

   --  LOW-LEVEL SYSTEM CALLS
   pragma Export (C, Close, "_close");
   pragma Weak_External (Close);
   pragma Export (C, Fstat, "_fstat");
   pragma Weak_External (Fstat);
   pragma Export (C, Isatty, "_isatty");
   pragma Weak_External (Isatty);
   pragma Export (C, Lseek, "_lseek");
   pragma Weak_External (Lseek);
   pragma Export (C, Read, "_read");
   pragma Weak_External (Read);
   pragma Export (C, Write, "_write");
   pragma Weak_External (Write);

   --  FILE OPERATIONS - Basic File Management
   pragma Export (C, Gnat_Fopen, "__gnat_fopen");
   pragma Export (C, Gnat_Freopen, "__gnat_freopen");
   pragma Export (C, Gnat_Open_Append, "__gnat_open_append");
   pragma Export (C, Gnat_Open_Create, "__gnat_open_create");
   pragma Export (C, Gnat_Open_New, "__gnat_open_new");
   pragma Export (C, Gnat_Open_New_Temp, "__gnat_open_new_temp");
   pragma Export (C, Gnat_Open_Read, "__gnat_open_read");
   pragma Export (C, Gnat_Open_RW, "__gnat_open_rw");

   --  File Attributes and Properties
   pragma Export (C, Gnat_Copy_Attribs, "__gnat_copy_attribs");
   pragma Export (C, Gnat_File_Exists, "__gnat_file_exists");
   pragma Export (C, Gnat_File_Time_Fd, "__gnat_file_time_fd");
   pragma Export (C, Gnat_File_Time_Name, "__gnat_file_time_name");
   pragma Export (C, Gnat_Fileno, "__gnat_fileno");
   pragma Export (C, Gnat_Is_Executable_File, "__gnat_is_executable_file");
   pragma Export (C, Gnat_Is_Fifo, "__gnat_is_fifo");
   pragma Export (C, Gnat_Is_Read_Accessible_File,
                  "__gnat_is_read_accessible_file");
   pragma Export (C, Gnat_Is_Readable_File, "__gnat_is_readable_file");
   pragma Export (C, Gnat_Is_Regular_File, "__gnat_is_regular_file");
   pragma Export (C, Gnat_Is_Regular_File_Fd, "__gnat_is_regular_file_fd");
   pragma Export (C, Gnat_Is_Symbolic_Link, "__gnat_is_symbolic_link");
   pragma Export (C, Gnat_Is_Write_Accessible_File,
                  "__gnat_is_write_accessible_file");
   pragma Export (C, Gnat_Is_Writable_File, "__gnat_is_writable_file");

   --  File Operations
   pragma Export (C, Gnat_Create_Output_File, "__gnat_create_output_file");
   pragma Export (C, Gnat_Create_Output_File_New,
                  "__gnat_create_output_file_new");
   pragma Export (C, Gnat_Rename, "__gnat_rename");
   pragma Export (C, Gnat_Unlink, "__gnat_unlink");

   --  File Modes and I/O Control
   pragma Export (C, Gnat_Set_Binary_Mode, "__gnat_set_binary_mode");
   pragma Export (C, Gnat_Set_Text_Mode, "__gnat_set_text_mode");
   pragma Export (C, Gnat_Text_Translation_Required,
                  "__gnat_text_translation_required");

   --  FILE ERROR HANDLING AND STATUS
   pragma Export (C, Gnat_Feof, "__gnat_feof");
   pragma Export (C, Gnat_Ferror, "__gnat_ferror");
   pragma Export (C, Gnat_Is_File_Not_Found_Error,
                  "__gnat_is_file_not_found_error");

   --  FILE CONSTANTS AND PARAMETERS
   pragma Export (C, Gnat_Constant_Eof, "__gnat_constant_eof");
   pragma Export (C, Gnat_Constant_Iofbf, "__gnat_constant_iofbf");
   pragma Export (C, Gnat_Constant_Iolbf, "__gnat_constant_iolbf");
   pragma Export (C, Gnat_Constant_Ionbf, "__gnat_constant_ionbf");
   pragma Export (C, Gnat_Constant_Seek_End, "__gnat_constant_seek_end");
   pragma Export (C, Gnat_Constant_Stderr, "__gnat_constant_stderr");
   pragma Export (C, Gnat_Constant_Stdin, "__gnat_constant_stdin");
   pragma Export (C, Gnat_Constant_Stdout, "__gnat_constant_stdout");

   --  PATH AND DIRECTORY OPERATIONS
   pragma Export (C, Gnat_Dir_Separator, "__gnat_dir_separator");
   pragma Export (C, Gnat_Full_Name, "__gnat_full_name");
   pragma Export (C, Gnat_Get_Current_Dir, "__gnat_get_current_dir");
   pragma Export (C, Gnat_Get_File_Names_Case_Sensitive,
                  "__gnat_get_file_names_case_sensitive");
   pragma Export (C, Gnat_Is_Absolute_Path, "__gnat_is_absolute_path");
   pragma Export (C, Gnat_Is_Directory, "__gnat_is_directory");
   pragma Export (C, Gnat_Locate_Regular_File, "__gnat_locate_regular_file");
   pragma Export (C, Gnat_Max_Path_Len, "__gnat_max_path_len");
   pragma Export (C, Gnat_Tmp_Name, "__gnat_tmp_name");

   --  INPUT/OUTPUT OPERATIONS
   pragma Export (C, Getc_Immediate, "getc_immediate");
   pragma Export (C, Getc_Immediate_Nowait, "getc_immediate_nowait");
   pragma Export (C, Gnat_Getc_Immediate, "__gnat_getc_immediate");
   pragma Export (C, Gnat_Getc_Immediate_Nowait,
                  "__gnat_getc_immediate_nowait");

   --  PROCESS AND SYSTEM CONTROL
   pragma Export (C, Gnat_Kill, "__gnat_kill");
   pragma Export (C, Gnat_KillProcessTree, "__gnat_killprocesstree");
   pragma Export (C, Gnat_Os_Exit, "__gnat_os_exit");
   pragma Export (C, Gnat_Portable_No_Block_Spawn,
                  "__gnat_portable_no_block_spawn");
   pragma Export (C, Gnat_Portable_No_Block_Wait,
                  "__gnat_portable_no_block_wait");
   pragma Export (C, Gnat_Portable_Spawn, "__gnat_portable_spawn");
   pragma Export (C, Gnat_Portable_Wait, "__gnat_portable_wait");

   --  File Permissions and Security
   pragma Export (C, Gnat_Set_Close_On_Exec, "__gnat_set_close_on_exec");
   pragma Export (C, Gnat_Set_Executable, "__gnat_set_executable");
   pragma Export (C, Gnat_Set_File_Time_Name, "__gnat_set_file_time_name");
   pragma Export (C, Gnat_Set_Non_Readable, "__gnat_set_non_readable");
   pragma Export (C, Gnat_Set_Non_Writable, "__gnat_set_non_writable");
   pragma Export (C, Gnat_Set_Readable, "__gnat_set_readable");
   pragma Export (C, Gnat_Set_Writable, "__gnat_set_writable");

   --  ENVIRONMENT AND RUNTIME VARIABLES
   pragma Export (C, Gnat_Argc, "gnat_argc");
   pragma Export (C, Gnat_Argv, "gnat_argv");
   pragma Export (C, Gnat_Envp, "gnat_envp");
   pragma Export (C, Gnat_Exit_Status, "gnat_exit_status");
   pragma Export (C, Gnat_Getenv, "__gnat_getenv");
   pragma Export (C, Gnat_Setenv, "__gnat_setenv");

   --  TIME AND DATE OPERATIONS
   pragma Export (C, Gnat_Current_Time_String, "__gnat_current_time_string");
   pragma Export (C, Gnat_To_Gm_Time, "__gnat_to_gm_time");
   pragma Export (C, Gnat_To_Os_Time, "__gnat_to_os_time");

   pragma Export (C, Ada_Calendar_UTC_Time_Offset,
                  "__ada_calendar_utc_time_offset");
   pragma Export (C, Ada_Calendar_Elabb, "__ada_calendar__elabb");
   pragma Export (C, GL_Leap_Seconds_Support, "__gl_leap_seconds_support");
   pragma Export (C, Gnat_Localtime_Tzoff, "__gnat_localtime_tzoff");

   --  ERROR HANDLING AND DIAGNOSTICS
   pragma Export (C, Get_Errno, "__get_errno");
   pragma Export (C, Gnat_Get_Errno, "__gnat_get_errno");

   --  UTILITY FUNCTIONS
   pragma Export (C, Gnat_Argument_Needs_Quote, "__gnat_argument_needs_quote");
   pragma Export (C, Gnat_Decode, "__gnat_decode");
   pragma Export (C, Gnat_Dup, "__gnat_dup");
   pragma Export (C, Gnat_Dup2, "__gnat_dup2");
   pragma Export (C, Gnat_FillArg, "__gnat_fill_arg");
   pragma Export (C, Gnat_LenArg, "__gnat_len_arg");
   pragma Export (C, Gnat_Readlink, "__gnat_readlink");

   --  GNAT COMPILER SUPPORT
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

   --  DEBUG AND DEVELOPMENT SUPPORT
   pragma Export (C, DlAddr, "dladdr");
   pragma Export (C, Gnat_Bkpt_Trap, "__gnat_bkpt_trap");
   pragma Export (C, Gnat_UnhandledTerminate,
                  "__gnat_unhandled_terminate");
   pragma Export (C, Put_Char, "put_char");
   pragma Export (C, Put_Char_Stderr, "put_char_stderr");
   pragma Export (C, Put_Int, "put_int");
   pragma Export (C, Put_Int_Stderr, "put_int_stderr");
   pragma Export (C, R_Debug, "_r_debug");

   --  ADA RUNTIME GLOBALS (GL_*)
   pragma Export (C, Ada_Setup_PLL, "_ada_setup_pll");
   pragma Export (C, GL_Main_CPU, "__gl_main_cpu");
   pragma Export (C, GL_Interrupt_States, "__gl_interrupt_states");
   pragma Export (C, GL_Num_Interrupt_States, "__gl_num_interrupt_states");
   pragma Export (C, GL_Unreserve_All_Interrupts,
                  "__gl_unreserve_all_interrupts");
   pragma Export (C, GL_Default_Stack_Size, "__gl_default_stack_size");
   pragma Export (C, GL_Detect_Blocking, "__gl_detect_blocking");
   pragma Export (C, GL_Locking_Policy, "__gl_locking_policy");
   pragma Export (C, GL_Num_Specific_Dispatching,
                  "__gl_num_specific_dispatching");
   pragma Export (C, GL_Priority_Specific_Dispatching,
                  "__gl_priority_specific_dispatching");
   pragma Export (C, GL_Queuing_Policy, "__gl_queuing_policy");
   pragma Export (C, GL_Task_Dispatching_Policy,
                  "__gl_task_dispatching_policy");
   pragma Export (C, GL_Time_Slice_Val, "__gl_time_slice_val");
   pragma Export (C, GL_WC_Encoding, "__gl_wc_encoding");
   pragma Export (C, GL_Xdr_Stream, "__gl_xdr_stream");

end Unimplemented_Functions;