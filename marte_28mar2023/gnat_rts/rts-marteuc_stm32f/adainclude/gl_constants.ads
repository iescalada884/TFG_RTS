package GL_Constants is
   Main_Priority : constant Integer := 10;
   pragma Export (C, Main_Priority, "__gl_main_priority");

   Secondary_Priority : constant Integer := 5;
   pragma Export (C, Secondary_Priority, "__gl_secondary_priority");

   Max_Task_Count : constant Integer := 100;
   pragma Export (C, Max_Task_Count, "__gl_max_task_count");

   Gnat_File_Length_Long : constant Integer := 0;
   pragma Export (C, Gnat_File_Length_Long, "__gnat_file_length_long");

   Gnat_Lseek : constant Integer := 0;
   pragma Export (C, Gnat_Lseek, "__gnat_lseek");

   Getpagesize : constant Integer := 0;
   pragma Export (C, Getpagesize, "getpagesize");

   Mmap : constant Integer := 0;
   pragma Export (C, Mmap, "mmap");

   Munmap : constant Integer := 0;
   pragma Export (C, Munmap, "munmap");

   System_Text_IO_Is_Tx_Ready : constant Integer := 0;
   pragma Export (C, System_Text_IO_Is_Tx_Ready, "system__text_io__is_tx_rd");

   System_Text_IO_Put : constant Integer := 0;
   pragma Export (C, System_Text_IO_Put, "system__text_io__put");

   System_Text_IO_Initialize : constant Integer := 0;
   pragma Export (C, System_Text_IO_Initialize, "system__text_io__initialize");

   System_Text_IO_Use_Cr_Lf_For_New_Line : constant Integer := 0;
   pragma Export (C, System_Text_IO_Use_Cr_Lf_For_New_Line,
                  "system__text_io__use_cr_lf_for_new_line");

   System_Text_IO_Initialized : constant Integer := 0;
   pragma Export (C, System_Text_IO_Initialized, "system__text_io__init");

   Gnat_Personality_V0 : constant Integer := 0;
   pragma Export (C, Gnat_Personality_V0, "__gnat_personality_v0");

   Close_Constant : constant Integer := 0;
   pragma Export (C, Close_Constant, "_close");

   Isatty : constant Integer := 0;
   pragma Export (C, Isatty, "_isatty");

   Lseek : constant Integer := 0;
   pragma Export (C, Lseek, "_lseek");

   Fstat : constant Integer := 0;
   pragma Export (C, Fstat, "_fstat");

   Read : constant Integer := 0;
   pragma Export (C, Read, "_read");

   Exit_Constant : constant Integer := 0;
   pragma Export (C, Exit_Constant, "_exit");

   Write : constant Integer := 0;
   pragma Export (C, Write, "_write");

   Sbrk : constant Integer := 0;
   pragma Export (C, Sbrk, "_sbrk");

   Gnat_Unwind_RaiseException : constant Integer := 0;
   pragma Export (C, Gnat_Unwind_RaiseException,
                  "__gnat_Unwind_RaiseException");

   Gnat_Unwind_ForcedUnwind : constant Integer := 0;
   pragma Export (C, Gnat_Unwind_ForcedUnwind, "__gnat_Unwind_ForcedUnwind");

   Gnat_Cleanupunwind_Handler : constant Integer := 0;
   pragma Export (C, Gnat_Cleanupunwind_Handler,
                  "__gnat_cleanupunwind_handler");

   Gnat_Exception_Tracebacks : constant Integer := 0;
   pragma Export (C, Gnat_Exception_Tracebacks,
                  "__gl_exception_tracebacks");

   Gnat_Exception_Tracebacks_Symbolic : constant Integer := 0;
   pragma Export (C, Gnat_Exception_Tracebacks_Symbolic,
                  "__gl_exception_tracebacks_symbolic");

   Gnat_Backtrace : constant Integer := 0;
   pragma Export (C, Gnat_Backtrace, "__gnat_backtrace");

   Gnat_Locate_Exec_On_Path : constant Integer := 0;
   pragma Export (C, Gnat_Locate_Exec_On_Path, "__gnat_locate_exec_on_path");

   Gnat_Get_Executable_Load_Address : constant Integer := 0;
   pragma Export (C, Gnat_Get_Executable_Load_Address,
                  "__gnat_get_executable_load_address");

   Gnat_Argv : constant Integer := 0;
   pragma Export (C, Gnat_Argv, "gnat_argv");

   Gnat_Initialize : constant Integer := 0;
   pragma Export (C, Gnat_Initialize, "__gnat_initialize");

   Gnat_Finalize : constant Integer := 0;
   pragma Export (C, Gnat_Finalize, "__gnat_finalize");

   Gnat_Argc : constant Integer := 0;
   pragma Export (C, Gnat_Argc, "gnat_argc");

   Gnat_Envp : constant Integer := 0;
   pragma Export (C, Gnat_Envp, "gnat_envp");

   Gnat_Exit_Status : constant Integer := 0;
   pragma Export (C, Gnat_Exit_Status, "gnat_exit_status");

   Gnat_Decode : constant Integer := 0;
   pragma Export (C, Gnat_Decode, "__gnat_decode");

   Gnat_Fileno : constant Integer := 0;
   pragma Export (C, Gnat_Fileno, "__gnat_fileno");

   Gnat_Set_Binary_Mode : constant Integer := 0;
   pragma Export (C, Gnat_Set_Binary_Mode, "__gnat_set_binary_mode");

   Gnat_Ferror : constant Integer := 0;
   pragma Export (C, Gnat_Ferror, "__gnat_ferror");

   Gnat_Set_Text_Mode : constant Integer := 0;
   pragma Export (C, Gnat_Set_Text_Mode, "__gnat_set_text_mode");

   Gnat_Text_Translation_Required : constant Integer := 0;
   pragma Export (C, Gnat_Text_Translation_Required,
                  "__gnat_text_translation_required");

   Gnat_Constant_Eof : constant Integer := 0;
   pragma Export (C, Gnat_Constant_Eof, "__gnat_constant_eof");

   Gnat_Constant_Stderr : constant Integer := 0;
   pragma Export (C, Gnat_Constant_Stderr, "__gnat_constant_stderr");

   Gnat_Is_Regular_File_Fd : constant Integer := 0;
   pragma Export (C, Gnat_Is_Regular_File_Fd, "__gnat_is_regular_file_fd");

   Gnat_Constant_Stdin : constant Integer := 0;
   pragma Export (C, Gnat_Constant_Stdin, "__gnat_constant_stdin");

   Gnat_Constant_Stdout : constant Integer := 0;
   pragma Export (C, Gnat_Constant_Stdout, "__gnat_constant_stdout");

   Gnat_Wc_Encoding : constant Integer := 0;
   pragma Export (C, Gnat_Wc_Encoding, "__gl_wc_encoding");

   Gnat_Xdr_Stream : constant Integer := 0;
   pragma Export (C, Gnat_Xdr_Stream, "__gl_xdr_stream");

   Time_Slice_Val : constant Integer := 0;
   pragma Export (C, Time_Slice_Val, "__gl_time_slice_val");

   Locking_Policy : constant Integer := 0;
   pragma Export (C, Locking_Policy, "__gl_locking_policy");

   Queuing_Policy : constant Integer := 0;
   pragma Export (C, Queuing_Policy, "__gl_queuing_policy");

   Task_Dispatching_Policy : constant Integer := 0;
   pragma Export (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");

   Priority_Specific_Dispatching : constant Integer := 0;
   pragma Export (C, Priority_Specific_Dispatching,
                  "__gl_priority_specific_dispatching");

   Num_Specific_Dispatching : constant Integer := 0;
   pragma Export (C, Num_Specific_Dispatching,
                  "__gl_num_specific_dispatching");

   Main_CPU : constant Integer := 0;
   pragma Export (C, Main_CPU, "__gl_main_cpu");

   Interrupt_States : constant Integer := 0;
   pragma Export (C, Interrupt_States, "__gl_interrupt_states");

   Num_Interrupt_States : constant Integer := 0;
   pragma Export (C, Num_Interrupt_States, "__gl_num_interrupt_states");

   Unreserve_All_Interrupts : constant Integer := 0;
   pragma Export (C, Unreserve_All_Interrupts,
                  "__gl_unreserve_all_interrupts");

   Detect_Blocking : constant Integer := 0;
   pragma Export (C, Detect_Blocking, "__gl_detect_blocking");

   Default_Stack_Size : constant Integer := 0;
   pragma Export (C, Default_Stack_Size, "__gl_default_stack_size");
end GL_Constants;