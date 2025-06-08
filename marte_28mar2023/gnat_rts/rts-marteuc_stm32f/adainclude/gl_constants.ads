package GL_Constants is
   --  Task-related constants

   Main_Priority : constant Integer := 10;
   pragma Export (C, Main_Priority, "__gl_main_priority");

   Secondary_Priority : constant Integer := 5;
   pragma Export (C, Secondary_Priority, "__gl_secondary_priority");

   Max_Task_Count : constant Integer := 100;
   pragma Export (C, Max_Task_Count, "__gl_max_task_count");

   --  File-related constants
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

   --  System Text IO constants
   System_Text_IO_Is_Tx_Ready : constant Integer := 0;
   pragma Export (C, System_Text_IO_Is_Tx_Ready,
                  "system__text_io__is_tx_ready");

   System_Text_IO_Put : constant Integer := 0;
   pragma Export (C, System_Text_IO_Put, "system__text_io__put");

   System_Text_IO_Initialize : constant Integer := 0;
   pragma Export (C, System_Text_IO_Initialize, "system__text_io__initialize");

   System_Text_IO_Use_Cr_Lf_For_New_Line : constant Integer := 0;
   pragma Export (C, System_Text_IO_Use_Cr_Lf_For_New_Line,
                  "system__text_io__use_cr_lf_for_new_line");

   System_Text_IO_Initialized : constant Integer := 0;
   pragma Export (C, System_Text_IO_Initialized,
                  "system__text_io__initialized");

   --  GNAT runtime constants
   Gnat_Personality_V0 : constant Integer := 0;
   pragma Export (C, Gnat_Personality_V0, "__gnat_personality_v0");

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

   --  GNAT initialization and finalization constants
   Gnat_Initialize : constant Integer := 0;
   pragma Export (C, Gnat_Initialize, "__gnat_initialize");

   Gnat_Finalize : constant Integer := 0;
   pragma Export (C, Gnat_Finalize, "__gnat_finalize");

   Gnat_Runtime_Initialize : constant Integer := 0;
   pragma Export (C, Gnat_Runtime_Initialize, "__gnat_runtime_initialize");

   Gnat_Runtime_Finalize : constant Integer := 0;
   pragma Export (C, Gnat_Runtime_Finalize, "__gnat_runtime_finalize");

   --  MaRTE OS constants
   Marte_Init : constant Integer := 0;
   pragma Export (C, Marte_Init, "marte_init");
end GL_Constants;