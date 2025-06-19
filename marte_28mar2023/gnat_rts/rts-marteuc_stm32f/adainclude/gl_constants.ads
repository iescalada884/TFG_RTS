package GL_Constants is
   --  Task-related constants

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

   Gnat_Backtrace : constant Integer := 0;
   pragma Export (C, Gnat_Backtrace, "__gnat_backtrace");

   Gnat_Locate_Exec_On_Path : constant Integer := 0;
   pragma Export (C, Gnat_Locate_Exec_On_Path, "__gnat_locate_exec_on_path");

   Gnat_Get_Executable_Load_Address : constant Integer := 0;
   pragma Export (C, Gnat_Get_Executable_Load_Address,
                  "__gnat_get_executable_load_address");

end GL_Constants;