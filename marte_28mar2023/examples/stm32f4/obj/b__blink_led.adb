pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__blink_led.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__blink_led.adb");
pragma Suppress (Overflow_Check);

package body ada_main is

   E086 : Short_Integer; pragma Import (Ada, E086, "system__os_lib_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "ada__exceptions_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__soft_links_E");
   E039 : Short_Integer; pragma Import (Ada, E039, "system__exception_table_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "ada__containers_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "ada__io_exceptions_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "ada__numerics_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__strings_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "ada__strings__maps_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "ada__strings__maps__constants_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "interfaces__c_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "system__exceptions_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "system__object_reader_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "system__dwarf_lines_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "system__soft_links__initialize_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "system__traceback__symbolic_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "leds_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := null;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E039 := E039 + 1;
      Ada.Containers'Elab_Spec;
      E054 := E054 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E081 := E081 + 1;
      Ada.Numerics'Elab_Spec;
      E046 := E046 + 1;
      Ada.Strings'Elab_Spec;
      E068 := E068 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E070 := E070 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E073 := E073 + 1;
      Interfaces.C'Elab_Spec;
      E058 := E058 + 1;
      System.Exceptions'Elab_Spec;
      E040 := E040 + 1;
      System.Object_Reader'Elab_Spec;
      E097 := E097 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E063 := E063 + 1;
      System.Os_Lib'Elab_Body;
      E086 := E086 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E029 := E029 + 1;
      E023 := E023 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E053 := E053 + 1;
      E019 := E019 + 1;
      Leds'Elab_Body;
      E005 := E005 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_blink_led");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/stm32f4.o
   --   /home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/stm32f4-gpio.o
   --   /home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/stm32f4-reset_clock_control.o
   --   /home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/stm32f4-sysconfig_control.o
   --   /home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/registers.o
   --   /home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/leds.o
   --   /home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/blink_led.o
   --   -L/home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/
   --   -L/home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/examples/stm32f4/obj/
   --   -L/home/ubuntfg/Documents/TFG_RTS/marte_28mar2023/gnat_rts/rts-marteuc_stm32f/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
