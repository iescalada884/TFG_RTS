pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test_uart.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test_uart.adb");
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
   E003 : Short_Integer; pragma Import (Ada, E003, "leds_E");

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

      if E019 = 0 then
         Ada.Exceptions'Elab_Spec;
      end if;
      if E023 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E039 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E039 := E039 + 1;
      if E054 = 0 then
         Ada.Containers'Elab_Spec;
      end if;
      E054 := E054 + 1;
      if E081 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E081 := E081 + 1;
      if E046 = 0 then
         Ada.Numerics'Elab_Spec;
      end if;
      E046 := E046 + 1;
      if E068 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E068 := E068 + 1;
      if E070 = 0 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      E070 := E070 + 1;
      if E073 = 0 then
         Ada.Strings.Maps.Constants'Elab_Spec;
      end if;
      E073 := E073 + 1;
      if E058 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      E058 := E058 + 1;
      if E040 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E040 := E040 + 1;
      if E097 = 0 then
         System.Object_Reader'Elab_Spec;
      end if;
      E097 := E097 + 1;
      if E063 = 0 then
         System.Dwarf_Lines'Elab_Spec;
      end if;
      E063 := E063 + 1;
      if E086 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E086 := E086 + 1;
      if E029 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E029 := E029 + 1;
      E023 := E023 + 1;
      if E053 = 0 then
         System.Traceback.Symbolic'Elab_Body;
      end if;
      E053 := E053 + 1;
      E019 := E019 + 1;
      if E003 = 0 then
         Leds'Elab_Body;
      end if;
      E003 := E003 + 1;
   end adainit;

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
