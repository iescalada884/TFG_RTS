pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (martemain, Spec_File_Name => "b__marte.ads");
pragma Source_File_Name (martemain, Body_File_Name => "b__marte.adb");
pragma Suppress (Overflow_Check);

package body martemain is

   E159 : Short_Integer; pragma Import (Ada, E159, "marte__sll__advanced_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "marte__sll__map_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "marte__sll__map__show_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "marte__sll__order_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "marte__sll__order_advanced_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "marte__sll__prio_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "marte__sll__resources_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "marte__sll__show_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "marte__stacks_management_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "marte__kernel_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "marte__kernel__debug_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "marte__spy_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "marte__stacks_management__debug_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "marte__kernel__devices_table_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "marte__kernel__replenishment_te_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "marte__kernel__semaphores_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "marte__kernel__signals_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "marte__kernel__signals__global_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "marte__kernel__signals__pool_sdbs_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "marte__kernel__task_suspension_timed_events_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "marte__kernel__timed_events_queue_E");
   E036 : Short_Integer; pragma Import (Ada, E036, "marte__kernel__group_clocks_E");
   E034 : Short_Integer; pragma Import (Ada, E034, "marte__kernel__group_clocks__internals_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "marte__kernel__task_sets_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "marte__kernel__task_sets__internals_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "marte__kernel__timed_handlers__internals_E");
   E017 : Short_Integer; pragma Import (Ada, E017, "marte__kernel__application_scheduler_task_body_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "marte__kernel__mutexes_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "marte__kernel__condition_variables_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "marte__kernel__condition_variables__debug_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "marte__kernel__condition_variables__internals_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "marte__kernel__mutexes__debug_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "marte__kernel__mutexes__internals_appsched_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "marte__kernel__pool_tcbs_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "marte__kernel__pool_tcbs_debug_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "marte__kernel__semaphores__internals_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "marte__kernel__signals__handler_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "marte__kernel__signals__internals_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "marte__kernel__signals__pending_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "marte__kernel__tasks_operations_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "marte__kernel__scheduler_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "marte__kernel__hardware_interrupts_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "marte__kernel__tasks_operations__initialize_tcbs_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "marte__kernel__tasks_operations__internals_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "marte__kernel__timers_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "marte__kernel__application_scheduling_data_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "marte__kernel__mutexes__srp_ceiling_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "marte__kernel__signals__application_scheduler_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "marte__kernel__tasks_operations__application_scheduler_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "marte__kernel__timer_timed_events_pool_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "marte__kernel__timers__internals_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "marte__kernel__file_system_E");
   E002 : Short_Integer; pragma Import (Ada, E002, "drivers_marte_E");
   E004 : Short_Integer; pragma Import (Ada, E004, "gnat_io_driver_functions_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "marte__kernel__hardware_interrupts__operations_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "marte__kernel__mutexes__attributes_srp_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "marte__kernel__semaphores__operations_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "marte__kernel__signals__posix_functions_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "marte__kernel__initialization_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "marte__kernel__tasks_operations__attributes_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "marte__kernel__tasks_operations__attributes_edf_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "marte__kernel__tasks_operations__clock_nanosleep_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "marte__kernel__tasks_operations__nanosleep_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "marte__kernel__timed_handlers__operations_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "marte__kernel__task_sets__operations_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "marte__posix_interrupt_control_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "marte__posix_pthread_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "marte__posix_sched_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "marte__posix_semaphore_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "marte__posix_signal_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "marte__posix_time_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "marte__posix_unistd_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "marte__pthread_once_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "marte__kernel__types_sizes_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);


   procedure marteinit is
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");

      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      null;

      martemain'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;


      E159 := E159 + 1;
      E163 := E163 + 1;
      E161 := E161 + 1;
      E165 := E165 + 1;
      E167 := E167 + 1;
      E169 := E169 + 1;
      E171 := E171 + 1;
      E173 := E173 + 1;
      E027 := E027 + 1;
      E177 := E177 + 1;
      E140 := E140 + 1;
      E179 := E179 + 1;
      if E181 = 0 then
         MARTE.STACKS_MANAGEMENT'ELAB_BODY;
      end if;
      E181 := E181 + 1;
      if E029 = 0 then
         MARTE.KERNEL.DEVICES_TABLE'ELAB_SPEC;
      end if;
      E029 := E029 + 1;
      if E060 = 0 then
         MARTE.KERNEL.REPLENISHMENT_TE'ELAB_BODY;
      end if;
      E060 := E060 + 1;
      E072 := E072 + 1;
      if E088 = 0 then
         Marte.Kernel.Signals'Elab_Spec;
      end if;
      E088 := E088 + 1;
      if E077 = 0 then
         Marte.Kernel.Signals.Global'Elab_Spec;
      end if;
      E077 := E077 + 1;
      if E085 = 0 then
         MARTE.KERNEL.SIGNALS.POOL_SDBS'ELAB_BODY;
      end if;
      E085 := E085 + 1;
      if E096 = 0 then
         Marte.Kernel.Task_Suspension_Timed_Events'Elab_Body;
      end if;
      E096 := E096 + 1;
      if E126 = 0 then
         MARTE.KERNEL.TIMED_EVENTS_QUEUE'ELAB_BODY;
      end if;
      E126 := E126 + 1;
      E036 := E036 + 1;
      E034 := E034 + 1;
      E094 := E094 + 1;
      if E090 = 0 then
         MARTE.KERNEL.TASK_SETS.INTERNALS'ELAB_BODY;
      end if;
      E090 := E090 + 1;
      E128 := E128 + 1;
      if E054 = 0 then
         MARTE.KERNEL.MUTEXES'ELAB_SPEC;
      end if;
      E050 := E050 + 1;
      if E058 = 0 then
         Marte.Kernel.Pool_Tcbs_Debug'Elab_Body;
      end if;
      E058 := E058 + 1;
      if E083 = 0 then
         MARTE.KERNEL.SIGNALS.PENDING'ELAB_BODY;
      end if;
      E083 := E083 + 1;
      E021 := E021 + 1;
      if E040 = 0 then
         Marte.Kernel.Hardware_Interrupts'Elab_Spec;
      end if;
      if E040 = 0 then
         MARTE.KERNEL.HARDWARE_INTERRUPTS'ELAB_BODY;
      end if;
      E040 := E040 + 1;
      E046 := E046 + 1;
      E068 := E068 + 1;
      E079 := E079 + 1;
      if E019 = 0 then
         MARTE.KERNEL.APPLICATION_SCHEDULING_DATA'ELAB_SPEC;
      end if;
      if E019 = 0 then
         Marte.Kernel.Application_Scheduling_Data'Elab_Body;
      end if;
      E019 := E019 + 1;
      if E025 = 0 then
         MARTE.KERNEL.CONDITION_VARIABLES'ELAB_BODY;
      end if;
      E025 := E025 + 1;
      E023 := E023 + 1;
      E054 := E054 + 1;
      E052 := E052 + 1;
      if E056 = 0 then
         MARTE.KERNEL.POOL_TCBS'ELAB_BODY;
      end if;
      E056 := E056 + 1;
      E074 := E074 + 1;
      if E081 = 0 then
         MARTE.KERNEL.SIGNALS.INTERNALS'ELAB_BODY;
      end if;
      E081 := E081 + 1;
      E120 := E120 + 1;
      E104 := E104 + 1;
      E017 := E017 + 1;
      E116 := E116 + 1;
      if E133 = 0 then
         MARTE.KERNEL.TIMER_TIMED_EVENTS_POOL'ELAB_BODY;
      end if;
      E133 := E133 + 1;
      E135 := E135 + 1;
      if E064 = 0 then
         MARTE.KERNEL.SCHEDULER'ELAB_BODY;
      end if;
      E064 := E064 + 1;
      E114 := E114 + 1;
      E137 := E137 + 1;
      E031 := E031 + 1;
      E002 := E002 + 1;
      E004 := E004 + 1;
      if E038 = 0 then
         MARTE.KERNEL.HARDWARE_INTERRUPTS.OPERATIONS'ELAB_BODY;
      end if;
      E038 := E038 + 1;
      E044 := E044 + 1;
      E070 := E070 + 1;
      if E087 = 0 then
         MARTE.KERNEL.SIGNALS.POSIX_FUNCTIONS'ELAB_BODY;
      end if;
      E087 := E087 + 1;
      if E042 = 0 then
         MARTE.KERNEL.INITIALIZATION'ELAB_BODY;
      end if;
      E042 := E042 + 1;
      E106 := E106 + 1;
      E108 := E108 + 1;
      E110 := E110 + 1;
      E118 := E118 + 1;
      E130 := E130 + 1;
      E092 := E092 + 1;
      E143 := E143 + 1;
      E145 := E145 + 1;
      E147 := E147 + 1;
      E149 := E149 + 1;
      E151 := E151 + 1;
      E153 := E153 + 1;
      E155 := E155 + 1;
      E157 := E157 + 1;
      if E138 = 0 then
         Marte.Kernel.Types_Sizes'Elab_Spec;
      end if;
      E138 := E138 + 1;
   end marteinit;

--  BEGIN Object file/option list
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-configuration_parameters.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-debug_messages.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-integer_types.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-direct_io.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-hal-registers.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-hal.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-posix_constants.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-timespec.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll-advanced.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll-map.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll-map-show.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll-order.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll-order_advanced.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll-prio.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll-resources.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-sll-show.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-spy.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-stacks_management-debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-stacks_management.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-file_system_data_types.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-devices_table.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-replenishment_te.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-semaphores.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals-global.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals-pool_sdbs.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-task_suspension_timed_events.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_lists_prio.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_lists_show.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_map_lists_show.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-scheduler_cdbg.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timed_events_and_timer_debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timed_events_queue.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timed_events_and_timer.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timed_handlers.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-group_clocks.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-group_clocks-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-task_sets.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-task_sets-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timed_handlers-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-mutexes-internals_appsched.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-pool_tcbs_debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals-pending.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-condition_variables-debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-hardware_interrupts.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-mutexes-debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-scheduler-debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals-debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations-debug.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-semaphores-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals-handler.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-application_scheduling_data.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-application_scheduler.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-mutexes-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-condition_variables.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-condition_variables-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-mutexes.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-mutexes-srp_ceiling.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-pool_tcbs.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals-application_scheduler.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations-application_scheduler.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-application_scheduler_task_body.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timer_timed_events_pool.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timers-internals.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-scheduler.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations-initialize_tcbs.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timers.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-file_system.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/drivers_marte.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/gnat_io_driver_functions.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-hardware_interrupts-operations.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-mutexes-attributes_srp.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-semaphores-operations.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-signals-posix_functions.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-initialization.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations-attributes.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations-attributes_edf.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations-clock_nanosleep.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-tasks_operations-nanosleep.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-timed_handlers-operations.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-task_sets-operations.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-posix_interrupt_control.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-posix_pthread.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-posix_sched.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-posix_semaphore.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-posix_signal.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-posix_time.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-posix_unistd.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-pthread_once.o
   --   /home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/marte-kernel-types_sizes.o
   --   -L/home/ubuntfg/Documents/marte_28mar2023/objs/stm32f_objs/objs/
   --   -L/home/ubuntfg/opt/GNAT/2019-arm-elf/arm-eabi/lib/gnat/zfp-stm32f4/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end martemain;
