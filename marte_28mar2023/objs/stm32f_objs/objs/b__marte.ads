pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package martemain is

   procedure marteinit;
   pragma Export (C, marteinit, "marteinit");

   procedure martefinal;
   pragma Export (C, martefinal, "martefinal");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#7ac29857#;
   pragma Export (C, u00001, "drivers_marteB");
   u00002 : constant Version_32 := 16#cbc22675#;
   pragma Export (C, u00002, "drivers_marteS");
   u00003 : constant Version_32 := 16#a4a7f411#;
   pragma Export (C, u00003, "gnat_io_driver_functionsB");
   u00004 : constant Version_32 := 16#44ae6148#;
   pragma Export (C, u00004, "gnat_io_driver_functionsS");
   u00005 : constant Version_32 := 16#a1a2e31e#;
   pragma Export (C, u00005, "marte__configuration_parametersS");
   u00006 : constant Version_32 := 16#b6e188cf#;
   pragma Export (C, u00006, "marte__debug_messagesS");
   u00007 : constant Version_32 := 16#6747964d#;
   pragma Export (C, u00007, "marte__direct_ioB");
   u00008 : constant Version_32 := 16#6f062a55#;
   pragma Export (C, u00008, "marte__direct_ioS");
   u00009 : constant Version_32 := 16#36b81250#;
   pragma Export (C, u00009, "marte__hal__registersB");
   u00010 : constant Version_32 := 16#e235d01e#;
   pragma Export (C, u00010, "marte__hal__registersS");
   u00011 : constant Version_32 := 16#37107d5d#;
   pragma Export (C, u00011, "marte__halB");
   u00012 : constant Version_32 := 16#3928c941#;
   pragma Export (C, u00012, "marte__halS");
   u00013 : constant Version_32 := 16#1dff3570#;
   pragma Export (C, u00013, "marte__integer_typesS");
   u00014 : constant Version_32 := 16#538e478e#;
   pragma Export (C, u00014, "marte__kernel__application_schedulerB");
   u00015 : constant Version_32 := 16#ca6081cf#;
   pragma Export (C, u00015, "marte__kernel__application_schedulerS");
   u00016 : constant Version_32 := 16#9eb17952#;
   pragma Export (C, u00016, "marte__kernel__application_scheduler_task_bodyB");
   u00017 : constant Version_32 := 16#5cf720a7#;
   pragma Export (C, u00017, "marte__kernel__application_scheduler_task_bodyS");
   u00018 : constant Version_32 := 16#eb9c4d5a#;
   pragma Export (C, u00018, "marte__kernel__application_scheduling_dataB");
   u00019 : constant Version_32 := 16#2fad6f6c#;
   pragma Export (C, u00019, "marte__kernel__application_scheduling_dataS");
   u00020 : constant Version_32 := 16#3024a7bf#;
   pragma Export (C, u00020, "marte__kernel__condition_variables__debugB");
   u00021 : constant Version_32 := 16#50d040f4#;
   pragma Export (C, u00021, "marte__kernel__condition_variables__debugS");
   u00022 : constant Version_32 := 16#afd17fbd#;
   pragma Export (C, u00022, "marte__kernel__condition_variables__internalsB");
   u00023 : constant Version_32 := 16#c6a480ff#;
   pragma Export (C, u00023, "marte__kernel__condition_variables__internalsS");
   u00024 : constant Version_32 := 16#10cc60c2#;
   pragma Export (C, u00024, "marte__kernel__condition_variablesB");
   u00025 : constant Version_32 := 16#7f0b1f43#;
   pragma Export (C, u00025, "marte__kernel__condition_variablesS");
   u00026 : constant Version_32 := 16#3458e61b#;
   pragma Export (C, u00026, "marte__kernel__debugB");
   u00027 : constant Version_32 := 16#e2831a3e#;
   pragma Export (C, u00027, "marte__kernel__debugS");
   u00028 : constant Version_32 := 16#b38668c2#;
   pragma Export (C, u00028, "marte__kernel__devices_tableB");
   u00029 : constant Version_32 := 16#e5c9ffa2#;
   pragma Export (C, u00029, "marte__kernel__devices_tableS");
   u00030 : constant Version_32 := 16#ad21fe63#;
   pragma Export (C, u00030, "marte__kernel__file_systemB");
   u00031 : constant Version_32 := 16#f1e7eb51#;
   pragma Export (C, u00031, "marte__kernel__file_systemS");
   u00032 : constant Version_32 := 16#ecb93590#;
   pragma Export (C, u00032, "marte__kernel__file_system_data_typesS");
   u00033 : constant Version_32 := 16#26c73cbd#;
   pragma Export (C, u00033, "marte__kernel__group_clocks__internalsB");
   u00034 : constant Version_32 := 16#26916460#;
   pragma Export (C, u00034, "marte__kernel__group_clocks__internalsS");
   u00035 : constant Version_32 := 16#90121e64#;
   pragma Export (C, u00035, "marte__kernel__group_clocksB");
   u00036 : constant Version_32 := 16#dbf88db2#;
   pragma Export (C, u00036, "marte__kernel__group_clocksS");
   u00037 : constant Version_32 := 16#7e722af3#;
   pragma Export (C, u00037, "marte__kernel__hardware_interrupts__operationsB");
   u00038 : constant Version_32 := 16#46f3d407#;
   pragma Export (C, u00038, "marte__kernel__hardware_interrupts__operationsS");
   u00039 : constant Version_32 := 16#97465463#;
   pragma Export (C, u00039, "marte__kernel__hardware_interruptsB");
   u00040 : constant Version_32 := 16#3f1926d0#;
   pragma Export (C, u00040, "marte__kernel__hardware_interruptsS");
   u00041 : constant Version_32 := 16#aef4eb5e#;
   pragma Export (C, u00041, "marte__kernel__initializationB");
   u00042 : constant Version_32 := 16#b5df364b#;
   pragma Export (C, u00042, "marte__kernel__initializationS");
   u00043 : constant Version_32 := 16#3e94aff2#;
   pragma Export (C, u00043, "marte__kernel__mutexes__attributes_srpB");
   u00044 : constant Version_32 := 16#f92b2493#;
   pragma Export (C, u00044, "marte__kernel__mutexes__attributes_srpS");
   u00045 : constant Version_32 := 16#4755875f#;
   pragma Export (C, u00045, "marte__kernel__mutexes__debugB");
   u00046 : constant Version_32 := 16#48ae3a8e#;
   pragma Export (C, u00046, "marte__kernel__mutexes__debugS");
   u00047 : constant Version_32 := 16#97558fc5#;
   pragma Export (C, u00047, "marte__kernel__mutexes__internalsB");
   u00048 : constant Version_32 := 16#96cf78ac#;
   pragma Export (C, u00048, "marte__kernel__mutexes__internalsS");
   u00049 : constant Version_32 := 16#683be531#;
   pragma Export (C, u00049, "marte__kernel__mutexes__internals_appschedB");
   u00050 : constant Version_32 := 16#e1d8f6de#;
   pragma Export (C, u00050, "marte__kernel__mutexes__internals_appschedS");
   u00051 : constant Version_32 := 16#95b345a5#;
   pragma Export (C, u00051, "marte__kernel__mutexes__srp_ceilingB");
   u00052 : constant Version_32 := 16#11d2a13a#;
   pragma Export (C, u00052, "marte__kernel__mutexes__srp_ceilingS");
   u00053 : constant Version_32 := 16#5d3e2d18#;
   pragma Export (C, u00053, "marte__kernel__mutexesB");
   u00054 : constant Version_32 := 16#68a42f01#;
   pragma Export (C, u00054, "marte__kernel__mutexesS");
   u00055 : constant Version_32 := 16#45924a26#;
   pragma Export (C, u00055, "marte__kernel__pool_tcbsB");
   u00056 : constant Version_32 := 16#72175018#;
   pragma Export (C, u00056, "marte__kernel__pool_tcbsS");
   u00057 : constant Version_32 := 16#95ff560e#;
   pragma Export (C, u00057, "marte__kernel__pool_tcbs_debugB");
   u00058 : constant Version_32 := 16#b7b6bedf#;
   pragma Export (C, u00058, "marte__kernel__pool_tcbs_debugS");
   u00059 : constant Version_32 := 16#a9be2d85#;
   pragma Export (C, u00059, "marte__kernel__replenishment_teB");
   u00060 : constant Version_32 := 16#86fbdf71#;
   pragma Export (C, u00060, "marte__kernel__replenishment_teS");
   u00061 : constant Version_32 := 16#b758da0c#;
   pragma Export (C, u00061, "marte__kernel__scheduler__debugB");
   u00062 : constant Version_32 := 16#bcb172a0#;
   pragma Export (C, u00062, "marte__kernel__scheduler__debugS");
   u00063 : constant Version_32 := 16#bc652480#;
   pragma Export (C, u00063, "marte__kernel__schedulerB");
   u00064 : constant Version_32 := 16#bacff71d#;
   pragma Export (C, u00064, "marte__kernel__schedulerS");
   u00065 : constant Version_32 := 16#c30c2328#;
   pragma Export (C, u00065, "marte__kernel__scheduler_cdbgB");
   u00066 : constant Version_32 := 16#bf0aaeba#;
   pragma Export (C, u00066, "marte__kernel__scheduler_cdbgS");
   u00067 : constant Version_32 := 16#64d025f3#;
   pragma Export (C, u00067, "marte__kernel__semaphores__internalsB");
   u00068 : constant Version_32 := 16#712464f0#;
   pragma Export (C, u00068, "marte__kernel__semaphores__internalsS");
   u00069 : constant Version_32 := 16#e74a78e7#;
   pragma Export (C, u00069, "marte__kernel__semaphores__operationsB");
   u00070 : constant Version_32 := 16#570e3955#;
   pragma Export (C, u00070, "marte__kernel__semaphores__operationsS");
   u00071 : constant Version_32 := 16#df155afa#;
   pragma Export (C, u00071, "marte__kernel__semaphoresB");
   u00072 : constant Version_32 := 16#e0997211#;
   pragma Export (C, u00072, "marte__kernel__semaphoresS");
   u00073 : constant Version_32 := 16#cfe9d5f5#;
   pragma Export (C, u00073, "marte__kernel__signals__application_schedulerB");
   u00074 : constant Version_32 := 16#f77ff5bf#;
   pragma Export (C, u00074, "marte__kernel__signals__application_schedulerS");
   u00075 : constant Version_32 := 16#e65711fc#;
   pragma Export (C, u00075, "marte__kernel__signals__debugB");
   u00076 : constant Version_32 := 16#a7f9a6bf#;
   pragma Export (C, u00076, "marte__kernel__signals__debugS");
   u00077 : constant Version_32 := 16#dcb3218f#;
   pragma Export (C, u00077, "marte__kernel__signals__globalS");
   u00078 : constant Version_32 := 16#8493715a#;
   pragma Export (C, u00078, "marte__kernel__signals__handlerB");
   u00079 : constant Version_32 := 16#ebcd2e42#;
   pragma Export (C, u00079, "marte__kernel__signals__handlerS");
   u00080 : constant Version_32 := 16#4b4c6f59#;
   pragma Export (C, u00080, "marte__kernel__signals__internalsB");
   u00081 : constant Version_32 := 16#b6ed61d0#;
   pragma Export (C, u00081, "marte__kernel__signals__internalsS");
   u00082 : constant Version_32 := 16#781a4785#;
   pragma Export (C, u00082, "marte__kernel__signals__pendingB");
   u00083 : constant Version_32 := 16#da9739b9#;
   pragma Export (C, u00083, "marte__kernel__signals__pendingS");
   u00084 : constant Version_32 := 16#0c914a53#;
   pragma Export (C, u00084, "marte__kernel__signals__pool_sdbsB");
   u00085 : constant Version_32 := 16#9a066e6e#;
   pragma Export (C, u00085, "marte__kernel__signals__pool_sdbsS");
   u00086 : constant Version_32 := 16#547164c9#;
   pragma Export (C, u00086, "marte__kernel__signals__posix_functionsB");
   u00087 : constant Version_32 := 16#e3b2c5a0#;
   pragma Export (C, u00087, "marte__kernel__signals__posix_functionsS");
   u00088 : constant Version_32 := 16#a35eef68#;
   pragma Export (C, u00088, "marte__kernel__signalsS");
   u00089 : constant Version_32 := 16#c671bb17#;
   pragma Export (C, u00089, "marte__kernel__task_sets__internalsB");
   u00090 : constant Version_32 := 16#e7b7b02d#;
   pragma Export (C, u00090, "marte__kernel__task_sets__internalsS");
   u00091 : constant Version_32 := 16#4db924a8#;
   pragma Export (C, u00091, "marte__kernel__task_sets__operationsB");
   u00092 : constant Version_32 := 16#508b65d8#;
   pragma Export (C, u00092, "marte__kernel__task_sets__operationsS");
   u00093 : constant Version_32 := 16#4988b630#;
   pragma Export (C, u00093, "marte__kernel__task_setsB");
   u00094 : constant Version_32 := 16#2ef1c945#;
   pragma Export (C, u00094, "marte__kernel__task_setsS");
   u00095 : constant Version_32 := 16#b265e944#;
   pragma Export (C, u00095, "marte__kernel__task_suspension_timed_eventsB");
   u00096 : constant Version_32 := 16#b5f357b3#;
   pragma Export (C, u00096, "marte__kernel__task_suspension_timed_eventsS");
   u00097 : constant Version_32 := 16#7a51de9c#;
   pragma Export (C, u00097, "marte__kernel__tasks_lists_prioB");
   u00098 : constant Version_32 := 16#4a690173#;
   pragma Export (C, u00098, "marte__kernel__tasks_lists_prioS");
   u00099 : constant Version_32 := 16#df11ad56#;
   pragma Export (C, u00099, "marte__kernel__tasks_lists_showB");
   u00100 : constant Version_32 := 16#dd5364a9#;
   pragma Export (C, u00100, "marte__kernel__tasks_lists_showS");
   u00101 : constant Version_32 := 16#89d62743#;
   pragma Export (C, u00101, "marte__kernel__tasks_map_lists_showB");
   u00102 : constant Version_32 := 16#273ea12c#;
   pragma Export (C, u00102, "marte__kernel__tasks_map_lists_showS");
   u00103 : constant Version_32 := 16#9ae10451#;
   pragma Export (C, u00103, "marte__kernel__tasks_operations__application_schedulerB");
   u00104 : constant Version_32 := 16#3acf0869#;
   pragma Export (C, u00104, "marte__kernel__tasks_operations__application_schedulerS");
   u00105 : constant Version_32 := 16#823888c4#;
   pragma Export (C, u00105, "marte__kernel__tasks_operations__attributesB");
   u00106 : constant Version_32 := 16#8a546649#;
   pragma Export (C, u00106, "marte__kernel__tasks_operations__attributesS");
   u00107 : constant Version_32 := 16#79a0594d#;
   pragma Export (C, u00107, "marte__kernel__tasks_operations__attributes_edfB");
   u00108 : constant Version_32 := 16#712bc976#;
   pragma Export (C, u00108, "marte__kernel__tasks_operations__attributes_edfS");
   u00109 : constant Version_32 := 16#b5cf1d3f#;
   pragma Export (C, u00109, "marte__kernel__tasks_operations__clock_nanosleepB");
   u00110 : constant Version_32 := 16#14f9c28c#;
   pragma Export (C, u00110, "marte__kernel__tasks_operations__clock_nanosleepS");
   u00111 : constant Version_32 := 16#a721ed01#;
   pragma Export (C, u00111, "marte__kernel__tasks_operations__debugB");
   u00112 : constant Version_32 := 16#89b5d6a5#;
   pragma Export (C, u00112, "marte__kernel__tasks_operations__debugS");
   u00113 : constant Version_32 := 16#fe664a4c#;
   pragma Export (C, u00113, "marte__kernel__tasks_operations__initialize_tcbsB");
   u00114 : constant Version_32 := 16#f1480fba#;
   pragma Export (C, u00114, "marte__kernel__tasks_operations__initialize_tcbsS");
   u00115 : constant Version_32 := 16#e937abc5#;
   pragma Export (C, u00115, "marte__kernel__tasks_operations__internalsB");
   u00116 : constant Version_32 := 16#bf6c698c#;
   pragma Export (C, u00116, "marte__kernel__tasks_operations__internalsS");
   u00117 : constant Version_32 := 16#df334a9d#;
   pragma Export (C, u00117, "marte__kernel__tasks_operations__nanosleepB");
   u00118 : constant Version_32 := 16#4a78f853#;
   pragma Export (C, u00118, "marte__kernel__tasks_operations__nanosleepS");
   u00119 : constant Version_32 := 16#064222db#;
   pragma Export (C, u00119, "marte__kernel__tasks_operationsB");
   u00120 : constant Version_32 := 16#f90bfd61#;
   pragma Export (C, u00120, "marte__kernel__tasks_operationsS");
   u00121 : constant Version_32 := 16#7083898d#;
   pragma Export (C, u00121, "marte__kernel__timed_events_and_timerB");
   u00122 : constant Version_32 := 16#1a00da5f#;
   pragma Export (C, u00122, "marte__kernel__timed_events_and_timerS");
   u00123 : constant Version_32 := 16#012398c2#;
   pragma Export (C, u00123, "marte__kernel__timed_events_and_timer_debugB");
   u00124 : constant Version_32 := 16#c02495c0#;
   pragma Export (C, u00124, "marte__kernel__timed_events_and_timer_debugS");
   u00125 : constant Version_32 := 16#9d263c56#;
   pragma Export (C, u00125, "marte__kernel__timed_events_queueB");
   u00126 : constant Version_32 := 16#12231172#;
   pragma Export (C, u00126, "marte__kernel__timed_events_queueS");
   u00127 : constant Version_32 := 16#7d8a4a89#;
   pragma Export (C, u00127, "marte__kernel__timed_handlers__internalsB");
   u00128 : constant Version_32 := 16#7fde78d5#;
   pragma Export (C, u00128, "marte__kernel__timed_handlers__internalsS");
   u00129 : constant Version_32 := 16#3cc398da#;
   pragma Export (C, u00129, "marte__kernel__timed_handlers__operationsB");
   u00130 : constant Version_32 := 16#9bfd7ef3#;
   pragma Export (C, u00130, "marte__kernel__timed_handlers__operationsS");
   u00131 : constant Version_32 := 16#2aad7123#;
   pragma Export (C, u00131, "marte__kernel__timed_handlersS");
   u00132 : constant Version_32 := 16#64fc4918#;
   pragma Export (C, u00132, "marte__kernel__timer_timed_events_poolB");
   u00133 : constant Version_32 := 16#ad8bd00f#;
   pragma Export (C, u00133, "marte__kernel__timer_timed_events_poolS");
   u00134 : constant Version_32 := 16#6093e320#;
   pragma Export (C, u00134, "marte__kernel__timers__internalsB");
   u00135 : constant Version_32 := 16#963e9d2a#;
   pragma Export (C, u00135, "marte__kernel__timers__internalsS");
   u00136 : constant Version_32 := 16#900210f7#;
   pragma Export (C, u00136, "marte__kernel__timersB");
   u00137 : constant Version_32 := 16#5d524d1e#;
   pragma Export (C, u00137, "marte__kernel__timersS");
   u00138 : constant Version_32 := 16#75e266da#;
   pragma Export (C, u00138, "marte__kernel__types_sizesS");
   u00139 : constant Version_32 := 16#16c85b11#;
   pragma Export (C, u00139, "marte__kernelB");
   u00140 : constant Version_32 := 16#e593c053#;
   pragma Export (C, u00140, "marte__kernelS");
   u00141 : constant Version_32 := 16#3a76628c#;
   pragma Export (C, u00141, "marte__posix_constantsS");
   u00142 : constant Version_32 := 16#217c1b88#;
   pragma Export (C, u00142, "marte__posix_interrupt_controlB");
   u00143 : constant Version_32 := 16#80f66ebf#;
   pragma Export (C, u00143, "marte__posix_interrupt_controlS");
   u00144 : constant Version_32 := 16#51ae8e82#;
   pragma Export (C, u00144, "marte__posix_pthreadB");
   u00145 : constant Version_32 := 16#381f20e9#;
   pragma Export (C, u00145, "marte__posix_pthreadS");
   u00146 : constant Version_32 := 16#aa472d94#;
   pragma Export (C, u00146, "marte__posix_schedB");
   u00147 : constant Version_32 := 16#b9576df1#;
   pragma Export (C, u00147, "marte__posix_schedS");
   u00148 : constant Version_32 := 16#a3e68c42#;
   pragma Export (C, u00148, "marte__posix_semaphoreB");
   u00149 : constant Version_32 := 16#853e6f66#;
   pragma Export (C, u00149, "marte__posix_semaphoreS");
   u00150 : constant Version_32 := 16#07bdcd9a#;
   pragma Export (C, u00150, "marte__posix_signalB");
   u00151 : constant Version_32 := 16#f8ad7c17#;
   pragma Export (C, u00151, "marte__posix_signalS");
   u00152 : constant Version_32 := 16#e602938a#;
   pragma Export (C, u00152, "marte__posix_timeB");
   u00153 : constant Version_32 := 16#507de01d#;
   pragma Export (C, u00153, "marte__posix_timeS");
   u00154 : constant Version_32 := 16#eeb79bae#;
   pragma Export (C, u00154, "marte__posix_unistdB");
   u00155 : constant Version_32 := 16#ccba9630#;
   pragma Export (C, u00155, "marte__posix_unistdS");
   u00156 : constant Version_32 := 16#92fd6fc2#;
   pragma Export (C, u00156, "marte__pthread_onceB");
   u00157 : constant Version_32 := 16#a12ed07d#;
   pragma Export (C, u00157, "marte__pthread_onceS");
   u00158 : constant Version_32 := 16#4d3d4d0b#;
   pragma Export (C, u00158, "marte__sll__advancedB");
   u00159 : constant Version_32 := 16#d8fcf2c1#;
   pragma Export (C, u00159, "marte__sll__advancedS");
   u00160 : constant Version_32 := 16#50c920c7#;
   pragma Export (C, u00160, "marte__sll__map__showB");
   u00161 : constant Version_32 := 16#20b3297d#;
   pragma Export (C, u00161, "marte__sll__map__showS");
   u00162 : constant Version_32 := 16#7b28b3b4#;
   pragma Export (C, u00162, "marte__sll__mapB");
   u00163 : constant Version_32 := 16#a8e24a08#;
   pragma Export (C, u00163, "marte__sll__mapS");
   u00164 : constant Version_32 := 16#87d5e333#;
   pragma Export (C, u00164, "marte__sll__orderB");
   u00165 : constant Version_32 := 16#b2a4eff8#;
   pragma Export (C, u00165, "marte__sll__orderS");
   u00166 : constant Version_32 := 16#7542859f#;
   pragma Export (C, u00166, "marte__sll__order_advancedB");
   u00167 : constant Version_32 := 16#d2bbc068#;
   pragma Export (C, u00167, "marte__sll__order_advancedS");
   u00168 : constant Version_32 := 16#bb09de19#;
   pragma Export (C, u00168, "marte__sll__prioB");
   u00169 : constant Version_32 := 16#1a3f886b#;
   pragma Export (C, u00169, "marte__sll__prioS");
   u00170 : constant Version_32 := 16#8b634eb7#;
   pragma Export (C, u00170, "marte__sll__resourcesB");
   u00171 : constant Version_32 := 16#c8c7f7a4#;
   pragma Export (C, u00171, "marte__sll__resourcesS");
   u00172 : constant Version_32 := 16#52a32019#;
   pragma Export (C, u00172, "marte__sll__showB");
   u00173 : constant Version_32 := 16#24fb2ce5#;
   pragma Export (C, u00173, "marte__sll__showS");
   u00174 : constant Version_32 := 16#6289c5c8#;
   pragma Export (C, u00174, "marte__sllB");
   u00175 : constant Version_32 := 16#6022d486#;
   pragma Export (C, u00175, "marte__sllS");
   u00176 : constant Version_32 := 16#51004724#;
   pragma Export (C, u00176, "marte__spyB");
   u00177 : constant Version_32 := 16#16219d8e#;
   pragma Export (C, u00177, "marte__spyS");
   u00178 : constant Version_32 := 16#fb5b3612#;
   pragma Export (C, u00178, "marte__stacks_management__debugB");
   u00179 : constant Version_32 := 16#6a01475a#;
   pragma Export (C, u00179, "marte__stacks_management__debugS");
   u00180 : constant Version_32 := 16#aba1c8fc#;
   pragma Export (C, u00180, "marte__stacks_managementB");
   u00181 : constant Version_32 := 16#2791e3b0#;
   pragma Export (C, u00181, "marte__stacks_managementS");
   u00182 : constant Version_32 := 16#b94e4055#;
   pragma Export (C, u00182, "marte__timespecB");
   u00183 : constant Version_32 := 16#b002a177#;
   pragma Export (C, u00183, "marte__timespecS");
   u00184 : constant Version_32 := 16#99bc4a3f#;
   pragma Export (C, u00184, "marteS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.atomic_operations%s
   --  system.bb%s
   --  system.bb.board_parameters%s
   --  system.bb.mcu_parameters%s
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  system.spark%s
   --  system.spark.cut_operations%s
   --  system.spark.cut_operations%b
   --  system.stm32%s
   --  system.bb.parameters%s
   --  system.storage_elements%s
   --  system.return_stack%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_9%s
   --  system.concat_9%b
   --  system.traceback%s
   --  system.traceback%b
   --  ada.characters.handling%s
   --  system.atomic_operations.test_and_set%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_lli%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.numerics.big_numbers%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.atomic_primitives%s
   --  system.atomic_primitives%b
   --  system.exceptions%s
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  ada.characters.handling%b
   --  system.atomic_operations.test_and_set%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.img_int%s
   --  system.img_uns%s
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap%b
   --  system.mmap.unix%s
   --  system.mmap.os_interface%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.strings.utf_encoding%s
   --  ada.strings.utf_encoding%b
   --  ada.strings.utf_encoding.strings%s
   --  ada.strings.utf_encoding.strings%b
   --  ada.strings.utf_encoding.wide_strings%s
   --  ada.strings.utf_encoding.wide_strings%b
   --  ada.strings.utf_encoding.wide_wide_strings%s
   --  ada.strings.utf_encoding.wide_wide_strings%b
   --  gnat%s
   --  gnat.io%s
   --  gnat.io%b
   --  gnat.source_info%s
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  system.fat_llf%s
   --  system.val_uns%s
   --  ada.tags%s
   --  ada.tags%b
   --  ada.strings.text_buffers%s
   --  ada.strings.text_buffers%b
   --  ada.strings.text_buffers.utils%s
   --  ada.strings.text_buffers.utils%b
   --  system.put_images%s
   --  system.put_images%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.stream_attributes%s
   --  system.stream_attributes.xdr%s
   --  system.stream_attributes.xdr%b
   --  system.stream_attributes%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  marte%s
   --  marte.configuration_parameters%s
   --  marte.debug_messages%s
   --  marte.integer_types%s
   --  marte.direct_io%s
   --  marte.direct_io%b
   --  marte.hal%s
   --  marte.hal.registers%s
   --  marte.hal.registers%b
   --  marte.hal%b
   --  marte.posix_constants%s
   --  marte.timespec%s
   --  marte.timespec%b
   --  marte.sll%s
   --  marte.sll%b
   --  marte.sll.advanced%s
   --  marte.sll.advanced%b
   --  marte.sll.map%s
   --  marte.sll.map%b
   --  marte.sll.map.show%s
   --  marte.sll.map.show%b
   --  marte.sll.order%s
   --  marte.sll.order%b
   --  marte.sll.order_advanced%s
   --  marte.sll.order_advanced%b
   --  marte.sll.prio%s
   --  marte.sll.prio%b
   --  marte.sll.resources%s
   --  marte.sll.resources%b
   --  marte.sll.show%s
   --  marte.sll.show%b
   --  marte.stacks_management%s
   --  marte.kernel%s
   --  marte.kernel.debug%s
   --  marte.kernel.debug%b
   --  marte.spy%s
   --  marte.spy%b
   --  marte.kernel%b
   --  marte.stacks_management.debug%s
   --  marte.stacks_management.debug%b
   --  marte.stacks_management%b
   --  marte.kernel.file_system_data_types%s
   --  marte.kernel.devices_table%s
   --  marte.kernel.devices_table%b
   --  marte.kernel.replenishment_te%s
   --  marte.kernel.replenishment_te%b
   --  marte.kernel.semaphores%s
   --  marte.kernel.semaphores%b
   --  marte.kernel.signals%s
   --  marte.kernel.signals.global%s
   --  marte.kernel.signals.pool_sdbs%s
   --  marte.kernel.signals.pool_sdbs%b
   --  marte.kernel.task_suspension_timed_events%s
   --  marte.kernel.task_suspension_timed_events%b
   --  marte.kernel.tasks_lists_prio%s
   --  marte.kernel.tasks_lists_prio%b
   --  marte.kernel.tasks_lists_show%s
   --  marte.kernel.tasks_lists_show%b
   --  marte.kernel.tasks_map_lists_show%s
   --  marte.kernel.tasks_map_lists_show%b
   --  marte.kernel.scheduler_cdbg%s
   --  marte.kernel.scheduler_cdbg%b
   --  marte.kernel.timed_events_and_timer_debug%s
   --  marte.kernel.timed_events_and_timer_debug%b
   --  marte.kernel.timed_events_queue%s
   --  marte.kernel.timed_events_queue%b
   --  marte.kernel.timed_events_and_timer%s
   --  marte.kernel.timed_events_and_timer%b
   --  marte.kernel.timed_handlers%s
   --  marte.kernel.group_clocks%s
   --  marte.kernel.group_clocks%b
   --  marte.kernel.group_clocks.internals%s
   --  marte.kernel.group_clocks.internals%b
   --  marte.kernel.task_sets%s
   --  marte.kernel.task_sets%b
   --  marte.kernel.task_sets.internals%s
   --  marte.kernel.task_sets.internals%b
   --  marte.kernel.timed_handlers.internals%s
   --  marte.kernel.timed_handlers.internals%b
   --  marte.kernel.application_scheduler_task_body%s
   --  marte.kernel.mutexes%s
   --  marte.kernel.condition_variables%s
   --  marte.kernel.condition_variables.debug%s
   --  marte.kernel.condition_variables.internals%s
   --  marte.kernel.mutexes.debug%s
   --  marte.kernel.mutexes.internals_appsched%s
   --  marte.kernel.mutexes.internals_appsched%b
   --  marte.kernel.pool_tcbs%s
   --  marte.kernel.pool_tcbs_debug%s
   --  marte.kernel.pool_tcbs_debug%b
   --  marte.kernel.semaphores.internals%s
   --  marte.kernel.signals.handler%s
   --  marte.kernel.signals.internals%s
   --  marte.kernel.signals.pending%s
   --  marte.kernel.signals.pending%b
   --  marte.kernel.tasks_operations%s
   --  marte.kernel.scheduler%s
   --  marte.kernel.condition_variables.debug%b
   --  marte.kernel.hardware_interrupts%s
   --  marte.kernel.hardware_interrupts%b
   --  marte.kernel.mutexes.debug%b
   --  marte.kernel.scheduler.debug%s
   --  marte.kernel.scheduler.debug%b
   --  marte.kernel.signals.debug%s
   --  marte.kernel.signals.debug%b
   --  marte.kernel.tasks_operations.debug%s
   --  marte.kernel.tasks_operations.debug%b
   --  marte.kernel.tasks_operations.initialize_tcbs%s
   --  marte.kernel.tasks_operations.internals%s
   --  marte.kernel.semaphores.internals%b
   --  marte.kernel.signals.handler%b
   --  marte.kernel.timers%s
   --  marte.kernel.application_scheduling_data%s
   --  marte.kernel.application_scheduling_data%b
   --  marte.kernel.mutexes.srp_ceiling%s
   --  marte.kernel.application_scheduler%s
   --  marte.kernel.application_scheduler%b
   --  marte.kernel.mutexes.internals%s
   --  marte.kernel.mutexes.internals%b
   --  marte.kernel.condition_variables%b
   --  marte.kernel.condition_variables.internals%b
   --  marte.kernel.mutexes%b
   --  marte.kernel.mutexes.srp_ceiling%b
   --  marte.kernel.pool_tcbs%b
   --  marte.kernel.signals.application_scheduler%s
   --  marte.kernel.signals.application_scheduler%b
   --  marte.kernel.signals.internals%b
   --  marte.kernel.tasks_operations%b
   --  marte.kernel.tasks_operations.application_scheduler%s
   --  marte.kernel.tasks_operations.application_scheduler%b
   --  marte.kernel.application_scheduler_task_body%b
   --  marte.kernel.tasks_operations.internals%b
   --  marte.kernel.timer_timed_events_pool%s
   --  marte.kernel.timer_timed_events_pool%b
   --  marte.kernel.timers.internals%s
   --  marte.kernel.timers.internals%b
   --  marte.kernel.scheduler%b
   --  marte.kernel.tasks_operations.initialize_tcbs%b
   --  marte.kernel.timers%b
   --  marte.kernel.file_system%s
   --  marte.kernel.file_system%b
   --  drivers_marte%s
   --  drivers_marte%b
   --  gnat_io_driver_functions%s
   --  gnat_io_driver_functions%b
   --  marte.kernel.hardware_interrupts.operations%s
   --  marte.kernel.hardware_interrupts.operations%b
   --  marte.kernel.mutexes.attributes_srp%s
   --  marte.kernel.mutexes.attributes_srp%b
   --  marte.kernel.semaphores.operations%s
   --  marte.kernel.semaphores.operations%b
   --  marte.kernel.signals.posix_functions%s
   --  marte.kernel.signals.posix_functions%b
   --  marte.kernel.initialization%s
   --  marte.kernel.initialization%b
   --  marte.kernel.tasks_operations.attributes%s
   --  marte.kernel.tasks_operations.attributes%b
   --  marte.kernel.tasks_operations.attributes_edf%s
   --  marte.kernel.tasks_operations.attributes_edf%b
   --  marte.kernel.tasks_operations.clock_nanosleep%s
   --  marte.kernel.tasks_operations.clock_nanosleep%b
   --  marte.kernel.tasks_operations.nanosleep%s
   --  marte.kernel.tasks_operations.nanosleep%b
   --  marte.kernel.timed_handlers.operations%s
   --  marte.kernel.timed_handlers.operations%b
   --  marte.kernel.task_sets.operations%s
   --  marte.kernel.task_sets.operations%b
   --  marte.posix_interrupt_control%s
   --  marte.posix_interrupt_control%b
   --  marte.posix_pthread%s
   --  marte.posix_pthread%b
   --  marte.posix_sched%s
   --  marte.posix_sched%b
   --  marte.posix_semaphore%s
   --  marte.posix_semaphore%b
   --  marte.posix_signal%s
   --  marte.posix_signal%b
   --  marte.posix_time%s
   --  marte.posix_time%b
   --  marte.posix_unistd%s
   --  marte.posix_unistd%b
   --  marte.pthread_once%s
   --  marte.pthread_once%b
   --  marte.kernel.types_sizes%s
   --  END ELABORATION ORDER

end martemain;
