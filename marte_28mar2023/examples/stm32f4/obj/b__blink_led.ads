pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 14.2.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_blink_led" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#0f9b1b22#;
   pragma Export (C, u00001, "blink_ledB");
   u00002 : constant Version_32 := 16#30305195#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0626cc96#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#94487797#;
   pragma Export (C, u00004, "ledsB");
   u00005 : constant Version_32 := 16#3be316d2#;
   pragma Export (C, u00005, "ledsS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#c8aeaca7#;
   pragma Export (C, u00007, "registersS");
   u00008 : constant Version_32 := 16#8b0a87f8#;
   pragma Export (C, u00008, "stm32f4S");
   u00009 : constant Version_32 := 16#9111f9c1#;
   pragma Export (C, u00009, "interfacesS");
   u00010 : constant Version_32 := 16#ee021456#;
   pragma Export (C, u00010, "system__unsigned_typesS");
   u00011 : constant Version_32 := 16#14286b0f#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#1acdc035#;
   pragma Export (C, u00012, "stm32f4__gpioS");
   u00013 : constant Version_32 := 16#29bef750#;
   pragma Export (C, u00013, "stm32f4__reset_clock_controlS");
   u00014 : constant Version_32 := 16#8977ddd8#;
   pragma Export (C, u00014, "stm32f4__sysconfig_controlS");
   u00015 : constant Version_32 := 16#d8f6bfe7#;
   pragma Export (C, u00015, "system__storage_elementsS");
   u00016 : constant Version_32 := 16#8f2423cb#;
   pragma Export (C, u00016, "system__memoryB");
   u00017 : constant Version_32 := 16#0cbcf715#;
   pragma Export (C, u00017, "system__memoryS");
   u00018 : constant Version_32 := 16#9a5d1b93#;
   pragma Export (C, u00018, "ada__exceptionsB");
   u00019 : constant Version_32 := 16#64d9391c#;
   pragma Export (C, u00019, "ada__exceptionsS");
   u00020 : constant Version_32 := 16#0740df23#;
   pragma Export (C, u00020, "ada__exceptions__last_chance_handlerB");
   u00021 : constant Version_32 := 16#a028f72d#;
   pragma Export (C, u00021, "ada__exceptions__last_chance_handlerS");
   u00022 : constant Version_32 := 16#fd5f5f4c#;
   pragma Export (C, u00022, "system__soft_linksB");
   u00023 : constant Version_32 := 16#455c24f2#;
   pragma Export (C, u00023, "system__soft_linksS");
   u00024 : constant Version_32 := 16#524f7d04#;
   pragma Export (C, u00024, "system__secondary_stackB");
   u00025 : constant Version_32 := 16#bae33a03#;
   pragma Export (C, u00025, "system__secondary_stackS");
   u00026 : constant Version_32 := 16#a43efea2#;
   pragma Export (C, u00026, "system__parametersB");
   u00027 : constant Version_32 := 16#21bf971e#;
   pragma Export (C, u00027, "system__parametersS");
   u00028 : constant Version_32 := 16#0286ce9f#;
   pragma Export (C, u00028, "system__soft_links__initializeB");
   u00029 : constant Version_32 := 16#2ed17187#;
   pragma Export (C, u00029, "system__soft_links__initializeS");
   u00030 : constant Version_32 := 16#8599b27b#;
   pragma Export (C, u00030, "system__stack_checkingB");
   u00031 : constant Version_32 := 16#d3777e19#;
   pragma Export (C, u00031, "system__stack_checkingS");
   u00032 : constant Version_32 := 16#ca878138#;
   pragma Export (C, u00032, "system__concat_2B");
   u00033 : constant Version_32 := 16#a1d318f8#;
   pragma Export (C, u00033, "system__concat_2S");
   u00034 : constant Version_32 := 16#752a67ed#;
   pragma Export (C, u00034, "system__concat_3B");
   u00035 : constant Version_32 := 16#9e5272ad#;
   pragma Export (C, u00035, "system__concat_3S");
   u00036 : constant Version_32 := 16#63bad2e6#;
   pragma Export (C, u00036, "system__concat_9B");
   u00037 : constant Version_32 := 16#40c7a364#;
   pragma Export (C, u00037, "system__concat_9S");
   u00038 : constant Version_32 := 16#c71e6c8a#;
   pragma Export (C, u00038, "system__exception_tableB");
   u00039 : constant Version_32 := 16#99031d16#;
   pragma Export (C, u00039, "system__exception_tableS");
   u00040 : constant Version_32 := 16#268dd43d#;
   pragma Export (C, u00040, "system__exceptionsS");
   u00041 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00041, "system__exceptions__machineB");
   u00042 : constant Version_32 := 16#46355a4a#;
   pragma Export (C, u00042, "system__exceptions__machineS");
   u00043 : constant Version_32 := 16#7706238d#;
   pragma Export (C, u00043, "system__exceptions_debugB");
   u00044 : constant Version_32 := 16#2426335c#;
   pragma Export (C, u00044, "system__exceptions_debugS");
   u00045 : constant Version_32 := 16#36b7284e#;
   pragma Export (C, u00045, "system__img_intS");
   u00046 : constant Version_32 := 16#f2c63a02#;
   pragma Export (C, u00046, "ada__numericsS");
   u00047 : constant Version_32 := 16#174f5472#;
   pragma Export (C, u00047, "ada__numerics__big_numbersS");
   u00048 : constant Version_32 := 16#5c7d9c20#;
   pragma Export (C, u00048, "system__tracebackB");
   u00049 : constant Version_32 := 16#92b29fb2#;
   pragma Export (C, u00049, "system__tracebackS");
   u00050 : constant Version_32 := 16#5f6b6486#;
   pragma Export (C, u00050, "system__traceback_entriesB");
   u00051 : constant Version_32 := 16#dc34d483#;
   pragma Export (C, u00051, "system__traceback_entriesS");
   u00052 : constant Version_32 := 16#258eaced#;
   pragma Export (C, u00052, "system__traceback__symbolicB");
   u00053 : constant Version_32 := 16#140ceb78#;
   pragma Export (C, u00053, "system__traceback__symbolicS");
   u00054 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00054, "ada__containersS");
   u00055 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00055, "ada__exceptions__tracebackB");
   u00056 : constant Version_32 := 16#26ed0985#;
   pragma Export (C, u00056, "ada__exceptions__tracebackS");
   u00057 : constant Version_32 := 16#0390ef72#;
   pragma Export (C, u00057, "interfaces__cB");
   u00058 : constant Version_32 := 16#1a6d7811#;
   pragma Export (C, u00058, "interfaces__cS");
   u00059 : constant Version_32 := 16#0978786d#;
   pragma Export (C, u00059, "system__bounded_stringsB");
   u00060 : constant Version_32 := 16#63d54a16#;
   pragma Export (C, u00060, "system__bounded_stringsS");
   u00061 : constant Version_32 := 16#9f0c0c80#;
   pragma Export (C, u00061, "system__crtlS");
   u00062 : constant Version_32 := 16#a604bd9c#;
   pragma Export (C, u00062, "system__dwarf_linesB");
   u00063 : constant Version_32 := 16#f38e5e19#;
   pragma Export (C, u00063, "system__dwarf_linesS");
   u00064 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00064, "ada__charactersS");
   u00065 : constant Version_32 := 16#9de61c25#;
   pragma Export (C, u00065, "ada__characters__handlingB");
   u00066 : constant Version_32 := 16#729cc5db#;
   pragma Export (C, u00066, "ada__characters__handlingS");
   u00067 : constant Version_32 := 16#cde9ea2d#;
   pragma Export (C, u00067, "ada__characters__latin_1S");
   u00068 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00068, "ada__stringsS");
   u00069 : constant Version_32 := 16#c5e1e773#;
   pragma Export (C, u00069, "ada__strings__mapsB");
   u00070 : constant Version_32 := 16#6feaa257#;
   pragma Export (C, u00070, "ada__strings__mapsS");
   u00071 : constant Version_32 := 16#b451a498#;
   pragma Export (C, u00071, "system__bit_opsB");
   u00072 : constant Version_32 := 16#d9dbc733#;
   pragma Export (C, u00072, "system__bit_opsS");
   u00073 : constant Version_32 := 16#b459efcb#;
   pragma Export (C, u00073, "ada__strings__maps__constantsS");
   u00074 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00074, "system__address_imageB");
   u00075 : constant Version_32 := 16#b5c4f635#;
   pragma Export (C, u00075, "system__address_imageS");
   u00076 : constant Version_32 := 16#7da15eb1#;
   pragma Export (C, u00076, "system__img_unsS");
   u00077 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00077, "system__ioB");
   u00078 : constant Version_32 := 16#8a6a9c40#;
   pragma Export (C, u00078, "system__ioS");
   u00079 : constant Version_32 := 16#e15ca368#;
   pragma Export (C, u00079, "system__mmapB");
   u00080 : constant Version_32 := 16#da9a152c#;
   pragma Export (C, u00080, "system__mmapS");
   u00081 : constant Version_32 := 16#367911c4#;
   pragma Export (C, u00081, "ada__io_exceptionsS");
   u00082 : constant Version_32 := 16#dd82c35a#;
   pragma Export (C, u00082, "system__mmap__os_interfaceB");
   u00083 : constant Version_32 := 16#37fd3b64#;
   pragma Export (C, u00083, "system__mmap__os_interfaceS");
   u00084 : constant Version_32 := 16#c8a05a18#;
   pragma Export (C, u00084, "system__mmap__unixS");
   u00085 : constant Version_32 := 16#29c68ba2#;
   pragma Export (C, u00085, "system__os_libB");
   u00086 : constant Version_32 := 16#ee44bb50#;
   pragma Export (C, u00086, "system__os_libS");
   u00087 : constant Version_32 := 16#94d23d25#;
   pragma Export (C, u00087, "system__atomic_operations__test_and_setB");
   u00088 : constant Version_32 := 16#57acee8e#;
   pragma Export (C, u00088, "system__atomic_operations__test_and_setS");
   u00089 : constant Version_32 := 16#d34b112a#;
   pragma Export (C, u00089, "system__atomic_operationsS");
   u00090 : constant Version_32 := 16#553a519e#;
   pragma Export (C, u00090, "system__atomic_primitivesB");
   u00091 : constant Version_32 := 16#5f776048#;
   pragma Export (C, u00091, "system__atomic_primitivesS");
   u00092 : constant Version_32 := 16#b98923bf#;
   pragma Export (C, u00092, "system__case_utilB");
   u00093 : constant Version_32 := 16#db3bbc5a#;
   pragma Export (C, u00093, "system__case_utilS");
   u00094 : constant Version_32 := 16#256dbbe5#;
   pragma Export (C, u00094, "system__stringsB");
   u00095 : constant Version_32 := 16#8faa6b17#;
   pragma Export (C, u00095, "system__stringsS");
   u00096 : constant Version_32 := 16#edf7b7b1#;
   pragma Export (C, u00096, "system__object_readerB");
   u00097 : constant Version_32 := 16#87571f07#;
   pragma Export (C, u00097, "system__object_readerS");
   u00098 : constant Version_32 := 16#75406883#;
   pragma Export (C, u00098, "system__val_lliS");
   u00099 : constant Version_32 := 16#838eea00#;
   pragma Export (C, u00099, "system__val_lluS");
   u00100 : constant Version_32 := 16#47d9a892#;
   pragma Export (C, u00100, "system__sparkS");
   u00101 : constant Version_32 := 16#a571a4dc#;
   pragma Export (C, u00101, "system__spark__cut_operationsB");
   u00102 : constant Version_32 := 16#629c0fb7#;
   pragma Export (C, u00102, "system__spark__cut_operationsS");
   u00103 : constant Version_32 := 16#1bac5121#;
   pragma Export (C, u00103, "system__val_utilB");
   u00104 : constant Version_32 := 16#b851cf14#;
   pragma Export (C, u00104, "system__val_utilS");
   u00105 : constant Version_32 := 16#bad10b33#;
   pragma Export (C, u00105, "system__exception_tracesB");
   u00106 : constant Version_32 := 16#f8b00269#;
   pragma Export (C, u00106, "system__exception_tracesS");
   u00107 : constant Version_32 := 16#fd158a37#;
   pragma Export (C, u00107, "system__wch_conB");
   u00108 : constant Version_32 := 16#cd2b486c#;
   pragma Export (C, u00108, "system__wch_conS");
   u00109 : constant Version_32 := 16#5c289972#;
   pragma Export (C, u00109, "system__wch_stwB");
   u00110 : constant Version_32 := 16#e03a646d#;
   pragma Export (C, u00110, "system__wch_stwS");
   u00111 : constant Version_32 := 16#7cd63de5#;
   pragma Export (C, u00111, "system__wch_cnvB");
   u00112 : constant Version_32 := 16#cbeb821c#;
   pragma Export (C, u00112, "system__wch_cnvS");
   u00113 : constant Version_32 := 16#e538de43#;
   pragma Export (C, u00113, "system__wch_jisB");
   u00114 : constant Version_32 := 16#7e5ce036#;
   pragma Export (C, u00114, "system__wch_jisS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.atomic_operations%s
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  system.spark%s
   --  system.spark.cut_operations%s
   --  system.spark.cut_operations%b
   --  system.storage_elements%s
   --  system.stack_checking%s
   --  system.stack_checking%b
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
   --  stm32f4%s
   --  stm32f4.gpio%s
   --  stm32f4.reset_clock_control%s
   --  stm32f4.sysconfig_control%s
   --  registers%s
   --  leds%s
   --  leds%b
   --  blink_led%b
   --  END ELABORATION ORDER

end ada_main;
