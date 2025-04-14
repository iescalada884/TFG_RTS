# Global Data

@ALL_ARCHS = ("x86", "linux", "linux_lib", "rpi", "gnat_arm_bb", "stm32f", "ev3");

# Supported architectures
#@SUPPORTED_ARCHS=("x86", "linux", "linux_lib", "rpi", "gnat_arm_bb");
#@SUPPORTED_ARCHS=("x86", "linux", "linux_lib", "rpi", "gnat_arm_bb", "stm32f");
@SUPPORTED_ARCHS=("x86", "rpi", "stm32f", "ev3");


# Compiler prefix
%COMPILER_PREFIX=("x86" =>       "",
		  "linux" =>     "",
		  "linux_lib" => "",
		  "rpi" =>       "arm-eabi-",
		  "gnat_arm_bb" => "arm-eabi-",
		  "stm32f" => "arm-eabi-",
		  "ev3"    => "arm-eabi-",
		  "xtratum" => "");

# Includes
# I also want to use the standard includes in linux_lib architecture
%INCLUDES=("x86",       "-nostdinc -I$MPATH/arch/include",
           "linux",     "-nostdinc -I$MPATH/arch/include",
           "linux_lib", "-I$MPATH/arch/include",
	   "rpi",       "-nostdinc -I$GNAT_LIBS_PATH{rpi}/include -I$MPATH/arch/include",
	   "gnat_arm_bb" => "-nostdinc -I$MPATH/arch/include",
	   "stm32f" => "-nostdinc -I$MPATH/arch/include",
	   "ev3"    => "-nostdinc -I$MPATH/arch/include",
	   "xtratum", "-nostdinc -I$MPATH/arch/include");

# GNAT RTS
%GNAT_RTS = 
    ("x86"       => "marteuc_x86",
     "linux"     => "marteuc_linux",
     "linux_lib" => "marteuc_linux_lib",
     "rpi"       => "$MPATH/gnat_rts/rts-marteuc_rpi",
     "gnat_arm_bb" => "$GNAT_BIN_PATH{$CURRENT_ARCH}/../arm-eabi/lib/gnat/marte-stm32f4",
     "stm32f"      => "$GNAT_BIN_PATH{$CURRENT_ARCH}/../arm-eabi/lib/gnat/zfp-stm32f4/",
     "ev3"         => "$GNAT_BIN_PATH{$CURRENT_ARCH}/../arm-eabi/lib/gnat/zfp-stm32f4/",
     "xtratum"   => "marteuc_xtratum",
     "none" => " ARCH_VALUE_NOT_SET ");

# Libs
%LIBS_PATH_PROC_STM32=("f4" => " ",
                       "f7" => " -L$MPATH/objs/stm32f_objs/stm32cube_fw_f7/",
                        "i386" => " UNEXPECTED_PROC_VALUE ",
                        "pi"   => " UNEXPECTED_PROC_VALUE ",
                        "pii"  => " UNEXPECTED_PROC_VALUE ",
		        "none" => " PROC_VALUE_NOT_SET ");
%LIBS_PATH=("x86", "-L$MPATH/lib -L$MPATH/gnat_rts/rts/adalib " .
	    "-L$GNAT_LIBS_PATH{x86}",
            "linux", "-L$MPATH/lib -L$MPATH/gnat_rts/rts/adalib " .
            "-L/usr/lib32 -L/usr/lib -L/usr/lib/gcc/x86_64-linux-gnu/4.8/32",
            "linux_lib","-L$MPATH/lib -L$MPATH/gnat_rts/rts/adalib " .
            "-L/usr/lib32 -L/usr/lib -L/usr/lib/gcc/x86_64-linux-gnu/4.8/32 ",
	    "rpi", "-L$MPATH/lib -L$MPATH/gnat_rts/rts/adalib -L$MPATH/rpi_arch ",
	    "gnat_arm_bb", "-L$MPATH/lib ",
	    "stm32f"      => $LIBS_PATH_PROC_STM32{$CURRENT_PROC} .
                             " -L$MPATH/lib/libmarte/ " .
                             "-L$GNAT_RTS{$CURRENT_ARCH}/adalib/ ",
	    "ev3"         => " -L$MPATH/lib/libmarte/ " .
                             "-L$GNAT_RTS{$CURRENT_ARCH}/adalib/ "
                      #"-L,$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/arm-eabi/7.4.1/thumb/v7e-m/fpv4-sp/hard/," .
                      #"-L,$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/arm-eabi/7.4.1/," .
                      #"-L,$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/," .
                      #"-L,$GNAT_BIN_PATH{$CURRENT_ARCH}/../arm-eabi/lib/,"
           );
	    #"xtratum", "-L$MPATH/lib -L$MPATH/gnat_rts/rts/adalib " .
	    #"-L$GNAT_LIBS_PATH{xtratum}");

%LIBS_GCC_PROC_STM32=("f4" => " ",
                      "f7" => "-lstm32cube_fw_f7",
                      "i386" => " UNEXPECTED_PROC_VALUE ",
                      "pi"   => " UNEXPECTED_PROC_VALUE ",
                      "pii"  => " UNEXPECTED_PROC_VALUE ",
		      "none" => " PROC_VALUE_NOT_SET ");
%LIBS_GCC=("x86",       "-lmarte -lgnarl -lgnat -lmarte -lgcc_sjlj",
           "linux",     "-lmarte -lgnarl -lgnat -lmarte",
           "linux_lib", "-lmarte -lgnarl -lgnat -lmarte",
	   "rpi",       "-lmarte -lgnarl -lgnat -lmarte -lgcc -lgcov",
	   "gnat_arm_bb", "-Wl,--start-group -lmarte -lgnarl -lgnat -lmarte -lgcc -lgcov -lc -Wl,--end-group",
	   "stm32f"    => "-Wl,--start-group -lmarte -lgnat -lmarte -lgcc -lc -Wl,--end-group " .
                          $LIBS_GCC_PROC_STM32{$CURRENT_PROC}, # add -lgnarl when full RTS
	   "ev3"      => "-Wl,--start-group -lmarte -lgnat -lmarte -lgcc -lc -Wl,--end-group ", # add -lgnarl when full RTS
	   "xtratum",       "-lmarte -lgnarl -lgnat -lmarte");

%ENV_VARS_VS_ARCH=("x86",       "",
		   "linux",     "LIBRARY_PATH=/usr/lib32 LD_LIBRARY_PATH=/usr/lib32 ",
		   "linux_lib", "LIBRARY_PATH=/usr/lib32 LD_LIBRARY_PATH=/usr/lib32 ",
		   "rpi",       "",
		   "gnat_arm_bb",  "",
		   "stm32f",  "",
		   "ev3",     "",
		   "xtratum",   ""); 
	   
# GNAT_LIBS_DIRS
# Added by gcc when linking programs
%GNAT_LIBS_DIRS=
   ("GPL2009" =>
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/i686-pc-linux-gnu/4.3.4 " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/i686-pc-linux-gnu/4.3.4/../../..",
    "GPL2010" =>
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/i686-pc-linux-gnu/4.3.6 " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/i686-pc-linux-gnu/4.3.6/../../..",
    "GPL2012" =>
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/i686-pc-linux-gnu/4.5.4 " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/i686-pc-linux-gnu/4.5.4/../../..",
    "GPL2014" =>
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/i686-pc-linux-gnu/4.7.4 " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/i686-pc-linux-gnu/4.7.4/../../..",
    "GPL2016" =>
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/x86_64-pc-linux-gnu/4.9.4 " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/x86_64-pc-linux-gnu/4.9.4/../../..",
    "GPL2017" =>
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/x86_64-pc-linux-gnu/6.3.1 " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/x86_64-pc-linux-gnu/6.3.1/../../..",
    "Community2018" =>
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc/arm-eabi/7.3.1/ " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib/gcc " .
    "-L$GNAT_BIN_PATH{$CURRENT_ARCH}/../lib",
    "Community2019" =>
    " LIBS_Community2019_NOT_USED ");

# Especial options to compile MaRTE kernel
$GNAT_COMPILE_KERNEL_OPS="-fno-strict-aliasing";

# Architecture dependent gcc options
%ARCH_GCC_OPTS=("x86",       " ",
                "linux",     " -Dmain=user_main ",
                "linux_lib", " -Dmain=user_main ",
		"rpi",       " ",
		"gnat_arm_bb" => " -Dmain=user_main ",
		"stm32f"      => " -Dmain=user_main ",
		"ev3"         => " -Dmain=user_main ",
		"xtratum",       "-D" . '"' . "__XM_INCFLD(_fld)=<xm_inc/_fld>" . '"');

# Code generation options for gcc and gnatmake
%CODE_GEN_OPTS_PROC_X86=("i386" => " -m32 -march=i386 ",
                         "pi"   => " -m32 -march=i586 ",
                         "pii"  => " -m32 -march=i686 ",
                         "f4" => " UNEXPECTED_PROC_VALUE ",
                         "f7" => " UNEXPECTED_PROC_VALUE ",
			 "none" => " PROC_VALUE_NOT_SET ");
%CODE_GEN_OPTS_PROC_STM32=("f4" => " -mlittle-endian -mhard-float -mcpu=cortex-m4 -mfpu=fpv4-sp-d16 -mthumb ",
                           "f7" => " -x c -mlittle-endian -mhard-float -mcpu=cortex-m7 -mfpu=fpv5-d16 -mthumb ",
                           "i386" => " UNEXPECTED_PROC_VALUE ",
                           "pi"   => " UNEXPECTED_PROC_VALUE ",
                           "pii"  => " UNEXPECTED_PROC_VALUE ",
			   "none" => " PROC_VALUE_NOT_SET ");
%CODE_GEN_OPTS=("x86",       $CODE_GEN_OPTS_PROC_X86{$CURRENT_PROC}, #" -m32 -march=i386",
                "linux",     $CODE_GEN_OPTS_PROC_X86{$CURRENT_PROC} . " -m32 -march=i686 ",
                "linux_lib", $CODE_GEN_OPTS_PROC_X86{$CURRENT_PROC} . " -m32 -march=i686 ",
		"rpi",       " -mcpu=arm1176jzf-s -marm ", #-march=armv6 -mthumb-interwork -mthumb",
		"gnat_arm_bb" => " -mlittle-endian -mhard-float -mcpu=cortex-m4 -mfpu=fpv4-sp-d16 -mthumb -ffunction-sections -fdata-sections ",
		"stm32f"      => " $CODE_GEN_OPTS_PROC_STM32{$CURRENT_PROC} -ffunction-sections -fdata-sections ",
		"ev3"      => " -ffreestanding -mcpu=ARM926EJ-s -ffunction-sections -fdata-sections ",
		"xtratum",   " -m32 -march=i686 ");

# Architecture dependent gnatbind options
%ARCH_GNATBIND_OPTS=("x86",       " -F ",
                     "linux",     " -F -Muser_main ",
                     "linux_lib", " -F -Muser_main ",
		     "rpi",       " -F -Muser_main ",
		     "gnat_arm_bb" => " -F -Muser_main ",
		     "stm32f"      => " -F -Muser_main ",
		     "ev3"         => " -F -Muser_main ",
		     "xtratum",       " -F ");

# Architecture dependent linking options for gcc
%ARCH_GCC_LD_OPTS=
    ("x86", " -Wl,-T,$MPATH/utils/linker.lds -static -nostartfiles",
     "linux", " ",
     "linux_lib", " ",
     "rpi", " -nodefaultlibs -nostdlib -nostartfiles -static -Wl,--no-wchar-size-warning,--no-undefined,-T,$MPATH/rpi_arch/marte_rpi.ld,-Map,mprogram.map  ",
     "gnat_arm_bb" => " -nodefaultlibs -nostdlib -nostartfiles -Wl,--gc-sections -Wl,-L,$GNAT_BIN_PATH{$CURRENT_ARCH}/../arm-eabi/lib/gnat/marte-stm32f4/adalib,-L,$GNAT_BIN_PATH{$CURRENT_ARCH}/../arm-eabi/lib/gnat/marte-stm32f4/ld,-T,memory-map.ld,-T,common-ROM.ld,-Map=map.txt ",
     "stm32f"      => " -nodefaultlibs -nostdlib -nostartfiles -Wl,--gc-sections " .
                      "-Wl,-L,$GNAT_RTS{$CURRENT_ARCH}/ld/," .
                      "-T,memory-map.ld,-T,common-ROM.ld,-Map=map.txt ",
     "ev3"         => " -nodefaultlibs -nostdlib -nostartfiles -Wl,--gc-sections " .
                      "-Wl,-T,falta_linker_script_en_globals.ld,-Map=map.txt ",
     "xtratum", "no_link_form_script_in_this_architecture");

# Architecture dependent c++ library
%ARCH_CXX_LIB=("x86", " -lsupc++ -lustl ",
               "linux", "  -lsupc++ -lustl ",
               "linux_lib", "  -lstdc++ ",
               "rpi", " -lsupc++ -lustl ",
               "xtratum", " -lsupc++ -lustl ");

# Architecture dependent make options
%ARCH_MAKE_OPTS=("x86", " ",
                 "linux", " VIRTUAL_TIMER=USE_ITIMER ",
                 "linux_lib", " VIRTUAL_TIMER=USE_ITIMER ",
                 "rpi", " ",
		 "gnat_arm_bb" => " ",
		 "stm32f"      => " ",
		 "ev3"         => " ",
                 "xtratum", " ");

# MaRTE Ada Code Locations
$MARTE_ADA_SRC_LOCATIONS="-aI$MPATH/kernel -aI$MPATH/sll " .
    "-aI$MPATH/arch/hwi -aI$MPATH/misc -aI$MPATH/posix5";

# Architecture depending objects files

%_EXIT_O=("x86", "  ",
          "linux", "$MPATH/arch/call_main/_exit.o ",
          "linux_lib", "$MPATH/arch/call_main/_exit.o ",
          "rpi", "  ",
          "xtratum", "$MPATH/arch/call_main/_exit.o ");

%WRAPPER_MAIN=("c", " $MPATH/arch/call_main/wrapper_main_c.o ",
               "cxx", " $MPATH/arch/call_main/wrapper_main_cpp.o ",
               "ada", " $MPATH/arch/call_main/wrapper_main_ada.o ");

# Objects files location
#$KERNEL_OBJS_LIBMARTE="$MPATH/kernel/*.o $MPATH/arch/hwi/*.o $MPATH/sll/*.o $MPATH/misc/*.o $MPATH/kernel/dynamic_memory/*.o ";

# Remove unneeded from the kernel
%RM_UNNEEDED=
   ("x86"       => "objcopy -O elf32-i386 --remove-section=.comment --remove-section=.note --strip-unneeded ",
    "linux"     => "objcopy -O elf32-i386 --remove-section=.comment --remove-section=.note --strip-unneeded ",
    "linux_lib" => "objcopy -O elf32-i386 --remove-section=.comment --remove-section=.note --strip-unneeded ",
    "rpi"       => "  ",
    "xtratum"   => "  ");
# $RM_UNNEEDED="objcopy -O elf32-i386 --remove-section=.comment --remove-section=.anno --remove-section=.note --remove-section=.eh_frame --remove-section=.ctors --remove-section=.dtors --strip-all $PROG_NAME";


#-----------------------------------------------------
# Some utility functions

# subfunction is_arch_rts_compiled
sub is_arch_rts_compiled
{
    my $arch = shift(@_);
    if ($arch eq "stm32f") {
       return 1;
    }
    return (-e "$MPATH/gnat_rts/rts-marteuc_$arch/adainclude" ||
            -e "$MPATH/gnat_rts/rts-marteuc_$arch/ada_source_path") &&
	-e "$MPATH/gnat_rts/rts-marteuc_$arch/adalib" &&
	-e "$MPATH/gnat_rts/rts-marteuc_$arch/adalib/libgnat.a";
}

# subfunction is_arch_libmarte_compiled
sub is_arch_libmarte_compiled
{
    my $arch = shift(@_);
    my $objs = "objs/${arch}_objs";
    if ($arch eq "stm32f" || $arch eq "ev3") {
        $objs .= "/libmarte";
    }
    return -e "$MPATH/$objs/libmarte.a";
}

# subfunction exec_command
sub exec_command
{
    my $cmd = shift(@_);
    printf "$cmd\n";
    system ($cmd) == 0 or die "Can't execute $cmd\n";
}
sub exec_command_no_echo
{
    my $cmd = shift(@_);
    #printf "$cmd\n";
    system ($cmd) == 0 or die "Can't execute $cmd\n";
}

# subfunction drivers_directories
# Returns all the driver directories for one architecture
sub drivers_directories
{
    my $arch = shift(@_);
    my $arch_dir="$MPATH/${arch}_arch"; # architecture directory
    my $drivers_dirs = "";
    my $file = "";
    opendir DIR, "$arch_dir/drivers" or
	die "$arch_dir/drivers directory not found";
    my @all_files = grep !/^\./, readdir DIR;
    foreach $file (@all_files) {
	if (-d "$arch_dir/drivers/$file") {
	    $drivers_dirs .= " -aI$arch_dir/drivers/$file";
	}
    }
    return $drivers_dirs . " ";
}

# get output of a shell command
sub get_cmd_output
{
    my $cmd = shift(@_);

    open SYS_OUT, "$cmd |";
    my $ret = <SYS_OUT>;
    close SYS_OUT;
    
    $ret = "" if (!$ret); # initialize if empty
    $ret =~ s/\n//; # remove endig \n

    return $ret;
}

# Write 'current_arch.pl' file
#
# Params
#    arch
#    proc
sub write_current_arch_file
{
    my $arch = shift;
    my $proc = shift;

    my $file_name="$MPATH/utils/current_arch.pl";

    if (-e "$file_name") {
	exec_command_no_echo "rm -f $file_name";
    }
    open ARCH_F, ">$file_name" or die "Couldn't open $file_name";

    print ARCH_F "#  automatically generated by msetcurrentarch\n";
    print ARCH_F "\$CURRENT_ARCH=\"$arch\";\n";
    print ARCH_F "\$CURRENT_PROC=\"$proc\";\n";
    print ARCH_F "return 1;\n";

    close ARCH_F;
}


# is_arch_available
#
# Check if the arch is in @AVAILABLE_ARCHS (local_config.pl)
# Param:
#   arch
# Return
#   available (1) or not (0)
sub is_arch_available
{
    my $arch = shift;
    my $i=0;
    for($i = 0; $i < @AVAILABLE_ARCHS; $i++) {
	return 1 if ($arch eq $AVAILABLE_ARCHS[$i]);
    }
    return 0;
}
