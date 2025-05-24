# Do NOT edit!!
# Created by 'minstall'
$MPATH="/home/ubuntfg/Documents/TFG_RTS/marte_28mar2023";

@AVAILABLE_ARCHS = ("stm32f");

%GNAT_BIN_PATH = (
	"stm32f" => "/home/ubuntfg/Downloads/gnat-arm-elf-linux64-x86_64-14.2.0-1/bin",
	"none" => "none");

%GNAT_LIBS_PATH = (
	"x86" => "/x86_not_available",
	"linux" => "/linux_not_available",
	"linux_lib" => "/linux_lib_not_available",
	"rpi" => "/rpi_not_available",
	"gnat_arm_bb" => "/gnat_arm_bb_not_available",
	"stm32f" => "/home/ubuntfg/Downloads/gnat-arm-elf-linux64-x86_64-14.2.0-1/lib/gcc/arm-eabi/14.2.0",
	"ev3" => "/ev3_not_available",
	"none" => "none");

%GNAT_VERSION = (
	"stm32f" => "",
	"none" => "none");

return 1;