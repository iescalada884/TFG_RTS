# Do NOT edit!!
# Created by 'minstall'
$MPATH="/home/ubuntfg/Documents/marte_28mar2023";

@AVAILABLE_ARCHS = ("stm32f","ev3");

%GNAT_BIN_PATH = (
	"stm32f" => "/home/ubuntfg/opt/GNAT/2019-arm-elf/bin",
	"ev3" => "/home/ubuntfg/opt/GNAT/2019-arm-elf/bin",
	"none" => "none");

%GNAT_LIBS_PATH = (
	"x86" => "/x86_not_available",
	"linux" => "/linux_not_available",
	"linux_lib" => "/linux_lib_not_available",
	"rpi" => "/rpi_not_available",
	"gnat_arm_bb" => "/gnat_arm_bb_not_available",
	"stm32f" => "/home/ubuntfg/opt/GNAT/2019-arm-elf/lib/gcc/arm-eabi/7.4.1",
	"ev3" => "/home/ubuntfg/opt/GNAT/2019-arm-elf/lib/gcc/arm-eabi/7.4.1",
	"none" => "none");

%GNAT_VERSION = (
	"stm32f" => "Community2019",
	"ev3" => "Community2019",
	"none" => "none");

return 1;