#!/bin/bash

# Changes files in GNAT to create the MaRTE RTS

#set -o xtrace  # show commands

printf "\nTODO: create_marte_rts_in_gnat.sh: create arm-eabi/BSPs/cortex-m/stm32/stm32f4/marte/ directory\n"
#arm-eabi/BSPs/cortex-m/stm32/stm32f4$ cp -r ravenscar-full/ marte/
#cd marte
#$ rm -f adalib/* obj/*

# Get SRC PATH
cd rts-marte-files/
RTS_SRC_PATH="`pwd`"
readonly RTS_SRC_PATH

# GNAT PATH
GNAT_PATH="/home/mario/gnat-arm-gpl-2017"
readonly GNAT_PATH
printf "\nGNAT 2017 in dir: $GNAT_PATH\n\n"

# Link each entry in the RTS src directory
for entry in *; do 
	
    # skip security copies   
    if [[ ${entry} = *"~" ]]; then
	printf "Skipping ${entry}\n\n"
	continue
    fi
    
    # Get entry name and entry path
    entry_full_path=${entry//__//} # replace __ with /
    entry_path="$(dirname $entry_full_path)"
    entry_name="$(basename $entry_full_path)"
    printf "$entry -> \n   $GNAT_PATH/$entry_path\n   $entry_name\n"

    # create copy of the original file or directory (only if it does not exists)
     if [ ! -e $GNAT_PATH/${entry_full_path}.orig ]; then
 	mv $GNAT_PATH/$entry_full_path $GNAT_PATH/${entry_full_path}.orig
     fi

    # remove link (if it exists)
    rm -f $GNAT_PATH/$entry_full_path
    
    # Create link in the gnat directory
    cd $GNAT_PATH/$entry_path && ln -s ${RTS_SRC_PATH}/${entry}  ${entry_name}
    
    #echo "`pwd`"

#    if [ -d "${entry}" ]; then
#	printf "Directory\n\n"
#    else
#	printf "File\n\n"
#    fi		      
done

printf "\nLinks created in $GNAT_PATH GNAT installation\n"
printf "To create and install RTS run: gprclean, gprbuild y gprinstall\n"
#printf "gprbuild -P $GNAT_PATH/arm-eabi/BSPs/ravenscar_sfp_stm32f4.gpr"
