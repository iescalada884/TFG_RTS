# TFG_RTS

## Author
    > Ismael Escalada Diego
    > TFG Directors: Mario Aldea Rivas, Héctor Perez

## Version
- **GNAT Native:** 14.2.1  
- **GNAT Cross Compiler:** 14.2.0  

## Requirements
To build and compile this project, ensure you have the following dependencies installed:

- **GNAT Studio**
- **GPRBuild**
- **Alire (alr)**
- **From GCC Master Repository:**
  - `gcc/ada/libgnat/libgnat_common.gpr`
  - `gcc/ada/libgnat/libgnarl.gpr`
  - `gcc/ada/libgnat/libgnat.gpr`
- **GNAT Native**
    ```sh
    alr get gnat_native~14.2
    ```
- **For Cross-Compilation:**
  - `gnat-arm-elf-linux64-x86_64-14.2.0-1`



## Marte compilation
1. Export path of cross compiler and gprbuild and marte folder
    ```sh
    PATH=.../TFG_RTS/marte_28mar2023/utils/:$PATH; 
    PATH=~/Documents/gprbuild_22.0.1_24dfc1b5/bin:$PATH; PATH=~/Downloads/gnat-arm-elf-linux64-x86_64-14.2.0-1/bin:$PATH
    ```
2. now go to marte_28mar2023 and execute .minstall
    ```sh
    ./minstall
    ```
3. set the desired arch
    ```sh
    msetcurrentarch stm32f f4
    ```
4. compile the rts and marte
    ```sh
     mkrtsmarteuc && mkmarte
    ```

## RTS Native Compilation
To compile natively, follow these steps:

1. Copy from `gnat_native` `lib/gcc/x86_64-pc-linux-gnu/14.2.0/adainclude`  folder into your RTS folder.
2. Navigate to the `Adalib` folder and execute `list_gen.sh` to generate `.lst` files.
    ```sh
    ./list_gen.sh libgnat.a libgnat.lst
    ./list_gen.sh libgnarl.a libgnarl.lst
    mv *.list <rts_folder_path>/adainclude
   ```
3. Create adalib and obj-static in your rts folder

3. Add gnat-native /bin folder to path and gprbuild
    ```sh
    PATH=~/Documents/gnat-native/bin:$PATH
    PATH=~/Documents/gprbuild_22.0.1_24dfc1b5/bin:$PATH
    ``` 
4. Create obj-static in your rts folder

3. Create adalib and obj-static in your rts folder

5. Run the build command:
   ```sh
   gprbuild -P libada
   ```

## RTS Cross-Compilation
For cross-compiling the project, follow these steps:
1. Copy Adainclude folder from Debian gnat
2. use listgen.sh in debian adalib to generate .lsts
3. Copy gprs into adainclude
4. Add cross-compiler and gprbuild to path
    ```sh
    PATH=~/Downloads/gnat-arm-elf-linux64-x86_64-14.2.0-1/bin:$PATH
    PATH=~/Documents/gprbuild_22.0.1_24dfc1b5/bin:$PATH
    ``
5. Create adalib and obj-static folders
6. Run the build command with the target specification:
   ```sh
   gprbuild -P libada --target=arm-eabi
   ```

