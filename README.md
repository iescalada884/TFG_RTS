# TFG_RTS

## Author
    > Ismael Escalada Diego
    > TFG Directors: Mario Aldea Rivas, Héctor Perez

## Version
- **GNAT Native:** 14.2.1  
- **GNAT Cross Compiler:** 14.2.0  

## Requisites
To build and compile this project, ensure you have the following dependencies installed:

- **GNAT Studio**
- **GPRBuild**
- **Alire (alr)**
- **From GCC Master Repository:**
  - `gcc/ada/libgnat/libgnat_common.gpr`
  - `gcc/ada/libgnat/libgnarl.gpr`
  - `gcc/ada/libgnat/libgnat.gpr`
- **GNAT Native**
- **For Cross-Compilation:**
  - `gnat-arm-elf-linux64-x86_64-14.2.0-1`

## Native Compilation
To compile natively, follow these steps:

1. Copy from `gnat_native` `lib/gcc/x86_64-pc-linux-gnu/14.2.0/adainclude`  folder into your RTS folder.
2. Naviigate to the `Adalib` folder and execute `list_gen.sh` to generate `.lst` files.
    ```sh
    ./list_gen.sh libgnat.a libgnat.lst
    ./list_gen.sh libgnarl.a libgnarl.lst
    mv *.list <rts_folder_path>/adainclude
   ```
3. Create obj-static in your rts folder

4. Run the build command:
   ```sh
   gprbuild -P libada
   ```

## Cross-Compilation
For cross-compiling the project, follow these steps:

1. Add cross-compiler and gprbuild to path
    ```sh
    PATH=~/Downloads/gnat-arm-elf-linux64-x86_64-14.2.0-1/bin:$PATH
    PATH=~/Documents/gprbuild_22.0.1_24dfc1b5/bin:$PATH
    ``    
2. Run the build command with the target specification:
   ```sh
   gprbuild -P libada --target=arm-eabi
   ```

