/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *
 *                        'w r a p p e r _ m a i n _ c'
 *
 *                                      C
 *
 * File 'wrapper_main_c.c'                                             by MAR.
 *
 * SRM32F version.
 *
 * Defines the 'main' function of a "MaRTE OS on ARM" C program.
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *  MaRTE OS web page: http://marte.unican.es
 *  Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                     Michael Gonzalez Harbour      mgh@unican.es
 *
 * MaRTE OS  is free software; you can  redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as published by the
 * Free Software Foundation;  either  version 2, or (at  your option) any
 * later version.
 *
 * MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 * WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 * MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 * General Public License for more details.
 *
 * You should have received  a  copy of  the  GNU General Public  License
 * distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 * Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 * 02111-1307, USA.
 *
 * As a  special exception, if you  link this  unit  with other  files to
 * produce an   executable,   this unit  does  not  by  itself cause  the
 * resulting executable to be covered by the  GNU General Public License.
 * This exception does  not however invalidate  any other reasons why the
 * executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/

//extern int init_dynamic_memory(); // 'dynamic_memory/init_dynamic_memory.c'

extern int user_main(int argc, char *argv[], char *envp[]);
extern void marteinit();  // MaRTE Ada packages initialization
//extern void martefinal(); // MaRTE Ada packages finalization
extern void __gnat_runtime_initialize(); // s-init.adb
extern char __is_marte_initialized;
//extern void marte_c_init();  // MaRTE Ada packages initialization
//extern void marte_c_final(); // MaRTE Ada packages finalization
extern void init_lang_supp (void);
extern void finish_lang_supp (void);

extern void direct_write_on_stdout (char * str, int count); // 'Direct_IO'
extern void basic_stdout_initialization (); //  From 'Direct_IO'
extern void basic_stderr_initialization (); //  From 'Direct_IO'

extern void marte__hal__disable_interrupts();
extern void marte__hal__finish_hardware_use ();

// To execute the static C++ constructors. Section .init_array must exists in
// the linker scripts (see linker scripts in stm32f_arch/stm32f769disco_ld) and
// -Lstm32f_arch/stm32f769disco_ld must be added in stm32_shared_switches.gpr
static void callConstructors()
{
    // Start and end points of the constructor list,
    // defined by the linker script.
    extern void (*__init_array_start)();
    extern void (*__init_array_end)();

    // Call each function in the list.
    // We have to take the address of the symbols, as __init_array_start *is*
    // the first function pointer, not the address of it.
    for (void (**p)() = &__init_array_start; p < &__init_array_end; ++p) {
        (*p)();
    }
}

#if defined(MaRTE_stm32f769disco)
// To force inclusion of premain.cpp (stm32f_arch/hwi/stm32duino/) in the
// application executable file
extern int to_include_premain_marte_os;
#endif

void _ZSt25__throw_bad_function_callv() {
  }

int main(int argc, char *argv[], char *envp[])
{
  int ret;

  basic_stderr_initialization ();
  basic_stdout_initialization ();
  direct_write_on_stdout
    ("                       -=  M a R T E   O S  =-\n", 47);
  direct_write_on_stdout
    ("                           V2.0 2019-05-24\n", 43);
  direct_write_on_stdout
    ("            Copyright (C) Universidad de Cantabria, SPAIN\n", 58);

  //init_dynamic_memory (); // Initialize dynamic_memory

  marteinit(); // Call elaboration of MaRTE Ada packages
//  __is_marte_initialized = 1;
//  __gnat_runtime_initialize(); // s-init.adb
  // marte_c_init(); // Call elaboration of marte_os_c.ads
  // marte_c_init() Not required in ARM since there are not Ada drivers
  // with tasks in this architecture.

  //init_lang_supp(); no C++ support in ARM yet

#if defined(MaRTE_stm32f769disco)
  // Call C++ static constructors
  //to_include_premain_marte_os = 1;
  callConstructors();
#endif

  ret = user_main(argc, argv, envp);

  //finish_lang_supp(); ARM

  //marte_c_final(); ARM
  //martefinal();


#if defined(MaRTE_stm32f769disco)
  // end program. Only in stm32f769disco, since in STM32F4 we want the program
  // to finish in order to finish qemu when running tests
  direct_write_on_stdout("--- User's main finished; Halted ---\n", 37);
  marte__hal__disable_interrupts();
  marte__hal__finish_hardware_use ();
  while(1);
#endif

  return ret;
}
