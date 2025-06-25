/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *
 *                      'w r a p p e r _ m a i n _ a d a'
 *
 *                                      C
 *
 * File 'wrapper_main_ada.c'                                          by MAR.
 *
 * GNAT ARM BB version.
 *
 * Defines the 'main' function of a "MaRTE OS on ARM" program.  At
 * linking time is included either this file or 'wrapper_main_c.o'
 * depending on the language of the main procedure of the application.
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
// name of the main function given by the binder (option "-Muser_main")

extern void marteinit();  // Elaboration of MaRTE Ada packages

extern void direct_write_on_stdout (char * str, int count); // 'Direct_IO'
extern void basic_stdout_initialization (); //  From 'Direct_IO'
extern void basic_stderr_initialization (); //  From 'Direct_IO'

int wrapper_main(int argc, char *argv[], char *envp[])
{
  basic_stderr_initialization ();
  basic_stdout_initialization ();
  direct_write_on_stdout
    ("                       -=  M a R T E   O S  =-\n", 47);
  direct_write_on_stdout
    ("                           V2.0 2019-05-24\n", 44);
  direct_write_on_stdout
    ("            Copyright (C) Universidad de Cantabria, SPAIN\n", 58);

  //init_dynamic_memory (); // Initialize dynamic_memory

  marteinit(); // Call elaboration of MaRTE Ada packages

  return (user_main(argc, argv, envp));
}
