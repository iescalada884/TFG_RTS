/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                           I N I T I A L I Z E                            *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2014-2016, Free Software Foundation, Inc.       *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 *                                                                          *
 *                                                                          * 
 *                                                                          * 
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/
/* MaRTE OS version */

/*  This unit provides implementation for __gnat_runtime_initialize ()
    which is called in adainit() to do special initialization needed by
    the GNAT runtime.  */

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

int __gnat_rt_init_count = 0;

/***********************************************/
/* __gnat_runtime_initialize (default version) */
/***********************************************/

void
__gnat_runtime_initialize(int install_handler)
{
  // printc("__gnat_runtime_initialize\n");
  /*  increment the reference counter */

  __gnat_rt_init_count++;

  /*  if already initialized return now */
  if (__gnat_rt_init_count > 1)
    return;

  if (install_handler) {
    // printc("__gnat_runtime_initialize:install_handler\n");
    __gnat_install_handler();
  }
}

#ifdef __cplusplus
}
#endif
