/*
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
 *--------------------------------------------------------------------------*/

// We define the errno macro in order to use the errno value given by
// MaRTE to its threads instead of the errno given by Linux to the
// only thread that implements the MaRTE application

#include "/usr/include/errno.h"

// Add the MaRTE-specific errnos
#undef ENOTSUP
// ENOTSUP has to be undefined to avoid a warning because it is
// defined in /usr/include/bits/errno.h as "# define ENOTSUP
// EOPNOTSUPP". Of course the MaRTE definition of ENOTSUP has the same
// number than EOPNOTSUPP in Linux
#include <sys/marte_errno.h>

// Define MaRTE errno
#undef errno
#define errno  (*pthread_errno())		/* per-thread error number */
extern int *pthread_errno();
