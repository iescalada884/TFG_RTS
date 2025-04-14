/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                      'f a k e _ l i n u x _ s i g s e t'
 *
 *                                      C
 *
 * File 'fake_linux_sigset.c'                                        By MAR.
 *
 * Function to manage Linux 'sigset_t'. Linux funcions cannot be uses
 * since they could create a confusion with MaRTE functions with the
 * same names.
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
 * ----------------------------------------------------------------------
 * The idea of running MaRTE OS as a Linux process is due to Miguel Angel
 * Masmano     Tello     (Universidad    Politecnica     de     Valencia)
 * <mimastel@doctor.upv.es>.
 * He  is  also  the  author  of   most  of  the  code  involved  in  the
 * implementation of the hardware interface for this architecture.
 *
 *--------------------------------------------------------------------------*/
#include <signal.h>
#include <string.h>
#include <bits/siginfo.h>

/* -------------------------------------------------------------------------
 * ----  Signal mask management  -------------------------------------------
 * -------------------------------------------------------------------------*/
// These functions have to be hand made to avoid confusion between
// Linux and MaRTE OS versions.
// type sigset is defined in  '/usr/include/bits/sigset.h'
//  /* A `sigset_t' has a bit for each signal.  */
//
// # define _SIGSET_NWORDS (1024 / (8 * sizeof (unsigned long int)))
// typedef struct
//   {
//     unsigned long int __val[_SIGSET_NWORDS];
//   } __sigset_t;

#define VSIZE sizeof(unsigned long)
// taken from 'glibc-2.3.3/signal/sigsetopts.h'
#define	BITS		(_NSIG - 1)
#define	ELT(signo)	(((signo) - 1) / BITS)
#define	MASK(signo)	(1 << (((signo) - 1) % BITS))

void fake_linux_sigemptyset (sigset_t *set)
{
  memset (set, 0, sizeof (sigset_t));
}

/* Set all signals in SET.  */
void fake_linux_sigfillset (sigset_t *set)
{
  memset (set, 0xFF, sizeof (sigset_t));
}

/* Add SIGNO to SET.  */
void fake_linux_sigaddset (sigset_t *set, int signo)
{
  set->__val[ELT(signo)] |= MASK(signo);
}

/* Remove SIGNO from SET.  */
void fake_linux_sigdelset (sigset_t *set, int signo)
{
  unsigned a = ELT(signo);
  unsigned long b = ~MASK(signo);
  set->__val[ELT(signo)] &= ~MASK(signo);
}

/* Return 1 if SIGNO is in SET, 0 if not.  */
int fake_linux_sigismember (const sigset_t *set, int signo)
{
  return (set->__val[ELT(signo)] & MASK(signo)) != 0;
}
