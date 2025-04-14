/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                         'l i n u x _ s i g n a l s'
 *
 *                                      C
 *
 * File 'linux_signals.c'                       By MAR and
 *                                                 Miguel Ángel Masmano Tello.
 *
 * ----------------------------------------------------------------------
 * The idea of running MaRTE OS as a Linux process is due to Miguel Angel
 * Masmano     Tello     (Universidad    Politecnica     de     Valencia)
 * <mimastel@doctor.upv.es>.
 * He  is  also  the  author  of   most  of  the  code  involved  in  the
 * implementation of the hardware interface for this architecture.
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
 *--------------------------------------------------------------------------*/
#include <signal.h>
#include <stdlib.h>
#include "marte_functions.h"
#include "sig_timer.h"
#include "fake_linux_sigset.h"

//#define PRN_DBG(p) p;
#define PRN_DBG(p) {}

extern int linux_sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
// defined in 'linux_syscalls/lib/syscall_signal.c'


static volatile sigset_t used_signals_set;
static sigset_t empty_set;
static volatile int timer_signal_masked = 1;

/*----------------------------------------------*
 *--  linux_signals_del_signal_from_used_set  --*
 *----------------------------------------------*/
void linux_signals_del_signal_from_used_set (int signo)
{
  PRN_DBG (printc ("Del signal %d from used set\n", signo));
  fake_linux_sigdelset (&used_signals_set, signo);
}

/*--------------------------------------------*
 *--  linux_signals_add_signal_to_used_set  --*
 *--------------------------------------------*/
void linux_signals_add_signal_to_used_set (int signo)
{
  PRN_DBG (printc ("Add signal %d to used set\n", signo));
  fake_linux_sigaddset (&used_signals_set, signo);
}

/*----------------------------------------*
 *--  linux_signals_unmask_all_signals  --*
 *----------------------------------------*/
void linux_signals_unmask_all_signals ()
{
  PRN_DBG (printc ("unmask all signals\n"));
  if (linux_sigprocmask (SIG_SETMASK, &empty_set, NULL)) {
    printe ("ERROR: linux_sigprocmask\n");
    exit (-1);
  }
  timer_signal_masked = 0;
}

/*---------------------------------------*
 *--  linux_signals_mask_used_signals  --*
 *---------------------------------------*/
void linux_signals_mask_used_signals ()
{
  PRN_DBG (printc ("linux_signals_mask_used_signals\n"));
  if (linux_sigprocmask (SIG_SETMASK, &used_signals_set, NULL)) {
    printe ("ERROR: linux_sigprocmask\n");
    exit (-1);
  }
  timer_signal_masked = 1;
}

/*------------------------------------------------*
 *--  linux_signals_save_mask_and_mask_signals  --*
 *------------------------------------------------*/
void linux_signals_save_mask_and_mask_signals (int *not_used)
{
  PRN_DBG (printc ("linux_signals_save_mask_and_mask_signals\n"));
  if (linux_sigprocmask (SIG_SETMASK, &used_signals_set, NULL)) {
    printe ("ERROR: linux_sigprocmask\n");
    exit (-1);
  }
  timer_signal_masked = 1;
  //*not_used = timer_signal_masked; // nuevo
}

/*----------------------------------*
 *--  linux_signals_restore_mask  --*
 *----------------------------------*/
void linux_signals_restore_mask (int not_used)
{
  PRN_DBG (printc ("linux_signals_restore_mask\n"));
  if (linux_sigprocmask (SIG_SETMASK, &empty_set, NULL)) {
    printe ("ERROR: linux_sigprocmask\n");
    exit (-1);
  }
  //timer_signal_masked = not_used; // nuevo
  timer_signal_masked = 0;
}

/*--------------------------------------------*
 *--  linux_signals_is_timer_signal_masked  --*
 *--------------------------------------------*/
// Used to check if in kernel (after calling
// 'linux_signals_mask_all_signals' and before calling
// 'linux_signals_recover_old_mask').
int linux_signals_is_timer_signal_masked ()
{
  PRN_DBG (printc ("is timer_signal_masked?=%d\n", timer_signal_masked));
  return timer_signal_masked;
}

/*--------------------------------------*
 *--  linux_signals_get_timer_signal  --*
 *--------------------------------------*/
// get signal number used for timer expirations
int linux_signals_get_timer_signal ()
{
  return SIG_TIMER;
}

/*--------------------------*
 *--  linux_signals_init  --*
 *--------------------------*/
void linux_signals_init ()
{
  PRN_DBG (printc ("linux_signals_init\n"));
  fake_linux_sigemptyset (&empty_set);
  timer_signal_masked = 1;
}
