/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                             'i n t e r r u p t s'
 *
 *                                      C
 *
 * File 'interrupts.c'                          By Miguel Ángel Masmano Tello
 *                                                 and MAR.
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
//#include "hw_timer.h"
#include "struct_trap.h"
#include <stdio.h>
#include "sig_timer.h"
#include "fake_linux_sigset.h"

extern int printe(const char *__format, ...);
// MaRTE OS print error function

extern void linux_signals_unmask_all_signals ();
extern void linux_signals_mask_used_signals ();
// defined in 'linux_signals.c'

extern int linux_sigaction(int signum, const struct sigaction *act,
			   struct sigaction *oldact);
extern int linux_sigemptyset(sigset_t *set);

extern void scheduler__start_interrupt();
extern void scheduler__end_interrupt();

#define MARTE_SIG_NUM 16


//#define PRN_DBG(p) p;  // print debug messages on console
#define PRN_DBG(p) {}

/*----------------------------*
 *-- marte_signal_handler_t --*
 *----------------------------*/
// user's signal handlers
typedef void (*marte_signal_handler_t) (struct trap_state *);


/*---------------------------------*
 *-- marte_signal_handlers_table --*
 *---------------------------------*/
// This will be the table of all user's signal handlers
marte_signal_handler_t  marte_signal_handlers_table [MARTE_SIG_NUM];


/*-------------------------------------------------*
 *-- interrupts_inicialize_signal_handlers_table --*
 *-------------------------------------------------*/
// Used by Interrupt_Tables.adb
void interrupts_inicialize_signal_handlers_table (void) {
  int n;

  for (n = 0; n < MARTE_SIG_NUM; n++)
    marte_signal_handlers_table [n] = 0;
}

static void marte_signal_handler_wrapper (int signo, siginfo_t *siginfo,
					  void *ptr); // defined bellow

/*---------------------------------------*
 *-- interrupts_install_signal_handler --*
 *---------------------------------------*/
static  struct sigaction sigact;
void interrupts_install_signal_handler (int signo,
					marte_signal_handler_t handler)
{
  //struct sigaction sigact;

  marte_signal_handlers_table [signo] = handler;

  // install 'marte_signal_handler_wrapper'
  sigact.sa_sigaction = marte_signal_handler_wrapper;
  sigact.sa_flags = SA_RESTART;   // SA_SIGINFO | SA_RESTART;
  fake_linux_sigemptyset (&sigact.sa_mask);
  // Empty mask: only blocks signo during execution of handler
  //sigact.sa_restorer = NULL;

  if (linux_sigaction(signo, &sigact, NULL)) {
    printe (">>> Error: sigaction()\n");
    exit(-1);
  }
}


/*----------------------------------*
 *-- marte_signal_handler_wrapper --*
 *----------------------------------*/
// General wrapper for user's signal handlers

unsigned char base_irq_nest = 0x80;
// base_irq_nest is used to know when interrupts are called
// and it is also used in order to call scheduler when it is 0x00

extern void do_scheduling (void);
// Perform contest switch when necessary. Defined in 'kernel/k-scheduler'

static volatile struct trap_state ts;
static void marte_signal_handler_wrapper (int signo, siginfo_t *siginfo,
					  void *ptr)
{
  PRN_DBG( printc (" marte_signal_handler_wrapper\n") );
  linux_signals_mask_used_signals ();
  ts.err = signo;

  scheduler__start_interrupt();

  /* An handler is been executed */
  base_irq_nest ++;

  /* If user has installed a handler, here it will be executed */
  if (marte_signal_handlers_table [signo] != 0)
    (*(marte_signal_handlers_table [signo])) (&ts);
  else
    printe ("ERROR: Unexpected signal: no handler associated");

  /* The interruption has finished */
  base_irq_nest --;

  /* Does we call MaRTE scheduler? It may be */
  if (base_irq_nest == 0) {
    //base_irq_nest = 0x80;  done en do_scheduling
    (*do_scheduling)();
  } else {
    scheduler__end_interrupt();
  }
  linux_signals_unmask_all_signals ();
}
