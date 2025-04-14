/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                        'T i m e d    H a n d l e r s'
 *
 *                                      H
 *
 * File 'timed_handlers.h'                                         By MAR and
 *                                                                    MGH.
 *
 * Timed handlers management.
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
#ifndef	__TIMED_HANDLERS_H__
#define	__TIMED_HANDLERS_H__ 1
#include <sys/cpp_macros.h>
#include <sys/marte_general_constants.h>
#include <time.h>

CPP_BEGIN_DECLS

#define PERIODIC_HANDLER    _MARTE_PERIODIC_HANDLER

/*--------------------------------*
 *--  marte_timed_handler_init  --*
 *--------------------------------*/
 int marte_timed_handler_init
     (marte_timed_handler_t *th,
      clockid_t clock_id,
      void (*handler_code) (void *area, marte_timed_handler_t *th),
      const void * area,
      size_t size);

/*-----------------------------------*
 *--  marte_timed_handler_destroy  --*
 *-----------------------------------*/
int marte_timed_handler_destroy (marte_timed_handler_t *th);


/*-------------------------------*
 *--  marte_timed_handler_set  --*
 *-------------------------------*/
// Options: 0, TIMER_ABSTIME or PERIODIC_HANDLER
// ts: If null, the expiration time of the timed handler remains unchanged
int marte_timed_handler_set (marte_timed_handler_t *th,
			     int options,
			     const struct timespec *ts);

/*-----------------------------------*
 *--  marte_timed_handler_disable  --*
 *-----------------------------------*/
int marte_timed_handler_disable (marte_timed_handler_t *th);

/*----------------------------------*
 *--  marte_timed_handler_enable  --*
 *----------------------------------*/
int marte_timed_handler_enable (marte_timed_handler_t *th);

/*-----------------------------------------*
 *--  marte_timed_handler_global_enable  --*
 *-----------------------------------------*/
int marte_timed_handler_global_enable ();

/*------------------------------------------*
 *--  marte_timed_handler_global_disable  --*
 *------------------------------------------*/
int marte_timed_handler_global_disable ();

CPP_END_DECLS

#endif	// __TIMED_HANDLERS_H__
