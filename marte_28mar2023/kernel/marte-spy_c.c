/*----------------------------------------------------------------------------
 *--------------------        M a R T E     O S        -----------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                   't a s k s _ i n s p e c t o r _ c'
 *
 *                                   C
 *
 *
 * File 'tasks_inspector_c.c'                                          By Mar.
 *
 * Operantions to send scheduling events to the host using a serial
 * port. The events sent using 'Send_Event' can be interpretated by
 * the "Tasks Inspector" tool to display the tasks activity
 * graphically.
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
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <misc/logger.h>
#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>
#include <assert.h>

#define BUFF_LENGTH 50
static char buff[BUFF_LENGTH];
// can be declared here because the send event functions are going to be called
// from critic regions (no preemption).

/*
 * Logical constants initialized in _marte_spy_init
 */
#if MARTE_ARCHITECTURE == ARCH_X86 || MARTE_ARCHITECTURE == ARCH_LINUX_LIB
static int LOG_FD = 0;
#endif
static int USE_LOGGER_TASK = 0;

/*
 * write_to_logger
 */
static void write_to_logger(char * buff, int length)
{
#if MARTE_ARCHITECTURE == ARCH_X86
  if (USE_LOGGER_TASK) {
    // write data in the memory buffer. The data will be extracted by the
    // logger task
    write (LOG_FD, buff, length);
  } else {
    // write data directly to the logger device
    CHKE( logger_direct_call(buff, length) );
  }

#elif MARTE_ARCHITECTURE == ARCH_LINUX_LIB
  // write data directly to the file
  write (LOG_FD, buff, length);

#else
  assert (0);

#endif
}

/*
 * send_event_one_object
 */
void _marte_spy_send_event_one_object
  (unsigned long long timestamp,
   char c_op1, // I don't want to pass strings
   char c_op2,
   int id, int prio)
{
  int length = snprintf (buff, BUFF_LENGTH,
//			 "\n#%1qu#%c%c(%1d,%1d)#",
			 "\n#%llu#%c%c(%1d,%1d)#",
			 timestamp, c_op1, c_op2, id, prio);

  write_to_logger(buff, length);
}

/*
 * send_event_two_objects
 */
void _marte_spy_send_event_two_objects
  (unsigned long long timestamp,
   char c_op1, // I don't want to pass strings
   char c_op2,
   int id1, int prio1,
   int id2, int prio2)
{
  //char *p = buff;
  int length = snprintf (buff, BUFF_LENGTH,
//			 "\n#%1qu#%c%c(%1d,%1d)->(%1d,%1d)#",
			 "\n#%llu#%c%c(%1d,%1d)->(%1d,%1d)#",
			 timestamp, c_op1, c_op2,
			 id1, prio1, id2, prio2);

  write_to_logger(buff, length);
}

/*
 * send_event_time
 */
void _marte_spy_send_event_time
  (unsigned long long timestamp,
   char c_op1, // I don't want to pass strings
   char c_op2,
   unsigned long long t)
{
  //char *p = buff;
  int length = snprintf (buff, BUFF_LENGTH,
//			 "\n#%1qu#%c%c(%1qu)#",
			 "\n#%llu#%c%c(%llu)#",
			 timestamp, c_op1, c_op2, t);

  write_to_logger(buff, length);
}

/*
 * Init Serial Port
 */
void _marte_spy_init (enum log_device_id_t log_device, char use_logger_task)
{
  USE_LOGGER_TASK = use_logger_task;

#if MARTE_ARCHITECTURE == ARCH_X86
  {
    // loger init
    CHK( logger_init(log_device) );

    if (use_logger_task) {
      struct timespec period={0,50000000};
      CHK( logger_thread_create(&period) );
      LOG_FD = open("/dev/membuffer", O_RDWR);
    }
  }

#elif MARTE_ARCHITECTURE == ARCH_LINUX_LIB

    // In ARCH_LINUX_LIB write directly to a file
  CHKE( LOG_FD = open("trace_marte.dat", O_WRONLY | O_CREAT | O_DSYNC,
		      S_IREAD | S_IWRITE) );

#else
  assert (0);

#endif
}

