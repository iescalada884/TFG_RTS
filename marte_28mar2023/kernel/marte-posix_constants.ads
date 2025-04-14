------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                     'P O S I X _ C o n s t a n t s'
--
--                                   Spec
--
--
--  File 'marte-posix_constants.ads'                             By MAR. and
--                                                                  Fguerreira
--
--  POSIX constants values.
--
--  The values assigned here are used in 'Kernel' and some of its
--  child packages to define constants for different purposes.
--
--  The constants descriptions are in
--  'write_marte_c_headers/general_constants_info'.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------
with MaRTE.Integer_Types;

package MaRTE.POSIX_Constants is

   pragma Pure;

   --  Scheduling policies
   SCHED_FIFO     : constant := 0;
   SCHED_RR       : constant := 1;
   SCHED_SPORADIC : constant := 2;
   SCHED_OTHER    : constant := 3;
   SCHED_APP      : constant := 4;
   SCHED_EDF      : constant := 5;

   --  Pthread "detachstate" attribute
   PTHREAD_CREATE_JOINABLE : constant := 0;
   PTHREAD_CREATE_DETACHED : constant := 1;

   --  Pthread "contentionscope" attribute
   PTHREAD_SCOPE_SYSTEM  : constant := 10;
   PTHREAD_SCOPE_PROCESS : constant := 11;

   --  Pthread "inheritsched" attribute
   PTHREAD_INHERIT_SCHED  : constant := 20;
   PTHREAD_EXPLICIT_SCHED : constant := 21;

   --  Blocking and Unblocking Signals
   SIG_BLOCK   : constant := 0;
   SIG_UNBLOCK : constant := 1;
   SIG_SETMASK : constant := 2;

   --  Signal Action
   SIG_DFL : constant := 1;
   SIG_IGN : constant := 2;

   --  Signal Event Notification
   NO_NOTIFICATION     : constant := 0;
   SIGNAL_NOTIFICATION : constant := 1;

   --  'sigaction' flags
   SA_SIGINFO : constant := 1;

   --  Cause of Signals
   SI_USER  : constant := 0;
   SI_QUEUE : constant := 1;
   SI_TIMER : constant := 2;

   --  Mutexes policies
   NO_PRIORITY_INHERITANCE  : constant := 0;
   HIGHEST_BLOCKED_TASK     : constant := 1;
   HIGHEST_CEILING_PRIORITY : constant := 2;
   APPSCHED_PROTOCOL        : constant := 3;

   --  Clocks and timers
   CLOCK_REALTIME           : constant := 1;
   CLOCK_THREAD_CPUTIME_ID  : constant := 2;
   CLOCK_MONOTONIC          : constant := 3;
   CLOCK_INTERRUPTS_CPUTIME : constant := 4;
   ABSOLUTE_TIMER    : constant := 1;
   PERIODIC_HANDLER  : constant := 2;  --  for Timed handlers

   --  Application-defined scheduling events
   APPSCHED_NEW                        : constant :=  0;
   APPSCHED_TERMINATE                  : constant :=  1;
   APPSCHED_READY                      : constant :=  2;
   APPSCHED_BLOCK                      : constant :=  3;
   APPSCHED_YIELD                      : constant :=  4;
   APPSCHED_SIGNAL                     : constant :=  5;
   APPSCHED_CHANGE_SCHED_PARAM         : constant :=  6;
   APPSCHED_EXPLICIT_CALL              : constant :=  7;
   APPSCHED_EXPLICIT_CALL_WITH_DATA    : constant :=  8;
   APPSCHED_TIMEOUT                    : constant :=  9;
   APPSCHED_PRIORITY_INHERIT           : constant := 10;
   APPSCHED_PRIORITY_UNINHERIT         : constant := 11;
   APPSCHED_INIT_MUTEX                 : constant := 12;
   APPSCHED_DESTROY_MUTEX              : constant := 13;
   APPSCHED_LOCK_MUTEX                 : constant := 14;
   APPSCHED_TRY_LOCK_MUTEX             : constant := 15;
   APPSCHED_UNLOCK_MUTEX               : constant := 16;
   APPSCHED_BLOCK_AT_MUTEX             : constant := 17;
   APPSCHED_CHANGE_SCHED_PARAM_MUTEX   : constant := 18;
   APPSCHED_TASK_NOTIFICATION          : constant := 19;
   APPSCHED_EXECUTION_TIMER_EXPIRATION : constant := 20; --  These three are
   APPSCHED_GROUP_TIMER_EXPIRATION     : constant := 21; --  only used in the
   APPSCHED_ABORT_TASK                 : constant := 22; --  Ada interface

   --  Application scheduler or regular task
   PTHREAD_REGULAR       : constant :=  20;
   PTHREAD_APPSCHEDULER  : constant :=  21;

   --  Scheduler task flags
   APPSCHED_ABSTIMEOUT           : constant :=  2;

   --  File modes
   READ_ONLY   : constant := 1;
   WRITE_ONLY  : constant := 2;
   READ_WRITE  : constant := 3;
   FILE_MODE_MASK : constant := 3; --  Mask for the open mode
   O_NONBLOCK  : constant := 8;
   O_CREAT     : constant := 16;

   --  Lseek constants for Whence parameter
   SEEK_SET : constant := 0;
   SEEK_CUR : constant := 1;
   SEEK_END : constant := 2;

   --  Interrupt handler return codes
   POSIX_INTR_HANDLED_NOTIFY        : constant := 30;
   POSIX_INTR_HANDLED_DO_NOT_NOTIFY : constant := 31;
   POSIX_INTR_NOT_HANDLED           : constant := 32;

   --  Interval timers (setitimer, getitimer)
   --  (They should be 3 consecutive values in this order)
   ITIMER_REAL    : constant := 40;
   ITIMER_VIRTUAL : constant := 41;
   ITIMER_PROF    : constant := 42;

   --  Error_Codes
   --
   --  These numbers should match the ones used by Linux (see file
   --  /usr/include/asm/errno.h) and also the used in s-oscons.ads and
   --  in s-linux.ads
   --
   --  Values added here should be also added to 'marte-error_codes_info.ads'
   subtype Error_Code is MaRTE.Integer_Types.Int;

   NO_ERROR                         : constant :=   0;
   BAD_ADDRESS                      : constant :=  14;
   RESOURCE_TEMPORARILY_UNAVAILABLE : constant :=  11;
   RESOURCE_BUSY                    : constant :=  16;
   RESOURCE_DEADLOCK_AVOIDED        : constant :=  35;
   INTERRUPTED_OPERATION            : constant :=   4;
   INVALID_ARGUMENT                 : constant :=  22;
   NO_SUCH_PROCESS                  : constant :=   3;
   OPERATION_NOT_PERMITTED          : constant :=   1;
   OPERATION_NOT_SUPPORTED          : constant :=  95;
   OPERATION_NOT_IMPLEMENTED        : constant :=  38;
   TIMED_OUT                        : constant := 110;
   NOT_ENOUGH_SPACE                 : constant :=  12;
   RESULT_TOO_LARGE                 : constant :=  34;
   APPSCHED_EVENT_MASKED            : constant := 200;
   APPSCHED_REJECTED                : constant := 201;
   APPSCHED_POLICY_ERROR            : constant := 202;
   TOO_MANY_OPEN_FILES              : constant :=  24;
   FILENAME_TOO_LONG                : constant :=  36;
   BAD_FILE_DESCRIPTOR              : constant :=   9;
   NO_SUCH_FILE_OR_DIRECTORY        : constant :=   2;
   PERMISSION_DENIED                : constant :=  13;
   NO_ISR_ASSOCIATED                : constant := 300;

   --  Signals Values
   --
   --  Any change here should be synchronized with file s-osinte.ads and
   --  in s-linux.ads
   SIGNULL    : constant :=  0;
   SIGHUP     : constant := 1; --  hangup
   SIGINT     : constant := 2; --  interrupt (rubout)
   SIGQUIT    : constant := 3; --  quit (ASCD FS)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGTRAP    : constant := 5; --  trace trap (not reset)
   SIGIOT     : constant := 6; --  IOT instruction
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the  future
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 7; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGUSR1    : constant := 10; --  user defined signal 1
   SIGUSR2    : constant := 12; --  user defined signal 2
   SIGCLD     : constant := 17; --  alias for SIGCHLD
   SIGCHLD    : constant := 17; --  child status change
   SIGPWR     : constant := 30; --  power-fail restart
   SIGWINCH   : constant := 28; --  window size change
   SIGURG     : constant := 23; --  urgent condition on IO channel
   SIGPOLL    : constant := 29; --  pollable event occurred
   SIGIO      : constant := 29; --  I/O now possible (4.2 BSD)
   SIGLOST    : constant := 29; --  File lock lost
   SIGSTOP    : constant := 19; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 20; --  user stop requested from tty
   SIGCONT    : constant := 18; --  stopped process has been continued
   SIGTTIN    : constant := 21; --  background tty read attempted
   SIGTTOU    : constant := 22; --  background tty write attempted
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 27; --  profiling timer expired
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGUNUSED  : constant := 31; --  unused signal (GNU/Linux)
   SIGSTKFLT  : constant := 16; --  coprocessor stack fault (Linux)

   SIGRTMIN   : constant := 32;
   SIGRTMAX   : constant := 63; --  By now should be between 32 and 63
   --  SIGRTMAX also indicates the last valid value for a signal.
end MaRTE.POSIX_Constants;
