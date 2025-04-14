------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                'G e n e r a l _ C o n s t a n t s _ I n f o'
--
--                                   Spec
--
--
--  File 'general_constants_info.ads'                            By MAR. and
--                                                                  Fguerreira
--
--  Description strings for the MaRTE general constants.
--
--  The list defined here is used by 'write_marte_c_headers.adb' to
--  write automatically during the MaRTE installation the file
--  'sys/marte_general_constants.h'.
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
with MaRTE.POSIX_Constants;

package General_Constants_Info is

   package PC renames MaRTE.POSIX_Constants;

   type Constant_Names is (--  Scheduling policies
                           SCHED_FIFO,
                           SCHED_RR,
                           SCHED_SPORADIC,
                           SCHED_OTHER,
                           SCHED_APP,
                           SCHED_EDF,

                           -- Pthread "detachstate" attribute
                           PTHREAD_CREATE_JOINABLE,
                           PTHREAD_CREATE_DETACHED,

                           --  Pthread "contentionscope" attribute
                           PTHREAD_SCOPE_SYSTEM,
                           PTHREAD_SCOPE_PROCESS,

                           --  Pthread "inheritsched" attribute
                           PTHREAD_INHERIT_SCHED,
                           PTHREAD_EXPLICIT_SCHED,

                           --  Blocking and Unblocking Signals
                           SIG_BLOCK,
                           SIG_UNBLOCK,
                           SIG_SETMASK,

                           --  Signal Action
                           SIG_DFL,
                           SIG_IGN,

                           --  Signal Event Notification
                           NO_NOTIFICATION,
                           SIGNAL_NOTIFICATION,

                           --  'sigaction' flags
                           SA_SIGINFO,

                           --  Cause of Signals
                           SI_USER,
                           SI_QUEUE,
                           SI_TIMER,

                           -- Mutexes policies
                           NO_PRIORITY_INHERITANCE,
                           HIGHEST_CEILING_PRIORITY,
                           HIGHEST_BLOCKED_TASK,
                           APPSCHED_PROTOCOL,

                           --  Clocks and timers
                           CLOCK_REALTIME,
                           CLOCK_THREAD_CPUTIME_ID,
                           CLOCK_MONOTONIC,
                           CLOCK_INTERRUPTS_CPUTIME,
                           ABSOLUTE_TIMER,
                           PERIODIC_HANDLER,

                           --  Application scheduler or regular task
                           PTHREAD_REGULAR,
                           PTHREAD_APPSCHEDULER,

                           -- Scheduler task flags
                           APPSCHED_ABSTIMEOUT,

                           --  File modes
                           READ_ONLY,
                           WRITE_ONLY,
                           READ_WRITE,
                           O_NONBLOCK,
                           O_CREAT,
                           --  Lseek constants for Whence
                           SEEK_SET,
                           SEEK_CUR,
                           SEEK_END,

                           --  Interrupt handler return codes
                           POSIX_INTR_HANDLED_NOTIFY,
                           POSIX_INTR_HANDLED_DO_NOT_NOTIFY,
                           POSIX_INTR_NOT_HANDLED,

                           --  Interval timers (setitimer, getitimer)
                           ITIMER_REAL,
                           ITIMER_VIRTUAL,
                           ITIMER_PROF);

   type Sched_Events_Codes is
      (--  Application-defined scheduling events
       APPSCHED_NEW,
       APPSCHED_TERMINATE,
       APPSCHED_READY,
       APPSCHED_BLOCK,
       APPSCHED_YIELD,
       APPSCHED_SIGNAL,
       APPSCHED_CHANGE_SCHED_PARAM,
       APPSCHED_EXPLICIT_CALL,
       APPSCHED_EXPLICIT_CALL_WITH_DATA,
       APPSCHED_TIMEOUT,
       APPSCHED_PRIORITY_INHERIT,
       APPSCHED_PRIORITY_UNINHERIT,
       APPSCHED_INIT_MUTEX,
       APPSCHED_DESTROY_MUTEX,
       APPSCHED_LOCK_MUTEX,
       APPSCHED_TRY_LOCK_MUTEX,
       APPSCHED_UNLOCK_MUTEX,
       APPSCHED_BLOCK_AT_MUTEX,
       APPSCHED_CHANGE_SCHED_PARAM_MUTEX,
       APPSCHED_TASK_NOTIFICATION);


   type Constant_Info is record
      Value         : Integer;
      C_Name        : String (1..40);  -- POSIX.C name
      C_Description : String (1..35);
   end record;

   List : constant array (Constant_Names) of Constant_Info :=
     (--  Scheduling policies
      SCHED_FIFO        => (PC.SCHED_FIFO,
                            "SCHED_FIFO                              ",
                            "FIFO scheduling policy             "),
      SCHED_RR          => (PC.SCHED_RR,
                            "SCHED_RR                                ",
                            "Round robin scheduling policy      "),
      SCHED_SPORADIC    => (PC.SCHED_SPORADIC,
                            "SCHED_SPORADIC                          ",
                            "Sporadic Server scheduling policy  "),
      SCHED_OTHER       => (PC.SCHED_OTHER,
                            "SCHED_OTHER                             ",
                            "Another scheduling policy          "),
      SCHED_APP         => (PC.SCHED_APP,
                            "SCHED_APP                               ",
                            "Application-defined sched. policy  "),
      SCHED_EDF         => (PC.SCHED_EDF,
                            "SCHED_EDF                               ",
                            "EDF scheduling policy              "),

      -- Pthread "detachstate" attribute
      PTHREAD_CREATE_JOINABLE => (PC.PTHREAD_CREATE_JOINABLE,
                                  "PTHREAD_CREATE_JOINABLE                 ",
                                  "Joinable pthread                   "),
      PTHREAD_CREATE_DETACHED => (PC.PTHREAD_CREATE_DETACHED,
                                  "PTHREAD_CREATE_DETACHED                 ",
                                  "Detached pthread                   "),

      --  Pthread "contentionscope" attribute
      PTHREAD_SCOPE_SYSTEM  => (PC.PTHREAD_SCOPE_SYSTEM,
                                "PTHREAD_SCOPE_SYSTEM                    ",
                                "System scheduling contention scope "),
      PTHREAD_SCOPE_PROCESS => (PC.PTHREAD_SCOPE_PROCESS,
                                "PTHREAD_SCOPE_PROCESS                   ",
                                "Process scheduling contention scope"),

      --  Pthread "inheritsched" attribute
      PTHREAD_INHERIT_SCHED  => (PC.PTHREAD_INHERIT_SCHED,
                                 "PTHREAD_INHERIT_SCHED                   ",
                                 "Policy and attributes inherited    "),
      PTHREAD_EXPLICIT_SCHED => (PC.PTHREAD_EXPLICIT_SCHED,
                                 "PTHREAD_EXPLICIT_SCHED                  ",
                                 "Policy and attributes explicit     "),

      --  Blocking and Unblocking Signals
      SIG_BLOCK   => (PC.SIG_BLOCK,
                      "SIG_BLOCK                               ",
                      "Add signals to the current set     "),
      SIG_UNBLOCK => (PC.SIG_UNBLOCK,
                      "SIG_UNBLOCK                             ",
                      "Remove signals from the current set"),
      SIG_SETMASK => (PC.SIG_SETMASK,
                      "SIG_SETMASK                             ",
                      "Assign current set                 "),

      --  Signal Action
      SIG_DFL     => (PC.SIG_DFL,
                      "SIG_DFL                                 ",
                      "Request for default signal handling"),
      SIG_IGN     => (PC.SIG_IGN,
                      "SIG_IGN                                 ",
                      "Request that signal be ignored     "),

      --  Signal Event Notification
      NO_NOTIFICATION     => (PC.NO_NOTIFICATION,
                              "SIGEV_NONE                              ",
                              "No notification when event occurs  "),
      SIGNAL_NOTIFICATION => (PC.SIGNAL_NOTIFICATION,
                              "SIGEV_SIGNAL                            ",
                              "Generate signal when event occurs  "),

      --  'sigaction' flags
      SA_SIGINFO          => (PC.SA_SIGINFO,
                              "SA_SIGINFO                              ",
                              "Real-Time signal behaviour         "),

      --  Cause of Signals
      SI_USER             => (PC.SI_USER,
                              "SI_USER                                 ",
                              "Signal sent by the kill function   "),
      SI_QUEUE            => (PC.SI_QUEUE,
                              "SI_QUEUE                                ",
                              "Signal sent by sigqueue function   "),
      SI_TIMER            => (PC.SI_TIMER,
                              "SI_TIMER                                ",
                              "Signal sent by timer expiration    "),

      -- Mutexes policies
      NO_PRIORITY_INHERITANCE  => (PC.NO_PRIORITY_INHERITANCE,
                                   "PTHREAD_PRIO_NONE                       ",
                                   "No priority inheritance            "),
      HIGHEST_CEILING_PRIORITY => (PC.HIGHEST_CEILING_PRIORITY,
                                   "PTHREAD_PRIO_PROTECT                    ",
                                   "Highest ceiling priority protocol  "),
      HIGHEST_BLOCKED_TASK     => (PC.HIGHEST_BLOCKED_TASK,
                                   "PTHREAD_PRIO_INHERIT                    ",
                                   "Highest blocked task protocol      "),
      APPSCHED_PROTOCOL        => (PC.APPSCHED_PROTOCOL,
                                   "PTHREAD_APPSCHED_PROTOCOL               ",
                                   "Application-defined protocol       "),

      --  Clocks and timers
      CLOCK_REALTIME          => (PC.CLOCK_REALTIME,
                                  "CLOCK_REALTIME                          ",
                                  "Realtime clock                     "),
      CLOCK_THREAD_CPUTIME_ID => (PC.CLOCK_THREAD_CPUTIME_ID,
                                  "CLOCK_THREAD_CPUTIME_ID                 ",
                                  "CPU-time clock of calling thread   "),
      CLOCK_MONOTONIC         => (PC.CLOCK_MONOTONIC,
                                  "CLOCK_MONOTONIC                         ",
                                  "Monotonic clock                    "),
      CLOCK_INTERRUPTS_CPUTIME => (PC.CLOCK_INTERRUPTS_CPUTIME,
                                   "CLOCK_INTERRUPTS_CPUTIME                ",
                                   "Time consumed by interrupt handlers"),
      ABSOLUTE_TIMER          => (PC.ABSOLUTE_TIMER,
                                  "TIMER_ABSTIME                           ",
                                  "Absolute timer                     "),
      PERIODIC_HANDLER        => (PC.PERIODIC_HANDLER,
                                  "PERIODIC_HANDLER                        ",
                                  "Periodic Timed Handler             "),

      --  Application scheduler or regular task
      PTHREAD_REGULAR      => (PC.PTHREAD_REGULAR,
                               "PTHREAD_REGULAR                         ",
                               "Regular thread                     "),
      PTHREAD_APPSCHEDULER => (PC.PTHREAD_APPSCHEDULER,
                               "PTHREAD_APPSCHEDULER                    ",
                               "Application scheduler thread       "),

      -- Scheduler task flags
      APPSCHED_ABSTIMEOUT => (PC.APPSCHED_ABSTIMEOUT,
                              "POSIX_APPSCHED_ABSTIMEOUT               ",
                              "                                   "),

      --  File modes
      READ_ONLY          =>  (PC.READ_ONLY,
                              "O_RDONLY                                ",
                              "File open for reading only         "),
      WRITE_ONLY         =>  (PC.WRITE_ONLY,
                              "O_WRONLY                                ",
                              "File open for writing only         "),
      READ_WRITE         =>  (PC.READ_WRITE,
                              "O_RDWR                                  ",
                              "File open for reading and writing  "),
      O_NONBLOCK         =>  (PC.O_NONBLOCK,
                              "O_NONBLOCK                              ",
                              "Non-blocking read and write        "),
      O_CREAT            =>  (PC.O_CREAT,
                              "O_CREAT                                 ",
                              "If file does not exist, create     "),

      --  File modes
      SEEK_SET           =>  (PC.SEEK_SET,
                              "SEEK_SET                                ",
                              "Set file offset to offset          "),
      SEEK_CUR           =>  (PC.SEEK_CUR,
                              "SEEK_CUR                                ",
                              "Set offset to current plus offset  "),
      SEEK_END           =>  (PC.SEEK_END,
                              "SEEK_END                                ",
                              "Set offset to EOF plus offset      "),

      --  Interrupt handler return codes
      POSIX_INTR_HANDLED_NOTIFY
                         =>  (PC.POSIX_INTR_HANDLED_NOTIFY,
                              "POSIX_INTR_HANDLED_NOTIFY               ",
                              "Interrupt handled, notify thread   "),
      POSIX_INTR_HANDLED_DO_NOT_NOTIFY
                         =>  (PC.POSIX_INTR_HANDLED_DO_NOT_NOTIFY,
                              "POSIX_INTR_HANDLED_DO_NOT_NOTIFY        ",
                              "Interrupt handled, do not notify   "),
      POSIX_INTR_NOT_HANDLED
                         =>  (PC.POSIX_INTR_NOT_HANDLED,
                              "POSIX_INTR_NOT_HANDLED                  ",
                              "Interrupt not handled              "),
      --  Interval timers
      ITIMER_REAL        =>  (PC.ITIMER_REAL,
                              "ITIMER_REAL                             ",
                              "Decrements in real time            "),
      ITIMER_VIRTUAL     =>  (PC.ITIMER_VIRTUAL,
                              "ITIMER_VIRTUAL                          ",
                              "Decrements in process virtual time "),
      ITIMER_PROF        =>  (PC.ITIMER_PROF,
                              "ITIMER_PROF                             ",
                              "Dec. both in p. v. time and system ")
     );

   Sched_List : constant array (Sched_Events_Codes) of Constant_Info :=
     (--  Application-defined scheduling events
      APPSCHED_NEW              => (PC.APPSCHED_NEW,
                                    "POSIX_APPSCHED_NEW                      ",
                                    "                                   "),
      APPSCHED_TERMINATE        => (PC.APPSCHED_TERMINATE,
                                    "POSIX_APPSCHED_TERMINATE                ",
                                    "                                   "),
      APPSCHED_READY            => (PC.APPSCHED_READY,
                                    "POSIX_APPSCHED_READY                    ",
                                    "                                   "),
      APPSCHED_BLOCK            => (PC.APPSCHED_BLOCK,
                                    "POSIX_APPSCHED_BLOCK                    ",
                                    "                                   "),
      APPSCHED_YIELD            => (PC.APPSCHED_YIELD,
                                    "POSIX_APPSCHED_YIELD                    ",
                                    "                                   "),
      APPSCHED_SIGNAL           => (PC.APPSCHED_SIGNAL,
                                    "POSIX_APPSCHED_SIGNAL                   ",
                                    "                                   "),
      APPSCHED_CHANGE_SCHED_PARAM
                                => (PC.APPSCHED_CHANGE_SCHED_PARAM,
                                    "POSIX_APPSCHED_CHANGE_SCHED_PARAM       ",
                                    "                                   "),
      APPSCHED_EXPLICIT_CALL    => (PC.APPSCHED_EXPLICIT_CALL,
                                    "POSIX_APPSCHED_EXPLICIT_CALL            ",
                                    "                                   "),
      APPSCHED_EXPLICIT_CALL_WITH_DATA
                                => (PC.APPSCHED_EXPLICIT_CALL_WITH_DATA,
                                    "POSIX_APPSCHED_EXPLICIT_CALL_WITH_DATA  ",
                                    "                                   "),
      APPSCHED_TIMEOUT          => (PC.APPSCHED_TIMEOUT,
                                    "POSIX_APPSCHED_TIMEOUT                  ",
                                    "                                   "),
      APPSCHED_PRIORITY_INHERIT => (PC.APPSCHED_PRIORITY_INHERIT,
                                    "POSIX_APPSCHED_PRIORITY_INHERIT         ",
                                    "                                   "),
      APPSCHED_PRIORITY_UNINHERIT
                                => (PC.APPSCHED_PRIORITY_UNINHERIT,
                                    "POSIX_APPSCHED_PRIORITY_UNINHERIT       ",
                                    "                                   "),
      APPSCHED_INIT_MUTEX       => (PC.APPSCHED_INIT_MUTEX,
                                    "POSIX_APPSCHED_INIT_MUTEX               ",
                                     "                                   "),
      APPSCHED_DESTROY_MUTEX    => (PC.APPSCHED_DESTROY_MUTEX,
                                    "POSIX_APPSCHED_DESTROY_MUTEX            ",
                                    "                                   "),
      APPSCHED_LOCK_MUTEX       => (PC.APPSCHED_LOCK_MUTEX,
                                    "POSIX_APPSCHED_LOCK_MUTEX               ",
                                    "                                   "),
      APPSCHED_TRY_LOCK_MUTEX   => (PC.APPSCHED_TRY_LOCK_MUTEX,
                                    "POSIX_APPSCHED_TRY_LOCK_MUTEX           ",
                                     "                                   "),
      APPSCHED_UNLOCK_MUTEX     => (PC.APPSCHED_UNLOCK_MUTEX,
                                    "POSIX_APPSCHED_UNLOCK_MUTEX             ",
                                    "                                   "),
      APPSCHED_BLOCK_AT_MUTEX   => (PC.APPSCHED_BLOCK_AT_MUTEX,
                                    "POSIX_APPSCHED_BLOCK_AT_MUTEX           ",
                                    "                                   "),
      APPSCHED_CHANGE_SCHED_PARAM_MUTEX
                             => (PC.APPSCHED_CHANGE_SCHED_PARAM_MUTEX,
                                 "POSIX_APPSCHED_CHANGE_SCHED_PARAM_MUTEX ",
                                 "                                   "),
      APPSCHED_TASK_NOTIFICATION
                             => (PC.APPSCHED_TASK_NOTIFICATION,
                                 "POSIX_APPSCHED_TASK_NOTIFICATION        ",
                                 "                                   ")
     );


end General_Constants_Info;
