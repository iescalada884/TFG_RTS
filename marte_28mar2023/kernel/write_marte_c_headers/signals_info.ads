------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                         'S i g n a l s _ I n f o'
--
--                                   Spec
--
--
--  File 'signals_info.ads'                                            By MAR.
--
--  Description strings for the MaRTE signals.
--
--  The list defined here is used by 'write_marte_c_headers.adb' to
--  write automatically during the MaRTE installation the file
--  'sys/marte_signals.h'.
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

package Signals_Info is

   type Signals_Names is (SIGNULL,
                          SIGHUP,
                          SIGINT,
                          SIGQUIT,
                          SIGILL,
                          SIGTRAP,
                          SIGABRT,
                          SIGBUS,
                          SIGFPE,
                          SIGKILL,
                          SIGUSR1,
                          SIGSEGV,
                          SIGUSR2,
                          SIGPIPE,
                          SIGALRM,
                          SIGTERM,
                          SIGCHLD,
                          SIGCONT,
                          SIGSTOP,
                          SIGTSTP,
                          SIGTTIN,
                          SIGTTOU,
                          SIGVTALRM,
                          SIGPROF,
                          SIGUNUSED,
                          SIGRTMIN,
                          SIGRTMAX);

   type Signal_Info is record
      Number      : Integer;
      Description : String (1..45);
   end record;

   List : constant array (Signals_Names) of Signal_Info :=
     (SIGNULL   =>
        (Number => MaRTE.POSIX_Constants.SIGNULL,
         Description => "null signal                                  "),
      SIGHUP    =>
        (Number => MaRTE.POSIX_Constants.SIGHUP,
         Description => "hangup                                       "),
      SIGINT    =>
        (Number => MaRTE.POSIX_Constants.SIGINT,
         Description => "interrupt                                    "),
      SIGQUIT   =>
        (Number => MaRTE.POSIX_Constants.SIGQUIT,
         Description => "quit                                         "),
      SIGILL    =>
        (Number => MaRTE.POSIX_Constants.SIGILL,
         Description => "illegal instruction                          "),
      SIGTRAP   =>
        (Number => MaRTE.POSIX_Constants.SIGTRAP,
         Description => "trace trap                                   "),
      SIGABRT   =>
        (Number => MaRTE.POSIX_Constants.SIGABRT,
         Description => "abort()                                      "),
      SIGBUS    =>
        (Number => MaRTE.POSIX_Constants.SIGBUS,
         Description => "bus error                                    "),
      SIGFPE    =>
        (Number => MaRTE.POSIX_Constants.SIGFPE,
         Description => "floating point exception                     "),
      SIGKILL   =>
        (Number => MaRTE.POSIX_Constants.SIGKILL,
         Description => "kill (cannot be caught or ignored)           "),
      SIGUSR1   =>
        (Number => MaRTE.POSIX_Constants.SIGUSR1,
         Description => "user defined signal 1                        "),
      SIGSEGV   =>
        (Number => MaRTE.POSIX_Constants.SIGSEGV,
         Description => "segmentation violation                       "),
      SIGUSR2   =>
        (Number => MaRTE.POSIX_Constants.SIGUSR2,
         Description => "user defined signal 2                        "),
      SIGPIPE   =>
        (Number => MaRTE.POSIX_Constants.SIGPIPE,
         Description => "write on a pipe with no one to read it       "),
      SIGALRM   =>
        (Number => MaRTE.POSIX_Constants.SIGALRM,
         Description => "alarm clock                                  "),
      SIGTERM   =>
        (Number => MaRTE.POSIX_Constants.SIGTERM,
         Description => "Termination Signal                           "),
      SIGCHLD   =>
        (Number => MaRTE.POSIX_Constants.SIGCHLD,
         Description => "to parent on child stop or exit              "),
      SIGCONT   =>
        (Number => MaRTE.POSIX_Constants.SIGCONT,
         Description => "continue a stopped process                   "),
      SIGSTOP   =>
        (Number => MaRTE.POSIX_Constants.SIGSTOP,
         Description => "sendable stop signal not from tty            "),
      SIGTSTP   =>
        (Number => MaRTE.POSIX_Constants.SIGTSTP,
         Description => "stop signal from tty                         "),
      SIGTTIN   =>
        (Number => MaRTE.POSIX_Constants.SIGTTIN,
         Description => "Read from ctr term by member of bg proc group"),
      SIGTTOU   =>
        (Number => MaRTE.POSIX_Constants.SIGTTOU,
         Description => "Write to ctr term by member of bg proc group "),
      SIGVTALRM =>
        (Number => MaRTE.POSIX_Constants.SIGVTALRM,
         Description => "virtual timer expired                        "),
      SIGPROF =>
        (Number => MaRTE.POSIX_Constants.SIGPROF,
         Description => "PROF timer expired                           "),
      SIGUNUSED =>
        (Number => MaRTE.POSIX_Constants.SIGUNUSED,
         Description => "unused signal                                "),
      SIGRTMIN =>
        (Number => MaRTE.POSIX_Constants.SIGRTMIN,
         Description => "minimum value for the RT signals             "),
      SIGRTMAX =>
        (Number => MaRTE.POSIX_Constants.SIGRTMAX,
         Description => "maximum value for the RT signals             "));

end Signals_Info;
