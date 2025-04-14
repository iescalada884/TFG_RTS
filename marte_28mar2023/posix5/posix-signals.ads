------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                         P O S I X . S I G N A L S                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--  This  file is a component  of FLORIST,  an implementation of the POSIX  --
--  Ada  bindings  for  use with the GNAT Ada compiler and the FSU Gnu Ada  --
--  Runtime Library (GNARL).                                                --
--                                                                          --
--  This package specification contains some text extracted from  IEEE STD  --
--  1003.5: 1990, Information Technology -- POSIX Ada Language  Interfaces  --
--  Part 1: Binding  for  System Application Program Interface, as amended  --
--  by IEEE STD 1003.5b: 1996, Amendment 1: Realtime Extensions, copyright  --
--  1996 by the Institute of Electrical and Electronics Engineers, Inc.     --
--                                                                          --
--  The package specifications in the IEEE standards cited above represent  --
--  only a  portion  of  the  documents  and  are  not to be interpreteted  --
--  outside the context  of  the documents.  The standards must be used in  --
--  conjunction  with  the  package   specifications  in  order  to  claim  --
--  conformance.   The IEEE takes no responsibility for and will assume no  --
--  liability for damages resulting from the reader's misinterpretation of  --
--  said  information resulting from its out-of-context nature.   To order  --
--  copies of the IEEE standards,  please contact the  IEEE Service Center  --
--  at 445 Hoes Lane, PO Box 1331, Piscataway, NJ 08855-1331; via phone at  --
--  1-800-678-IEEE, 908-981-1393; or via fax at 908-981-9667.               --
--                                                                          --
--  These  package  specifications are  distributed in  the hope that they  --
--  will  be useful, but  WITHOUT  ANY  WARRANTY; without even the implied  --
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision: 858 $]
------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                        'P O S I X - S i g n a l s'
--
--                                  Spec
--
--
--  File 'posix-signals.ads'                                           By MAR.
--
--
--  Package 'POSIX_Signals' as defined in IEEE Std 1003.5b-1996.
--
--  MaRTE OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
--
--  This package calls directly the operations defined in
--  'MaRTE.Kernel.Signals', but taking into account the necessary treatment of
--  the signals reserved for Gnat run time use.
--
--  This file is based on the Florist implementation.
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

with Ada_Task_Identification;
with POSIX;
with System;
with System.Interrupt_Management;
with System.Storage_Elements;
with MaRTE.POSIX_Signal;
with MaRTE.POSIX_Constants;
with MaRTE.Kernel.Signals;
package POSIX.Signals is

   package MSignal renames MaRTE.POSIX_Signal;
   -- to avoid confusion with the type 'Signal'

   --  Signal Type

   type Signal is
     new System.Interrupt_Management.Interrupt_ID'Base
     range 0 .. MaRTE.POSIX_Constants.SIGRTMAX;
   for Signal'Size use MSignal.Signal'Size;

   function Image (Sig : Signal) return String;
   function Value (Str : String) return Signal;

   --  Standard Signals (required)

   Signal_Null,
   SIGNULL                    : constant Signal := 0;
   Signal_Abort,
   SIGABRT                    : constant Signal := MaRTE.POSIX_Constants.SIGABRT;
   Signal_Alarm,
   SIGALRM                    : constant Signal := MaRTE.POSIX_Constants.SIGALRM;
   Signal_Bus_Error,
   SIGBUS                     : constant Signal := MaRTE.POSIX_Constants.SIGBUS;
   Signal_Floating_Point_Error,
   SIGFPE                     : constant Signal := MaRTE.POSIX_Constants.SIGFPE;
   Signal_Hangup,
   SIGHUP                     : constant Signal := MaRTE.POSIX_Constants.SIGHUP;
   Signal_Illegal_Instruction,
   SIGILL                     : constant Signal := MaRTE.POSIX_Constants.SIGILL;
   Signal_Interrupt,
   SIGINT                     : constant Signal := MaRTE.POSIX_Constants.SIGINT;
   Signal_Kill,
   SIGKILL                    : constant Signal := MaRTE.POSIX_Constants.SIGKILL;
   Signal_Pipe_Write,
   SIGPIPE                    : constant Signal := MaRTE.POSIX_Constants.SIGPIPE;
   Signal_Quit,
   SIGQUIT                    : constant Signal := MaRTE.POSIX_Constants.SIGQUIT;
   Signal_Segmentation_Violation,
   SIGSEGV                    : constant Signal := MaRTE.POSIX_Constants.SIGSEGV;
   Signal_Terminate,
   SIGTERM                    : constant Signal := MaRTE.POSIX_Constants.SIGTERM;
   Signal_User_1,
   SIGUSR1                    : constant Signal := MaRTE.POSIX_Constants.SIGUSR1;
   Signal_User_2,
   SIGUSR2                    : constant Signal := MaRTE.POSIX_Constants.SIGUSR2;

   --  Standard Signals (job control)

   Signal_Child,
   SIGCHLD                    : constant Signal := MaRTE.POSIX_Constants.SIGCHLD;
   Signal_Continue,
   SIGCONT                    : constant Signal := MaRTE.POSIX_Constants.SIGCONT;
   Signal_Stop,
   SIGSTOP                    : constant Signal := MaRTE.POSIX_Constants.SIGSTOP;
   Signal_Terminal_Stop,
   SIGTSTP                    : constant Signal := MaRTE.POSIX_Constants.SIGTSTP;
   Signal_Terminal_Input,
   SIGTTIN                    : constant Signal := MaRTE.POSIX_Constants.SIGTTIN;
   Signal_Terminal_Output,
   SIGTTOU                    : constant Signal := MaRTE.POSIX_Constants.SIGTTOU;

   --  Signals from P1003.5c

--    Signal_IO,
--    SIGIO                      : constant Signal := MaRTE.POSIX_Constants.SIGIO;
--    Signal_Out_Of_Band_Data,
--    SIGURG                     : constant Signal := MaRTE.POSIX_Constants.SIGURG;

   subtype Realtime_Signal is Signal range
     MaRTE.POSIX_Constants.SIGRTMIN .. MaRTE.POSIX_Constants.SIGRTMAX;

   --  Signal sets

   type Signal_Set is private;

   procedure Add_Signal
     (Set : in out Signal_Set;
      Sig : in Signal);
   procedure Add_All_Signals (Set : in out Signal_Set);
   procedure Delete_Signal
     (Set : in out Signal_Set;
      Sig : in Signal);
   procedure Delete_All_Signals (Set : in out Signal_Set);
   function Is_Member
     (Set : Signal_Set;
      Sig : Signal)
     return Boolean;

   --  Blocking and Unblocking Signals

   procedure Set_Blocked_Signals
     (New_Mask : in Signal_Set;
      Old_Mask : out Signal_Set);
   procedure Block_Signals
     (Mask_to_Add : in Signal_Set;
      Old_Mask    : out Signal_Set);
   procedure Unblock_Signals
     (Mask_to_Subtract : in Signal_Set;
      Old_Mask         : out Signal_Set);
   function Blocked_Signals return Signal_Set;

   --  Ignoring Signals

   procedure Ignore_Signal (Sig : in Signal);
   procedure Unignore_Signal (Sig : in Signal);
   function Is_Ignored (Sig : Signal) return Boolean;
   procedure Install_Empty_Handler (Sig : Signal);

   --  Controlling Delivery of Signal_Child Signal

--    procedure Set_Stopped_Child_Signal (Enable : in Boolean := True);
--    function Stopped_Child_Signal_Enabled return Boolean;

   --  Examining Pending Signals

   type Signal_Event is private;
   type Signal_Data  is private;

   subtype Notification is MaRTE.Kernel.Signals.Notification;
   No_Notification     : constant Notification :=
     MaRTE.Kernel.Signals.NO_NOTIFICATION;
   Signal_Notification : constant Notification :=
     MaRTE.Kernel.Signals.SIGNAL_NOTIFICATION;

   function Get_Signal (Event : Signal_Event) return Signal;
   procedure Set_Signal
     (Event : in out Signal_Event;
      Sig   : in Signal);
   function Get_Notification (Event : Signal_Event) return Notification;
   procedure Set_Notification (Event  : in out Signal_Event;
                               Notify : in     Notification);
   function Get_Data (Event : Signal_Event) return Signal_Data;
   procedure Set_Data (Event : in out Signal_Event;
                       Data  : in     Signal_Data);

   type Signal_Source is range Integer'First .. Integer'Last;
   From_Send_Signal   : constant Signal_Source :=
     Signal_Source (MaRTE.Kernel.Signals.SI_USER);
   From_Queue_Signal  : constant Signal_Source :=
     Signal_Source (MaRTE.Kernel.Signals.SI_QUEUE);
   From_Timer         : constant Signal_Source :=
     Signal_Source (MaRTE.Kernel.Signals.SI_TIMER);
--    From_Async_IO      : constant Signal_Source := POSIX.C.SI_ASYNCIO;
--    From_Message_Queue : constant Signal_Source := POSIX.C.SI_MESGQ;

   type Signal_Info is private;
   function Get_Signal (Info : Signal_Info) return Signal;
   pragma Inline (Get_Signal);
   procedure Set_Signal
     (Info : in out Signal_Info;
      Sig  : in Signal);
   pragma Inline (Set_Signal);
   function Get_Source (Info : Signal_Info) return Signal_Source;
   pragma Inline (Get_Source);
   procedure Set_Source
     (Info   : in out Signal_Info;
      Source : in Signal_Source);
   pragma Inline (Set_Source);
   function Has_Data (Source : Signal_Source) return Boolean;
   pragma Inline (Has_Data);
   function Get_Data (Info : Signal_Info) return Signal_Data;
   pragma Inline (Get_Data);
   procedure Set_Data
     (Info : in out Signal_Info;
      Data : in Signal_Data);
   pragma Inline (Set_Data);

--    procedure Enable_Queueing (Sig : in Signal);
--    procedure Disable_Queueing (Sig : in Signal);

   function Await_Signal (Set : Signal_Set) return Signal;
--    function Await_Signal_Or_Timeout
--      (Set     : Signal_Set;
--       Timeout : POSIX.Timespec)
--      return Signal;
   function Await_Signal (Set : Signal_Set) return Signal_Info;
--    function Await_Signal_Or_Timeout
--      (Set     : Signal_Set;
--       Timeout : POSIX.Timespec)
--      return Signal_Info;

   Signal_Abort_Ref           : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGABRT));
   Signal_Hangup_Ref          : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGHUP));
   Signal_Interrupt_Ref       : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGINT));
   Signal_Pipe_Write_Ref      : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGPIPE));
   Signal_Quit_Ref            : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGQUIT));
   Signal_Terminate_Ref       : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGTERM));
   Signal_User_1_Ref          : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGUSR1));
   Signal_User_2_Ref          : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGUSR2));
   Signal_Child_Ref           : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGCHLD));
   Signal_Continue_Ref        : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGCONT));
   Signal_Terminal_Stop_Ref   : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGTSTP));
   Signal_Terminal_Input_Ref  : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGTTIN));
   Signal_Terminal_Output_Ref : constant System.Address
     := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (SIGTTOU));

   function Signal_Reference (Sig : Signal) return System.Address;

--    procedure Send_Signal
--      (Process : in POSIX.Process_Identification.Process_ID;
--       Sig     : in Signal);
--    procedure Send_Signal
--       (Group : in POSIX.Process_Identification.Process_Group_ID;
--        Sig   : in Signal);
   procedure Send_Signal (Sig : in Signal);

--    procedure Queue_Signal
--      (Process : in POSIX.Process_Identification.Process_ID;
--       Sig     : in Signal;
--       Data    : in Signal_Data);
   procedure Queue_Signal (Sig     : in Signal;
                           Data    : in Signal_Data);

   procedure Interrupt_Task
     (T : in Ada_Task_Identification.Task_Id);

private

   type Signal_Set is record
      C : aliased MSignal.Signal_Set := MaRTE.Kernel.Signals.Empty_Set;
   end record;

   type Signal_Info is new MaRTE.Kernel.Signals.Siginfo_T;
   type Signal_Event is new MaRTE.Kernel.Signals.Signal_Event;
   type Signal_Data is new MaRTE.Kernel.Signals.Sigval;

end POSIX.Signals;
