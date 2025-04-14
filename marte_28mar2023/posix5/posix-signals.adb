------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                         P O S I X . S I G N A L S                        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996-1998 Florida State University (FSU).    All Rights   --
--  Reserved.                                                               --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--  As a special exception, if other files instantiate generics from  this  --
--  unit, or you link this unit with other files to produce an  executable, --
--  this  unit does not by itself cause the  resulting  executable  to  be  --
--  covered  by the  GNU  General  Public License. This exception does not  --
--  however invalidate any other  reasons why the executable file might be  --
--  covered by the GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision: 857 $]

--  Please take care in future maintenance updates to avoid making
--  direct system calls that modify the signal action or signal
--  masking, and to coordinate changes with the GNAT runtime.

--  The implementation of this package is closely dependent on the
--  GNAT packages System.Interrupts, and
--  System.Interrupt_Management.  See comments in those packages
--  for related explanation of the design for signal handling.

--  Unfortunately, this means maintenance changes to Florist and
--  GNAT need to be synchronized.  A person with an older version of
--  GNAT will have problems using the current version of Florist.

--  The present design is a compromise.  If it were not for the
--  backward compatibility issue, all of the necessary POSIX
--  signal management support would be implemented directly in
--  the package System.Interrupts.  (That was the original design.)
--  We have tried to avoid changing the GNARL runtime system package
--  interfaces, in order that it would be possible to compile Florist
--  using earlier versions of GNAT.  This has meant in some cases
--  putting the implementation of new functionality (e.g., the
--  POSIX.5b Interrupt_Task and the POSIX.5c Install_Empty_Handler)
--  directly into the body of POSIX.Signals.  As a result, the
--  functionality is now divided between the two packages, in a
--  way that may not make much sense to a new reader.

--  With luck, it should be possible to compile this version of Florist
--  with earlier versions of GNAT.  There will a variable degree of
--  effect on the functioning of the signal management interfaces.
--  Since there were significant defects in this part of earlier releases
--  of Florist (detected by the POSIX.5b validation tests), we hope
--  no earlier Florist users are dependent on the way these operations
--  "worked" before.  We had to make the changes.

--  If the version of GNAT is out of sync with the version of Florist
--  there will be two distinct degrees of "reserved" signals.

--  1) Signals that the OS does not allow us to accept with sigwait or to
--     block with pthread_sigmask, or which are required to be reserved by
--     the POSIX Ada binding standard.
--     We call these "Reserved_Signals".

--  2) Signals that the GNAT runtime system reserves, and so we cannot
--     pass to operations like SI.Block_Signal.
--     We call these "SI_Reserved_Signals".

--  For simplicity, we merge Reserved_Signals into SI_Reserved_Signals,
--  so that we Resered_Signals is a subset of SI_Reserved_Signals.

--  If the versions of Florist and GNAT are in sync., these two sets
--  of reserved signals should be identical.

--  Ideally, there should be no operations in here that directly modify the
--  signal state of the process or thread.  For safety, all such operations
--  should be implemented by calls to operations in System.Interrupts.
--  Otherwise, we could break invariants upon which the Ada tasking
--  runtime system depends.  However, to allow this version of Florist
--  to be used with earlier versions of GNAT, there are some places where
--  direct system calls are done.  People doing maintenance should beware
--  of adding other direct calls without careful analysis of how they
--  might interact with what the GNAT runtime system is doing.
------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                        'P O S I X - S i g n a l s'
--
--                                  Body
--
--
--  File 'posix-signals.adb'                                           By MAR.
--
--
--  Package 'POSIX' as defined in IEEE Std 1003.5b-1996.
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
with Ada.Unchecked_Conversion;
with POSIX;
--  with POSIX.C;
with POSIX.Implementation;
with POSIX.Implementation.OK_Signals;
with System;
with System.Storage_Elements;
with System.Tasking;
with System.Interrupts;
with System.Interrupt_Management;
with System.Task_Primitives.Operations;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Kernel.Signals;
pragma Elaborate_All (MaRTE.Kernel.Signals);
with MaRTE.Kernel.Signals.POSIX_Functions;
package body POSIX.Signals is

   --  use POSIX.C;
   use POSIX.Implementation;
   use System;
   use System.Storage_Elements;
   use System.Tasking;

   use type MaRTE.Integer_Types.Int;

   package KSIG_POSIX renames MaRTE.Kernel.Signals.POSIX_Functions;

   package SI renames System.Interrupts;
   subtype SIID is SI.Interrupt_ID;

   function To_MSignal is
     new Ada.Unchecked_Conversion (Signal, MaRTE.Kernel.Signals.Signal);

   function To_MSigval is
     new Ada.Unchecked_Conversion (Signal_Data, MaRTE.Kernel.Signals.Sigval);

   function To_Signal_Data is
     new Ada.Unchecked_Conversion (MaRTE.Kernel.Signals.Sigval, Signal_Data);

   package Bogus_Signal_Enum is

      package PS renames POSIX.Signals;
      type Signal_Name_Enum is
        (Signal_Null,
         SIGNULL,
         Signal_Abort,
         SIGABRT,
         Signal_Alarm,
         SIGALRM,
         Signal_Bus_Error,
         SIGBUS,
         Signal_Floating_Point_Error,
         SIGFPE,
         Signal_Hangup,
         SIGHUP,
         Signal_Illegal_Instruction,
         SIGILL,
         Signal_Interrupt,
         SIGINT,
         Signal_Kill,
         SIGKILL,
         Signal_Pipe_Write,
         SIGPIPE,
         Signal_Quit,
         SIGQUIT,
         Signal_Segmentation_Violation,
         SIGSEGV,
         Signal_Terminate,
         SIGTERM,
         Signal_User_1,
         SIGUSR1,
         Signal_User_2,
         SIGUSR2,
         Signal_Child,
         SIGCHLD,
         Signal_Continue,
         SIGCONT,
         Signal_Stop,
         SIGSTOP,
         Signal_Terminal_Stop,
         SIGTSTP,
         Signal_Terminal_Input,
         SIGTTIN,
         Signal_Terminal_Output,
         SIGTTOU
         );

      Enum_To_Signal : array (Signal_Name_Enum'Range) of Signal :=
        (Signal_Null                   => 0,
         SIGNULL                       => 0,
         Signal_Abort                  => PS.SIGABRT,
         SIGABRT                       => PS.SIGABRT,
         Signal_Alarm                  => PS.SIGALRM,
         SIGALRM                       => PS.SIGALRM,
         Signal_Bus_Error              => PS.SIGBUS,
         SIGBUS                        => PS.SIGBUS,
         Signal_Floating_Point_Error   => PS.SIGFPE,
         SIGFPE                        => PS.SIGFPE,
         Signal_Hangup                 => PS.SIGHUP,
         SIGHUP                        => PS.SIGHUP,
         Signal_Illegal_Instruction    => PS.SIGILL,
         SIGILL                        => PS.SIGILL,
         Signal_Interrupt              => PS.SIGINT,
         SIGINT                        => PS.SIGINT,
         Signal_Kill                   => PS.SIGKILL,
         SIGKILL                       => PS.SIGKILL,
         Signal_Pipe_Write             => PS.SIGPIPE,
         SIGPIPE                       => PS.SIGPIPE,
         Signal_Quit                   => PS.SIGQUIT,
         SIGQUIT                       => PS.SIGQUIT,
         Signal_Segmentation_Violation => PS.SIGSEGV,
         SIGSEGV                       => PS.SIGSEGV,
         Signal_Terminate              => PS.SIGTERM,
         SIGTERM                       => PS.SIGTERM,
         Signal_User_1                 => PS.SIGUSR1,
         SIGUSR1                       => PS.SIGUSR1,
         Signal_User_2                 => PS.SIGUSR2,
         SIGUSR2                       => PS.SIGUSR2,
         Signal_Child                  => PS.SIGCHLD,
         SIGCHLD                       => PS.SIGCHLD,
         Signal_Continue               => PS.SIGCONT,
         SIGCONT                       => PS.SIGCONT,
         Signal_Stop                   => PS.SIGSTOP,
         SIGSTOP                       => PS.SIGSTOP,
         Signal_Terminal_Stop          => PS.SIGTSTP,
         SIGTSTP                       => PS.SIGTSTP,
         Signal_Terminal_Input         => PS.SIGTTIN,
         SIGTTIN                       => PS.SIGTTIN,
         Signal_Terminal_Output        => PS.SIGTTOU,
         SIGTTOU                       => PS.SIGTTOU
         );

      Signal_To_Enum : array (Signal'Range) of Signal_Name_Enum :=
        (0 => Signal_Null,
         PS.SIGABRT => Signal_Abort,
         PS.SIGALRM => Signal_Alarm,
         PS.SIGBUS  => Signal_Bus_Error,
         PS.SIGFPE  => Signal_Floating_Point_Error,
         PS.SIGHUP  => Signal_Hangup,
         PS.SIGILL  => Signal_Illegal_Instruction,
         PS.SIGINT  => Signal_Interrupt,
         PS.SIGKILL => Signal_Kill,
         PS.SIGPIPE => Signal_Pipe_Write,
         PS.SIGQUIT => Signal_Quit,
         PS.SIGSEGV => Signal_Segmentation_Violation,
         PS.SIGTERM => Signal_Terminate,
         PS.SIGUSR1 => Signal_User_1,
         PS.SIGUSR2 => Signal_User_2,
         PS.SIGCHLD => Signal_Child,
         PS.SIGCONT => Signal_Continue,
         PS.SIGSTOP => Signal_Stop,
         PS.SIGTSTP => Signal_Terminal_Stop,
         PS.SIGTTIN => Signal_Terminal_Input,
         PS.SIGTTOU => Signal_Terminal_Output,
         others  => Signal_Null);
   end Bogus_Signal_Enum;
   use Bogus_Signal_Enum;

   ------------------
   --  Global Data --
   ------------------

   Last_Unblocker : array (Signal) of Task_ID :=
     (others => Null_Task);
   pragma Volatile_Components (Last_Unblocker);
   --  Holds the ID of the last Task which Unblocked this Interrupt.
   --  It contains Null_Task if no tasks have ever requested the
   --  Unblocking operation or the Interrupt is currently Blocked.

   --  Reserved_Signal is the set of reserved signals, as defined
   --  by the POSIX.5 standard, augmented with the unblockable and
   --  uncatchable signals, SIGKILL and SIGSTOP.  The reserved signals
   --  includes the named required reserved signals, plus any other
   --  signals that are reserved by the implementation.  It is initialized
   --  in the begin-end block of the package body, below.

   type Signal_Bit_Vector is array (Signal) of Boolean;

   Reserved_Signal : Signal_Bit_Vector;

   --  SI_Reserved_Signal is the set of signals that are safe to pass to
   --  calls the operations of System.Interrupts, such as SI.Block_Signal.
   --  These signals also cannot be attached to Ada task entries.
   --  (This need not be the same as Reserved_Signal.)

   SI_Reserved_Signal : Signal_Bit_Vector;

   --  Signal_Disposition is use by Set_Blocked_Signals, to decide who
   --  should mask or unmask a given signal.

   type Signal_Disposition is
     (No_Change,
      SI_To_Mask,
      SI_To_Unmask);

   ------------------------
   --  Local Subprograms --
   ------------------------

   function Convert_Ids is new Ada.Unchecked_Conversion
     (Ada_Task_Identification.Task_Id, System.Tasking.Task_ID);


   procedure Check_Awaitable (Set : Signal_Set);
   pragma Inline (Check_Awaitable);

   procedure Null_Handler;
   pragma Convention (C, Null_Handler);

   procedure Void (Ignore : Int);
   pragma Inline (Void);

   --  The Await_Signal operations report Invalid_Argument for
   --  SIGKILL, SIGSTOP, and the reserved signals.

   procedure Check_Awaitable
     (Set : Signal_Set) is
   begin
      for Sig in Signal range 1 .. Signal'Last loop
         if Reserved_Signal (Sig) then
            --  The OS will not allow using sigwait with this signal.
            if MSignal.Sigismember (Set.C'Unchecked_Access,
                                    To_MSignal (Sig)) = 1 then
               Raise_POSIX_Error (Invalid_Argument);
            end if;
         elsif SI_Reserved_Signal (Sig) then
            --  The Ada runtime system will not allow attaching this signal
            --  to a task entry or protected procedure, but we can use it
            --  safely with sigwait.
            null;
         else
            --  This signal might be attached to a
            --  task entry or protected procedure
            if MSignal.Sigismember (Set.C'Unchecked_Access,
                                    To_MSignal (Sig)) = 1
              and then (SI.Is_Entry_Attached (SIID (Sig))
                or else SI.Is_Handler_Attached (SIID (Sig))) then
               Raise_POSIX_Error (Invalid_Argument);
            end if;
         end if;
      end loop;
   end Check_Awaitable;
   pragma Inline (Check_Awaitable);

   procedure Null_Handler is
   begin
      null;
   end Null_Handler;

   procedure Void (Ignore : Int) is
   begin
      null;
   end Void;

   -----------
   -- Image --
   -----------

   function Image (Sig : Signal) return String is
      Tmp : constant Signal_Name_Enum := Signal_To_Enum (Sig);
   begin
      if Tmp = Bogus_Signal_Enum.Signal_Null and then Sig /= 0 then
         declare
            Img : constant String := Signal'Image (Sig);
         begin
            return "SIGNAL_" & Img (Img'First + 1 .. Img'Last);
         end;
      else
         return Signal_Name_Enum'Image (Tmp);
      end if;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Str : String) return Signal is
      A : constant Positive := Str'First;
   begin
      if Str'Length > 7 and then Str (A .. A + 6) = "SIGNAL_"
        and then Str (A + 7) in '0' .. '9'
      then
         return Signal'Value (Str (A + 7 .. Str'Last));
      else
         return Enum_To_Signal (Signal_Name_Enum'Value (Str));
      end if;
   end Value;

   ----------------
   -- Add_Signal --
   ----------------

   procedure Add_Signal (Set : in out Signal_Set; Sig : Signal) is
   begin
      if Sig /= Signal_Null then
         Void (MSignal.Sigaddset (Set.C'Unchecked_Access,
                                  To_MSignal (Sig)));
      end if;
      --  Signal_Null (i.e., zero) is implicitly a member of every set.
   end Add_Signal;

   --------------------
   -- Add_All_Signal --
   --------------------

   procedure Add_All_Signals (Set : in out Signal_Set) is
   begin
      Void (MSignal.Sigfillset (Set.C'Unchecked_Access));
   end Add_All_Signals;

   -------------------
   -- Delete_Signal --
   -------------------

   procedure Delete_Signal (Set : in out Signal_Set; Sig : Signal) is
   begin
      if Sig /= Signal_Null then
         Void (MSignal.Sigdelset (Set.C'Unchecked_Access,
                                  To_MSignal (Sig)));
      end if;
   end Delete_Signal;

   ------------------------
   -- Delete_All_Signals --
   ------------------------

   procedure Delete_All_Signals (Set : in out Signal_Set) is
   begin
      if MSignal.Sigemptyset (Set.C'Unchecked_Access) = 0 then
         null;
      end if;
   end Delete_All_Signals;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (Set : Signal_Set; Sig : Signal) return Boolean is
   begin
      if Sig = Signal_Null
        or else MSignal.Sigismember (Set.C'Unchecked_Access,
                                     To_MSignal (Sig)) = 1 then
         return True;
      end if;
      return False;
   end Is_Member;

   -----------------------------------
   --  Set_Blocked_Signals   --
   -----------------------------------

   --  The operations that block/unblock signals do not raise an
   --  exception for any reserved or uncatchable signals, but
   --  quietly have no effect on the masking of SIGKILL, SIGSTOP,
   --  and the reserved signals.

   procedure Set_Blocked_Signals
     (New_Mask : in Signal_Set;
      Old_Mask : out Signal_Set) is
      os_new_mask : aliased MSignal.Signal_Set;
      Prev_Mask : Signal_Set;
      Disposition : array (Signal) of Signal_Disposition :=
        (others => No_Change);
   begin
      Begin_Critical_Section;
      Prev_Mask := Blocked_Signals;
      Void (MSignal.Pthread_Sigmask (MSignal.SIG_SETMASK,
                                     null,
                                     os_new_mask'Unchecked_Access));
      --  Partition the signals between those that
      --  are managed by System.Interrupts and those that we manage
      --  directly here.
      for Sig in Signal loop
         if SI_Reserved_Signal (Sig) then
            --  The OS and/or Ada runtime system will not allow us to
            --  change the mask of this signal.
            null;
         else
            --  It is OK to modify this signal's masking, using the
            --  interfaces of System.Interrupts.
            if MSignal.Sigismember
              (New_Mask.C'Unchecked_Access, To_MSignal (Sig)) = 1 then
               if not SI.Is_Blocked (SIID (Sig)) then
                  Disposition (Sig) := SI_To_Mask;
               end if;
            else
               if SI.Is_Blocked (SIID (Sig)) then
                  Disposition (Sig) := SI_To_Unmask;
               end if;
            end if;
         end if;
      end loop;
      --  Update the record of which task has which signal unblocked.
      for Sig in Signal loop
         case Disposition (Sig) is
         when No_Change => null;
         when SI_To_Mask =>
            SI.Block_Interrupt (SIID (Sig));
            --  ???? Rely that no exception can be raised, due to previous
            --  checks?  Otherwise, we need to provide a handler to end the
            --  critical section.
         when SI_To_Unmask =>
            SI.Unblock_Interrupt (SIID (Sig));
            --  ???? Rely that no exception can be raised, due to previous
            --  checks?  Otherwise, we need to provide a handler to end the
            --  critical section.
         end case;
      end loop;
      End_Critical_Section;
      Old_Mask := Prev_Mask;
   end Set_Blocked_Signals;

   ---------------------
   --  Block_Signals  --
   ---------------------

   procedure Block_Signals
     (Mask_to_Add : in Signal_Set;
      Old_Mask    : out Signal_Set) is
      os_new_mask : aliased MSignal.Signal_Set;
      Prev_Mask : Signal_Set;
      Disposition : array (Signal) of Signal_Disposition :=
        (others => No_Change);
   begin
      Begin_Critical_Section;
      Prev_Mask := Blocked_Signals;
      Void (MSignal.Sigemptyset (os_new_mask'Unchecked_Access));
      --  Partition the signals between those that
      --  are managed by System.Interrupts and those that we manage
      --  directly here.
      for Sig in Signal loop
         if SI_Reserved_Signal (Sig) then
            --  The OS and/or Ada runtime system will not allow us to
            --  change the mask of this signal.
            null;
         else
            --  It is OK to modify this signal's masking, using the
            --  interfaces of System.Interrupts.
            if MSignal.Sigismember
              (Mask_to_Add.C'Unchecked_Access, To_MSignal (Sig)) = 1 then
               if not SI.Is_Blocked (SIID (Sig)) then
                  Disposition (Sig) := SI_To_Mask;
               end if;
            else
               null;
            end if;
         end if;
      end loop;
      --  Update the record of which task has which signal unblocked.
      for Sig in Signal loop
         case Disposition (Sig) is
         when No_Change => null;
         when SI_To_Mask =>
            SI.Block_Interrupt (SIID (Sig));
            --  ???? Rely that no exception can be raised, due to previous
            --  checks?  Otherwise, we need to provide a handler to end the
            --  critical section.
         when SI_To_Unmask =>
            --  Should never get here!
            raise Program_Error;
         end case;
      end loop;
      End_Critical_Section;
      Old_Mask := Prev_Mask;
   end Block_Signals;

   -----------------------
   --  Unblock_Signals  --
   -----------------------

   procedure Unblock_Signals
     (Mask_to_Subtract : in Signal_Set;
      Old_Mask         : out Signal_Set) is
      os_new_mask : aliased MSignal.Signal_Set;
      Prev_Mask : Signal_Set;
      Disposition : array (Signal) of Signal_Disposition :=
        (others => No_Change);
   begin
      Begin_Critical_Section;
      Prev_Mask := Blocked_Signals;
      Void (MSignal.Sigemptyset (os_new_mask'Unchecked_Access));
      --  Partition the signals between those that
      --  are managed by System.Interrupts and those that we manage
      --  directly here.
      for Sig in Signal loop
         if SI_Reserved_Signal (Sig) then
            --  The OS and/or Ada runtime system will not allow us to
            --  change the mask of this signal.
            null;
         else
            --  It is OK to modify this signal's masking, using the
            --  interfaces of System.Interrupts.
            if MSignal.Sigismember
              (Mask_to_Subtract.C'Unchecked_Access, To_MSignal (Sig)) = 1 then
               if SI.Is_Blocked (SIID (Sig)) then
                  Disposition (Sig) := SI_To_Unmask;
               end if;
            end if;
         end if;
      end loop;
      --  Update the record of which task has which signal unblocked.
      for Sig in Signal loop
         case Disposition (Sig) is
         when No_Change => null;
         when SI_To_Mask =>
            raise Program_Error;
            --   Should never get here!
         when SI_To_Unmask =>
            SI.Unblock_Interrupt (SIID (Sig));
            --  ???? Rely that no exception can be raised, due to previous
            --  checks?  Otherwise, we need to provide a handler to end the
            --  critical section.
         end case;
      end loop;
      End_Critical_Section;
      Old_Mask := Prev_Mask;
   end Unblock_Signals;

   -----------------------
   --  Blocked_Signals  --
   -----------------------

   function Blocked_Signals return Signal_Set is
      Old_Mask : Signal_Set;
   begin
      --  Get thread-level signal mask, directly from OS.
      if MSignal.Pthread_Sigmask (MSignal.SIG_BLOCK,
                                  null,
                                  Old_Mask.C'Unchecked_Access) = 0 then
         null;
      end if;
      --  Merge in view from System.Interrupts.
      for Sig in Signal loop
         if not SI_Reserved_Signal (Sig) then
            if SI.Is_Blocked (SIID (Sig)) then
               Void (MSignal.Sigaddset (Old_Mask.C'Unchecked_Access,
                                        To_MSignal (Sig)));
            else
               Void (MSignal.Sigdelset (Old_Mask.C'Unchecked_Access,
                                        To_MSignal (Sig)));
            end if;
         end if;
      end loop;
      return Old_Mask;
   end Blocked_Signals;

   -------------------
   -- Ignore_Signal --
   -------------------

   --  The signal ignoring/unignoring operations report
   --  Invalid_Operation for SIGKILL, SIGSTOP, the reserved signals,
   --  Signal_Null, or any other signals for which the signal action
   --  is not permitted to be set by an application.

   procedure Ignore_Signal (Sig : in Signal) is
   begin
      if SI_Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
      else
         SI.Ignore_Interrupt (SIID (Sig));
      end if;
   end Ignore_Signal;

   ---------------------
   -- Unignore_Signal --
   ---------------------

   procedure Unignore_Signal (Sig : in Signal) is
   begin
      if SI_Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
      else
         SI.Unignore_Interrupt (SIID (Sig));
      end if;
   end Unignore_Signal;

   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Sig : Signal) return Boolean is
   begin
      if SI_Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
         return False;
      else
         return SI.Is_Ignored (SIID (Sig));
      end if;
   end Is_Ignored;

   ---------------------------
   -- Install_Empty_Handler --
   ---------------------------

   --  This is a POSIX.5c addition.

   procedure Install_Empty_Handler (Sig : Signal) is
      act, oact : aliased MSignal.Struct_Sig_Action;
      Result : Int;
   begin
      if Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      Begin_Critical_Section;
      act.Sa_Flags := 0; -- Option_Sets.Empty_Option_Set;
      act.Sa_Handler := Null_Handler'Address;
      Check (MSignal.Sigemptyset (act.Sa_Mask'Unrestricted_Access));
      Result := MSignal.Sigaction (To_MSignal (Sig),
                                   act'Unchecked_Access,
                                   oact'Unchecked_Access);
      End_Critical_Section;
      Check (Result);
   end Install_Empty_Handler;


   ------------------
   --  Get_Signal  --
   ------------------

   function Get_Signal (Event : Signal_Event) return Signal is
   begin
      return Signal (Event.Event_Signal);
   end Get_Signal;

   ------------------
   --  Set_Signal  --
   ------------------

   procedure Set_Signal
     (Event : in out Signal_Event;
      Sig   : in Signal) is
   begin
      Event.Event_Signal := To_MSignal (Sig);
   end Set_Signal;

   ------------------------
   --  Get_Notification  --
   ------------------------

   function Get_Notification (Event : Signal_Event) return Notification is
   begin
      return Event.Event_Notification;
   end Get_Notification;

   ------------------------
   --  Set_Notification  --
   ------------------------

   procedure Set_Notification
     (Event  : in out Signal_Event;
      Notify : in Notification) is
   begin
      Event.Event_Notification := Notify;
   end Set_Notification;

   ----------------
   --  Get_Data  --
   ----------------

   function Get_Data (Event : Signal_Event) return Signal_Data is
   begin
      return To_Signal_Data (Event.Event_Sigval);
   end Get_Data;

   ----------------
   --  Set_Data  --
   ----------------

   procedure Set_Data (Event : in out Signal_Event;
                       Data  : in     Signal_Data) is
   begin
      Event.Event_Sigval := To_MSigval (Data);
   end Set_Data;

   ------------------
   --  Get_Signal  --
   ------------------

   function Get_Signal (Info : Signal_Info) return Signal is
   begin
      return Signal (Info.Signo);
   end Get_Signal;

   ------------------
   --  Set_Signal  --
   ------------------

   procedure Set_Signal
     (Info : in out Signal_Info;
      Sig  : in Signal) is
   begin
      Info.Signo := To_MSignal (Sig);
   end Set_Signal;

   ------------------
   --  Get_Source  --
   ------------------

   function Get_Source (Info : Signal_Info) return Signal_Source is
   begin
      return Signal_Source (Info.Code);
   end Get_Source;

   ------------------
   --  Set_Source  --
   ------------------

   procedure Set_Source
     (Info   : in out Signal_Info;
      Source : in Signal_Source) is
   begin
      Info.Code := MaRTE.Kernel.Signals.Cause_Of_Signal (Source);
   end Set_Source;

   ----------------
   --  Has_Data  --
   ----------------

   function Has_Data (Source : Signal_Source) return Boolean is
   begin
      return (Source = From_Queue_Signal) or (Source = From_Timer);
   end Has_Data;

   ----------------
   --  Get_Data  --
   ----------------

   function Get_Data (Info : Signal_Info) return Signal_Data is
   begin
      return To_Signal_Data (Info.Value);
   end Get_Data;

   ----------------
   --  Set_Data  --
   ----------------

   procedure Set_Data
     (Info : in out Signal_Info;
      Data : in Signal_Data) is
   begin
      Info.Value := To_MSigval (Data);
   end Set_Data;


   --------------------
   --  Await_Signal  --
   --------------------

   function Await_Signal (Set : Signal_Set) return Signal is
      Result   : aliased MSignal.Signal;
   begin
      Check_Awaitable (Set);
      if MSignal.Sigwait (Set.C'Unchecked_Access,
                          Result'Unchecked_Access) = -1 then
         Raise_POSIX_Error (Fetch_Errno);
      end if;
      return Signal (Result);
   end Await_Signal;

   --------------------
   --  Await_Signal  --
   --------------------

   function Await_Signal (Set : Signal_Set) return Signal_Info is
      Info : aliased MaRTE.Kernel.Signals.Siginfo_T;
   begin
      Check_Awaitable (Set);
      Check (MSignal.Sigwaitinfo (Set.C'Unchecked_Access,
                                  Info'Unchecked_Access));
      return Signal_Info (Info);
   end Await_Signal;


   ------------------------
   --  Signal_Reference  --
   ------------------------

   function Signal_Reference (Sig : Signal) return System.Address is
   begin
      --  Signal_Reference reports Invalid_Argument if signal entries
      --  are not supported for the specified signal.
      if SI_Reserved_Signal (Sig) then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return To_Address (Integer_Address (Sig));
   end Signal_Reference;


   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal (Sig : in Signal) is
   begin
      Check (MSignal.Kill (0, To_MSignal (Sig)));
   end Send_Signal;

   --------------------
   --  Queue_Signal  --
   --------------------

   procedure Queue_Signal
     (Sig     : in Signal;
      Data    : in Signal_Data) is
   begin
      Check (KSIG_POSIX.Sigqueue (0, To_MSignal (Sig), To_MSigval (Data)));
   end Queue_Signal;

   ----------------------
   --  Interrupt_Task  --
   ----------------------

   procedure Interrupt_Task (T : in Ada_Task_Identification.Task_Id) is
   begin
      System.Task_Primitives.Operations.Abort_Task (Convert_Ids (T));
   end Interrupt_Task;

begin

   for Sig in Signal loop
      case Sig is
      when SIGALRM | SIGBUS | SIGILL | SIGSEGV | SIGFPE | SIGABRT |
           SIGKILL | SIGSTOP =>
         Reserved_Signal (Sig) := True;
      when others =>
         Reserved_Signal (Sig) :=
           not POSIX.Implementation.OK_Signals.OK (Integer (Sig));
      end case;
   end loop;

   SI_Reserved_Signal := Reserved_Signal;
   --  Merge in signals that are reserved by the Ada runtime system.
   for Sig in Signal loop
      if SIID'Base (Sig) in SIID'Range then
         if SI.Is_Reserved (SIID (Sig)) then
            SI_Reserved_Signal (Sig) := True;
         end if;
      else SI_Reserved_Signal (Sig) := True;
      end if;
   end loop;
   --  Temporary hack....trust the runtime system.
   Reserved_Signal := SI_Reserved_Signal;

   --  .....Fix POSIX.5?????
   --  There is presently no portable way to catch SIGCHLD, since
   --  the default action is to ignore it, and the OS is allowed to
   --  throw away ignored signals even when the signal is masked.
   --  This is also true of other signals for which the default action
   --  is to ignore the signal. The following is a temporary hack.
   --  It would be better to fix this in GNARL.
   --  See also the comments on Install_Empty_Handler.

   for Sig in Signal loop
      if not Reserved_Signal (Sig)
        and then POSIX.Implementation.OK_Signals.No_Default (Integer (Sig))
      then
         Install_Empty_Handler (Sig);
      end if;
   end loop;

end POSIX.Signals;
