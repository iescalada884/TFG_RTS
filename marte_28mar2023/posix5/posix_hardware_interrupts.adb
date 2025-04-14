------------------------------------------------------------------------------
----------------------         M a R T E   O S         -----------------------
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--            'P O S I X _ H a r d w a r e _ I n t e r r u p t s'
--
--                                 Body
--
--
--  File 'posix_hardware_interrupts.adb'                              By MAR.
--
--
--  Basic hardware interrupts management for Ada applications. Binding
--  for the POSIX draft for "Interrupt Control API" (P1003.2X/D1.0,
--  February 2001).
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
-------------------------------------------------------------------------------

with POSIX.Implementation;
with MaRTE.Kernel.Hardware_Interrupts.Operations;
with MaRTE.Timespec;
with MaRTE.Integer_Types;

package body POSIX_Hardware_Interrupts is

   package PI       renames POSIX.Implementation;
   package HWINT    renames MaRTE.Kernel.Hardware_Interrupts;
   package HWINTOP  renames MaRTE.Kernel.Hardware_Interrupts.Operations;
   package HWI      renames MaRTE.HAL;

   ---------------
   -- Associate --
   ---------------
   procedure Associate (Intr      : in Hardware_Interrupt;
                        Handler   : in Interrupt_Handler;
                        Area      : in System.Address;
                        Area_Size : in Natural) is
   begin
      PI.Check_NZ (HWINTOP.Posix_Intr_Associate
                   (Intr, HWINT.Interrupt_Handler_Function (Handler),
                    Area'Address,
                    MaRTE.Integer_Types.Size_T (Area'Size / 8)));
   end Associate;

   ------------------
   -- Disassociate --
   ------------------
   procedure Disassociate (Intr    : in Hardware_Interrupt;
                           Handler : in Interrupt_Handler) is
   begin
      PI.Check_NZ (HWINTOP.Posix_Intr_Disassociate
                   (Intr, HWINT.Interrupt_Handler_Function (Handler)));
   end Disassociate;

   ------------------------
   -- Wait for Interrupt --
   ------------------------
   Valid_Timespec : aliased MaRTE.Timespec.Timespec := (0, 0);
   procedure Wait (Intr    : out Hardware_Interrupt;
                   Handler : out Interrupt_Handler) is
   begin
      Handler := null; --  Just to avoid warning:
                       --  "Handler" is never assigned a value
      PI.Check_NZ
        (HWINTOP.Posix_Intr_Timedwait_HWTime
         (OS_Flags     => 0,
          Abs_Timeout  => 0,
          Abs_Timespec => null,
          Intr         => Intr'Unrestricted_Access,
          Handler      =>
            HWINT.Interrupt_Handler_Function (Handler)'Unrestricted_Access));
   end Wait;

   procedure Wait (Timeout : in  POSIX.Timespec;
                   Intr    : out Hardware_Interrupt;
                   Handler : out Interrupt_Handler) is
   begin
      Handler := null; --  Just to avoid warning:
                       --  "Handler" is never assigned a value
      PI.Check_NZ
        (HWINTOP.Posix_Intr_Timedwait_HWTime
         (OS_Flags     => 0,
          Abs_Timeout  => HWI.Duration_To_HWTime (Timeout),
          Abs_Timespec => Valid_Timespec'Access,
          Intr         => Intr'Unrestricted_Access,
          Handler      =>
            HWINT.Interrupt_Handler_Function (Handler)'Unrestricted_Access));
   end Wait;

   ----------
   -- Lock --
   ----------
   procedure Lock (Interrupt : in Hardware_Interrupt) is
   begin
      PI.Check_NZ (HWINTOP.Posix_Intr_Lock (Interrupt));
   end Lock;

   ------------
   -- Unlock --
   ------------
   procedure Unlock (Interrupt : in Hardware_Interrupt) is
   begin
      PI.Check_NZ (HWINTOP.Posix_Intr_unlock (Interrupt));
   end Unlock;

end POSIX_Hardware_Interrupts;
