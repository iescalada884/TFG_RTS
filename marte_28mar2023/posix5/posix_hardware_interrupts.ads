------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--            'P O S I X _ H a r d w a r e _ I n t e r r u p t s'
--
--                                 Spec
--
--
--  File 'posix_hardware_interrupts.ads'                              By MAR.
--
--
--  Basic hardware interrupts management for Ada applications. Binding
--  for the POSIX draft for "Interrupt Control API" (P1003.2X/D1.0,
--  February 2001).
--
--  This package allows installing interrupt handlers (and associate
--  tasks with interrupts) using 'Associate'. An associated task can
--  synchronize with the interrupt handler awaiting for the interrupt
--  using 'Wait'.
--
--  Semaphores can be used as an alternative synchronization mechanism
--  between tasks and handlers. It can be used for this purpose
--  package 'POSIX_Semaphores' but also 'MaRTE_Semaphores'
--  ('misc/marte_semaphores.ads') a simplified semaphores management
--  package specifically intended to be used in drivers (to avoid
--  including all the POSIX interface in MaRTE MaRTE.Kernel only because a
--  driver requires using semaphores).
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
with System;
with POSIX;
with MaRTE.Kernel.Hardware_Interrupts;
with MaRTE.HAL;

package POSIX_Hardware_Interrupts is

   ----------------------------
   -- PC hardware interrupts --
   ----------------------------
   subtype Hardware_Interrupt is MaRTE.Kernel.Hardware_Interrupts.Intr_T;

   TIMER_INTERRUPT       : Hardware_Interrupt renames
     MaRTE.HAL.TIMER_IRQ; -- Used by MaRTE OS
   KEYBOARD_INTERRUPT    : Hardware_Interrupt renames
     MaRTE.HAL.KEYBOARD_IRQ;
   CTLR2_INTERRUPT       : Hardware_Interrupt renames
     MaRTE.HAL.CTLR2_IRQ;
   SERIAL2_INTERRUPT     : Hardware_Interrupt renames
     MaRTE.HAL.SERIAL2_IRQ;
   SERIAL1_INTERRUPT     : Hardware_Interrupt renames
     MaRTE.HAL.SERIAL1_IRQ;
   PARALLEL2_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.PARALLEL2_IRQ;
   DISKETTE_INTERRUPT    : Hardware_Interrupt renames
     MaRTE.HAL.DISKETTE_IRQ;
   PARALLEL1_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.PARALLEL1_IRQ;
   RTC_INTERRUPT         : Hardware_Interrupt renames
     MaRTE.HAL.RTC_IRQ;
   SOFT_INTERRUPT        : Hardware_Interrupt renames
     MaRTE.HAL.SOFT_IRQ;
   RESERVED1_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.RESERVED1_IRQ;
   RESERVED2_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.RESERVED2_IRQ;
   RESERVED3_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.RESERVED3_IRQ;
   COPROCESSOR_INTERRUPT : Hardware_Interrupt renames
     MaRTE.HAL.COPROCESSOR_IRQ;
   FIXED_DISK_INTERRUPT  : Hardware_Interrupt renames
     MaRTE.HAL.FIXED_DISK_IRQ;
   RESERVED4_INTERRUPT   : Hardware_Interrupt renames
     MaRTE.HAL.RESERVED4_IRQ;

   --------------------------------
   -- Type 'Handler_Return_Code' --
   --------------------------------
   subtype Handler_Return_Code
                           is MaRTE.Kernel.Hardware_Interrupts.Handler_Return_Code;
   POSIX_INTR_HANDLED_NOTIFY : Handler_Return_Code
     renames MaRTE.Kernel.Hardware_Interrupts.POSIX_INTR_HANDLED_NOTIFY;
   --  The interrupt handler handled this interrupt, and the task that
   --  registered the handler should be notified that the interrupt
   --  occurred.
   POSIX_INTR_HANDLED_DO_NOT_NOTIFY : Handler_Return_Code
     renames MaRTE.Kernel.Hardware_Interrupts.POSIX_INTR_HANDLED_DO_NOT_NOTIFY;
   --  The interrupt handler handled this interrupt, but the task that
   --  registered the handler should NOT be notified that the interrupt
   --  occurred.
   POSIX_INTR_NOT_HANDLED : Handler_Return_Code
     renames MaRTE.Kernel.Hardware_Interrupts.POSIX_INTR_NOT_HANDLED;
   --  The interrupt handler did not handle this interrupt; if there are
   --  other handlers connected to this interrupt, then the next handler
   --  should be called.

   ------------------------------
   -- Type 'Interrupt_Handler' --
   ------------------------------
   type Interrupt_Handler is access function (Area : in System.Address;
                                              Intr : in Hardware_Interrupt)
                                             return Handler_Return_Code;

   -------------------------------------
   -- Associate tasks with interrupts --
   -------------------------------------
   --
   --  Install interrupt handlers and associate tasks with interrupts.
   procedure Associate (Intr      : in Hardware_Interrupt;
                        Handler   : in Interrupt_Handler;
                        Area      : in System.Address;
                        Area_Size : in Natural);

   procedure Disassociate (Intr    : in Hardware_Interrupt;
                           Handler : in Interrupt_Handler);

   ------------------------
   -- Wait for Interrupt --
   ------------------------
   --
   --  A task should be associated with the interrupt before calling
   --  one of these procedures.
   --
   --  Raises POSIX_Error if task is not associated with interrupt
   procedure Wait (Intr    : out Hardware_Interrupt;
                   Handler : out Interrupt_Handler);
   procedure Wait (Timeout : in  POSIX.Timespec;
                   Intr    : out Hardware_Interrupt;
                   Handler : out Interrupt_Handler);
   -----------------------------------
   -- Enable and Disable Interrupts --
   -----------------------------------
   procedure Lock (Interrupt : in Hardware_Interrupt);
   pragma Inline (Lock);

   procedure Unlock (Interrupt : in Hardware_Interrupt);
   pragma Inline (Unlock);

end POSIX_Hardware_Interrupts;
