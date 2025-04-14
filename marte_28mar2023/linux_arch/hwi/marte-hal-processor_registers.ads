------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'P r o c e s s o r _ R e g i s t e r s'
--
--                                  Spec
--
--
--  File 'processor_registers.ads'               By MAR and
--                                                  Miguel �ngel Masmano Tello.
--
--
--  x86 registers.
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
--  ----------------------------------------------------------------------
--  The idea of running MaRTE OS as a Linux process is due to Miguel Angel
--  Masmano     Tello     (Universidad    Politecnica     de     Valencia)
--  <mimastel@doctor.upv.es>.
--  He  is  also  the  author  of   most  of  the  code  involved  in  the
--  implementation of the hardware interface for this architecture.
--
------------------------------------------------------------------------------

with System; --  for 'Address'
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package MaRTE.HAL.Processor_Registers is
   pragma Preelaborate;

   -------------------
   -- CPU Registers --
   -------------------
   function Get_ESP_Register return Unsigned_32;
   pragma Import (C, Get_ESP_Register, "get_esp_register");
   --  Defined in 'processor_registers.c'.
   -------------------
   -- Context Swich --
   -------------------
   procedure Change_Return_Address_Of_Preempted_Task
     (Top_Of_Stack    : in Unsigned_32;
      New_Ret_Address : in System.Address);
   pragma Inline (Change_Return_Address_Of_Preempted_Task);
   pragma Export (C, Change_Return_Address_Of_Preempted_Task);

   procedure Context_Switch (Old_Task : in System.Address;
                             New_Task : in System.Address);
   pragma Inline (Context_Switch);

   procedure Change_To_Context (New_Task : in System.Address);
   pragma Inline (Change_To_Context);

end MaRTE.HAL.Processor_Registers;
