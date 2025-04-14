------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                       'H A L . R e g i s t e r s'
--
--                                  Body
--
--
--  File 'marte-hal-registers.ads'                                   By MAR.
--
--
--  Access to the registers in the ARMv7-M architecture.
--
--  STM32F Version.
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

with System.Machine_Code;

package body MaRTE.HAL.Registers is

   -------------
   -- PRIMASK --
   -------------

   function PRIMASK return Interfaces.Unsigned_32 is
      PRIMASK_Reg : Interfaces.Unsigned_32;
   begin
      System.Machine_Code.Asm
        ("mrs %0, PRIMASK",
         Outputs => Interfaces.Unsigned_32'Asm_Output ("=r", PRIMASK_Reg),
         Volatile => True);
      return PRIMASK_Reg;
   end PRIMASK;

   ----------
   -- IPSR --
   ----------

   function IPSR return Interfaces.Unsigned_32 is
      IPSR_Reg : Interfaces.Unsigned_32;
   begin
      System.Machine_Code.Asm
        ("mrs %0, IPSR",
         Outputs => Interfaces.Unsigned_32'Asm_Output ("=r", IPSR_Reg),
         Volatile => True);
      return IPSR_Reg;
   end IPSR;
   
   ---------------------------
   -- Sys_Tick_Handler_Prio --
   ---------------------------

   function Sys_Tick_Handler_Prio return Interfaces.Unsigned_32 is
      SHPR3_Reg : Interfaces.Unsigned_32 := SHPR3;
   begin
      return Interfaces.Shift_Right (SHPR3_Reg, 
                                     SHPR3_Sys_Tick_Prio_Displacement);
   end Sys_Tick_Handler_Prio;

   -------------------------
   -- PendSV_Handler_Prio --
   -------------------------

   function PendSV_Handler_Prio return Interfaces.Unsigned_32 is
      SHPR3_Reg : Interfaces.Unsigned_32 := SHPR3;
   begin
      SHPR3_Reg := SHPR3_Reg and SHPR3_PendSV_Prio_Mask;
      return Interfaces.Shift_Right (SHPR3_Reg, 
                                     SHPR3_PendSV_Prio_Displacement);
   end PendSV_Handler_Prio;
         
end MaRTE.HAL.Registers;
