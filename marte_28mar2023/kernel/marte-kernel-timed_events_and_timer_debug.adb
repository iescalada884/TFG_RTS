------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--   'K e r n e l . T i m e d _ E v e n t s _ A n d _ T i m e r _ D e b u g'
--
--                                 Body
--
--
--  File 'k-timed_events_and_timer_debug.adb'                          By MAR.
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
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with MaRTE.Debug_Messages; use MaRTE.Debug_Messages;
with MaRTE.Kernel.Debug;
with MaRTE.Spy;

package body MaRTE.Kernel.Timed_Events_And_Timer_Debug is

   package DBG renames K.Debug;
   -------------------
   -- Program_Event --
   -------------------
   procedure Timer_Program_Event (At_Time  : in HAL.HWTime;
                                  Interval : in HAL.HWTime;
                                  T        : in HAL.HWTime) is
   begin
      if Timed_Events_Debug_Messages then
         Put ("  |  SetHWTimer["); DBG.Show_In_Secs (T);
         Put ("] at:");            DBG.Show_In_Secs (At_Time);
         Put (" intrv:");          DBG.Show_In_Secs (Interval, DBG.ABSO);
      end if;
   end Timer_Program_Event;

   --------------------
   -- Program_Normal --
   --------------------
   procedure Timer_Program_Normal (At_Time  : in HAL.HWTime;
                                   Interval : in HAL.HWTime) is
   begin
      if Timed_Events_Debug_Messages then
         Put ("$");
         null;
         --  Put ("  |  Timer Normal at:"); Put (Unsigned_64(At_Time));
         --  Put (" intrv:"); Put (Unsigned_64(Interval));
      end if;
   end Timer_Program_Normal;
   
   --------------------------------------
   -- Reprogram_Timer_After_Expiration --
   --------------------------------------
   
   procedure Reprogram_Timer_After_Expiration_0 is
   begin
      if Timed_Events_Debug_Messages then
         Put ("  ReprogTimer");
      end if;
   end Reprogram_Timer_After_Expiration_0;
     
   procedure Reprogram_Timer_After_Expiration_Do_Not is
   begin
      if Timed_Events_Debug_Messages then
         Put ("->Don't_Reprog");
      end if;
   end Reprogram_Timer_After_Expiration_Do_Not; 
    
   procedure Reprogram_Timer_After_Expiration_CPU is
   begin
      if Timed_Events_Debug_Messages then
         Put ("->CPUEvnt");
      end if;
   end Reprogram_Timer_After_Expiration_CPU;
    
   procedure Reprogram_Timer_After_Expiration_EQHead is
   begin
      if Timed_Events_Debug_Messages then
         Put ("->EQHead");
      end if;
   end Reprogram_Timer_After_Expiration_EQHead;
   
   ---------------------------
   -- More_Urgent_CPU_Event --
   ---------------------------
   
   procedure More_Urgent_CPU_Event (T : HAL.HWTime) is
   begin
      if Timed_Events_Debug_Messages then
         Put ("  | UrgentCPUEventOfHeir[");
         DBG.Show_In_Secs (T);
         Put ("]");
      end if;
   end More_Urgent_CPU_Event;

   ------------------------
   -- New_Standard_Event --
   ------------------------

   procedure New_Standard_Event (At_Time : HAL.HWTime;
                                 T       : HAL.HWTime) is
   begin
      MaRTE.Spy.Send_Event (At_Time, "EV", T);
   end New_Standard_Event;
   
   ---------------------------
   -- Extract_Expired_Event --
   ---------------------------
   
   procedure Extract_Expired_Event (Timer_Activation_Time : HAL.HWTime;
                                    Suspension_Time_Minimum : HAL.HWTime;
                                    EQ_Head : Timed_Event_Ac;
                                    CPUQ_Head : Timed_Event_Ac) is
      use type Timed_Event_Ac;
   begin
      if Timed_Events_Debug_Messages then
         Put (" Extr_ExpEvnt[TmrActT:");
         DBG.Show_In_Secs (Timer_Activation_Time);         
--         Put (",SusTMin:");
--         DBG.Show_In_Secs (Suspension_Time_Minimum, DBG.ABSO);          
         Put (",EQhead:");
         if EQ_Head = null then
            Put ("null");
         else
            DBG.Show_In_Secs (EQ_Head.T);
         end if;                   
         Put (",CPUQhead:");
         if CPUQ_Head = null then
            Put ("null");
         else
            DBG.Show_In_Secs (CPUQ_Head.T);
         end if;
         Put ("]");
      end if;
   end Extract_Expired_Event;
   
   -----------------------
   -- Extract_CPU_Event --
   -----------------------
   
   --  To be called after Extract_Expired_Event
   
   procedure Extract_CPU_Event (T : HAL.HWTime) is
   begin
      if Timed_Events_Debug_Messages then
         Put ("->CPUEvnt["); DBG.Show_In_Secs (T);
         Put ("]");
      end if;
   end Extract_CPU_Event;
   
   -----------------------
   -- No_Expired_Events --
   -----------------------
   
   --  To be called after Extract_Expired_Event
   
   procedure No_Expired_Events is
   begin
      if Timed_Events_Debug_Messages then
         Put ("->EQs:no_exp_evnts");
      end if;
   end No_Expired_Events;

end MaRTE.Kernel.Timed_Events_And_Timer_Debug;
