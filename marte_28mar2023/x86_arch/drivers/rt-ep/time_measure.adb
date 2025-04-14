----------------------------------------------------------------------------
--  ---------------------           RT-EP           ------------------------
----------------------------------------------------------------------------
--                         Copyright (C) 2003-2005
--                     Universidad de Cantabria, SPAIN
--                        http://www.ctr.unican.es/
--
--    This program is free software; you can redistribute it and/or
--    modify it under the terms of the GNU General Public
--    License as published by the Free Software Foundation; either
--    version 2 of the License, or (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--    General Public License for more details.
--
--    You should have received a copy of the GNU General Public
--    License along with this program; if not, write to the
--    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
--    Boston, MA 02111-1307, USA.
--
--    As a special exception, if other files instantiate generics from
--    this unit, or you link this unit with other files to produce an
--    executable, this  unit  does not  by itself cause  the resulting
--    executable to be covered by the GNU General Public License. This
--    exception does not however invalidate any other reasons why the
--    executable file  might be covered by the  GNU Public License.
--  ------------------------------------------------------------------------
--                                                     RT-EP /Ada95 {V1.1}
--
--                       't i m e _ m e a s u r e . a d b'
--
--                                     Ada
--
--
--  File 'time_measure.adb'                                        By Chema.
--                                                       Jose Maria Martinez
--                                                      <martinjm@unican.es>
--  Execution Time measure functions (body file).
--
--
--
--
-----------------------------------------------------------------------------

with POSIX; pragma Elaborate_All (POSIX);
with POSIX_IO; pragma Elaborate_All (POSIX_IO);
with POSIX_Timers;
--  with Ethernet_Tools; use Ethernet_Tools;
with Var_Strings;
with Calendar;
with Interfaces;
with Interfaces.C;

package body Time_Measure is

   use type POSIX.Timespec;

   type CPU_Task_Time_Info_T is record

      Measure_Name  : Var_String;
      Measure_Clock : Time_Measure_Clock := CPU_Time_Clock;
      CPU_Time_First : POSIX.Timespec := POSIX.To_Timespec (0.0);
      CPU_Time_Last : POSIX.Timespec := POSIX.To_Timespec (0.0);
      CPU_Time_Overhead : POSIX.Timespec := POSIX.To_Timespec (0.0);
      First_Measure : Boolean := True;
      CPU_Time_First_Measure : POSIX.Timespec := POSIX.To_Timespec (0.0);
      --  We store the stadistics here.
      Tmax : POSIX.Timespec := POSIX.To_Timespec (0.0);
      Tmin : POSIX.Timespec := POSIX.To_Timespec (1000000.0);
      --  The average execution time.
      Tmed : POSIX.Timespec := POSIX.To_Timespec (0.0);
      Iterative_Counter : Integer := 0;
      Measure_Suspended : Boolean := False;
   end record;

   --  Since we need an atomic boolean we define a proper type.
   type Atomic_Boolean is new Boolean;
   pragma Atomic (Atomic_Boolean);

   type Measure_Unit_T is record
      Enabled : Atomic_Boolean := False;
      CPU_Task_Time_Info : CPU_Task_Time_Info_T;
   end record;

   type Used_Measure_ID_T is array (Measure_ID) of Boolean;

   protected Monitor_Measure_ID is
      procedure Extract_Unused_ID (ID : out Measure_ID);
      procedure Insert_Unused_ID (ID : in Measure_ID);
      function Is_ID_Used (ID : in Measure_ID) return Boolean;
   private
      Measure : Used_Measure_ID_T := (others => False);
   end Monitor_Measure_ID;

   protected body Monitor_Measure_ID is

      procedure Extract_Unused_ID (ID : out Measure_ID) is
      begin
         for I in 1 .. Measure_ID'Last loop
            if Measure (I) = False then
               Measure (I) := True;
               ID := I;
               return;
            end if;
         end loop;
         --  If threre is no IDs free we return the special ID 0 which may
         --  raise an exception.
         ID := 0;
      end Extract_Unused_ID;

      procedure Insert_Unused_ID (ID : in Measure_ID) is

      begin
         Measure (ID) := False;
      end Insert_Unused_ID;

      function Is_ID_Used (ID : in Measure_ID) return Boolean is
      begin
         return Measure (ID);
      end Is_ID_Used;
   end Monitor_Measure_ID;

   type Measure_List_T is array (Measure_ID) of Measure_Unit_T;

   --  Global Unprotected variable that will store the measures.
   Measures : Measure_List_T;


   --  Measure_Unit_In_Use : will raise if we attempt to reset a measure unit
   --                        that it is in use.
   --  Measure_Unit_In_Use : exception;


   --  Reset_CPU_Time_Values : Will reset all the CPU Time values for a
   --                          given Measure_ID;
   --                          Yo can only reset value of a disable measure.
   --                          Measure_Unit_T.Enabled := False
   procedure Reset_CPU_Time_Values (Required_ID : Measure_ID);

   procedure Reset_CPU_Time_Values (Required_ID : Measure_ID) is


   begin
      if Measures (Required_ID).Enabled = False then
         Measures (Required_ID).CPU_Task_Time_Info.Tmax :=
           POSIX.To_Timespec (0.0);
         Measures (Required_ID).CPU_Task_Time_Info.Tmin :=
           POSIX.To_Timespec (1000000.0);
         Measures (Required_ID).CPU_Task_Time_Info.Tmed :=
           POSIX.To_Timespec (0.0);
         Measures (Required_ID).CPU_Task_Time_Info.Iterative_Counter := 0;
         Measures (Required_ID).CPU_Task_Time_Info.First_Measure := True;
         Measures (Required_ID).Enabled := True;

      else
         --  Since it is an internal procedure if you get here is because an
         --  error;
         raise Internal_Time_Measure_Error;
      end if;

   end Reset_CPU_Time_Values;

   --  Init_Task_Measure : will initialize the timing structure. Returns an
   --                      identification for the timing measure.
   --                      it has to be called before any time_mark.
   function Init_Time_Measure
     (Measure_Name : in Var_String;
      Msr_Time : in Time_Measure_Clock := CPU_Time_Clock)
     return Measure_ID is

      Required_ID : Measure_ID;
   begin
      Monitor_Measure_ID.Extract_Unused_ID (Required_ID);
      case Required_ID is
         when 0 =>
            raise No_More_IDs;
         when 1 .. Max_Measures =>
            Reset_CPU_Time_Values (Required_ID);
            --  We assign a Name for the measures.
            Measures (Required_ID).CPU_Task_Time_Info.Measure_Name :=
              Measure_Name;
            Measures (Required_ID).CPU_Task_Time_Info.Measure_Clock
              := Msr_Time;
            return Required_ID;
         when others =>
            raise Internal_Time_Measure_Error;
      end case;

   end Init_Time_Measure;

   --  Finish_Time_Measure : Will close the Measure_ID timing.
   procedure Finish_Time_Measure (ID : in Measure_ID) is

   begin
      Monitor_Measure_ID.Insert_Unused_ID (ID);
   end Finish_Time_Measure;

   --  This procedure is used to measure the execution time.
   --  It will store the time wated between Interval_T'First..Interval_T'Last
   --  It has to be placed between the area we would like to measure.

   procedure Set_Time_Mark (ID : in Measure_ID; Where : in Interval_T) is

      CPU_Time : POSIX.Timespec;

   begin

      --  We check if the measures is activated for this ID.
      if Measures (ID).Enabled then
         case Where is
            when First =>
               if Measures (ID).CPU_Task_Time_Info.Measure_Clock =
                 CPU_Time_Clock then
                  Measures (ID).CPU_Task_Time_Info.CPU_Time_First :=
                    POSIX_Timers.Get_Time (POSIX_Timers.Clock_Task_Cputime_Id);
               else
                  Measures (ID).CPU_Task_Time_Info.CPU_Time_First :=
                    POSIX_Timers.Get_Time (POSIX_Timers.Clock_Realtime);
               end if;

            when Last =>
               if Measures (ID).CPU_Task_Time_Info.Measure_Clock =
                 CPU_Time_Clock then
                  Measures (ID).CPU_Task_Time_Info.CPU_Time_Last :=
                    POSIX_Timers.Get_Time (POSIX_Timers.Clock_Task_Cputime_Id);
               else
                  Measures (ID).CPU_Task_Time_Info.CPU_Time_Last :=
                    POSIX_Timers.Get_Time (POSIX_Timers.Clock_Realtime);
               end if;

               CPU_Time := Measures (ID).CPU_Task_Time_Info.CPU_Time_Last -
                 Measures (ID).CPU_Task_Time_Info.CPU_Time_First;
               --  We separate the first measure.
               if Measures (ID).CPU_Task_Time_Info.First_Measure then
                  Measures (ID).CPU_Task_Time_Info.CPU_Time_First_Measure :=
                    CPU_Time;
                  Measures (ID).CPU_Task_Time_Info.First_Measure := False;
               else
                  if CPU_Time > Measures (ID).CPU_Task_Time_Info.Tmax then
                     Measures (ID).CPU_Task_Time_Info.Tmax := CPU_Time;
                  end if;
                  if CPU_Time < Measures (ID).CPU_Task_Time_Info.Tmin then
                     Measures (ID).CPU_Task_Time_Info.Tmin := CPU_Time;
                  end if;
                  --  We increment the measure counter since there is a
                  --  proper measure
                  if Measures (ID).CPU_Task_Time_Info.Iterative_Counter =
                    Integer'Last
                  then
                     --  We have achieved the maximun number of loops.
                     raise Counter_Exhausted;
                  else
                     --  We store the average time and increment the counter.
                     Measures (ID).CPU_Task_Time_Info.Tmed :=
                       Measures (ID).CPU_Task_Time_Info.Tmed + CPU_Time;
                     Measures (ID).CPU_Task_Time_Info.Iterative_Counter :=
                       Measures (ID).CPU_Task_Time_Info.Iterative_Counter + 1;
                  end if;
               end if;

            when Suspend =>
               if not Measures (ID).CPU_Task_Time_Info.Measure_Suspended then

                  if Measures (ID).CPU_Task_Time_Info.Measure_Clock =
                    CPU_Time_Clock
                  then
                     Measures (ID).CPU_Task_Time_Info.CPU_Time_Overhead :=
                       POSIX_Timers.Get_Time
                       (POSIX_Timers.Clock_Task_Cputime_Id);
                  else
                     Measures (ID).CPU_Task_Time_Info.CPU_Time_Overhead :=
                       POSIX_Timers.Get_Time
                       (POSIX_Timers.Clock_Realtime);
                  end if;


                  Measures (ID).CPU_Task_Time_Info.CPU_Time_Overhead :=
                    Measures (ID).CPU_Task_Time_Info.CPU_Time_Overhead -
                    Measures (ID).CPU_Task_Time_Info.CPU_Time_First;
                  Measures (ID).CPU_Task_Time_Info.Measure_Suspended := True;
               else
                  raise Already_Suspended;
               end if;

            when Release =>
               if Measures (ID).CPU_Task_Time_Info.Measure_Suspended then
                  if Measures (ID).CPU_Task_Time_Info.Measure_Clock =
                    CPU_Time_Clock
                  then
                     Measures (ID).CPU_Task_Time_Info.CPU_Time_First :=
                       POSIX_Timers.Get_Time
                       (POSIX_Timers.Clock_Task_Cputime_Id);
                  else
                     Measures (ID).CPU_Task_Time_Info.CPU_Time_First :=
                       POSIX_Timers.Get_Time
                       (POSIX_Timers.Clock_Realtime);
                  end if;

                  Measures (ID).CPU_Task_Time_Info.CPU_Time_First :=
                    Measures (ID).CPU_Task_Time_Info.CPU_Time_First -
                    Measures (ID).CPU_Task_Time_Info.CPU_Time_Overhead;
                  Measures (ID).CPU_Task_Time_Info.Measure_Suspended := False;
               else
                  raise Measure_Not_Suspended;
               end if;
            when others =>
               raise Internal_Time_Measure_Error;
         end case;
      end if; -- end  if Measures(ID).Enabled then

   end Set_Time_Mark;

   function Get_Stadistics
     (ID : in Measure_ID) return CPU_Measure_Stadistics_T is
      CPU_Stadistics : CPU_Measure_Stadistics_T;
   begin

      --  We enter in the critic section
      Measures (ID).Enabled := False;
      --  We need to be sure that no one is using the shared type
      --      delay 0.1;
      CPU_Stadistics.Tmax :=
        Interfaces.C.double (POSIX.To_Duration (Measures (ID).CPU_Task_Time_Info.Tmax));
      CPU_Stadistics.Tmin :=
        Interfaces.C.double (POSIX.To_Duration (Measures (ID).CPU_Task_Time_Info.Tmin));
      CPU_Stadistics.Iter :=
        Interfaces.C.int (Measures (ID).CPU_Task_Time_Info.Iterative_Counter);
      --  We now calculate the average time.
      if Measures (ID).CPU_Task_Time_Info.Iterative_Counter = 0 then
         CPU_Stadistics.Tmed := 0.0;
      else
         CPU_Stadistics.Tmed :=
           Interfaces.C.double (POSIX.To_Duration (
                                      Measures (ID).CPU_Task_Time_Info.Tmed) /
                                      Measures (ID).CPU_Task_Time_Info.Iterative_Counter);
      end if;

      CPU_Stadistics.First_Measure :=
        Interfaces.C.double (POSIX.
                To_Duration (Measures (ID).
                            CPU_Task_Time_Info.CPU_Time_First_Measure));
      CPU_Stadistics.Measure_Name :=
        Measures (ID).CPU_Task_Time_Info.Measure_Name;
      --  We now reset the measure unit.
      Reset_CPU_Time_Values (ID);
      --  We exit in the critic section
      Measures (ID).Enabled := True;

      return CPU_Stadistics;
   end Get_Stadistics;
   ----------------------
   -- Measure_Reporter --
   ----------------------

   --  This task will take the stadistics and will report them to an specific
   --  media.

   task Measure_Reporter is
      pragma Priority (Priority_Measure_Task);

   end Measure_Reporter;

   task body Measure_Reporter is
      Fd_Ethernet : POSIX_IO.File_Descriptor;

      type Eth_Packet is record
         Dest_Addr   :  Ethernet_Address;
         Source_Addr :  Ethernet_Address;
         Protocol_ID : Interfaces.Unsigned_16;
         Packet_Size : Interfaces.Unsigned_16;
         Data : CPU_Measure_Stadistics_T;
      end record;
      pragma Pack (Eth_Packet);
      for Eth_Packet use record
         Dest_Addr at 0 range 0 .. 47;
         Source_Addr at 6 range 0 .. 47;
         Protocol_ID at 12 range 0 .. 15;
         Packet_Size at 14 range 0 .. 15;
      end record;

      Data_To_Send : Eth_Packet;

      procedure Write_Ethernet is new
        POSIX_IO.Generic_Write (Eth_Packet);

      Next_Time : Calendar.Time;
      use type Interfaces.Unsigned_16;
   begin

      if  Enable_Task_Measure'Last = False or Enable_Measures'Last = False then
         abort Measure_Reporter;
      end if;

      Fd_Ethernet := POSIX_IO.Open (Network_Interface, POSIX_IO.Read_Write);

      Data_To_Send.Dest_Addr := Measure_Station;
      Data_To_Send.Protocol_ID := Measure_Protocol;
      Data_To_Send.Packet_Size :=
         Time_Measure.CPU_Measure_Stadistics_T'Size / 8;
      Next_Time := Calendar.Clock;
      loop
         Next_Time := Calendar."+"(Next_Time, Duration (Measure_Period));
         delay until Next_Time;

         for I in  1 .. Measure_ID'Last loop
            if Monitor_Measure_ID.Is_ID_Used (I) then
               Data_To_Send.Data := Get_Stadistics (I);
               Write_Ethernet (Fd_Ethernet, Data_To_Send);
               delay 0.01;
            end if;
         end loop;
      end loop;
   end Measure_Reporter;

end Time_Measure;
