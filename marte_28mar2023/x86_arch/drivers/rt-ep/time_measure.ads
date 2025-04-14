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
--                       't i m e _ m e a s u r e . a d s'
--
--                                     Ada
--
--
--  File 'time_measure.ads'                                        By Chema.
--                                                       Jose Maria Martinez
--                                                      <martinjm@unican.es>
--  Execution Time measure functions (spec file).
--
--
--
--
-----------------------------------------------------------------------------

with POSIX;
--
with Var_Strings; use Var_Strings;
with Interfaces;
with Interfaces.C;

package Time_Measure is

   type Ethernet_Address is array (1 .. 6) of Interfaces.Unsigned_8;
   for Ethernet_Address'Size use 48;
   pragma Pack (Ethernet_Address);

   type Interval_T is (First, Last, Suspend, Release);
   --  This need to me in this way due to C bindings.
   for Interval_T use (First => 0,
                       Last => 1,
                       Suspend => 2,
                       Release => 3);
   for Interval_T'Size use 8;

   type Measure_ID is private;

   --  This will help to disable/enable the measures through the measuring code
   subtype Enable_Measures is Boolean range False .. False;

   --  Enable_Task_Measure will Enable / disable the Measure Task. Is useful to
   --  disable the measure task if we pretend to take the measures externally
   --  To disable define Enable_Task_Measure range False..False

   subtype Enable_Task_Measure is Boolean range False .. False;

   type CPU_Measure_Stadistics_T is
      record
         Measure_Name  : Var_String;
         First_Measure : Interfaces.C.double;
         Tmax          : Interfaces.C.double;
         Tmin          : Interfaces.C.double;
         Tmed          : Interfaces.C.double;
         Iter          : Interfaces.C.int;
      end record;
   --  We can adjust the priority of the Measure Task.
   Priority_Measure_Task : constant := 30;

   --  Definition of the network interface to open
   Network_Interface : POSIX.POSIX_String := "/dev/eth0";


   --  We have to assign the MAC address of the destination machine which
   --  is in charge of dealing with the measures.

   Measure_Station : Ethernet_Address :=
     (16#00#, 16#08#, 16#0D#, 16#96#, 16#15#, 16#F3#); -- portatil11
   --  (16#00#,16#A0#,16#24#,16#AE#,16#69#,16#92#); -- portatil8
   --  (16#00#,16#02#,16#B3#,16#BE#,16#34#,16#52#);
   --  In order to be able to fetch the measure packet, we have to assign
   --  a proper transmision ethernet protocol (default 0x1010)

   Measure_Protocol : Interfaces.Unsigned_16 := 16#1010#;

   --  Measure_Period is the period in seconds of the measure.
   --  (the measure will be periodic).
   Measure_Period : constant := 30.0;

   --  Exceptions:
   --  No_More_IDs : Will occur when attempting to Init_Time_Measure and there
   --                are no more Measure_ID free to use.

   No_More_IDs : exception;

   --  Internal_Time_Measure_Exception : Should never be raised, if raised
   --                                    you have to review the code or
   --                                    configuration.
   Internal_Time_Measure_Error : exception;

   --  Counter_Exhausted : Will raise if we attempt to do measuress for too
   --                      many loops: The more loops the more precise is the
   --                      timming information, but currently there is a
   --                      (implementation change) limit of Integer'last loops.
   --                      A loop is started by a Set_Time_Mark(ID,First) and
   --                      closed by a Set_Time_Mark(ID,Last)
   --
   Counter_Exhausted : exception;

   --  Already_Suspended : This exception will raise if we a attempting to
   --                      suspend an already suspended measure.
   Already_Suspended : exception;

   --  Measure_Not_Suspended : This exception will raise when trying to release
   --                          a unsuspended measure.
   Measure_Not_Suspended : exception;

   --  All the measures are taken identified whith a Measure_ID type.
   --  It is the responsability of the application to assure consistency
   --  in the use of the identifier. That is if you initialize a measure
   --  you have a Measure_ID that might be the one you use to take marks
   --  and also to finish the time taken procedure.

   --  Init_Task_Measure : will initialize the timing structure. Returns an
   --                      identification for the timing measure.
   --                      it has to be called before any time_mark.
   --  In order to generate proper output files the identifier string
   --  'measure_name' assigned to the initialization function
   --  init_time_measure has to be named as:
   --                   resource : operation
   --   See the importance of the ':' to separate both fields
   --  The Time_Measure_Clock will set the CPU time measure or the monotonic
   --  measure.

   type Time_Measure_Clock is (CPU_Time_Clock, ABS_Time_Clock);

   function Init_Time_Measure (Measure_Name : in Var_String;
                               Msr_Time : in Time_Measure_Clock :=
                                 CPU_Time_Clock)
                              return Measure_ID;

   --  Finish_Time_Measure : Will close the Measure_ID timing.

   procedure Finish_Time_Measure (ID : in Measure_ID);

   --  This procedure is used to measure the execution time.
   --  It will store the time wasted between Interval_T'First..Interval_T'Last
   --  It has to be placed between the area we would like to measure.

   procedure Set_Time_Mark (ID : in Measure_ID; Where : in Interval_T);

   --  Get_Stadistics will extract current stadistics for a specific ID.
   function Get_Stadistics (ID : in Measure_ID)
                           return CPU_Measure_Stadistics_T;

private

   --  We define the maximum number of measures that can be taken into
   --  account at a given time.

   Max_Measures : constant := 10000;

   type Measure_ID is new Interfaces.Unsigned_32 range 0 .. Max_Measures;

end Time_Measure;
