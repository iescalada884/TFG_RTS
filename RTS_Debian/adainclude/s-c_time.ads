------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . C _ T I M E                               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1998-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the timeval, timespec C types and conversions.
--  It hides all details about time_t, suseconds_t.

with Interfaces.C;
private with System.OS_Constants;

package System.C_Time
  with Pure
is

   --  These C structs represent non negative durations with different
   --  accuracies and maximal values.
   type timespec is private;  --  accurate to 1  nanosecond
   type timeval  is private;  --  accurate to 1 microsecond

   --  Duration is accurate to either 1 nanosecond or 20 milliseconds.
   subtype Non_Negative_Duration is Duration range 0.0 .. Duration'Last;

   --  All conversions round if the target type is less accurate than
   --  the source type, away from zero if the source value is midway
   --  between two values of the target type.
   --  They raise Constraint_Error when the value, after rounding,
   --  exceeds the last value of the target type.

   function To_Duration (T : timespec) return Non_Negative_Duration;
   function To_Duration (T : timeval)  return Non_Negative_Duration;

   function To_Timespec (D : Non_Negative_Duration) return timespec;
   function To_Timeval  (D : Non_Negative_Duration) return timeval;

   Timeval_Zero : constant timeval;
   --  g-sothco.ads

   function In_Duration (T : timeval) return Boolean with Inline;
   --  True if computing To_Duration (T) is safe,
   --  False if Constraint Error would be raised.
   --  g-socket.adb:

   subtype Non_Negative_Int is
     Interfaces.C.int range 0 .. Interfaces.C.int'Last;

   function Milliseconds_To_Timeval (M : Non_Negative_Int) return timeval
     with Inline;
   --  g-spogwa.adb

   function Nanoseconds_To_Timespec (N : Non_Negative_Int) return timespec
     with Inline;
   function To_Timespec (T : timeval) return timespec with Inline;
   --  s-osinte__darwin.adb

   --  These functions are provided for backward compatibility,
   --  but lead to non portable interfaces with C.
   --  Tv_sec and tv_nsec do not match the long int type on x32,
   --  or on 32 bits ARM with a 2038-compatible GNU libc.

   subtype Tv_Sec_Long  is Interfaces.C.long range 0 .. Interfaces.C.long'Last;
   subtype Tv_Nsec_Long is Interfaces.C.long range 0 .. 999_999_999;
   function To_Duration (tv_sec  : Tv_Sec_Long;
                         tv_nsec : Tv_Nsec_Long) return Non_Negative_Duration
     with Inline;
   procedure To_Struct_Timespec (D       : Non_Negative_Duration;
                                 tv_sec  : out Tv_Sec_Long;
                                 tv_nsec : out Tv_Nsec_Long) with Inline;
   --  a-calcon.ads

private

   type time_t is range -2 ** (OS_Constants.SIZEOF_tv_sec * 8 - 1) ..
                         2 ** (OS_Constants.SIZEOF_tv_sec * 8 - 1) - 1
     with Convention => C, Size => OS_Constants.SIZEOF_tv_sec * 8;

   type usec_t is range -2 ** (OS_Constants.SIZEOF_tv_usec * 8 - 1) ..
                         2 ** (OS_Constants.SIZEOF_tv_usec * 8 - 1) - 1
     with Convention => C, Size => OS_Constants.SIZEOF_tv_usec * 8;
   --  Larger than the suseconds_t C type on ARM 32 bits with GNU libc
   --  when __TIME_BITS=64.

   type nsec_t is range -2 ** (OS_Constants.SIZEOF_tv_nsec * 8 - 1) ..
                         2 ** (OS_Constants.SIZEOF_tv_nsec * 8 - 1) - 1
     with Convention => C, Size => OS_Constants.SIZEOF_tv_nsec * 8;
   --  Larger than the signed long int C type on x32.

   type timeval is record
      tv_sec  : time_t range 0 .. OS_Constants.MAX_tv_sec;  --  seconds
      tv_usec : usec_t range 0 .. 999_999;                  --  microseconds
   end record
     with Convention => C;

   type timespec is record
      tv_sec  : time_t range 0 .. OS_Constants.MAX_tv_sec;  --  seconds
      tv_nsec : nsec_t range 0 .. 999_999_999;              --  nanoseconds
   end record
     with Convention => C;

   Timeval_Zero : constant timeval := (tv_sec => 0, tv_usec => 0);

end System.C_Time;
