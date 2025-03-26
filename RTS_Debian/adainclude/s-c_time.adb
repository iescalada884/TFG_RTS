------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . C _ T I M E                               --
--                                                                          --
--                                  B o d y                                 --
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

package body System.C_Time is

   --  Two Duration representations are described in targparm.ads.
   --  Size       Delta       Last = (2**(Size - 1) - 1) * Delta
   --   32    0.02                   42_949_672.94
   --   64    0.000_000_001       9_223_372_036.854_775_807

   --  The overflows listed in comments must raise Constraint_Error.
   pragma Unsuppress (Overflow_Check);

   -----------------
   -- In_Duration --
   -----------------

   function In_Duration (T : timeval) return Boolean is

      --  Mimic To_Timeval (Duration'Last), except that
      --   * this computation happens at compile time
      --   * Sec may be far above MAX_tv_sec
      --   * on 64 bits, Usec is truncated instead of rounded up
      Sec  : constant := (if Duration'Size = 64
                          then (2**63 - 1)  /  1_000_000_000
                          else (2**31 - 1)  /             50);
      Usec : constant := (if Duration'Size = 64
                          then (2**63 - 1) mod 1_000_000_000 / 1_000
                          else (2**31 - 1) mod            50 * 20_000);

      pragma Warnings (Off, "condition is always");
      Dur_Covers_Tv_Sec : constant Boolean := OS_Constants.MAX_tv_sec < Sec;
      pragma Warnings (On, "condition is always");

      --  When Duration'Size = 64 and time_t'Size = 32, the compiler
      --  complains that Sec does not fit in time_t, hence cannot be
      --  compared with T.tv_sec.  But then Dur_Covers_Tv_Sec is True
      --  and the following comparisons are skipped.
      Maybe_Sec : constant := (if Dur_Covers_Tv_Sec then 1 else Sec);
   begin
      return Dur_Covers_Tv_Sec
        or else  T.tv_sec < Maybe_Sec
        or else (T.tv_sec = Maybe_Sec and then T.tv_usec <= Usec);
   end In_Duration;

   -----------------------------
   -- Milliseconds_To_Timeval --
   -----------------------------

   function Milliseconds_To_Timeval (M : Non_Negative_Int) return timeval is
      use Interfaces.C;
      Q : constant int range 0 .. int'Last / 1_000 := M  /  1_000;
      R : constant int range 0 .. 999              := M mod 1_000;
   begin
      return (tv_sec  => time_t (Q),
              tv_usec => 1_000 * usec_t (R));
   end Milliseconds_To_Timeval;

   -----------------------------
   -- Nanoseconds_To_Timespec --
   -----------------------------

   function Nanoseconds_To_Timespec (N : Non_Negative_Int) return timespec is
      use Interfaces.C;
      Q : constant int range 0 .. int'Last / 10**9 := N  /  10**9;
      R : constant int range 0 .. 999_999_999      := N mod 10**9;
   begin
      return (tv_sec  => time_t (Q),
              tv_nsec => nsec_t (R));
   end Nanoseconds_To_Timespec;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : timeval) return Non_Negative_Duration is
      Usec : usec_t range 0 .. 1_009_999;
      Frac : Duration range 0.0 .. 1.0;
   begin
      if Duration'Size = 64 then
         Usec := T.tv_usec;
      else
         --  Fix the rounding (999_999.0 / 1_000_000 = 0.98)
         --  with a bias of half Duration'Small = 10 ms.
         Usec := T.tv_usec + 10_000;
      end if;
      Frac := Duration (Usec) / 1_000_000;

      return Duration (T.tv_sec) + Frac;
      --  Both the conversion and the addition may overflow.
   end To_Duration;

   function To_Duration (T : timespec) return Non_Negative_Duration is
      Frac : Duration range 0.0 .. 1.0;
   begin
      if Duration'Size = 64 then
         Frac := Duration (T.tv_nsec) / 1_000_000_000;
      else
         --  Avoid an overflow (Duration'Last < 999_999_999).
         --  Fix the rounding (999_999_999.0 / 1_000_000_000 = 0.98)
         --  with a bias of half Duration'Small = 10 ms.
         Frac := Duration (T.tv_nsec / 10_000_000 + 1) / 100;
      end if;

      return Duration (T.tv_sec) + Frac;
      --  Both the conversion and the addition may overflow.
   end To_Duration;

   function To_Duration (tv_sec  : Tv_Sec_Long;
                         tv_nsec : Tv_Nsec_Long)
                        return Non_Negative_Duration is
   begin
      return To_Duration (timespec'(tv_sec  => time_t (tv_sec),
                                    tv_nsec => nsec_t (tv_nsec)));
   end To_Duration;

   ------------------------
   -- To_Struct_Timespec --
   ------------------------

   procedure To_Struct_Timespec (D       : Non_Negative_Duration;
                                 tv_sec  : out Tv_Sec_Long;
                                 tv_nsec : out Tv_Nsec_Long) is
      T : constant timespec := To_Timespec (D);
   begin
      tv_sec  := Tv_Sec_Long (T.tv_sec);  --  May overflow Interfaces.C.long.
      tv_nsec := Tv_Nsec_Long (T.tv_nsec);
   end To_Struct_Timespec;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (T : timeval) return timespec is
   begin
      return (tv_sec  => T.tv_sec,
              tv_nsec => 1_000 * nsec_t (T.tv_usec));
   end To_Timespec;

   function To_Timespec (D : Non_Negative_Duration) return timespec is
      --  See To_Timeval.
      Dm1  : constant Duration range -1.0 .. Duration'Last - 1.0 := D - 1.0;
      Sec  : constant time_t range -1 .. time_t'Last := time_t (Dm1);
      Frac : constant Duration range -0.5 .. 0.5 := Dm1 - Duration (Sec);
      Nsec : nsec_t range -500_000_000 .. 500_000_000;
   begin
      if Duration'Size = 64 then
         Nsec := nsec_t (1_000_000_000 * Frac);
      else
         --  Duration'Last < 500_000_000 so there is an overflow issue,
         --  easy to solve because Frac has few significative digits.
         Nsec := 20_000_000 * nsec_t (50 * Frac);
      end if;
      if Nsec < 0 then
         return (tv_sec => Sec,     tv_nsec => Nsec + 1_000_000_000);
      else
         return (tv_sec => Sec + 1, tv_nsec => Nsec);
      end if;
   end To_Timespec;

   -----------------
   -- To_Timeval --
   -----------------

   function To_Timeval (D : Non_Negative_Duration) return timeval is

      --   Most comments also apply to the timespec variant.

      --    Sec := time_t (D);
      --    Usec := usec_t (1_000_000 * (D - Duration (Sec)));
      --  fails when D is
      --    Duration'Last (Sec is rounded up and Duration (Sec) overflows)
      --    0.9           (Sec is rounded up and Usec < 0)

      --    Sec := time_t (D - 0.5);
      --    Usec := usec_t (1_000_000 * (D - Duration (Sec)));
      --  leads to Usec = 1_000_000 when D is
      --    0.0           (Sec is rounded down)
      --    0.999_999_999 (Usec is rounded up) (not an issue with timespec)

      Dm1  : constant Duration range -1.0 .. Duration'Last - 1.0 := D - 1.0;
      --  Converting D - 1 avoids overflows and simplifies roundings.

      Sec  : constant time_t range -1 .. time_t'Last := time_t (Dm1);
      --  The conversion of Dm1 may overflow if time_t has 32 bits.

      Frac : constant Duration range -0.5 .. 0.5 := Dm1 - Duration (Sec);
      --  The conversion back is always possible, Sec <= Duration'Last - 0.5.

      Usec : constant usec_t range -500_000 .. 500_000
        := usec_t (1_000_000 * Frac);
   begin
      --  Add the second substracted at the beginning, in a way fixing
      --  the Usec interval if necessary.
      --  In both cases, tv_sec may exceed MAX_tv_sec.
      if Usec < 0 then
         return (tv_sec => Sec,     tv_usec => Usec + 1_000_000);
      else
         return (tv_sec => Sec + 1, tv_usec => Usec);
      end if;
   end To_Timeval;

end System.C_Time;
