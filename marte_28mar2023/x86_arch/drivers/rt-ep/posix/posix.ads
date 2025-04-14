with Option_Sets;
with MaRTE.Integer_Types; use MaRTE.Integer_Types; -- for Int
package POSIX is

   --  Strings
   type POSIX_Character is new Standard.Character;
   type POSIX_String is array (Positive range <>) of aliased POSIX_Character;
   subtype Pathname is POSIX_String;

   function To_POSIX_String (Str : String) return POSIX_String;

   --  Time
   type Seconds is new Integer;
   type Minutes is new Integer;
   type Nanoseconds_Base is new Integer;
   subtype Nanoseconds   is Nanoseconds_Base range 0 .. (10**9) - 1;
   type Timespec is Private;

   function To_Timespec
     (S  : Seconds;
      NS : Nanoseconds) return Timespec;

   function "+" (Left, Right : Timespec) return Timespec;
   function "-" (Left, Right : Timespec) return Timespec;
   function ">" (Left, Right  : Timespec) return Boolean;
   function "<" (Left, Right  : Timespec) return Boolean;

   function To_Duration (Time : Timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return Timespec;
   pragma Inline (To_Timespec);

   subtype Option_Set is Option_Sets.Option_Set;

private
   type time_t is new Integer;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : Long_Integer;
   end record;
   pragma Convention (C, timespec);
   pragma Pack (timespec);

end POSIX;