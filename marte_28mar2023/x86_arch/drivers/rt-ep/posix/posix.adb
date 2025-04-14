with Unchecked_Conversion;

package body POSIX is

   -----------------------
   --  To_POSIX_String  --
   -----------------------
   type String_Ptr is access all String;
   type POSIX_String_Ptr is access all POSIX_String;

   function sptr_to_psptr is new Unchecked_Conversion
      (String_Ptr, POSIX_String_Ptr);

   function To_POSIX_String (Str : String)
      return POSIX_String is
   begin
      return sptr_to_psptr (Str'Unrestricted_Access).all;
   end To_POSIX_String;

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec
     (S  : Seconds;
      NS : Nanoseconds) return Timespec is
   begin
      return Timespec'(Tv_Sec  => time_t (S),
                       Tv_Nsec => Long_Integer (NS));
   end To_Timespec;

   -------------------
   --  To_Timespec  --
   -------------------

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;
   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);
      --  If F has negative value due to a round-up, adjust for positive F
      --  value.
      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;
      return timespec' (tv_sec => S,
        tv_nsec => Long_Integer (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -------------------
   --  To_Duration  --
   -------------------

   function To_Duration (Time : Timespec)  return Duration is
   begin
      return Duration (Time.tv_sec) + Duration (Time.tv_nsec) / 10#1#E9;
   end To_Duration;

   -----------
   --  "+"  --
   -----------

   function "+" (Left, Right : Timespec) return Timespec is
   begin
      return (0,0);
   end "+";

    -----------
    --  "-"  --
    -----------

    function "-" (Left, Right : Timespec) return Timespec is
    begin
       return (0,0);
    end "-";

    -----------
    --  "<"  --
    -----------

    function "<" (Left, Right : Timespec) return Boolean is
    begin
       return False;
    end "<";

   -----------
   --  ">"  --
   -----------

   function ">" (Left, Right : Timespec) return Boolean is
   begin
      return False;
   end ">";

end POSIX;
