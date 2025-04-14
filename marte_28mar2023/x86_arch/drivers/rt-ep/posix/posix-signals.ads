with MaRTE.POSIX_Constants;
with MaRTE.Integer_Types; use MaRTE.Integer_Types; -- for Int

package POSIX.Signals is

   type Signal is new Int range 0 .. MaRTE.POSIX_Constants.SIGRTMAX;
   for Signal'Size use Int'Size;
   subtype Signal_Set_Range is Signal range 1 .. Signal'Last;

   Signal_Null : constant Signal := MaRTE.POSIX_Constants.SIGNULL;
   Signal_Kill : constant Signal := MaRTE.POSIX_Constants.SIGKILL;

   subtype Realtime_Signal is Signal range
     MaRTE.POSIX_Constants.SIGRTMIN .. MaRTE.POSIX_Constants.SIGRTMAX;

   type Signal_Set is private;
   type Signal_Set_Ac is access all Signal_Set;
   type Signal_Set_Ac_Const is access constant Signal_Set;
   Empty_Set : constant Signal_Set;

   --  Cause of Signals
   type Cause_Of_Signal is new Int range 0 .. 2;
   for Cause_Of_Signal'Size use Int'Size;
   SI_USER  : constant Cause_Of_Signal := 0;
   SI_QUEUE : constant Cause_Of_Signal := 1;
   SI_TIMER : constant Cause_Of_Signal := 2;

   type Siginfo_T is record
      Signo : Signal;
      Code  : Cause_Of_Signal;
      Value : Int;
   end record;
   pragma Convention (C, Siginfo_T);
   subtype Signal_Info is Siginfo_T;

   type Notification is range 0 .. 1;
   for Notification'Size use Int'Size;
   NO_NOTIFICATION : constant Notification := 0;
   SIGNAL_NOTIFICATION : constant Notification := 1;

   subtype Sigval is Int;

   type Signal_Event is record
      Event_Notification : Notification := NO_NOTIFICATION;
      Event_Signal       : Signal       := Signal_Kill;
      Event_Sigval       : Sigval       := 0;
   end record;
   type Signal_Event_Ac_Const is access constant Signal_Event;

   procedure Add_Signal
     (Set : in out Signal_Set;
      Sig : in Signal);

   procedure Delete_All_Signals (Set : in out Signal_Set);

   procedure Set_Signal
     (Event : in out Signal_Event;
      Sig   : in Signal);

   procedure Set_Notification (Event  : in out Signal_Event;
                               Notify : in     Notification);

   function Get_Signal (Info : Signal_Info) return Signal;

   function Await_Signal (Set : Signal_Set) return Signal_Info;

private
   type Signal_Set is array (Signal_Set_Range) of Boolean;
   Empty_Set : constant Signal_Set := (others => False);
end POSIX.Signals;
