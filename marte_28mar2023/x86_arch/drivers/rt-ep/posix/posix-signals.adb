package body POSIX.Signals is

   procedure CHECK_ERROR (Value : Int);
   pragma Inline (CHECK_ERROR);

   procedure CHECK_ERROR (Value : Int) is
   begin
      null; --  Just ignore it for the moment
   end CHECK_ERROR;

   ----------------
   -- Add_Signal --
   ----------------

   function Sig_Add_Set (Set : access Signal_Set; Sig : in Signal) return Int;
   pragma Import (C, Sig_Add_Set, "sigaddset");

   procedure Add_Signal (Set : in out Signal_Set; Sig : Signal) is
      Tmp_Set : aliased Signal_Set := Set;
   begin
      if Sig /= Signal_Null then
         CHECK_ERROR (Sig_Add_Set (Tmp_Set'Access, Sig));
      end if;
      --  Signal_Null (i.e., zero) is implicitly a member of every set.
   end Add_Signal;

   ------------------------
   -- Delete_All_Signals --
   ------------------------

   function Sig_Empty_Set (Set : access Signal_Set) return Int;
   pragma Import (C, Sig_Empty_Set, "sigemptyset");

   procedure Delete_All_Signals (Set : in out Signal_Set) is
      Tmp_Set : aliased Signal_Set := Set;
   begin
      CHECK_ERROR (Sig_Empty_Set (Tmp_Set'Access));
   end Delete_All_Signals;

   ------------------
   --  Set_Signal  --
   ------------------

   procedure Set_Signal
     (Event : in out Signal_Event;
      Sig   : in Signal) is
   begin
      Event.Event_Signal := Sig;
   end Set_Signal;

   ------------------------
   --  Set_Notification  --
   ------------------------

   procedure Set_Notification
     (Event  : in out Signal_Event;
      Notify : in Notification) is
   begin
      Event.Event_Notification := Notify;
   end Set_Notification;

   ------------------
   --  Get_Signal  --
   ------------------

   function Get_Signal (Info : Signal_Info) return Signal is
   begin
      return Info.Signo;
   end Get_Signal;

   --------------------
   --  Await_Signal  --
   --------------------
   function Sig_Wait_Info (Set    : in Signal_Set_Ac_Const;
                           Siginf : access Siginfo_T) return Int;
   pragma Import (C, Sig_Wait_Info, "sigwaitinfo");

   function Await_Signal (Set : Signal_Set) return Signal_Info is
      Info : aliased Siginfo_T;
      Tmp_Set : aliased Signal_Set := Set;
   begin
      --  Check_Awaitable??
      CHECK_ERROR (Sig_Wait_Info (Tmp_Set'Unchecked_Access, Info'Access));
      return Signal_Info (Info);
   end Await_Signal;

end POSIX.Signals;
