with MaRTE.Kernel.Mutexes;
pragma Elaborate_All (MaRTE.Kernel.Mutexes);
with MaRTE.Kernel.Condition_Variables;
pragma Elaborate_All (MaRTE.Kernel.Condition_Variables);
with Queues;
with Ada.Text_IO;

package body Commands_Queue is

   use type MaRTE.Integer_Types.Int;  --  Mario

   package Km renames MaRTE.Kernel.Mutexes;
   package Kcv renames MaRTE.Kernel.Condition_Variables;

   procedure Check_NZ (Result : MaRTE.Integer_Types.Int) is  --  Mario
   --  procedure Check_NZ (Result : Integer) is
   begin
      if Result /= 0 then
         raise Unexpected_Error;
      end if;
   end Check_NZ;

   procedure DEBUG (Str     : in String;
                    Enabled : in Boolean) is
   begin
      if Enabled then
         Ada.Text_IO.Put_Line ("[DEBUG]: " & Str);
      end if;
   end DEBUG;
   pragma Inline (DEBUG);

   ------------------------
   -- The Commands Queue --
   ------------------------

   type Command_Status_List is array (Command_Id) of Command_Status;
   package Command_Lists is new Queues (Size+1, Command);

   type Commands_Queue_Type is record
      Status_List  : Command_Status_List;
      Command_List : Command_Lists.Queue;
      Mutex_Ref    : Km.Mutex_Descriptor;
      Cond_Ref     : Kcv.Condition_Descriptor;
   end record;

   COMMANDS : Commands_Queue_Type;

   ----------
   -- Init --
   ----------

   procedure Init is
      Attr      : aliased Km.Attributes;
      Cond_Attr : aliased Kcv.Attributes;
   begin
      --  Lists initialization
      COMMANDS.Status_List := (others => Not_In_Use);
      Command_Lists.Init (COMMANDS.Command_List);
      --  Mutex initialization
      COMMANDS.Mutex_Ref := new Km.Mutex;
      Check_NZ (Km.Pthread_Mutexattr_Init (Attr'access));
      Check_NZ (Km.Pthread_Mutexattr_Setprotocol
         (Attr'access, Km.Highest_Ceiling_Priority));
      Check_NZ (Km.Pthread_Mutexattr_Setprioceiling (Attr'access, Prio));
      Check_NZ (Km.Pthread_Mutex_Init (COMMANDS.Mutex_Ref, Attr'access));
      Check_NZ (Km.Pthread_Mutexattr_Destroy (Attr'access));
      --  Cond initialization
      Cond_Attr := Kcv.Default_Attributes;
      COMMANDS.Cond_Ref := new Kcv.Condition;
      Check_NZ (Kcv.Pthread_Cond_Init (COMMANDS.Cond_Ref, Cond_Attr'access));
   end Init;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
      (G_Cmd : in Generic_Command;
       Cmd   : out Command)
   is
      Full : Boolean := True;
   begin
      Cmd.Cmd := G_Cmd;
      Check_NZ (Km.Pthread_Mutex_Lock (COMMANDS.Mutex_Ref));
         --  Look for a free Command
         for I in COMMANDS.Status_List'Range loop
            DEBUG ("Status ("&Integer'Image(I)&"): "&Command_Status'Image
               (COMMANDS.Status_List (I)), Enable_DEBUG'First);
            if COMMANDS.Status_List (I) = Not_In_Use then
               Cmd.Id := I;
               Full := False;
               exit;
            end if;
         end loop;
         --  If all commands are in use raise exception
         if Full then
            Check_NZ (Km.Pthread_Mutex_Unlock (COMMANDS.Mutex_Ref));
            raise Commands_Queue_Full;
         end if;
         --  Set the status to 'Enqueued' and Enqueue it
         COMMANDS.Status_List (Cmd.Id) := Enqueued;
         Command_Lists.Enqueue (Cmd, COMMANDS.Command_List);
      Check_NZ (Kcv.Pthread_Cond_Signal (COMMANDS.Cond_Ref));
      Check_NZ (Km.Pthread_Mutex_Unlock (COMMANDS.Mutex_Ref));
   end Enqueue;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Cmd : in Command) return Command_Status is
      Stat : Command_Status;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (COMMANDS.Mutex_Ref));
         Stat := COMMANDS.Status_List (Cmd.Id);
      Check_NZ (Km.Pthread_Mutex_Unlock (COMMANDS.Mutex_Ref));
      return Stat;
   end Get_Status;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
      (Cmd  : in Command;
       Stat : in Command_Status) is
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (COMMANDS.Mutex_Ref));
         COMMANDS.Status_List (Cmd.Id) := Stat;
      Check_NZ (Kcv.Pthread_Cond_Broadcast (COMMANDS.Cond_Ref));
      Check_NZ (Km.Pthread_Mutex_Unlock (COMMANDS.Mutex_Ref));
   end Set_Status;

   ----------
   -- Wait --
   ----------

   function Wait (Cmd : in Command) return Command_Status
   is
      Stat : Command_Status;
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (COMMANDS.Mutex_Ref));
         while (COMMANDS.Status_List (Cmd.Id) = Enqueued) loop
            Check_NZ (Kcv.Pthread_Cond_Wait
               (COMMANDS.Cond_Ref, COMMANDS.Mutex_Ref));
         end loop;
         Stat := COMMANDS.Status_List (Cmd.Id);
      Check_NZ (Km.Pthread_Mutex_Unlock (COMMANDS.Mutex_Ref));
      return Stat;
   end Wait;

   -------------
   -- Dequeue --
   -------------

   procedure Dequeue
      (Cmd   : out Command;
       G_Cmd : out Generic_Command) is
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (COMMANDS.Mutex_Ref));
         while (Command_Lists.Empty (COMMANDS.Command_List)) loop
            Check_NZ (Kcv.Pthread_Cond_Wait
               (COMMANDS.Cond_Ref,COMMANDS.Mutex_Ref));
         end loop;
         Command_Lists.Dequeue (Cmd, COMMANDS.Command_List);
         G_Cmd := Cmd.Cmd;
      Check_NZ (Km.Pthread_Mutex_Unlock (COMMANDS.Mutex_Ref));
   end Dequeue;

begin
   Init;
end Commands_Queue;
