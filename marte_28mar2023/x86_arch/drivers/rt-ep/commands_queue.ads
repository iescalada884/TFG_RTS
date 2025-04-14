with MaRTE.Integer_Types;

generic
   Size : Positive;
   type Generic_Command is private;
   Prio : MaRTE.Integer_Types.Int;

package Commands_Queue is

   type Command_Status is (Not_In_Use, Enqueued, Workdone, Error);
   type Command is private;

   Unexpected_Error : exception;
   Commands_Queue_Full : exception;

   subtype Enable_DEBUG is Boolean range False .. False;

   -------------
   -- Enqueue --
   -------------
   --  The commands queue has a fixed size of 'Size'. When enqueing a command
   --  a free element of this queue will be used. If the Queue is full this
   --  operation will raise Commands_Queue_Full. A command is freed by setting
   --  its status to 'Not_In_Use'

   procedure Enqueue
      (G_Cmd : in Generic_Command;
       Cmd   : out Command);

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Cmd : in Command) return Command_Status;

   ----------------
   -- Set_Status --
   ----------------
   --  A command is freed by setting its status to 'Not_In_Use'

   procedure Set_Status
      (Cmd  : in Command;
       Stat : in Command_Status);

   ----------
   -- Wait --
   ----------

   function Wait (Cmd : in Command) return Command_Status;

   -------------
   -- Dequeue --
   -------------

   procedure Dequeue
      (Cmd   : out Command;
       G_Cmd : out Generic_Command);

private

   subtype Command_Id is Positive range 1 .. Size;

   type Command is record
      Cmd : Generic_Command;
      Id  : Command_Id;
   end record;

end Commands_Queue;
