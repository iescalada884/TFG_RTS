--------------------------------------------------------------------------------
--  Persistent_Signals
--------------------------------------------------------------------------------
--  Implementation using MUTEX and CONDITIONAL VARIABLES
--  TODO: errors and exceptions management
--  by Sangorrin <daniel.sangorrin@unican.es>
--------------------------------------------------------------------------------
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Condition_Variables;
with MaRTE.Integer_Types;

generic
   Ceiling : MaRTE.Kernel.Mutexes.Ceiling_Priority;
package Persistent_Signals is

   type Persistent_Signal is limited private;
   type Persistent_Signal_Ref is access all Persistent_Signal;

   Unexpected_Error : exception;

   procedure Init (PS      : in out Persistent_Signal_Ref;
                   Arrived : in Boolean);

   function Signal (PS : in Persistent_Signal_Ref)
                   return MaRTE.Integer_Types.Int;

   function Wait (PS : in Persistent_Signal_Ref)
                 return MaRTE.Integer_Types.Int;

   procedure Finalize (PS : in out Persistent_Signal_Ref);

private

   type Persistent_Signal is record
      Mutex_Ref : MaRTE.Kernel.Mutexes.Mutex_Descriptor;
      Cond_Ref  : MaRTE.Kernel.Condition_Variables.Condition_Descriptor;
      Signal_Arrived : Boolean;
   end record;
   pragma Pack (Persistent_Signal);

end Persistent_Signals;
