with Ada.Text_Io; -- debug
with Unchecked_Deallocation;
with MaRTE.Integer_Types;

package body Persistent_Signals is

   package Km renames MaRTE.Kernel.Mutexes;
   package Kcv renames MaRTE.Kernel.Condition_Variables;
   use type MaRTE.Integer_Types.Int;

   procedure Check_NZ (Result : MaRTE.Integer_Types.Int) is
      use type MaRTE.Integer_Types.Int;
   begin
      if Result /= 0 then
         raise Unexpected_Error;
      end if;
   end Check_NZ;
   pragma Inline (Check_NZ);

   ----------
   -- Init --
   ----------

   procedure Init (PS      : in out Persistent_Signal_Ref;
                   Arrived : in Boolean) is
      Attr      : aliased Km.Attributes;
      Cond_Attr : aliased Kcv.Attributes;
   begin
      if PS = null then
         PS := new Persistent_Signal;
         -- Mutex initialization
         PS.Mutex_Ref := new Km.Mutex;
         Check_NZ (Km.Pthread_Mutexattr_Init (Attr'access));
         Check_NZ (Km.Pthread_Mutexattr_Setprotocol
            (Attr'access, Km.Highest_Ceiling_Priority));
         Check_NZ (Km.Pthread_Mutexattr_Setprioceiling (Attr'access, Ceiling));
         Check_NZ (Km.Pthread_Mutex_Init (PS.Mutex_Ref, Attr'access));
         Check_NZ (Km.Pthread_Mutexattr_Destroy (Attr'access));
         -- Cond initialization
         Cond_Attr := Kcv.Default_Attributes;
         PS.Cond_Ref := new Kcv.Condition;
         Check_NZ (Kcv.Pthread_Cond_Init (PS.Cond_Ref, Cond_Attr'access));
         --  Set the rest of variables
         PS.Signal_Arrived := Arrived;
      end if;
   end Init;

   ------------
   -- Signal --
   ------------

   function Signal (PS : in Persistent_Signal_Ref)
                   return MaRTE.Integer_Types.Int is
   begin
      if PS /= null then
         Check_NZ (Km.Pthread_Mutex_Lock (PS.Mutex_Ref));
            PS.Signal_Arrived := True;
            Check_NZ (Kcv.Pthread_Cond_Signal (PS.Cond_Ref));
         Check_NZ (Km.Pthread_Mutex_Unlock (PS.Mutex_Ref));
      end if;
      return 0;
   exception
      when others =>
         Check_NZ (Km.Pthread_Mutex_Unlock (PS.Mutex_Ref));
         return -1;
   end Signal;

   ----------
   -- Wait --
   ----------

   function Wait (PS : in Persistent_Signal_Ref)
                 return MaRTE.Integer_Types.Int is
   begin
      if PS /= null then
         Check_NZ (Km.Pthread_Mutex_Lock (PS.Mutex_Ref));
            while not PS.Signal_Arrived loop
               Check_NZ (Kcv.Pthread_Cond_Wait (PS.Cond_Ref, PS.Mutex_Ref));
            end loop;
            PS.Signal_Arrived := False;
         Check_NZ (Km.Pthread_Mutex_Unlock (PS.Mutex_Ref));
      end if;
      return 0;
   exception
      when others =>
         Check_NZ (Km.Pthread_Mutex_Unlock (PS.Mutex_Ref));
         return -1;
   end Wait;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (PS : in out Persistent_Signal_Ref) is
      procedure Free is new Unchecked_Deallocation
         (Persistent_Signal, Persistent_Signal_Ref);
   begin
      if PS /= null then
         Check_NZ (Km.Pthread_Mutex_Destroy (PS.Mutex_Ref));
         Check_NZ (Kcv.Pthread_Cond_Destroy (PS.Cond_Ref));
         Free (PS);
      end if;
   end Finalize;

end Persistent_Signals;
