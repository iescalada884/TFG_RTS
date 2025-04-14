------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                          p r i o r i t y _ q u e u e s
--
--                                    body
--
-- File 'protected_priority_queues.adb'                         By Sangorrin
--
-- This package provides an priority queue list type that can be used
-- concurrently by several tasks and/or threads because an internal mutex
-- provides mutual exclusion
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2004   Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http:--marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael González Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                       Copyright (C) 2000-2002                     --
--                 Universidad de Cantabria, SPAIN                   --
--                                                                   --
-- Authors: Michael Gonzalez       mgh@unican.es                     --
--          Jose Javier Gutierrez  gutierjj@unican.es                --
--          Jose Carlos Palencia   palencij@unican.es                --
--          Jose Maria Drake       drakej@unican.es                  --
-----------------------------------------------------------------------
--                      PACKAGE BODY PRIORITY_QUEUE
--                      ===========================
--
--  The implementation of the priority queue is a heapform heap
--  [W. AMSBURY, "Data Structures. From Arrays to Priority Queues",
--  Wadsworth, 1985.
--
--  This implementation has the following characteristics:
--
--      Insertion : O(log n)
--      Highest_priority extraction : O(log n)
--      Lowest_priority extraction : O(n)
--      Read highest priority : O(1)
--      Deletion : O(n) (Change is Deletion + Insertion = O(n)).
-----------------------------------------------------------------------
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Condition_Variables;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package body Protected_Priority_Queues is

   package Km renames MaRTE.Kernel.Mutexes;
   package Kcv renames MaRTE.Kernel.Condition_Variables;

   ------------
   -- UpHeap --
   ------------

   procedure Upheap (
         N : in     Positive;
         Q : in out Queue) is

      P    : Positive;
      K    : Positive := N;
      Temp : Cell;

   begin
      while K >= 2 loop
         P:= K/2;
         if Q.Q(K).Pri > Q.Q(P).Pri then
            Temp:=Q.Q(K);
            Q.Q(K):=Q.Q(P);
            Q.Q(P):=Temp;
            K:=P;
         else
            exit;
         end if;
      end loop;
   end Upheap;

   procedure Init (
         Prio : in     Km.Ceiling_Priority;
         Q    : in out Queue) is
      Attr      : aliased Km.Attributes;
      Cond_Attr : aliased Kcv.Attributes;
      Ret       : Int;
   begin
      -- Queue initialization
      Q.Length:=0;
      Q.Q:=null;
      -- Mutex initialization
      Q.Mutex_Ref := new Km.Mutex;
      Ret := Km.Pthread_Mutexattr_Init (Attr'access);
      Ret := Km.Pthread_Mutexattr_Setprotocol (Attr'access,
         Km.Highest_Ceiling_Priority);
      Ret := Km.Pthread_Mutexattr_Setprioceiling (Attr'access, Prio);
      Ret := Km.Pthread_Mutex_Init (Q.Mutex_Ref, Attr'access);
      Ret := Km.Pthread_Mutexattr_Destroy (Attr'access);
      -- Cond initialization
      Cond_Attr := Kcv.Default_Attributes;
      Q.Cond_Ref := new Kcv.Condition;
      Ret := Kcv.Pthread_Cond_Init (Q.Cond_Ref, Cond_Attr'access);
   end Init;

   -----------
   -- Empty --
   -----------

   function Empty (
         Q : Queue)
     return Boolean is
      Is_Empty : Boolean;
      Ret      : Int;
   begin
      Ret := Km.Pthread_Mutex_Lock (Q.Mutex_Ref);
      Is_Empty := (Q.Length=0);
      Ret := Km.Pthread_Mutex_Unlock (Q.Mutex_Ref);
      return Is_Empty;
   end Empty;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (
         E : in     Element;
         P : in     Priority;
         Q : in out Queue) is
      New_Queue : Storing_Space_Ref;
      Size      : Positive;
      Ret       : Int;
   begin
      Ret := Km.Pthread_Mutex_Lock (Q.Mutex_Ref);
      if Q.Q=null then
         Q.Q:=new Storing_Space(1..Initial_Size);
      else
         Size := Q.Q'Length;
         if Q.Length>=Size then
            New_Queue:= new Storing_Space(1..Size*2);
            New_Queue(1..Size):=Q.Q.All;
            Q.Q:=New_Queue;
         end if;
      end if;
      Q.Length:=Q.Length+1;
      Q.Q(Q.Length):=(E,P);
      Upheap(Q.Length,Q);
      Ret := Kcv.Pthread_Cond_Signal (Q.Cond_Ref);
      Ret := Km.Pthread_Mutex_Unlock (Q.Mutex_Ref);
   end Enqueue;


   --------------
   -- DownHeap --
   --------------

   procedure Downheap (
         K : in     Positive;
         N : in     Natural;
         Q : in out Queue) is

      J    : Integer  := 2 * K;
      Half : Positive := K;
      Temp : Cell     := Q.Q (K);

   begin
      while J<=N loop
         if (J<N) and then (Q.Q(J+1).Pri>Q.Q(J).Pri) then
            J:=J+1;
         end if;
         if not (Q.Q(J).Pri > Temp.Pri) then
            Q.Q(Half):=Temp;
            return;
         end if;
         Q.Q(Half):=Q.Q(J);
         Half:=J;
         J:=2*J;
      end loop;
      Q.Q(Half):=Temp;
   end Downheap;


   -------------
   -- Dequeue --
   -------------

   procedure Dequeue (
         E :    out Element;
         P :    out Priority;
         Q : in out Queue) is
      Ret : Int;
   begin
      Ret := Km.Pthread_Mutex_Lock (Q.Mutex_Ref);
      while (Q.Length=0) loop
         Ret := Kcv.Pthread_Cond_Wait (Q.Cond_Ref, Q.Mutex_Ref);
      end loop;
      E:=Q.Q(1).E;
      P:=Q.Q(1).Pri;
      Q.Q(1):=Q.Q(Q.Length);
      Q.Length:=Q.Length-1;
      Downheap(1,Q.Length,Q);
      Ret := Km.Pthread_Mutex_Unlock (Q.Mutex_Ref);
   end Dequeue;

end Protected_Priority_Queues;
