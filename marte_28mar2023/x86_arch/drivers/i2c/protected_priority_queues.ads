------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                          p r i o r i t y _ q u e u e s
--
--                                    spec
--
-- File 'protected_priority_queues.ads'                        By Sangorrin
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
--                            PACKAGE PRIORITY_QUEUE
--                            ======================
--
-- This package defines a priority queue type and the operations that
-- can be performed on this type:
--
--     Empty queue functions.
--     Enqueue : insert in the queue, in priority order.
--     Dequeue : extract the highest priority element from the queue.
--               No particular order is imposed on equal priority
--               elements.
--
-- It is written as a generic package, in which the generic parameters
-- are:
--
--     Size : Maximum number of elements in the queue
--     Element : type of the queue elements
--     Priority : type of the priority used for ordering the queue
--                elements
--     ">" : Function used to order the priorities.
-------------------------------------------------------------------------------
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Condition_Variables;
generic
   type Element is private;
   type Priority is private;
   with function ">" (
         Left,
         Right : in     Priority)
     return Boolean;

package Protected_Priority_Queues is

   type Queue is private;

   procedure Init (
         Prio : in     MaRTE.Kernel.Mutexes.Ceiling_Priority;
         Q    : in out Queue);

   function Empty (
         Q : Queue)
     return Boolean;

   procedure Enqueue (
         E : in     Element;
         P : in     Priority;
         Q : in out Queue);


   procedure Dequeue (
         E :    out Element;
         P :    out Priority;
         Q : in out Queue);
   -- Blocking procedure (Check Empty if you want a Non-blocking behaviour)

private

   Initial_Size : constant Integer:=1024;

   type Cell is
      record
         E   : Element;
         Pri : Priority;
      end record;

   type Storing_Space is array (Positive range <>) of Cell;

   type Storing_Space_Ref is access Storing_Space;

   type Queue is
      record
         Length    : Natural                                         :=
           0;
         Q         : Storing_Space_Ref;
         Mutex_Ref : MaRTE.Kernel.Mutexes.Mutex_Descriptor;
         Cond_Ref  : MaRTE.Kernel.Condition_Variables.Condition_Descriptor;
      end record;

end Protected_Priority_Queues;
