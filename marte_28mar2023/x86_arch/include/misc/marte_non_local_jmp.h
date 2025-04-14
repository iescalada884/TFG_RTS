/*----------------------------------------------------------------------------
 *------------------------      M a R T E   O S      -------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                   'm a r t e _ n o n _ l o c a l _ j m p'
 *
 *                                    H
 *
 * File 'marte_non_local_jmp.h'                                       By MAR.
 *
 * Non-local jumps for preempted tasks.
 * Functionality implemented in 'non_local_jump.ad[ab]'.
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *  MaRTE OS web page: http://marte.unican.es
 *  Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                     Michael Gonzalez Harbour      mgh@unican.es
 *
 * MaRTE OS  is free software; you can  redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as published by the
 * Free Software Foundation;  either  version 2, or (at  your option) any
 * later version.
 *
 * MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 * WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 * MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 * General Public License for more details.
 *
 * You should have received  a  copy of  the  GNU General Public  License
 * distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 * Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 * 02111-1307, USA.
 *
 * As a  special exception, if you  link this  unit  with other  files to
 * produce an   executable,   this unit  does  not  by  itself cause  the
 * resulting executable to be covered by the  GNU General Public License.
 * This exception does  not however invalidate  any other reasons why the
 * executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/

#ifndef _MARTE_NON_LOCAL_JMP_H_
#define _MARTE_NON_LOCAL_JMP_H_

#include <sys/cpp_macros.h>
#include <sys/types.h>

CPP_BEGIN_DECLS

//typedef struct { int b[108/4]; } marte_nonlocaljmp_context_t; // GPL2005
typedef struct { int b[156/4]; } marte_nonlocaljmp_context_t; // GPL2007

/*-----------------------------------
 *-- marte_nonlocaljmp_savecontext --
 *-----------------------------------*/
/*
 *  Context stores the information required to modify the stack of a
 *  preempted task with 'restorecontext'.
 *
 *  This function stores in 'context' the registers and its stack frame. This
 *  information can be used for 'restorecontext' to change the stack of the
 *  task (when it is preempted) so that when it is scheduled again it returns
 *  to the end of this function
 */
void marte_nonlocaljmp_savecontext
   (marte_nonlocaljmp_context_t * context);

/*----------------------------------
 *--  marte_nonlocaljmp_afterjmp  --
 *----------------------------------*/
/*
 * To be invoked after 'Save_Context'. If invoked after a direct invocation
 * to 'Save_Context', 'After_Jump' shall return 0. If invoked after
 * returning from 'Save_Context' due to a call to 'Restore_Context',
 * 'After_Jump' shall return 1.
 */
 int marte_nonlocaljmp_afterjmp(const marte_nonlocaljmp_context_t * context);

/*--------------------------------------
 *-- marte_nonlocaljmp_restorecontext --
 *--------------------------------------*/
/*
 *  This procedure changes the return context of a preempted task.
 *
 *                    |        |               |       |
 *  TCB_Ac.Stack_Ptr->|  Regs  |               |       |
 *                    |        |               |       |
 *                       ...                      ...
 *                    |        |               |  ret  |<-TCB_Ac.Stack_Ptr
 *                    |        |               |  ebp  |
 *                    |        |<-Context.Esp->|  SC*  |
 *                    |        |               |  SC*  |
 *                    |        |               |  SC*  |
 *                    |        |               |       |
 *                      before                   after
 *  SC*: stack frame of 'Save_Context'.
 *
 *  The next time the task is scheduled will execute the final part of
 *  Save_Context.
 *
 *  The full process is the following: 'Save_Context' stores the registers
 *  and its stack frame in 'Jmp_Context'. 'Restore_Context' restores that
 *  context placing the stored frame in the same position of the stack it was
 *  originally. Over this frame it is placed the value of the ebp register
 *  and the address of label "1:" in 'Save_Context'.
 *  When the task is scheduled again, the first instruction it executes is
 *  "ret" in 'Processor_Registers.Context_Switch' and then the address of
 *  label "1:" in 'Save_Context' is popped from the stack. Once in
 *  'Save_Context', the ebp register is also popped and, with this
 *  instruction, the registers and stack are in the same situation it was the
 *  time that the contexts was stored, and then the final part of
 *  'Save_Context' can be executed successfully.
 */
void marte_nonlocaljmp_restorecontext
  (pthread_t th,
   marte_nonlocaljmp_context_t * context);

CPP_END_DECLS

#endif // _MARTE_NON_LOCAL_JMP_H_

