.text

# Current Processor Status Register (CPSR) does not exists in
# the processor used in this architecture
# .globl returnCPSR
# returnCPSR:
# 	mrs r0,cpsr
# 	mov pc, lr

.globl returnSP
returnSP:
	mov r0,sp
	mov pc, lr

.globl setSP
setSP:
	mov sp,r0
	mov pc, lr


# Not used in this architecture
# void context_switch(uint32 *old_sp, uint32 *new_sp)
#                       r0<-old_sp      r1<-new_sp
#
#            |    |                         /->| lr |
#            |    |                         |  |cprs|
#            |    |                         |  | r0 |
#            |    |                         |  | r1 |     
#            |    |          ----\          |  | ...|
#            |    |          ----/          |  | r12|
#         /->|XXXX|                         |  |XXXX|
# old_sp  |  |YYYY|                 old_sp  |  |YYYY|
#  |top0|-/  |ZZZZ|                  | sp |-/  |ZZZZ|
#
# .globl context_switch
# context_switch:
	#save state
# 	push {r1-r12}
# 	push {r0}
# 	mrs r2,cpsr
# 	push {r2}
# 	push {lr}
	
	#save stack pointer
# 	str sp,[r0]	

	#prepare for next step	
# 	mov r0,r1

	#continue on change_to_context

#void change_to_context(uint32 *new_sp)
#                         r0<-new_sp
#
#         /->| lr |                         |    |
#         |  |cprs|                         |    |
#         |  | r0 |                         |    |
#         |  | r1 |                         |    |    
#         |  | ...|          ----\          |    |
#         |  | r12|          ----/          |    |
#         |  |XXXX|                   sp -> |XXXX|
# new_sp  |  |YYYY|                         |YYYY|
#  | sp |-/  |ZZZZ|                         |ZZZZ|
#
# .globl change_to_context
# change_to_context:
	#switch to the new stack
# 	ldr sp,[r0]

# 	pop {lr}

	#restore state	
# 	pop {r0}		
# 	msr cpsr,r0

# 	pop {r0}
# 	pop {r1-r12}
	#jump to the return address	
# 	mov pc, lr

#void change_lr(uint32 *task_sp,uint *new_lr)
.globl change_lr
change_lr:
	str r1,[r0]
	mov pc,lr
