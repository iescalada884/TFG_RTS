.text
#Enable interrupts
.globl irqEnable
irqEnable:
        cpsie i
    mov pc, lr

#Disable interrupts
.globl irqDisable
irqDisable:
        cpsid i
    mov pc, lr

#Get interrupt Enabled/Disabled
.globl irqStatus
irqStatus:
        mrs r0,cpsr
        and r0,r0,$0x80
        lsr r0,r0,$7
        mov pc, lr
