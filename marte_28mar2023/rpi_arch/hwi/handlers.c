static unsigned int *Enable_IRQs1 = (unsigned int*)0x2000B210;
static unsigned int *Enable_IRQs2 = (unsigned int*)0x2000B214;
static unsigned int *Enable_IRQsB = (unsigned int*)0x2000B218;

static unsigned int *Pending_B = (unsigned int*)0x2000B200;
static unsigned int *Pending_1 = (unsigned int*)0x2000B204;
static unsigned int *Pending_2 = (unsigned int*)0x2000B208;

static unsigned int *armTimerLoad = (unsigned int *) 0x2000b400;
static unsigned int *armTimerValue = (unsigned int *) 0x2000b404;
static unsigned int *armTimerControl = (unsigned int *) 0x2000b408;
static unsigned int *armTimerIRQClear = (unsigned int *) 0x2000b40c;

static unsigned int *Timer_Compare0 = (unsigned int *)0x2000300C;
static unsigned int *Timer_Compare1 = (unsigned int *)0x20003010;
static unsigned int *Timer_Compare2 = (unsigned int *)0x20003014;
static unsigned int *Timer_Compare3 = (unsigned int *)0x20003018;

static unsigned int *Timer_CS = (unsigned int *)0x20003000;
static unsigned int *TimerStamp_Low = (unsigned int *)0x20003004;

extern int irqStatus();
extern void irqDisable();
extern void irqEnable();

extern direct_write_on_stdout(char *, int); // marte-direct_io

typedef struct TrapS {
	int HW_Int;
} TrapS;

TrapS trap;

void (*Handlers_B[32])(TrapS*);
void (*Handlers_1[32])(TrapS*);
void (*Handlers_2[32])(TrapS*);

void unexpected_interrupt(){
  direct_write_on_stdout("\nunexpected_interrupt\n",22);
  *Timer_CS=0x00000002;
}

unsigned char base_irq_nest = 0x80;
// base_irq_nest is used to know when interrupts are called
// and it is also used in order to call scheduler when it is 0x00

extern void do_scheduling (void);
// Perform contest switch when necessary. Defined in 'kernel/k-scheduler'

extern void scheduler__start_interrupt();
extern void scheduler__end_interrupt();
// Defined in 'kernel/k-scheduler'

/*
void irq_dbg0() {
  direct_write_on_stdout("(", 1);
}

void irq_dbg1() {
  direct_write_on_stdout(")", 1);
   }
*/

void irq_interrupt(){
	int i;
        int comp=1;

  scheduler__start_interrupt();

  /* An handler is been executed */
  base_irq_nest ++;
  //direct_write_on_stdout("at irq_interrupt\n", 17);
	/*hdmi_console_putString("irq_interrupt",13);
	hdmi_console_putInt(*Pending_B);
	hdmi_console_putString("irq_interrupt",13);
	hdmi_console_putInt(*Pending_1);
	hdmi_console_putString("irq_interrupt",13);
	hdmi_console_putInt(*Enable_IRQs1);
	hdmi_console_putString("irq_interrupt",13);
	hdmi_console_putInt(*Enable_IRQsB & 0x100);*/
	//hdmi_console_putInt(Timer_CS);
	//The pending interrupts are masked with the enabled ones
	if (*Enable_IRQsB & *Pending_B){
//		hdmi_console_putString("basic_interrupt",15);
		for (i=0;i<8;i++){
			if (*Enable_IRQsB & *Pending_B & comp) {
				trap.HW_Int = i + 64;
				(Handlers_B[i])(&trap);
			}
			comp = comp <<1;
		}
	}
	//If the pending register 1 has any bit set
	if (*Pending_B & 0x100){
		if (*Enable_IRQs1 & *Pending_1){
//			hdmi_console_putString("pending1_interrupt",18);
			comp=1;
			for (i=0;i<32;i++){
				if (*Enable_IRQs1 & *Pending_1 & comp) {
					trap.HW_Int = i;
					(Handlers_1[i])(&trap);
					if (i==1){
					  //TIMER_1 handled
					  *Timer_CS=0x00000002;
					}else{
					  if (i==3){
					    //TIMER_3 handled
					    *Timer_CS=0x00000008;
					  }
					}
				}
				comp = comp <<1;
			}
		}
	}
	if (*Pending_B & 0x200){
		if (*Enable_IRQs2 & *Pending_2){
//			hdmi_console_putString("pending2_interrupt",18);
			comp=1;
			for (i=0;i<32;i++){
				if (*Enable_IRQs2 & *Pending_2 & comp) {
					trap.HW_Int = i + 32;
					(Handlers_2[i])(&trap);
				}
				comp = comp <<1;
			}
		}
	}

  /* The interruption has finished */
  base_irq_nest --;

  /* Does we call MaRTE scheduler? It may be */
  if (base_irq_nest == 0) {
    //base_irq_nest = 0x80;  done en do_scheduling
    (*do_scheduling)();
    //direct_write_on_stdout("--2--", 5);
  } else {
    scheduler__end_interrupt();
  }
}

void unexpected(){
//	hdmi_console_putString("unexpected_interrupt",20);
	*Timer_CS=0x00000002;
}

void timer_interrupt(){
//	hdmi_console_putString("timer_interrupt",15);
	*Timer_CS=0x00000002;
}

//type is 0 for basic
void put_handler(int type,int n,void *function){
	switch (type){
		case 0:
		Handlers_1[n] = function;
		break;
		case 1:
		Handlers_2[n] = function;
		break;
		case 2:
		Handlers_B[n] = function;
		break;
		default:
//		hdmi_console_putString("wrong_type",10);
		break;
	}
}

void c_divided_by_0(){
	direct_write_on_stdout("divided_by_0",12);
}

void init_interrupts(){
	unsigned int aux;
	aux = irqStatus();
//	hdmi_console_putInt(aux);
//	hdmi_console_putString("init_interrupts",15);
//	*Enable_IRQs1 = 0x00000002;
	//Handlers_1[1] = timer_interrupt;
	//	put_handler(1,1,timer_interrupt);
	//	irqEnable();
	//	*Timer_Compare1=*TimerStamp_Low+0x00000100;
	//	aux = irqStatus();
//		hdmi_console_putInt(aux);
}

void undefined_interrupt(){
  direct_write_on_stdout("\nundefined_interrupt\n",22);
}

void swi_interrupt(){
  direct_write_on_stdout("\nswi_interrupt\n", 15);
}

void prefetch_interrupt(){
  direct_write_on_stdout("\nprefecth_interrupt\n", 20);
}

void data_interrupt(){
	direct_write_on_stdout("data_interrupt",14);
}

void unused_interrupt(){
	direct_write_on_stdout("unused_interrupt",16);
}

void fiq_interrupt(){
	direct_write_on_stdout("fiq_interrupt",13);
}
