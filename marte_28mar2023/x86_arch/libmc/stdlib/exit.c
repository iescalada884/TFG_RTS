/* MaRTE OS */


#include <unistd.h>	/* _exit (coded in 'x86/boot/_exit.c') */

extern void marte__hal__disable_interrupts ();

void _run_atexits(void);

void exit(int code)
{
  marte__hal__disable_interrupts ();
  _run_atexits();
  _exit(code);
}
