//  Test for all architectures
/**
 * Test program for the freelist implementation
 */

#include <misc/freelist.h>
#include <assert.h>
#include <stdio.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#if 1
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

int numbers[10];
freelist_t list;

void show()
{
        int i;

        DEBUG("\n");
        for (i=0;i<10;i++) {
                DEBUG("num[%d]= %d \n",i,numbers[i]);
        }
        DEBUG("\n");
}

void add(int value)
{
        int pos=freelist_alloc(&list);
        if (pos>=0) {
                numbers[pos]=value;
                DEBUG("added num[%d]= %d \n",pos,value);
        } else {
                DEBUG("alloc %d failed \n",value);
        }
}

void remove_pos(int pos)
{
        int err=freelist_free(&list,pos);
        if (err==0) {
                DEBUG("removed num[%d]= %d \n",pos,numbers[pos]);
                numbers[pos]=0;
        } else {
                DEBUG("free %d failed \n",pos);
        }
}

int main () {

        int i;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        freelist_init(&list,10);

        // add 11 elements to list

        for (i=100; i<111; i++) {
                add(i);
        }
        show();

        // remove 5 elements from list
        remove_pos(1);
        remove_pos(2);
        remove_pos(3);
        remove_pos(5);
        remove_pos(7);

        // remove already removed element
        remove_pos(3);
        show();

        // add 6 elements to list
        for (i=111; i<117; i++) {
                add(i);
        }
        show();

         // remove 5 elements from list
        remove_pos(1);
        remove_pos(2);
        remove_pos(3);
        remove_pos(5);
        remove_pos(7);
        show();

         // add 6 elements to list
        for (i=111; i<117; i++) {
                add(i);
        }
        show();

        printf("Test OK\n\n");
        return 0;
}
