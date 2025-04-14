
#include "ping.h"
#include <stdio.h>
#include <unistd.h>

#include "lwip/init.h"

#define PING_TARGET 127.0.0.1

int main (void)
{
    printf ("Starting...\n");
    fflush(stdout); 
    lwip_init();
    ping_init();
    fflush(stdout);
    sleep (1);//ping_send_now();
    printf ("Ending...");
    fflush(stdout);
    sleep (5);
    fflush(stdout);
    printf ("Ok\n");
    return 0;
}
