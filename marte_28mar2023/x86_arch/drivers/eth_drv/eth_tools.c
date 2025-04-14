/*----------------------------------------------------------------------------
 *-------------------------         R T - E P         ------------------------
 *----------------------------------------------------------------------------
 *                                                       RT-EP {RT-EP VERSION}
 *
 *                           'e t h _ t o o l s . c'
 *
 *                                     C
 *
 *
 * File 'eth_tools.c'                                                 By Chema.
 *                                                          Jose Maria Martinez
 *                                                         <martinjm@unican.es>
 *
 * Body functions of some specified in if_ether.h.
 *
 *
 *---------------------------------------------------------------------------*/
#include <stdio.h>
#include <drivers/if_ether.h>

void ether_ntoa (const struct ether_addr *addr, char *asc)
{
        sprintf (asc, "%02X:%02X:%02X:%02X:%02X:%02X",
                 addr->ether_addr_octet[0], addr->ether_addr_octet[1],
                 addr->ether_addr_octet[2], addr->ether_addr_octet[3],
                 addr->ether_addr_octet[4], addr->ether_addr_octet[5]);
}

int ether_aton(const char *asc, struct ether_addr *n)
{
        unsigned int i[ETH_ALEN];

        if (sscanf((char *)asc, " %x:%x:%x:%x:%x:%x ", &i[0], &i[1],
            &i[2], &i[3], &i[4], &i[5]) == ETH_ALEN) {
                    n->ether_addr_octet[0] = (unsigned char)i[0];
                    n->ether_addr_octet[1] = (unsigned char)i[1];
                    n->ether_addr_octet[2] = (unsigned char)i[2];
                    n->ether_addr_octet[3] = (unsigned char)i[3];
                    n->ether_addr_octet[4] = (unsigned char)i[4];
                    n->ether_addr_octet[5] = (unsigned char)i[5];
                    return 0;
        }
        return -1;
}
