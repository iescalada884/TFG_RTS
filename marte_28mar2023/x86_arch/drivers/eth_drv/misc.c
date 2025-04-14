/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *
 *                               'm i s c . c'
 *
 *                                     C
 *
 *
 * File 'misc.c'                                             Modified by Chema.
 *                                                          Jose Maria Martinez
 *                                                            <chema@gmx.net>
 * Useful functions thar are used in other modules such as printf
 * and friends (sprintf, vsprintf)
 *
 *---------------------------------------------------------------------------*/
/*****************************************************************************/
/* Most of this module is taken from the Etherboot proyect:                  */
/* http://etherboot.sourceforge.net v.5.1.3.                                 */

/*
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 */


/**************************************************************************
MISC Support Routines
**************************************************************************/
/* The hack works including stdio.h BEFORE eth_defs.h, in this way we will*/
/* be using the getchar and putchar functions from stdio.h                */
#include <stdio.h>
#include "eth_defs.h"


/**************************************************************************
IPCHKSUM - Checksum IP Header
**************************************************************************/
uint16_t ipchksum(void *p, int len)
{
	unsigned long sum = 0;
	uint16_t *ip = p;
	while (len > 1) {
		len -= 2;
		sum += *(ip++);
		if (sum > 0xFFFF)
			sum -= 0xFFFF;
	}
	if (len) {
		uint8_t *ptr = (void *)ip;
		sum += *ptr;
		if (sum > 0xFFFF)
			sum -= 0xFFFF;
	}
	return((~sum) & 0x0000FFFF);
}



/**************************************************************************
RANDOM - compute a random number between 0 and 2147483647L or 2147483562?
**************************************************************************/
/* long eth_random(void) */
/* { */
/* 	static long seed = 0; */
/* 	long q; */
/* 	if (!seed) */
/* Initialize linear congruential generator */
/* 		seed = currticks() + *(long *)&arptable[ARP_CLIENT].node */
/* 		       + ((short *)arptable[ARP_CLIENT].node)[2]; */
/* 	 simplified version of the LCG given in Bruce Schneier's */
/* 	   "Applied Cryptography" */
/* 	q = seed/53668; */
/* 	if ((seed = 40014*(seed-53668*q) - 12211*q) < 0) seed += 2147483563L; */
/* 	return seed; */
/* } */

/**************************************************************************
TWIDDLE
**************************************************************************/
void twiddle(void)
{
#ifdef	BAR_PROGRESS
	static unsigned long lastticks = 0;
	static int count=0;
	static const char tiddles[]="-\\|/";
	unsigned long ticks;
#ifdef FREEBSD_PXEEMU
	extern char pxeemu_nbp_active;
	if(pxeemu_nbp_active != 0)
		return;
#endif
	if ((ticks = currticks()) == lastticks)
		return;
	lastticks = ticks;
	putchar(tiddles[(count++)&3]);
	putchar('\b');
#else
#ifdef FREEBSD_PXEEMU
	extern char pxeemu_nbp_active;
	if(pxeemu_nbp_active != 0)
		return;
#endif
	putchar('.');
#endif	/* BAR_PROGRESS */
}

/**************************************************************************
STRCASECMP (not entirely correct, but this will do for our purposes)
**************************************************************************/
// int strcasecmp(const char *a, const char *b)
// {
// 	while (*a && *b && (*a & ~0x20) == (*b & ~0x20)) {a++; b++; }
// 	return((*a & ~0x20) - (*b & ~0x20));
// }

/**************************************************************************
PRINTF and friends

	Formats:
		%[#]x	- 4 bytes long (8 hex digits, lower case)
		%[#]X	- 4 bytes long (8 hex digits, upper case)
		%[#]hx	- 2 bytes int (4 hex digits, lower case)
		%[#]hX	- 2 bytes int (4 hex digits, upper case)
		%[#]hhx	- 1 byte int (2 hex digits, lower case)
		%[#]hhX	- 1 byte int (2 hex digits, upper case)
			- optional # prefixes 0x or 0X
		%d	- decimal int
		%c	- char
		%s	- string
		%@	- Internet address in ddd.ddd.ddd.ddd notation
		%!	- Ethernet address in xx:xx:xx:xx:xx:xx notation
	Note: width specification not supported
**************************************************************************/
static int eth_vsprintf(char *buf, const char *fmt, const int *dp)
{
	char *p, *s;

	s = buf;
	for ( ; *fmt != '\0'; ++fmt) {
		if (*fmt != '%') {
			buf ? *s++ = *fmt : putchar(*fmt);
			continue;
		}
		if (*++fmt == 's') {
			for (p = (char *)*dp++; *p != '\0'; p++)
				buf ? *s++ = *p : putchar(*p);
		}
		else {	/* Length of item is bounded */
			char tmp[20], *q = tmp;
			int alt = 0;
			int shift = 28;
			if (*fmt == '#') {
				alt = 1;
				fmt++;
			}
			if (*fmt == 'h') {
				shift = 12;
				fmt++;
			}
			if (*fmt == 'h') {
				shift = 4;
				fmt++;
			}
			/*
			 * Before each format q points to tmp buffer
			 * After each format q points past end of item
			 */
			if ((*fmt | 0x20) == 'x') {
				/* With x86 gcc, sizeof(long) == sizeof(int) */
				const long *lp = (const long *)dp;
				long h = *lp++;
				int ncase = (*fmt & 0x20);
				dp = (const int *)lp;
				if (alt) {
					*q++ = '0';
					*q++ = 'X' | ncase;
				}
				for ( ; shift >= 0; shift -= 4)
					*q++ = "0123456789ABCDEF"[(h >> shift) & 0xF] | ncase;
			}
			else if (*fmt == 'd') {
				int i = *dp++;
				char *r;
				if (i < 0) {
					*q++ = '-';
					i = -i;
				}
				p = q;		/* save beginning of digits */
				do {
					*q++ = '0' + (i % 10);
					i /= 10;
				} while (i);
				/* reverse digits, stop in middle */
				r = q;		/* don't alter q */
				while (--r > p) {
					i = *r;
					*r = *p;
					*p++ = i;
				}
			}
			else if (*fmt == '@') {
				unsigned char *r;
				union {
					long		l;
					unsigned char	c[4];
				} u;
				const long *lp = (const long *)dp;
				u.l = *lp++;
				dp = (const int *)lp;
				for (r = &u.c[0]; r < &u.c[4]; ++r)
					q += sprintf(q, "%d.", *r);
				--q;
			}
			else if (*fmt == '!') {
				char *r;
				p = (char *)*dp++;
				for (r = p + ETH_ALEN; p < r; ++p)
					q += sprintf(q, "%hhX:", *p);
				--q;
			}
			else if (*fmt == 'c')
				*q++ = *dp++;
			else
				*q++ = *fmt;
			/* now output the saved string */
			for (p = tmp; p < q; ++p)
				buf ? *s++ = *p : putchar(*p);
		}
	}
	if (buf)
		*s = '\0';
	return (s - buf);
}

int eth_sprintf(char *buf, const char *fmt, ...)
{
	return vsprintf(buf, fmt, ((const int *)&fmt)+1);
}

void eth_printf(const char *fmt, ...)
{
	(void)vsprintf(0, fmt, ((const int *)&fmt)+1);
}

/************************************************************************** */
/* INET_ATON - Convert an ascii x.x.x.x to binary form */
/* **************************************************************************/
/* int inet_aton(const char *start, in_addr *i) */
/* { */
/* 	const char *p = start; */
/* 	const char *digits_start; */
/* 	unsigned long ip = 0; */
/* 	unsigned long val; */
/* 	int j; */
/* 	for(j = 0; j <= 3; j++) { */
/* 		digits_start = p; */
/* 		val = strtoul(p, &p, 10); */
/* 		if ((p == digits_start) || (val > 255)) return 0; */
/* 		if ( ( j < 3 ) && ( *(p++) != '.' ) ) return 0; */
/* 		ip = (ip << 8) | val; */
/* 	} */
/* 	i->s_addr = htonl(ip); */
/* 	return p - start; */
/* } */


/* unsigned long strtoul(const char *p, const char **endp, int base) */
/* { */
/* 	unsigned long ret = 0; */
/* 	if (base != 10) return 0; */
/* 	while((*p >= '0') && (*p <= '9')) { */
/* 		ret = ret*10 + (*p - '0'); */
/* 		p++; */
/* 	} */
/* 	if (endp) */
/* 		*endp = p; */
/* 	return(ret); */

/* } */

#define K_RDWR		0x60		/* keyboard data & cmds (read/write) */
#define K_STATUS	0x64		/* keyboard status */
#define K_CMD		0x64		/* keybd ctlr command (write-only) */

#define K_OBUF_FUL	0x01		/* output buffer full */
#define K_IBUF_FUL	0x02		/* input buffer full */

#define KC_CMD_WIN	0xd0		/* read  output port */
#define KC_CMD_WOUT	0xd1		/* write output port */
#define KB_SET_A20	0xdf		/* enable A20,
					   enable output buffer full interrupt
					   enable data line
					   disable clock line */
#define KB_UNSET_A20	0xdd		/* enable A20,
					   enable output buffer full interrupt
					   enable data line
					   disable clock line */

enum { Disable_A20 = 0x2400, Enable_A20 = 0x2401, Query_A20_Status = 0x2402,
	Query_A20_Support = 0x2403 };

#if defined(PCBIOS) && !defined(IBM_L40)
static void empty_8042(void)
{
	unsigned long time;
	char st;

	time = currticks() + TICKS_PER_SEC;	/* max wait of 1 second */
	while ((((st = inb(K_CMD)) & K_OBUF_FUL) ||
	       (st & K_IBUF_FUL)) &&
	       currticks() < time)
		inb(K_RDWR);
}
#endif	/* IBM_L40 */

#if defined(PCBIOS)
/*
 * Gate A20 for high memory
 */
void gateA20_set(void)
{
#warning "gateA20_set should test to see if it is already set"
	if (int15(Enable_A20) == 0) {
		printf("A20 enabled via BIOS\n");
		return;
	}
#ifdef	IBM_L40
	outb(0x2, 0x92);
#else	/* IBM_L40 */
	empty_8042();
	outb(KC_CMD_WOUT, K_CMD);
	empty_8042();
	outb(KB_SET_A20, K_RDWR);
	empty_8042();
#endif	/* IBM_L40 */
}
#endif

#if defined(PCBIOS) && (defined(TAGGED_IMAGE) || defined(CAN_BOOT_DISK))
/*
 * Unset Gate A20 for high memory - some operating systems (mainly old 16 bit
 * ones) don't expect it to be set by the boot loader.
 */
void gateA20_unset(void)
{
	if (int15(Disable_A20) == 0) {
		printf("A20 disabled via BIOS\n");
		return;
	}
#ifdef	IBM_L40
	outb(0x0, 0x92);
#else	/* IBM_L40 */
	empty_8042();
	outb(KC_CMD_WOUT, K_CMD);
	empty_8042();
	outb(KB_UNSET_A20, K_RDWR);
	empty_8042();
#endif	/* IBM_L40 */
}
#endif

void
eth_putchar(int c)
{
	if (c == '\n')
		putchar('\r');
#ifdef	CONSOLE_CRT
	console_putc(c);
#endif
#ifdef	CONSOLE_SERIAL
	serial_putc(c);
#endif
}

/**************************************************************************
GETCHAR - Read the next character from input device WITHOUT ECHO
**************************************************************************/
int eth_getchar(void)
{
	int c = 256;

	do {
#if defined(PCBIOS) && defined(POWERSAVE)
		/* Doze for a while (until the next interrupt).  This works
		 * fine, because the keyboard is interrupt-driven, and the
		 * timer interrupt (approx. every 50msec) takes care of the
		 * serial port, which is read by polling.  This reduces the
		 * power dissipation of a modern CPU considerably, and also
		 * makes Etherboot waiting for user interaction waste a lot
		 * less CPU time in a VMware session.  */
		cpu_nap();
#endif	/* POWERSAVE */
#ifdef	CONSOLE_CRT
		if (console_ischar())
			c = console_getc();
#endif
#ifdef	CONSOLE_SERIAL
		if (serial_ischar())
			c = serial_getc();
#endif
	} while (c==256);
	if (c == '\r')
		c = '\n';
	return c;
}

int iskey(void)
{
#ifdef	CONSOLE_CRT
	if (console_ischar())
		return 1;
#endif
#ifdef	CONSOLE_SERIAL
	if (serial_ischar())
		return 1;
#endif
	return 0;
}

#ifdef DEBUG_UTILS

void eth_pause ( void ) {
	printf ( "\nPress a key" );
	getchar();
	printf ( "\r           \r" );
}

void more ( void ) {
	printf ( "---more---" );
	getchar();
	printf ( "\r          \r" );
}

/* Produce a paged hex dump of the specified data and length */
void hex_dump ( const char *data, const unsigned int len ) {
	unsigned int index;
	for ( index = 0; index < len; index++ ) {
		if ( ( index % 16 ) == 0 ) {
			printf ( "\n" );
		}
		if ( ( index % 368 ) == 352 ) {
			more();
		}
		if ( ( index % 16 ) == 0 ) {
			printf ( "%hX :", index );
		}
		printf ( " %hhX", data[index] );
	}
	printf ( "\n" );
}

#endif /* DEBUG_UTILS */

/*
 * Local variables:
 *  c-basic-offset: 8
 * End:
 */
