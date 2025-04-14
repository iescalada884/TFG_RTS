/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                'd o p r i n t'
 *
 *                                      C
 *
 * File 'doprint.c'                                                    by MAR.
 *
 *
 * Code taken from the Free BSD libc (file 'vfprintf.c').
 *
 *---------------------------------------------------------------------------*/
/*-
 * Copyright (c) 1990, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */


/*
 * Actual printf innards.
 *
 * This code is large and complicated...
 */

#include <sys/types.h>

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <stdarg.h>

#include <oskit/machine/pc/base_console.h>
#include "doprint.h"

extern void direct_write_on_stdout (char * str, int count); // 'Kernel_Console'
extern void direct_write_on_stderr (char * str, int count); // 'Kernel_Console'

/*
 * Types size
 */
#define     UQUAD_MAX     0xffffffffffffffffULL       /* max unsigned quad */
#define     QUAD_MAX      0x7fffffffffffffffLL        /* max signed quad */
// Sangorrin: ULONG_MAX and LONG_MAX are defined in limits.h
// #define     ULONG_MAX     0xffffffff      /* max value for an unsigned long */
// #define     LONG_MAX      2147483647      /* max value for a long */
typedef     long long            quad_t;
typedef     unsigned long long   u_quad_t;   /* quads */
typedef     unsigned short       u_short;
typedef     unsigned int         u_int;

struct __siov {
        void    *iov_base;
        size_t  iov_len;
};
struct __suio {
        struct  __siov *uio_iov;
        int     uio_iovcnt;
        int     uio_resid;
};

static void savechar(doprint_destiny_t *d, int c)
{
        if (d->max != SPRINTF_UNLIMITED)
          if (d->len == d->max)
            return;

        d->len++;
        *d->buf = c;
        d->buf++;
}

/*
 * __sfvwrite
 *
 * Here is where the characteres are actually written to the destiny
 * (string, console or another device).
 *
 * Write some memory regions.  Return zero.
 *
 * Taken from file 'fvwrite.c'. Simplified (only unbuffered output)
 * and adapted (call 'console_putbytes' to write on console)
 */
int
__sfvwrite(destiny, uio)
     doprint_destiny_t *destiny;
     register struct __suio *uio;
{
        register size_t len;
        register char *p;
        register int w;
        register struct __siov *iov;
        int i=0;

        if ((len = uio->uio_resid) == 0)
                return (0);

        iov = uio->uio_iov;
        p = iov->iov_base;
        len = iov->iov_len;
        iov++;
        do {
          while (len == 0) {
            p = iov->iov_base;
            len = iov->iov_len;
            iov++;
          }
          if (destiny->buf != NULL) // sprintf
            for(i=0; i < len; ++i)
              savechar(destiny, p[i]);
          else // printf
            if (destiny->fd == -STDERR_FILENO) { // called from 'printe'
              // '-STDERR_FILENO' is the special value used by
              // 'printe' to indicate we want to write directly to
              // stderr without passing through the file system. This
              // is a more direct and secure method.
              direct_write_on_stderr (p, len);
            } else if (destiny->fd == -STDOUT_FILENO) { // called from 'printc'
              // '-STDOUT_FILENO' is the special value used by
              // 'printc' to indicate we want to write directly to
              // stdout without passing through the file system. This
              // is a more direct and secure method.
              direct_write_on_stdout (p, len);
            } else { // print on any other device
              if (write (destiny->fd, p, len) == -1)
                return -1; // error
            }
          w = len;
          p += len;
          len = 0;
        } while ((uio->uio_resid -= w) != 0);
        return (0);
}

/* Define FLOATING_POINT to get floating point. */
#define FLOATING_POINT


/*
 * Flush out all the vectors defined by the given uio,
 * then reset it so that it can be reused.
 */
static int __sprint(destiny, uio)
     doprint_destiny_t *destiny;
     register struct __suio *uio;
{
        register int err;

        if (uio->uio_resid == 0) {
                uio->uio_iovcnt = 0;
                return (0);
        }
        err = __sfvwrite(destiny, uio);
        uio->uio_resid = 0;
        uio->uio_iovcnt = 0;
        return (err);
}

/*
 * Macros for converting digits to letters and vice versa
 */
#define to_digit(c)     ((c) - '0')
#define is_digit(c)     ((unsigned)to_digit(c) <= 9)
#define to_char(n)      ((n) + '0')

/*
 * Convert an unsigned long to ASCII for printf purposes, returning
 * a pointer to the first character of the string representation.
 * Octal numbers can be forced to have a leading zero; hex numbers
 * use the given digits.
 */
static char *
__ultoa(val, endp, base, octzero, xdigs)
        register unsigned long val;
        char *endp;
        int base, octzero;
        char *xdigs;
{
        register char *cp = endp;
        register long sval;

        /*
         * Handle the three cases separately, in the hope of getting
         * better/faster code.
         */
        switch (base) {
        case 10:
                if (val < 10) { /* many numbers are 1 digit */
                        *--cp = to_char(val);
                        return (cp);
                }
                /*
                 * On many machines, unsigned arithmetic is harder than
                 * signed arithmetic, so we do at most one unsigned mod and
                 * divide; this is sufficient to reduce the range of
                 * the incoming value to where signed arithmetic works.
                 */
                if (val > LONG_MAX) {
                        *--cp = to_char(val % 10);
                        sval = val / 10;
                } else
                        sval = val;
                do {
                        *--cp = to_char(sval % 10);
                        sval /= 10;
                } while (sval != 0);
                break;

        case 8:
                do {
                        *--cp = to_char(val & 7);
                        val >>= 3;
                } while (val);
                if (octzero && *cp != '0')
                        *--cp = '0';
                break;

        case 16:
                do {
                        *--cp = xdigs[val & 15];
                        val >>= 4;
                } while (val);
                break;

        default:                        /* oops */
                ;
        }
        return (cp);
}

/* Identical to __ultoa, but for quads. */
static char *
__uqtoa(val, endp, base, octzero, xdigs)
        register u_quad_t val;
        char *endp;
        int base, octzero;
        char *xdigs;
{
        register char *cp = endp;
        register quad_t sval;

        /* quick test for small values; __ultoa is typically much faster */
        /* (perhaps instead we should run until small, then call __ultoa?) */
        if (val <= ULONG_MAX)
                return (__ultoa((unsigned long)val, endp, base, octzero, xdigs));
        switch (base) {
        case 10:
                if (val < 10) {
                        *--cp = to_char(val % 10);
                        return (cp);
                }
                if (val > QUAD_MAX) {
                        *--cp = to_char(val % 10);
                        sval = val / 10;
                } else
                        sval = val;
                do {
                        *--cp = to_char(sval % 10);
                        sval /= 10;
                } while (sval != 0);
                break;

        case 8:
                do {
                        *--cp = to_char(val & 7);
                        val >>= 3;
                } while (val);
                if (octzero && *cp != '0')
                        *--cp = '0';
                break;

        case 16:
                do {
                        *--cp = xdigs[val & 15];
                        val >>= 4;
                } while (val);
                break;

        default:
                ;
        }
        return (cp);
}

#ifdef FLOATING_POINT
//#include <math.h>
/*
 * Floating point scanf/printf (input/output) definitions.
 */

/* 11-bit exponent (VAX G floating point) is 308 decimal digits */
#define MAXEXP          308
/* 128 bit fraction takes up 39 decimal digits; max reasonable precision */
#define MAXFRACT        39

#define BUF             (MAXEXP+MAXFRACT+1)     /* + decimal point */
#define DEFPREC         6

static char *cvt (double, int, int, char *, int *, int, int *);
static int exponent (char *, int, int);

// 'oskit-990722/freebsd/src/lib/libc/i386/gen/isinf.c'
static int isnan(double d)
{
        register struct IEEEdp {
                u_int manl : 32;
                u_int manh : 20;
                u_int  exp : 11;
                u_int sign :  1;
        } *p = (struct IEEEdp *)&d;

        return(p->exp == 2047 && (p->manh || p->manl));
}

// 'oskit-990722/freebsd/src/lib/libc/i386/gen/isinf.c'
static int isinf(d)
        double d;
{
        register struct IEEEdp {
                u_int manl : 32;
                u_int manh : 20;
                u_int  exp : 11;
                u_int sign :  1;
        } *p = (struct IEEEdp *)&d;

        return(p->exp == 2047 && !p->manh && !p->manl);
}


#else /* no FLOATING_POINT */

#define BUF             68

#endif /* FLOATING_POINT */


/*
 * Flags used during conversion.
 */
#define ALT             0x001           /* alternate form */
#define HEXPREFIX       0x002           /* add 0x or 0X prefix */
#define LADJUST         0x004           /* left adjustment */
#define LONGDBL         0x008           /* long double; unimplemented */
#define LONGINT         0x010           /* long integer */
#define QUADINT         0x020           /* quad integer */
#define SHORTINT        0x040           /* short integer */
#define ZEROPAD         0x080           /* zero (as opposed to blank) pad */
#define FPT             0x100           /* Floating point number */

/* MaRTE
int _doprint(destiny, fmt0, ap)
     doprint_destiny_t *destiny;
     const char *fmt0;
     va_list ap;*/
int _doprint(doprint_destiny_t *destiny,
	     const char *fmt0,
	     va_list ap)
{
        register char *fmt;     /* format string */
        register int ch;        /* character from fmt */
        register int n;         /* handy integer (short term usage) */
        register char *cp;      /* handy char pointer (short term usage) */
        register struct __siov *iovp;/* for PRINT macro */
        register int flags;     /* flags as above */
        int ret;                /* return value accumulator */
        int width;              /* width from format (%8d), or 0 */
        int prec;               /* precision from format (%.3d), or -1 */
        char sign;              /* sign prefix (' ', '+', '-', or \0) */
#ifdef FLOATING_POINT
        char softsign;          /* temporary negative sign for floats */
        double _double = 0.0;   /* double precision arguments %[eEfgG] */
        int expt;               /* integer value of exponent */
        int expsize = 0;        /* character count for expstr */
        int ndig;               /* actual number of digits returned by cvt */
        char expstr[7];         /* buffer for exponent string */
#endif
        unsigned long   ulval = 0;  /* integer arguments %[diouxX] */
        u_quad_t uqval = 0;     /* %q integers */
        int base;               /* base for [diouxX] conversion */
        int dprec;              /* a copy of prec if [diouxX], 0 otherwise */
        int realsz;             /* field size expanded by dprec, sign, etc */
        int size;               /* size of converted field or string */
        char *xdigs = NULL;     /* digits for [xX] conversion */
#define NIOV 8
        struct __suio uio;      /* output information: summary */
        struct __siov iov[NIOV];/* ... and individual io vectors */
        char buf[BUF];          /* space for %c, %[diouxX], %[eEfgG] */
        char ox[2];             /* space for 0x hex-prefix */

        /*
         * Choose PADSIZE to trade efficiency vs. size.  If larger printf
         * fields occur frequently, increase PADSIZE and make the initialisers
         * below longer.
         */
#define PADSIZE 16              /* pad chunk size */
        static char blanks[PADSIZE] =
         {' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '};
        static char zeroes[PADSIZE] =
         {'0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0'};

        // << MaRTE OS
        if ((destiny->fd <= NO_FD) && (destiny->buf == NULL)) {
          // nothing to do: no fd and no string
          return 0;
        }
        // MaRTE OS >>

        /*
         * BEWARE, these `goto error' on error, and PAD uses `n'.
         */
#define PRINT(ptr, len) { \
        iovp->iov_base = (ptr); \
        iovp->iov_len = (len); \
        uio.uio_resid += (len); \
        iovp++; \
        if (++uio.uio_iovcnt >= NIOV) { \
                if (__sprint(destiny, &uio)) \
                        goto error; \
                iovp = iov; \
        } \
}
#define PAD(howmany, with) { \
        if ((n = (howmany)) > 0) { \
                while (n > PADSIZE) { \
                        PRINT(with, PADSIZE); \
                        n -= PADSIZE; \
                } \
                PRINT(with, n); \
        } \
}
#define FLUSH() { \
        if (uio.uio_resid && __sprint(destiny, &uio)) \
                goto error; \
        uio.uio_iovcnt = 0; \
        iovp = iov; \
}

        /*
         * To extend shorts properly, we need both signed and unsigned
         * argument extraction methods.
         */
#define SARG() \
        (flags&LONGINT ? va_arg(ap, long) : \
            flags&SHORTINT ? (long)(short)va_arg(ap, int) : \
            (long)va_arg(ap, int))
#define UARG() \
        (flags&LONGINT ? va_arg(ap, unsigned long) : \
            flags&SHORTINT ? (unsigned long)(u_short)va_arg(ap, int) : \
            (unsigned long)va_arg(ap, u_int))

        fmt = (char *)fmt0;
        uio.uio_iov = iovp = iov;
        uio.uio_resid = 0;
        uio.uio_iovcnt = 0;
        ret = 0;

        /*
         * Scan the format for conversions (`%' character).
         */
        for (;;) {
                for (cp = fmt; (ch = *fmt) != '\0' && ch != '%'; fmt++)
                        /* void */;
                if ((n = fmt - cp) != 0) {
                        PRINT(cp, n);
                        ret += n;
                }
                if (ch == '\0')
                        goto done;
                fmt++;          /* skip over '%' */

                flags = 0;
                dprec = 0;
                width = 0;
                prec = -1;
                sign = '\0';

rflag:          ch = *fmt++;
reswitch:       switch (ch) {
                case ' ':
                        /*
                         * ``If the space and + flags both appear, the space
                         * flag will be ignored.''
                         *      -- ANSI X3J11
                         */
                        if (!sign)
                                sign = ' ';
                        goto rflag;
                case '#':
                        flags |= ALT;
                        goto rflag;
                case '*':
                        /*
                         * ``A negative field width argument is taken as a
                         * - flag followed by a positive field width.''
                         *      -- ANSI X3J11
                         * They don't exclude field widths read from args.
                         */
                        if ((width = va_arg(ap, int)) >= 0)
                                goto rflag;
                        width = -width;
                        /* FALLTHROUGH */
                case '-':
                        flags |= LADJUST;
                        goto rflag;
                case '+':
                        sign = '+';
                        goto rflag;
                case '.':
                        if ((ch = *fmt++) == '*') {
                                n = va_arg(ap, int);
                                prec = n < 0 ? -1 : n;
                                goto rflag;
                        }
                        n = 0;
                        while (is_digit(ch)) {
                                n = 10 * n + to_digit(ch);
                                ch = *fmt++;
                        }
                        prec = n < 0 ? -1 : n;
                        goto reswitch;
                case '0':
                        /*
                         * ``Note that 0 is taken as a flag, not as the
                         * beginning of a field width.''
                         *      -- ANSI X3J11
                         */
                        flags |= ZEROPAD;
                        goto rflag;
                case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                        n = 0;
                        do {
                                n = 10 * n + to_digit(ch);
                                ch = *fmt++;
                        } while (is_digit(ch));
                        width = n;
                        goto reswitch;
#ifdef FLOATING_POINT
                case 'L':
                        flags |= LONGDBL;
                        goto rflag;
#endif
                case 'h':
                        flags |= SHORTINT;
                        goto rflag;
                case 'l':
                        /* SANGORRIN: 'll' is like 'q' */
                        if (flags & LONGINT) {
                                flags = flags & (~LONGINT);
                                flags |= QUADINT;
                        } else {
                                flags |= LONGINT;
                        }
                        goto rflag;
                case 'q':
                        flags |= QUADINT;
                        goto rflag;
                case 'c':
                        *(cp = buf) = va_arg(ap, int);
                        size = 1;
                        sign = '\0';
                        break;
                case 'D':
                        flags |= LONGINT;
                        /*FALLTHROUGH*/
                case 'd':
                case 'i':
                        if (flags & QUADINT) {
                                uqval = va_arg(ap, quad_t);
                                if ((quad_t)uqval < 0) {
                                        uqval = -uqval;
                                        sign = '-';
                                }
                        } else {
                                ulval = SARG();
                                if ((long)ulval < 0) {
                                        ulval = -ulval;
                                        sign = '-';
                                }
                        }
                        base = 10;
                        goto number;
#ifdef FLOATING_POINT
                case 'e':
                case 'E':
                case 'f':
                        goto fp_begin;
                case 'g':
                case 'G':
                        if (prec == 0)
                                prec = 1;
fp_begin:               if (prec == -1)
                                prec = DEFPREC;
                        if (flags & LONGDBL)
                                _double = (double)va_arg(ap, long double);
                        else
                                _double = va_arg(ap, double);
                        /* do this before tricky precision changes */
                        if (isinf(_double)) {
                                if (_double < 0)
                                        sign = '-';
                                cp = "Inf";
                                size = 3;
                                break;
                        }
                        if (isnan(_double)) {
                                cp = "NaN";
                                size = 3;
                                break;
                        }
                        flags |= FPT;
                        cp = cvt(_double, prec, flags, &softsign,
                                &expt, ch, &ndig);
                        if (ch == 'g' || ch == 'G') {
                                if (expt <= -4 || expt > prec)
                                        ch = (ch == 'g') ? 'e' : 'E';
                                else
                                        ch = 'g';
                        }
                        if (ch <= 'e') {        /* 'e' or 'E' fmt */
                                --expt;
                                expsize = exponent(expstr, expt, ch);
                                size = expsize + ndig;
                                if (ndig > 1 || flags & ALT)
                                        ++size;
                        } else if (ch == 'f') {         /* f fmt */
                                if (expt > 0) {
                                        size = expt;
                                        if (prec || flags & ALT)
                                                size += prec + 1;
                                } else  /* "0.X" */
                                        size = prec + 2;
                        } else if (expt >= ndig) {      /* fixed g fmt */
                                size = expt;
                                if (flags & ALT)
                                        ++size;
                        } else
                                size = ndig + (expt > 0 ?
                                        1 : 2 - expt);

                        if (softsign)
                                sign = '-';
                        break;
#endif /* FLOATING_POINT */
                case 'n':
                        if (flags & QUADINT)
                                *va_arg(ap, quad_t *) = ret;
                        else if (flags & LONGINT)
                                *va_arg(ap, long *) = ret;
                        else if (flags & SHORTINT)
                                *va_arg(ap, short *) = ret;
                        else
                                *va_arg(ap, int *) = ret;
                        continue;       /* no output */
                case 'O':
                        flags |= LONGINT;
                        /*FALLTHROUGH*/
                case 'o':
                        if (flags & QUADINT)
                                uqval = va_arg(ap, u_quad_t);
                        else
                                ulval = UARG();
                        base = 8;
                        goto nosign;
                case 'p':
                        /*
                         * ``The argument shall be a pointer to void.  The
                         * value of the pointer is converted to a sequence
                         * of printable characters, in an implementation-
                         * defined manner.''
                         *      -- ANSI X3J11
                         */
                        ulval = (unsigned long)va_arg(ap, void *);
                        base = 16;
                        xdigs = "0123456789abcdef";
                        flags = (flags & ~QUADINT) | HEXPREFIX;
                        ch = 'x';
                        goto nosign;
                case 's':
                        if ((cp = va_arg(ap, char *)) == NULL)
                                cp = "(null)";
                        if (prec >= 0) {
                                /*
                                 * can't use strlen; can only look for the
                                 * NUL in the first `prec' characters, and
                                 * strlen() will go further.
                                 */
                                char *p = memchr(cp, 0, (size_t)prec);

                                if (p != NULL) {
                                        size = p - cp;
                                        if (size > prec)
                                                size = prec;
                                } else
                                        size = prec;
                        } else
                                size = strlen(cp);
                        sign = '\0';
                        break;
                case 'U':
                        flags |= LONGINT;
                        /*FALLTHROUGH*/
                case 'u':
                        if (flags & QUADINT)
                                uqval = va_arg(ap, u_quad_t);
                        else
                                ulval = UARG();
                        base = 10;
                        goto nosign;
                case 'X':
                        xdigs = "0123456789ABCDEF";
                        goto hex;
                case 'x':
                        xdigs = "0123456789abcdef";
hex:                    if (flags & QUADINT)
                                uqval = va_arg(ap, u_quad_t);
                        else
                                ulval = UARG();
                        base = 16;
                        /* leading 0x/X only if non-zero */
                        if (flags & ALT &&
                            (flags & QUADINT ? uqval != 0 : ulval != 0))
                                flags |= HEXPREFIX;

                        /* unsigned conversions */
nosign:                 sign = '\0';
                        /*
                         * ``... diouXx conversions ... if a precision is
                         * specified, the 0 flag will be ignored.''
                         *      -- ANSI X3J11
                         */
number:                 if ((dprec = prec) >= 0)
                                flags &= ~ZEROPAD;

                        /*
                         * ``The result of converting a zero value with an
                         * explicit precision of zero is no characters.''
                         *      -- ANSI X3J11
                         */
                        cp = buf + BUF;
                        if (flags & QUADINT) {
                                if (uqval != 0 || prec != 0)
                                        cp = __uqtoa(uqval, cp, base,
                                            flags & ALT, xdigs);
                        } else {
                                if (ulval != 0 || prec != 0)
                                        cp = __ultoa(ulval, cp, base,
                                            flags & ALT, xdigs);
                        }
                        size = buf + BUF - cp;
                        break;
                default:        /* "%?" prints ?, unless ? is NUL */
                        if (ch == '\0')
                                goto done;
                        /* pretend it was %c with argument ch */
                        cp = buf;
                        *cp = ch;
                        size = 1;
                        sign = '\0';
                        break;
                }

                /*
                 * All reasonable formats wind up here.  At this point, `cp'
                 * points to a string which (if not flags&LADJUST) should be
                 * padded out to `width' places.  If flags&ZEROPAD, it should
                 * first be prefixed by any sign or other prefix; otherwise,
                 * it should be blank padded before the prefix is emitted.
                 * After any left-hand padding and prefixing, emit zeroes
                 * required by a decimal [diouxX] precision, then print the
                 * string proper, then emit zeroes required by any leftover
                 * floating precision; finally, if LADJUST, pad with blanks.
                 *
                 * Compute actual size, so we know how much to pad.
                 * size excludes decimal prec; realsz includes it.
                 */
                realsz = dprec > size ? dprec : size;
                if (sign)
                        realsz++;
                else if (flags & HEXPREFIX)
                        realsz += 2;

                /* right-adjusting blank padding */
                if ((flags & (LADJUST|ZEROPAD)) == 0)
                        PAD(width - realsz, blanks);

                /* prefix */
                if (sign) {
                        PRINT(&sign, 1);
                } else if (flags & HEXPREFIX) {
                        ox[0] = '0';
                        ox[1] = ch;
                        PRINT(ox, 2);
                }

                /* right-adjusting zero padding */
                if ((flags & (LADJUST|ZEROPAD)) == ZEROPAD)
                        PAD(width - realsz, zeroes);

                /* leading zeroes from decimal precision */
                PAD(dprec - size, zeroes);

                /* the string or number proper */
#ifdef FLOATING_POINT
                if ((flags & FPT) == 0) {
                        PRINT(cp, size);
                } else {        /* glue together f_p fragments */
                        if (ch >= 'f') {        /* 'f' or 'g' */
                                if (_double == 0) {
                                        /* kludge for __dtoa irregularity */
                                        if (expt >= ndig &&
                                            (flags & ALT) == 0) {
                                                PRINT("0", 1);
                                        } else {
                                                PRINT("0.", 2);
                                                PAD(ndig - 1, zeroes);
                                        }
                                } else if (expt <= 0) {
                                        PRINT("0.", 2);
                                        PAD(-expt, zeroes);
                                        PRINT(cp, ndig);
                                } else if (expt >= ndig) {
                                        PRINT(cp, ndig);
                                        PAD(expt - ndig, zeroes);
                                        if (flags & ALT)
                                                PRINT(".", 1);
                                } else {
                                        PRINT(cp, expt);
                                        cp += expt;
                                        PRINT(".", 1);
                                        PRINT(cp, ndig-expt);
                                }
                        } else {        /* 'e' or 'E' */
                                if (ndig > 1 || flags & ALT) {
                                        ox[0] = *cp++;
                                        ox[1] = '.';
                                        PRINT(ox, 2);
                                        if (_double) {
                                                PRINT(cp, ndig-1);
                                        } else  /* 0.[0..] */
                                                /* __dtoa irregularity */
                                                PAD(ndig - 1, zeroes);
                                } else  /* XeYYY */
                                        PRINT(cp, 1);
                                PRINT(expstr, expsize);
                        }
                }
#else
                PRINT(cp, size);
#endif
                /* left-adjusting padding (always blank) */
                if (flags & LADJUST)
                        PAD(width - realsz, blanks);

                /* finally, adjust ret */
                ret += width > realsz ? width : realsz;

                FLUSH();        /* copy out the I/O vectors */
        }
done:
        FLUSH();
error:
        return (ret);
        /* NOTREACHED */
}

#ifdef FLOATING_POINT

extern char *__dtoa (double, int, int, int *, int *, char **);

/* MaRTE
static char *
cvt(value, ndigits, flags, sign, decpt, ch, length)
        double value;
        int ndigits, flags, *decpt, ch, *length;
        char *sign; */
static char *
cvt(double value, int ndigits, int flags, char *sign, int *decpt,
    int ch, int *length)
{
        int mode, dsgn;
        char *digits, *bp, *rve;

        if (ch == 'f')
                mode = 3;               /* ndigits after the decimal point */
        else {
                /*
                 * To obtain ndigits after the decimal point for the 'e'
                 * and 'E' formats, round to ndigits + 1 significant
                 * figures.
                 */
                if (ch == 'e' || ch == 'E')
                        ndigits++;
                mode = 2;               /* ndigits significant digits */
        }
        if (value < 0) {
                value = -value;
                *sign = '-';
        } else
                *sign = '\000';
        digits = __dtoa(value, mode, ndigits, decpt, &dsgn, &rve);
        if ((ch != 'g' && ch != 'G') || flags & ALT) {
                /* print trailing zeros */
                bp = digits + ndigits;
                if (ch == 'f') {
                        if (*digits == '0' && value)
                                *decpt = -ndigits + 1;
                        bp += *decpt;
                }
                if (value == 0) /* kludge for __dtoa irregularity */
                        rve = bp;
                while (rve < bp)
                        *rve++ = '0';
        }
        *length = rve - digits;
        return (digits);
}

static int
exponent(p0, exp, fmtch)
        char *p0;
        int exp, fmtch;
{
        register char *p, *t;
        char expbuf[MAXEXP];

        p = p0;
        *p++ = fmtch;
        if (exp < 0) {
                exp = -exp;
                *p++ = '-';
        }
        else
                *p++ = '+';
        t = expbuf + MAXEXP;
        if (exp > 9) {
                do {
                        *--t = to_char(exp % 10);
                } while ((exp /= 10) > 9);
                *--t = to_char(exp);
                for (; t < expbuf + MAXEXP; *p++ = *t++);
        }
        else {
                *p++ = '0';
                *p++ = to_char(exp);
        }
        return (p - p0);
}
#endif /* FLOATING_POINT */
