/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *                                 's t d i o'
 *
 *                                      H
 *
 * File 'stdio.h'                                                      By MAR.
 *
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
#ifndef _STDIO_H_
#define _STDIO_H_

#include <sys/cpp_macros.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/marte_configuration_parameters.h>

CPP_BEGIN_DECLS

#ifndef NULL
#define NULL 0
#endif

typedef	int FILE;

// Defined in stdio.c
extern FILE * stdin;
extern FILE * stdout;
extern FILE * stderr;

#ifndef EOF
#define EOF -1
// any change in this value should be also done in
// 'k-file_system.adb'.
#endif

int putchar(int __c);
int puts(const char *__str);
int printf(const char *__format, ...);
//int vprintf(const char *__format, va_list __vl);
int sprintf(char *__dest, const char *__format, ...);
int snprintf(char *__dest, size_t __size, const char *__format, ...);
//int vsprintf(char *__dest, const char *__format, va_list __vl);
//int vsnprintf(char *__dest, size_t __size, const char *__format, va_list __vl);
int scanf(const char *__format, ...);
int sscanf(const char *__str, const char *__format, ...);
int getchar(void);
int ungetc(int c, FILE *stream);
char *gets(char *__str);
char *fgets(char *__str, int __size, FILE *__stream);
int fileno(FILE *stream);
FILE *fopen(const char *__path, const char *__mode);
//FILE *fdopen(int fd, const char *__mode);
FILE *freopen(const char *path, const char *mode, FILE *stream);
int fflush(FILE *stream);
int fclose(FILE *__stream);
size_t fread(void *__buf, size_t __size, size_t __count, FILE *__stream);
size_t fwrite(void *__buf, size_t __size, size_t __count, FILE *__stream);
int fputc(int __c, FILE *__stream);
int fputs(const char *str, FILE *stream);
int fgetc(FILE *__stream);
int fprintf(FILE *__stream, const char *__format, ...);
//int vfprintf(FILE *__stream, const char *__format, va_list __vl);
int fscanf(FILE *__stream, const char *__format, ...);
int fseek(FILE *__stream, long __offset, int __whence);
long ftell(FILE *__stream);
//void rewind(FILE *__stream);
//int rename(const char *__from, const char *__to);
//int remove(const char *__path);
void dohexdump(void *__base, void *__buf, int __len, int __bytes);
#define hexdumpb(base, buf, nbytes) dohexdump(base, buf, nbytes, 0)
#define hexdumpw(base, buf, nwords) dohexdump(base, buf, nwords, 1)
void perror(const char *__string);

#define putc(c, stream) fputc(c, stream)
#define getc(stream) fgetc(stream)

// Macros used with setvbuf() function. Not used in MaRTE. Only
// defined in order to compile the GNAT runtime system
#define _IOFBF  1    // Input/output fully buffered.
#define _IOLBF  2    // Input/output line buffered.
#define _IONBF  4    // Input/output unbuffered.

// MaRTE OS specific
//
// To be used inside kernel or drivers
#if MARTE_ARCHITECTURE == ARCH_GNAT_BB_ARM || MARTE_ARCHITECTURE == ARCH_STM32F
#  define printe printf
#  define vprinte vprintf
#  define printc printf
#else
int printe(const char *__format, ...); // prints directly on stderr (error)
int vprinte(const char *fmt, va_list args); // prints directly on stderr
int printc(const char *__format, ...); // prints directly on stdout (console)
#endif

CPP_END_DECLS
#endif // _STDIO_H_
