/*
 * Copyright (c) 1994-1998 University of Utah and the Flux Group.
 * All rights reserved.
 *
 * This file is part of the Flux OSKit.  The OSKit is free software, also known
 * as "open source;" you can redistribute it and/or modify it under the terms
 * of the GNU General Public License (GPL), version 2, as published by the Free
 * Software Foundation (FSF).  To explore alternate licensing terms, contact
 * the University of Utah at csl-dist@cs.utah.edu or +1-801-585-3271.
 *
 * The OSKit is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GPL for more details.  You should have
 * received a copy of the GPL along with the OSKit; see the file COPYING.  If
 * not, write to the FSF, 59 Temple Place #330, Boston, MA 02111-1307, USA.
 */
/*----------------------------------------------------------------------------
 *-- -------------------         M a R T E   O S         ------------------ --
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                's t r i n g'
 *
 *                                      H
 *
 * File 'string.h'                                                      by Mar.
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
#ifndef _STRING_H_
#define _STRING_H_

#include <sys/types.h>
#include <sys/cpp_macros.h>

CPP_BEGIN_DECLS

#ifndef NULL
#define NULL 0
#endif


size_t strlen(const char *__s);
char *strcpy(char *__dest, const char *__src);
char *strncpy(char *__dest, const char *__src, size_t __n);
char *strdup(const char *__s);
char *strcat(char *__dest, const char *__src);
char *strncat(char *__dest, const char *__src, size_t __n);
int strcmp(const char *__a, const char *__b);
int strncmp(const char *__a, const char *__b, size_t __n);

char *strchr(const char *__s, int __c);
char *strrchr(const char *__s, int __c);
char *strstr(const char *__haystack, const char *__needle);
char *strtok(char *__s, const char *__delim);
char *strtok_r(char *__s, const char *__delim, char **__last);
char *strpbrk(const char *__s1, const char *__s2);
size_t strspn(const char *__s1, const char *__s2);
size_t strcspn(const char *__s1, const char *__s2);

#ifndef __GNUC__
void *memcpy(void *__to, const void *__from, size_t __n);
#else
#define memcpy __builtin_memcpy
#endif
void *memmove(void *__to, const void *__from, size_t __n);
void *memset(void *__to, int __ch, size_t __n);
#ifndef __GNUC__
int memcmp(const void *s1v, const void *s2v, size_t size);
#else
#define memcmp __builtin_memcmp
#endif

void * memchr(const void *b, int c, size_t len);

char *strerror(int __errno);

int strcasecmp(const char *s1, const char *s2);
int strncasecmp(const char *s1, const char *s2, size_t n);

CPP_END_DECLS

#endif // _STRING_H_
