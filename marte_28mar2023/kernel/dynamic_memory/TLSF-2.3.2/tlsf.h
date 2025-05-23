/*
 * Two Levels Segregate Fit memory allocator (TLSF)
 * Version 2.3.2
 *
 * Written by Miguel Masmano Tello <mimastel@doctor.upv.es>
 *
 * Thanks to Ismael Ripoll for his suggestions and reviews
 *
 * Copyright (C) 2007, 2006, 2005, 2004
 *
 * This code is released using a dual license strategy: GPL/LGPL
 * You can choose the licence that better fits your requirements.
 *
 * Released under the terms of the GNU General Public License Version 2.0
 * Released under the terms of the GNU Lesser General Public License Version 2.1
 *
 */

#ifndef _TLSF_H_
#define _TLSF_H_

#include <sys/types.h>

extern size_t init_memory_pool(size_t, void *);
extern size_t get_used_size(void *);
extern void destroy_memory_pool(void *);
extern void *malloc_ex(size_t, void *);
extern void free_ex(void *, void *);
extern void *realloc_ex(void *, size_t, void *);
extern void *calloc_ex(size_t, size_t, void *);

// << MaRTE OS
#define rtl_malloc  malloc
#define rtl_free    free
#define rtl_realloc realloc
#define rtl_calloc  calloc
// >> MaRTE OS

extern void *rtl_malloc(size_t size);
extern void rtl_free(void *ptr);
extern void *rtl_realloc(void *ptr, size_t size);
extern void *rtl_calloc(size_t nelem, size_t elem_size);

#endif
