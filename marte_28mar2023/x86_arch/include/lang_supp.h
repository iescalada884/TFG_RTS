/*!
 * @file lang_supp.h
 *
 * @brief language support functions and variables
 *
 * @version 0.01
 *
 * @date 31-Oct-2008
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@{gmail.com, unican.es}>
 *
 * @comments
 *
 * This module contains functions to give support to some programming
 * languages (for example for C++ or the constructors in C). It is based on
 * PaRTiKle's functions by Miguel Masmano.
 *
 * @license
 *
 * See MaRTE OS License
 *
 */

#ifndef _LANG_SUPP_H_
#define _LANG_SUPP_H_

#include <sys/cpp_macros.h>
#include <sys/types.h>

CPP_BEGIN_DECLS
struct dl_phdr_info {
        unsigned long dlpi_addr;
        const char *dlpi_name;
        const unsigned long *dlpi_phdr;
        unsigned short dlpi_phnum;
};

extern int __cxa_atexit(void (*f)(void *), void *p, void *d);
extern void __cxa_finalize(void *d);

CPP_END_DECLS

#endif
