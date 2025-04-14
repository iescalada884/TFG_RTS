
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define NO_ERROR 0

// Macros 'CHK' and 'CHK_INFO'
//
// Error check for POSIX functions that do NOT modify "errno"
// ("modern" POSIX functions). Functions that return zero if
// successful; otherwise, they return an error number to indicate the
// error.
#define CHK(p) { int ret;                                      \
                 if ((ret = p)) {			       \
                   printf ("Error:"#p":%s\n", strerror (ret)); \
                   exit (-1);                                  \
                 }                                             \
               }

#define CHK_INFO(p) { int ret;                                      \
                      if ((ret = p)) {                              \
                        printf ("Error:"#p":%s\n", strerror (ret)); \
                      }                                             \
                    }

// Macros 'CHKE' and 'CHKE_INFO'
//
// Error check for POSIX functions that modify "errno". Functions that
// return zero or a positive value if successful; otherwise, if an
// error occurs, they return the value -1.
#define CHKE(p) {if ((p)==-1) {perror (#p); exit (-1);}}
#define CHKE_INFO(p) {if ((p)==-1) {perror (#p);}}

// Macros 'ASSERT' AND 'ASSERT_INFO'.
//

// When the condition (c) is false the message (m) is displayed
#define ASSERT(c,m) {if (!(c)) \
                      {printf("ASSERT:("#c") failed. "m"\n"); exit (-1);}}
#define ASSERT_INFO(c,m) {if (!(c)) \
                           {printf("ASSERT:("#c") failed. "m"\n");}}


