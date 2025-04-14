/*!
 * @file lang_supp.c
 *
 * @brief language support functions and variables
 *
 * @version 0.02
 *
 * @date 12-Jan-2008
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

#include <lang_supp.h>
#include <sys/marte_configuration_parameters.h>

#if (MARTE_ARCHITECTURE == ARCH_X86)

//--------------//
// __dso_handle //
//--------------//

void *__dso_handle; /*only the address of this symbol is taken by gcc*/

// struct object
// {
//         void (*f)(void*);
//         void *p;
//         void *d;
// } object[32] = {[0 ... 31] = {0,0,0}};
// unsigned int iObject = 0;

struct fde_vector
{
   const void *orig_data;
   size_t count;
   const struct dwarf_fde *array[];
};

struct object
{
   void *pc_begin;
   void *tbase;
   void *dbase;
   union {
      const struct dwarf_fde *single;
      struct dwarf_fde **array;
      struct fde_vector *sort;
   } u;

   union {
      struct {
         unsigned long sorted : 1;
         unsigned long from_array : 1;
         unsigned long mixed_encoding : 1;
         unsigned long encoding : 8;
      /* ??? Wish there was an easy way to detect a 64-bit host here;
         we've got 32 bits left to play with...  */
         unsigned long count : 21;
      } b;
      size_t i;
   } s;

// #ifdef DWARF2_OBJECT_END_PTR_EXTENSION
//   char *fde_end;
// #endif

  struct object *next;
};


//--------------//
// __cxa_atexit //
//--------------//

int __cxa_atexit(void (*f)(void *), void *p, void *d)
{
//         if (iObject >= 32) return -1;
//         object[iObject].f = f;
//         object[iObject].p = p;
//         object[iObject].d = d;
//         ++iObject;
        return 0;
}

//----------------//
// __cxa_finalize //
//----------------//

void __cxa_finalize(void *d)
{
//         unsigned int i = iObject;
//         for (; i > 0; --i) {
//                 --iObject;
//                 object[iObject].f(object[iObject].p);
//         }
}

//-----------------------//
// __register_frame_info //
//-----------------------//

void __register_frame_info (const void *begin, struct object *ob)
                __attribute__ ((weak));

void __register_frame_info (const void *begin, struct object *ob) {
}

//-------------------------//
// __deregister_frame_info //
//-------------------------//

void *__deregister_frame_info (const void *begin)
                __attribute__ ((weak));

void *__deregister_frame_info (const void *begin) {
        return 0;
}

//-------------//
// frame_dummy //
//-------------//

extern int __FRAME_END__[];
extern char __EH_FRAME_BEGIN__[];

static void frame_dummy (void) {
   static struct object object;

   if (__register_frame_info)
      __register_frame_info (__EH_FRAME_BEGIN__, &object);
}

//----------------//
// init_lang_supp //
//----------------//

extern void (*__CTOR_LIST__)();

void init_lang_supp (void)
{
        int total;
        void (**constructor)() = &__CTOR_LIST__;

        frame_dummy ();
        total = *(int *)constructor;
        constructor++;
        while(total) {
                (*constructor)();
                total--;
                constructor++;
        }
}

//------------------//
// finish_lang_supp //
//------------------//

typedef void (*func_ptr) (void);
extern func_ptr __DTOR_LIST__[];

void finish_lang_supp (void) {
        func_ptr *p = __DTOR_LIST__ + 1;
        static _Bool completed = 0;
        func_ptr f;

        __cxa_finalize (0);

        if (__builtin_expect (completed, 0))
                return;

        while ((f = *p)) {
                p++;
                f ();
        }

        if (__deregister_frame_info)
                __deregister_frame_info (__EH_FRAME_BEGIN__);

        completed = 1;
}

//------------------//
// __stack_chk_fail //
//------------------//
// Called when gcc's -fstack-protector feature is used, and
// gcc detects corruption of the on-stack canary value

extern void panic(const char *fmt, ...);

void __stack_chk_fail(void)
{
    panic("stack-protector: Kernel stack is corrupted");
}

#endif
