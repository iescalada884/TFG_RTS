/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *      's i m p l e _ d y n a m i c _ m e m o r y _ f u n c t i o n s'
 *
 *                                      C
 *
 * File 'simple_dynamic_memory_functions.c'                           By MAR.
 *
 * Defines the array 'dynamic_memory_pool', function
 * 'init_dynamic_memory()' to be called during MaRTE OS initialization
 * and wrappers to the malloc(), calloc(), realloc() and free()
 * functions defined in Ada package 'Simple_Dynamic_Memory'.
 *
 *---------------------------------------------------------------------------*/
#include <sys/marte_configuration_parameters.h>

#ifndef USE_TLSF_MEMORY_ALLOCATOR
#if MARTE_ARCHITECTURE != ARCH_LINUX_LIB
#include <sys/types.h>

/*
 * Pool of dynamic memory
 */
char dynamic_memory_pool [DYNAMIC_MEMORY_POOL_SIZE_IN_K * 1024];

/*
 * Imports from Ada package 'Simple_Dynamic_Memory'
 */
extern void simple_dynamic_memory__initialize (char * pool);
extern void simple_dynamic_memory___elabb ();
extern void *simple_dynamic_memory__malloc (size_t size);
extern void *simple_dynamic_memory__calloc (size_t nelem, size_t elem_size);
extern void *simple_dynamic_memory__realloc (void *p, size_t new_len);
extern void simple_dynamic_memory__free (void *ptr);

/*
 * init_dynamic_memory
 */
int init_dynamic_memory ()
{
  simple_dynamic_memory___elabb ();
  simple_dynamic_memory__initialize (dynamic_memory_pool);
  return DYNAMIC_MEMORY_POOL_SIZE_IN_K * 1024;
}

/*
 * malloc
 */
void *malloc (size_t size)
{
  return simple_dynamic_memory__malloc (size);
}

/*
 * calloc
 */
void *calloc (size_t nelem, size_t elem_size)
{
  return simple_dynamic_memory__calloc (nelem, elem_size);
}

/*
 * realloc
 */
void *realloc (void *p, size_t new_len)
{
  return simple_dynamic_memory__realloc (p, new_len);
}

/*
 * free
 */
void free (void *ptr)
{
  simple_dynamic_memory__free (ptr);
}

#endif // MARTE_ARCHITECTURE != ARCH_LINUX_LIB
#endif // not USE_TLSF_MEMORY_ALLOCATOR
