/*!
 * @file tlsf_marte.c
 *
 * @brief init MaRTE OS dynamic_memory_pool
 *
 * @version 0.01
 *
 * @date 5-Dic-2007
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * In MaRTE OS there is only one big diynamic memory pool which is declared
 * and initialized here. init_dynamic_memory() is called at initialization
 * time.
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <sys/marte_configuration_parameters.h>

#ifdef USE_TLSF_MEMORY_ALLOCATOR
#if MARTE_ARCHITECTURE != ARCH_LINUX_LIB

#include "tlsf.h"
#include <stdio.h>

#if ((MARTE_ARCHITECTURE == ARCH_X86) && (DYNAMIC_MEMORY_POOL_SIZE_IN_K == 0))
#include <oskit/x86/multiboot.h>
extern struct multiboot_info boot_info;
extern char _sfreemem[], _smarte[];  // declared at the linker script
char *dynamic_memory_pool = NULL;
#else
char dynamic_memory_pool[DYNAMIC_MEMORY_POOL_SIZE_IN_K * 1024];
#endif

int init_dynamic_memory(void);
extern void direct_write_on_stdout (char * str, int count); // 'Kernel_Console'

int init_dynamic_memory(void)
{
        char msg[80];
        int chars;
        size_t dynamic_memory_pool_size;

#if ((MARTE_ARCHITECTURE == ARCH_X86) && (DYNAMIC_MEMORY_POOL_SIZE_IN_K == 0))
        dynamic_memory_pool = _sfreemem;
        dynamic_memory_pool_size = boot_info.mem_upper*1024 - (_sfreemem - _smarte);
#else
        dynamic_memory_pool_size = sizeof(dynamic_memory_pool);
#endif

        /* TLSF limitation to 1GB of memory */
        int limitated = 0;
        if (dynamic_memory_pool_size > (1 << 30) ) {
                dynamic_memory_pool_size = (1 << 30);
                limitated = 1;
        }
        chars =  snprintf(msg,
                          sizeof(msg),
                          "TLSF 2.3.2 dynamic memory pool: %u bytes %s\n",
                          dynamic_memory_pool_size,
                          limitated ? " [truncated]" : "" );

        direct_write_on_stdout(msg, (size_t)chars);
        return (int)init_memory_pool(dynamic_memory_pool_size,
                                     (void *)dynamic_memory_pool);
}

#endif // MARTE_ARCHITECTURE != ARCH_LINUX_LIB
#endif // USE_TLSF_MEMORY_ALLOCATOR
