/*
 * MaRTE OS
 * Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
 *
 * This file has been generated automatically by 'mkmarte'
 * using constants defined in the Ada part of the kernel.
 *
 * Do Not Edit.
 */

#ifndef _MARTE_SYS_MARTE_CONFIGURATION_PARAMETERS_H_
#define _MARTE_SYS_MARTE_CONFIGURATION_PARAMETERS_H_

// Supported Architectures
#define ARCH_X86            0
#define ARCH_LINUX          1
#define ARCH_LINUX_LIB      2
#define ARCH_XTRATUM        3
#define ARCH_RPI            4
#define ARCH_GNAT_BB_ARM    5
// Current Architecture
#define MARTE_ARCHITECTURE   ARCH_GNAT_BB_ARM

// Main task's stack size (only used in x86 architecture)
#define _MARTE_CONFIG_MAIN_TASK_STACK_SIZE_IN_BYTES       32000

// Dinamic Memory
//extern char dynamic_memory_pool[]; <--- remove??
#define DYNAMIC_MEMORY_POOL_SIZE_IN_K           1
#define USE_TLSF_MEMORY_ALLOCATOR
#undef TLSF_DEBUG
#define TLSF_MAX_SL_LOG2_INDEX           5

// SIGQUEUE MAX
#define MARTE_SIGQUEUE_MAX          32

// Devices filesystem
#undef USE_DEVICES_FILESYSTEM
#define OPEN_FILES_MX              3
#define DEVICES_FILES_MX           3
#define DEVICES_MX                 3
#define MINOR_NUMBER_MX          255
#define PATH_MX                   16


#endif /* _MARTE_SYS_MARTE_CONFIGURATION_PARAMETERS_H_ */
