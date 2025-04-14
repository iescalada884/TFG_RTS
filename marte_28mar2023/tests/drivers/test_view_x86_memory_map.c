// Test for: x86
/*!
 * @file test_view_x86_memory_map.c
 *
 * @brief Simple test to check the memory map in x86
 *
 * @version 0.01
 *
 * @date 21 - Oct -2008
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * Undefine AUTOMATIC to do the test step by step.
 * TODO: improve boot to get rid of oskit headers
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <oskit/x86/multiboot.h>
#include <sys/marte_configuration_parameters.h>

extern struct multiboot_info boot_info;

extern unsigned long _smarte[], _emarte[], _stext[], _etext[], _sdata[], _edata[],
  _srodata[], _erodata[], _sbss[], _ebss[];
extern unsigned long _sfreemem;

/* The memory map. Be careful that the offset 0 is base_addr_low but no size. */
typedef struct memory_map {
        unsigned long size;
        unsigned long base_addr_low;
        unsigned long base_addr_high;
        unsigned long length_low;
        unsigned long length_high;
        unsigned long type;
} memory_map_t;

#define CHECK_FLAG(flags,bit)   ((flags) & (1 << (bit)))

#define AUTOMATIC

#ifdef AUTOMATIC
#include <drivers/console_switcher.h>
#endif

void pause(){
#ifdef AUTOMATIC
        return;
#else
        char character;
        printf("press...");
        character = getchar();
#endif
}

int main()
{
#ifdef AUTOMATIC
        SERIAL_CONSOLE_INIT();
#endif
        printf("Print out the flags. "); pause();
        printf ("flags = 0x%x\n", (unsigned) boot_info.flags);

        printf("Are mem_* valid? "); pause();
        if (CHECK_FLAG (boot_info.flags, 0))
                printf ("mem_lower = %uKB, mem_upper = %uMB\n",
                        (unsigned) boot_info.mem_lower,
                        (unsigned) boot_info.mem_upper/1024);

        printf("Is boot_device valid? "); pause();
        if (CHECK_FLAG (boot_info.flags, 1))
                printf ("boot_device = 0x%x\n",
                        (unsigned) boot_info.boot_device);

        printf("Is the command line passed? "); pause();
        if (CHECK_FLAG (boot_info.flags, 2))
                printf ("cmdline = %s\n", (char *) boot_info.cmdline);

        printf("Are mods_* valid? "); pause();
        if (CHECK_FLAG (boot_info.flags, 3))
        {
                struct multiboot_module *mod;
                int i;

                printf ("mods_count = %d, mods_addr = 0x%x\n",
                        (int) boot_info.mods_count, (int) boot_info.mods_addr);
                for (i = 0, mod = (struct multiboot_module *) boot_info.mods_addr;
                     i < boot_info.mods_count;
                     i++, mod++)
                        printf (" mod_start = 0x%x, mod_end = 0x%x, string = %s\n",
                                (unsigned) mod->mod_start,
                                (unsigned) mod->mod_end,
                                (char *) mod->string);
        }

        printf("Bits 4 and 5 are mutually exclusive! "); pause();
        if (CHECK_FLAG (boot_info.flags, 4) && CHECK_FLAG (boot_info.flags, 5))
        {
                printf ("Both bits 4 and 5 are set.\n");
                exit(-1);
        }

        printf("Is the section header table of ELF valid? "); pause();
        if (CHECK_FLAG (boot_info.flags, 5))
        {
                printf ("elf_sec: num = %u, size = 0x%x,"
                                " addr = 0x%x, shndx = 0x%x\n",
                (unsigned) boot_info.syms.e.num, (unsigned) boot_info.syms.e.size,
                (unsigned) boot_info.syms.e.addr, (unsigned) boot_info.syms.e.shndx);
        }

        printf("Are mmap_* valid? "); pause();
        if (CHECK_FLAG (boot_info.flags, 6))
        {
                memory_map_t *mmap;

                printf ("mmap_addr = 0x%x, mmap_length = 0x%x\n",
                        (unsigned) boot_info.mmap_addr, (unsigned) boot_info.mmap_count);
                for (mmap = (memory_map_t *) boot_info.mmap_addr;
                     (unsigned long) mmap < boot_info.mmap_addr + boot_info.mmap_count;
                     mmap = (memory_map_t *) ((unsigned long) mmap
                                     + mmap->size + sizeof (mmap->size)))
                        printf (" size = 0x%x, base_addr = 0x%x%x,"
                                     " length = 0x%x%x, type = 0x%x\n",
                     (unsigned) mmap->size,
                     (unsigned) mmap->base_addr_high,
                     (unsigned) mmap->base_addr_low,
                     (unsigned) mmap->length_high,
                     (unsigned) mmap->length_low,
                     (unsigned) mmap->type);
        }

        pause();

        unsigned long sysmem;

        sysmem = 0x100000 + boot_info.mem_upper * 1024;

        printf("\nTotal Memory: %lu MB\n\n", (unsigned long) sysmem/1024/1024);

        if (CHECK_FLAG (boot_info.flags, 0))
                printf ("mem_lower = %uKB, mem_upper = %uMB\n",
                        (unsigned) boot_info.mem_lower,
                        (unsigned) boot_info.mem_upper/1024);

        printf ("MaRTE OS (%lu MB): ",
                ((unsigned long)_emarte - (unsigned long)_smarte)/1024/1024);
        printf ("[.text=%lu Kb ",
                ((unsigned long)_etext - (unsigned long)_stext)/1024);
        printf (".data=%lu Kb ",
                ((unsigned long)_edata - (unsigned long)_sdata)/1024);
        printf (".rodata=%lu Kb ",
                ((unsigned long)_erodata - (unsigned long)_srodata)/1024);
        printf (".bss=%lu Kb]\n",
                ((unsigned long)_ebss - (unsigned long)_sbss)/1024);
        printf ("Free memory starts at: %lu\n", (unsigned long) &_sfreemem);

        if (CHECK_FLAG (boot_info.flags, 6))
        {
                memory_map_t *mmap;

                printf ("mmap_addr = 0x%x, mmap_length = 0x%x\n",
                        (unsigned) boot_info.mmap_addr,
                        (unsigned) boot_info.mmap_count);

                for (mmap = (memory_map_t *) boot_info.mmap_addr;
                     (unsigned long) mmap < boot_info.mmap_addr + boot_info.mmap_count;
                     mmap = (memory_map_t *) ((unsigned long) mmap
                                     + mmap->size + sizeof (mmap->size)))
                        printf (" size = 0x%x, base_addr = 0x%x%x,"
                                     " length = 0x%x%x, type = 0x%x\n",
                     (unsigned) mmap->size,
                     (unsigned) mmap->base_addr_high,
                     (unsigned) mmap->base_addr_low,
                     (unsigned) mmap->length_high,
                     (unsigned) mmap->length_low,
                     (unsigned) mmap->type);
        }

        printf("Test OK\n");
        return 0;
}
