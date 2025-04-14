/**
 * linker.lds
 *
 * linker script to build MaRTE OS for the i386 architecture
 *
 * Author: Daniel Sangorrin <daniel.sangorrin@gmail.com>
 *
 */

OUTPUT_FORMAT("elf32-i386", "elf32-i386", "elf32-i386")
OUTPUT_ARCH(i386)
ENTRY (_start)
INPUT (multiboot.o)

SECTIONS
{
    . = 0x00100000;

    _smarte = .;

    .text :
    {
        _stext = .;
        multiboot.o(.text)
        *(.text)
        _etext = .;
    }

    .rodata ALIGN (0x1000) :
    {
        _srodata = .;
        * (.rodata)
        * (.rodata.*)
        * (.rodata.*.*)

        . = ALIGN(4);
        __CTOR_LIST__ = .;
        LONG((__CTOR_END__ - __CTOR_LIST__) / 4 - 2)
        *(.ctors)
        LONG(0)
        __CTOR_END__ = .;

        . = ALIGN(4);
        __DTOR_LIST__ = .;
        LONG(-1)
        * (.dtors)
        . = ALIGN(4);
        __DTOR_END__ = .;
        LONG(0)

        . = ALIGN (4);
        __JCR_LIST__ = .;
        * (.jcr)
        . = ALIGN (4);
        __JCR_END__ = .;
        LONG(0)

        . = ALIGN (4);
        __EH_FRAME_BEGIN__ = .;
        * (.eh_frame)
        . = ALIGN (4);
        __FRAME_END__ = .;
        LONG(0)

        . = ALIGN (4);
        * (.gcc_except_table)
        _erodata = .;
    }

    .data ALIGN (0x1000) :
    {
        _sdata = .;
        *(.data)
        _edata = .;
    }

    .bss :
    {
        _sbss = .;
        *(COMMON)
        *(.bss)
        _ebss = .;
    }

    _emarte = .;

   . = ALIGN(4K);

    _sfreemem = .;

    /DISCARD/ :
    {
        *(.note)
        *(.comment*)
    }
}
