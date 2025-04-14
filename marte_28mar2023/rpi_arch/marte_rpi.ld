/******************************************************************************
*	kernel.ld
*	 by Alex Chadwick
*
*	A linker script for generation of raspberry pi kernel images, with C
*	code.
******************************************************************************/

ENTRY(_start)

SECTIONS {
	/*
	* First and formost we need the .init section, containing the code to 
        * be run first. We allow room for the ATAGs and stack and conform to 
        * the bootloader's expectation by putting this code at 0x8000.
	*/
	. = 0x200;
	. += 0x400;
	_irq_stack = .;
	_sp_init =.;
	. = 0x8000;
	_sp_end = .;

	.init : {
		*(.init)
	}
	
	/* 
	* Next we put the data.
	*/
	.data : {
		*(.data)
	}
	
	/* 
	* Next we put the rest of the code.
	*/
	.text : {
		*(.text)
	}

    .ARM.extab   : {
        *(.ARM.extab* .gnu.linkonce.armextab.*)
    }

    .ARM : {
        __exidx_start = .;
        *(.ARM.exidx*)
        __exidx_end = .;
    }

	.rodata : {
		*(.rodata)
	        * (.rodata.*)
	        * (.rodata.*.*)

	}

	.bss : {
		PROVIDE (_bss_start = .);
		*(.bss);
		*(COMMON);
		PROVIDE (_bss_end = .);
	}

	/*
	* Finally comes everything else. A fun trick here is to put all other 
	* sections into this section, which will be discarded by default.
	*
	/DISCARD/ : {
		*(*)
	}*/
}
