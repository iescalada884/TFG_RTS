/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                   's e r i a l _ p o r t _ d r i v e r _ c'
 *
 *                                      C
 *
 *  File 'serial_port_driver_c.c'                           by Fguerreira,
 *                                                             MAR and
 *                                                             MGH.
 *
 *  I/O functions used by the serial port. Pooling mode and interrupt
 *  modes supported.
 *
 *  The size of driver buffers can be changed in
 *  'serial_port_driver_import.ads'. (by default they are set to 100 bytes)
 *
 *  The interrupt number assign to each serial is set in the array
 *  'IRQ_number' in this file.
 *
 *
 *  If this driver is going to be used as stdout and/or stderr devices
 *  '/dev/stdout' and '/dev/stderr' ('kernel-devices_table.ads') must
 *  have assigned 0 as their minor numbers. Otherwise value of
 *  CONSOLE_SERIAL_PORT should be changed.
 *
 *  Thanks a lot to the University of Zaragoza (Danilo Tardioli and
 *  Francisco Feijoo) for their input correcting some bugs in the driver.
 *
 * Based on code taken from the "Flux OSKit"
 * Copyright (c) 1996, 1998 University of Utah and the Flux Group.
 * All rights reserved.
 *
 * This file is part of the Flux OSKit.  The OSKit is free software, also known
 * as "open source;" you can redistribute it and/or modify it under the terms
 * of the GNU General Public License (GPL), version 2, as published by the Free
 * Software Foundation (FSF).  To explore alternate licensing terms, contact
 * the University of Utah at csl-dist@cs.utah.edu or +1-801-585-3271.
 *
 * The OSKit is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GPL for more details.  You should have
 * received a copy of the GPL along with the OSKit; see the file COPYING.  If
 * not, write to the FSF, 59 Temple Place #330, Boston, MA 02111-1307, USA.
 *
 *
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *   MaRTE OS web page: http://marte.unican.es
 *   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                      Michael Gonzalez Harbour      mgh@unican.es
 *
 *  MaRTE OS  is free software; you can  redistribute it and/or  modify it
 *  under the terms of the GNU General Public License  as published by the
 *  Free Software Foundation;  either  version 2, or (at  your option) any
 *  later version.
 *
 *  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 *  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 *  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 *  General Public License for more details.
 *
 *  You should have received  a  copy of  the  GNU General Public  License
 *  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 *  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 *  02111-1307, USA.
 *
 *  As a  special exception, if you  link this  unit  with other  files to
 *  produce an   executable,   this unit  does  not  by  itself cause  the
 *  resulting executable to be covered by the  GNU General Public License.
 *  This exception does  not however invalidate  any other reasons why the
 *  executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/


#include <drivers/serial_port_driver.h>
#include <drivers/drivers_marte.h>  // For 'get_minor'
#include <drivers/ring_buffers.h>

#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/pio.h>
#include <intr.h>

#include <misc/error_checks.h>


 /* Base directions for each port */
static const int ser_io_base[4] = {0x3F8, 0x2F8, 0x3E8, 0x2E8};
#define TX  0 // Transmision register      (DLAB = 0)
#define RX  0 // Reception register        (DLAB = 0)
#define DLL 0 // LSB latch divisor         (DLAB = 1)
#define DLM 1 // MSB latch divisor         (DLAB = 1)
#define IER 1 // Interrupts enable/disable (DLAB = 0)
#define IIR 2 // Interrupts identification
#define LCR 3 // Line control (set/reset DLAB)
#define MCR 4 // Modem control
#define LSR 5 // Line status
#define MSR 6 // Modem status

//#define DBG(p) p;
#define DBG(p) {}

#define NUM_OF_SERIAL_PORTS 4


/* IRQ numbers for each port */
static const int IRQ_number[NUM_OF_SERIAL_PORTS] =
  {SERIAL1_HWINTERRUPT, SERIAL2_HWINTERRUPT,
   SERIAL1_HWINTERRUPT, SERIAL2_HWINTERRUPT};

/* Configuration parameters for each port */
static termios_t ser_termios[NUM_OF_SERIAL_PORTS];
static struct {
  char interrupts_enabled;
  unsigned open_count;
  unsigned char last_line_error;
  struct timespec *rel_timeout_ptr;
  struct timespec rel_timeout;
} port_config[NUM_OF_SERIAL_PORTS];
static ring_input_buffer_id_t  inbuf[NUM_OF_SERIAL_PORTS];
static ring_output_buffer_id_t outbuf[NUM_OF_SERIAL_PORTS];

// Information for the interrup handler
//
// The same function is used as interrupt handler for the four
// ports. The parameter 'intr' can not be used to know the port
// associated with the handler, since two ports can use the same
// IRQ. For that reason we use the 'area' parameter.
volatile static const int handler_area_info[NUM_OF_SERIAL_PORTS] =
  {0, 1, 2, 3};

/* Forward functions declarations */
static void set_port_attr (int port, termios_t *com_params);
static ssize_t basic_write (unsigned int port, void *buffer, size_t bytes,
			    int blocking_mode);
int serial_port_ioctl (int file_descriptor, int request, void* argp);


// Basic default termios settings for a typical raw-mode console.
// (8 bits, No Parity bit, 1 Stop bit)
termios_t raw_termios =
  {
    0,             /* input flags */
    0,             /* output flags */
    CS8,           /* control flags */
    0,             /* local flags */
    {
      'D'-64,                   /* VEOF */
      VDISABLE,                 /* VEOL */
      0,
      'H'-64,                   /* VERASE */
      0,
      'U'-64,                   /* VKILL */
      0,
      0,
      'C'-64,                   /* VINTR */
      '\\'-64,                  /* VQUIT */
      'Z'-64,                   /* VSUSP */
      0,
      'Q'-64,                   /* VSTART */
      'S'-64,                   /* VSTOP */
    },
    B9600,         /* input speed */
    B9600,         /* output speed */
  };



/* Basic default termios settings for a typical cooked-mode console. */

termios_t cooked_termios =
  {
    ICRNL | IXON,                           /* input flags */
    OPOST,                                  /* output flags */
    CS8,                                    /* control flags */
    ECHO | ECHOE | ECHOK | ICANON,          /* local flags */
    {
      'D'-64,                   /* VEOF */
      VDISABLE,                 /* VEOL */
      0,
      'H'-64,                   /* VERASE */
      0,
      'U'-64,                   /* VKILL */
      0,
      0,
      'C'-64,                   /* VINTR */
      '\\'-64,                  /* VQUIT */
      'Z'-64,                   /* VSUSP */
      0,
      'Q'-64,                   /* VSTART */
      'S'-64,                   /* VSTOP */
    },
    B9600,         /* input speed */
    B9600,         /* output speed */
  };
/*******************
 * send_first_byte *
 *******************/
void send_first_byte_0 (char c)
{
  DBG( printc(" first0:0x%x(%c) ", (int)c, c) );
  outb_p(ser_io_base[0] + TX, c);
}
void send_first_byte_1 (char c)
{
  DBG( printc(" first1:0x%x(%c) ", (int)c, c) );
  outb_p(ser_io_base[1] + TX, c);
}
void send_first_byte_2 (char c)
{
  DBG( printc(" first2:0x%x(%c) ", (int)c, c) );
  outb_p(ser_io_base[2] + TX, c);
}
void send_first_byte_3 (char c)
{
  DBG( printc(" first3:0x%x(%c) ", (int)c, c) );
  outb_p(ser_io_base[3] + TX, c);
}
void *send_first_byte[NUM_OF_SERIAL_PORTS] =
  {send_first_byte_0, send_first_byte_1,
   send_first_byte_2, send_first_byte_3};


/******************************/
/*  serial_interrupt_handler  */
/******************************/
#define PORT(area) (*(int *)area)
int serial_interrupt_handler(void * area, intr_t intr)
{
    unsigned char valor;
    int idx;
    struct timespec ts;

    DBG( printc(" int") );
    //DBG(printc("(LSR=%x)", (unsigned) inb_p(ser_io_base[PORT(area)] + LSR)));
    //DBG(printc("(MSR=%x)", (unsigned) inb_p(ser_io_base[PORT(area)] + MSR)));
    switch (inb_p(ser_io_base[PORT(area)] + IIR) & 0x07) {
        case 0x06: // IDI1=1, IDI0=1, IP=0: Line status error
            port_config[PORT(area)].last_line_error =
            inb_p(ser_io_base[PORT(area)] + LSR) & 0x1E;
            DBG( printe("Serial driver: Line status error %x in port %d\n",
                (int)port_config[PORT(area)].last_line_error, PORT(area)) );
            break;

        case 0x04: // IDI1=1, IDI0=0, IP=0: Data in reception register
            DBG( printc("RX") );
            // Read a byte from the serial port
            put_in_input_buffer
                (inbuf[PORT(area)], inb_p(ser_io_base[PORT(area)] + RX), 0);

            // While we are attending this interrupt request, other bytes could
            // have arrived and stored in the buffer of the chip. We read the
            // LSR to check if there are more bytes to read
            valor= (unsigned) inb_p(ser_io_base[PORT(area)] + LSR);
            valor &=1;
            while (valor) {
                valor=inb_p(ser_io_base[PORT(area)] + RX);
                put_in_input_buffer(inbuf[PORT(area)], valor, 0);
                valor=(unsigned) inb_p(ser_io_base[PORT(area)] + LSR);
                valor &=1;
            }
            break;

        case 0x02: //  IDI1=0, IDI0=1, IP=0: transmision buffer free
        {
            char c;
            int empty;

            // First, we check if there are bytes in the buffer
            valor= (unsigned) inb_p(ser_io_base[PORT(area)] + LSR);
            valor &=1;
            while (valor){
                put_in_input_buffer
                    (inbuf[PORT(area)], inb_p(ser_io_base[PORT(area)] + RX), 0);
                valor= (unsigned) inb_p(ser_io_base[PORT(area)] + LSR);
                valor &=1;
            }

            // and now attend the case 0x02 (transmision buffer is free)
            get_from_output_buffer(outbuf[PORT(area)], &c, &empty);
            if (!empty) {
                DBG( printc("TX:0x%x(%c) ", (int)c, c) );
                outb_p(ser_io_base[PORT(area)] + TX, c);
            } else {
                DBG( printc(" rst ") );
                reset_last_interrupt_pending(outbuf[PORT(area)]);
            }
            break;
        }

        case 0x00: //  IDI1=0, IDI0=0, IP=0: modem status change
            inb_p(ser_io_base[PORT(area)] + MSR); // reset interrupt
            // This interrupt shouldn't be enabled
            ASSERT_INFO(0,"Serial port: Unexpected interrupt cause");
            return POSIX_INTR_NOT_HANDLED;
            break;

        case 0x01: //  IDI1=0, IDI0=0, IP=1: no pending interrupt for this port
            DBG( printc(" not_hndld ") );
            return POSIX_INTR_NOT_HANDLED;
            break;

        default:
            ASSERT_INFO(0,"Serial port error");
            return POSIX_INTR_NOT_HANDLED;
    }

    return POSIX_INTR_HANDLED_DO_NOT_NOTIFY;
}

/******************************/
/*  serial_port_init_console  */
/******************************/
// Configure the serial port to be used from the very beggining of
// excution (before 'open' has been invoked). This is necessary when
// serial port is going to be used as stdout and/or stderr.
#define CONSOLE_SERIAL_PORT 0 // In 'k-devices_table.ads',
	       // devices '/dev/stdout' and '/dev/stderr' must have
	       // assigned 0 as their minor numbers.
int serial_port_init_console ()
{
  static int initialized = 0;
  if (!initialized) {
    // default 'port_config' setup (no interrupts)
    port_config[CONSOLE_SERIAL_PORT].open_count = 0;
    port_config[CONSOLE_SERIAL_PORT].interrupts_enabled = 0;
    port_config[CONSOLE_SERIAL_PORT].last_line_error = 0;

    // Port is opened with RAW-MODE (8N1)
    // (8N1) -> (8 bits, No Parity bit, 1 Stop bit)
    ser_termios [CONSOLE_SERIAL_PORT] = raw_termios;
    // ispeed = ospeed = B115200
    ser_termios [CONSOLE_SERIAL_PORT].ispeed = B115200;
    ser_termios [CONSOLE_SERIAL_PORT].ospeed = B115200;
    // set port attributes
    set_port_attr (CONSOLE_SERIAL_PORT, &ser_termios [CONSOLE_SERIAL_PORT]);
    initialized = 1;
  }
  return 0;
}

/*******************************/
/*  serial_port_console_write  */
/*******************************/
// Used when serial port is going to be used as stdout and/or
// stderr.
ssize_t serial_port_console_write (int file_descriptor, void *buffer,
				   size_t bytes)
{
  return basic_write (CONSOLE_SERIAL_PORT, buffer, bytes, 0);
}

/************/
/*  Create  */
/************/
int serial_port_create ()
{
  int p;
  // assign ring buffers used to share data between the interrupt
  // handler and the read and write functions.

  extern int com1_inbuf, com2_inbuf,
    com3_inbuf, com4_inbuf;   // Created in 'serial_port_driver.ads'
  extern int com1_outbuf, com2_outbuf,
    com3_outbuf, com4_outbuf; // Created in 'serial_port_driver.ads'

  inbuf[0] = &com1_inbuf;  inbuf[1] = &com2_inbuf;
  inbuf[2] = &com3_inbuf;  inbuf[3] = &com4_inbuf;

  outbuf[0] = &com1_outbuf;  outbuf[1] = &com2_outbuf;
  outbuf[2] = &com3_outbuf;  outbuf[3] = &com4_outbuf;

  for (p=0; p < NUM_OF_SERIAL_PORTS; p++) {
    port_config[p].open_count = 0;
    port_config[p].rel_timeout_ptr=NULL;
  }
  return 0;
}


/************/
/*  Remove  */
/************/
/* Not necessary
int serial_port_remove ()
{
  return 0;
}*/



/**********/
/*  Open  */
/**********/

int serial_port_open (int file_descriptor, int file_access_mode)
{
  int port;

  port = get_minor (file_descriptor);

  if (port_config[port].open_count == 0) {

    // Initialize port ring buffers to share data between the interrupt
    // handler and the read and write functions
    init_input_buffer(inbuf[port]);
    init_output_buffer(outbuf[port]);

    // Install interrupt handler
    CHK( posix_intr_associate(IRQ_number[port],
			      serial_interrupt_handler,
			      (void *)&handler_area_info[port],
			      sizeof(handler_area_info[port])) );

    // Unlock interrupt
    CHK( posix_intr_unlock (IRQ_number[port]) );

    /* Port is opened with RAW-MODE (8N1) B9600 as default */
    /* (8N1) -> (8 bits, No Parity bit, 1 Stop bit) */
    // interrupts enabled: they are enabled by serial_port_ioctl()
    // (actually by set_port_attr())
    ser_termios [port] = raw_termios;
    port_config[port].interrupts_enabled = 1;
    serial_port_ioctl (file_descriptor, SERIAL_SETATTR,
		       ((void *)&ser_termios [port]));

    // Init 'last_line_error'
    port_config[port].last_line_error = 0;
  }
  port_config[port].open_count++;

  return 0;
}



/***********/
/*  Close  */
/***********/
/* Not necessary
int serial_port_close (int file_descriptor)
{
  return 0;
}*/




/**********/
/*  Read  */
/**********/

ssize_t serial_port_read (int file_descriptor, void *buffer, size_t bytes)
{
  int port = get_minor (file_descriptor);

  if (port_config[port].interrupts_enabled) { // interrupts enabled

    return read_from_input_buffer
      (inbuf[port], buffer, bytes,
       (get_fd_access_mode(file_descriptor) & O_NONBLOCK) == 0,
       port_config[port].rel_timeout_ptr);


  } else { // Interrupts disabled
    int i, byte, bytes_read = 0;
    unsigned char mode;
    for(i=0; i<bytes; i++)
      {
	/* Wait for a character to arrive.  */
	for (;;)
	  {
	    /* Grab it.  */
	    mode = inb_p(ser_io_base[port] + 3);
	    outb_p(ser_io_base[port] + 3, 0x7F & mode);     /* DLAB = 0 */
	    if (inb_p(ser_io_base[port] + 5) & 0x01)
	      {
		byte = inb_p(ser_io_base[port] + 0);
		break;
	      }
	  }
      }

      ((char*)buffer)[i] = byte;
      bytes_read++;

      return bytes_read;
    }
}



/***********/
/*  Write  */
/***********/

ssize_t serial_port_write (int file_descriptor, void *buffer, size_t bytes)
{
  return basic_write (get_minor (file_descriptor), buffer, bytes,
		      (get_fd_access_mode(file_descriptor) & O_NONBLOCK) == 0);
}


/*****************/
/*  Basic Write  */
/*****************/

static ssize_t basic_write (unsigned int port, void *buffer, size_t bytes,
			    int blocking_mode)
{
  unsigned int i, bytes_written = 0;
  unsigned char mode;

  if (port_config[port].interrupts_enabled) { // interrupts enabled

    return write_to_output_buffer(outbuf[port], buffer, bytes,
				  blocking_mode,
				  send_first_byte[port]);

  } else { // Interrupts disabled,
    for(i=0; i<bytes; i++) {
      if (ser_termios[port].oflag & OPOST) {
	if (((char *)buffer)[i] == '\n') {
	  /* Wait for the transmit buffer to become available.  */
	  while (!(inb_p(ser_io_base[port] + 5) & 0x20));
	  outb_p(ser_io_base[port] + 0, '\r');
	}
      }
      mode = inb_p(ser_io_base[port] + 3);
      outb_p(ser_io_base[port] + 3, 0x7F & mode);           /* DLAB = 0 */
      /* Wait for the transmit buffer to become available.  */
      while (!(inb_p(ser_io_base[port] + 5) & 0x20));
      outb_p(ser_io_base[port] + 0, ((char *)buffer)[i]);

      bytes_written++;
    }
  }

  return bytes_written;
}


/***********/
/*  Ioctl  */
/***********/

int serial_port_ioctl (int file_descriptor, int request, void* argp)
{
  int port;
  unsigned char mode;
  unsigned freq_divisor;
  termios_t *com_params = (termios_t *) argp;

  port = get_minor (file_descriptor);

  switch (request)
    {
    case SERIAL_SETATTR:
      set_port_attr (port, com_params);
      break;


    case SERIAL_GETATTR:
      /* Clear struct data to get the parameters */
      com_params->iflag = 0x00000000;
      com_params->oflag = 0x00000000;
      com_params->cflag = 0x00000000;
      com_params->lflag = 0x00000000;
      com_params->ispeed = 0;
      com_params->ospeed = 0;

      mode = inb_p(ser_io_base[port] + 3);  /* reading the Line Control
					     * Register (LCR) */

      switch (mode & 0x03)      /* Length of the word  (WLS1 - WLS0) */
	{
	case 0x00: com_params->cflag |= CS5; break;
	case 0x01: com_params->cflag |= CS6; break;
	case 0x02: com_params->cflag |= CS7; break;
	case 0x03: com_params->cflag |= CS8; break;
	}

      if (mode & 0x08)    /* Parity bit enable  (PEN) */
	{
	  com_params->cflag |= PARENB;

	  if (!(mode & 0x10))    /* Odd parity (PEN) */
	    com_params->cflag |= PARODD;
	}


      if (mode & 0x04)    /* Stop bits number  (STB) */
	com_params->cflag |= CSTOPB;

      /* DLAB = 1 */
      outb_p(ser_io_base[port] + 3, 0x80 | mode);
      freq_divisor =
	inb_p(ser_io_base[port] + 1) << 8 | inb_p(ser_io_base[port] + 0);
         /* MSB latch divisor */            /* LSB latch divisor */
      outb_p(ser_io_base[port] + 3, 0x7F & mode);    /* DLAB = 0 */

      /* Convert the divisor latch value into a baud rate. */
      com_params->ispeed = 115200 / freq_divisor;
      com_params->ospeed = 115200 / freq_divisor;
      break;


    case SERIAL_SETSPEED:
      /* Convert the baud rate into a divisor latch value.  */
      freq_divisor = 115200 / com_params->ospeed;

      /* Initialize the serial port.  */
      mode = inb_p(ser_io_base[port] + 3);  /* reading the Line Control
					     * Register (LCR) */
      outb_p(ser_io_base[port] + 3, 0x80 | mode);           /* DLAB = 1 */
      outb_p(ser_io_base[port] + 0, freq_divisor & 0x00FF); /* LSB latch
							     * divisor */
      outb_p(ser_io_base[port] + 1, freq_divisor >> 8); /* MSB latch divisor */
      outb_p(ser_io_base[port] + 3, 0x7F & mode);    /* DLAB = 0 */

      break;


    case SERIAL_GETSPEED:
      mode = inb_p(ser_io_base[port] + 3);  /* reading the Line Control
					     * Register (LCR) */
      outb_p(ser_io_base[port] + 3, 0x80 | mode);    /* DLAB = 1 */
      freq_divisor =
	inb_p(ser_io_base[port] + 1) << 8 | inb_p(ser_io_base[port] + 0);
         /* MSB latch divisor */            /* LSB latch divisor */
      outb_p(ser_io_base[port] + 3, 0x7F & mode);    /* DLAB = 0 */

      /* Convert the divisor latch value into a baud rate. */
      com_params->ispeed = 115200 / freq_divisor;
      com_params->ospeed = 115200 / freq_divisor;
      break;


    case SERIAL_ENABLE_INTERRUPTS:
      mode = inb_p(ser_io_base[port] + LCR);
      outb_p(ser_io_base[port] + LCR, 0x7F & mode);  /* DLAB = 0 */
      outb_p(ser_io_base[port] + IER, 0x0F); /* enable interrupts */
      port_config[port].interrupts_enabled = 1;
      break;

    case SERIAL_DISABLE_INTERRUPTS:
      mode = inb_p(ser_io_base[port] + LCR);
      outb_p(ser_io_base[port] + LCR, 0x7F & mode);  /* DLAB = 0 */
      outb_p(ser_io_base[port] + IER, 0x00);         /* disable interrupts */
      port_config[port].interrupts_enabled = 0;
      break;

    case SERIAL_GET_LAST_LINE_ERROR:
      *(unsigned char *)argp = port_config[port].last_line_error;
      break;

    case SERIAL_RESET_LAST_LINE_ERROR:
      port_config[port].last_line_error = 0;
      break;

    case SERIAL_FLUSH:
      /*
       * Line Status Register (LSR)  :
       * bit #6 is 'HIGH' if there is no byte in hold register or shift
       * register
       */
      while (!(inb_p(ser_io_base[port] + 5) & 0x40));
      break;

    case SERIAL_SET_REL_TIMEOUT:
      port_config[port].rel_timeout=*((struct timespec *)argp);
      port_config[port].rel_timeout_ptr=&port_config[port].rel_timeout;
      break;


    default:
      return -1;
      break;

    }

  return 0;
}

/*******************/
/*  set_port_attr  */
/*******************/
static void set_port_attr (int port, termios_t *com_params)
{
  unsigned char mode;
  unsigned freq_divisor;
  /* Determine what to plug in the data format register.  */
  if (com_params->cflag & PARENB)
    {
      if (com_params->cflag & PARODD)
	mode = 0x08;
      else
	mode = 0x18;
    }
  else
    mode = 0x00;

  if (com_params->cflag & CSTOPB)
    mode |= 0x04;

  switch (com_params->cflag & 0x00000300)
    {
    case CS5: mode |= 0x00; break;
    case CS6: mode |= 0x01; break;
    case CS7: mode |= 0x02; break;
    case CS8: mode |= 0x03; break;
    }

  /* Convert the baud rate into a divisor latch value.  */
  freq_divisor = 115200 / com_params->ospeed;

  /* Initialize the serial port.  */
  outb_p(ser_io_base[port] + LCR, 0x80 | mode);           /* DLAB = 1 */
  outb_p(ser_io_base[port] + 0, freq_divisor & 0x00FF); /* LSB latch divisor */
  outb_p(ser_io_base[port] + 1, freq_divisor >> 8); /* MSB latch divisor */

  outb_p(ser_io_base[port] + LCR, 0x7F & mode);    /* DLAB = 0 */
  if (port_config[port].interrupts_enabled)
    outb_p(ser_io_base[port] + IER, 0x0F); /* enable interrupts */
  else
    outb_p(ser_io_base[port] + IER, 0x00);     /* disable interrupts */
  outb_p(ser_io_base[port] + 4, 0x0B);     /* OUT2, RTS, and DTR enabled */

  /* make sure the FIFO is on */
  outb_p(ser_io_base[port] + 2, 0x41); /* 4 byte trigger (0x40); on (0x01) */

  /* Clear all serial interrupts.  */
  inb_p(ser_io_base[port] + 6);   /* ID 0: read RS-232 status register */
  inb_p(ser_io_base[port] + 2);   /* ID 1: read interrupt identification reg */
  inb_p(ser_io_base[port] + 0);   /* ID 2: read receive buffer register */
  inb_p(ser_io_base[port] + 5);   /* ID 3: read serialization status reg */
}
