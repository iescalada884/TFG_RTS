/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.51  Oct 2005
 *
 *                                  i 2 c
 *
 *                                    H
 *
 * File 'i2c.h'                                             By Sangorrin
 *
 *
 * This file is the C interface for the I2C daemon (see i2c.ads and i2c.adb).
 * You should add a 'With I2C;' code line to the file '/kernel/marte_os_c.ads'.
 *
 * There are some important aspects that you must take into account about
 * priorities. The file i2c.ads contains this important priority constants.
 * I2C clients should make sure that their priorities are lower or equal to
 * the internal protected object priorities.
 *
 * Philips I2C protocol manual
 * http://www.semiconductors.philips.com/acrobat/applicationnotes/AN10216_1.pdf
 *
 * how to use it --> test_i2c.c
 *
 *---------------------------------------------------------------------------*/
#ifndef __I2C_H__
#define __I2C_H__

#include <stdint.h>

typedef uint16_t i2c_address;

typedef uint16_t i2c_priority;

typedef uint16_t i2c_operation_id;
#define OP_1  1  /* used by cmps03 */
#define OP_2  2
#define OP_3  3
#define OP_4  4
#define OP_5  5
#define OP_6  6
#define OP_7  7
#define OP_8  8
#define OP_9  9
#define OP_10 10

typedef uint8_t i2c_operation_status;
#define NOT_IN_USE  1
#define WORKDONE  2
#define WAITING  3
#define I2C_ERROR  4

typedef uint16_t i2c_adapter_id;
#define ELITE  1
#define PARPORT  2
#define PCM3718 3

typedef uint16_t i2c_flags;
#define I2C_DEFAULT_FLAGS 0x00
#define I2C_M_RD 0x01
#define I2C_M_TEN 0x10
#define I2C_M_NOSTART 0x4000
#define I2C_M_REV_DIR_ADDR 0x2000
#define I2C_M_IGNORE_NAK 0x1000
#define I2C_M_NO_RD_ACK 0x0800

typedef uint32_t i2c_buffer;

typedef int i2c_data_count;

typedef struct {
   i2c_address	addr;
   i2c_flags flags;
   i2c_buffer buffer;
} i2c_c_msg;

extern void i2c_adainit(void);
extern void i2c_adafinal(void);

extern int flags_init (i2c_flags *flags);
extern int flags_set_tenbitaddress (i2c_flags *flags);
extern int flags_set_nostart (i2c_flags *flags);
extern int flags_set_revdiraddr (i2c_flags *flags);
extern int flags_set_ignorenack (i2c_flags *flags);
extern int flags_set_nordack (i2c_flags *flags);

extern i2c_buffer create_recv_buff (i2c_data_count num_bytes);

extern i2c_buffer create_send_buff (const char *data, i2c_data_count count);

extern void free_buffer (i2c_buffer buffer);

extern int i2c_msg_c_init (i2c_c_msg *msg,
                           i2c_address addr,
                           i2c_buffer buffer,
                           i2c_flags flags);

extern int i2c_transfer ( i2c_adapter_id adap,
                          i2c_operation_id op,
                          i2c_priority prio,
                          i2c_c_msg *msg_list,
                          int count);

extern int i2c_master_send ( i2c_adapter_id adap,
                             i2c_operation_id op,
                             i2c_priority prio,
                             i2c_address addr,
                             i2c_buffer buffer,
                             i2c_flags flags);

extern int i2c_master_recv ( i2c_adapter_id adap,
                             i2c_operation_id op,
                             i2c_priority prio,
                             i2c_address addr,
                             i2c_buffer buffer,
                             i2c_flags flags);

extern i2c_operation_status i2c_get_status (i2c_operation_id op);

extern i2c_operation_status i2c_wait (i2c_operation_id op);

extern i2c_operation_status read_buffer (char *data,
                                       i2c_data_count count,
                                       i2c_buffer buff,
                                       i2c_operation_id op);

#endif	/* __I2C_H__ */
