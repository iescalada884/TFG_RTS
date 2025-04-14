/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.51  Oct 2005
 *
 *                                 'c m p s 0 3 _ c'
 *
 *                                      C
 *
 *  File 'cmps03_c.c'                                  by Sangorrin
 *
 *  This is the driver to control the CMPS03 Compass through the
 *  I2C protocol. Applications should not use this functions directly.
 *
 *  In order to be able to use the driver you should first install it by
 *  filling the corresponding fields on kernel/kernel-devices_table.ads
 *  with the imported functions in cmps03_functions.ads (see marte_ug.html)
 *
 *  How to use it --> See test_cmps03.c [.adb]
 *
 *---------------------------------------------------------------------------*/
//#include <stdio.h> /*PRUEBAS*/
#include <stdlib.h> /*For malloc*/
#include <stdint.h>
#include <sys/i2c.h> /*For Interfacing the I2C daemon*/
#include <drivers/cmps03.h>

#define ERROR(s) {perror (s); return -1;}

static int boolean = 0;  /* 1=device in use 0=device not in use*/
static i2c_buffer buffer_read;
static i2c_buffer buffer_send;

/*--------------*
 *--  Create  --*
 *--------------*/

int cmps03_c_create (void)
{
  i2c_adainit();
  return 0;
}

/*--------------*
 *--  Remove  --*
 *--------------*/

int cmps03_c_remove (void)
{
  i2c_adafinal();
  return 0;
}

/*------------*
 *--  Open  --*
 *------------*/

int cmps03_c_open (int file_descriptor, int file_access_mode)
{
  if (boolean) {
    return -1;
  }else{
    boolean = 1;
    return 0;
  }
}

/*-------------*
 *--  Close  --*
 *-------------*/
int cmps03_c_close (int file_descriptor)
{
  boolean = 0;
  return 0;
}

/*------------*
 *--  Read  --*
 *------------*/
ssize_t cmps03_c_read (int file_descriptor, void *buffer, size_t bytes)
{
  i2c_operation_status stat;
  uint8_t byte_buffer;
  uint8_t word_buffer[2];
  switch (bytes){
  case 1:
    stat = read_buffer (&byte_buffer,1,buffer_read,I2C_OPERATION);
    ((uint8_t *)buffer)[0] = byte_buffer;
    break;
  case 2:
    stat = read_buffer (word_buffer,2,buffer_read,I2C_OPERATION);
    ((uint8_t *)buffer)[0] = word_buffer[1];
    ((uint8_t *)buffer)[1] = word_buffer[0];
    break;
  default:
    return 0;
  }
  free_buffer (buffer_read);
  free_buffer (buffer_send);
  return bytes;
}

/*-------------*
 *--  IOCTL  --*
 *-------------*/
int cmps03_c_ioctl (int file_descriptor, int request, void* argp)
{
  int ret;
  cmps03_ioctl_arg *ioctl_arg;
  ioctl_arg = (cmps03_ioctl_arg *)argp;
  i2c_flags flags;
  uint8_t reg_byte = REG_CMPS03_BEARING_BYTE;
  uint8_t reg_word = REG_CMPS03_BEARING_WORD;

  i2c_c_msg msg;
  i2c_c_msg msg_list[2];

  ret = flags_init (&flags);
  switch(request){
  case START_CONVERSION:
    switch(ioctl_arg->mode){
    case BEARING_MODE_BYTE:
      buffer_send = create_send_buff (&reg_byte, 1);
      buffer_read = create_recv_buff (1);
      ret = i2c_msg_c_init (&msg, SLAVE_ADDRESS, buffer_send, flags);
      msg_list[0] = msg;
      ret = i2c_msg_c_init (&msg, SLAVE_ADDRESS, buffer_read, flags);
      msg_list[1] = msg;
      break;
    case BEARING_MODE_WORD:
      buffer_send = create_send_buff (&reg_word, 1);
      buffer_read = create_recv_buff (2);
      ret = i2c_msg_c_init (&msg, SLAVE_ADDRESS, buffer_send, flags);
      msg_list[0] = msg;
      ret = i2c_msg_c_init (&msg, SLAVE_ADDRESS, buffer_read, flags);
      msg_list[1] = msg;
      break;
    }
    ret = i2c_transfer (I2C_ADAPTER,
                        I2C_OPERATION,
                        CMPS03_I2C_PRIORITY,
                        msg_list,
                        2);
    break;
  case GET_STATUS:
    ioctl_arg->status = (cmps03_status)i2c_get_status(I2C_OPERATION);
    ret = 0;
    break;
  default:
    ret = -1;
  }
  return ret;
}
