/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.51  Oct 2005
 *
 *                          	t e s t _ i 2 c
 *
 *                                    C
 *
 * File 'test_i2c.c'                                        By Sangorrin
 *
 * to compile: mgcc test_i2c_c.c
 *
 *---------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/i2c.h>
#include <pthread.h>

void pause(){
  char pulsacion;
  printf("press...");
  pulsacion = getchar();
}

int main()
{
  int ret;
  i2c_priority prio = 4;
  i2c_address addr = 0x60;
  i2c_adapter_id adap = ELITE;
  i2c_operation_id op = OP_1;
  i2c_flags flags;

  i2c_buffer buffer_read;
  i2c_buffer buffer_send;
  uint8_t reg = 0x01;
  uint8_t medida;

  i2c_c_msg *msg_list;
  i2c_c_msg msg;
  i2c_operation_status stat;

  ret = pthread_setschedprio (pthread_self(),5);

  i2c_adainit();

  buffer_read = create_recv_buff (1);
  buffer_send = create_send_buff (&reg, 1);
  ret = flags_init (&flags);

  msg_list = (i2c_c_msg *)malloc(2*sizeof(i2c_c_msg));

  ret = i2c_msg_c_init (&msg, addr, buffer_send, flags);
  msg_list[0] = msg;
  ret = i2c_msg_c_init (&msg, addr, buffer_read, flags);
  msg_list[1] = msg;

  while (1){
    printf("PRUEBA\n");
    pause();

    ret = i2c_transfer (adap,
                        op,
                        prio,
                        msg_list,
                        2);
    // do{
    //   stat = i2c_get_status (op);
    // }while( stat == WAITING );

    stat = read_buffer (&medida,1,buffer_read,op);
    printf("medida: %d \n", medida);
  }

  i2c_adafinal();

  exit(0);
}
