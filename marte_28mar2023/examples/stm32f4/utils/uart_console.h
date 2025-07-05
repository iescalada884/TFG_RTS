/*
 * uart_console.h
 *
 *  Created on: Nov 14, 2021
 *      Author: Kunal
 */

#ifndef _UART_CONSOLE_CONSOLE_H_
#define _UART_CONSOLE_CONSOLE_H_

void uart_console_init(int baudrate);

void uart_print_console(char *msg);

#endif /* _UART_CONSOLE_CONSOLE_H_ */
