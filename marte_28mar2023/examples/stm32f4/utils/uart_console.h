/*
 * uart_console.h
 *
 *  Created on: Nov 14, 2021
 *      Author: Kunal
 */

#ifndef _UART_CONSOLE_CONSOLE_H_
#define _UART_CONSOLE_CONSOLE_H_

#include <stddef.h>
#include <stdlib.h>

void uart_console_init(int baudrate);

void uart_print_console(char *msg);

void uart_console_putchar(char c);

int uart_console_write(int fd, const void *buffer, size_t bytes);

#endif /* _UART_CONSOLE_CONSOLE_H_ */
