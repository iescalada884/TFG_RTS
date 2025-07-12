/*
 * uart_console.c
 *
 *  Created on: Nov 17, 2021
 *      Author: Kunal
 *      Modified by: Juan Román Peña
 *
 * This file has been simplified to be part of the M2OS project implementation.
 * Uses USART2: RX->PA2, TX->PA3
 */

#include "uart_console.h"
#include "GPIO.h"
#include "stm32f407xx.h"
#include "stm32f4xx.h"
#include "system_stm32f4xx.h"

USART_TypeDef *port = USART2;

void
uart_console_init (int baudrate)
{
  RCC->APB1ENR |= RCC_APB1ENR_USART2EN;
  GPIO_Pin_Setup ('A', 2, ALTERNATE_FUNCTION_OUTPUT_PUSHPULL, USART2_TX);
  GPIO_Pin_Setup ('A', 3, ALTERNATE_FUNCTION_OUTPUT_OPENDRAIN, USART2_RX);
  port->CR1 |= USART_CR1_UE;
  
  // Correct baud rate calculation for STM32F4
  // For USART2 on APB1: BRR = fCK / baudrate
  // Where fCK is the APB1 clock (typically SystemCoreClock for default config)
  port->BRR = SystemCoreClock / baudrate;
  
  port->CR1 |= USART_CR1_TE;
  port->CR1 |= USART_CR1_RE;
}

void
uart_print_console (char *msg)
{
  while (*msg != '\0')
    {
      port->DR = *msg++;
      while (!(port->SR & USART_SR_TXE))
        ;
    }
}

void
uart_console_putchar (char c)
{
  while (!(port->SR & USART_SR_TXE)) {
  }

  port->DR = c;
}

// Function to write multiple bytes to UART console
// Compatible with MaRTE Direct_IO write function signature
int uart_console_write(int fd, const void *buffer, size_t bytes) {
  const char *buf = (const char *)buffer;
  size_t i;
  
  for (i = 0; i < bytes; i++) {
    uart_console_putchar(buf[i]);
  }
  
  return (int)bytes; // Return number of bytes written
}

void uart_print_error(const char *msg) {
  char* error_header = "ERROR: ";
  uart_print_console(error_header);
  uart_print_console((char *)msg);
  uart_print_console("\n");
  
}
/*
void
print_console_int (int num)
{
  if (num == 0) {
    char msg = 0x30;
    print_console_byte(&msg);
    return;
  }

  int aux = num;
  int len = 1;

  while (aux > 0)
    {
      aux = aux / 10;
      len *= 10;
    }
  len /= 10;
  char msg; //
             // conver char by char manually
  int digit;
  while (len > 0)
    {
      digit = num / len;
      num = num % len;
      len /= 10;
      msg = digit + 0x30;
      print_console_byte(&msg);
    }

}

void print_console_newline(){
  print_console("\n");
   }
*/
