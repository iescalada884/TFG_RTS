/*
 * uart_console.c
 *
 *  Created on: Nov 17, 2021
 *      Author: Kunal
 *      Modified by: Juan Romón Peña
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
  port->BRR = (int)(84000000 / ((16 * baudrate) * 2)) << 4;
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
