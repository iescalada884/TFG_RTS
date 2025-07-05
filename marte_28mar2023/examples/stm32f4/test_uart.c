/*
 * Ejemplo de uso de la UART en la placa STM32F407G-DISC1.
 *
 * Este ejemplo utiliza el puerto UART2 de la placa, con la siguiente conexión:
 *   - GND  ->  GND
 *   - RX   ->  PA2
 *   - TX   ->  PA3
 *   - 3.3V ->  3V
 *
 * Se recomienda utilizar un adaptador UART a USB para la comunicación con el PC.
 *
 * Una vez compilado el programa, se puede cargar en la placa con el siguiente comando:
 *
 *   openocd -f board/stm32f4discovery.cfg -c "program test_uart.bin verify reset exit 0x08000000"
 *
 * En el lado receptor (PC), se pueden utilizar herramientas como **minicom** o **cutecom**
 * para visualizar los datos recibidos por la UART.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils/uart_console.h"

void test_uart_console() {
    int test_baudrate = 115200;
    char *test_msg = "Test MaRTE OK\n";

    // Inicializar UART
    uart_console_init(test_baudrate);

    // Imprimir mensaje por consola
    uart_print_console(test_msg);
}

int main() {
    //printf ("Test UART \n");
    test_uart_console();
    //printf ("Done \n");
    exit (0);
}
