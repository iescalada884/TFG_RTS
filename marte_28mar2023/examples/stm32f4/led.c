/*
 * Ejemplo de uso de un led en la placa STM32F407G-DISC1.
 *
 * Este ejemplo enciende y apaga el led verde que está entre los dos botones.
 * Es completamente bare-metal salvo la inicialización de MaRTE OS y
 * emplea una espera activa (busy-wait)
 */

#include <stdint.h>

#define RCC_BASE        0x40023800
#define GPIOD_BASE      0x40020C00

#define RCC_AHB1ENR     (*(volatile uint32_t *)(RCC_BASE + 0x30))
#define GPIOD_MODER     (*(volatile uint32_t *)(GPIOD_BASE + 0x00))
#define GPIOD_ODR       (*(volatile uint32_t *)(GPIOD_BASE + 0x14))

#define LED_PIN         12  // 12, 13 14 o 15

void delay(volatile uint32_t t) {
    while (t--) {
        __asm__("nop");
    }
}

int main(void) {
    // Habilita reloj para GPIOD
    RCC_AHB1ENR |= (1 << 3);

    // Configura PD12 como salida
    GPIOD_MODER &= ~(0x3 << (LED_PIN * 2)); // limpia
    GPIOD_MODER |=  (0x1 << (LED_PIN * 2)); // salida

    // Bucle: parpadear
    while (1) {
        GPIOD_ODR ^= (1 << LED_PIN); // toggle LED
        delay(1000000);
    }
}
