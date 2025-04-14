/*
  MaRTE version of wiring_time.c
*/

#include <time.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>

uint32_t millis(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  //printf("arduinomillis\n");
  //printf("%d nsec \n" , ts.tv_nsec );
  //printf("%d sec \n" , ts.tv_sec );
  return ts.tv_sec *1000 + ts.tv_nsec/1000000;
}

// Interrupt-compatible version of micros
uint32_t micros(void)
{
  //printf("check1\n");
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  //printf("%d nsec \n" , ts.tv_nsec );
  //printf("%d sec \n" , ts.tv_sec );
  //printf("%d valor gettime: \n" , aux );
  return ts.tv_sec *1000000 + ts.tv_nsec/1000.0 ;
}

void delay(uint32_t ms)
{
  //printf("arduinodelay\n");
  struct timespec ts = {ms/1000, ms%1000 * 1000000};
  nanosleep(&ts, NULL);
}

void delayMicroseconds(uint32_t us)
{
  uint32_t final = micros() + us;
  while (micros() <= final);
}
