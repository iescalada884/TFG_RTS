
#include <stdio.h>
#include <unistd.h>
#include <Ultrasonic_c.h>

int main (int argc, char **argv) {
  printf("Ultrasonic ranger demo\n");
  Ultrasonic_t sensor = ultrasonic_create(7);

  while (1) {
    long range_in_centimeters;
    range_in_centimeters =  ultrasonic_measure_in_cm(sensor);

    printf("Centimeters: %ld \n" , range_in_centimeters);//0~400cm

    sleep(2);
  }

  return 0;
}
