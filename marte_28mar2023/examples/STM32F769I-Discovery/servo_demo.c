#define ARDUINO_MAIN
#include <Arduino.h>
#include <unistd.h>
#include <stdio.h>
#include <Servo_c.h>

int main (int argc, char **argv) {
  initVariant();    
  printf("s3\n");
    
  Servo_t  sensor= servo_create();
  printf("s4\n");

  printf("%d\n", servo_attach(sensor,5));
  printf("%s", servo_attached(sensor) ? "true\n" : "false\n");

  int pos=0;
  for (;;) {
    //printf("dentroDelFor\n");
    for (pos = 0; pos <= 180; pos += 10) {

      printf("%d\n", pos);
      servo_write(sensor,pos);
      //printf("DespuesDelWrite\n");
      //delay(500);
      sleep(1);
    }
    for (pos = 180; pos >= 0; pos -= 10) {

      printf("%d\n", pos);
      servo_write(sensor,pos);
      //delay(500);
      sleep(1);
    }

    //sleep(2);
  }

  return 0;
}
