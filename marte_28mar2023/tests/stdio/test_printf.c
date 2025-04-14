//  Test for all architectures
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

int main()
{
  int ret;
  float f = 1.234;
  int i = 98765;
  char str[20];
  const char str_float[] = "f/1.234000/";

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  printf("int:%d\n", i);
  printf("float:%f:\n", f);
  printf("int:%d\n", i);

  sprintf(str, "f/%f/", f);
  printf("sprintf float:%s:\n", str);
  ret = strncmp(str, str_float, sizeof(str));
  if (ret != 0) {
    printf("ERROR: Wrong float to str conversion\n");
    return -1;
  }

  printf("Test OK\n");
  return 0;
}
