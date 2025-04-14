//#include <sgtty.h>
//#include <sys/ioctl.h>
#include <termio.h>
#include <stdio.h>
//#include <fcntl.h>
//#include <unistd.h>

void console_management_disable_echo ()
{
  struct termios tio;
  tcgetattr(0, &tio);
  tio.c_lflag &= ~ECHO;
  tcsetattr(0, TCSANOW, &tio);
}

void console_management_enable_echo ()
{
  struct termios tio;
  tcgetattr(0, &tio);
  tio.c_lflag |= ECHO;
  tcsetattr(0, TCSANOW, &tio);
}

