/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *                       'n o n _ i m p l e m e n t e d'
 *
 *                                      C
 *
 * File 'non_implemented.c'                                            by MAR.
 *
 * Non-implemented functions. Only to avoid linker errors.
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *  MaRTE OS web page: http://marte.unican.es
 *  Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                     Michael Gonzalez Harbour      mgh@unican.es
 *
 * MaRTE OS  is free software; you can  redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as published by the
 * Free Software Foundation;  either  version 2, or (at  your option) any
 * later version.
 *
 * MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 * WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 * MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 * General Public License for more details.
 *
 * You should have received  a  copy of  the  GNU General Public  License
 * distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 * Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 * 02111-1307, USA.
 *
 * As a  special exception, if you  link this  unit  with other  files to
 * produce an   executable,   this unit  does  not  by  itself cause  the
 * resulting executable to be covered by the  GNU General Public License.
 * This exception does  not however invalidate  any other reasons why the
 * executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/
#include <stdio.h>
//#include <oskit/c/time.h>
#include <unistd.h>
#include <oskit/c/termios.h>
#include <sys/marte_configuration_parameters.h>


pid_t getppid(void)
{
  printf("  INTERNAL ERROR: function getppid non implemented yet\n");
  return 0;
}

pid_t getpid(void)
{
  printf("  INTERNAL ERROR: function getpid non implemented yet\n");
  return 0;
}

int link(const char *oldpath, const char *newpath)
{
  printf("  INTERNAL ERROR: function link non implemented yet\n");
  return 0;
}

// int unlink(const char *pathname)
// {
//   printf("  INTERNAL ERROR: function unlink non implemented yet\n");
//   return 0;
// }

pid_t fork(void)
{
  printf("  INTERNAL ERROR: function fork non implemented yet\n");
  return 0;
}

int execv(const char *path, char *const argv[])
{
  printf("  INTERNAL ERROR: function execv non implemented yet\n");
  return 0;
}

pid_t wait(int *status)
{
  printf("  INTERNAL ERROR: function wait non implemented yet\n");
  return 0;
}

int tcsetattr (int fd, int optional_actions, const struct termios *termios_p)
{
  printf("  INTERNAL ERROR: function tcsetattr non implemented yet\n");
  return 0;
}

int tcgetattr ( int fd, struct termios *termios_p )
{
  printf("  INTERNAL ERROR: function tcgetattr non implemented yet\n");
  return 0;
}

int setvbuf(FILE *stream, char *buf, int mode , size_t size)
{
  if (size)
    printf("  INTERNAL ERROR: function setvbuf non implemented yet\n");
  return 0;
}

void clearerr( FILE *stream)
{
  printf("  INTERNAL ERROR: function clearerr non implemented yet\n");
}

char *tmpnam(char *s)
{
  printf("  INTERNAL ERROR: function tmpnam non implemented yet\n");
  return 0;
}

FILE  *freopen  (const  char *path, const char *mode, FILE *stream)
{
  printf("  INTERNAL ERROR: function freopen non implemented yet\n");
  return 0;
}

int __xstat()
{
  printf("  INTERNAL ERROR: function __xstat non implemented yet\n");
  return 0;
}

int __fxstat()
{
  // printf("  INTERNAL ERROR: function __fxstat non implemented yet\n");
  return 0;
}

char *getwd(char *buf)
{
  printf("  INTERNAL ERROR: function getwd non implemented yet\n");
  return 0;
}

char *getcwd(char *buf, size_t size)
{
  printf("  INTERNAL ERROR: function getcwd non implemented yet\n");
  return 0;
}

struct dirent {};
int readdir(unsigned int fd, struct dirent *dirp, unsigned int count)
{
  printf("  INTERNAL ERROR: function readdir non implemented yet\n");
  return 0;
}

char *realpath(const char *path, char *resolved_path)
{
  printf("  INTERNAL ERROR: function realpath non implemented yet\n");
  return 0;
}

int feof( FILE *stream)
{
  printf("  INTERNAL ERROR: function feof non implemented yet\n");
  return 0;
}

int ferror( FILE *stream)
{
  // printf("  INTERNAL ERROR: function ferror non implemented yet\n");
  return 0;
}

int isatty ( int desc )
{
  printf("  INTERNAL ERROR: function isatty non implemented yet\n");
  return 0;
}

int mkdir(const char *pathname, int mode)
{
  printf("  INTERNAL ERROR: function mkdir non implemented yet\n");
  return 0;
}

/* //int dladdr(void *addr, Dl_info *info);
int dladdr(void *addr, void *info)
{
  printe("  INTERNAL ERROR: function dladdr not implemented yet\n");
  return 0;
}*/

#if 0 // MARTE_ARCHITECTURE == ARCH_X86
int dl_iterate_phdr(int (*callback) (),
		    void *data)
{
  printe("  INTERNAL ERROR: function dl_iterate_phdr not implemented yet\n");
  return 0;
}
#endif

long sysconf(int name)
{
  // used by GNAT (s-osinte.ads)
  printe("  INTERNAL ERROR: function sysconf not implemented yet\n");
  return -1;
}

// int sigaltstack(const stack_t *ss, stack_t *oss);
int sigaltstack(const void *ss, void *oss)
{
  // used by GNAT (s-osinte.ads)
  printe("  INTERNAL ERROR: function sigaltstack not implemented\n");
  return -1;
}

// for GNAT-GPL-2009
int rmdir(const char *pathname) {
  printe("  INTERNAL ERROR: function rmdir not implemented\n");
  return -1;
}

int chdir(const char *path) {
  printe("  INTERNAL ERROR: function chdir not implemented\n");
  return -1;
}

int rename(const char *oldpath, const char *newpath) {
  printe("  INTERNAL ERROR: function rename not implemented\n");
  return -1;
}

int clearenv(void) {
  printe("  INTERNAL ERROR: function clearenv not implemented\n");
  return -1;
}

int unsetenv(const char *name) {
  printe("  INTERNAL ERROR: function unsetenv not implemented\n");
  return -1;
}

int putenv(char *string) {
  printe("  INTERNAL ERROR: function putenv not implemented\n");
  return -1;
}
