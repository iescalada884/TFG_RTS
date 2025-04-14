/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            A D A I N T - X I                             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2013, Free Software Foundation, Inc.            *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 *                                                                          *
 *                                                                          * 
 *                                                                          * 
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is a variant of adaint.c for bareboards.  Currently this is just a
   stub.  */

// MaRTE OS (ARM architecture)
#ifndef MaRTE
#error "MaRTE expects MaRTE symbol to be defined"
#endif

#ifndef IN_RTS
#error "MaRTE RTS expected"
#endif

#ifndef IN_GCC
#error "MaRTE IN_GCC expected"
#endif

#ifndef inhibit_libc
#error "MaRTE inhibit_libc expected"
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <signal.h>

#include "tsystem.h"


#ifdef __cplusplus
extern "C" {
#endif

const void *
__gnat_get_executable_load_address (void)
{
  return NULL;
}

#define DIR_SEPARATOR '/'

char __gnat_dir_separator = DIR_SEPARATOR;

#define GNAT_MAX_PATH_LEN 256

int __gnat_max_path_len = GNAT_MAX_PATH_LEN;

void
__gnat_get_current_dir (char *dir, int *length)
{
  *length = 1;
  dir[0] = DIR_SEPARATOR;
  dir[*length] = '\0';
}

int
__gnat_readlink (char *path,
		 char *buf,
		 size_t bufsiz)
{
  return -1;
}

int
__gnat_symlink (char *oldpath,
		char *newpath)
{
  return -1;
}

/* Try to lock a file, return 1 if success.  */
int
__gnat_try_lock (char *dir, char *file)
{
  return 1;
}

int
__gnat_open_read (char *path, int fmode)
{
  return -1;
}

int
__gnat_open_rw (char *path, int fmode)
{
  return -1;
}

int
__gnat_open_create (char *path, int fmode)
{
  return -1;
}

int
__gnat_create_output_file (char *path)
{
  return -1;
}

int
__gnat_create_output_file_new (char *path)
{
  return -1;
}

int
__gnat_open_append (char *path, int fmode)
{
  return -1;
}

int
__gnat_open_new (char *path, int fmode)
{
  return -1;
}

int
__gnat_open_new_temp (char *path, int fmode)
{
  return -1;
}

int_least64_t __gnat_file_length (int fd)
{
  return -1;
}

int_least64_t __gnat_named_file_length (char *name)
{
  return -1;
}

void
__gnat_tmp_name (char *tmp_filename)
{
  *tmp_filename = 0;
}

char *
__gnat_readdir (void *dirp, char *buffer, int *i)
{
  return NULL;
}

int
__gnat_dup (int oldfd)
{
  return -1;
}

int
__gnat_dup2 (int oldfd, int newfd)
{
  return -1;
}int
__gnat_stat (char *name, void *statbuf)
{
  return -1;
}

int
__gnat_file_exists (char *name)
{
  return -1;
}

int
__gnat_is_absolute_path (char *name, int length)
{
  return (length != 0) && (*name == DIR_SEPARATOR);
}

int
__gnat_is_regular_file (char *name)
{
  return -1;
}

int
__gnat_is_regular_file_fd (int fd)
{
  return -1;
}

int
__gnat_is_directory (char *name)
{
  return -1;
}

int
__gnat_is_readable_file (char *name)
{
  return -1;
}

int
__gnat_is_writable_file (char *name)
{
  return -1;
}

int
__gnat_is_executable_file (char *name)
{
}

void
__gnat_set_writable (char *name)
{
}

void
__gnat_set_executable (char *name, int not_used)
{
}

void
__gnat_set_non_writable (char *name)
{
}

void
__gnat_set_readonly (char *name)
{
}

void
__gnat_set_readable (char *name)
{
}

void
__gnat_set_non_readable (char *name)
{
}

int
__gnat_is_symbolic_link (char *name)
{
  return -1;
}

int
__gnat_portable_spawn (char *args[])
{
  return -1;
}

/* This functions copy the file attributes from a source file to a
   destination file.

   mode = 0  : In this mode copy only the file time stamps (last access and
               last modification time stamps).

   mode = 1  : In this mode, time stamps and read/write/execute attributes are
               copied.

   mode = 2  : In this mode, only read/write/execute attributes are copied

   Returns 0 if operation was successful and -1 in case of error. */

int
__gnat_copy_attribs (char *from, char *to,
                     int mode)
{
  return -1;
}

/* Locate an executable using the Systems default PATH.  */

char *
__gnat_locate_exec_on_path (char *exec_name)
{
  return NULL;
}

int
__gnat_rename (char *from, char *to)
{
  return -1;
}

int
__gnat_set_close_on_exec (int fd,
                          int close_on_exec_p)
{
  return -1;
}

int
__gnat_portable_wait (int *process_status)
{
  return -1;
}

void *
__gnat_freopen (char *path,
		char *mode,
		void *stream,
		int encoding)
{
  return NULL;
}

void
__gnat_os_exit (int status)
{
  exit (status);
}

int
__gnat_unlink (char *path)
{
  return -1;
}

typedef long OS_Time;

OS_Time
__gnat_file_time_name (char *name)
{
 OS_Time ret;
 return ret;
}

OS_Time
__gnat_file_time_fd (int fd)
{
 OS_Time ret;
 return ret;
}

void
__gnat_get_object_suffix_ptr (int *len, const char **value)
{
}

void
__gnat_get_executable_suffix_ptr (int *len, const char **value)
{
  /**value = "";
   *len = 0;*/
}

void
__gnat_get_debuggable_suffix_ptr (int *len, const char **value)
{
}

void
__gnat_current_time_string (char *result)
{
  // << MaRTE
  result [0]  = 'M';
  result [1]  = 'a';
  result [2]  = 'R';
  result [3]  = 'T';
  result [4]  = 'E';
  result [5]  = ':';
  result [6]  = 'n';
  result [7]  = 'o';
  result [8]  = 't';
  result [9]  = ' ';
  result [10] = 's';
  result [11] = 'u';
  result [12] = 'p';
  result [13] = 'p';
  result [14] = 0;
}

void
__gnat_to_os_time (OS_Time *p_time, int year, int month, int day,
		   int hours, int mins, int secs)
{
}

void
__gnat_kill (int pid, int sig, int close)
{
  kill (pid, sig);
}

void __gnat_killprocesstree (int pid, int sig_num)
{
  kill (pid, sig_num);
}

char *
__gnat_locate_regular_file (char *file_name, char *path_val)
{
  return NULL;
}

int __gnat_argument_needs_quote = 0;

int
__gnat_portable_no_block_spawn (char *args[])
{
  return -1;
}

char *
__gnat_to_canonical_file_spec (char *filespec)
{
  return filespec;
}

int
__gnat_lseek (int fd, long offset, int whence)
{
  return -1; // <---- MaRTE OS
  //  return (int) lseek (fd, offset, whence);
}

void
__gnat_set_file_time_name (char *name, time_t time_stamp)
{
}

void
__gnat_to_gm_time (OS_Time *p_time, int *p_year, int *p_month, int *p_day,
		   int *p_hours, int *p_mins, int *p_secs)
{
  struct tm *res;
  time_t time = (time_t) *p_time;

  res = gmtime (&time);
  if (res)
    {
      *p_year = res->tm_year;
      *p_month = res->tm_mon;
      *p_day = res->tm_mday;
      *p_hours = res->tm_hour;
      *p_mins = res->tm_min;
      *p_secs = res->tm_sec;
    }
  else
    *p_year = *p_month = *p_day = *p_hours = *p_mins = *p_secs = 0;
}

/* Return nonzero if file names are case sensitive.  */
int
__gnat_get_file_names_case_sensitive (void)
{
  return 1;
}

// FILE * __gnat_fopen (char *path, char *mode, int encoding)
void *
__gnat_fopen (char *path, char *mode, int encoding)
{
  return (void *) fopen (path, mode);
}

// CPU sets

#define cpu_set_t void

cpu_set_t *__gnat_cpu_alloc (size_t count ATTRIBUTE_UNUSED)
{
  return NULL;
}

size_t __gnat_cpu_alloc_size (size_t count ATTRIBUTE_UNUSED)
{
  return 0;
}

void __gnat_cpu_free (cpu_set_t *set)
{
  ;
}

void __gnat_cpu_zero (size_t count ATTRIBUTE_UNUSED, cpu_set_t *set)
{
  ;
}

void __gnat_cpu_set (int cpu, size_t count ATTRIBUTE_UNUSED, cpu_set_t *set)
{
  ;
}

int
__gnat_number_of_cpus (void)
{
  return 1;
}

#ifdef __cplusplus
}
#endif
