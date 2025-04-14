/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                's y s / s t a t'
 *
 *                                      H
 *
 * File 'sys/stat.h'                                                  by MAR.
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
 *--------------------------------------------------------------------------*/

#ifndef	_MARTE_SYS_STAT_H_
#define _MARTE_SYS_STAT_H_

#include <sys/types.h>
#include <sys/cpp_macros.h>

CPP_BEGIN_DECLS

// this struct is defined only to compile the GNAT runtime
// took from 'man 2 stat'
struct stat {
  dev_t     st_dev;     /* ID of device containing file */
  ino_t     st_ino;     /* inode number */
  mode_t    st_mode;    /* protection */
  nlink_t   st_nlink;   /* number of hard links */
  uid_t     st_uid;     /* user ID of owner */
  gid_t     st_gid;     /* group ID of owner */
  dev_t     st_rdev;    /* device ID (if special file) */
  off_t     st_size;    /* total size, in bytes */
  // blksize_t st_blksize; /* blocksize for filesystem I/O */
  int st_blksize;
  // blkcnt_t  st_blocks;  /* number of 512B blocks allocated */
  int st_blocks;
  time_t    st_atime;   /* time of last access */
  time_t    st_mtime;   /* time of last modification */
  time_t    st_ctime;   /* time of last status change */

  /*
  dev_t     st_dev;     // Device ID of device containing file.
  mode_t    st_mode;   // Mode of file (see below).
  ino_t     st_ino;     // File serial number.
  nlink_t   st_nlink;   // Number of hard links to the file.
  uid_t     st_uid;     // User ID of file.
  gid_t     st_gid;     // Group ID of file.
  //   dev_t     st_rdev    Device ID (if file is character or block special).
  off_t     st_size;    // For regular files, the file size in bytes.
                       // For symbolic links, the length in bytes of the
                       // pathname contained in the symbolic link.
                       // For a shared memory object, the length in bytes.
                       // For a typed memory object, the length in bytes.
                       // For other file types, the use of this field is
                       // unspecified.
  time_t    st_atime;   // Time of last access.
  time_t    st_mtime;   // Time of last data modification.
  time_t    st_ctime;   // Time of last status change.
     //   blksize_t st_blksize A file system-specific preferred I/O block size for
     //                        this object. In some file system types, this may
     //                        vary from file to file.
     //   blkcnt_t  st_blocks  Number of blocks allocated for this object.
  */
};

int stat(const char *path, struct stat *buf);
int fstat(int fildes, struct stat *buf);
// in libmc/stat.c

#define S_ISBLK(m) 0
// Test for a block special file.

#define S_ISCHR(m) 1
// Test for a character special file.

#define S_ISDIR(m) 0
// Test for a directory.

#define S_ISFIFO(m) 0
// Test for a pipe or FIFO special file.

#define S_ISREG(m) 0
// Test for a regular file.

#define S_ISLNK(m) 0
// Test for a symbolic link.

#define S_ISSOCK(m) 0
// Test for a socket.

#define S_IRUSR 1
#define S_IWUSR 2
#define S_IXUSR 4
#define S_IRGRP 8
#define S_IWGRP 16
#define S_IXGRP 32
#define S_IROTH 64
#define S_IWOTH 128
#define S_IXOTH 256
#define S_ISUID 512
#define S_ISGID 1024

#define S_IRWXU S_IRUSR | S_IWUSR | S_IXUSR
#define S_IRWXG S_IRGRP | S_IWGRP | S_IXGRP
#define S_IRWXO S_IROTH | S_IWOTH | S_IXOTH

CPP_END_DECLS
#endif /* _MARTE_SYS_STAT_H_ */
