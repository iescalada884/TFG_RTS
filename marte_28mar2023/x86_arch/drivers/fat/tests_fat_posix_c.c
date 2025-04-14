/*----------------------------------------------------------------------------
--                            TESTS - FAT POSIX C                           --
------------------------------------------------------------------------------
--  Test for the FAT through a C-POSIX interface. You have to install the   --
--  fat_driver_functions in MaRTE OS (see marteos users guide)              --
--  Uncomment/comment the tests as you want                                 --
--                                            daniel.sangorrin_at_gmail.com --
----------------------------------------------------------------------------*/

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <debug_marte.h> // For Debugging

#define ERROR(s) {perror (s); exit (-1);}

void pause(){
        char key;
        printf(" press Enter...");
        key = getchar();
}

void msg(char *s){
   printf(s);
   pause();
}

int main ()
{
   int fd, fd2, ret;
   int count;
   char buffer[20];
   off_t offset, offset_ret;
   int whence;

   msg ("BEGIN OF FAT POSIX-C INTERFACE TESTS");

   // init_serial_communication_with_gdb (SERIAL_PORT_1);
   // set_break_point_here;

   /*-----------------------------------------------------
   -- TEST 1: Read a text file and display its contents --
   -----------------------------------------------------*/
   // msg ("TEST 1: Read a text file and display its contents");
   // if ((fd = open ("/fat/file1", O_RDONLY)) == -1)
   //    ERROR (" test1-open ");
   //
   // while ((count = read(fd, buffer, 19)) > 0) {
   //    if (count == -1)
   //       ERROR (" test1-read ");
   //    buffer [count] = '\0';
   //    printf ("%s",buffer);
   // }
   //
   // if (close(fd) == -1)
   //    ERROR (" test1-close ");
   /*-----------------------------------------------------
   -- TEST 2: Copy a file /file1 to another file /file2 --
   -----------------------------------------------------*/
   // msg ("TEST 2: Copy a file /file1 to another file /file2");
   // if ((fd = open ("/fat/file1", O_RDONLY)) == -1)
   //    ERROR (" test2-open1 ");
   //
   // if ((fd2 = open ("/fat/file2", O_WRONLY | O_CREAT)) == -1)
   //    ERROR (" test2-open2 ");
   //
   // while ((count = read(fd, buffer, 20)) > 0) {
   //    if (count == -1)
   //       ERROR (" test2-read ");
   //    if ((count = write(fd2, buffer, count)) == -1)
   //       ERROR (" test2-write ");
   // }
   //
   // if (close(fd) == -1)
   //    ERROR (" test2-close1 ");
   // if (close(fd2) == -1)
   //    ERROR (" test2-close2 ");
   /*----------------------------------------------------
   -- TEST 3: Create a file /file3 and write some text --
   ----------------------------------------------------*/
   // msg ("TEST 3: Create a file /file3 and write some text");
   // if ((fd = open ("/fat/file3", O_WRONLY | O_CREAT)) == -1)
   //    ERROR (" test3-open ");
   //
   // if ((count = write(fd, "Hello World!!", 14)) == -1)
   //    ERROR (" test3-write ");
   //
   // if (close(fd) == -1)
   //    ERROR (" test3-close ");
   /*---------------------------------
   -- TEST 4: Delete a file /file3  --
   ---------------------------------*/
   // msg ("TEST 4: Delete a file /file3 (while it is opened)");
   // if ((fd = open ("/fat/file3", O_RDONLY)) == -1)
   //    ERROR (" test4-open ");
   //
   // if ((count = read(fd, buffer, 19)) == -1)
   //    ERROR (" test4-read ");
   // buffer [count] = '\0';
   // printf ("%s", buffer);
   //
   // if ((ret = unlink ("/fat/file3")) == -1)
   //    ERROR (" test4-unlink ");
   //
   // if (close(fd) == -1)
   //    ERROR (" test4-close ");
   /*------------------------------------------------
   -- TEST 5: Seek 5 chars and read a file /file1  --
   ------------------------------------------------*/
   // msg ("TEST 5: Seek and read a file /file1");
   // if ((fd = open ("/fat/file1", O_RDONLY)) == -1)
   //    ERROR (" test5-open ");
   //
   // msg ("a) Seek -5 from the end");
   // whence = SEEK_END;
   // offset = -5;
   // if ((offset_ret = lseek(fd, offset, whence)) == -1)
   //    ERROR (" test5-lseek1 ");
   // if ((count = read(fd, buffer, 19)) == -1)
   //    ERROR (" test5-read1 ");
   // buffer [count] = '\0';
   // printf ("%s", buffer);
   // // redo the seek because the 'read' seeks too!
   // if ((offset_ret = lseek(fd, offset, whence)) == -1)
   //    ERROR (" test5-lseek1b ");
   //
   // msg ("b) Seek +2 from current offset");
   // whence = SEEK_CUR;
   // offset = 2;
   // if ((offset_ret = lseek(fd, offset, whence)) == -1)
   //    ERROR (" test5-lseek2 ");
   // if ((count = read(fd, buffer, 19)) == -1)
   //    ERROR (" test5-read2 ");
   // buffer [count] = '\0';
   // printf ("%s", buffer);
   //
   // msg ("c) Seek to absolute offset 4");
   // whence = SEEK_SET;
   // offset = 4;
   // if ((offset_ret = lseek(fd, offset, whence)) == -1)
   //    ERROR (" test5-lseek3 ");
   // if ((count = read(fd, buffer, 19)) == -1)
   //    ERROR (" test5-read3 ");
   // buffer [count] = '\0';
   // printf ("%s", buffer);
   //
   // if (close(fd) == -1)
   //    ERROR (" test5-close ");
   //------------------------------------------------
   msg ("END OF FAT POSIX-C INTERFACE TESTS");
   return 0;
}
