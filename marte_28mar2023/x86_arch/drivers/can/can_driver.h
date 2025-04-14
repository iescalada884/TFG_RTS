/*!
 * @file can_driver.h
 *
 * @brief can driver for MaRTE OS
 *
 * @version 0.01
 *
 * @date 11-Feb-2008
 *
 * @author
 *      Daniel Sangorrin
 *
 * @comments
 *
 * This module contains the MaRTE OS filesystem functions to install
 * the driver in MaRTE OS. CAN bus chips are distinguised by using the
 * minor number of the device files.
 *
 * @license
 *
 * See MaRTE OS license
 *
 */

#ifndef _MARTE_CAN_DRIVER_H_
#define _MARTE_CAN_DRIVER_H_

#include <sys/types.h>

extern int can_driver_create ();
extern int can_driver_remove ();
extern int can_driver_open (int fd, int mode);
extern int can_driver_close (int fd);
extern int can_driver_ioctl (int fd, int request, void *argp);
extern ssize_t can_driver_read (int fd, void *buffer, size_t bytes);
extern ssize_t can_driver_write (int fd, void *buffer, size_t bytes);

#endif // _MARTE_CAN_DRIVER_H_
