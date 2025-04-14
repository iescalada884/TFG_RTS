/*!
 * @file can_debug.h
 *
 * @brief debuggin macros and flags for the can driver
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
 * This module contains macros and flags to enable debugging in the
 * can bus driver for MaRTE OS
 *
 * @license
 *
 * See MaRTE OS license
 *
 */

#ifndef _MARTE_CAN_DEBUG_H_
#define _MARTE_CAN_DEBUG_H_

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

/**
 * DEBUG macros
 **/

#define DEBUG(enable,x,args...) if(enable) printc("DEBUG (%s): " x, __func__ , ##args)
#define ERROR(x,args...) {printe("ERROR (%s): " x, __func__ , ##args);} // exit(-1);}
#define WARNING(x,args...) printe("WARNING (%s): " x, __func__ , ##args)

/**
 * DEBUGGING FLAGS to enable/disable debugging messages
 **/

#define CAN_FRAMESPOOL_ENABLE_DEBUG     false
#define CAN_DRIVER_ENABLE_DEBUG         false
#define ADLINK7841_ENABLE_DEBUG         false
#define SJA1000_ENABLE_DEBUG            false
#define SJA1000_ENABLE_DETAILED_DEBUG   true

#endif // _MARTE_CAN_DEBUG_H_
