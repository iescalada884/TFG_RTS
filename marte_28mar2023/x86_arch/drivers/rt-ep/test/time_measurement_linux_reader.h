/*----------------------------------------------------------------------------
 *-------------------------        TIME_MEASURE       ------------------------
 *----------------------------------------------------------------------------
 *                                                             {LINUX_VERSION}
 *
 *                  'r e a d e r _ t i m e _ m e a  s u r e . h'
 *
 *                                     C
 *
 *
 * File 'reader_time_measure.h'                                       By Chema.
 *                                                          Jose Maria Martinez
 *                                                         <martinjm@unican.es>
 *
 * Header file used to configure the reception part of the linux measure..
 *
 *
 *---------------------------------------------------------------------------*/
/*****************************************************************************/
/* This module is the reception pair of the time measure modules             */
/* exec_measure or time_measure (depend on C or ADA programming). In order to*/
/* generate proper output files the identifier string assigned to the        */
/* initialization function init_time_measure has to be named as              */
/* resource : operation .See the importance of the ':' to separate both      */
/* fields                                                                    */

#ifndef READER_TIME_MEASURE_H
#define READER_TIME_MEASURE_H

/* In order to be able to fetch the measure packet, we have to assign      */
/* a proper transmision ethernet protocol (default 0x1010)                 */
#define TIME_MEASURE_PROTOCOL 0x1010

/* In OUTPUT_XXXXXXX_FILE define the name of the output file for the       */
/* resources and operations */
#define OUTPUT_RESOURCES_FILE "resources.txt"
#define OUTPUT_OPERATIONS_FILE "operations.txt"


/* We define here he NIC interface to be used.                             */
#define NIC_INTERFACE "eth0"


/* ITERATIONS_THRESHOLD : will define a threshold for including the average*/
/*                        time in the measure, if the measure of an item   */
/*                        has been taken for more han ITERATIONS_THRESHOLD */
/*                        times then it will be included in the output file*/
#define ITERATIONS_THRESHOLD 10



/* stations_dns_t : This type is the prototipe for kind of Domain Name MAC */
/*                  resolution.                                            */
typedef struct {
  char *mac_addr;
  char *name;
} stations_dns_t;

/* stations_table : The Stations Table is the table to look up when         */
/*                  assigning the time measured to a procesor, yo have to   */
/*                  identify each station that will measure times with a    */
/*                  human readable word in order to label the results.      */
/*                  The human readable word has to be different.            */

stations_dns_t stations_table[]={
        {"00:30:64:04:EA:AB", "cubo1"},
        {"00:30:64:05:74:73", "cubo2"},
        {"00:30:64:06:B5:EA", "cubo3"},
        {"00:30:64:05:77:77", "cubo4"},
        {"00:30:64:04:63:78", "cubo5"},
        {"00:30:64:06:B5:F2", "cubo6"}
};

#endif /* READER_TIME_MEASURE_H */
