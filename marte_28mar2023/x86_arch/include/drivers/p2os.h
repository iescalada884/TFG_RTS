/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                   'p 2 o s'
 *
 *                                      C
 *
 *  File 'p2os.h'                                        by F.J.Feijoo
 *                                            University of Zaragoza (UNIZAR)
 *
 *
 *
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2000-2007, Universidad de Cantabria, SPAIN
 *
 *   MaRTE OS web page: http://marte.unican.es
 *   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                      Michael Gonzalez Harbour      mgh@unican.es
 *
 *  MaRTE OS  is free software; you can  redistribute it and/or  modify it
 *  under the terms of the GNU General Public License  as published by the
 *  Free Software Foundation;  either  version 2, or (at  your option) any
 *  later version.
 *
 *  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 *  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 *  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 *  General Public License for more details.
 *
 *  You should have received  a  copy of  the  GNU General Public  License
 *  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 *  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 *  02111-1307, USA.
 *
 *  As a  special exception, if you  link this  unit  with other  files to
 *  produce an   executable,   this unit  does  not  by  itself cause  the
 *  resulting executable to be covered by the  GNU General Public License.
 *  This exception does  not however invalidate  any other reasons why the
 *  executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/

/*
 *  Player - One Hell of a Robot Server
 *  Copyright (C) 2000
 *     Brian Gerkey, Kasper Stoy, Richard Vaughan, & Andrew Howard
 *
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/*
 * $Id: p2os.h,v 1.30 2006/04/04 21:31:37 gbiggs Exp $
 *
 *   the P2OS device.  it's the parent device for all the P2 'sub-devices',
 *   like gripper, position, sonar, etc.  there's a thread here that
 *   actually interacts with P2OS via the serial line.  the other
 *   "devices" communicate with this thread by putting into and getting
 *   data out of shared buffers.
 */
#ifndef _P2OSDEVICE_H
#define _P2OSDEVICE_H

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <drivers/serial_port_driver.h>
#include <drivers/player.h>
#include "p2os-packet.h"

#define     NO_SYNC         0
#define     AFTER_FIRST_SYNC    1
#define     AFTER_SECOND_SYNC   2
#define     READY           3

// Default max speeds
//#define MOTOR_DEF_MAX_SPEED 0.5
#define MOTOR_DEF_MAX_SPEED 2.0
#define MOTOR_DEF_MAX_TURNSPEED DTOR(100)

/*
 * Apparently, newer kernel require a large value (200000) here.  It only
 * makes the initialization phase take a bit longer, and doesn't have any
 * impact on the speed at which packets are received from P2OS
 */
#define P2OS_CYCLETIME_USEC 200000

/* p2os constants */

#define P2OS_NOMINAL_VOLTAGE 12.0

/* Command numbers */
#define SYNC0 0
#define SYNC1 1
#define SYNC2 2

#define PULSE 0
#define OPEN 1
#define CLOSE 2
#define ENABLE 4
#define SETA 5
#define SETV 6
#define SETO 7
#define VEL 11
#define RVEL 21
#define SETRA 23
#define SONAR 28
#define STOP 29
#define VEL2 32
#define GRIPPER 33
#define GRIPPERVAL 36
#define TTY2 42		// Added in AmigOS 1.2
#define GETAUX 43	// Added in AmigOS 1.2
#define BUMP_STALL 44
#define JOYDRIVE 47
#define GYRO 58         // Added in AROS 1.8
#define ROTKP 82        // Added in P2OS1.M
#define ROTKV 83        // Added in P2OS1.M
#define ROTKI 84        // Added in P2OS1.M
#define TRANSKP 85      // Added in P2OS1.M
#define TRANSKV 86      // Added in P2OS1.M
#define TRANSKI 87      // Added in P2OS1.M
#define TTY3 66		// Added in AmigOS 1.3
#define GETAUX2 67	// Added in AmigOS 1.3
#define ARM_INFO 70
#define ARM_STATUS 71
#define ARM_INIT 72
#define ARM_CHECK 73
#define ARM_POWER 74
#define ARM_HOME 75
#define ARM_PARK 76
#define ARM_POS 77
#define ARM_SPEED 78
#define ARM_STOP 79
#define ARM_AUTOPARK 80
#define ARM_GRIPPARK 81
#define SOUND 90
#define PLAYLIST 91

/* Server Information Packet (SIP) types */
#define STATUSSTOPPED	0x32
#define STATUSMOVING	0x33
#define	ENCODER		0x90
#define SERAUX		0xB0
#define SERAUX2		0xB8	// Added in AmigOS 1.3
#define GYROPAC         0x98    // Added AROS 1.8
#define ARMPAC    160   // ARMpac
#define ARMINFOPAC  161   // ARMINFOpac
//#define PLAYLIST	0xD0

/* Argument types */
#define ARGINT		0x3B	// Positive int (LSB, MSB)
#define ARGNINT		0x1B	// Negative int (LSB, MSB)
#define ARGSTR		0x2B	// String (Note: 1st byte is length!!)

/* gripper stuff */
#define GRIPopen   1
#define GRIPclose  2
#define GRIPstop   3
#define LIFTup     4
#define LIFTdown   5
#define LIFTstop   6
#define GRIPstore  7
#define GRIPdeploy 8
#define GRIPhalt   15
#define GRIPpress  16
#define LIFTcarry  17

/* CMUcam stuff */
#define CMUCAM_IMAGE_WIDTH	80
#define CMUCAM_IMAGE_HEIGHT	143
#define CMUCAM_MESSAGE_LEN	10

/* conection stuff */
#define DEFAULT_P2OS_PORT "/dev/ttyS0"
#define DEFAULT_P2OS_TCP_REMOTE_HOST "localhost"
#define DEFAULT_P2OS_TCP_REMOTE_PORT 8101

typedef struct ArmJoint
{
    char speed;
    unsigned char home;
    unsigned char min;
    unsigned char centre;
    unsigned char max;
    unsigned char ticksPer90;
} ArmJoint;

typedef struct SIP
{
 //private:
    int PositionChange;//( unsigned short, unsigned short );
    int param_idx; // index of our robot's data in the parameter table

 //public:
  // these values are returned in every standard SIP
    int lwstall, rwstall;
    unsigned char status, battery, sonarreadings, analog, digin, digout;
    unsigned short ptu, compass, timer, rawxpos;
    unsigned short rawypos, frontbumpers, rearbumpers;
    short angle, lvel, rvel, control;
    unsigned short sonars[PLAYER_SONAR_MAX_SAMPLES];
    int xpos, ypos;
    int x_offset,y_offset,angle_offset;

  // these values are returned in a CMUcam serial string extended SIP
  // (in host byte-order)
    unsigned short blobmx, blobmy;    // Centroid
    unsigned short blobx1, blobx2, bloby1, bloby2;    // Bounding box
    unsigned short blobarea, blobconf;    // Area and confidence
    unsigned int   blobcolor;

  // This value is filled by ParseGyro()
    int32_t gyro_rate;

  // This information comes from the ARMpac and ARMINFOpac packets
    int armPowerOn, armConnected;
    int armJointMoving[6];
    unsigned char armJointPos[6];
    double armJointPosRads[6];
    unsigned char armJointTargetPos[6];
    char *armVersionString;
    unsigned char armNumJoints;
    ArmJoint *armJoints;
} SIP;

typedef struct player_p2os_data
{
  player_position2d_data_t position;
  player_sonar_data_t sonar;
  player_gripper_data_t gripper;
  player_power_data_t power;
  player_bumper_data_t bumper;
  player_dio_data_t dio;
  player_aio_data_t aio;
  player_blobfinder_data_t blobfinder;
  player_position2d_data_t compass;
  player_position2d_data_t gyro;
  player_actarray_data_t actarray;
} __attribute__ ((packed)) player_p2os_data_t;

//SIP sip;

// Forward declaration of the KineCalc_Base class declared in kinecalc_base.h
//class KineCalc;

typedef struct  P2OS //: public Driver
{
  //private:
    player_p2os_data_t p2os_data;

    player_devaddr_t position_id;
    player_devaddr_t sonar_id;
    player_devaddr_t aio_id;
    player_devaddr_t dio_id;
    player_devaddr_t gripper_id;
    player_devaddr_t bumper_id;
    player_devaddr_t power_id;
    player_devaddr_t compass_id;
    player_devaddr_t gyro_id;
    player_devaddr_t blobfinder_id;
    player_devaddr_t sound_id;
    player_devaddr_t actarray_id;
    player_devaddr_t limb_id;

    // bookkeeping to only send new gripper I/O commands
    int sent_gripper_cmd;
    player_gripper_cmd_t last_gripper_cmd;

    // Same for actarray commands
    int last_actarray_cmd_was_pos;
    player_actarray_position_cmd_t last_actarray_pos_cmd;
    player_actarray_home_cmd_t last_actarray_home_cmd;

    // bookkeeping to only send new sound I/O commands
    int sent_sound_cmd;
    player_sound_cmd_t last_sound_cmd;
    // PID settings
    int rot_kp, rot_kv, rot_ki, trans_kp, trans_kv, trans_ki;

    //mutex para bloquear la lectura del laser de forma correcta
    pthread_mutexattr_t mutexattr;
    pthread_mutex_t mutex;

    int position_subscriptions;
    int sonar_subscriptions;
    int actarray_subscriptions;

    SIP* sippacket;

    int param_idx;  // index in the RobotParams table for this robot
    int direct_wheel_vel_control; // false -> separate trans and rot vel
    int psos_fd;               // p2os device file descriptor
    const char* psos_serial_port; // name of serial port device
    int psos_use_tcp;    // use TCP port instead of serial port
    const char* psos_tcp_host;  // hostname to use if using TCP
    int psos_tcp_port;  // remote port to use if using TCP

   // struct timeval lastblob_tv;

    // Max motor speeds (mm/sec,deg/sec)
    int motor_max_speed;
    int motor_max_turnspeed;

    // Bound the command velocities
    int use_vel_band;

    // Max motor accel/decel (mm/sec/sec, deg/sec/sec)
    short motor_max_trans_accel, motor_max_trans_decel;
    short motor_max_rot_accel, motor_max_rot_decel;

    int radio_modemp; // are we using a radio modem?
    int joystickp; // are we using a joystick?
    int bumpstall; // should we change the bumper-stall behavior?

    float pulse; // Pulse time
    double lastPulseTime; // Last time of sending a pulse or command to the robot

}P2OS;

P2OS p2os;

int SendReceive(P2OSPacket* pkt, int publish_data);
void ResetRawPositions();
// toggle sonars on/off, according to val
void ToggleSonarPower(unsigned char val);
// toggle motors on/off, according to val
void ToggleMotorPower(unsigned char val);

void PutData();
void HandlePositionCommand(player_position2d_cmd_vel_t position_cmd);
void HandleGripperCommand(player_gripper_cmd_t gripper_cmd);
void HandleSoundCommand(player_sound_cmd_t sound_cmd);

/////////////////
// Actarray stuff
inline double TicksToDegrees (int joint, unsigned char ticks);
inline unsigned char DegreesToTicks (int joint, double degrees);
inline double TicksToRadians (int joint, unsigned char ticks);
inline unsigned char RadiansToTicks (int joint, double rads);
inline double RadsPerSectoSecsPerTick (int joint, double speed);
inline double SecsPerTicktoRadsPerSec (int joint, double secs);
void ToggleActArrayPower (unsigned char val, int lock );   // Toggle actarray power on/off
void SetActArrayJointSpeed (int joint, double speed);             // Set a joint speed
void HandleActArrayPosCmd (player_actarray_position_cmd_t cmd);
void HandleActArrayHomeCmd (player_actarray_home_cmd_t cmd);

void SendPulse ();
void SendVel ();

int Subscribe(player_devaddr_t id);
int Unsubscribe(player_devaddr_t id);

int initP2os(char * port);

//     int Setup();
int p2osShutdown();

/* toggle motors on/off */
void p2osMotorOn();
void p2osMotorOff();
/* toggle sonar on/off */
void p2osSonarOn();
void p2osSonarOff();

/* Actualiza los valors del sippacket. Esta función debe ser periodica (todo el rato en un bucle)*/
void p2osGetValues();

//Funciones para el acceso al paquete SIP desde ADA
int p2osGetLwstall();
int p2osGetRwstall();
unsigned char p2osGetStatus();
unsigned char p2osGetBattery();
unsigned char p2osGetSonarreadings();
unsigned char p2osGetAnalog();
unsigned char p2osGetDigin();
unsigned char p2osGetDigout();
unsigned short p2osGetPtu();
unsigned short p2osGetCompass();
unsigned short p2osGetTimer();
unsigned short p2osGetRawxpos();
unsigned short p2osGetRawypos();
unsigned short p2osGetFrontbumpers();
unsigned short p2osGetRearbumpers();

short p2osGetLvel();
short p2osGetRvel();
short p2osGetControl();
unsigned short p2osGetSonar(int I);
float p2osGetXpos();
float p2osGetYpos();
float p2osGetAngle();

int p2osGetX_offset();
int p2osGetY_offset();
int p2osGetAngle_offset();

float p2osGetXSpeed();
float p2osGetYawSpeed();
//muestra la informacion del paquete SIP
void PrintP2OS_SIP();

//Establece la velocidad de translacion y rotacion del robot
void SetSpeed(float trans, float rot);
void lockP2os();
void unlockP2os();

#endif
