/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                   's i p'
 *
 *                                      C
 *
 *  File 'sip.c'                                        by F.J.Feijoo
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
 * $Id: sip.cc,v 1.20 2006/01/20 02:44:25 gerkey Exp $
 *
 * part of the P2OS parser.  methods for filling and parsing server
 * information packets (SIPs)
 */
#include <stdio.h>
#include <limits.h>
#include <stdbool.h>
#include <math.h>  /* rint(3) */
#include <stdlib.h> /* for abs() */
#include <unistd.h>
#include <string.h> // for memcpy
#include "sip.h"
#include "error.h"
#include <drivers/playercommon.h>
#include "robot_params.h"

void makeSIP(SIP *sip,int idx)
{
    int i;

    sip->param_idx = idx;
    for(i=0;i<PLAYER_SONAR_MAX_SAMPLES;i++)
      sip->sonars[i] = 0;

    sip->xpos = INT_MAX;
    sip->ypos = INT_MAX;

    // intialise some of the internal values
    sip->blobmx = sip->blobmy = sip->blobx1 = sip->blobx2 = sip->bloby1 = sip->bloby2 = sip->blobarea = sip->blobconf = sip->blobcolor = 0;
    sip->armPowerOn = sip->armConnected = false;
    sip->armVersionString = NULL;
    //sip->armJoints = NULL;
    //sip->armNumJoints = 0;
    //for ( i = 0; i < 6; ++i)
    //{
      //armJointMoving[i] = false;
      //armJointPos[i] = 0;
      //armJointPosRads[i] = 0;
      //armJointTargetPos[i] = 0;
    //}
}

void Fill(SIP *sip,player_p2os_data_t* data)
{
  int i;
  ///////////////////////////////////////////////////////////////
  // odometry


  // initialize position to current offset
  data->position.pos.px = sip->x_offset / 1e3;
  data->position.pos.py = sip->y_offset / 1e3;
  // now transform current position by rotation if there is one
  // and add to offset
  if(sip->angle_offset != 0)
  {
    double rot = DTOR(sip->angle_offset);    // convert rotation to radians
    data->position.pos.px +=  ((sip->xpos/1e3) * cos(rot) -
                               (sip->ypos/1e3) * sin(rot));
    data->position.pos.py +=  ((sip->xpos/1e3) * sin(rot) +
                               (sip->ypos/1e3) * cos(rot));
    data->position.pos.pa = DTOR(sip->angle_offset + sip->angle);
  }
  else
  {
    data->position.pos.px += sip->xpos / 1e3;
    data->position.pos.py += sip->ypos / 1e3;
    data->position.pos.pa = DTOR(sip->angle);
  }

  data->position.vel.px = (((sip->lvel) + (sip->rvel) ) / 2) / 1e3;
  data->position.vel.py = 0.0;
  data->position.vel.pa = ((double)(sip->rvel - sip->lvel) /
                           (2.0/PlayerRobotParams[sip->param_idx].DiffConvFactor));
  data->position.stall = (unsigned char)(sip->lwstall || sip->rwstall);

  ///////////////////////////////////////////////////////////////
  // compass
  memset(&(data->compass),0,sizeof(data->compass));
  data->compass.pos.pa = DTOR(sip->compass);
  //printf("compass fill %f\n",data->compass.pos.pa);

  ///////////////////////////////////////////////////////////////
  // gyro
  memset(&(data->gyro),0,sizeof(data->gyro));
  data->gyro.pos.pa = DTOR(sip->gyro_rate);

  ///////////////////////////////////////////////////////////////
  // sonar
  data->sonar.ranges_count = PlayerRobotParams[sip->param_idx].SonarNum;
  for( i=0;i<MIN(PlayerRobotParams[sip->param_idx].SonarNum,PLAYER_SONAR_MAX_SAMPLES);i++)
    data->sonar.ranges[i] = sip->sonars[i] / 1e3;

  ///////////////////////////////////////////////////////////////
  // gripper
  data->gripper.state = (unsigned char)(sip->timer >> 8);
  data->gripper.beams = (unsigned char)sip->digin;

  ///////////////////////////////////////////////////////////////
  // bumper
  data->bumper.bumpers_count = 10;
  int j = 0;
  for( i=4;i>=0;i--)
    data->bumper.bumpers[j++] =
      (unsigned char)((sip->frontbumpers >> i) & 0x01);
  for( i=4;i>=0;i--)
    data->bumper.bumpers[j++] =
      (unsigned char)((sip->rearbumpers >> i) & 0x01);

  ///////////////////////////////////////////////////////////////
  // power
  // set the bits that indicate which fields we're using
  data->power.valid = PLAYER_POWER_MASK_VOLTS | PLAYER_POWER_MASK_PERCENT;
  data->power.volts = sip->battery / 1e1;
  data->power.percent = 1e2 * (data->power.volts / P2OS_NOMINAL_VOLTAGE);

  ///////////////////////////////////////////////////////////////
  // digital I/O
  data->dio.count = (unsigned char)8;
  data->dio.digin = (unsigned int)sip->digin;

  ///////////////////////////////////////////////////////////////
  // analog I/O
  //TODO: should do this smarter, based on which analog input is selected
  data->aio.voltages_count = (unsigned char)1;
  data->aio.voltages[0] = (sip->analog / 255.0) * 5.0;

  /* CMUcam blob tracking interface.  The CMUcam only supports one blob
  ** (and therefore one channel too), so everything else is zero.  All
  ** data is storde in the blobfinder packet in Network byte order.
  ** Note: In CMUcam terminology, X is horizontal and Y is vertical, with
  ** (0,0) being TOP-LEFT (from the camera's perspective).  Also,
  ** since CMUcam doesn't have range information, but does have a
  ** confidence value, I'm passing it back as range. */
  memset(data->blobfinder.blobs,0,
         sizeof(player_blobfinder_blob_t)*PLAYER_BLOBFINDER_MAX_BLOBS);
  data->blobfinder.width = CMUCAM_IMAGE_WIDTH;
  data->blobfinder.height = CMUCAM_IMAGE_HEIGHT;

  if (sip->blobarea > 1)    // With filtering, definition of track is 2 pixels
  {
    data->blobfinder.blobs_count = 1;
    data->blobfinder.blobs[0].color = sip->blobcolor;
    data->blobfinder.blobs[0].x = sip->blobmx;
    data->blobfinder.blobs[0].y = sip->blobmy;
    data->blobfinder.blobs[0].left = sip->blobx1;
    data->blobfinder.blobs[0].right = sip->blobx2;
    data->blobfinder.blobs[0].top = sip->bloby1;
    data->blobfinder.blobs[0].bottom = sip->bloby2;
    data->blobfinder.blobs[0].area = sip->blobarea;
    data->blobfinder.blobs[0].range = sip->blobconf;
  }
  else
    data->blobfinder.blobs_count = 0;

/*
  // Fill in arm data
  memset (data->actarray.actuators, 0, sizeof (player_actarray_actuator_t) * PLAYER_ACTARRAY_NUM_ACTUATORS);
  data->actarray.actuators_count = armNumJoints;
  for (int ii = 0; ii < armNumJoints; ii++)
  {
    data->actarray.actuators[ii].position = armJointPosRads[ii];
    data->actarray.actuators[ii].speed = 0;
    // State is complex. It can be idle, moving, or stalled (we don't have brakes so don't need to worry about the brake state).
    // Moving means have moving state from status packet
    // Idle means have not moving state from status packet and are at target position
    // Stalled means have not moving state from status packet and are not at target position
    if (armJointMoving[ii])
      data->actarray.actuators[ii].state = PLAYER_ACTARRAY_ACTSTATE_MOVING;
    else
    {
      if (armJointPos[ii] == armJointTargetPos[ii])
        data->actarray.actuators[ii].state = PLAYER_ACTARRAY_ACTSTATE_IDLE;
      else
        data->actarray.actuators[ii].state = PLAYER_ACTARRAY_ACTSTATE_STALLED;
    }
  }
*/
}

int PositionChange( SIP *sip,unsigned short from, unsigned short to )
{
  int diff1, diff2;

  /* find difference in two directions and pick shortest */
  if ( to > from ) {
    diff1 = to - from;
    diff2 = - ( from + 4096 - to );
  }
  else {
    diff1 = to - from;
    diff2 = 4096 - from + to;
  }

  if ( abs(diff1) < abs(diff2) )
    return(diff1);
  else
    return(diff2);

}

void PrintSIP(SIP *sip)
{
  int i;

  printf("lwstall:%d rwstall:%d\n", sip->lwstall, sip->rwstall);

  printf("Front bumpers: ");
  for(i=0;i<5;i++) {
    printf("%d", (sip->frontbumpers >> i) & 0x01 );
  }
  puts("");

  printf("Rear bumpers: ");
  for(i=0;i<5;i++) {
    printf("%d", (sip->rearbumpers >> i) & 0x01 );
  }
  puts("");

  printf("status: 0x%x analog: %d ", sip->status, sip->analog);
  printf("digin: ");
  for(i=0;i<8;i++) {
    printf("%d", (sip->digin >> (7-i) ) & 0x01);
  }
  printf(" digout: ");
  for(i=0;i<8;i++) {
    printf("%d", (sip->digout >> (7-i) ) & 0x01);
  }
  puts("");
  printf("battery: %d compass: %d sonarreadings: %d\n", sip->battery, sip->compass, sip->sonarreadings);
  printf("xpos: %d ypos:%d ptu:%hu timer:%hu\n", sip->xpos, sip->ypos, sip->ptu, sip->timer);
  printf("angle: %d lvel: %d rvel: %d control: %d\n",sip->angle, sip->lvel, sip->rvel, sip->control);

  PrintSonars(sip);
  printf("DistConvFactor: %f\n",PlayerRobotParams[sip->param_idx].DistConvFactor);
  //PrintArmInfo (sip);
  //PrintArm (sip);
}

void PrintSonars(SIP *sip)
{
  int i;
  printf("Sonars: ");
  for(i = 0; i < 16; i++){
    printf("%hu ", sip->sonars[i]);
  }
  puts("");
}

void PrintArm (SIP *sip)
{int ii;
    printf ("Arm power is %s\tArm is %sconnected\n", (sip->armPowerOn ? "on" : "off"), (sip->armConnected ? "" : "not "));
    printf ("Arm joint status:\n");
    for (ii = 0; ii < 6; ii++)
        printf ("Joint %d   %s   %d\n", ii + 1, (sip->armJointMoving[ii] ? "Moving " : "Stopped"), sip->armJointPos[ii]);
}

void PrintArmInfo (SIP *sip)
{int ii;
    printf ("Arm version:\t%s\n", sip->armVersionString);
    printf ("Arm has %d joints:\n", sip->armNumJoints);
    printf ("  |\tSpeed\tHome\tMin\tCentre\tMax\tTicks/90\n");
    for (ii = 0; ii < sip->armNumJoints; ii++)
        printf ("%d |\t%d\t%d\t%d\t%d\t%d\t%d\n", ii, sip->armJoints[ii].speed, sip->armJoints[ii].home, sip->armJoints[ii].min, sip->armJoints[ii].centre, sip->armJoints[ii].max, sip->armJoints[ii].ticksPer90);
}

void Parse( SIP *sip,unsigned char *buffer )
{
  unsigned char i;
  int cnt = 0, change;
  unsigned short newxpos, newypos;


  sip->status = buffer[cnt];
  cnt += sizeof(unsigned char);
  /*
   * Remember P2OS uses little endian:
   * for a 2 byte short (called integer on P2OS)
   * byte0 is low byte, byte1 is high byte
   * The following code is host-machine endian independant
   * Also we must or (|) bytes together instead of casting to a
   * short * since on ARM architectures short * must be even byte aligned!
   * You can get away with this on a i386 since shorts * can be
   * odd byte aligned. But on ARM, the last bit of the pointer will be ignored!
   * The or'ing will work on either arch.
   */
  newxpos = ((buffer[cnt] | (buffer[cnt+1] << 8))
         & 0xEFFF) % 4096; /* 15 ls-bits */

  if (sip->xpos!=INT_MAX) {
    change = (int) rint(PositionChange( sip, sip->rawxpos, newxpos ) *
            PlayerRobotParams[sip->param_idx].DistConvFactor);
//     if (abs(change)>100)
//       PLAYER_WARN1("invalid odometry change [%d]; odometry values are tainted", change);
//     else
      sip->xpos += change;
  }
  else
    sip->xpos = 0;

  sip->rawxpos = newxpos;
  cnt += sizeof(short);

  newypos = ((buffer[cnt] | (buffer[cnt+1] << 8))
         & 0xEFFF) % 4096; /* 15 ls-bits */

  if (sip->ypos!=INT_MAX) {
    change = (int) rint(PositionChange(sip, sip->rawypos, newypos ) *
            PlayerRobotParams[sip->param_idx].DistConvFactor);
//     if (abs(change)>100)
//       PLAYER_WARN1("invalid odometry change [%d]; odometry values are tainted", change);
//     else
      sip->ypos += change;
  }
  else
    sip->ypos = 0;

  sip->rawypos = newypos;
  cnt += sizeof(short);

  sip->angle = (short)
    rint(((short)(buffer[cnt] | (buffer[cnt+1] << 8))) *
     PlayerRobotParams[sip->param_idx].AngleConvFactor * 180.0/M_PI);
  cnt += sizeof(short);

  sip->lvel = (short)
    rint(((short)(buffer[cnt] | (buffer[cnt+1] << 8))) *
     PlayerRobotParams[sip->param_idx].VelConvFactor);
  cnt += sizeof(short);

  sip->rvel = (short)
    rint(((short)(buffer[cnt] | (buffer[cnt+1] << 8))) *
     PlayerRobotParams[sip->param_idx].VelConvFactor);
  cnt += sizeof(short);

  sip->battery = buffer[cnt];
  cnt += sizeof(unsigned char);

  sip->lwstall = buffer[cnt] & 0x01;
  sip->rearbumpers = buffer[cnt] >> 1;
  cnt += sizeof(unsigned char);

  sip->rwstall = buffer[cnt] & 0x01;
  sip->frontbumpers = buffer[cnt] >> 1;
  cnt += sizeof(unsigned char);

  sip->control = (short)
    rint(((short)(buffer[cnt] | (buffer[cnt+1] << 8))) *
     PlayerRobotParams[sip->param_idx].AngleConvFactor);
  cnt += sizeof(short);

  sip->ptu = (buffer[cnt] | (buffer[cnt+1] << 8));
  cnt += sizeof(short);

  //compass = buffer[cnt]*2;
  if(buffer[cnt] != 255 && buffer[cnt] != 0 && buffer[cnt] != 181){
    sip->compass = (buffer[cnt]-1)*2;
    //fprintf(stderr,"COMPAS SIP %d \n",sip->compass);
  }
  cnt += sizeof(unsigned char);

  sip->sonarreadings = buffer[cnt];
  cnt += sizeof(unsigned char);

  //printf("%hu sonar readings:\n", sonarreadings);
  for( i = 0;i < sip->sonarreadings;i++) {
    sip->sonars[buffer[cnt]]=   (unsigned short)
      rint((buffer[cnt+1] | (buffer[cnt+2] << 8)) *
       PlayerRobotParams[sip->param_idx].RangeConvFactor);
    //printf("%d %hu:",buffer[cnt],*((unsigned short *)&buffer[cnt+1]));
    //
    //printf("%hu %hu %hu\n", buffer[cnt], buffer[cnt+1], buffer[cnt+2]);
    //printf("index %d value %hu\n", buffer[cnt], sonars[buffer[cnt]]);
    cnt += 3*sizeof(unsigned char);
  }
  //printf("\n");

  sip->timer = (buffer[cnt] | (buffer[cnt+1] << 8));
  cnt += sizeof(short);

  sip->analog = buffer[cnt];
  cnt += sizeof(unsigned char);

  sip->digin = buffer[cnt];
  cnt += sizeof(unsigned char);

  sip->digout = buffer[cnt];
  cnt += sizeof(unsigned char);
  // for debugging:
  //PrintSIP(sip);
}


/** Parse a SERAUX SIP packet.  For a CMUcam, this will have blob
 **  tracking messages in the format (all one-byte values, no spaces):
 **
 **      255 M mx my x1 y1 x2 y2 pixels confidence  (10-bytes)
 **
 ** Or color info messages of the format:
 **
 **      255 S Rval Gval Bval Rvar Gvar Bvar    (8-bytes)
 */
void ParseSERAUX(SIP *sip, unsigned char *buffer )
{
  unsigned char type = buffer[1];
  if (type != SERAUX && type != SERAUX2)
  {
    // Really should never get here...
    printf("ERROR: Attempt to parse non SERAUX packet as serial data.\n");
    return;
  }

  int len  = (int)buffer[0]-3;      // Get the string length

  /* First thing is to find the beginning of last full packet (if possible).
  ** If there are less than CMUCAM_MESSAGE_LEN*2-1 bytes (19), we're not
  ** guaranteed to have a full packet.  If less than CMUCAM_MESSAGE_LEN
  ** bytes, it is impossible to have a full packet.
  ** To find the beginning of the last full packet, search between bytes
  ** len-17 and len-8 (inclusive) for the start flag (255).
  */
  int ix;
  for (ix = (len>18 ? len-17 : 2); ix<=len-8; ix++)
    if (buffer[ix] == 255)
      break;        // Got it!
  if (len < 10 || ix > len-8) {
    printf("ERROR: Failed to get a full blob tracking packet.\n");
    return;
  }

  // There is a special 'S' message containing the tracking color info
  if (buffer[ix+1] == 'S')
  {
     // Color information (track color)
     printf("Tracking color (RGB):  %d %d %d\n"
            "       with variance:  %d %d %d\n",
              buffer[ix+2], buffer[ix+3], buffer[ix+4],
              buffer[ix+5], buffer[ix+6], buffer[ix+7]);
     sip->blobcolor = buffer[ix+2]<<16 | buffer[ix+3]<<8 | buffer[ix+4];
     return;
  }

  // Tracking packets with centroid info are designated with a 'M'
  if (buffer[ix+1] == 'M')
  {
     // Now it's easy.  Just parse the packet.
     sip->blobmx    = buffer[ix+2];
     sip->blobmy    = buffer[ix+3];
     sip->blobx1    = buffer[ix+4];
     sip->bloby1    = buffer[ix+5];
     sip->blobx2    = buffer[ix+6];
     sip->bloby2    = buffer[ix+7];
     sip->blobconf  = buffer[ix+9];
     // Xiaoquan Fu's calculation for blob area (max 11297).
     sip->blobarea  = (sip->bloby2 - sip->bloby1 +1)*(sip->blobx2 - sip->blobx1 + 1)*sip->blobconf/255;
     return;
  }

  printf("ERROR: Unknown blob tracker packet type: %c\n", buffer[ix+1]);
  return;
}

// Parse a set of gyro measurements.  The buffer is formatted thusly:
//     length (2 bytes), type (1 byte), count (1 byte)
// followed by <count> pairs of the form:
//     rate (2 bytes), temp (1 byte)
// <rate> falls in [0,1023]; less than 512 is CCW rotation and greater
// than 512 is CW
void ParseGyro(SIP *sip,unsigned char* buffer)
{
  // Get the message length (account for the type byte and the 2-byte
  // checksum)
  int len  = (int)buffer[0]-3;

  unsigned char type = buffer[1];
  if(type != GYROPAC)
  {
    // Really should never get here...
    PLAYER_ERROR("ERROR: Attempt to parse non GYRO packet as gyro data.\n");
    return;
  }

  if(len < 1)
  {
    PLAYER_WARN("Couldn't get gyro measurement count");
    return;
  }

  // get count
  int count = (int)buffer[2];

  // sanity check
  if((len-1) != (count*3))
  {
    PLAYER_WARN("Mismatch between gyro measurement count and packet length");
    return;
  }

  // Actually handle the rate values.  Any number of things could (and
  // probably should) be done here, like filtering, calibration, conversion
  // from the gyro's arbitrary units to something meaningful, etc.
  //
  // As a first cut, we'll just average all the rate measurements in this
  // set, and ignore the temperatures.
  float ratesum = 0;
  int bufferpos = 3;
  unsigned short rate;
  unsigned char temp;
  int i;
  for(i=0; i<count; i++)
  {
    rate = (unsigned short)(buffer[bufferpos++]);
    rate |= buffer[bufferpos++] << 8;
    temp = bufferpos++;

    ratesum += rate;
  }

  int32_t average_rate = (int32_t)rint(ratesum / (float)count);

  // store the result for sending
  sip->gyro_rate = average_rate;
}

void ParseArm (SIP *sip, unsigned char *buffer)
{
    int length = (int) buffer[0] - 2;

    if (buffer[1] != ARMPAC)
    {
        PLAYER_ERROR ("ERROR: Attempt to parse a non ARM packet as arm data.\n");
        return;
    }

    if (length < 1 || length != 9)
    {
        PLAYER_WARN ("ARMpac length incorrect size");
        return;
    }

    unsigned char status = buffer[2];
    if (status & 0x01)
        sip->armPowerOn = true;     // Power is on
    else
        sip->armPowerOn = false;        // Power is off

    if (status & 0x02)
        sip->armConnected = true;   // Connection is established
    else
        sip->armConnected = false;  // Connection is not established

    unsigned char motionStatus = buffer[3];
    if (motionStatus & 0x01)
        sip->armJointMoving[0] = true;
    if (motionStatus & 0x02)
        sip->armJointMoving[1] = true;
    if (motionStatus & 0x04)
        sip->armJointMoving[2] = true;
    if (motionStatus & 0x08)
        sip->armJointMoving[3] = true;
    if (motionStatus & 0x10)
        sip->armJointMoving[4] = true;
    if (motionStatus & 0x20)
        sip->armJointMoving[5] = true;

    memcpy (sip->armJointPos, &buffer[4], 6);
    memset (sip->armJointPosRads, 0, 6 * sizeof (double));
}

/*void ParseArmInfo (SIP *sip, unsigned char *buffer)
{
    int length = (int) buffer[0] - 2;
    if (buffer[1] != ARMINFOPAC)
    {
        PLAYER_ERROR ("ERROR: Attempt to parse a non ARMINFO packet as arm info.\n");
        return;
    }

    if (length < 1)
    {
        PLAYER_WARN ("ARMINFOpac length bad size");
        return;
    }

    // Copy the version string
    if (sip->armVersionString)
        free (sip->armVersionString);
        // strndup() isn't available everywhere (e.g., Darwin)
    //armVersionString = strndup ((char*) &buffer[2], length);      // Can't be any bigger than length
        sip->armVersionString = (char*)calloc(length+1,sizeof(char));
        assert(sip->armVersionString);
        strncpy(sip->armVersionString,(char*)&buffer[2], length);
    int dataOffset = strlen (sip->armVersionString) + 3;        // +1 for packet size byte, +1 for packet ID, +1 for null byte

    sip->armNumJoints = buffer[dataOffset];
    if (sip->armJoints)
        delete[] sip->armJoints;
    if (sip->armNumJoints <= 0)
        return;
    sip->armJoints = new ArmJoint[armNumJoints];
    dataOffset += 1;
    for (int ii = 0; ii < armNumJoints; ii++)
    {
        armJoints[ii].speed = buffer[dataOffset + (ii * 6)];
        armJoints[ii].home = buffer[dataOffset + (ii * 6) + 1];
        armJoints[ii].min = buffer[dataOffset + (ii * 6) + 2];
        armJoints[ii].centre = buffer[dataOffset + (ii * 6) + 3];
        armJoints[ii].max = buffer[dataOffset + (ii * 6) + 4];
        armJoints[ii].ticksPer90 = buffer[dataOffset + (ii * 6) + 5];
    }

}*/
