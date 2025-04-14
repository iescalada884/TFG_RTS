/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                   'p 2 o s'
 *
 *                                      C
 *
 *  File 'p2os.c'                                        by F.J.Feijoo
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
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <pthread.h>
#include <debug_marte.h>
#include <sys/pio.h>
#include <math.h>
#include <drivers/osdep.h> //para ntohs
#include <drivers/playercommon.h>
#include <drivers/p2os.h>
#include "robot_params.h"
#include "sip.h"

#define O_SYNC     0x0400

void configurar()
{
// zero ids, so that we'll know later which interfaces were requested
  memset(&p2os.position_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.sonar_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.aio_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.dio_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.gripper_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.bumper_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.power_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.compass_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.gyro_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.blobfinder_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.sound_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.actarray_id, 0, sizeof(player_devaddr_t));
  memset(&p2os.limb_id, 0, sizeof(player_devaddr_t));

  p2os.position_subscriptions = p2os.sonar_subscriptions = p2os.actarray_subscriptions = 0;
 // p2os->pulse = -1;

  // intialise members
  p2os.sippacket = NULL;

  // esto podria ir en una funcion anterior con un fichero de configuracion:
    initialize_robot_params();

//kiko, les pongo los valores por defecto si no estan instanciados en el fichero de configuracion!
// Read config file options
  p2os.bumpstall = -1;
  p2os.pulse = -1;
  p2os.rot_kp = -1;
  p2os.rot_kv = -1;
  p2os.rot_ki = -1;
  p2os.trans_kp = -1;
  p2os.trans_kv = -1;
  p2os.trans_ki = -1;

  p2os.psos_serial_port = DEFAULT_P2OS_PORT;
  //this->psos_use_tcp = cf->ReadBool(section, "use_tcp", false); // TODO after ReadBool added
  p2os.psos_use_tcp = 0;
  p2os.psos_tcp_host =  DEFAULT_P2OS_TCP_REMOTE_HOST;
  p2os.psos_tcp_port =  DEFAULT_P2OS_TCP_REMOTE_PORT;
  p2os.radio_modemp = 0;
  p2os.joystickp = 0;
  p2os.direct_wheel_vel_control = 0;
  p2os.motor_max_speed = (int)rint(1e3 *  MOTOR_DEF_MAX_SPEED);
  p2os.motor_max_turnspeed = (int)rint(RTOD(MOTOR_DEF_MAX_TURNSPEED));
  p2os.motor_max_trans_accel = (short)rint(1e3 * 0);
  p2os.motor_max_trans_decel = (short)rint(1e3 * 0);
  p2os.motor_max_rot_accel = (short)rint(RTOD(0));
  p2os.motor_max_rot_decel = (short)rint(RTOD(0));

  p2os.use_vel_band =  0;
  p2os.psos_fd = -1;

  p2os.sent_gripper_cmd = FALSE;
  p2os.last_actarray_cmd_was_pos = TRUE;
  memset (&p2os.last_actarray_pos_cmd, 0, sizeof (player_actarray_position_cmd_t));
  memset (&p2os.last_actarray_home_cmd, 0, sizeof (player_actarray_home_cmd_t));

}

int initP2os (char * port)
{

  struct timespec ts;
 // int bauds[] = {B9600, B38400, B19200, B115200, B57600};
 // int numbauds = sizeof(bauds);
  //int currbaud = 0;

  int num_sync_attempts, psos_state;
  unsigned char command;
  P2OSPacket packet, receivedpacket;
  //char c;
  char name[20], type[20], subtype[20];
  int cnt;
  int sent_close=0;
  termios_t new_termios;
// kiko y sus intentos: INICIALIZACION (SETUP)

   configurar();


  pthread_mutexattr_init (&p2os.mutexattr);
  pthread_mutexattr_setprotocol(&p2os.mutexattr,PTHREAD_PRIO_INHERIT);
  pthread_mutex_init (&p2os.mutex, &p2os.mutexattr);


// Serial port:
    p2os.psos_serial_port = port;

    printf("P2OS connection opening serial port %s...\n",p2os.psos_serial_port);
    //ioctl(p2os.psos_fd,SERIAL_FLUSH,NULL);

//    if((p2os.psos_fd= open("/dev/ttyS0", O_RDWR)) < 0 )
    if((p2os.psos_fd= open(p2os.psos_serial_port, O_RDWR)) < 0 )
    {
      perror("P2OS::InitP2os():open():");
      return(1);
    }

   ioctl(p2os.psos_fd,SERIAL_GETATTR,((void *)&new_termios));
   new_termios.ospeed = B9600;
   new_termios.ispeed = B9600;
   ioctl(p2os.psos_fd,SERIAL_SETATTR,((void *)&new_termios));



 if( ioctl(p2os.psos_fd, SERIAL_FLUSH, NULL) < 0)
            {
            perror("ERROR vaciando la linea serie");
            close(p2os.psos_fd);
            p2os.psos_fd = -1;
            return(1);
            }

  printf("empieza sincronizacion \n");
  psos_state = NO_SYNC;
  num_sync_attempts = 3;
  while(psos_state != READY)
  {
    switch(psos_state)
    {
      case NO_SYNC:

    command = SYNC0;
        Build(&packet,&command, 1);
        Send(p2os.psos_fd,packet);
        printf("SYNCO: \n");
    ts.tv_sec = 0;
        ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
        nanosleep (&ts, NULL);

        break;
      case AFTER_FIRST_SYNC:
    printf("AFTER_FIRST_SYNC: \n");
        command = SYNC1;
        Build(&packet,&command, 1);
        Send(p2os.psos_fd,packet);
        break;
      case AFTER_SECOND_SYNC:
    printf("AFTER_SECOND_SYNC: \n");
        command = SYNC2;
        Build(&packet,&command, 1);
        Send(p2os.psos_fd,packet);
        break;
      default:
        puts("P2OS::InitP2os():shouldn't be here...");
        break;
    }

    ts.tv_sec = 0;
    ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
    nanosleep (&ts, NULL);

    //printf("intento recibir!\n");
    if(Receive(p2os.psos_fd,&receivedpacket))
    {
      //sino consigo recibir lo intento mas veces ...
      if((psos_state == NO_SYNC) && (num_sync_attempts >= 0))
      {
        num_sync_attempts--;

        ts.tv_sec = 0;
        ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
        nanosleep (&ts, NULL);
        continue;
      }
      else{
        if( ioctl(p2os.psos_fd, SERIAL_FLUSH, NULL) < 0)
            {
            perror("ERROR cada vez que se envia se hace flush");
            close(p2os.psos_fd);
            p2os.psos_fd = -1;
            return(1);
            }
    printf("INTENTAR OTRAS VELOCIDADES!!\n");
    }
    }


     switch(receivedpacket.packet[3])
    {
      case SYNC0:
        psos_state = AFTER_FIRST_SYNC;
    printf("entro SYNC0 \n");
        break;
      case SYNC1:
        psos_state = AFTER_SECOND_SYNC;
    printf("entro SYNC1 \n");
        break;
      case SYNC2:
        psos_state = READY;
    printf("entro SYNC2 \n");
        break;
      default:
        // maybe P2OS is still running from last time.  let's try to CLOSE
        // and reconnect
        if(!sent_close)
        {
//por si acaso, normalmente solo close!
          printf("sending STOP\n");
    command = STOP;
    Build(&packet,&command, 1);
    Send(p2os.psos_fd,packet);
    ts.tv_sec = 0;
    ts.tv_nsec = 2*P2OS_CYCLETIME_USEC*1000;
    nanosleep (&ts, NULL);


          printf("sending CLOSE\n");
          command = CLOSE;
          Build(&packet, &command, 1);
          Send(p2os.psos_fd,packet);
          sent_close = TRUE;
      ts.tv_sec = 0;
          ts.tv_nsec = 2*P2OS_CYCLETIME_USEC*1000;
          nanosleep (&ts, NULL);
          //tcflush(this->psos_fd,TCIFLUSH);
    //se supone que esto hace flush para la linea serie en Marte
      if( ioctl(p2os.psos_fd, SERIAL_FLUSH, NULL) < 0)
            {
            perror("ERROR cada vez que se envia se hace flush");
            close(p2os.psos_fd);
            p2os.psos_fd = -1;
            return(1);
            }
          psos_state = NO_SYNC;
        }
        break;
    }

    ts.tv_sec = 0;
    ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
    nanosleep (&ts, NULL);
  }



if(psos_state != READY)
  {
    close(p2os.psos_fd);
    p2os.psos_fd = -1;
    return(1);
  }

  cnt = 4;
  cnt += sprintf(name, "%s", &receivedpacket.packet[cnt]);
  cnt++;
  cnt += sprintf(type, "%s", &receivedpacket.packet[cnt]);
  cnt++;
  cnt += sprintf(subtype, "%s", &receivedpacket.packet[cnt]);
  cnt++;


  command = OPEN;
  Build(&packet,&command, 1);
  Send(p2os.psos_fd,packet);
  ts.tv_sec = 0;
  ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
  nanosleep (&ts, NULL);

  command = PULSE;
  Build(&packet,&command, 1);
  Send(p2os.psos_fd,packet);
  ts.tv_sec = 0;
  ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
  nanosleep (&ts, NULL);

  printf("Done.\n   Connected to %s, a %s %s\n", name, type, subtype);

 int i;
 //SIP sippacket;
 // now, based on robot type, find the right set of parameters
  for(i=0;i<PLAYER_NUM_ROBOT_TYPES;i++)
  {
    if(!strcasecmp(PlayerRobotParams[i].Class,type) &&
       !strcasecmp(PlayerRobotParams[i].Subclass,subtype))
    {
      makeSIP(p2os.sippacket,i);
//  printf("!!!!!!!DistConvFactor: %f\n",PlayerRobotParams[i].DistConvFactor);
//  get_char();
      break;
    }
  }
  if(i == PLAYER_NUM_ROBOT_TYPES)
  {
    fputs("P2OS: Warning: couldn't find parameters for this robot; "
            "using defaults\n",stderr);
    //makeSIP(p2os.sippacket,0);
    return (1);
  }

 printf("Numero de indice del robot %d\n",p2os.sippacket->param_idx);


  p2os.sippacket->x_offset = 0;
  p2os.sippacket->y_offset = 0;
  p2os.sippacket->angle_offset = 0;

  SendReceive((P2OSPacket*)NULL,false);


    ts.tv_sec = 0;
    ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
    nanosleep (&ts, NULL);
  // turn off the sonars at first
  ToggleSonarPower(0);
  ts.tv_sec = 0;
    ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
    nanosleep (&ts, NULL);



  ResetRawPositions();

      ts.tv_sec = 0;
      ts.tv_nsec = 2*P2OS_CYCLETIME_USEC*1000;
      nanosleep (&ts, NULL);

    ToggleMotorPower(1);

    ts.tv_sec = 0;
      ts.tv_nsec = 2*P2OS_CYCLETIME_USEC*1000;
      nanosleep (&ts, NULL);

  return 0;
}


int SendReceive(P2OSPacket* pkt, int publish_data)
{
  P2OSPacket packet;

  // zero the combined data buffer.  it will be filled with the latest data
  // by SIP::Fill()
//   memset(&(p2os.p2os_data),0,sizeof(player_p2os_data_t));
  if((p2os.psos_fd >= 0) ){ //KIKO sin esto funciona! && (p2os->sippacket != NULL)

    //printf("entro al if de sendReceive\n");
   if(pkt != NULL){
      //printf("entro la primera vez y no deberia\n");
      Send(p2os.psos_fd,*pkt);
    }
    /* receive a packet */
    if(Receive(p2os.psos_fd,&packet) < 0)
    {
      puts("RunPsosThread(): Receive errored");
      //pthread_exit(NULL);
    }

    if((packet.packet[0] == 0xFA) && (packet.packet[1] == 0xFB) &&
       ((packet.packet[3] == 0x30 || packet.packet[3] == 0x31) ||
        (packet.packet[3] == 0x32 || packet.packet[3] == 0x33) ||
        (packet.packet[3] == 0x34)))
    {
      /* It is a server packet, so process it */

      Parse( p2os.sippacket, &packet.packet[3] );

// kiko cambio para asegurar datos correctos completos.
    pthread_mutex_lock(&p2os.mutex);

    Fill(p2os.sippacket,&(p2os.p2os_data));

    pthread_mutex_unlock(&p2os.mutex);

//       if(publish_data)
//         PutData(p2os);
    }
    else if(packet.packet[0] == 0xFA && packet.packet[1] == 0xFB &&
            packet.packet[3] == SERAUX)
    {
       // This is an AUX serial packet
    }
    else if(packet.packet[0] == 0xFA && packet.packet[1] == 0xFB &&
            packet.packet[3] == SERAUX2)
    {
    //KIKO: de momento dejo la camara

/*      // This is an AUX2 serial packet
      if(blobfinder_id.interf)
      {
        // It is an extended SIP (blobfinder) packet, so process it
        // Be sure to pass data size too (packet[2])!

    ParseSERAUX(p2os->sippacket, &packet.packet[2] );
    Fill(p2os->sippacket,&(this->p2os_data));

        if(publish_data)
          PutData(p2os);


        P2OSPacket cam_packet;
        unsigned char cam_command[4];

//         ** We cant get the entire contents of the buffer,
//         ** and we cant just have P2OS send us the buffer on a regular basis.
//         ** My solution is to flush the buffer and then request exactly
//         ** CMUCAM_MESSAGE_LEN * 2 -1 bytes of data.  This ensures that
//         ** we will get exactly one full message, and it will be "current"
//         ** within the last 2 messages.  Downside is that we end up pitching
//         ** every other CMUCAM message.  Tradeoffs...
        // Flush
        cam_command[0] = GETAUX2;
        cam_command[1] = ARGINT;
        cam_command[2] = 0;
        cam_command[3] = 0;
        cam_packet.Build(cam_command, 4);
        this->SendReceive(&cam_packet,publish_data);

        // Reqest next packet
        cam_command[0] = GETAUX2;
        cam_command[1] = ARGINT;
        // Guarantee exactly 1 full message
        cam_command[2] = CMUCAM_MESSAGE_LEN * 2 -1;
        cam_command[3] = 0;
        cam_packet.Build(cam_command, 4);
        this->SendReceive(&cam_packet,publish_data);
        GlobalTime->GetTime(&lastblob_tv);  // Reset last blob packet time
      }
*/
    }
    else if(packet.packet[0] == 0xFA && packet.packet[1] == 0xFB &&
            ((packet.packet[3] == 0x50 || packet.packet[3] == 0x80) ||
//            (packet.packet[3] == 0xB0 || packet.packet[3] == 0xC0) ||
            (packet.packet[3] == 0xC0) ||
            (packet.packet[3] == 0xD0 || packet.packet[3] == 0xE0)))
    {
      // It is a vision packet from the old Cognachrome system

      // we don't understand these yet, so ignore
    }
    else if(packet.packet[0] == 0xFA && packet.packet[1] == 0xFB &&
            packet.packet[3] == GYROPAC)
    {
    printf("GYRO\n");
      if(p2os.gyro_id.interf)
      {
        /* It's a set of gyro measurements */

    ParseGyro(p2os.sippacket,&packet.packet[2]);
    Fill(p2os.sippacket,&(p2os.p2os_data));

//         if(publish_data)
//           PutData(p2os);

        /* Now, the manual says that we get one gyro packet each cycle,
         * right before the standard SIP.  So, we'll call SendReceive()
         * again (with no packet to send) to get the standard SIP.  There's
         * a definite danger of infinite recursion here if the manual
         * is wrong.
         */
    //SendReceive(NULL,publish_data);
      }
    }
    else if(packet.packet[0] == 0xFA && packet.packet[1] == 0xFB &&
            (packet.packet[3] == 0x20))
    {
      //printf("got a CONFIGpac:%d\n",packet.size);
    }
    else if (packet.packet[0] == 0xFA && packet.packet[1] == 0xFB && packet.packet[3] == ARMPAC)
    {
    /*  if (actarray_id.interf)
      {
        // ARMpac - current arm status
        double joints[6];
        sippacket->ParseArm (&packet.packet[2]);
        for (int ii = 0; ii < 6; ii++)
        {
          sippacket->armJointPosRads[ii] = TicksToRadians (ii, sippacket->armJointPos[ii]);
          joints[ii] = sippacket->armJointPosRads[ii];
        }
        sippacket->Fill(&p2os_data);
        if(kineCalc)
        {
          kineCalc->CalculateFK (joints);
          limb_data.position.px = kineCalc->GetP ().x + armOffsetX;
          limb_data.position.py = -kineCalc->GetP ().y + armOffsetY;
          limb_data.position.pz = kineCalc->GetP ().z + armOffsetZ;
          limb_data.approach.px = kineCalc->GetA ().x;
          limb_data.approach.py = -kineCalc->GetA ().y;
          limb_data.approach.pz = kineCalc->GetA ().z;
          limb_data.orientation.px = kineCalc->GetO ().x;
          limb_data.orientation.py = -kineCalc->GetO ().y;
          limb_data.orientation.pz = kineCalc->GetO ().z;
          if (limb_data.state != PLAYER_LIMB_STATE_OOR && limb_data.state != PLAYER_LIMB_STATE_COLL)
          {
            if (sippacket->armJointMoving[0] || sippacket->armJointMoving[1] || sippacket->armJointMoving[2] ||
                sippacket->armJointMoving[3] || sippacket->armJointMoving[4])
            {
              limb_data.state = PLAYER_LIMB_STATE_MOVING;
            }
            else
              limb_data.state = PLAYER_LIMB_STATE_IDLE;
          }
        }
      }
      if(publish_data)
        this->PutData();
      // Go for another SIP - there had better be one or things will probably go boom
      SendReceive(NULL,publish_data);
*/    }
    else if (packet.packet[0] == 0xFA && packet.packet[1] == 0xFB && packet.packet[3] == ARMINFOPAC)
    {
/*      // ARMINFOpac - arm configuration stuff
      if (actarray_id.interf)
      {
        sippacket->ParseArmInfo (&packet.packet[2]);
        // Update the KineCalc with the new info for joints - one would assume this doesn't change, though...
        if (kineCalc)
        {
          for (int ii = 0; ii < 5; ii++)
            kineCalc->SetJointRange (ii, TicksToRadians (ii, sippacket->armJoints[ii].min), TicksToRadians (ii, sippacket->armJoints[ii].max));
          // Go for another SIP - there had better be one or things will probably go boom
        }
        SendReceive(NULL,publish_data);
      }
*/    }
    else
    {
      //PrintHex(packet);
    }
  }
  return(0);
}

/* toggle sonars on/off, according to val */
void ToggleSonarPower(unsigned char val)
{
  unsigned char command[4];
  P2OSPacket packet;

  command[0] = SONAR;
  command[1] = ARGINT;
  command[2] = val;
  command[3] = 0;
  Build(&packet, command, 4);
  //SendReceive(p2os, &packet,FALSE);
  Send(p2os.psos_fd, packet);

  //PrintHex(packet);
}

/* toggle motors on/off, according to val */
void ToggleMotorPower(unsigned char val)
{
  unsigned char command[4];
  P2OSPacket packet;

  command[0] = ENABLE;
  command[1] = ARGINT;
  command[2] = val;
  command[3] = 0;
  Build(&packet, command, 4);

  Send(p2os.psos_fd, packet);
}

int p2osShutdown()
{
  unsigned char command[20],buffer[20];
  P2OSPacket packet;
  struct timespec ts;

  memset(buffer,0,20);

  if(p2os.psos_fd == -1)
    return(0);


  command[0] = STOP;
  Build(&packet,command, 1);
  Send(p2os.psos_fd,packet);
  ts.tv_sec = 0;
  ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
  nanosleep (&ts, NULL);


  command[0] = CLOSE;
  Build(&packet,command, 1);
  Send(p2os.psos_fd,packet);
  ts.tv_sec = 0;
  ts.tv_nsec = P2OS_CYCLETIME_USEC*1000;
  nanosleep (&ts, NULL);


  close(p2os.psos_fd);
  p2os.psos_fd = -1;
  puts("P2OS has been shutdown");

  p2os.sippacket = NULL;

  return(0);
}
void ResetRawPositions()
{
  P2OSPacket pkt;
  unsigned char p2oscommand[4];

//OJO! esto en C no es lo mismo!
  if(p2os.sippacket != NULL)
  {

    p2os.sippacket->rawxpos = 0;
    p2os.sippacket->rawypos = 0;
    p2os.sippacket->xpos = 0;
    p2os.sippacket->ypos = 0;
    p2oscommand[0] = SETO;
    p2oscommand[1] = ARGINT;
    Build(&pkt,p2oscommand, 2);

    Send(p2os.psos_fd,pkt);
  }
}

void SendPulse ()
{
  unsigned char command;
  P2OSPacket packet;

  command = PULSE;
  Build(&packet, &command, 1);

  Send(p2os.psos_fd,packet);
}

//este sendvel es de juguete y manda la misma velocidad siempre ...
void SendVel ()
{
  P2OSPacket pkt;
  unsigned char p2oscommand[4];
  unsigned short absspeedDemand;
  int speedDemand;

    speedDemand = (int)rint(0.5 * 1e3);
    absspeedDemand = (unsigned short)abs(speedDemand);
    p2oscommand[0] = VEL;
    p2oscommand[1] = ARGINT;
    p2oscommand[2] = absspeedDemand & 0x00FF;
    p2oscommand[3] = (absspeedDemand & 0xFF00) >> 8;
    Build(&pkt,p2oscommand, 4);

    Send(p2os.psos_fd,pkt);

}

void HandleSoundCommand(player_sound_cmd_t sound_cmd)
{
  unsigned char soundcommand[4];
  P2OSPacket soundpacket;
  unsigned short soundindex;

  soundindex = ntohs(sound_cmd.index);


    soundcommand[0] = SOUND;
    soundcommand[1] = ARGINT;
    soundcommand[2] = soundindex & 0x00FF;
    soundcommand[3] = (soundindex & 0xFF00) >> 8;
    Build(&soundpacket,soundcommand,4);

    Send(p2os.psos_fd,soundpacket);

}

void HandlePositionCommand(player_position2d_cmd_vel_t position_cmd)
{
  int speedDemand, turnRateDemand;
  double leftvel, rightvel;
  double rotational_term;
  unsigned short absspeedDemand, absturnRateDemand;
  unsigned char motorcommand[4];
  P2OSPacket motorpacket;

  speedDemand = (int)rint(position_cmd.vel.px * 1e3);
  turnRateDemand = (int)rint(RTOD(position_cmd.vel.pa));

  if(p2os.direct_wheel_vel_control)
  {
    //printf("Entro direct_wheel\n");
    // convert xspeed and yawspeed into wheelspeeds
    rotational_term = (M_PI/180.0) * turnRateDemand /
            PlayerRobotParams[p2os.param_idx].DiffConvFactor;
    leftvel = (speedDemand - rotational_term);
    rightvel = (speedDemand + rotational_term);

    // Apply wheel speed bounds
    if(fabs(leftvel) > p2os.motor_max_speed)
    {
      if(leftvel > 0)
      {
        rightvel *= p2os.motor_max_speed/leftvel;
        leftvel = p2os.motor_max_speed;
        puts("Left wheel velocity threshholded!");
      }
      else
      {
        rightvel *= -p2os.motor_max_speed/leftvel;
        leftvel = -p2os.motor_max_speed;
      }
    }
    if(fabs(rightvel) > p2os.motor_max_speed)
    {
      if(rightvel > 0)
      {
        leftvel *= p2os.motor_max_speed/rightvel;
        rightvel = p2os.motor_max_speed;
        puts("Right wheel velocity threshholded!");
      }
      else
      {
        leftvel *= -p2os.motor_max_speed/rightvel;
        rightvel = -p2os.motor_max_speed;
      }
    }

    // Apply control band bounds
    if(p2os.use_vel_band)
    {
      // This band prevents the wheels from turning in opposite
      // directions
      if (leftvel * rightvel < 0)
      {
        if (leftvel + rightvel >= 0)
        {
          if (leftvel < 0)
            leftvel = 0;
          if (rightvel < 0)
            rightvel = 0;
        }
        else
        {
          if (leftvel > 0)
            leftvel = 0;
          if (rightvel > 0)
            rightvel = 0;
        }
      }
    }

    // Apply byte range bounds
    if (leftvel / PlayerRobotParams[p2os.param_idx].Vel2Divisor > 126)
      leftvel = 126 * PlayerRobotParams[p2os.param_idx].Vel2Divisor;
    if (leftvel / PlayerRobotParams[p2os.param_idx].Vel2Divisor < -126)
      leftvel = -126 * PlayerRobotParams[p2os.param_idx].Vel2Divisor;
    if (rightvel / PlayerRobotParams[p2os.param_idx].Vel2Divisor > 126)
      rightvel = 126 * PlayerRobotParams[p2os.param_idx].Vel2Divisor;
    if (rightvel / PlayerRobotParams[p2os.param_idx].Vel2Divisor < -126)
      rightvel = -126 * PlayerRobotParams[p2os.param_idx].Vel2Divisor;

    // send the speed command
    motorcommand[0] = VEL2;
    motorcommand[1] = ARGINT;
    motorcommand[2] = (char)(rightvel /
                             PlayerRobotParams[p2os.param_idx].Vel2Divisor);
    motorcommand[3] = (char)(leftvel /
                             PlayerRobotParams[p2os.param_idx].Vel2Divisor);

    Build(&motorpacket, motorcommand, 4);
   // SendReceive(p2os,&motorpacket);
    Send(p2os.psos_fd,motorpacket);

  }
  else
  {
    // do separate trans and rot vels
    //printf("Entro NO direct_wheel\n");

    motorcommand[0] = VEL;
    if(speedDemand >= 0)
      motorcommand[1] = ARGINT;
    else
      motorcommand[1] = ARGNINT;

    absspeedDemand = (unsigned short)abs(speedDemand);
    if(absspeedDemand < p2os.motor_max_speed)
    {
      motorcommand[2] = absspeedDemand & 0x00FF;
      motorcommand[3] = (absspeedDemand & 0xFF00) >> 8;
    }
    else
    {
      puts("Speed demand threshholded!");
      motorcommand[2] = p2os.motor_max_speed & 0x00FF;
      motorcommand[3] = (p2os.motor_max_speed & 0xFF00) >> 8;
    }
    Build(&motorpacket, motorcommand, 4);
    //p2os->SendReceive(&motorpacket);
    Send(p2os.psos_fd,motorpacket);

    motorcommand[0] = RVEL;
    if(turnRateDemand >= 0)
      motorcommand[1] = ARGINT;
    else
      motorcommand[1] = ARGNINT;

    absturnRateDemand = (unsigned short)abs(turnRateDemand);
    if(absturnRateDemand < p2os.motor_max_turnspeed)
    {
      motorcommand[2] = absturnRateDemand & 0x00FF;
      motorcommand[3] = (absturnRateDemand & 0xFF00) >> 8;
    }
    else
    {
      puts("Turn rate demand threshholded!");
      motorcommand[2] = p2os.motor_max_turnspeed & 0x00FF;
      motorcommand[3] = (p2os.motor_max_turnspeed & 0xFF00) >> 8;
    }

    Build(&motorpacket, motorcommand, 4);
    //SendReceive(p2os,&motorpacket);
    Send(p2os.psos_fd,motorpacket);

  }
}


//***********************************************************************************
//  Nuevas funciones para manejar la estructura del P2OS desde fuera (conversion ADA)
//***********************************************************************************


/* toggle motors on/off */
void p2osMotorOn(){
    ToggleMotorPower(1);
}
void p2osMotorOff(){
    ToggleMotorPower(0);
}
/* toggle sonar on/off */
void p2osSonarOn(){
    ToggleSonarPower(1);
}
void p2osSonarOff(){
    ToggleSonarPower(0);
}

void p2osGetValues(){
    SendReceive(NULL,false);
}

//Funciones para el acceso al paquete SIP desde ADA
int p2osGetLwstall(){
    return p2os.sippacket->lwstall;
}
int p2osGetRwstall(){
    return p2os.sippacket->rwstall;
}

unsigned char p2osGetStatus(){
    return p2os.sippacket->status;
}

unsigned char p2osGetBattery(){
    return p2os.sippacket->battery;
}

unsigned char p2osGetSonarreadings(){
    return p2os.sippacket->sonarreadings;
}

unsigned char p2osGetAnalog(){
    return p2os.sippacket->analog;
}

unsigned char p2osGetDigin(){
    return p2os.sippacket->digin;
}
unsigned char p2osGetDigout(){
    return p2os.sippacket->digout;
}

unsigned short p2osGetPtu(){
    return p2os.sippacket->ptu;
}

unsigned short p2osGetCompass(){
    return p2os.sippacket->compass;
}

unsigned short p2osGetTimer(){
    return p2os.sippacket->timer;
}
unsigned short p2osGetRawxpos(){
    return p2os.sippacket->rawxpos;
}

unsigned short p2osGetRawypos(){
    return p2os.sippacket->rawypos;
}
unsigned short p2osGetFrontbumpers(){
    return p2os.sippacket->frontbumpers;
}
unsigned short p2osGetRearbumpers(){
    return p2os.sippacket->rearbumpers;
}

short p2osGetLvel(){
    return p2os.sippacket->lvel;
}
short p2osGetRvel(){
    return p2os.sippacket->rvel;
}
short p2osGetControl(){
    return p2os.sippacket->control;
}

unsigned short p2osGetSonar(int I) {
    if ((I< 0) || (I > PLAYER_SONAR_MAX_SAMPLES)){
       perror("P2OS::InitP2os():open():");
           return(1);
    }
    else return p2os.sippacket->sonars[I];
}

float p2osGetXpos(){
//  return p2os.sippacket->xpos;
    return p2os.p2os_data.position.pos.px;
}
float p2osGetYpos(){
//  return p2os.sippacket->ypos;
    return p2os.p2os_data.position.pos.py;
}

float p2osGetAngle(){
//  return p2os.sippacket->angle;
    return p2os.p2os_data.position.pos.pa;
}


float p2osGetXSpeed(){
    //printf("GETSPEED x= %f\n",p2os.p2os_data.position.vel.px);
    return p2os.p2os_data.position.vel.px;
}
float p2osGetYawSpeed(){
    return p2os.p2os_data.position.vel.pa;
}

int p2osGetX_offset(){
    return p2os.sippacket->x_offset;
}
int p2osGetY_offset(){
    return p2os.sippacket->y_offset;
}
int p2osGetAngle_offset(){
    return p2os.sippacket->angle_offset;
}


//muestra la informacion del paquete SIP
void PrintP2OS_SIP(){
    PrintSIP(p2os.sippacket);
}

//Establece la velocidad de translacion y rotacion del robot
void SetSpeed(float trans, float rot){
    player_position2d_cmd_vel_t position_cmd;
    position_cmd.vel.px=trans;
        position_cmd.vel.pa=rot;
    //printf("SetSPEED x= %f, a= %f \n",trans,rot);
        HandlePositionCommand(position_cmd);
}


void lockP2os(){
      pthread_mutex_lock(&p2os.mutex);

};
void unlockP2os(){
      pthread_mutex_unlock(&p2os.mutex);

};


// //***********************************************************************************
// //  Nuevas funciones para manejar la estructura del P2OS desde fuera (conversion ADA)
// //***********************************************************************************
//
//
// /* toggle motors on/off */
// void p2osMotorOn(){
//  ToggleMotorPower(1);
// }
// void p2osMotorOff(){
//  ToggleMotorPower(0);
// }
// /* toggle sonar on/off */
// void p2osSonarOn(){
//  ToggleSonarPower(1);
// }
// void p2osSonarOff(){
//  ToggleSonarPower(0);
// }
//
// void p2osGetValues(){
//  SendReceive(NULL,false);
// }
//
// //Funciones para el acceso al paquete SIP desde ADA
// int p2osGetLwstall(){
//  return p2os.sippacket->lwstall;
// }
// int p2osGetRwstall(){
//  return p2os.sippacket->rwstall;
// }
//
// unsigned char p2osGetStatus(){
//  return p2os.sippacket->status;
// }
//
// unsigned char p2osGetBattery(){
//  return p2os.sippacket->battery;
// }
//
// unsigned char p2osGetSonarreadings(){
//  return p2os.sippacket->sonarreadings;
// }
//
// unsigned char p2osGetAnalog(){
//  return p2os.sippacket->analog;
// }
//
// unsigned char p2osGetDigin(){
//  return p2os.sippacket->digin;
// }
// unsigned char p2osGetDigout(){
//  return p2os.sippacket->digout;
// }
//
// unsigned short p2osGetPtu(){
//  return p2os.sippacket->ptu;
// }
//
// unsigned short p2osGetCompass(){
//  return p2os.sippacket->compass;
// }
//
// unsigned short p2osGetTimer(){
//  return p2os.sippacket->timer;
// }
// unsigned short p2osGetRawxpos(){
//  return p2os.sippacket->rawxpos;
// }
//
// unsigned short p2osGetRawypos(){
//  return p2os.sippacket->rawypos;
// }
// unsigned short p2osGetFrontbumpers(){
//  return p2os.sippacket->frontbumpers;
// }
// unsigned short p2osGetRearbumpers(){
//  return p2os.sippacket->rearbumpers;
// }
//
// short p2osGetLvel(){
//  return p2os.sippacket->lvel;
// }
// short p2osGetRvel(){
//  return p2os.sippacket->rvel;
// }
// short p2osGetControl(){
//  return p2os.sippacket->control;
// }
//
// unsigned short p2osGetSonar(int I) {
//  if ((I< 0) || (I > PLAYER_SONAR_MAX_SAMPLES)){
//     perror("P2OS::InitP2os():open():");
//             return(1);
//  }
//  else return p2os.sippacket->sonars[I];
// }
//
// float p2osGetXpos(){
// //   return p2os.sippacket->xpos;
//  return p2os.p2os_data.position.pos.px;
// }
// float p2osGetYpos(){
// //   return p2os.sippacket->ypos;
//  return p2os.p2os_data.position.pos.py;
// }
//
// float p2osGetAngle(){
// //   return p2os.sippacket->angle;
//  return p2os.p2os_data.position.pos.pa;
// }
//
//
//
// int p2osGetX_offset(){
//  return p2os.sippacket->x_offset;
// }
// int p2osGetY_offset(){
//  return p2os.sippacket->y_offset;
// }
// int p2osGetAngle_offset(){
//  return p2os.sippacket->angle_offset;
// }
//
// float p2osGetXSpeed(){
//  return (((p2os.sippacket->lvel) + (p2os.sippacket->rvel) ) / 2) / 1e3;
// }
// float p2osGetYawSpeed(){
//  return ((double)(p2os.sippacket->rvel - p2os.sippacket->lvel) /
//                            (2.0/PlayerRobotParams[p2os.sippacket->param_idx].DiffConvFactor));
// }
//
// //muestra la informacion del paquete SIP
// void PrintP2OS_SIP(){
//  PrintSIP(p2os.sippacket);
// }
//
// //Establece la velocidad de translacion y rotacion del robot
// void SetSpeed(float trans, float rot){
//  player_position2d_cmd_vel_t position_cmd;
//  position_cmd.vel.px=trans;
//          position_cmd.vel.pa=rot;
//
//          HandlePositionCommand(position_cmd);
// }
