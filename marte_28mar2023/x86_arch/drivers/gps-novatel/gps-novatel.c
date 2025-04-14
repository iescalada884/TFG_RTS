/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                'n o v a t e l . c'
 *
 *                                      C
 *
 *  File 'novatel.c'                                  by F.J.Feijoo
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
#include <drivers/player.h>
#include <drivers/serial_port_driver.h>
#include <drivers/gps-novatel.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/time.h>

#include "utmCpp.h"
#include "utmCpp.c"
#include "crc32.h"
#include "crc32.c"
#include "elipsoides.h"

#define O_SYNC     0x0400

// TIPOS DE DATOS
#define TAM_BUF 256

typedef enum bool { false_ = 0, true_} bool; //conflicto con TRUE y FALSE

typedef unsigned char UChar;
typedef short int Short;
typedef unsigned short int UShort;
typedef long int Long;
typedef unsigned long int ULong;

typedef Long Enum;
typedef Long GPSec;

enum PORTS          {ALL_PORTS = 8, COM1 = 32, COM2 = 64, COM3 = 96,
                     THISPORT = 192};
enum TRIGGER        {ONNEW = 0,  ONCHANGED, ONTIME, ONNEXT, ONCE, ONMARK};
enum HOLD           {NOHOLD = 0, HOLD};

enum IDCOMANDOS     {LOG = 1, UNLOGALL = 38};
enum IDMENSAJES     {BESTPOS = 42};

enum SOLUTIONSTATUS {SOL_COMPUTED = 0, INSUFFICIENT_OBS, NO_CONVERGENCE,
                     SINGULARITY, COV_TRACE, TEST_DIST, COLD_START,
                     V_H_LIMIT, VARIANCE, RESIDUALS, DELTA_POS, NEGATIVE_VAR,
                     INTEGRITY_WARNING = 13};

typedef struct {
    char      sync[3];
    UChar     longitudCabecera;
    UShort    mensajeID;
    char      tipoMensaje;
    char      direccionPuerto;
    UShort    longitudMensaje;
    UShort    secuencia;
    char      tiempoLibre;
    char      estadoTiempo;
    UShort    semana;
    GPSec     milisegundos;
    ULong     estadoRecepcion;
    UShort    reservado;
    UShort    versionSoftware;
} Cabecera;

typedef struct{
    Enum      respuestaID;
    char *    mensajeError;
} Respuesta;

typedef struct {
    Enum      port;
    UShort    mensajeID;
    char      tipoMensaje;
    char      reservado;
    Enum      trigger;
    double    periodo;
    double    offset;
    Enum      hold;
} LogCommand;

typedef struct {
    Enum      puerto;
    Enum      reservado;
} UnLogCommand;

typedef struct {
    Enum      sol_status;
    Enum      pos_type;
    double    lat, lon, alt;
    float     ondulacion;
    float     sigmaLat, sigmaLon, sigmaAlt;
    char      baseID[4];
    float     diff_age, sol_age;
    UChar     nObs, nGPSL1, nL1, nL2;
    char      reservado[4];
} BestPosLog;

// Definicion de la clase del GPS
typedef struct Novatel
{
//private:
  // Configuracion del puerto serie
  const char *puerto;
  int gps_baud;
  int gps_fd;

  //termios_t oldTerm, newTerm;

  // Banderitas informativas
  bool localizado;
  bool hayHuso;
  bool lecturaValida;

  // Conversion de long/lat a X/Y
  const char * elipsoide;
  double semiejeMayor, semiejeMenor;
  Elipsoide e;
  GeoUTM *geoUTM;

  // Tiempo de muestreo
  unsigned char tiempo;

  // Buffers para realizar las comunicaciones
  unsigned char buf [TAM_BUF];
  unsigned int sizeBuf;
  unsigned char bufRec [TAM_BUF];
  unsigned int sizeBufRec;

  // Datos GPS anterior
  //bool inicializado;
  double xAnt, yAnt;

  double x, y;

  // Datos GPS actuales
  player_gps_data_t data;

} Novatel;

static termios_t oldTerm, newTerm;

// comandos para el GPS
void logBestPos();
void finalizarLogs();

// Funciones para la localizacion inicial, la orientacion inicial es el angulo
// que forma el robot con repecto al Este geografico
    // permite establecer el pto del mundo que se considera el origen. Pasarlo
    // en mm.
void localizar(double origenX, double origenY);
    // toma la 1o localizacion valida del GPS como pto origen;
void localizar2(void);

// Actualizacion periodica de la posicion con los datos del GPS
void actualizarPosicionTask(void);
bool analizaPaquete();

//variables estaticas globales. Cambio para poder traducir a ADA!!!!
static GeoUTM utm;
static Novatel gps;

void initNovatel(char * puerto)
{
  memset(&gps.data,0,sizeof(gps.data));
  gps.gps_fd = -1;
  gps.puerto = puerto;
  gps.gps_baud = 9600;
  gps.elipsoide = "WGS84";

  if(strcasecmp(gps.elipsoide, "usuario") == 0){
     // Por defecto los semiejes del elipsoide WGS84
     gps.semiejeMayor =  6378137.0;
     gps.semiejeMenor =  6356752.314245;
     //geoUTM = new GeoUTM(semiejeMayor, semiejeMenor);
     initGeoUTM2(&utm,gps.semiejeMayor, gps.semiejeMenor);
     if(gps.semiejeMenor < 0 || gps.semiejeMayor < 0 ||
        gps.semiejeMenor > gps.semiejeMayor){
        fprintf(stderr, "Los semiejes son incorrectos\n");
        //SetError(gps,-1);
        return;
     }
  }else{
     gps.e = traducirElipsoide(gps.elipsoide);
     gps.semiejeMayor = semiejes[gps.e][0];
     gps.semiejeMenor = semiejes[gps.e][1];
     initGeoUTM(&utm,gps.e);
  }
  gps.data.semiejeMayor = gps.semiejeMayor;
  gps.data.semiejeMenor = gps.semiejeMenor;

/*  umbralDistanciaMinimo = cf->ReadFloat(section, "umbral_min", 250);
  umbralDistanciaMaximo = cf->ReadFloat(section, "umbral_max", 1000);
  if((umbralDistanciaMinimo >= umbralDistanciaMaximo) ||
     (umbralDistanciaMinimo < 0)){
     umbralDistanciaMinimo = 250;
  }
  if(umbralDistanciaMaximo <= umbralDistanciaMinimo){
     umbralDistanciaMaximo = 1000;
  }*/

  gps.tiempo = (unsigned char) 1;
  gps.sizeBuf = gps.sizeBufRec = 0;
  gps.hayHuso = false_;

//   if (cf->ReadDeviceAddr(&(this->m_gps_addr), section,
//                          "provides", PLAYER_GPS_CODE, -1, NULL) != 0){
//     this->SetError(-1);
//     return;
//   }

// printf("puerto inicial %s\n",gps.puerto);
// SetupNovatel(gps);
  //inicializado = false;
  // printf("puerto inicial2 %s\n",gps.puerto);
  fprintf(stderr, "GPS CARGADO\n");
}

///////////////////////////////////////////////////////////////////////////
int SetupNovatel(){
  // Set up the serial port to talk to the GPS unit
  printf("Setup Novatel\n");

  if (SetupSerial() != 0){
    fprintf(stderr, "GPS NO INICIALIZADO\n");
    return -1;
 }
  puts("Done.");
  // start the thread to talk with the GPS unit
  //StartThread(gps);
  fprintf(stderr, "GPS INICIALIZADO\n");
  return(0);
}

///////////////////////////////////////////////////////////////////////////
int ShutdownNovatel(){

  //StopThread(gps);
  finalizarLogs();
  sleep(1);
  ShutdownSerial();
  //esta es la unica forma que he encontrado de momento para acabar
  //con el puerto serial
  write(gps.gps_fd,"c",1);
  return(0);
}

///////////////////////////////////////////////////////////////////////////
// Initialize the serial port
int SetupSerial(){
  int attempt;
  int maxattempts = 20;
  printf("Setup Serial %s \n",gps.puerto);
  fprintf(stderr, "Inicializando la conexion al GPS (%s)...\n", gps.puerto);
  printf("Setup Serial2\n");

  if((gps.gps_fd = open(gps.puerto, O_RDWR | O_SYNC)) < 0){
    printf("open(): %s\n", strerror(errno));
    return(-1);
  }

   if (ioctl(gps.gps_fd,SERIAL_FLUSH,NULL) <0){
      fprintf(stderr,"Error al hacer flush\n");
      getchar();
   };

   if (ioctl(gps.gps_fd,SERIAL_GETATTR,(void *)&oldTerm) < 0){
      fprintf(stderr,"Error al hacer flush\n");
      getchar();
   };

 newTerm.cflag = B9600 | CS8 | CLOCAL | CREAD;
 newTerm.iflag = IGNPAR;
 newTerm.oflag = 0;

  if (gps.gps_baud == 9600){
    newTerm.ospeed = B9600;
    newTerm.ispeed = B9600;
  }else if (gps.gps_baud == 19200){
    newTerm.ospeed = B19200;
    newTerm.ispeed = B19200;
  }else if (gps.gps_baud == 38400){
    newTerm.ospeed = B38400;
    newTerm.ispeed = B38400;
  }else{
    newTerm.ospeed = B4800;
    newTerm.ispeed = B4800;
  }

  if(ioctl(gps.gps_fd,SERIAL_SETATTR,(void *)&newTerm) < 0){
    printf("tcsetattr(): %s\n", strerror(errno));
    close(gps.gps_fd);
    gps.gps_fd = -1;
    return(-1);
  }

  // FIN INICIALIZACION DEL PUERTO SERIE
  logBestPos(gps);

  fprintf(stderr, "Comprobando conexion\n");

  /* Intentamos leer datos del GPS para comprobar la conexion */
  for(attempt=0;attempt<maxattempts;attempt++){
    if(!leerSerie(gps))
      break;
  }

  if(attempt==maxattempts){
    printf("No ha sido posible la conexion a la unidad GPS, lo mas probable\n"
                  "es que la unidad no este conectada a %s\n",
                  gps.puerto);
    finalizarLogs(gps);
    ShutdownSerial(gps);
    return -1;
  }

  fprintf(stderr, "Conexion comprobada\n");

  return 0;
}

///////////////////////////////////////////////////////////////////////////
// Shutdown the serial port
void ShutdownSerial(){
  ioctl(gps.gps_fd,SERIAL_FLUSH,NULL);
  ioctl(gps.gps_fd,SERIAL_SETATTR,(void *)&oldTerm);//(void *)&gps.oldTerm);
  close(gps.gps_fd);
  gps.gps_fd=-1;
  return;
}

// ///////////////////////////////////////////////////////////////////////////
//
// /*
//  * Driver thread runs here.  We have to poll the serial port
//  */
// int main(){
// //   struct pollfd fds[1];
//   int i;
//   Novatel gps;
//
//    printf("Empieza la inicializacion del GPS. Pulsa una tecla ...");
//    getchar();
//    initNovatel(&gps);
//    if (SetupNovatel(&gps) < 0) return 0;
//
//   i = 0;
//   while(i < 100){
//      i ++;
//     // Read incoming data from the GPS
//       if(leerSerie(&gps)){
//         printf("ERORR: leyendo de la unidad de GPS");
// 	break;
//       }
//       leeGps(&gps);
//   }
//
//   ShutdownNovatel(&gps);
// //esta es la unica forma que he encontrado de momento para acabar con el
// //puerto serial
//   write(gps.gps_fd,"c",1);
//
//   return 0;
// }

//****************************************************************************

void logBestPos(){
   Cabecera        header;
   LogCommand      log;
   unsigned long   crc32;
   unsigned char * ptr;

   header.sync[0] = 0xAA;
   header.sync[1] = 0x44;
   header.sync[2] = 0x12;

   header.longitudCabecera = sizeof(Cabecera);
   header.mensajeID = LOG;
   header.tipoMensaje = 0;
   header.direccionPuerto =  THISPORT;
   header.longitudMensaje = sizeof(LogCommand);
   header.secuencia = 0;
   header.tiempoLibre = 0;
   header.estadoTiempo = 0;
   header.semana = 0;
   header.milisegundos = 0;
   header.estadoRecepcion = 0;
   header.reservado = 0;
   header.versionSoftware = 0;

   log.port =  THISPORT;
   log.mensajeID = BESTPOS;
   log.tipoMensaje = 0;
   log.reservado = 0;
   log.trigger = ONTIME;
   log.periodo = gps.tiempo;
   log.offset = 0.0;
   log.hold = 0;

   ptr = gps.buf;
//    bcopy ((void *) &header, (void *) ptr, sizeof(Cabecera));
   memmove ((void *) ptr, (void *) &header, sizeof(Cabecera));
   gps.sizeBuf = sizeof(Cabecera);
   ptr += gps.sizeBuf;

//    bcopy ((void *) &log, (void *) ptr, sizeof(LogCommand));
   memmove ((void *) ptr, (void *) &log, sizeof(LogCommand));
   gps.sizeBuf += sizeof(LogCommand);

   crc32 = CalculateBlockCRC32(gps.sizeBuf, gps.buf);

   ptr = gps.buf + gps.sizeBuf;

//    bcopy((void *)&crc32, (void *)ptr, sizeof(long));
   memmove((void *)ptr, (void *)&crc32, sizeof(long));
   gps.sizeBuf += sizeof(float);

//    printf("Toca hacer el envio");
//    getchar();
   enviar(gps);
}

//****************************************************************************

void finalizarLogs(){
   Cabecera        header;
   UnLogCommand    unLog;
   unsigned long   crc32;
   unsigned char * ptr;

   header.sync[0] = 0xAA;
   header.sync[1] = 0x44;
   header.sync[2] = 0x12;

   header.longitudCabecera = sizeof(Cabecera);
   header.mensajeID = UNLOGALL;
   header.tipoMensaje = 0;
   header.direccionPuerto = THISPORT; //COM1;
   header.longitudMensaje = sizeof(UnLogCommand);
   header.secuencia = 0;
   header.tiempoLibre = 0;
   header.estadoTiempo = 0;
   header.semana = 0;
   header.milisegundos = 0;
   header.estadoRecepcion = 0;
   header.reservado = 0;
   header.versionSoftware = 0;

   unLog.puerto = THISPORT;//ALL_PORTS;
   unLog.reservado = 0;

   gps.sizeBuf = sizeof(Cabecera);
   ptr = gps.buf;
//    bcopy ((void *) &header, (void *) ptr, sizeof(Cabecera));
   memmove ((void *) ptr, (void *) &header, sizeof(Cabecera));

   ptr += gps.sizeBuf;
//    bcopy ((void *) &unLog, (void *) ptr, sizeof(UnLogCommand));
   memmove ((void *) ptr, (void *) &unLog, sizeof(UnLogCommand));
   gps.sizeBuf += sizeof(UnLogCommand);

   crc32 = CalculateBlockCRC32(gps.sizeBuf,gps. buf);

   ptr = gps.buf + gps.sizeBuf;

//    bcopy((void *)&crc32, (void *)ptr, sizeof(long));
   memmove((void *)ptr, (void *)&crc32, sizeof(long));
   gps.sizeBuf += sizeof(float);

   //printf("Numero de bytes de cierre .. %d",gps.sizeBuf);
   enviar(gps);
}

//****************************************************************************

void enviar(){
   int i = 0, enviado = 0;

   if (ioctl(gps.gps_fd,SERIAL_FLUSH,NULL) <0){
      fprintf(stderr,"Error al hacer flush\n");
   };

   fprintf(stderr,"Mandando a GPS %d bytes\n", gps.sizeBuf);
   while (gps.sizeBuf > 0){
      enviado = write(gps.gps_fd, (const char*)(gps.buf + i), gps.sizeBuf);
      if(enviado >= 0){
         i += enviado;
         gps.sizeBuf -= enviado;
//          printf("Quedan %d\n",gps.sizeBuf);
// 	 for (j=0; j<enviado; j++)
// 		printf("%02x ",gps.buf[j]);
      }
   }
   fprintf(stderr,"Enviado\n");
}

//****************************************************************************
int leerSerie(){
   unsigned int size;
   char lecturasFallidas, bytesLeidos;

   bytesLeidos = size = 0;
   lecturasFallidas = 0;

  unsigned char header[3];
  int cnt;


  memset(gps.bufRec,0,sizeof(gps.bufRec));

// Lectura de la cabecera, aqui sincronizamos....
    memset(header,0,sizeof(header));

    while(1)
    {
      cnt = 0;
      //se reciben los dos primeros bytes
      while( cnt!=1 )
      {
        cnt+=read( gps.gps_fd, &header[3], 1 );
        if ( cnt < 0 )
        {
          fprintf(stderr,"Error reading packet header from robot \
                         connection: P2OSPacket():Receive():read():");
          return(-1);
        }
      }

      if (header[0]==0xAA && header[1]==0x44 && header[2]==0x12) break;
      header[0]=header[1];
      header[1]=header[2];
      header[2]=header[3];

    }
     //printf("LEIDA LA SINCRONIZACION!!\n");

    size = header[3];
    memcpy( gps.bufRec, header, 4);

    cnt = 0;
    while( cnt!=(header[3]-4))
    {
      //printf("ENTRO AL WHILE2\n");
      if ( (cnt+=read( gps.gps_fd, &gps.bufRec[4+cnt],  header[3]-4-cnt )) < 0 )
      {
        fprintf(stderr,"Error reading packet body from robot connection: \
                P2OSPacket():Receive():read():");
        return(-1);
      }
    }

// CABECERA LEIDA COMPLETA ... N BYTES DEFINIDOS POR HEADER LENGTH (BYTE 4)
// LEEMOS LOS DATOS, MESSAGE LENGTH EN BYTE 8 DEL HEADER...

    cnt = 0;
    int message_length = 0;
    message_length = gps.bufRec[8] + gps.bufRec[9];

    while( cnt!= message_length)
    {
      if ((cnt+=read(gps.gps_fd, &gps.bufRec[size+cnt], message_length-cnt ))
           < 0 )
      {
        fprintf(stderr,"Error reading packet body from robot connection: \
                P2OSPacket():Receive():read():");
        return(-1);
      }
    }
    size+=message_length;

// LEIDO EL CUERPO DEL MENSAJE
// LEEMOS EL CRC
    cnt = 0;
    while( cnt!= 4)
    {
      if ( (cnt+=read( gps.gps_fd, &gps.bufRec[size+cnt],  4-cnt )) < 0 )
      {
        fprintf(stderr,"Error reading packet body from robot connection: \
                P2OSPacket():Receive():read():");
        return(-1);
      }
    }
    size+=4;
    gps.sizeBufRec = size;

  return(0);
}

//****************************************************************************

bool analizaPaquete(){
   Cabecera      header;
   BestPosLog    bestPos;
   Respuesta     respuesta;

   unsigned long crc32, crc32Rec;

   gps.sizeBufRec -= sizeof(float);
   crc32 = CalculateBlockCRC32(gps.sizeBufRec, gps.bufRec);

   memmove((void *)&crc32Rec,
           (void *)(gps.bufRec + gps.sizeBufRec),
           sizeof(long));

   if(crc32 != crc32Rec){
      fprintf(stderr,
              "GPS >>> CRC no valido %lu  %lu %d\n",
              crc32, crc32Rec, gps.sizeBufRec);
      return false_;
   }

   // Si mensaje es una respuesta => \0 al final del string de respuesta
   gps.bufRec[gps.sizeBufRec] = 0;

//    bcopy((void *) bufRec,(void *)&header, sizeof(Cabecera));
   memmove((void *)&header, (void *) gps.bufRec,sizeof(Cabecera));
   gps.sizeBufRec = sizeof(Cabecera);

   // Si mensaje leido es una respuesta
   if (header.tipoMensaje & 0x80){
//       bcopy((void *) (bufRec + sizeBufRec), (void *)&respuesta.respuestaID,
//             sizeof(Enum));
      memmove((void *)&respuesta.respuestaID,
              (void *) (gps.bufRec + gps.sizeBufRec), sizeof(Enum));
      gps.sizeBufRec += sizeof(Enum);
      respuesta.mensajeError = (char *)(gps.bufRec + gps.sizeBufRec);
      fprintf(stderr, "GPS >>> %s\n",respuesta.mensajeError);
      return false_;
   }

   // Mensaje es un log de BestPos
   memmove((void *)&bestPos,
           (void *) (gps.bufRec + gps.sizeBufRec),sizeof(BestPosLog));

   if(bestPos.sol_status != SOL_COMPUTED){
      fprintf(stderr, "GPS >>> No hay informacion de la posicion\n");
      return false_;
   }

   if (gps.hayHuso){ // Ya conocemos el huso
      setGeograficas(&utm, bestPos.lon, bestPos.lat);
      geo2UTM_Huso(&utm);
   }else{
      setGeograficas(&utm, bestPos.lon, bestPos.lat);
      geo2UTM(&utm);
      gps.hayHuso = true_;
   }

   //data.latitude = (int32_t)rint(bestPos.lat);
   //data.longitude = (int32_t)rint(bestPos.lon);
/*
   // Grados decimales
   data.latitude  = bestPos.lat;
   data.longitude = bestPos.lon;
*/
   // Grados decimales * 1e7 => compatibilidad con player
   gps.data.latitude  = bestPos.lat * 1e7;
   gps.data.longitude = bestPos.lon * 1e7;
   //printf("Latitud = %d, Longitud %d\n",gps.data.latitude,gps.data.longitude);

   // Obtencion de la posicion en metros
   //getUTM(&utm, &(gps.x), &(gps.y), huso, h);
   gps.x=utm.x;
   gps.y=utm.y;
// //hemisferio = geo->h;
   //printf("utm x = %f, y utm y%f\n",rint(1000.0 *utm.x),rint(1000.0 *utm.y));

   //printf("Primer x = %f, y = %f\n",gps.x,gps.y);
   gps.data.zone = utm.huso;

   // Paso de metros a milimetros, truncado de la parte decimal
   gps.x = rint(1000.0 * gps.x);
   gps.y = rint(1000.0 * gps.y);
   //printf("Segundo ...x = %f, y = %f\n",gps.x,gps.y);
   return true_;
}

//****************************************************************************

void leeGps(){
   double dx=0.0, dy=0.0;
   double distancia=0.0;


  if (!analizaPaquete(gps)){
     //std::cerr << this << endl;
     /*Publish(device_addr, NULL, PLAYER_MSGTYPE_DATA, PLAYER_GPS_DATA_STATE,
             &data,sizeof(player_gps_data_t),NULL);
     fprintf(stderr," *** MAL PUBLICADO\n");*/

	return;
  }

  // Si distancia(lectura Anterior,lecturaGPS) < umbral => suponemos que son
  // peque�as diferencias de posicion que da el GPS para un mismo pto
  // El umbralMaximo esta colocado para suavizar las posibles lecturas
  // espurias que puede dar el GPS cuando el robot esta fijo en un pto
  // aunque este ultimo caso lo corrige el propio GPS aplicando un filtro
  // de Kalman y utilizando DGPS
  dx = gps.x - gps.xAnt;
  dy = gps.y - gps.yAnt;
  distancia = sqrt(dx * dx + dy * dy);
  //fprintf(stderr, "%f %f %f\n",
  //        distancia, umbralDistanciaMinimo, umbralDistanciaMaximo);

// if (!inicializado ||
 //     ((distancia >= umbralDistanciaMinimo) &&
 //     (distancia <= umbralDistanciaMaximo))){

     //inicializado = true;

     // Variables de posicion
     gps.xAnt = gps.x;
     gps.yAnt = gps.y;

/*
     // PUBLICACION DE DATOS
     // paso a m
     data.utm_e = x * 0.001;
     data.utm_n = y * 0.001;
*/

     // PUBLICACION DE DATOS
     // paso a cm
     gps.data.utm_e = gps.x * 0.1;
     gps.data.utm_n = gps.y * 0.1;

   //fprintf(stderr, "NOVATEL X = %f\tY =%f\n", gps.data.utm_e, gps.data.utm_n);

//  }
}

float gps_X(){
  return gps.data.utm_e;
}
float gps_Y(){
  return gps.data.utm_n;
}
