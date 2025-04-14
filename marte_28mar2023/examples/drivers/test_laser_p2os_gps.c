#include <stdio.h>
#include <stdbool.h>
#include <pthread.h>
#include <drivers/laser-sick-lms200.h>
#include <drivers/p2os.h>
#include <drivers/gps-novatel.h>
#include <drivers/playercommon.h>

// Available tests:
#define  LASER_TEST_DEMAND      1
#define  LASER_TEST_THREAD_P2OS 2
#define  LASER_TEST_THREAD      3
#define  GPS_TEST               4

// TEST: The test to do this time
#define TEST LASER_TEST_THREAD_P2OS

typedef struct buffer {
 // P2OS p2os;
  double buf[MAXNDATA];
  int datalen;
}buffer;

double limit(double a, double min, double max)
{
    if (a < min)
        return min;
    else if (a > max)
        return max;
    else
        return a;
}

int navegacion(buffer *mybuffer)
{
// player_position2d_cmd_vel_t position_cmd;
    double newspeed = 0;
    double newturnrate = 0;
    double minR = 1e9;
    double minL = 1e9;
    unsigned int count;
    unsigned int j;

    ToggleMotorPower(1);

    // go into read-think-act loop
    for(;;)
    {
        newspeed = 0;
        newturnrate = 0;
        minR = 1e9;
        minL = 1e9;
        // this blocks until new data comes; 10Hz by default
        //robot.Read();
        //esto no lo puedo hacer desde aqui, tendra que haber un SendReceive
        //y un SendReceiveDemand
        //ioctl(mybuffer->p2os.psos_fd,SERIAL_FLUSH,NULL);
        SendReceive(NULL,false);

        // laser avoid (stolen from esben's java example)
        count = mybuffer->datalen;
        printf("count = %d\n",count);

        for (j=0; j < count/2; j++)
        {
            if (minR > mybuffer->buf[j]/1000)
                minR = mybuffer->buf[j]/1000;
        }

        for (j = count/2; j < count; j++)
        {
            if (minL > mybuffer->buf[j]/1000)
                minL = mybuffer->buf[j]/1000;
        }

        printf("minR: %f , minL: %f\n",minR,minL);

        double l = (1e5*minR)/500-100;
        double r = (1e5*minL)/500-100;

        if (l > 100)
            l = 100;
        if (r > 100)
            r = 100;

        newspeed = (r+l)/1e3;

        newturnrate = (r-l);
        newturnrate = limit(newturnrate, -40.0, 40.0);
        newturnrate = DTOR(newturnrate);


        printf("speed: %f , turn: %f\n",newspeed,newturnrate);
        // write commands to robot
        //  position_cmd.vel.px=newspeed;
        //  position_cmd.vel.pa=newturnrate;
        //  HandlePositionCommand(position_cmd);
        SetSpeed(newspeed,newturnrate);
    }
}

void * mandaComandos(void *arg){

    char c;
    //player_sound_cmd_t sound_cmd;
    //       sound_cmd.index=1;
    //       HandleSoundCommand(p2os,sound_cmd);

    struct buffer *my_buffer;
    my_buffer = (struct buffer *)arg;

    ToggleMotorPower(1);
    //my_buffer->p2os.direct_wheel_vel_control = 0;

    for(;;){

        printf("\n*********************************\n");
        printf(" 1) Leer/Mostrar valores SIP       \n");
        printf(" 2) Laser                          \n");
        printf(" 3) Translacion                    \n");
        printf(" 4) Rotacion                       \n");
        printf(" 5) velocidad Maxima               \n");
        printf(" 6) Navegacion laser/p2os          \n");
        printf(" 8) 4.4 metros                     \n");
        printf(" 7) Salir                          \n");
        printf(" **********************************\n\n");

        c=getchar();

        //ToggleSonarPower(p2os,1);

        if (c == '1'){
            //esto no lo puedo hacer desde aqui, tendra que haber un
            //SendReceive y un SendReceiveDemand
            //      ioctl(my_buffer->p2os.psos_fd,SERIAL_FLUSH,NULL);
            SendReceive(NULL,false);

            PrintP2OS_SIP();
        }
        if (c == '2'){
            int i = 0;

            for (i = 0; my_buffer->datalen > i; i++){
                printf("%d : %d\t",i,laserazo(i));
            }

        }
        if (c == '3'){
            SendVel();
        }
        if (c == '4'){

            SetSpeed(0.0,0.5);
        }
        if (c == '5'){

            SetSpeed(2.0,0.0);
        }
        if (c == '6'){
            navegacion(my_buffer);
            break;
        }
        float x,y = 0.0;
        float s = 0.0;
        if (c == '7'){
            while (x < 3.0){
                SendReceive(NULL,false);
                s=p2osGetXSpeed();
                x=p2osGetXpos();
                y=p2osGetYpos();
                printf("pos x = %f || pos y = %f || vel = %f\n",x,y,s);
                SetSpeed(0.2,0.0);
            }
        }
        if (c == '8'){
            p2osShutdown();
            break;
        }
        c=getchar();
    }

    pthread_exit (NULL);
    return NULL;
}

void * usaLaser(void *arg)
{
    uchar buf[MAXNDATA];
    char defaultdev[]="/dev/ttyS2", *port=defaultdev;
    int datalen,count=0;
    int range_mode=RANGE_100,res_mode=RES_1_DEG,unit_mode=MMMODE;
    int baud_sel=BAUD_9600;//BAUD_38400;//BAUD_19200;// BAUD_9600;

    struct buffer *my_buffer;
    my_buffer = (struct buffer *)arg;

    // initialisation - put the LMS in a mode where it constantly sends dat

    connectToLMS(range_mode, res_mode, unit_mode, port, baud_sel);
    printf("Fin de inicializacion\n");

    int i,j;
    for (;;) {

        datalen = readLMSdata(buf);
        if (datalen != 0) {
            j=7;
            my_buffer->datalen = datalen /2;
            for(i=0; i<my_buffer->datalen; i++) {//len
                my_buffer->buf[i]=(double)( (buf[j+1] & 0x1f) <<8  |buf[j]);
                j=j+2;
            }
            count++;
        }
    }

    stopLMS();
    resetLMS();
    return NULL;
}


#if TEST  == LASER_TEST_THREAD_P2OS
int main(){

    struct buffer my_buffer;
    char defaultdev[]="/dev/ttyS0", *port=defaultdev;
    pthread_t t1, t2;

    //********************************************************
    //   LASER SICK_LMS
    printf("LASER:   Empieza la inicializacion: (ESPERA A QUE SE PONGA EL LED VERDE SOLO!!!!");
    getchar();
    // thread para el laser
    pthread_create (&t1, NULL, usaLaser, &my_buffer);

    //********************************************************
    //   P2OS
    printf("P2OS:   Empieza la inicializacion");
    initP2os(port);
    pthread_create (&t2, NULL, mandaComandos, &my_buffer);


    //threads finalizados....
    pthread_join (t1, NULL) ;
    pthread_join (t2, NULL) ;


    return (0);
}

#elif TEST  == LASER_TEST_DEMAND
int main(){

    uchar buf[MAXNDATA];
    char c, defaultdev[]="/dev/ttyS2", *port=defaultdev;
    int datalen,count=0;
    int range_mode=RANGE_100,res_mode=RES_1_DEG,unit_mode=MMMODE;
    int baud_sel=BAUD_9600;//BAUD_38400;//BAUD_19200;// BAUD_9600;


    //********************************************************
    //   LASER SICK_LMS
    printf("LASER:   Empieza la inicializacion: (ESPERA A QUE SE PONGA EL LED VERDE SOLO!!!!");
    getchar();
     //iniciamos el laser y le pedimos la lectura del mismo con un getchar()
    // initialisation - put the LMS in a mode where it constantly sends dat

    connectToLMS(range_mode, res_mode, unit_mode, port, baud_sel);
    printf("Fin de inicializacion\n");

    for (;;) {

        printf("\n*********************************\n");
        printf(" 1) Dame el valor del laser ahora  \n");
        printf(" 2) Salir                          \n");
        printf(" **********************************\n\n");

        c=getchar();


        if (c == '1'){
            //ioctl(fd,SERIAL_FLUSH,NULL);
            printf("LECTURA DEL LASER AHORA ...:\n");
            datalen = readLMSdataDemand(buf);
            if (datalen != 0) {
                chkstatus(buf[datalen+7]);
                showLaser(datalen, buf);
                count++;
            }
        }
        if (c=='2') break;
    }

    printf("Test del laser bajo demanda finalizado\n");
    //ioctl(fd,SERIAL_FLUSH,NULL); //necesario para que no haya problemas
    stopLMS();
    resetLMS();
    return NULL;


    return (0);
}

#elif TEST  == LASER_TEST_THREAD
void unizar();
int main(int argc, char **argv)
{
    uchar buf[MAXNDATA];
    char defaultdev[]="/dev/ttyS2", *port=defaultdev;
    int datalen,count=0,count_max=100;
    int range_mode=RANGE_100,res_mode=RES_1_DEG,unit_mode=MMMODE;
    int baud_sel=BAUD_38400;//BAUD_38400;//BAUD_19200;// BAUD_9600;

    // initialisation - put the LMS in a mode where it constantly sends dat
    printf("Empieza la inicializacion: (ESPERA A QUE SE PONGA EL LED VERDE SOLO!!!!");
    getchar();
    connectToLMS(range_mode, res_mode, unit_mode, port, baud_sel);
    printf("Fin de inicializacion\n");

    // Read back and print out that data
    while (count < count_max) {
        //     datalen = readLMSdata(buf);
        //printf("leo");
        //     datalen = readLMSValuesDemand();
        datalen = readLMSValues();
        if (datalen != 0) {
            //       showLaser(datalen, buf);
            //       chkstatus(buf[datalen+7]);
            printf("El laser numero 10 vale %d\tESTADO = %d\n",laserazo(10),getStatus());

            count++;
        }
    }
    stopLMS();
    resetLMS();
    return 0;
}

#elif TEST  == GPS_TEST
int main(){
    int i;
    char defaultdev[]="/dev/ttyS3", *port=defaultdev;
    printf("Empieza la inicializacion del GPS. Pulsa una tecla ...");
    getchar();
    initNovatel(port);
    if (SetupNovatel() < 0) return 0;

    i = 0;
    while(i < 100){
        i ++;
        // Read incoming data from the GPS
        if(leerSerie()){
            printf("ERORR: leyendo de la unidad de GPS");
            break;
        }
        leeGps();
        printf("Coordenada x = %f\t",gps_X());
        printf("Coordenada y = %f\n",gps_Y());
    }

    ShutdownNovatel();

    return 0;
}
#endif
