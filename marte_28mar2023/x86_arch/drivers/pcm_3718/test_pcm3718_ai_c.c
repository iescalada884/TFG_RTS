/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                              test_pcm3718_c
 *
 *                                    C
 *
 * File 'test_pcm3718_c.c'                                     By Sangorrin
 *
 *---------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <drivers/pcm3718.h>

#define ERROR(s) {perror (s); exit (-1);}

static void pause()
{
    char pulsacion;
    printf("press...");
    pulsacion = getchar();
}

static void msg(char *s)
{
    printf(s);
    pause();
}

static float sample2volt(uint16_t the_sample, float range_p, int bipolar)
{
    float range_n = 0.0;
    if(bipolar){
        range_n = range_p;
    }
    return (range_p-(-range_n))*(float)the_sample/4096.0-range_n;
}

int main()
{
    int i,N,R,Mode;
    int fd;
    ssize_t bytes;
    ai_ioctl_cmd ai_command;
    ai_ioctl_arg ai_arg;
    range_type ranges[16] = {UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,
        UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,
        UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5,UNIPOLAR_5};
    analog_data_type *read_buffer;

    msg("This is the C Test program for Analog Input of PCM-3718H. ");
    if ((fd = open ("/dev/daq", O_RDWR)) == -1)
        ERROR ("open");

    // Set the Range of each channel we want to use
    ai_command = SET_RANGE_OF_CHANNEL;
    for(i=0;i<=15;i++){
        ai_arg.start_ch = i;
        ai_arg.input_range = ranges[i];
        if (ioctl(fd,ai_command,&ai_arg) == -1)
            ERROR ("ioctl");
    }

    while(1)
    {
        printf("Enter a number of samples: ");
        scanf("%d",&N);
        printf("Enter Rate (us): ");
        scanf("%d",&R);
        printf("Mode soft(1) fixed(2) scan(3): ");
        scanf("%d",&Mode);
        // Create the buffer to hold the samples
        read_buffer = (analog_data_type *)malloc( N*sizeof(analog_data_type));

        switch(Mode) {
        case 1: // A) SOFTWARE TRIGGER
            msg("SOFTWARE TRIGGER test. ");
            // set the parameters for software trigger
            ai_command = SET_PARAMETERS;
            ai_arg.start_ch = 0;
            ai_arg.stop_ch      = 15;
            ai_arg.trigger  = SOFTWARE;
            ai_arg.count    = N;
            if (ioctl(fd,ai_command,&ai_arg) == -1)
                ERROR ("ioctl");
            // read
            if ((bytes = read(fd,read_buffer,N*sizeof(analog_data_type))) == -1)
                ERROR ("read");
            // and display the samples
            for(i=0; i<bytes/sizeof(analog_data_type); i++){
                printf("%d Channel:%d, Code:%d, Volt:%f\n",i,
                        read_buffer[i].the_channel,read_buffer[i].the_sample,
                        sample2volt(read_buffer[i].the_sample,5.0,0));
            }
            break;

        case 2: // B) PACER TRIGGER Fixed MODE
            msg("PACER TRIGGER Fixed MODE test.");
            ai_command = SET_PARAMETERS;
            ai_arg.start_ch = 0;
            ai_arg.stop_ch      = 15;
            ai_arg.trigger  = PACER;
            ai_arg.mode     = FIXED;
            ai_arg.count    = N;
            ai_arg.scan_rate    = R;
            if (ioctl(fd,ai_command,&ai_arg) == -1)
                ERROR ("ioctl");
            printf("aproximated rate: %d\n",ai_arg.scan_rate);
            // read
            if ((bytes = read(fd,read_buffer,N*sizeof(analog_data_type))) == -1)
                ERROR ("read");
            // and display the samples
            for(i=0; i<bytes/sizeof(analog_data_type); i++){
                printf("%d Channel:%d, Code:%d, Volt:%f\n",i,
                        read_buffer[i].the_channel,read_buffer[i].the_sample,
                        sample2volt(read_buffer[i].the_sample,5.0,0));
            }
            break;

        case 3: // C) PACER TRIGGER SCAN MODE
            msg("PACER TRIGGER SCAN MODE test.");
            ai_command = FLUSH;
            if (ioctl(fd,ai_command,&ai_arg) == -1)
                ERROR ("ioctl");
            ai_command = SET_PARAMETERS;
            ai_arg.start_ch = 0;
            ai_arg.stop_ch      = 15;
            ai_arg.trigger  = PACER;
            ai_arg.mode     = SCAN;
            ai_arg.scan_rate    = R;
            if (ioctl(fd,ai_command,&ai_arg) == -1)
                ERROR ("ioctl");
            printf("aproximated rate: %d\n",ai_arg.scan_rate);
            sleep(0.01);
            // read
            if ((bytes = read(fd,read_buffer,N*sizeof(analog_data_type))) == -1)
                ERROR ("read");
            // get status
            ai_command = GET_STATUS;
            if (ioctl(fd,ai_command,&ai_arg) == -1)
                ERROR ("ioctl");
            printf("samples in buffer: %d\n",ai_arg.count);
            // and display the samples
            for(i=0; i<bytes/sizeof(analog_data_type); i++){
                printf("%d Channel:%d, Code:%d, Volt:%f\n",i,
                        read_buffer[i].the_channel,read_buffer[i].the_sample,
                        sample2volt(read_buffer[i].the_sample,5.0,0));
            }
            sleep(0.6);
            // get status
            ai_command = GET_STATUS;
            if (ioctl(fd,ai_command,&ai_arg) == -1)
                ERROR ("ioctl");
            printf("samples in buffer: %d\n",ai_arg.count);
        }

        free (read_buffer);
        msg("THE END");
    }

    if (close (fd) == -1) {
        ERROR ("close");
        exit (-1);
    }

    return 0;
}
