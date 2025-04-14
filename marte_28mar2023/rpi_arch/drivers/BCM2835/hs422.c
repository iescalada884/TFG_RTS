

#include <stdio.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <malloc.h>
#include <misc/timespec_operations.h>
#include <misc/error_checks.h>
#include "hs422.h"
pthread_t threads;

struct servo_data_t
{
    int pos;
    servo_t servo;
    int running;
    struct servo_data_t *next;
    
};

struct servo_data_t *servos = NULL;

int INIT = 0;

int hs422_init()
{
	if(phat_init())
	{
		return -1;	
	}
	
	struct servo_data_t *base = malloc(sizeof(struct servo_data_t));
	struct servo_data_t *codo = malloc(sizeof(struct servo_data_t));
	struct servo_data_t *pinza = malloc(sizeof(struct servo_data_t));
	base->pos = 0; base->servo = BASE; base->running = 0; base->next = codo;
	codo->pos = 0; codo->servo = CODO; codo->running = 0; codo->next = pinza;
	pinza->pos = 0; pinza->servo = PINZA; pinza->running = 0; pinza->next =NULL;
	
	servos = base;
	INIT = 1;
	return 0;
	
}
int calculate_pos(int grados)
{
	if(grados < 0 || grados > 180)
		return -1;
	switch(grados)
	{
		case(0):
			return CEROG;
		case(90):
			return NOVENTAG;
		case(180):
			return CIENTOOCHENTAG;
		default:
			return CEROG + (grados * INCREMENTO);
	}
}


void delayNano(int nanoSec)
{
		struct timespec req,rem;
		req.tv_sec = 0;
		req.tv_nsec = nanoSec;
		if(nanosleep(&req , &rem) == -1)
			printf("FATAL ERROR");
}

struct servo_data_t* hs422_find(servo_t servo)
{	
	switch(servo)
    {
		case(BASE):
			return servos;
		case(CODO):
			return servos->next;
		case (PINZA):
			return servos->next->next;
		default:
			return NULL;
	}
	
}

void *hs422_thread(void *threadarg)
{
    struct timespec my_period, next_activation;
    struct servo_data_t *servo = (struct servo_data_t*)threadarg;
    
    
    my_period.tv_sec = 0;
	my_period.tv_nsec = 20 * 1000000;
	
	if (clock_gettime(CLOCK_MONOTONIC, &next_activation))
			printf ("Error in clock_realtime\n");

    while (servo->running)
    {
		incr_timespec (&next_activation, &my_period);
		
		phat_output_on(servo->servo);
		
		delayNano(servo->pos);
				
		phat_output_off(servo->servo);
			
		if (clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
			&next_activation, NULL))
			printf("Error in clock_nanosleep");
    }

    // clean up
    pthread_exit(NULL);
}

void hs422_changePosition(servo_t servo, int grados)
{
	struct servo_data_t *serv = hs422_find(servo);
    if(serv == NULL)
		return;
	
	int newPos = calculate_pos(grados);
	if(newPos == -1)
		return;
		
    serv->pos = newPos;
}


void hs422_start(servo_t servo)
{
	if(INIT)
	{
		struct servo_data_t *serv = hs422_find(servo);
		if(serv == NULL)
			return;
	
		serv->running = 1;
		if (pthread_create(&threads, NULL, hs422_thread, (void *)serv) != 0)
		{
			// btc fixme - error
			serv->running = 0;
			return;
		}
	}
}

void hs422_stop(servo_t servo)
{
    struct servo_data_t *serv = hs422_find(servo);
	if(serv == NULL)
		return;
		
	serv->running = 0;
}



