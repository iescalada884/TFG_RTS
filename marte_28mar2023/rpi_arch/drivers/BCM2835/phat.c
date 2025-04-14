/* 
*/
#include "phat.h"
#include "soft_pwm.h"
#include "bcm2835.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>


typedef struct
{
	unsigned int 	pinB;
	unsigned int 	pinF;
		   float	speed;
}motor_data_t;

motor_data_t phat_motors[2] = 
	{{.pinB = 19, .pinF = 20, .speed = 0.0},
	{.pinB = 21, .pinF = 26, .speed = 0.0}};

unsigned int phat_outputs[4] = 
	{ 6, 12, 13, 16};
	
unsigned int phat_inputs[4] =
	{ 23, 22, 24, 25}; 


int phat_init()
{
    if (!bcm2835_init())
        return -1;
    //motors
    bcm2835_gpio_fsel(phat_motors[PHAT_MOTOR_1].pinB,BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_fsel(phat_motors[PHAT_MOTOR_1].pinF,BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_fsel(phat_motors[PHAT_MOTOR_2].pinB,BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_fsel(phat_motors[PHAT_MOTOR_2].pinF,BCM2835_GPIO_FSEL_OUTP);
    
	bcm2835_gpio_clr(phat_motors[PHAT_MOTOR_1].pinB);
    bcm2835_gpio_clr(phat_motors[PHAT_MOTOR_1].pinF);
    bcm2835_gpio_clr(phat_motors[PHAT_MOTOR_2].pinB);
    bcm2835_gpio_clr(phat_motors[PHAT_MOTOR_2].pinF);

    pwm_set_duty_cycle(phat_motors[PHAT_MOTOR_1].pinB,0.0);
    pwm_set_duty_cycle(phat_motors[PHAT_MOTOR_1].pinF,0.0);    
    pwm_set_duty_cycle(phat_motors[PHAT_MOTOR_2].pinB,0.0);
    pwm_set_duty_cycle(phat_motors[PHAT_MOTOR_2].pinF,0.0);
    
    //outputs
    bcm2835_gpio_fsel(phat_outputs[OUT1],BCM2835_GPIO_FSEL_OUTP);
	bcm2835_gpio_fsel(phat_outputs[OUT2],BCM2835_GPIO_FSEL_OUTP);
	bcm2835_gpio_fsel(phat_outputs[OUT3],BCM2835_GPIO_FSEL_OUTP);
	bcm2835_gpio_fsel(phat_outputs[OUT4],BCM2835_GPIO_FSEL_OUTP);
	
	bcm2835_gpio_clr(phat_outputs[OUT1]);
	bcm2835_gpio_clr(phat_outputs[OUT2]);
	bcm2835_gpio_clr(phat_outputs[OUT3]);
	bcm2835_gpio_clr(phat_outputs[OUT4]);
	
	//inputs
	bcm2835_gpio_fsel(phat_inputs[IN1],BCM2835_GPIO_FSEL_INPT);
	bcm2835_gpio_fsel(phat_inputs[IN2],BCM2835_GPIO_FSEL_INPT);
	bcm2835_gpio_fsel(phat_inputs[IN3],BCM2835_GPIO_FSEL_INPT);
	bcm2835_gpio_fsel(phat_inputs[IN4],BCM2835_GPIO_FSEL_INPT);

    return 0;
}

void phat_motor_start(motor_t motor)
{
	if(phat_motors[motor].speed > 0)
	{	
		pwm_set_duty_cycle(phat_motors[motor].pinF,phat_motors[motor].speed);
		pwm_start(phat_motors[motor].pinF);
		//pwm_set_duty_cycle(phat_motors[motor].pinB,0.0);
		bcm2835_gpio_clr(phat_motors[motor].pinB);
		
	}
	if(phat_motors[motor].speed < 0)
	{
		pwm_set_duty_cycle(phat_motors[motor].pinB,phat_motors[motor].speed);
		pwm_start(phat_motors[motor].pinB);
		//pwm_set_duty_cycle(phat_motors[motor].pinF,0.0);
		bcm2835_gpio_clr(phat_motors[motor].pinF);
	}
}

int phat_motor_changeSpeed(motor_t motor,float value)
{
	if(value < -100 || value > 100)
	{
		return -1;
	}
	if(value > 0)
	{
		if(phat_motors[motor].speed < 0)
		{
			pwm_stop(phat_motors[motor].pinB);
			pwm_set_duty_cycle(phat_motors[motor].pinB,0.0);
			bcm2835_gpio_clr(phat_motors[motor].pinB);
			pwm_set_duty_cycle(phat_motors[motor].pinF,value);
			pwm_start(phat_motors[motor].pinF);
		}else
		{
			pwm_set_duty_cycle(phat_motors[motor].pinB,0.0);
			bcm2835_gpio_clr(phat_motors[motor].pinB);
			pwm_set_duty_cycle(phat_motors[motor].pinF,value);
		}
		
	}
	if(value < 0)
	{
		if(phat_motors[motor].speed > 0)
		{
			pwm_stop(phat_motors[motor].pinF);
			pwm_set_duty_cycle(phat_motors[motor].pinB,fabs(value));
			pwm_set_duty_cycle(phat_motors[motor].pinF,0.0);
			bcm2835_gpio_clr(phat_motors[motor].pinF);
			pwm_start(phat_motors[motor].pinB);
		}else{
			pwm_set_duty_cycle(phat_motors[motor].pinB,fabs(value));
			pwm_set_duty_cycle(phat_motors[motor].pinF,0.0);
			bcm2835_gpio_clr(phat_motors[motor].pinF);
		}
	}
	if(value == 0)
	{
		pwm_stop(phat_motors[motor].pinF);
		pwm_stop(phat_motors[motor].pinB);
		pwm_set_duty_cycle(phat_motors[motor].pinB,0.0);
		pwm_set_duty_cycle(phat_motors[motor].pinF,0.0);
		bcm2835_gpio_clr(phat_motors[motor].pinF);
		bcm2835_gpio_clr(phat_motors[motor].pinB);
	}
	phat_motors[motor].speed = value;

}


void phat_motor_invert(motor_t motor)
{

   if(phat_motors[motor].speed < 0)
   {
		phat_motors[motor].speed = fabs(phat_motors[motor].speed);
		pwm_stop(phat_motors[motor].pinB);	
		pwm_set_duty_cycle(phat_motors[motor].pinB,0.0);
		pwm_set_duty_cycle(phat_motors[motor].pinF,phat_motors[motor].speed);
		pwm_start(phat_motors[motor].pinF);
   }
   if(phat_motors[motor].speed > 0)
   {
		pwm_stop(phat_motors[motor].pinF);
		pwm_set_duty_cycle(phat_motors[motor].pinB,phat_motors[motor].speed);
		pwm_set_duty_cycle(phat_motors[motor].pinF,0.0);
		pwm_start(phat_motors[motor].pinB);
		phat_motors[motor].speed = phat_motors[motor].speed * (-1.0);           
   }

}


void phat_motor_stop(motor_t motor)
{

   pwm_stop(phat_motors[motor].pinB);
   pwm_stop(phat_motors[motor].pinF);

}

void phat_output_on(output_t pinout)
{
	bcm2835_gpio_set(phat_outputs[pinout]);
}

void phat_output_off(output_t pinout)
{
	bcm2835_gpio_clr(phat_outputs[pinout]);
}

unsigned int phat_input_read(input_t inputpin)
{
	return bcm2835_gpio_lev(phat_inputs[inputpin]);
}


int phat_analog_read(adc_t channel)
{  
    bcm2835_i2c_begin();
    bcm2835_i2c_setSlaveAddress(I2C_ADDRESS);

	
	

	// Start with default values
	int config = 					    0x0003 |
											0x0100 |
							SAMPLES_PER_SECOND1600 |
							PROGRAMMABLE_GAIN144V  |
							SINGLE_SHOT;
	// Set single-ended input channel
	switch (channel)
	{
		case (0):
			config |= CHANNEL1;
			break;
		case (1):
			config |= CHANNEL2;
			break;
		case (2):
			config |= CHANNEL3;
			break;
		case (3):
			config |= CHANNEL4;
			break;
	}

	char configuration[3] = {REG_CONFIG, (config >> 8) & 0xFF, config & 0xFF};
	//char configuration[2] = {(config >> 8) & 0xFF, config & 0xFF};
	bcm2835_i2c_write(configuration, 3);

	// Wait for the conversion to complete
	bcm2835_delay(300);

	// Read the conversion results
	char buf[2];
	
	bcm2835_i2c_read_register_rs(REG_CONVERT, buf, 2);
	int value = (((buf[0] << 8) | buf[1]) >> 4);
	
	return value;
	
	
}

float phat_read_distance (adc_t sensor)
{
	int value = phat_analog_read(sensor); 
	float valueTemp, valueTemp2; 
	if(value < 35 && value > 100)
		return 0;
	if(value >= 35 && value <= 300) //Objeto entre 40 y 15 cm
	{
		if(value == 35)
			return 40.0; //Objeto aproximadamente a 40 cm
		if(value == 300)
			return 15.0; //Objeto aproximadamenta a 15 cm
		valueTemp = 35.0 * 40.0 /(float)value;
		valueTemp2 = 300.0 * 15.0 /(float)value;
		return (valueTemp + valueTemp2)/2;
			
	}else if(value > 300 && value <= 585) //Objeto entre 15 y 5 cm
	{
		if(value == 585)
			return 5.0;
		valueTemp = 300.0 * 15.0 /(float)value;
		valueTemp2 = 585.0 * 5.0 /(float)value;
		return (valueTemp + valueTemp2)/2;
	}else if(value > 585 && value <= 958) //Objeto entre 5 y 2 cm
	{
		if(value == 958)
			return 2.0;
		valueTemp = 585.0 * 5.0 /(float)value;
		valueTemp2 = 958.0 * 2.0 /(float)value;
		return (valueTemp + valueTemp2)/2;
	}
}









