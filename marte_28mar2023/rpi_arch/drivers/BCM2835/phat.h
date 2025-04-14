/*! \mainpage C library for Explorer Phat with Raspberry Pi Zero
 * 
 * This library gives the capabilities to use the Explorer Phat
 * with Rasperry Pi Zero in a easy way.
 * 
 * It provides functions to control the motors, the four analog
 * inputs, the four digital inputs and outputs.
 * 
 */

/*!
 * 
 * Defines for the I2C communication, just the same values as used in
 * Pimoronis Python Library
 * 
 */

#define	I2C_ADDRESS					0x48
#define SAMPLES_PER_SECOND1600		0x0080				
#define	CHANNEL4					0x4000	
#define CHANNEL3					0x5000
#define	CHANNEL2					0x6000
#define	CHANNEL1					0x7000
#define PROGRAMMABLE_GAIN144V		0x0000
#define	SINGLE_SHOT					0x8000
#define REG_CONVERT 				0x00
#define REG_CONFIG					0x01

typedef enum
{
	PHAT_MOTOR_1 = 0, 
	PHAT_MOTOR_2 = 1
}motor_t; 

typedef enum
{
	OUT1 = 0,
	OUT2 = 1,
	OUT3 = 2,
	OUT4 = 3
}output_t;

typedef enum
{
	IN1 = 0,
	IN2 = 1,
	IN3 = 2,
	IN4 = 3
}input_t;

typedef enum
{
	ADC1 = 0,
	ADC2 = 1,
	ADC3 = 2,
	ADC4 = 3
}adc_t;



/*! Initializes the phat.
  Sets the function of each GPIO pin, and the initial dutycicle of each motor pin to 0.
  \return 0 if succeeded, -1 if error occurred.
*/
int phat_init();

/*! Start moving the motor.
 * If the motor velocity is greater than 0 the motor will move, if not, you must
 * call changeSpeed and the motor will move.
  \param[in] The identifier of which motor do you want to start moving.
*/
void phat_motor_start(motor_t motor);

/*! Change the motor velocity.
 * Change the motor velocity, if the motor is stopped, it wont move, you must call function start and
 * it will move at the velocity you previously set.
 * If the motor is moving, velocity change.
  \param[in] The identifier of which motor do you want to start moving.
  \param[in] The value in percent of the speed you want to set. Value must be 
  between -100.0 and 100.0
  \return 0 if succeeded, -1 if the value was incorrect.
*/
int phat_motor_changeSpeed(motor_t motor,float value);

/*! Invert the motor direction
 \param[in] The identifier of which motor direction you want to change.
*/
void phat_motor_invert(motor_t motor);

/*! Stop the motor.
 \param[in] The identifier of which motor you want to stop.
*/
void phat_motor_stop(motor_t motor);

/*! Enables the output.
 * The 4 outputs on Explorer can sink 5V, but not source. This means you need to connect
 * your load to one of the 5V pins, and then to the output. When you turn the output on it will connect your circuit to ground, 
 * allowing current to flow and your load to turn on.
 \param[in] The identifier of which output you want to turn on.
*/
void phat_output_on(output_t output);

/*! Disables the output
 \param[in] The identifier of which output you want to turn on.
*/
void phat_output_off(output_t output);

/*! Reads the value of one of the inputs.
 * This function reads the value of one of the digital pins
 \param[in] The identifier of which input you want to read.
 \return the value read, 0 if low, 1 if high.
*/
unsigned int phat_input_read(input_t input);

int phat_analog_read(adc_t channel);

float phat_read_distance(adc_t channel);


