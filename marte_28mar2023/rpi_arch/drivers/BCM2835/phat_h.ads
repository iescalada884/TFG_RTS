  --!\mainpage C library for Explorer Phat with Raspberry Pi Zero
  -- * 
  -- * This library gives the capabilities to use the Explorer Phat
  -- * with Rasperry Pi Zero in a easy way.
  -- * 
  -- * It provides functions to control the motors, the four analog
  -- * inputs, the four digital inputs and outputs.
  -- * 
pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package phat_h is

   -- * Defines for the I2C communication, just the same values as used in
   -- * Pimoronis Python Library
 
   I2C_ADDRESS : constant := 16#48#;  --  phat.h:18
   SAMPLES_PER_SECOND1600 : constant := 16#0080#;  --  phat.h:19
   CHANNEL4 : constant := 16#4000#;  --  phat.h:20
   CHANNEL3 : constant := 16#5000#;  --  phat.h:21
   CHANNEL2 : constant := 16#6000#;  --  phat.h:22
   CHANNEL1 : constant := 16#7000#;  --  phat.h:23
   PROGRAMMABLE_GAIN144V : constant := 16#0000#;  --  phat.h:24
   SINGLE_SHOT : constant := 16#8000#;  --  phat.h:25
   REG_CONVERT : constant := 16#00#;  --  phat.h:26
   REG_CONFIG : constant := 16#01#;  --  phat.h:27

   -- Definición de tipos

   type motor_t is
   (
	PHAT_MOTOR_1,
      	PHAT_MOTOR_2
   );
   pragma Convention (C, motor_t);  -- phat.h:33

   type output_t is 
   (
	OUT1,
	OUT2,
        OUT3,
        OUT4
   );
   pragma Convention (C, output_t);  -- phat.h:41

   type input_t is 
   (
	IN1,
        IN2,
        IN3,
        IN4
   );
   pragma Convention (C, input_t);  -- phat.h:49

   type adc_t is 
   (
	ADC1,
        ADC2,
        ADC3,
        ADC4
   );
   pragma Convention (C, adc_t);  -- phat.h:57

  --  Initializes the phat.
  --  Sets the function of each GPIO pin, and the initial dutycicle of each motor pin to 0.
  --  return 0 if succeeded, -1 if error occurred.

   function phat_init return int;  -- phat.h:65
   pragma Import (C, phat_init, "phat_init");

  -- Start moving the motor.
  -- If the motor velocity is greater than 0 the motor will move, if not, you must
  -- call changeSpeed and the motor will move.
  -- param[in] The identifier of which motor do you want to start moving.

   procedure phat_motor_start (arg1 : motor_t);  -- phat.h:72
   pragma Import (C, phat_motor_start, "phat_motor_start");

  -- Change the motor velocity.
  -- Change the motor velocity, if the motor is stopped, it wont move, you must call function start and
  -- it will move at the velocity you previously set.
  -- If the motor is moving, velocity change.
  -- \param[in] The identifier of which motor do you want to start moving.
  -- \param[in] The value in percent of the speed you want to set. Value must be 
  -- between -100.0 and 100.0
  -- \return 0 if succeeded, -1 if the value was incorrect.

   function phat_motor_changeSpeed (arg1 : motor_t; arg2 : float) return int;  -- phat.h:83
   pragma Import (C, phat_motor_changeSpeed, "phat_motor_changeSpeed");

  -- Invert the motor direction
  -- param[in] The identifier of which motor direction you want to change.

   procedure phat_motor_invert (arg1 : motor_t);  -- phat.h:88
   pragma Import (C, phat_motor_invert, "phat_motor_invert");

  -- Stop the motor.
  -- param[in] The identifier of which motor you want to stop.

   procedure phat_motor_stop (arg1 : motor_t);  -- phat.h:93
   pragma Import (C, phat_motor_stop, "phat_motor_stop");

  -- Enables the output.
  -- The 4 outputs on Explorer can sink 5V, but not source. This means you need to connect
  -- your load to one of the 5V pins, and then to the output. When you turn the output on it will connect your circuit to ground, 
  -- allowing current to flow and your load to turn on.
  -- param[in] The identifier of which output you want to turn on.

   procedure phat_output_on (pin_salida : output_t);  -- phat.h:101
   pragma Import (C, phat_output_on, "phat_output_on");

  -- Disables the output
  -- param[in] The identifier of which output you want to turn on.

  procedure phat_output_off (pin_salida : output_t);  -- phat.h:106
  pragma Import (C, phat_output_off, "phat_output_off");

  -- Reads the value of one of the inputs.
  -- This function reads the value of one of the digital pins
  -- param[in] The identifier of which input you want to read.
  -- return the value read, 0 if low, 1 if high.

  function phat_input_read (pin_entrada : input_t) return unsigned;  -- phat.h:113
  pragma Import (C, phat_input_read, "phat_input_read");

  function phat_analog_read (pin_entrada : adc_t) return int;  -- phat.h:115
  pragma Import (C, phat_analog_read, "phat_analog_read");

  function phat_read_distance (arg1 : adc_t) return float;  -- phat.h:117
  pragma Import (C, phat_read_distance, "phat_read_distance");


end phat_h;
