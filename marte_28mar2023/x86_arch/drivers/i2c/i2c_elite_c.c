/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.51  Oct 2005
 *
 *                            i 2 c _ e l i t e _ c
 *
 *                                    C
 *
 * File 'i2c_elite_c.c'                                             By Sangorrin
 *
 * 
 * This file and its header i2c_elite.h provides the way to emulate the i2c
 * protocol on the STPC Elite CPU. It makes use of some bits of the DDC control
 * register.
 *
 * Philips I2C protocol manual
 * http://www.semiconductors.philips.com/acrobat/literature/9398/39340011.pdf
 *
 * how to use it --> test_i2c_elite.c
 *
 *---------------------------------------------------------------------------*/

#include <sys/pio.h>      /* for inb and outb */
#include <sys/i2c_elite.h>    /* for some definitions */
#include <time.h> /* for the function wait */

#define ACCESS         0x22
#define DATA           0x23
#define OFFSET         0x97
#define MASK_SCL       0x01
#define MASK_SDA       0x0A
#define MASK_READ_SDA  0x08
#define MASK_READ_SCL  0x04
#define SET_SDA        0x02
#define SET_SCL        0x01
#define VALID          0xF0

/*---------------------------------------------------------------------------*/
/* 1.- Low-level functions for accesing the DDC Register of STPC ELite       */
/*---------------------------------------------------------------------------*/
/* a) Wait one semi-cycle */
static struct timespec ts = {0,I2C_SEMIPERIOD};
void I2C_Elite_Wait()
{  
   nanosleep (&ts, 0);
}
/* b) Get the whole DDC control register to see its bits */
unsigned char GET_DDC_REG(void) 
{
	outb (ACCESS, OFFSET);
	return inb (DATA);
}
/* c) Set the the DDC control register to change its bits */
void SET_DDC_REG(unsigned char val)	
{
	outb (ACCESS, OFFSET);
	outb (DATA, val);
}
/* d) Set SDA line to a high value */
void I2C_Elite_SDA_High (void)
{
	unsigned char byte ;
	byte = GET_DDC_REG () ;
	byte = byte & MASK_SCL ;
	if ( byte ){
		SET_DDC_REG (SET_SDA | SET_SCL | VALID) ;  /* SCL=1 SDA=1 */
	} else {
		SET_DDC_REG ( SET_SDA | VALID ) ;  /* SCL=0 SDA=1 */
	}
}
/* e) Set SDA line to a low value */
void I2C_Elite_SDA_Low (void)
{
	unsigned char byte ;
	byte = GET_DDC_REG() ;
	byte = byte & MASK_SCL ;
	if ( byte ){
		SET_DDC_REG ( SET_SCL | VALID ) ;  /* SCL=1 SDA=0 */
	} else {
		SET_DDC_REG ( VALID ) ;  /* SCL=0 SDA=0 */
	}
}
/* f) Set SCL line to a high value */
void I2C_Elite_SCL_High (void)
{
	unsigned char byte ;
	byte  = GET_DDC_REG () ;
	byte  = byte & MASK_SDA ;
	if ( byte ){
		SET_DDC_REG (SET_SDA | SET_SCL | VALID) ; /* SCL=1 SDA=1 */
	} else {
		SET_DDC_REG (SET_SCL | VALID) ; /* SCL=1 SDA=0 */
	}
}
/* g) Set SCL line to a low value */
void I2C_Elite_SCL_Low (void)
{
	unsigned char byte ;
	byte = GET_DDC_REG () ;
	byte = byte & MASK_SDA ;
	if ( byte ){
		SET_DDC_REG (SET_SDA | VALID) ; /* SCL=0 SDA=1 */
	} else {
		SET_DDC_REG (VALID ) ;  /* SCL=0 SDA=0 */
	}
}
/* h) Read the current SDA value in the I2C bus*/
unsigned char I2C_Elite_Read_SDA (void)
{
 	unsigned char byte ;
 	unsigned char boolean ;
	byte = GET_DDC_REG () ;
   byte = byte & MASK_READ_SDA ;
   if ( byte ){
   	boolean = 1 ;
   }else{
   	boolean = 0 ;
   }
	return( boolean);
}
/* h) Read the current SCL value in the I2C bus*/
unsigned char I2C_Elite_Read_SCL (void)
{
   unsigned char byte ;
   unsigned char boolean ;
   byte = GET_DDC_REG () ;
   byte = byte & MASK_READ_SCL ;
   if ( byte ){
      boolean = 1 ;
   }else{
      boolean = 0 ;
   }
   return( boolean);
}
/*---------------------------------------------------------------------------*/
/* 2.- Public functions (see i2c-elite.h for the meaning and I2C Manual for  */
/* understanding the protocol)                                               */
/*---------------------------------------------------------------------------*/
void I2C_Elite_Start (void)
{
	I2C_Elite_Wait	() ;        //        __
	I2C_Elite_SDA_Low  () ;     //   SDA    |__
	I2C_Elite_Wait	() ;        //        _____
	I2C_Elite_SCL_Low  () ;     //   SCL       |
}
/*---------------------------------------------------------------------------*/
void I2C_Elite_ReStart (void)
{
	I2C_Elite_SDA_Low  () ;
	I2C_Elite_Wait	 () ;
	I2C_Elite_SDA_High  () ;     //          _______
	I2C_Elite_Wait	 () ;         //  SDA  __|      |__
	I2C_Elite_SCL_High  () ;     //            ________
	I2C_Elite_Wait	 () ;         //  SCL  ____|       |
	I2C_Elite_Wait	 () ;
	I2C_Elite_SDA_Low   () ;
	I2C_Elite_Wait	 () ;
	I2C_Elite_SCL_Low   () ;
}
/*---------------------------------------------------------------------------*/
void I2C_Elite_Stop (void)
{
	I2C_Elite_SDA_Low  () ;       //
	I2C_Elite_Wait	() ;           // SDA _____|
	I2C_Elite_SCL_High () ;       //        __
	I2C_Elite_Wait 	() ;        // SCL __|
	I2C_Elite_SDA_High () ;
}
/*---------------------------------------------------------------------------*/
void I2C_Elite_Write ( unsigned char byte )
{
	unsigned char i;
	// We write each bit in the bus from the most significant to the least one
	for ( i=0 ; i<=7 ; i++ )   {
		if ( byte & 0x80 ){
			I2C_Elite_SDA_High () ;      //      ______________________
		} else {                        // SDA |__7__|__6__|__ . . 0_|
			I2C_Elite_SDA_Low () ;       //        __    __        __
		}	                             // SCL __|  |__|  |__ . .|  |
		I2C_Elite_Wait();
		I2C_Elite_SCL_High ();
		I2C_Elite_Wait();
		I2C_Elite_SCL_Low () ;
		byte <<= 1;
   }
}
/*---------------------------------------------------------------------------*/
unsigned char I2C_Elite_Get_Ack (void)
{
	unsigned char boolean;
	I2C_Elite_SDA_High () ; // The slave should stick SDA line to a low value
	I2C_Elite_Wait();
	I2C_Elite_SCL_High () ;
	I2C_Elite_Wait();
	boolean = I2C_Elite_Read_SDA ();	
	I2C_Elite_SDA_Low () ;
	I2C_Elite_SCL_Low () ;
	I2C_Elite_Wait();
	return !boolean;
}
/*---------------------------------------------------------------------------*/
unsigned char I2C_Elite_Read (void)
{
	unsigned char i ;
	unsigned char byte  = 0	;
	unsigned char bit	;
	
	I2C_Elite_SDA_High (); // The slave should stick SDA line to a low value
	// We read each bit, from the most significant to the least one
	for ( i = 0; i <= 7; i++ )	{
	   I2C_Elite_Wait();
	   I2C_Elite_SCL_High ();
	  	bit   = I2C_Elite_Read_SDA() ;
	  	byte  = byte | bit ;
		if ( i != 7 ){
			byte  <<= 1 ;
		}	
		I2C_Elite_Wait();
		I2C_Elite_SCL_Low () ;
	}
	return( byte ) ;
}
/*---------------------------------------------------------------------------*/
void I2C_Elite_Set_Ack (void)
{
	I2C_Elite_SCL_Low () ;
	I2C_Elite_SDA_Low () ;        //
	I2C_Elite_Wait();             // SDA   ______
	I2C_Elite_SCL_High ();        //          __
	I2C_Elite_Wait();             // SCL   __|  |__
	I2C_Elite_SCL_Low () ;
}
/*---------------------------------------------------------------------------*/
void I2C_Elite_Set_Nack (void)
{
	I2C_Elite_SCL_Low () ;
	I2C_Elite_SDA_High ();        //       ______
	I2C_Elite_Wait();             // SDA
	I2C_Elite_SCL_High ();        //          __
	I2C_Elite_Wait();             // SCL   __|  |
	I2C_Elite_SCL_Low () ;
}



