/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.51  Oct 2005
 *
 *                            i 2 c - e l i t e
 *
 *                                    H
 *
 * File 'i2c-elite.h'                                             By Sangorrin
 *
 * 
 * This file and i2c-elite.c provides the way to emulate the i2c protocol 
 * on the STPC Elite CPU. It makes use of some bits of the DDC control
 * register. It supposes that you are the only master in the bus.
 *
 * Philips I2C protocol manual
 * http://www.semiconductors.philips.com/acrobat/applicationnotes/AN10216_1.pdf
 *
 * how to use it --> test-i2c-elite.c
 *
 *---------------------------------------------------------------------------*/
#ifndef __I2C_ELITE_H__
#define __I2C_ELITE_H__

#define ACK	1
#define NACK	0
#define I2C_SEMIPERIOD 10000000   /* Nanoseconds */

/*---------------------------------------------------------------------------*/
/* 1.- LOW-LEVEL functions for accesing the DDC Register of STPC ELite       */
/* You probably don't need to use this functions in your app or driver. They */
/* are only provided for testing and giving more power to the drivers.       */
/*---------------------------------------------------------------------------*/
/* a) Wait one semi-cycle. You can adjust the time with I2C_SEMIPERIOD. 
     The Standard use of I2C is 0 to 100Kbit/s */
extern void I2C_Elite_Wait();

/* b) Get the whole DDC control register to see its bits */
extern unsigned char GET_DDC_REG(void); 

/* c) Set the the DDC control register to change its bits */
extern void SET_DDC_REG(unsigned char val);	

/* d) Set SDA line to a high value */
extern void I2C_Elite_SDA_High (void) ;

/* e) Set SDA line to a low value */
extern void I2C_Elite_SDA_Low (void);

/* f) Set SCL line to a high value */
extern void I2C_Elite_SCL_High (void);

/* g) Set SCL line to a low value */
extern void I2C_Elite_SCL_Low (void);

/* h) Read the current SDA value in the I2C bus*/
extern unsigned char I2C_Elite_Read_SDA (void);

/*---------------------------------------------------------------------------*/
/* 2.- HIGH-LEVEL functions for controlling an I2C slave device              */
/*---------------------------------------------------------------------------*/
/* ------------------------------------------------------------
// Function:  void I2C_Elite_Start (void)
// Description: Start bit of the I2C protocol
// ------------------------------------------------------------*/
extern void I2C_Elite_Start (void);
/* ------------------------------------------------------------
// Function:  void I2C_Elite_ReStart (void)
// Description: Repeated Start bit of the I2C protocol
// ------------------------------------------------------------*/
extern void I2C_Elite_ReStart (void);
/* ------------------------------------------------------------
// Function:  void I2C_Elite_Stop (void)
// Description: Stop bit of the I2C protocol
// ------------------------------------------------------------*/
extern void I2C_Elite_Stop (void);
/* ------------------------------------------------------------
// Function:  void I2C_Elite_Write ( unsigned char byte)
// Description: Write a byte in the bus
// ------------------------------------------------------------*/
extern void I2C_Elite_Write ( unsigned char byte );
/* ------------------------------------------------------------
// Function:  unsigned char I2C_Elite_Get_Ack (void)
// Description: Retreive acknowledge if data is sended correctly.
// It can return ACK or NACK (1 and 0).
// ------------------------------------------------------------*/
extern unsigned char I2C_Elite_Get_Ack (void);
/* ------------------------------------------------------------
// Function:  unsigned char I2C_Elite_Read (void)
// Description: Read a byte from the SDA line
// ------------------------------------------------------------*/
extern unsigned char I2C_Elite_Read (void);
/* ------------------------------------------------------------
// Function:  void I2C_Elite_Set_Ack (void)
// Description: Send an Acknowledge
// ------------------------------------------------------------*/
extern void I2C_Elite_Set_Ack (void);
/* ------------------------------------------------------------
// Function:  void I2C_Elite_Set_Nack (void)
// Description: Send a No Acknowledge. See the I2C protocol
// manual in order to know when you should use Nacks.
// ------------------------------------------------------------*/
extern void I2C_Elite_Set_Nack (void);

#endif	/* __I2C_ELITE_H__ */
