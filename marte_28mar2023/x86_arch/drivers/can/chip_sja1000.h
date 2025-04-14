/*!
 * @file chip_sja1000.h
 *
 * @brief module for the chip sja1000
 *
 * @version 0.01
 *
 * @date 11-Feb-2008
 *
 * @author
 *      Juan Lopez (original author)
 *      Daniel Sangorrin (rewriting, documentation and reorganization)
 *
 * @comments
 *
 * This module contains the code that manages sja1000 chips.
 *
 * @license
 *
 * See MaRTE OS license
 *
 */

#ifndef _MARTE_CHIP_SJA1000_H_
#define _MARTE_CHIP_SJA1000_H_

#include <drivers/can.h>

/** sja1000 register set (in PeliCAN mode) */
enum sja1000_PeliCAN_regs_t {
        SJA_MOD   =  0, // Mode register
        SJA_CMR   =  1, // Command register
        SJA_SR    =  2, // Status register
        SJA_IR    =  3, // Interrupt register
        SJA_IER   =  4, // Interrupt Enable register
        SJA_BTR0  =  6, // Bus Timing register 0
        SJA_BTR1  =  7, // Bus Timing register 1
        SJA_OCR   =  8, // Output Control register
        SJA_ALC   = 11, // Arbitration Lost Capture
        SJA_ECC   = 12, // Error Code Capture
        SJA_EWLR  = 13, // Error Warning Limit
        SJA_RXERR = 14, // RX Error Counter
        SJA_TXERR = 15, // TX Error Counter
        SJA_RMC   = 29, // Rx Message Counter (number of msgs. in RX FIFO)
        SJA_RBSA  = 30, // Rx Buffer Start Addr. (address of current MSG)
        SJA_FRM   = 16, // Transmit (write) and Receive Buffer (read) Frame Inf
        SJA_ID0   = 17, // ID bytes (11 bits in 0 and 1 or 16 bits in 0,1
        SJA_ID1   = 18, // and 13 bits in 2,3 (extended))
        SJA_ID2   = 19, // ID cont. for extended frames
        SJA_ID3   = 20, // ID cont. for extended frames
        SJA_DATS  = 19, // Data start standard frame
        SJA_DATE  = 21, // Data start extended frame
        SJA_ACR0  = 16, // Acceptance Code (4 bytes) in RESET mode
        SJA_AMR0  = 20, // Acceptance Mask (4 bytes) in RESET mode
        SJA_CDR   = 31  // Clock Divider
};

/** Mode Register 0x00 */
enum sja1000_PeliCAN_MOD {
        sjaMOD_SM = 1<<4,  // Sleep Mode (writable only in OPERATING mode)
        sjaMOD_AFM= 1<<3,  // Acceptance Filter Mode (writable only in RESET)
        sjaMOD_STM= 1<<2,  // Self Test Mode (writable only in RESET)
        sjaMOD_LOM= 1<<1,  // Listen Only Mode (writable only in RESET)
        sjaMOD_RM = 1      // Reset Mode
};

/** Command Register 0x01 */
enum sja1000_PeliCAN_CMR {
        sjaCMR_SRR= 1<<4,  // Self Reception Request (GoToSleep in BASIC mode)
        sjaCMR_CDO= 1<<3,  // Clear Data Overrun
        sjaCMR_RRB= 1<<2,  // Release Receive Buffer
        sjaCMR_AT = 1<<1,  // Abort Transmission
        sjaCMR_TR = 1      // Transmission Request
};

/** Status Register 0x02 */
enum sja1000_SR {
        sjaSR_BS  = 1<<7,  // Bus Status
        sjaSR_ES  = 1<<6,  // Error Status
        sjaSR_TS  = 1<<5,  // Transmit Status
        sjaSR_RS  = 1<<4,  // Receive Status
        sjaSR_TCS = 1<<3,  // Transmission Complete Status
        sjaSR_TBS = 1<<2,  // Transmit Buffer Status
        sjaSR_DOS = 1<<1,  // Data Overrun Status
        sjaSR_RBS = 1      // Receive Buffer Status
};

/** Interrupt Enable Register 0x04
 * WARNING: the chip automatically enters RESET (bus off) mode when
 * error counter > 255 */
enum sja1000_PeliCAN_IER {
        sjaIER_BEIE= 1<<7, // Bus Error Interrupt Enable
        sjaIER_ALIE= 1<<6, // Arbitration Lost Interrupt Enable
        sjaIER_EPIE= 1<<5, // Error Passive Interrupt Enable
        sjaIER_WUIE= 1<<4, // Wake-Up Interrupt Enable
        sjaIER_DOIE= 1<<3, // Data Overrun Interrupt Enable
        sjaIER_EIE = 1<<2, // Error Warning Interrupt Enable
        sjaIER_TIE = 1<<1, // Transmit Interrupt Enable
        sjaIER_RIE = 1,    // Receive Interrupt Enable
        sjaENABLE_INTERRUPTS = sjaIER_BEIE | sjaIER_ALIE | sjaIER_EPIE |
                               sjaIER_WUIE | sjaIER_DOIE | sjaIER_EIE  |
                               sjaIER_TIE  | sjaIER_RIE,
        sjaDISABLE_INTERRUPTS = 0
};

/** Arbitration Lost Capture Register 0x0b.
 * Counting starts from 0 (bit1 of ID). Bits 5-7 reserved */
enum sja1000_PeliCAN_ALC {
        sjaALC_SRTR = 0x0b,// Arbitration lost in bit SRTR
        sjaALC_IDE  = 0x1c, // Arbitration lost in bit IDE
        sjaALC_RTR  = 0x1f, // Arbitration lost in RTR
};

/** Error Code Capture Register 0x0c */
enum sja1000_PeliCAN_ECC {
        sjaECC_ERCC1 = 1<<7,
        sjaECC_ERCC0 = 1<<6,
        sjaECC_BIT   = 0,
        sjaECC_FORM  = sjaECC_ERCC0,
        sjaECC_STUFF = sjaECC_ERCC1,
        sjaECC_OTHER = sjaECC_ERCC0 | sjaECC_ERCC1,
        sjaECC_DIR   = 1<<5,     // 1 == RX, 0 == TX
        sjaECC_SEG_M = (1<<5) -1 // Segment mask (p.37 of SJA1000 Datasheet)
};

/** Frame format information 0x10 */
enum sja1000_PeliCAN_FRM {
        sjaFRM_FF    = 1<<7,     // Frame Format 1 == extended, 0 == standard
        sjaFRM_RTR   = 1<<6,     // Remote request
        sjaFRM_DLC_M = (1<<4)-1  // Length Mask
};

/** Interrupt (status) Register 0x03 */
enum sja1000_PeliCAN_IR {
        sjaIR_BEI = 1<<7,  // Bus Error Interrupt
        sjaIR_ALI = 1<<6,  // Arbitration Lost Interrupt
        sjaIR_EPI = 1<<5,  // Error Passive Interrupt
        sjaIR_WUI = 1<<4,  // Wake-Up Interrupt
        sjaIR_DOI = 1<<3,  // Data Overrun Interrupt
        sjaIR_EI  = 1<<2,  // Error Warning Interrupt
        sjaIR_TI  = 1<<1,  // Transmit Interrupt
        sjaIR_RI  = 1,     // Receive Interrupt
        sjaIR_ANYERROR = sjaIR_BEI | sjaIR_ALI | sjaIR_EPI |
                         sjaIR_DOI | sjaIR_EI,
};

/** Bus Timing 1 Register 0x07 */
enum sja1000_BTR1 {
        sjaMAX_TSEG1 = 15,
        sjaMAX_TSEG2 = 7
};

/** Output Control Register 0x08 */
enum sja1000_OCR {
        sjaOCR_MODE_BIPHASE = 0,
        sjaOCR_MODE_TEST    = 1,
        sjaOCR_MODE_NORMAL  = 2,
        sjaOCR_MODE_CLOCK   = 3,
        sjaOCR_TX0_LH       = 0x18, // TX0 push-pull not inverted
        sjaOCR_TX1_ZZ       = 0     // TX1 floating (off)
};

/** Clock Divider register 0x1f */
enum sja1000_CDR {
        sjaCDR_PELICAN      = 1<<7,
        sjaCDR_CBP          = 1<<6, // bypass input comparator
        sjaCDR_RXINPEN      = 1<<5, // switch TX1 to generate RX INT
        sjaCDR_CLK_OFF      = 1<<3, // f_out = f_osc/(2*(CDR[2:0]+1))
        sjaCDR_CLKOUT_DIV1  = 7,    // or f_osc if CDR[2:0]==7
        sjaCDR_CLKOUT_DIV2  = 0,
        sjaCDR_CLKOUT_DIV4  = 1,
        sjaCDR_CLKOUT_DIV6  = 2,
        sjaCDR_CLKOUT_DIV8  = 3,
        sjaCDR_CLKOUT_DIV10 = 4,
        sjaCDR_CLKOUT_DIV12 = 5,
        sjaCDR_CLKOUT_DIV14 = 6,
        sjaCDR_CLKOUT_MASK  = 7
};

// sja1000 chip variables (struct can_chip_t -> void *chip_data)
struct sj1000_chip_data_t {
        uint16_t sja1000_cdr_reg;        // CDR register
        uint16_t sja1000_ocr_reg;        // OCR register
};

extern struct can_chip_ops_t the_sja1000_ops;

#endif // _MARTE_CHIP_SJA1000_H_
