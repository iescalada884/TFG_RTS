/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                           'l c d _ d r i v e r _ c'
 *
 *                                      C
 *
 *  File 'lcd_driver_c.c'                                  by MAR.
 *
 *  Driver for the LCD of the stm32f769disco board.
 *
 *
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
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

#include <stdint.h>
#include <string.h>
#include "stm32f7xx_hal.h"
#include "stm32f769i_discovery.h"
#include "stm32f769i_discovery_lcd.h"

// Defined in GNAT_IO_Driver_Functions
int gnat_io_driver_functions__create();
ssize_t gnat_io_driver_functions__write(int file_descriptor,
					uint8_t *buffer, size_t bytes);
 
static int initialized = 0;

// cursor position
static uint16_t cursorLine = 0;
static uint16_t cursorCol = 0;

// screen parameters
static uint16_t lineHeight = 10;
static uint16_t colWidth = 10;
static uint16_t LINES_MX = 10;
static uint16_t COLS_MX = 10;

//static uint8_t lineBuffer[100];
#define TOP_OFFSET 0

#define CURSOR_Y  (cursorLine * lineHeight + TOP_OFFSET)
#define CURSOR_X  (cursorCol * colWidth)

ssize_t lcd_write (int file_descriptor, uint8_t *buffer, size_t bytes);

/************/
/*  Create  */
/************/

int lcd_create ()
{
  // LCD can not be used during MaRTE initialization since, during its
  // initialization, it performs a delay which requires interrupts to be
  // enabled in order to count ticks (otherwise initialization gets blocked in
  // the delay).
  // We use a led to show lcd_write has been called.
    
  BSP_LED_Init(LED1);
  BSP_LED_Init(LED2);
  BSP_LED_Toggle(LED1);
  return 0;
}

/**************************************/
/*  lcd_end_of_kernel_initialization  */
/**************************************/

int lcd_end_of_kernel_initialization ()
{
  uint32_t  lcd_status = LCD_OK;

  // Initialize the LCD
  lcd_status = BSP_LCD_Init();
  while(lcd_status != LCD_OK);

  BSP_LCD_LayerDefaultInit(0, LCD_FB_START_ADDRESS);

  // Clear the LCD
  BSP_LCD_Clear(LCD_COLOR_WHITE);
  
  // set font 
  BSP_LCD_SetFont(&Font20);
  
  // console parameters
  lineHeight = ((sFONT *)BSP_LCD_GetFont())->Height;
  colWidth = ((sFONT *)BSP_LCD_GetFont())->Width;
  LINES_MX = (BSP_LCD_GetYSize()-TOP_OFFSET) / lineHeight;
  COLS_MX = BSP_LCD_GetXSize() / colWidth;
  
  initialized = 1;
  
  // write marte header
  BSP_LCD_SetTextColor(LCD_COLOR_BLUE);
  lcd_write(1, (uint8_t *)"MaRTE OS executing user's code...\n", 34); 
  
  BSP_LCD_SetTextColor(LCD_COLOR_BLACK);
  
  return 0;
}

static void nextCol();

static void nextLine() {
  cursorLine = (cursorLine + 1) % LINES_MX;
  cursorCol = 0;
  // clear next line
  for(int i=0; i<COLS_MX; i++) {
    BSP_LCD_DisplayChar(CURSOR_X, CURSOR_Y, ' ');
    cursorCol++;
  }
  cursorCol = 0;
}

static void nextCol() {
  cursorCol = (cursorCol + 1) % COLS_MX;
  if (cursorCol == 0) {
    nextLine();
  }
}
    
/***********/
/*  Write  */
/***********/

ssize_t lcd_write (int file_descriptor, uint8_t *buffer, size_t bytes)
{
  if (!initialized) {
    BSP_LED_Toggle(LED2);
    return bytes;
  }
  
  for(int i=0; i<bytes; i++) {
    if (buffer[i] == '\n') {
      nextLine();
    } else {
      BSP_LCD_DisplayChar(CURSOR_X, CURSOR_Y, buffer[i]);
      nextCol();
    }
  }
  
/*   if (buffer[bytes-1] != '\n') { */
/*     BSP_LCD_DisplayChar(CURSOR_X, CURSOR_Y, '\n'); */
/*   } */
    
/*   memcpy(lineBuffer, buffer, bytes); */
/*   lineBuffer[bytes] = 0; */
/*    */
/*   BSP_LCD_DisplayStringAt(CURSOR_X, CURSOR_Y, lineBuffer, LEFT_MODE); */
/*    */
/*   cursorLine++; */
/*   cursorCol = 0; */
  
  return bytes;
}


