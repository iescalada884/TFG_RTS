#include "hdmi_console.h"

//#define DISABLE_HDMI_CONSOLE 1

#ifdef DISABLE_HDMI_CONSOLE
// uses UART
static volatile unsigned int * const UART0DR = (unsigned int *)0x101f1000;
 
static void print_uart0(const char *s) {
 while(*s != '\0') { /* Loop until end of string */
 *UART0DR = (unsigned int)(*s); /* Transmit char */
 s++; /* Next char */
 }
}

static void put_char_uart0(char c) {
  *UART0DR = (unsigned int)c;
}

static void put_string_uart0(char *str, int length) {
  int i=0;
  for(i=0; i<length; i++) {
    put_char_uart0(*str);
    str++;
  }
}
#endif

#define resX 656
#define resY 416
#define tamX 10
#define tamY 20

extern unsigned long DrawCharacter(char character, unsigned int x, unsigned int y);
extern void DrawString(char* string, unsigned int length, unsigned int x, unsigned int y);
extern FrameBuferDescription* InitialiseFrameBuffer(unsigned int width,unsigned int height,unsigned int bitDepth);

int Initialized=0;

int hdmi_console_init(){
#ifdef DISABLE_HDMI_CONSOLE
  return 0;
#endif
	if (Initialized != 1){
		FrameBuferDescription* frameBuffer;
		position.x=0;
		position.y=0;

		frameBuffer=InitialiseFrameBuffer(656,416,16);
		SetGraphicsAddress(frameBuffer);
	}
	Initialized = 1;
	return 0;
}

void hdmi_console_end_of_kernel_initialization(){
	;
}

void hdmi_console_reset(){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
	FrameBuferDescription* frameBuffer;
	position.x=0;
	position.y=0;

	frameBuffer=InitialiseFrameBuffer(656,416,16);
	SetGraphicsAddress(frameBuffer);
}

void hdmi_console_putChar(char c){
#ifdef DISABLE_HDMI_CONSOLE
  put_char_uart0(c);
#else
	DrawCharacter(c,position.x,position.y);
	
	position.x+=tamX;
	if (position.x>resX){
		position.x=0;
		position.y+=tamY;
		if (position.y>resY){
			position.y=0;
		}
	}
#endif
}


void hdmi_console_putInt(int number,int base){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
	if (number<0){
		hdmi_console_putChar('-');
		number = -number;
	}
	if (base==16){
		hdmi_console_putIntHex(number);
	}else{
		hdmi_console_putIntDec(number);
	}
	
}

void hdmi_console_putIntHex(int number){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
	int n;
	if (number>=16){
			hdmi_console_putIntHex(number/16);
	}
	n = number%16;
	if (n>9){
		hdmi_console_putChar(n-10+65);
	}else{
		hdmi_console_putChar(n+48);
	}
}

void hdmi_console_putIntDec(int number){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
        int n;
        if (number>=10){
        	hdmi_console_putIntDec(number/10);
        }
        hdmi_console_putChar(number%10+48);
}


void hdmi_console_putUInt(unsigned long long int number,int base){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
	if (base==16){
		hdmi_console_putUIntHex(number);
	}else{
		hdmi_console_putUIntDec(number);
	}
}

void hdmi_console_putUIntHex(unsigned long int number){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
	int n;
	if (number>=16){
		hdmi_console_putUIntHex(number/16);
	}
	n = number%16;
	if (n>9){
		hdmi_console_putChar(n-10+65);
	}else{
		hdmi_console_putChar(n+48);
	}
}

void hdmi_console_putUIntDec(unsigned long int number){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
        if (number>=10){
                hdmi_console_putUIntDec(number/10);
        }
        hdmi_console_putChar(number%10+48);
}


void hdmi_console_putString(char *str, int length){
#ifdef DISABLE_HDMI_CONSOLE
  put_string_uart0(str, length);
#else
  if (position.x+length*tamX>resX-50){
    position.x=0;
		position.y+=tamY;
		if (position.y>resY){
			position.y=0;
		}
  }
	DrawString(str,length,position.x,position.y);
	position.x+=length*tamX;
	if (position.x>resX){
		position.x=0;
		position.y+=tamY;
		if (position.y>resY){
			position.y=0;
		}
	}
#endif
}

void hdmi_console_putLine(){
#ifdef DISABLE_HDMI_CONSOLE
  put_char_uart0('\n');
#else
	position.y+=tamY;
	if (position.y > resY) {
		position.y=0;
	}
	position.x=0;
#endif
}

int hdmi_console_putDirect(int fd,char *str,int lenght){
	hdmi_console_putString(str,lenght);
#ifndef DISABLE_HDMI_CONSOLE
	if (str[lenght-1]=='\n') {
		hdmi_console_putLine();
	}
#endif
	return lenght;
}

void ledOn(){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
    /* Assign the address of the GPIO peripheral (Using ARM Physical Address) */
    unsigned int* gpio = (unsigned int*)0x20200000;
 
    /* Write 1 to the GPIO16 init nibble in the Function Select 1 GPIO
       peripheral register to enable GPIO16 as an output */
    gpio[1] |= (1 << 18);
 
    /* Write 1 to the 16th bit in the Clear0 GPIO peripheral register to set
       the output level of GPIO16 as 0 (logic low) */
    gpio[10] = (1 << 16);
}

void ledOff(){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
    /* Assign the address of the GPIO peripheral (Using ARM Physical Address) */
    unsigned int* gpio = (unsigned int*)0x20200000;

    /* Write 1 to the GPIO16 init nibble in the Function Select 1 GPIO
       peripheral register to enable GPIO16 as an output */
    gpio[1] |= (1 << 18);

    /* Write 1 to the 16th bit in the Clear0 GPIO peripheral register to set
       the output level of GPIO16 as 0 (logic low) */
    gpio[7] = (1 << 16);
}

unsigned char hdmi_console_read(){
	return 'a';
}

void hdmi_console_marte_int32(int i, int base){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
  hdmi_console_putInt(i,base);
}

void hdmi_console_marte_int8(char c, int base){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
  hdmi_console_putInt((int)c,base);
}


void hdmi_console_marte_unsigned64(unsigned long long i, int base){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
  hdmi_console_putUInt(i,base);
}

void hdmi_console_marte_unsigned32(unsigned int i, int base){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
  hdmi_console_putUInt((unsigned long long)i,base);
}

void hdmi_console_marte_unsigned8(unsigned char c, int base){
#ifdef DISABLE_HDMI_CONSOLE
  return;
#endif
  hdmi_console_putUInt((unsigned long long)c,base);
}

