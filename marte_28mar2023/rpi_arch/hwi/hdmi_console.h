#ifndef HDMI_CONSOLE_H
#define HDMI_CONSOLE_H

typedef struct Coord {
   unsigned int x;
   unsigned int y;
} Coord;

typedef struct FrameBuferDescription {
	unsigned int width;
	unsigned int height;
	unsigned int vWidth;
	unsigned int vHeight;
	unsigned int pitch;
	unsigned int bitDepth;
	unsigned int x;
	unsigned int y;
	void* pointer;
	unsigned int size;
} FrameBuferDescription;

Coord position;

void hdmi_console_putChar(char c);
void hdmi_console_putInt(int i,int base);
void hdmi_console_putUInt(unsigned long long int i,int base);
void hdmi_console_putString(char *str, int length);
int hdmi_console_putDirect(int fd,char *str,int lenght);
void hdmi_console_putLine();
int hdmi_console_init();
void hdmi_console_end_of_kernel_initialization();
void hdmi_console_reset();
void ledOn();
void ledOff();
unsigned char hdmi_console_read();

void hdmi_console_marte_int32(int i, int base);
void hdmi_console_marte_int8(char c, int base);
void hdmi_console_marte_unsigned64(unsigned long long i, int base);
void hdmi_console_marte_unsigned32(unsigned int i, int base);
void hdmi_console_marte_unsigned8(unsigned char c, int base);

#endif	/* HDMI_CONSOLE_H */
