/* Desarrollo de la prueba general */

/* En primer lugar, se ha optado por la inicialización de la tarjeta gráfica con la función que utiliza la memoria asociada. Se utiliza ésta porque permite el uso tanto de las funciones diseñadas para la memoria asociada como las que acceden directamente a la memoria de vídeo. Además esta incialización hace internamente uso de la incialización directa. El modo de vídeo que se utilizará, ha de ser uno de 16 bits por color ya que si no, nos daría error la función de inicialización (recordemos que ésta sólo funciona con modos de dos bytes por píxel). El modo finalmente será el de 800x600x64K. */

/* Después se han utilizado las primitivas correspondientes a la memoria asociada. Así tendremos una nota de texto en la parte superior de la pantalla con el mensaje "SVGADEMO", en color rojo sobre fondo negro, una línea una recta con pendiente -1 en la esquina superior izquierda en color verde, un rectángulo relleno en la esquina superior derecha de color azul, un rectángulo a modo de marco de color amarillo, una circunferencia en la mitad derecha de la pantalla con un círculo concéntrico en su interior, siendo una rosa y otro blanco. */

/* Estas figuras aparecerán en pantalla gracias a que se hace un volcado de esa memoria auxilar a la memoria de vídeo. Esa función es refresca_pantalla_16(). */

/* En ese instante tenemos dibujadas las funciones primitivas correspondientes a la memoria asociada. El programa esperará a que se introduzca un carácter por teclado. */

/* Ahora dibujamos las primitivas que utilizan directamente la memoria de vídeo. Estas son una línea perpendicular a la creada anteriormente, un círculo y una circunferencia dentro de la circunferencia de la memoria asociada. Una elipse inscrita en un rectángulo relleno en la parte central izquierda de la pantalla. Bajo esas dos figuras otra elipse, en este caso rellena, dentro de un rectángulo sin relleno. */

/* De nuevo el programa se detendrá a la espera de que se pulse una tecla. En cuanto es pulsada la tecla, se dibuja una fotografía de un coche en la parte inferior izquierda de la pantalla. En la parte inferior derecha aparece el texto “Mclaren F1” negro sobre fondo blanco. */

/* La ejecución se detendrá y se tendrá que volver a pulsar una tecla. Cuando se haya pulsado, el programa hará una copia del cuadrante inferior derecho de la pantalla, lugar donde se encuentran parte de la fotografía, el texto y parte de las circunferencias y círculos dibujados. Con ese mismo cuadrante se hará en primer lugar un desplazamiento (scroll) horizontal, en el que veremos como se recorta parte de la fotografía original del coche. Después se pegará en el cuadrante superior izquierdo la copia que se había hecho del cuadrante inferior izquierdo anteriormente. */

/* Finalmente, y tras una nueva pulsación en el teclado se pasará a modo texto y se finalizará la demostración. */
/* El código de la prueba general se muestra a continuación, ya que puede ser útil como guía para los usuarios. */

#include <stdio.h>
#include <vga.h>
#include <unistd.h>
#include <stdlib.h>
//#include <debug_marte.h>
#include <sys/pci_ids.h>
#define INITSTR G320x240x256
//#define INITSTR G640x480x16

//#define INITSTR G800x600x256 //SVGAlib standard mode definitions
#define VENDOR VGA//Video vendor
#define CARD PCI_DEVICE_ID_S3_TRIO64V2//Video driver
//#include "mclaren.xpm	"//Nombre de fotografía
//#define COCHE mclaren

int main ()
{
  	int i;
  //	char c;
  //	slice_t semiframe;
	point_t point1,point2;
	//	point_t point3;
	//struct xpm foto;
	//init_serial_communication_with_gdb (SERIAL_PORT_1);
	//set_break_point_here;
  	if (init_vga(INITSTR,VENDOR,CARD)) {
   		exit(1);
  	}

/* 	grx_text("SVGADEMO", 10,25,rgb16(255,0,0), 0); */
/* 	grx_line(10,10,50,50,rgb16(0,255,0)); */
/* 	grx_box(350,50,400,100,rgb16(0,0,255)); */
/* 	grx_rect(0,0,799,599,rgb16(255,255,0)); */
/* 	grx_circle(600,400,50,rgb16(255,255,255)); */
/* 	grx_disc(600,400,5,rgb16(255,0,127)); */
/* 	refresh_screen(); */
/* 	getchar(); */
	for (i=0;i<256;i++){
	  point1.x=i;
	  point1.y=0;
	  point2.x=i;
	  point2.y=239;
	  vga_line(point1,point2,i);
	};

	getchar();

	vga_setmode(TEXT);
  
  	printf("HASTA AQUI\n");
  	exit(0);
}
