/* Desarrollo de la prueba general */

/* En primer lugar, se ha optado por la inicializaci�n de la tarjeta gr�fica con la funci�n que utiliza la memoria asociada. Se utiliza �sta porque permite el uso tanto de las funciones dise�adas para la memoria asociada como las que acceden directamente a la memoria de v�deo. Adem�s esta incializaci�n hace internamente uso de la incializaci�n directa. El modo de v�deo que se utilizar�, ha de ser uno de 16 bits por color ya que si no, nos dar�a error la funci�n de inicializaci�n (recordemos que �sta s�lo funciona con modos de dos bytes por p�xel). El modo finalmente ser� el de 800x600x64K. */

/* Despu�s se han utilizado las primitivas correspondientes a la memoria asociada. As� tendremos una nota de texto en la parte superior de la pantalla con el mensaje "SVGADEMO", en color rojo sobre fondo negro, una l�nea una recta con pendiente -1 en la esquina superior izquierda en color verde, un rect�ngulo relleno en la esquina superior derecha de color azul, un rect�ngulo a modo de marco de color amarillo, una circunferencia en la mitad derecha de la pantalla con un c�rculo conc�ntrico en su interior, siendo una rosa y otro blanco. */

/* Estas figuras aparecer�n en pantalla gracias a que se hace un volcado de esa memoria auxilar a la memoria de v�deo. Esa funci�n es refresca_pantalla_16(). */

/* En ese instante tenemos dibujadas las funciones primitivas correspondientes a la memoria asociada. El programa esperar� a que se introduzca un car�cter por teclado. */

/* Ahora dibujamos las primitivas que utilizan directamente la memoria de v�deo. Estas son una l�nea perpendicular a la creada anteriormente, un c�rculo y una circunferencia dentro de la circunferencia de la memoria asociada. Una elipse inscrita en un rect�ngulo relleno en la parte central izquierda de la pantalla. Bajo esas dos figuras otra elipse, en este caso rellena, dentro de un rect�ngulo sin relleno. */

/* De nuevo el programa se detendr� a la espera de que se pulse una tecla. En cuanto es pulsada la tecla, se dibuja una fotograf�a de un coche en la parte inferior izquierda de la pantalla. En la parte inferior derecha aparece el texto �Mclaren F1� negro sobre fondo blanco. */

/* La ejecuci�n se detendr� y se tendr� que volver a pulsar una tecla. Cuando se haya pulsado, el programa har� una copia del cuadrante inferior derecho de la pantalla, lugar donde se encuentran parte de la fotograf�a, el texto y parte de las circunferencias y c�rculos dibujados. Con ese mismo cuadrante se har� en primer lugar un desplazamiento (scroll) horizontal, en el que veremos como se recorta parte de la fotograf�a original del coche. Despu�s se pegar� en el cuadrante superior izquierdo la copia que se hab�a hecho del cuadrante inferior izquierdo anteriormente. */

/* Finalmente, y tras una nueva pulsaci�n en el teclado se pasar� a modo texto y se finalizar� la demostraci�n. */
/* El c�digo de la prueba general se muestra a continuaci�n, ya que puede ser �til como gu�a para los usuarios. */

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
//#include "mclaren.xpm	"//Nombre de fotograf�a
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
