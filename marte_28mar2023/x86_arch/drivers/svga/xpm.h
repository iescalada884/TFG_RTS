/*Cabecera usada para dibujar imagenes en xpm en MaRTE OS.	*
*Es necesario haber inicializado la pantalla			*/

#include "vga.h"

/*Struct que nos permitira manipular la imagen*/
/* - caracteres_totales nos indica el numero de caracters distintos presentes en la codificacion de la foto.
   - nombre_struct_foto es un puntero a la foto, esto es, al array de strings que nos devuelve el archivo
de la foto. Para conseguir este puntero tendremos que abrir con un editor el fichero xpm y verlo*/

struct xpm {
  int alto;
  int ancho;
  int n_colores;
  int caracterespixel;
  int caracteres_totales;
  char ** nombre_struct_foto;
};

/*Consigue los valores de la cabecera de la fotografia. Es necesario para cada fotografia
 - foto nos devuelve completo el struct xpm
 - nombre_foto pasa el array de strings de la foto en xpm(leer el comentario anterior puesto que se
 rellenara con este valor el campo nombre_struct_foto)*/
int init_struct_xpm(struct xpm * foto,char ** nombre_foto);


/*Devuelve el valor RGB24 en binario a partir de la posicion que ocupa un pixel en una foto(24 bits)*/
int return_rgb(struct xpm foto, point_t pixel);


/*Dibuja la imagen total a partir de la posicion point*/
int draw_image_complete(struct xpm foto, point_t point);


/*Dibuja un rectangulo de la imagen total
	point1
		¡-------¡
		¡	¡
		¡_______¡
			  point2
a partir de la posicion newposition*/
/*Recuerdo que la esquina superior izquierda es la coordenada (0,0)*/
int draw_image_partial(struct xpm foto, point_t new_position, point_t point1,point_t point2);



























