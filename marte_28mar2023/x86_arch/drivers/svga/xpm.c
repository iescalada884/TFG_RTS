#include <stdio.h>	//Para sscanf()
#include "xpm.h"


int comienzo_rgb;

/*Consigo los valores de la cabecera de la fotografia. Devuelve 0 si se ha rellenado
correctamente el struct. Es necesario para cada fotografia.*/
int init_struct_xpm(struct xpm * foto,char ** nombre_foto)
{
  unsigned char * linea;
  int valor;
  int i;
  
  foto->nombre_struct_foto=nombre_foto;
  linea=nombre_foto[0];
  valor=sscanf(linea,"%d %d %d %d",&foto->ancho,&foto->alto,&foto->n_colores,&foto->caracterespixel);
  if (valor!=4)
    return 1;
  
/*Buscamos el numero de caracteres distintos de la foto.
Suponemos que la fotografia sigue un patron de letras (hasta el momento se ha comprobado en todas	
las vistas). Este patron consiste en colocar en primer lugar los distintos caracteres utilizados	
(como mucho 256, de ahi la condicion de error de la funcion). Si se precisan mas de 1 caracter se	
colocan como segundo siguiendo el mismo patron: primero se pone el primer caracter el numero	
combinandolo con el resto en el mismo orden, despues el segundo	
	aa	
 	ba	
  	ca	
   	ab	
    	bb	
     	cb	
      	ac	
       	bc	
        cc.	
*/	
  if (foto->caracterespixel>1)	
    {	
      for(i=2;i<257;i++)	
	if (foto->nombre_struct_foto[1][0]==foto->nombre_struct_foto[i][0])	
	  {	
	    foto->caracteres_totales=i-1;	
	    return 0;	
	  }	
    }else{	
      foto->caracteres_totales=foto->n_colores;	
      return 0;	
    }	
  return -1;	
}

/*Funcion que eleva el primer valor al segundo*/
int elevado_a(int x,int y)
{
	int i;
	int resultado=1;
	for(i=1;i<(y+1);i++)
		resultado=resultado*x;
	return resultado;
}

/*Devuelve a partir de la combinacion de caracteres por pixel el valor RGB en binario (24 bits)*/
int return_rgb(struct xpm foto,point_t pixel)
{
  char *combinacion;
  char valor;
  int i,j;
  int auxiliar=0;
  int devolver=0;
  
  /*Consigo la combinacion de caracteres de un pixel concreto*/
  combinacion=&foto.nombre_struct_foto[foto.n_colores+1+pixel.y][foto.caracterespixel*pixel.x];
  
  /*Localizo la posicion en la paleta de la combinacion de letras*/	
  auxiliar=0;
  for(j=0;j<foto.caracterespixel;j++)
    {	
      valor=combinacion[j];
      for(i=0;i<=foto.caracteres_totales-1;i++)
	if(foto.nombre_struct_foto[i+1][0]==valor)
	  {
	    auxiliar=auxiliar+i*elevado_a(foto.caracteres_totales,j);
	    break;
	  }
    }
  auxiliar=auxiliar+1;/*Debido a que empiza la tabla a partir de la posicion 2*/
  
  /*Localizo el lugar donde se encuentra el código RGB*/
  if (comienzo_rgb==0)
	{
	  i=foto.caracterespixel;
	  while(foto.nombre_struct_foto[auxiliar][i]!='\0')
	    {
	      if (foto.nombre_struct_foto[auxiliar][i]=='#')
		break;
	      i++;
	    }
	  comienzo_rgb=i+1;
 	}
  if(foto.nombre_struct_foto[auxiliar][comienzo_rgb-1]=='\0')
    return 0;	//Devuelve negro si no ha encontrado la combinacion
  
  /*Extraigo el valor RGB*/
  valor=0;
  i=comienzo_rgb;
  while(foto.nombre_struct_foto[auxiliar][i]!='\0')
    {
      if (foto.nombre_struct_foto[auxiliar][i]<58)	/*Se trata de un numero*/
	devolver=devolver*16+(foto.nombre_struct_foto[auxiliar][i]-48);
      else                            /*Se trata de una letra mayuscula*/
	devolver=devolver*16+(foto.nombre_struct_foto[auxiliar][i]-55);
      
      i+=1;
    }
  return(devolver); 	
}


/*Dibujo la imagen total en la posicion point*/
int draw_image_complete(struct xpm foto,point_t point)
{
  int i,j;
  int limite_ancho,limite_alto;
  int width;
  int high;
  point_t auxiliar;
  unsigned int rgb24;
  
  char *video_buf; //Buffer de video de tarjeta	
  
  video_buf=vga_getgraphmem();	
  
  width=vga_getxdim();
  high=vga_getydim();
  
  if ((point.x<0) | (point.x>width))	
    return -1;	
  if ((point.y<0) | (point.y>high))	
    return -1;	
  
  if((foto.ancho+point.x)>width)
    limite_ancho=width-point.x;
  else
    limite_ancho=foto.ancho;
  
  if((foto.alto+point.y)>high)
    limite_alto=high-point.y;		
  else		
    limite_alto=foto.alto;
  
  for(i=0;i<limite_alto;i++)
    for(j=0;j<limite_ancho;j++)
      {
	auxiliar.x=j;
	auxiliar.y=i;
	rgb24=return_rgb(foto,auxiliar);
	vga_setcolor(conversor_24_a_16(rgb24));
	vga_drawpixel(j+point.x,i+point.y);
	//	grx_pixel(j+point.x,i+point.y,conversor_24_a_16(rgb24));
      }	
  //  copy_videomem_16to16(buf_rgb, video_buf,width*high*2/4);
  return 0;
}

/*Dibujo una parte de la imagen total*/
int draw_image_partial(struct xpm foto, point_t newposition,point_t point1,point_t point2)
{
  unsigned int rgb24;
  int i,j;
  int limite_ancho,limite_alto;
  int width,high;
  char *video_buf; //Buffer de video de tarjeta
  point_t auxiliar;
  int colores_pantalla;
  
  video_buf=vga_getgraphmem();
  colores_pantalla=vga_getcolors();
  width=vga_getxdim();
  high=vga_getydim();
  
  if ((newposition.x<0) | (newposition.x>width))
    return -1;
  if ((newposition.y<0) | (newposition.y>high))
    return -1;
  
  if ((point1.x<0) | (point2.x>foto.ancho))
    return -1;
  if ((point1.y<0) | (point2.y>foto.alto))
    return -1;
  
  if((point2.x-point1.x+newposition.x)>width)
    limite_ancho=width-newposition.x;
  else
    limite_ancho=point2.x-point1.x;
  
  if(point2.y-point1.y+newposition.y>high)
    limite_alto=high-newposition.y;	
  else		
    limite_alto=point2.y-point1.y;
  
  for(i=0;i<limite_alto;i++)
    for(j=0;j<limite_ancho;j++)
      { 
	auxiliar.x=j+point1.x;
	auxiliar.y=i+point1.y;
	rgb24=return_rgb(foto,auxiliar);	
	grx_pixel(j+newposition.x,i+newposition.y,conversor_24_a_16(rgb24));	
      };
  
  copy_rect_videomem_16to16(buf_rgb,video_buf,newposition.x,newposition.y,(point2.x-point1.x+newposition.x),(point2.y-point1.y+newposition.y));
  
  return 0;
}












