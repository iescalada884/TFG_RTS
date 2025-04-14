/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                'u t m C p p . c'
 *
 *                                      C
 *
 *  File 'utmCpp.c'                                  by F.J.Feijoo
 *                                            University of Zaragoza (UNIZAR)
 *
 *
 *  Implementacion Ecuaciones de Coticchia-Surace para cambio de coordenadas
 *
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2000-2007, Universidad de Cantabria, SPAIN
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


#include <stdio.h>
#include "utmCpp.h"
//#include <iostream>
//#define DEBUG 0
/** Funciones privadas **/
/* Ecuaciones de Coticchia - Surace */
void CS(GeoUTM *geo){

   /* Variables */
   double aux;
   double A, A1, A2, B, J2, J4, J6;
   
   /* Letras griegas */
   double dseda, eta, ipsilon, sigma, alfa, beta, gamma;     
   
   A = cos(geo->latRad) * sin(geo->distAngular);
   
   dseda = 0.5 * log((1.0 + A) / (1.0 - A));
   eta = atan( tan(geo->latRad) / cos(geo->distAngular) ) - geo->latRad;
   
   aux = cos(geo->latRad);
   aux = aux * aux;   
   ipsilon = geo->c * 0.9996 / sqrt(1.0 + geo->ep2 * aux);
   
   sigma = 0.5 * geo->ep2 * dseda * dseda * aux;
   
   A1 = sin(2.0 * geo->latRad); A2 = A1 * aux;
   
   J2 = geo->latRad + A1 * 0.5; 
   J4 = 0.75 * J2 + 0.25 * A2;
   J6 = (5.0 * J4 + A2*aux)/3;
   
   alfa = 0.75 * geo->ep2;
   aux = alfa * alfa;
   beta = (5.0 * aux) / 3.0;
   gamma = (35.0 * aux * alfa) / 27.0;
   
   B = 0.9996 * geo->c *(geo->latRad - alfa * J2 + beta * J4 - gamma * J6);
   
   geo->x = dseda * ipsilon * ( 1.0 + sigma / 3.0) + 500000.0;
   geo->y = eta * ipsilon * ( 1.0 + sigma) + B;
   
   geo->h = N;

   if(geo->y < 0){
	   geo->y += 10000000.0; 
	   geo->h = S;
   }

// #if DEBUG == 1
//    printf( "         CS\n" );
//    printf( "         X=%f\n", geo->x);
//    printf( "         Y=%f\n", geo->y);;
//    printf( "         huso=%d\n",  geo->huso );
//    printf( "         Mer=%f\n",  geo->merCentral);
//    printf( "         dis=%f\n",  geo->distAngular);
//    printf( "-----------------------------\n");
// #endif
}

/*****************************************************************************/

// Constructor por defecto
void initGeoUTM(GeoUTM *geo, Elipsoide e){
    setElipsoide(geo, e);
}

/*****************************************************************************/

// Constructor general
void initGeoUTM2(GeoUTM *geo,double semiejeMayor, double semiejeMenor){
	setElipsoideSemiejes(geo, semiejeMayor, semiejeMenor);
}

/*****************************************************************************/

void setElipsoide(GeoUTM *geo, Elipsoide e){
	geo->a = semiejes[e][0];
	geo->b = semiejes[e][1];
	geo->ep = sqrt( (geo->a * geo->a) - (geo->b * geo->b)) / geo->b;
	geo->ep2 = geo->ep * geo->ep;
	geo->c = (geo->a * geo->a) / geo->b;
}

/*****************************************************************************/

void setElipsoideSemiejes(GeoUTM *geo,double semiejeMayor, double semiejeMenor){
	geo->a = semiejeMayor;
	geo->b = semiejeMenor;
	geo->ep = sqrt( (geo->a * geo->a) - (geo->b * geo->b)) / geo->b;
	geo->ep2 = geo->ep * geo->ep;
	geo->c = (geo->a * geo->a) / geo->b;
}

/*****************************************************************************/

void geo2UTM(GeoUTM *geo){
   
   /* Calculos previos */
   geo->latRad = (geo->lat * M_PI) / 180.0;
   geo->lonRad = (geo->lon * M_PI) / 180.0;
   geo->huso = (int)(geo->lon / 6 + 31);
   
   geo->merCentral = ((geo->huso * 6 - 183) * M_PI) / 180;
      
   geo->distAngular = geo->lonRad - geo->merCentral;

// #if DEBUG == 1
//    printf( "         geo2UTM\n");
//    printf( "         lat= %f\n", geo->lat);
//    printf( "         lon= %f\n", geo->lon);
//    printf( "         latR= %f\n", geo->latRad);
//    printf( "         lonR= %f\n", geo->lonRad);
//    printf( "         huso= %d\n",geo-> huso); 
//    printf( "         Mer= %f\n", geo->merCentral);
//    printf( "         dis= %f\n", geo->distAngular);
//    printf( "-----------------------------\n");
// #endif

   CS(geo);
   
}

/*****************************************************************************/

void geo2UTM_Huso(GeoUTM *geo){
   
   /* Calculos previos */
   geo->latRad = (geo->lat * M_PI) / 180.0;
   geo->lonRad = (geo->lon * M_PI) / 180.0;
   
   geo->distAngular = geo->lonRad - geo->merCentral;

// #if DEBUG == 1
//    printf( "         geo2UTM\n");
//    printf( "         lat= %f\n", geo->lat);
//    printf( "         lon= %f\n", geo->lon );
//    printf( "         latR= %f\n", geo->latRad );
//    printf( "         lonR= %f\n", geo->lonRad );
//    printf( "         huso= %d\n",geo-> huso); 
//    printf( "         Mer= %f\n", geo->merCentral );
//    printf( "         dis= %f\n", geo->distAngular);
//    printf( "-----------------------------\n");
// #endif

   CS(geo);
   
}

/*****************************************************************************/

void UTM2geo(GeoUTM *geo){

   double auxX, auxY;
   double auxAlfa, aux;
   double A1, A2, J2, J4, J6, B;
   
   /* Letras griegas */
   double dseda, eta, ipsilon, sigma, alfa, beta, gamma, tau;
   
   auxX = geo->x - 500000.0;
   auxY = geo->y;
   if(geo->h == S ){
	   auxY -= 10000000.0;
   }
   
   geo->latRad = auxY/(6366197.724 * 0.9996);
   
   aux = cos(geo->latRad);
   aux = aux * aux;   
   ipsilon = geo->c * 0.9996 / sqrt(1.0 + geo->ep2 * aux);
   
   double a,b;

   a = auxX / ipsilon;

   A1 = sin(2 * geo->latRad);
   A2 = A1 * aux;
   
   J2 = geo->latRad + 0.5 * A1;
   J4 = 0.75 * J2 + 0.25 * A2;
   J6 = (5.0 * J4 + A2*aux)/3;
   
   alfa = 0.75 * geo->ep2;
   auxAlfa = alfa * alfa;
   beta = (5.0 * auxAlfa) / 3.0;
   gamma = (35.0 * auxAlfa * alfa) / 27.0;
   
   B = 0.9996 * geo->c *(geo->latRad - alfa * J2 + beta * J4 - gamma * J6);

   b = (auxY - B) / ipsilon;

   sigma = 0.5 * geo->ep2 * a * a * aux;
   dseda = a * (1 - sigma / 3.0);
   eta = b * (1 - sigma) + geo->latRad;

   geo->distAngular = atan2(sinh(dseda), cos(eta));

   tau = atan(cos(geo->distAngular) * tan(eta));

   geo->lonRad = geo->distAngular + geo->merCentral;
   geo->lon = (geo->lonRad * 180.0) / M_PI;

   geo->latRad = geo->latRad + 
	      ( 1.0 + geo->ep2 * aux 
	        - 1.5 * geo->ep2 * sin(geo->latRad) * cos(geo->latRad) * (tau - geo->latRad)
		  ) * (tau - geo->latRad);
   geo->lat = (geo->latRad * 180.0) / M_PI;

//    #if DEBUG == 1
//    printf( "         UTM2geo\n");
//    printf( "         lat= %f\n", geo->lat);
//    printf( "         lon= %f\n", geo->lon);
//    printf( "         latR= %f\n", geo->latRad);
//    printf( "         lonR= %f\n", geo->lonRad);
//    printf( "         dis= %f\n", geo->distAngular);
//    printf( "         huso= %d\n", geo->huso);
//    printf( "         B= %f\n", B);
//    printf( "         tau= %f\n", tau);
//    printf( "-----------------------------\n");
//    #endif
}

