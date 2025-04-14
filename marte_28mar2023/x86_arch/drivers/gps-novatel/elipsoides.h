/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                            'e l i p s o i d e s . h'
 *
 *                                      C
 *
 *  File 'elipsoides.h'                                  by F.J.Feijoo
 *                                              University of Zaragoza (UNIZAR)
 *
 *
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
#ifndef __ELIPSOIDES_H_
#define __ELIPSOIDES_H_

#include <string.h>
#include <drivers/player.h> // for strcasecmp

/*
Elipsoide       Fecha   a (semieje mayor)   b (semieje menor)

Airy1830        1830    6377563,396000      6356256,910000
Airy1965        1965    6377340,189000      6356034,447900
Bessel          1841    6377397,155000      6356078,962840
Clarke1866      1866    6378206,400000      6356583,800000
Clarke1880      1880    6378249,145000      6356514,869550
Fischer1960     1960    6378166,000000      6356784,280000
Fischer1968     1968    6378150,000000      6356768,330000
GRS80           1980    6378137,000000      6356752,314140
Hayford         1909    6378388,000000      6356911,946130  ==> Europeo 1950
Helmert1906     1906    6378200,000000      6356818,170000
Hough1960       1960    6378270,000000      6356794,343479
Krasovsky       1940    6378245,000000      6356863,018800
Mercury1960     1960    6378166,000000      6356784,283666
Mercury1968     1968    6378150,000000      6356768,337303
Internacional           1967    6378157,500000      6356772,200000
Sudamericano            1969    6378160,000000      6356774,720000
Walbeck         1817    6376896,000000      6355834,846700
WGS66           1966    6378145,000000      6356759,769356
WGS72           1972    6378135,000000      6356750,519915
WGS84           1984    6378137,000000      6356752,314245
*/

// Elipsoides conocidos
typedef enum Elipsoide{
    Airy1830 = 0,
    Airy1965,
    Bessel,
    Clarke1866,
    Clarke1880,
    Fischer1960,
    Fischer1968,
    GRS80,
    Hayford, // Europeo 1950 ED50
    Helmert1906,
    Hough1960,
    Krasovsky,
    Mercury1960,
    Mercury1968,
    Internacional1967,
    Sudamericano,
    Walbeck,
    WGS66,
    WGS72,
    WGS84
}Elipsoide;

// 1er dato semieje mayor a
// 2nd  dato semieje menor b
const double semiejes[][2] = {
    {6377563.396000, 6356256.910000},
    {6377340.189000, 6356034.447900},
    {6377397.155000, 6356078.962840},
    {6378206.400000, 6356583.800000},
    {6378249.145000, 6356514.869550},
    {6378166.000000, 6356784.280000},
    {6378150.000000, 6356768.330000},
    {6378137.000000, 6356752.314140},
    {6378388.000000, 6356911.946130},
    {6378200.000000, 6356818.170000},
    {6378270.000000, 6356794.343479},
    {6378245.000000, 6356863.018800},
    {6378166.000000, 6356784.283666},
    {6378150.000000, 6356768.337303},
    {6378157.500000, 6356772.200000},
    {6378160.000000, 6356774.720000},
    {6376896.000000, 6355834.846700},
    {6378145.000000, 6356759.769356},
    {6378135.000000, 6356750.519915},
    {6378137.000000, 6356752.314245}
};

Elipsoide traducirElipsoide(const char * elipsoide){
    if(strcasecmp(elipsoide, "Airy1830")== 0){
        return Airy1830;
    }else if(strcasecmp(elipsoide, "Airy1965")== 0){
        return Airy1965;
    }else if(strcasecmp(elipsoide, "Bessel")== 0){
        return Bessel;
    }else if(strcasecmp(elipsoide, "Clarke1866")== 0){
        return Clarke1866;
    }else if(strcasecmp(elipsoide, "Clarke1880")== 0){
        return Clarke1880;
    }else if(strcasecmp(elipsoide, "Fischer1960")== 0){
        return Fischer1960;
    }else if(strcasecmp(elipsoide, "Fischer1968")== 0){
        return Fischer1968;
    }else if(strcasecmp(elipsoide, "GRS80")== 0){
        return GRS80;
    }else if(strcasecmp(elipsoide, "Hayford")== 0 ||
             strcasecmp(elipsoide, "ED50") == 0){
        return Hayford;
    }else if(strcasecmp(elipsoide, "Helmert1906")== 0){
        return Helmert1906;
    }else if(strcasecmp(elipsoide, "Hough1960")== 0){
        return Hough1960;
    }else if(strcasecmp(elipsoide, "Krasovsky")== 0){
        return Krasovsky;
    }else if(strcasecmp(elipsoide, "Mercury1960")== 0){
        return Mercury1960;
    }else if(strcasecmp(elipsoide, "Mercury1968")== 0){
        return Mercury1968;
    }else if(strcasecmp(elipsoide, "Internacional1967")== 0){
        return Internacional1967;
    }else if(strcasecmp(elipsoide, "Sudamericano") == 0){
        return Sudamericano;
    }else if(strcasecmp(elipsoide, "Walbeck")== 0){
        return Walbeck;
    }else if(strcasecmp(elipsoide, "WGS66")== 0){
        return WGS66;
    }else if(strcasecmp(elipsoide, "WGS72")== 0){
        return WGS72;
    }

    return WGS84;
}

#endif
