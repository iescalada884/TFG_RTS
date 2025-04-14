#ifndef __UTMCPP_H_
#define __UTMCPP_H_

#include <stdlib.h>
#include <math.h>

#include "elipsoides.h"

/*#define M_PI 3.14159265358979323846264338327950288419716939937510
#define M_E  2.71828182845904523536028747135266249775724709369996*/


typedef enum Hemisferio {N=1, S}Hemisferio;

typedef struct GeoUTM
{
//private:
   // Caracteristicas del elipsoide
    double a, b, c;
    double ep, ep2;
    // Coordenadas UTM
//public:
//OJO!!!

//private:
    Hemisferio h;
    int huso;
    double x, y;

    // Coordenada geograficas
    double lat, lon;
    double latRad, lonRad;

    // Variables para calculos
    double merCentral, distAngular;

}GeoUTM;

        /* Ecuaciones de Coticchia - Surace*/
        void CS(GeoUTM *geo);

//public:

    // Inicializacion del elipsoide, por defecto WGS84
    //void initGeoUTM(Elipsoide e = WGS84); // Constructor por defecto e = WGS84
    void initGeoUTM(GeoUTM *geo, Elipsoide e);
    void initGeoUTM2(GeoUTM *geo, double semiejeMayor, double semiejeMenor);
    // Constructor general

    // Funciones de establecimiento y obtencion de parametros
    //void setElipsoide(Elipsoide e);
    void setElipsoide(GeoUTM *geo,Elipsoide e);
    void setElipsoideSemiejes(GeoUTM *geo,double semiejeMayor,
                              double semiejeMenor);

    void setGeograficas(GeoUTM *geo, double longitud, double latitud){
        geo->lon = longitud;
        geo->lat = latitud;
    }

    void getGeograficas(GeoUTM *geo, double longitud, double latitud){
        longitud = geo->lon;
        latitud = geo->lat;
    }

    void setUTM(GeoUTM *geo,double utmX, double utmY, int utmHuso,
                Hemisferio hemisferio){
        geo->x = utmX;
        geo->y = utmY;
        geo->huso = utmHuso;
        geo->merCentral = ((geo->huso * 6 - 183) * M_PI) / 180.0;
        geo->h = hemisferio;
    }

        void getUTM(GeoUTM *geo,double *utmX, double *utmY, int utmHuso,
                    Hemisferio hemisferio){
        utmX = &(geo->x);
        utmY = &(geo->y);
        utmHuso = geo->huso;
        hemisferio = geo->h;
    }

    void setHuso(GeoUTM *geo,int utmHuso){
        geo->huso = utmHuso;
        geo->merCentral = ((geo->huso * 6 - 183) * M_PI) / 180.0;
    }

    int getHuso(GeoUTM *geo){
        return geo->huso;
    }

    // Paso de geograficas a UTM
        void geo2UTM(GeoUTM *geo);
        void geo2UTM_Huso(GeoUTM *geo);

    // Paso de UTM a geograficas
    void UTM2geo(GeoUTM *geo);


#endif
