/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             {MARTE_VERSION}
 *
 *                            H e l l o    W o r l d
 *
 *                                    CPP
 *
 * File 'hello_world_cpp.cpp'                                   By Sangorrin.
 *
 *
 * A simple "hello world" program for C++.
 *
 *    $ mgcc hello_world_cpp.cpp -L$(LIB_CPP_PATH) (for C++ application)
 *            LIB_CPP_PATH is the path for libstc++.a
 *            (find /usr/lib -name libstdc++.a)
 *
 *---------------------------------------------------------------------------*/

#include <stdio.h>

class CRectangle
{
        int x, y;
        public:
                void set_values (int,int);
                int area () {return (x*y);}
};

void CRectangle::set_values (int a, int b) {
        x = a;
        y = b;
}

int main () {
        CRectangle rect;
        rect.set_values (3,4);
        printf("hello world in c++\n");
        printf("A 3x4 rectangle has an area = %d\n", rect.area());
        return 0;
}
