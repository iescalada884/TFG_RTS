//  Test for all architectures
/*!
 * @file test_dynamic_cast.c
 *
 * @brief Test for a dynamic_cast
 *
 * @version 0.01
 *
 * @date 31-Oct-2008
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * Test for dynamic_cast
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

using namespace std;

class Base {
    public:
        virtual ~Base() {}
        virtual void f() {
            printf("Inside Base\n");
        }

};

class Derived : public Base {
    public:
        virtual ~Derived() {}
        void f() {
            printf("Inside Derived\n");
        }
};

int main()
{
    Base *bp, baseObject;
    Derived *dp, d_ob;

#if MARTE_ARCHITECTURE == ARCH_X86
    SERIAL_CONSOLE_INIT();
#endif

    dp = dynamic_cast<Derived *> (&d_ob);
    if(dp) {
        printf("Cast from Derived * to Derived * OK.\n");
        dp->f();
    } else {
        printf("Error\n");
        exit(-1);
    }

    printf("\n");

    bp = dynamic_cast<Base *> (&d_ob);
    if(bp) {
        printf("Cast from Derived * to Base * OK.\n");
        bp->f();
    } else {
        printf("Error\n");
        exit(-1);
    }
    printf("\n");

    bp = dynamic_cast<Base *> (&baseObject);
    if(bp) {
        printf("Cast from Base * to Base * OK.\n");
        bp->f();
    } else {
        printf("Error\n");
        exit(-1);
    }

    printf("\n");

    bp = &d_ob;                           // bp points to Derived object
    dp = dynamic_cast<Derived *> (bp);

    if(dp) {
        printf("Casting bp to a Derived * OK\n");
        printf("because bp is really pointing\n");
        printf("to a Derived object.\n");
        dp->f();
    } else {
        printf("Error\n");
        exit(-1);
    }

    printf("\n");

    bp = &baseObject;                          // bp points to Base object
    dp = dynamic_cast<Derived *> (bp);
    if(dp){
        printf("Error\n");
        exit(-1);
    }else {
        printf("Now casting bp to a Derived *\n");
        printf("is not OK because bp is really \n");
        printf("pointing to a Base object.\n");
    }

    printf("\n");

    dp = &d_ob;                         // dp points to Derived object
    bp = dynamic_cast<Base *> (dp);
    if(bp) {
        printf("Casting dp to a Base * is OK.\n");
        bp->f();
    } else{
        printf("Error\n");
        exit(-1);
    }

    printf("Test OK\n");
    return 0;
}
