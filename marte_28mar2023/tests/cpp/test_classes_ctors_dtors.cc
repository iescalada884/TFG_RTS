//  Test for all architectures
/*!
 * @file test_classes_ctors_dtors.c
 *
 * @brief Test for constructors and destructors of global classes
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
 * Test for constructors and destructors of classes in C++. This constructors
 * are called with the function init_lang_support() just before calling the
 * main. Destructors should be called in reverse order.
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <stdio.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

class A {
    protected:
        int a;
    public:
        virtual void print()=0;
        A() { a = 1; printf("initializing A\n");}
        virtual ~A() { printf("uninitializing A\n"); }
};

class B : public A
{
    public:
        B() { a = 1; printf("initializing B\n");}
        ~B() { printf("uninitializing B\n");}
        virtual void print() { printf("B::print: %d\n", a); }
};

B a, c;
A *ptr_b;

int main () {
    ptr_b = new B;
    ptr_b -> print();
    delete ptr_b;

#if MARTE_ARCHITECTURE == ARCH_X86
    SERIAL_CONSOLE_INIT();
#endif

    printf("Test OK\n");
    return 0;
}
