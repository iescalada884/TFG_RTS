//  Test for all architectures
/*!
 * @file test_stl_list.cc
 *
 * @brief Test for the list container in STL
 *
 * @version 0.01
 *
 * @date 1-Nov--2008
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * Test for the list container in STL TODO: put assertions for the results
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_LINUX_LIB

#include <iostream>
#include <list>
using namespace std;

#else

#include <stdio.h>
#include <ustl/ustl.h>
using namespace ustl;

#endif

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

int main()
{
    list<int> L;

#if MARTE_ARCHITECTURE == ARCH_X86
    SERIAL_CONSOLE_INIT();
#endif

    L.push_back(0);           // Insert a new element at the end
    L.push_front(0);          // Insert a new element at the beginning
    L.insert(L.begin(),2);    // Insert "2" before position of first argument
    L.push_back(5);
    L.push_back(6);

    list<int>::iterator i;

#if (MARTE_ARCHITECTURE == ARCH_LINUX_LIB)
    for(i=L.begin(); i != L.end(); ++i) cout << *i << endl;
    cout << "Test OK" << endl;
#else
    for(i=L.begin(); i != L.end(); ++i) printf("%d\n", *i);
    printf("Test OK\n");
#endif

    return 0;
}
