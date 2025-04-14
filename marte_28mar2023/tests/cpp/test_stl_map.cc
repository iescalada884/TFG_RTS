//  Test for all architectures
/*!
 * @file test_stl_map.cc
 *
 * @brief Test for the map container in STL
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
 * Test for the map container in STL TODO: put assertions for the results
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_LINUX_LIB

#include <string.h>
#include <iostream>
#include <map>
#include <utility>
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
    map<int, string> Employees;

#if MARTE_ARCHITECTURE == ARCH_X86
    SERIAL_CONSOLE_INIT();
#endif

   // 1) Assignment using array index notation
    Employees[5234] = "Mike C.";
    Employees[3374] = "Charlie M.";
    Employees[1923] = "David D.";
    Employees[7582] = "John A.";
    Employees[5328] = "Peter Q.";

#if (MARTE_ARCHITECTURE == ARCH_LINUX_LIB)
    cout << "Employees[3374]=" << Employees[3374] << endl << endl;
    cout << "Map size: " << Employees.size() << endl;
#else
    printf("Employees[3374]=%s\n\n", (char *)Employees[3374]);
    printf("Map size: %d\n", Employees.size());
#endif

    for( map<int,string>::iterator ii=Employees.begin();
            ii!=Employees.end(); ++ii)
    {
#if (MARTE_ARCHITECTURE == ARCH_LINUX_LIB)
       cout << (*ii).first << ": " << (*ii).second << endl;
#else
       printf("%d:%s\n", (*ii).first, (char *)(*ii).second);
#endif
    }

#if (MARTE_ARCHITECTURE == ARCH_LINUX_LIB)
    cout << "Test OK" << endl;
#else
    printf("Test OK\n");
#endif

    return 0;
}
