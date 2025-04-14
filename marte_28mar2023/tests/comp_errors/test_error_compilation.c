//  Test for all architectures
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

int main()
{
        int ret  //  <---- error: ";" expected

        char s1[] = "my name is friday";
        char s2[] = "oh my god";
        char s3[] = "my name is friday";

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        printf("test for strncmp\n");

        ret = strncmp(s1, s2, sizeof(s1));
        printf("s1 vs s2: %d\n", ret);

        ret = strncmp(s1, s3, sizeof(s1));
        printf("s1 vs s3: %d\n", ret);

        ret = strncmp(s2, s3, sizeof(s1));
        printf("s2 vs s3: %d\n", ret);

        printf("Test OK");
        return 0;
}
