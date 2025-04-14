// MaRTE OS

#ifndef TARGET_OBJECT_SUFFIX
#define TARGET_OBJECT_SUFFIX ".o"
#endif

#ifndef TARGET_EXECUTABLE_SUFFIX
#define TARGET_EXECUTABLE_SUFFIX ""
#endif

#ifdef __cplusplus
extern "C" {
#endif

const char *__gnat_target_object_extension = TARGET_OBJECT_SUFFIX;
const char *__gnat_target_executable_extension = TARGET_EXECUTABLE_SUFFIX;
const char *__gnat_target_debuggable_extension = TARGET_EXECUTABLE_SUFFIX;

#ifdef __cplusplus
}
#endif
