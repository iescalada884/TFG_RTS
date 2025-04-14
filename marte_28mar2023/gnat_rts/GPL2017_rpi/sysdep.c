#include "tsystem.h"

#define FILE void

void
__gnat_set_binary_mode (int handle)
{
}

void
__gnat_set_text_mode (int handle)
{
}


void
__gnat_set_mode (int handle, int mode)
{
}

void getc_immediate (FILE *stream, int *ch, int *end_of_file) {
  *end_of_file=1;
  *ch = 0;
}

void getc_immediate_nowait (FILE *stream, int *ch,
			    int *end_of_file, int *avail) {
  *end_of_file = 0;
  *avail = 0;
  *ch = 0;
}

int
__gnat_is_file_not_found_error (int errno_val) {
  return 1;
}

const char __gnat_text_translation_required = 0;


#define time_t void
void
__gnat_localtime_tzoff (const time_t *timer ATTRIBUTE_UNUSED,
			const int *is_historic ATTRIBUTE_UNUSED,
			long *off ATTRIBUTE_UNUSED)
{
  *off = 0;
}
