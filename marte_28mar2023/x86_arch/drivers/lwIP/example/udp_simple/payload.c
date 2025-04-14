//      payload.c
//      
//      Copyright 2010 Hector Perez <perezh@unican.es>
//      
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
//      (at your option) any later version.
//      
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//      
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.

#ifndef PAYLOAD_H
#define PAYLOAD_H

#include <error_checks.h>

#define ENABLE_DEBUG 0

char* create_char_buffer (long data_size) {
	
	char* buffer = NULL;
	int i = 0;
	
	buffer = (char *) calloc (data_size, sizeof(char));
	/* Memory could not be allocated */
	ASSERT (buffer != NULL, "Memory could not be allocated for char buffer");
	
	/* Fill with some interesting data */
		
    for (i = 0; i < data_size-1; ++i) { 
        buffer[i] = ('A' + i%26);
    }
    buffer[i] = '\0'; /* Make a valid C string */

    if (ENABLE_DEBUG) {
		printf ("%s\n", buffer);
	}
    
    return &buffer[0];
}

void release_char_buffer (char* buffer) {
 
  //  Free memory                                                    
  free(buffer);
  buffer = NULL;  
}
#endif
