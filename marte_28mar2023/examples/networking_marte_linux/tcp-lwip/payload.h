/*
 *      payload.h
 *      
 *      Copyright 2010 Hector Perez <perezh@unican.es>
 *      
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *      
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *      
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *      MA 02110-1301, USA.
 */

// 
// name: create_char_buffer. Creates a buffer[data_size] and fill it 
//       with interesting stuff
// @param: data_size Size of the buffer
// @return: the buffer
char* create_char_buffer (long data_size);

// 
// name: create_char_buffer. Creates a buffer[data_size] and fill it 
//       with interesting stuff
// @param: data_size Size of the buffer
// @return: the buffer
void release_char_buffer (char* buffer);
