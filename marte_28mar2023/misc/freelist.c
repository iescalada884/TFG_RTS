/*!
 * @file freelist.c
 *
 * @brief Freelist for managing free/used resources
 *
 * @version 0.01
 *
 * @date 26-Sept-2007
 *
 * @author
 *      Michael Gonzalez Harbour <mgh@unican.es>
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * This module implements a sinly linked list that is used to find free
 * cells in some external table.
 *
 * Parallel to the external table, there is a table of indexes
 * organized as a singly linked list; it is the free list. A separate
 * index, free_cell, indicates which is the first element of the sibly
 * linked list. The list is terminated with an index of -1.
 *
 *
 * @license
 *
 * See MaRTE OS License
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "freelist.h"

/**
 *  Initialize a free list adding all the cells to the list of free cells
 *  The size of the list is specified by size. Returns 0 if successful, or
 *  -1 if not enough memory
 */
int freelist_init(freelist_t *list, int size)
{
        int i;

        // create the table of the specified size
        list->freelist=malloc(sizeof(int)*((size_t) size));

        // check for possible error
        if (list->freelist==NULL) {
                return -1;
        }

        // initialize values
        list->first_free=0;
        for(i=size-2; i>=0; i--) {
                list->freelist[i]=i+1;
        }
        list->freelist[size-1]=-1; // end of the list

        return 0;
}

/**
 *  Obtain the index of a free cell from the free list specified by list
 *  the cell is removed from the list
 *  Returns the index to the requested cell, or -1 if there are no free cells
 */
int freelist_alloc(freelist_t *list)
{
        int pos;

         // check if there is space available
        if (list->first_free==-1) {
                return -1;
        }

        // obtain the first element of the list
        pos=list->first_free;

        // advace first free element to next element in list
        list->first_free=list->freelist[list->first_free];

        // set value to non free and return obtained index
        list->freelist[pos]=-1;
        return pos;
}


/**
 * Deallocate the cell specified by index cell adding it to the list
 * of free cells specified by list
 * Returns 0 if successful, or -1 if the cell was already free
 */
int freelist_free(freelist_t *list, int cell)
{
        // check the error
        if (list->freelist[cell]!=-1) {
                return -1;
        }

        // link first list element from deallocated cell
        list->freelist[cell]=list->first_free;

        // change first element of list to deallocaled cell and return
        list->first_free=cell;
        return 0;
}
