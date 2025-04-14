/*!
 * @file freelist.h
 *
 * @brief Freelist for managing free/used resources
 *
 * @version 0.01
 *
 * @date 26-Sept-2007
 *
 * @author
 *      Michael Gonzalez Harbour <mgh@unican.es>
 *      Michal Sojka <sojkam1@fel.cvut.cz>
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
 * @license
 *
 * See MaRTE OS License
 *
 */

#ifndef FREELIST_H
#define FREELIST_H

typedef struct {
        int *freelist;
        int first_free;
} freelist_t;

/**
 *  Initialize a free list adding all the cells to the list of free cells
 *  The size of the list is specified by size. Returns 0 if successful, or
 *  -1 if not enough memory
 */
extern int freelist_init(freelist_t *list, int size);

/**
 *  Obtain the index of a free cell from the free list specified by list
 *  the cell is removed from the list
 *  Returns the index to the requested cell, or -1 if there are no free cells
 */
extern int freelist_alloc(freelist_t *list);

/**
 * Deallocate the cell specified by index cell adding it to the list
 * of free cells specified by list
 * Returns 0 if successful, or -1 if the cell was already free
 */
extern int freelist_free(freelist_t *list, int cell);

#endif // FREELIST_H
