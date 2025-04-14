/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                         'g e n e r i c _ l i s t s'
 *
 *                                      H
 *
 * File 'generic_lists.h'                                              by MAR.
 *
 * "Generic" single linked lists. They can be used with any struct type
 * like this:
 *
 * struct any_type {
 *     struct any_type * next;
 *     some other fields;
 * };
 *
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *  MaRTE OS web page: http://marte.unican.es
 *  Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                     Michael Gonzalez Harbour      mgh@unican.es
 *
 * MaRTE OS  is free software; you can  redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as published by the
 * Free Software Foundation;  either  version 2, or (at  your option) any
 * later version.
 *
 * MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 * WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 * MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 * General Public License for more details.
 *
 * You should have received  a  copy of  the  GNU General Public  License
 * distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 * Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 * 02111-1307, USA.
 *
 * As a  special exception, if you  link this  unit  with other  files to
 * produce an   executable,   this unit  does  not  by  itself cause  the
 * resulting executable to be covered by the  GNU General Public License.
 * This exception does  not however invalidate  any other reasons why the
 * executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/

#ifndef	_MARTE_MISC_GENERIC_LISTS_H_
#define _MARTE_MISC_GENERIC_LISTS_H_
typedef struct list_ele  list_ele_t;
typedef list_ele_t      *list_t;

struct list_ele {
  list_ele_t *next;
};

#define lele ((list_ele_t *)ele)

/*
 * enqueue_head
 */
void enqueue_head (void *ele, list_t *list)
{
  lele->next = *list;
  *list = ele;
}

/*
 * enqueue_tail
 */
void enqueue_tail (void *ele, list_t *list)
{
  list_ele_t *p = *list;

  lele->next = NULL;

  if (!p) {
    *list = lele;
    return;
  }

  while (p->next) {
    p = p->next;
  }
  p->next = lele;
}

/*
 * dequeue
 */
void dequeue (void *ele, list_t *list)
{
  list_ele_t *p = *list;

  if (!*list) {
    printf ("  ERROR: dequeuing from empty list\n");
    return;
  }
  if (*list == (list_ele_t *)ele) {
    *list = (*list)->next;
    return;
  }
  while (p->next) {
    if (p->next == lele) {
      p->next = lele->next;
      return;
    }
    p = p->next;
  }
  printf ("  ERROR: dequeue: element not in list\n");
}

/*
 * dequeue_head
 */
void * dequeue_head (list_t *list)
{
  list_ele_t *p = *list;

  if (*list) {
    *list = p->next;
  }
  return p;
}

/*
 * is_in_list
 */
int is_in_list (list_ele_t *ele, list_t list)
{
  list_ele_t *p = list;

  while (p) {
    if (p == ele) return 1; // found
    p = p->next;
  }
  return 0;
}

/*
 * Head
 */
void *head (list_t list)
{
  return list;
}

/*
 * next
 */
void * next (void *ele)
{
  return lele->next;
}
#endif /* _MARTE_MISC_GENERIC_LISTS_H_ */
