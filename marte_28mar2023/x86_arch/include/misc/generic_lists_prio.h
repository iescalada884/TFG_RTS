/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                   'g e n e r i c _ l i s t s _ p r i o'
 *
 *                                      H
 *
 * File 'generic_lists_prio.h'                                         by MAR.
 *
 * To be included after 'generic_lists.h'.
 *
 * "Generic" single linked lists ordered by priority. They can be used
 * with any struct type like this:
 *
 * struct any_type {
 *     struct any_type * next;
 *     int prio;
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

#ifndef	_MARTE_MISC_GENERIC_LISTS_PRIO_H_
#define _MARTE_MISC_GENERIC_LISTS_PRIO_H_
typedef struct list_ele_prio  list_ele_prio_t;

struct list_ele_prio {
  list_ele_prio_t *next;
  int prio;
};

#define prio_ele ((list_ele_prio_t *)ele)
#define prio_list ((list_ele_prio_t *)list)

/*
 * enqueue_in_prio_order
 */
void enqueue_in_prio_order (void *ele, list_t *list)
{
  list_ele_prio_t * p =  prio_list;

  if (!prio_list || prio_ele->prio > prio_list->prio) {
    // Enqueue in the head
    prio_ele->next = prio_list;
    prio_list = prio_ele;
  } else {
    while (p->next && prio_ele->prio < p->next->prio) {
      p = p->next;
    }
    prio_ele->next = p->next;
    p->next = prio_ele;
  }
}

/*
 * find_ele_mx_prio
 */
void * find_ele_mx_prio (list_t *list)
{
  int tmp_prio = 0;
  list_ele_prio_t * p =  prio_list;
  list_ele_prio_t * ele_mx_prio = NULL;

  while (p) {
    if (p->prio > tmp_prio) {
      tmp_prio = p->prio;
      ele_mx_prio = p;
    }
    p = p->next;
  }

  return ele_mx_prio;
}


#endif /* _MARTE_MISC_GENERIC_LISTS_PRIO_H_ */
