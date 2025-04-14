/*----------------------------------------------------------------------------
 *-- -------------------         M a R T E   O S         ------------------ --
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                 'c t y p e'
 *
 *                                      H
 *
 * File 'ctype.h'                                                      by Mar.
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

#ifndef _MARTE_CTYPE_H_
#define _MARTE_CTYPE_H_

static inline int isascii(int c)
{
        return ((c & ~0x7f) == 0);
}

static inline int iscntrl(int c)
{
        return ((c) < ' ') || ((c) > 126);
}

static inline int isdigit(int c)
{
        return ((c) >= '0') && ((c) <= '9');
}

static inline int isgraph(int c)
{
        return ((c) > ' ') && ((c) <= 126);
}

static inline int islower(int c)
{
        return (c >= 'a') && (c <= 'z');
}

static inline int isprint(int c)
{
        return ((c) >= ' ') && ((c) <= 126);
}

static inline int isspace(int c)
{
        return ((c) == ' ') || ((c) == '\f')
                || ((c) == '\n') || ((c) == '\r')
                || ((c) == '\t') || ((c) == '\v');
}

static inline int isupper(int c)
{
        return (c >= 'A') && (c <= 'Z');
}

static inline int isxdigit(int c)
{
        return isdigit(c) ||
                ((c >= 'A') && (c <= 'F')) ||
                ((c >= 'a') && (c <= 'f'));
}

static inline int isalpha(int c)
{
        return islower(c) || isupper(c);
}

static inline int isalnum(int c)
{
        return isalpha(c) || isdigit(c);
}

static inline int ispunct(int c)
{
        return isgraph(c) && !isalnum(c);
}

static inline int toascii(int c)
{
        return ((c) & 0x7f);
}

static inline int toupper(int c)
{
        return ((c >= 'a') && (c <= 'z')) ? (c - 'a' + 'A') : c;
}

static inline int tolower(int c)
{
        return ((c >= 'A') && (c <= 'Z')) ? (c - 'A' + 'a') : c;
}

#endif // _MARTE_CTYPE_H_
