/* packet-rtep.c
 * Routines for Real-Time Ethernet Protocol dissection
 * Copyright 200x, Daniel Sangorrin <daniel.sangorrin@gmail.com>
 *
 * $Id: README.developer 19855 2006-11-07 09:12:30Z guy $
 *
 * Wireshark - Network traffic analyzer
 * By Gerald Combs <gerald@wireshark.org>
 * Copyright 1998 Gerald Combs
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#ifndef ETHERTYPE_RTEP
#define ETHERTYPE_RTEP 0xA000 /* RT-EP: Real-Time Ethernet Protocol */
#endif

#include <epan/packet.h>
#include <epan/prefs.h>

/* IF PROTO exposes code to other dissectors, then it must be exported
   in a header file. If not, a header file is not needed at all.
#include "packet-rtep.h" */

/* Forward declaration we need below */
void proto_reg_handoff_rtep(void);

/* Initialize the protocol and registered fields */
static int proto_rtep = -1;
static int hf_rtep_dest_address = -1;
static int hf_rtep_type = -1;
static int hf_rtep_prio = -1;
static int hf_rtep_packet_number = -1;
static int hf_rtep_master_id = -1;
static int hf_rtep_failure_station = -1;
static int hf_rtep_failure_station_id = -1;
static int hf_rtep_highest_prio_station = -1;
static int hf_rtep_multicast_type = -1;
static int hf_rtep_mutex_id = -1;
static int hf_rtep_mutex_holder = -1;
static int hf_rtep_channel_id = -1;
static int hf_rtep_info_length = -1;
static int hf_rtep_info = -1;

/* Global sample preference ("controls" display of numbers) */
/* static gboolean gPREF_HEX = FALSE; */

/* Initialize the subtree pointers */
static gint ett_rtep = -1;

/*
 * To understand how the protocol is dissected consult the doc/ directory
 * in the RT-EP sources.
 *
 *	http://www.ctr.unican.es/publications/jmm-mgh-2005a.html
 */

#define RTEP_TYPE_INFO                  0x49  /* 'I'=0x49 */
#define RTEP_TYPE_REGULAR_TOKEN         0x54  /* 'T'=0x54 */
#define RTEP_TYPE_TRANSMIT_TOKEN        0x50  /* 'P'=0x50 */
#define RTEP_TYPE_ACK                   0x41  /* 'A'=0x41 */
#define RTEP_TYPE_REQUEST               0x52  /* 'R'=0x52 */

#define RTEP_MULTICAST_TYPE_NO_MULTICAST    0x00
#define RTEP_MULTICAST_TYPE_MULTICAST_MSG   0x01
#define RTEP_MULTICAST_TYPE_LOCK_MUTEX      0x02
#define RTEP_MULTICAST_TYPE_UNLOCK_MUTEX    0x03

static const value_string rtep_type_vals[] = {
  { RTEP_TYPE_INFO, "INFO" },
  { RTEP_TYPE_REGULAR_TOKEN, "REGULAR_TOKEN" },
  { RTEP_TYPE_TRANSMIT_TOKEN, "TX_GRANT_TOKEN" },
  { RTEP_TYPE_ACK, "INIT_ACK" },
  { RTEP_TYPE_REQUEST, "INIT_REQUEST" },
  { 0, NULL }
};

/* Code to actually dissect the packets */
static void
dissect_rtep(tvbuff_t *tvb, packet_info *pinfo, proto_tree *tree)
{
    guint offset = 0;
    guint8 type, multicast_type;
    guint16 len, dest;

    /* Set up structures needed to add the protocol subtree and manage it */
    proto_item *ti;
    proto_tree *rtep_tree;

    /* Make entries in Protocol column and Info column on summary display */
    if (check_col(pinfo->cinfo, COL_PROTOCOL))
        col_set_str(pinfo->cinfo, COL_PROTOCOL, "rtep");

/* This field shows up as the "Info" column in the display; you should use
   it, if possible, to summarize what's in the packet, so that a user looking
   at the list of packets can tell what type of packet it is. See section 1.5
   for more information.

   Before changing the contents of a column you should make sure the column is
   active by calling "check_col(pinfo->cinfo, COL_*)". If it is not active
   don't bother setting it.

   If you are setting the column to a constant string, use "col_set_str()",
   as it's more efficient than the other "col_set_XXX()" calls.

   If you're setting it to a string you've constructed, or will be
   appending to the column later, use "col_add_str()".

   "col_add_fstr()" can be used instead of "col_add_str()"; it takes
   "printf()"-like arguments.  Don't use "col_add_fstr()" with a format
   string of "%s" - just use "col_add_str()" or "col_set_str()", as it's
   more efficient than "col_add_fstr()".

   If you will be fetching any data from the packet before filling in
   the Info column, clear that column first, in case the calls to fetch
   data from the packet throw an exception because they're fetching data
   past the end of the packet, so that the Info column doesn't have data
   left over from the previous dissector; do

    if (check_col(pinfo->cinfo, COL_INFO))
        col_clear(pinfo->cinfo, COL_INFO);

   */

  type = tvb_get_guint8(tvb, 2);

/* A protocol dissector can be called in 2 different ways:

    (a) Operational dissection

        In this mode, Wireshark is only interested in the way protocols
        interact, protocol conversations are created, packets are reassembled
        and handed over to higher-level protocol dissectors.
        In this mode Wireshark does not build a so-called "protocol tree".

    (b) Detailed dissection

        In this mode, Wireshark is also interested in all details of a given
        protocol, so a "protocol tree" is created.

   Wireshark distinguishes between the 2 modes with the proto_tree pointer:
    (a) <=> tree == NULL
    (b) <=> tree != NULL

   In the interest of speed, if "tree" is NULL, avoid building a
   protocol tree and adding stuff to it, or even looking at any packet
   data needed only if you're building the protocol tree, if possible.

   Note, however, that you must fill in column information, create
   conversations, reassemble packets, build any other persistent state
   needed for dissection, and call subdissectors regardless of whether
   "tree" is NULL or not.  This might be inconvenient to do without
   doing most of the dissection work; the routines for adding items to
   the protocol tree can be passed a null protocol tree pointer, in
   which case they'll return a null item pointer, and
   "proto_item_add_subtree()" returns a null tree pointer if passed a
   null item pointer, so, if you're careful not to dereference any null
   tree or item pointers, you can accomplish this by doing all the
   dissection work.  This might not be as efficient as skipping that
   work if you're not building a protocol tree, but if the code would
   have a lot of tests whether "tree" is null if you skipped that work,
   you might still be better off just doing all that work regardless of
   whether "tree" is null or not. */
    if (tree) {

/* NOTE: The offset and length values in the call to
   "proto_tree_add_item()" define what data bytes to highlight in the hex
   display window when the line in the protocol tree display
   corresponding to that item is selected.

   Supplying a length of -1 is the way to highlight all data from the
   offset to the end of the packet. */

        /* create display subtree for the protocol */
        ti = proto_tree_add_item(tree, proto_rtep, tvb, 0, -1, FALSE);
        rtep_tree = proto_item_add_subtree(ti, ett_rtep);

        /* add an item to the subtree, see section 1.6 for more information */
        dest = tvb_get_letohs(tvb, (gint) offset);
        proto_tree_add_item(rtep_tree, hf_rtep_dest_address, tvb, offset, 2, FALSE);
        offset += 2;
        proto_tree_add_item(rtep_tree, hf_rtep_type, tvb, offset, 1, FALSE);
        offset += 1;
    switch (type) {
      case RTEP_TYPE_INFO:
        if (check_col(pinfo->cinfo, COL_INFO))
          col_add_fstr(pinfo->cinfo, COL_INFO, "RTEP_INFO_PACKET Dest: %u ", dest);
        proto_tree_add_item(rtep_tree, hf_rtep_prio, tvb, offset, 1, FALSE);
        offset += 1;
        proto_tree_add_item(rtep_tree, hf_rtep_packet_number, tvb, offset, 2, FALSE);
        offset += 2;
        proto_tree_add_item(rtep_tree, hf_rtep_channel_id, tvb, offset, 2, FALSE);
        offset += 2;
        len = tvb_get_letohs(tvb, (gint) offset);
        proto_tree_add_item(rtep_tree, hf_rtep_info_length, tvb, offset, 2, FALSE);
        offset += 2;
        proto_tree_add_item(rtep_tree, hf_rtep_info, tvb, offset, len, FALSE);
        offset += len;
        break;
      case RTEP_TYPE_ACK:
        if (check_col(pinfo->cinfo, COL_INFO))
          col_add_fstr(pinfo->cinfo, COL_INFO, "RTEP_INIT_ACK Dest: %u ", dest);
        break;
      case RTEP_TYPE_REQUEST:
        if (check_col(pinfo->cinfo, COL_INFO))
          col_add_fstr(pinfo->cinfo, COL_INFO, "RTEP_INIT_REQUEST Dest: %u ", dest);
        break;
      case RTEP_TYPE_TRANSMIT_TOKEN:
      case RTEP_TYPE_REGULAR_TOKEN:
        proto_tree_add_item(rtep_tree, hf_rtep_prio, tvb, offset, 1, FALSE);
        offset += 1;
        proto_tree_add_item(rtep_tree, hf_rtep_packet_number, tvb, offset, 2, FALSE);
        offset += 2;
        proto_tree_add_item(rtep_tree, hf_rtep_master_id, tvb, offset, 2, FALSE);
        offset += 2;
        proto_tree_add_item(rtep_tree, hf_rtep_highest_prio_station, tvb, offset, 2, FALSE);
        offset += 2;
        proto_tree_add_item(rtep_tree, hf_rtep_failure_station_id, tvb, offset, 2, FALSE);
        offset += 2;
        proto_tree_add_item(rtep_tree, hf_rtep_failure_station, tvb, offset, 1, FALSE);
        offset += 1;

        if (type == RTEP_TYPE_TRANSMIT_TOKEN) {
            if (check_col(pinfo->cinfo, COL_INFO))
                col_add_fstr(pinfo->cinfo, COL_INFO, "RTEP_TRANSMIT_TOKEN Dest: %u ", dest);
            break;
        }

        if (check_col(pinfo->cinfo, COL_INFO))
            col_add_fstr(pinfo->cinfo, COL_INFO, "RTEP_REGULAR_TOKEN Dest: %u ", dest);

        multicast_type = tvb_get_guint8(tvb, offset);
        proto_tree_add_item(rtep_tree, hf_rtep_multicast_type, tvb, offset, 1, FALSE);
        offset += 1;
        switch (multicast_type) {
          case RTEP_MULTICAST_TYPE_NO_MULTICAST:
            if (check_col(pinfo->cinfo, COL_INFO))
              col_append_fstr(pinfo->cinfo, COL_INFO, "No Multicast Message ");
            break;
          case RTEP_MULTICAST_TYPE_MULTICAST_MSG:
            if (check_col(pinfo->cinfo, COL_INFO))
              col_append_fstr(pinfo->cinfo, COL_INFO, "Multicast message ");
            break;
          case RTEP_MULTICAST_TYPE_LOCK_MUTEX:
            if (check_col(pinfo->cinfo, COL_INFO))
              col_append_fstr(pinfo->cinfo, COL_INFO, "Lock mutex ");
              proto_tree_add_item(rtep_tree, hf_rtep_mutex_id, tvb, offset, 2, FALSE);
              offset += 2;
              proto_tree_add_item(rtep_tree, hf_rtep_mutex_holder, tvb, offset, 2, FALSE);
              offset += 2;
              break;
          case RTEP_MULTICAST_TYPE_UNLOCK_MUTEX:
              if (check_col(pinfo->cinfo, COL_INFO))
                      col_append_fstr(pinfo->cinfo, COL_INFO, "Unlock mutex ");
              proto_tree_add_item(rtep_tree, hf_rtep_mutex_id, tvb, offset, 2, FALSE);
              offset += 2;
              break;
        }
    }

    /* Continue adding tree items to process the packet here */

  } /* end if (tree) */

/* If this protocol has a sub-dissector call it here, see section 1.8 */
}


/* Register the protocol with Wireshark */

/* this format is require because a script is used to build the C function
   that calls all the protocol registration.
*/

void
proto_register_rtep(void)
{
/*  module_t *rtep_module; */

/* Setup list of header fields  See Section 1.6.1 for details*/
    static hf_register_info hf[] = {
        { &hf_rtep_dest_address,
            { "Dest_Station", "rtep.dest",
            FT_UINT16, BASE_HEX, NULL, 0x0,
            "Destination Station in the Ring", HFILL }
        },
        { &hf_rtep_type,
            { "Type", "rtep.type",
            FT_UINT8, BASE_HEX, VALS(rtep_type_vals), 0x0,
            "", HFILL }
        },
        { &hf_rtep_prio,
            { "Prio", "rtep.prio",
            FT_UINT8, BASE_HEX, NULL, 0x0,
            "Priority of the message", HFILL }
        },
        { &hf_rtep_packet_number,
            { "Packet Number", "rtep.packet_number",
            FT_UINT16, BASE_HEX, NULL, 0x0,
            "For replicated packets", HFILL }
        },
        { &hf_rtep_master_id,
            { "Master_ID", "rtep.token_master_id",
            FT_UINT16, BASE_HEX, NULL, 0x0,
            "", HFILL }
        },
        { &hf_rtep_highest_prio_station,
        { "Highest_prio_station", "rtep.highest_prio_station",
        FT_UINT16, BASE_HEX, NULL, 0x0,
        "", HFILL }
        },
        { &hf_rtep_failure_station_id,
            { "Failure_Station_Id", "rtep.failure_station_id",
            FT_UINT16, BASE_HEX, NULL, 0x0,
            "Failure Station ID", HFILL }
        },
        { &hf_rtep_failure_station,
        { "Failure_Station", "rtep.failure_station",
        FT_UINT8, BASE_HEX, NULL, 0x0,
        "Is there a failure station?", HFILL }
        },
    { &hf_rtep_multicast_type,
      { "Multicast_Type", "rtep.multicast_type",
      FT_UINT8, BASE_HEX, NULL, 0x0,
      "Type of Multicast message", HFILL }
    },
    { &hf_rtep_mutex_id,
      { "Mutex_ID", "rtep.mutex_id",
      FT_UINT16, BASE_HEX, NULL, 0x0,
      "Mutex Id", HFILL }
    },
    { &hf_rtep_mutex_holder,
    { "Mutex_Holder", "rtep.mutex_holder",
    FT_UINT16, BASE_HEX, NULL, 0x0,
    "Mutex Holder", HFILL }
    },
        { &hf_rtep_channel_id,
            { "Channel_Id", "rtep.channel_id",
            FT_UINT16, BASE_HEX, NULL, 0x0,
            "", HFILL }
        },
        { &hf_rtep_info_length,
            { "Info_Length", "rtep.info_length",
            FT_UINT16, BASE_HEX, NULL, 0x0,
            "", HFILL }
        },
        { &hf_rtep_info,
            { "Info", "rtep.info",
            FT_BYTES, BASE_HEX, NULL, 0x0,
            "", HFILL }
        }
    };

/* Setup protocol subtree array */
    static gint *ett[] = {
        &ett_rtep
    };

/* Register the protocol name and description */
    proto_rtep = proto_register_protocol("Real-Time Ethernet Protocol", "RT-EP", "rtep");

/* Required function calls to register the header fields and subtrees used */
    proto_register_field_array(proto_rtep, hf, array_length(hf));
    proto_register_subtree_array(ett, array_length(ett));

/* Register preferences module (See Section 2.6 for more on preferences) */
/*  rtep_module = prefs_register_protocol(proto_rtep, proto_reg_handoff_rtep);*/

/* Register a sample preference */
/*  prefs_register_bool_preference(rtep_module, "showHex",
         "Display numbers in Hex",
         "Enable to display numerical values in hexadecimal.",
         &gPREF_HEX);*/
}


/* If this dissector uses sub-dissector registration add a registration routine.
   This exact format is required because a script is used to find these routines
   and create the code that calls these routines.

   This function is also called by preferences whenever "Apply" is pressed
   (see prefs_register_protocol above) so it should accommodate being called
   more than once.
*/
void
proto_reg_handoff_rtep(void)
{
    static gboolean inited = FALSE;

    if (!inited) {

        dissector_handle_t rtep_handle;

        rtep_handle = create_dissector_handle(dissect_rtep, proto_rtep);
        dissector_add("ethertype", ETHERTYPE_RTEP, rtep_handle);

        inited = TRUE;
    }
}
