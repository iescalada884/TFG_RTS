/*------------------------------------------------------------------------------
 *-------------------------        ATH5K Driver          -----------------------
 *------------------------------------------------------------------------------
 *                                                           V1.0  08/02/2010
 *
 *
 *  Feb 2010 - Samuel Cabrero <samuelcabrero@gmail.com>
 *		Initial release
 *
 *  ----------------------------------------------------------------------------
 *  Copyright (C) 2000-2010, Universidad de Zaragoza, SPAIN
 *
 *  Authors:
 *		Samuel Cabrero        <samuelcabrero@gmail.com>
 *		Danilo Tardioli	      <dantard@unizar.es>
 *		Jose Luis Villarroel  <jlvilla@unizar.es>
 *
 *  This is a simplified version of the original ath5k driver. It should work 
 *  with all Atheros 5xxx WLAN cards. The 802.11 layer have been removed so it
 *  just send and receive frames over the air, as if it were an Ethernet bus
 *  interface.
 *
 *  Please read ath5k_interface.h for instructions.
 *
 *  This program is distributed under the terms of GPL version 2 and in the 
 *  hope that it will be useful, but WITHOUT ANY WARRANTY; without even the 
 *  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *  See the GNU General Public License for more details.
 *
 *----------------------------------------------------------------------------*/

/*-
 * Copyright (c) 2002-2007 Sam Leffler, Errno Consulting
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer,
 *    without modification.
 * 2. Redistributions in binary form must reproduce at minimum a disclaimer
 *    similar to the "NO WARRANTY" disclaimer below ("Disclaimer") and any
 *    redistribution must be conditioned upon including a substantially
 *    similar Disclaimer requirement for further binary redistribution.
 * 3. Neither the names of the above-listed copyright holders nor the names
 *    of any contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * Alternatively, this software may be distributed under the terms of the
 * GNU General Public License ("GPL") version 2 as published by the Free
 * Software Foundation.
 *
 * NO WARRANTY
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF NONINFRINGEMENT, MERCHANTIBILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 * THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGES.
 *
 */

/*
 * Defintions for the Atheros Wireless LAN controller driver.
 */
#ifndef _DEV_ATH_ATHVAR_H
#define _DEV_ATH_ATHVAR_H

#include <misc/linux_list.h>
#include "ath5k.h"
#include "debug.h"
#include "wifi_rx_queue.h"
#include "ath5k_interface.h"

#define	ATH_RXBUF	40		/* number of RX buffers */
#define	ATH_TXBUF	200		/* number of TX buffers */

struct ath5k_buf {
	struct list_head	list;
	struct ath5k_desc	*desc;	/* virtual addr of desc */
	struct sk_buff		*skb;	/* skbuff for buf */
};

/*
 * Data transmit queue state.  One of these exists for each
 * hardware transmit queue.  Packets sent to us from above
 * are assigned to queues based on their priority.  Not all
 * devices support a complete set of hardware transmit queues.
 * For those devices the array sc_ac2q will map multiple
 * priorities to fewer hardware queues (typically all to one
 * hardware queue).
 */
struct ath5k_txq {
	unsigned int		qnum;	/* hardware q number */
	u32			*link;	/* link ptr in last TX desc */
	struct list_head	q;	/* transmit queue */
	bool			setup;
};

#define ATH_CHAN_MAX	(26+26+26+200+200)

/* Software Carrier, keeps track of the driver state
 * associated with an instance of a device */
struct ath5k_softc {
	struct pci_device	*pdev;		/* for dma mapping */
	void				*iobase;	/* address of the device */
	
	struct ieee80211_low_level_stats ll_stats;
	struct ieee80211_supported_band sbands[IEEE80211_NUM_BANDS];
	struct ieee80211_channel channels[ATH_CHAN_MAX];
	struct ieee80211_rate	rates[IEEE80211_NUM_BANDS][AR5K_MAX_RATES];
	struct ath5k_hw		*ah;		/* Atheros HW */

	struct ieee80211_supported_band		*curband;

	unsigned int	debug_level;		/* debug info */

	struct ath5k_buf	*bufptr;	/* allocated buffer ptr */
	struct ath5k_desc	*desc;		/* TX/RX descriptors */
	size_t				desc_len;	/* size of TX/RX descriptors */
	u16			cachelsz;	/* cache line size */

	DECLARE_BITMAP(status, 5);
#define ATH_STAT_INVALID	0		/* disable hardware accesses */
#define ATH_STAT_MRRETRY	1		/* multi-rate retry support */
#define ATH_STAT_PROMISC	2
#define ATH_STAT_LEDSOFT	3		/* enable LED gpio status */
#define ATH_STAT_STARTED	4		/* opened & irqs enabled */

	unsigned int		filter_flags;	/* HW flags, AR5K_RX_FILTER_* */
	struct ieee80211_channel *curchan;	/* current h/w channel */

	enum ath5k_int		imask;		/* interrupt mask copy */

	unsigned char		bssidmask[ETH_ALEN];

	unsigned int		rxbufsize;	/* rx size based on mtu */
	struct list_head	rxbuf;		/* receive buffer */
	unsigned int		*rxlink;	/* link ptr in last RX desc */
	wifi_ring_t			rxring;

	struct list_head	txbuf;		/* transmit buffer */
	unsigned int		txbuf_len;	/* buf count in txbuf list */
	struct ath5k_txq	txqs[1];	/* beacon and tx */
	struct ath5k_txq	*txq;		/* beacon and tx*/
	struct ieee80211_tx_info tx_info;
	sem_t				tx_sem;		/* tx semaphore */

	int 			power_level;	/* Requested tx power in dbm */
	bool			assoc;		/* assocate state */
};

#define ath5k_hw_hasbssidmask(_ah) \
	(ath5k_hw_get_capability(_ah, AR5K_CAP_BSSIDMASK, 0, NULL) == 0)
#define ath5k_hw_hasveol(_ah) \
	(ath5k_hw_get_capability(_ah, AR5K_CAP_VEOL, 0, NULL) == 0)

#endif
