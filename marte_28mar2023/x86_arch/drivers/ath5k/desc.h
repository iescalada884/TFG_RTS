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

/*
 * Copyright (c) 2004-2008 Reyk Floeter <reyk@openbsd.org>
 * Copyright (c) 2006-2008 Nick Kossifidis <mickflemm@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

/*
 * Internal RX/TX descriptor structures
 * (rX: reserved fields possibily used by future versions of the ar5k chipset)
 */

/*
 * common hardware RX control descriptor
 */
struct ath5k_hw_rx_ctl {
	u32	rx_control_0; /* RX control word 0 */
	u32	rx_control_1; /* RX control word 1 */
} __packed;

/* RX control word 0 field/sflags */
#define AR5K_DESC_RX_CTL0			0x00000000

/* RX control word 1 fields/flags */
#define AR5K_DESC_RX_CTL1_BUF_LEN		0x00000fff
#define AR5K_DESC_RX_CTL1_INTREQ		0x00002000

/*
 * common hardware RX status descriptor
 * 5210/11 and 5212 differ only in the flags defined below
 */
struct ath5k_hw_rx_status {
	u32	rx_status_0; /* RX status word 0 */
	u32	rx_status_1; /* RX status word 1 */
} __packed;

/* 5210/5211 */
/* RX status word 0 fields/flags */
#define AR5K_5210_RX_DESC_STATUS0_DATA_LEN		0x00000fff
#define AR5K_5210_RX_DESC_STATUS0_MORE			0x00001000
#define AR5K_5210_RX_DESC_STATUS0_RECEIVE_RATE		0x00078000
#define AR5K_5210_RX_DESC_STATUS0_RECEIVE_RATE_S	15
#define AR5K_5210_RX_DESC_STATUS0_RECEIVE_SIGNAL	0x07f80000
#define AR5K_5210_RX_DESC_STATUS0_RECEIVE_SIGNAL_S	19
#define AR5K_5210_RX_DESC_STATUS0_RECEIVE_ANTENNA	0x38000000
#define AR5K_5210_RX_DESC_STATUS0_RECEIVE_ANTENNA_S	27

/* RX status word 1 fields/flags */
#define AR5K_5210_RX_DESC_STATUS1_DONE			0x00000001
#define AR5K_5210_RX_DESC_STATUS1_FRAME_RECEIVE_OK	0x00000002
#define AR5K_5210_RX_DESC_STATUS1_CRC_ERROR		0x00000004
#define AR5K_5210_RX_DESC_STATUS1_FIFO_OVERRUN		0x00000008
#define AR5K_5210_RX_DESC_STATUS1_DECRYPT_CRC_ERROR	0x00000010
#define AR5K_5210_RX_DESC_STATUS1_PHY_ERROR		0x000000e0
#define AR5K_5210_RX_DESC_STATUS1_PHY_ERROR_S		5
#define AR5K_5210_RX_DESC_STATUS1_KEY_INDEX_VALID	0x00000100
#define AR5K_5210_RX_DESC_STATUS1_KEY_INDEX		0x00007e00
#define AR5K_5210_RX_DESC_STATUS1_KEY_INDEX_S		9
#define AR5K_5210_RX_DESC_STATUS1_RECEIVE_TIMESTAMP	0x0fff8000
#define AR5K_5210_RX_DESC_STATUS1_RECEIVE_TIMESTAMP_S	15
#define AR5K_5210_RX_DESC_STATUS1_KEY_CACHE_MISS	0x10000000

/* 5212 */
/* RX status word 0 fields/flags */
#define AR5K_5212_RX_DESC_STATUS0_DATA_LEN		0x00000fff
#define AR5K_5212_RX_DESC_STATUS0_MORE			0x00001000
#define AR5K_5212_RX_DESC_STATUS0_DECOMP_CRC_ERROR	0x00002000
#define AR5K_5212_RX_DESC_STATUS0_RECEIVE_RATE		0x000f8000
#define AR5K_5212_RX_DESC_STATUS0_RECEIVE_RATE_S	15
#define AR5K_5212_RX_DESC_STATUS0_RECEIVE_SIGNAL	0x0ff00000
#define AR5K_5212_RX_DESC_STATUS0_RECEIVE_SIGNAL_S	20
#define AR5K_5212_RX_DESC_STATUS0_RECEIVE_ANTENNA	0xf0000000
#define AR5K_5212_RX_DESC_STATUS0_RECEIVE_ANTENNA_S	28

/* RX status word 1 fields/flags */
#define AR5K_5212_RX_DESC_STATUS1_DONE			0x00000001
#define AR5K_5212_RX_DESC_STATUS1_FRAME_RECEIVE_OK	0x00000002
#define AR5K_5212_RX_DESC_STATUS1_CRC_ERROR		0x00000004
#define AR5K_5212_RX_DESC_STATUS1_DECRYPT_CRC_ERROR	0x00000008
#define AR5K_5212_RX_DESC_STATUS1_PHY_ERROR		0x00000010
#define AR5K_5212_RX_DESC_STATUS1_MIC_ERROR		0x00000020
#define AR5K_5212_RX_DESC_STATUS1_KEY_INDEX_VALID	0x00000100
#define AR5K_5212_RX_DESC_STATUS1_KEY_INDEX		0x0000fe00
#define AR5K_5212_RX_DESC_STATUS1_KEY_INDEX_S		9
#define AR5K_5212_RX_DESC_STATUS1_RECEIVE_TIMESTAMP	0x7fff0000
#define AR5K_5212_RX_DESC_STATUS1_RECEIVE_TIMESTAMP_S	16
#define AR5K_5212_RX_DESC_STATUS1_KEY_CACHE_MISS	0x80000000

/*
 * common hardware RX error descriptor
 */
struct ath5k_hw_rx_error {
	u32	rx_error_0; /* RX status word 0 */
	u32	rx_error_1; /* RX status word 1 */
} __packed;

/* RX error word 0 fields/flags */
#define AR5K_RX_DESC_ERROR0			0x00000000

/* RX error word 1 fields/flags */
#define AR5K_RX_DESC_ERROR1_PHY_ERROR_CODE	0x0000ff00
#define AR5K_RX_DESC_ERROR1_PHY_ERROR_CODE_S	8

/* PHY Error codes */
#define AR5K_DESC_RX_PHY_ERROR_NONE		0x00
#define AR5K_DESC_RX_PHY_ERROR_TIMING		0x20
#define AR5K_DESC_RX_PHY_ERROR_PARITY		0x40
#define AR5K_DESC_RX_PHY_ERROR_RATE		0x60
#define AR5K_DESC_RX_PHY_ERROR_LENGTH		0x80
#define AR5K_DESC_RX_PHY_ERROR_64QAM		0xa0
#define AR5K_DESC_RX_PHY_ERROR_SERVICE		0xc0
#define AR5K_DESC_RX_PHY_ERROR_TRANSMITOVR	0xe0

/*
 * 5210/5211 hardware 2-word TX control descriptor
 */
struct ath5k_hw_2w_tx_ctl {
	u32	tx_control_0; /* TX control word 0 */
	u32	tx_control_1; /* TX control word 1 */
} __packed;

/* TX control word 0 fields/flags */
#define AR5K_2W_TX_DESC_CTL0_FRAME_LEN		0x00000fff
#define AR5K_2W_TX_DESC_CTL0_HEADER_LEN		0x0003f000 /*[5210 ?]*/
#define AR5K_2W_TX_DESC_CTL0_HEADER_LEN_S	12
#define AR5K_2W_TX_DESC_CTL0_XMIT_RATE		0x003c0000
#define AR5K_2W_TX_DESC_CTL0_XMIT_RATE_S	18
#define AR5K_2W_TX_DESC_CTL0_RTSENA		0x00400000
#define AR5K_2W_TX_DESC_CTL0_CLRDMASK		0x01000000
#define AR5K_2W_TX_DESC_CTL0_LONG_PACKET	0x00800000 /*[5210]*/
#define AR5K_2W_TX_DESC_CTL0_VEOL		0x00800000 /*[5211]*/
#define AR5K_2W_TX_DESC_CTL0_FRAME_TYPE		0x1c000000 /*[5210]*/
#define AR5K_2W_TX_DESC_CTL0_FRAME_TYPE_S	26
#define AR5K_2W_TX_DESC_CTL0_ANT_MODE_XMIT_5210	0x02000000
#define AR5K_2W_TX_DESC_CTL0_ANT_MODE_XMIT_5211	0x1e000000

#define AR5K_2W_TX_DESC_CTL0_ANT_MODE_XMIT			\
		(ah->ah_version == AR5K_AR5210 ?		\
		AR5K_2W_TX_DESC_CTL0_ANT_MODE_XMIT_5210 :	\
		AR5K_2W_TX_DESC_CTL0_ANT_MODE_XMIT_5211)

#define AR5K_2W_TX_DESC_CTL0_ANT_MODE_XMIT_S	25
#define AR5K_2W_TX_DESC_CTL0_INTREQ		0x20000000
#define AR5K_2W_TX_DESC_CTL0_ENCRYPT_KEY_VALID	0x40000000

/* TX control word 1 fields/flags */
#define AR5K_2W_TX_DESC_CTL1_BUF_LEN		0x00000fff
#define AR5K_2W_TX_DESC_CTL1_MORE		0x00001000
#define AR5K_2W_TX_DESC_CTL1_ENCRYPT_KEY_INDEX_5210	0x0007e000
#define AR5K_2W_TX_DESC_CTL1_ENCRYPT_KEY_INDEX_5211	0x000fe000

#define AR5K_2W_TX_DESC_CTL1_ENCRYPT_KEY_INDEX				\
			(ah->ah_version == AR5K_AR5210 ?		\
			AR5K_2W_TX_DESC_CTL1_ENCRYPT_KEY_INDEX_5210 :	\
			AR5K_2W_TX_DESC_CTL1_ENCRYPT_KEY_INDEX_5211)

#define AR5K_2W_TX_DESC_CTL1_ENCRYPT_KEY_INDEX_S	13
#define AR5K_2W_TX_DESC_CTL1_FRAME_TYPE		0x00700000 /*[5211]*/
#define AR5K_2W_TX_DESC_CTL1_FRAME_TYPE_S	20
#define AR5K_2W_TX_DESC_CTL1_NOACK		0x00800000 /*[5211]*/
#define AR5K_2W_TX_DESC_CTL1_RTS_DURATION	0xfff80000 /*[5210 ?]*/

/* Frame types */
#define AR5K_AR5210_TX_DESC_FRAME_TYPE_NORMAL   0x00
#define AR5K_AR5210_TX_DESC_FRAME_TYPE_ATIM     0x04
#define AR5K_AR5210_TX_DESC_FRAME_TYPE_PSPOLL   0x08
#define AR5K_AR5210_TX_DESC_FRAME_TYPE_NO_DELAY 0x0c
#define AR5K_AR5210_TX_DESC_FRAME_TYPE_PIFS     0x10

/*
 * 5212 hardware 4-word TX control descriptor
 */
struct ath5k_hw_4w_tx_ctl {
	u32	tx_control_0; /* TX control word 0 */

#define AR5K_4W_TX_DESC_CTL0_FRAME_LEN		0x00000fff
#define AR5K_4W_TX_DESC_CTL0_XMIT_POWER		0x003f0000
#define AR5K_4W_TX_DESC_CTL0_XMIT_POWER_S	16
#define AR5K_4W_TX_DESC_CTL0_RTSENA		0x00400000
#define AR5K_4W_TX_DESC_CTL0_VEOL		0x00800000
#define AR5K_4W_TX_DESC_CTL0_CLRDMASK		0x01000000
#define AR5K_4W_TX_DESC_CTL0_ANT_MODE_XMIT	0x1e000000
#define AR5K_4W_TX_DESC_CTL0_ANT_MODE_XMIT_S	25
#define AR5K_4W_TX_DESC_CTL0_INTREQ		0x20000000
#define AR5K_4W_TX_DESC_CTL0_ENCRYPT_KEY_VALID	0x40000000
#define AR5K_4W_TX_DESC_CTL0_CTSENA		0x80000000

	u32	tx_control_1; /* TX control word 1 */

#define AR5K_4W_TX_DESC_CTL1_BUF_LEN		0x00000fff
#define AR5K_4W_TX_DESC_CTL1_MORE		0x00001000
#define AR5K_4W_TX_DESC_CTL1_ENCRYPT_KEY_INDEX	0x000fe000
#define AR5K_4W_TX_DESC_CTL1_ENCRYPT_KEY_INDEX_S	13
#define AR5K_4W_TX_DESC_CTL1_FRAME_TYPE		0x00f00000
#define AR5K_4W_TX_DESC_CTL1_FRAME_TYPE_S	20
#define AR5K_4W_TX_DESC_CTL1_NOACK		0x01000000
#define AR5K_4W_TX_DESC_CTL1_COMP_PROC		0x06000000
#define AR5K_4W_TX_DESC_CTL1_COMP_PROC_S	25
#define AR5K_4W_TX_DESC_CTL1_COMP_IV_LEN	0x18000000
#define AR5K_4W_TX_DESC_CTL1_COMP_IV_LEN_S	27
#define AR5K_4W_TX_DESC_CTL1_COMP_ICV_LEN	0x60000000
#define AR5K_4W_TX_DESC_CTL1_COMP_ICV_LEN_S	29

	u32	tx_control_2; /* TX control word 2 */

#define AR5K_4W_TX_DESC_CTL2_RTS_DURATION		0x00007fff
#define AR5K_4W_TX_DESC_CTL2_DURATION_UPDATE_ENABLE	0x00008000
#define AR5K_4W_TX_DESC_CTL2_XMIT_TRIES0		0x000f0000
#define AR5K_4W_TX_DESC_CTL2_XMIT_TRIES0_S		16
#define AR5K_4W_TX_DESC_CTL2_XMIT_TRIES1		0x00f00000
#define AR5K_4W_TX_DESC_CTL2_XMIT_TRIES1_S		20
#define AR5K_4W_TX_DESC_CTL2_XMIT_TRIES2		0x0f000000
#define AR5K_4W_TX_DESC_CTL2_XMIT_TRIES2_S		24
#define AR5K_4W_TX_DESC_CTL2_XMIT_TRIES3		0xf0000000
#define AR5K_4W_TX_DESC_CTL2_XMIT_TRIES3_S		28

	u32	tx_control_3; /* TX control word 3 */

#define AR5K_4W_TX_DESC_CTL3_XMIT_RATE0		0x0000001f
#define AR5K_4W_TX_DESC_CTL3_XMIT_RATE1		0x000003e0
#define AR5K_4W_TX_DESC_CTL3_XMIT_RATE1_S	5
#define AR5K_4W_TX_DESC_CTL3_XMIT_RATE2		0x00007c00
#define AR5K_4W_TX_DESC_CTL3_XMIT_RATE2_S	10
#define AR5K_4W_TX_DESC_CTL3_XMIT_RATE3		0x000f8000
#define AR5K_4W_TX_DESC_CTL3_XMIT_RATE3_S	15
#define AR5K_4W_TX_DESC_CTL3_RTS_CTS_RATE	0x01f00000
#define AR5K_4W_TX_DESC_CTL3_RTS_CTS_RATE_S	20
} __packed;

/*
 * Common TX status descriptor
 */
struct ath5k_hw_tx_status {
	u32	tx_status_0; /* TX status word 0 */
	u32	tx_status_1; /* TX status word 1 */
} __packed;

/* TX status word 0 fields/flags */
#define AR5K_DESC_TX_STATUS0_FRAME_XMIT_OK	0x00000001
#define AR5K_DESC_TX_STATUS0_EXCESSIVE_RETRIES	0x00000002
#define AR5K_DESC_TX_STATUS0_FIFO_UNDERRUN	0x00000004
#define AR5K_DESC_TX_STATUS0_FILTERED		0x00000008
/*???
#define AR5K_DESC_TX_STATUS0_RTS_FAIL_COUNT	0x000000f0
#define AR5K_DESC_TX_STATUS0_RTS_FAIL_COUNT_S	4
*/
#define AR5K_DESC_TX_STATUS0_SHORT_RETRY_COUNT	0x000000f0
#define AR5K_DESC_TX_STATUS0_SHORT_RETRY_COUNT_S	4
/*???
#define AR5K_DESC_TX_STATUS0_DATA_FAIL_COUNT	0x00000f00
#define AR5K_DESC_TX_STATUS0_DATA_FAIL_COUNT_S	8
*/
#define AR5K_DESC_TX_STATUS0_LONG_RETRY_COUNT	0x00000f00
#define AR5K_DESC_TX_STATUS0_LONG_RETRY_COUNT_S	8
#define AR5K_DESC_TX_STATUS0_VIRT_COLL_COUNT	0x0000f000
#define AR5K_DESC_TX_STATUS0_VIRT_COLL_COUNT_S	12
#define AR5K_DESC_TX_STATUS0_SEND_TIMESTAMP	0xffff0000
#define AR5K_DESC_TX_STATUS0_SEND_TIMESTAMP_S	16

/* TX status word 1 fields/flags */
#define AR5K_DESC_TX_STATUS1_DONE		0x00000001
#define AR5K_DESC_TX_STATUS1_SEQ_NUM		0x00001ffe
#define AR5K_DESC_TX_STATUS1_SEQ_NUM_S		1
#define AR5K_DESC_TX_STATUS1_ACK_SIG_STRENGTH	0x001fe000
#define AR5K_DESC_TX_STATUS1_ACK_SIG_STRENGTH_S	13
#define AR5K_DESC_TX_STATUS1_FINAL_TS_INDEX	0x00600000
#define AR5K_DESC_TX_STATUS1_FINAL_TS_INDEX_S	21
#define AR5K_DESC_TX_STATUS1_COMP_SUCCESS	0x00800000
#define AR5K_DESC_TX_STATUS1_XMIT_ANTENNA	0x01000000

/*
 * 5210/5211 hardware TX descriptor
 */
struct ath5k_hw_5210_tx_desc {
	struct ath5k_hw_2w_tx_ctl	tx_ctl;
	struct ath5k_hw_tx_status	tx_stat;
} __packed;

/*
 * 5212 hardware TX descriptor
 */
struct ath5k_hw_5212_tx_desc {
	struct ath5k_hw_4w_tx_ctl	tx_ctl;
	struct ath5k_hw_tx_status	tx_stat;
} __packed;

/*
 * common hardware RX descriptor
 */
struct ath5k_hw_all_rx_desc {
	struct ath5k_hw_rx_ctl			rx_ctl;
	union {
		struct ath5k_hw_rx_status	rx_stat;
		struct ath5k_hw_rx_error	rx_err;
	} u;
} __packed;

/*
 * Atheros hardware descriptor
 * This is read and written to by the hardware
 */
struct ath5k_desc {
	u32	ds_link;	/* physical address of the next descriptor */
	u32	ds_data;	/* physical address of data buffer (skb) */

	union {
		struct ath5k_hw_5210_tx_desc	ds_tx5210;
		struct ath5k_hw_5212_tx_desc	ds_tx5212;
		struct ath5k_hw_all_rx_desc	ds_rx;
	} ud;
} __packed;

#define AR5K_RXDESC_INTREQ	0x0020

#define AR5K_TXDESC_CLRDMASK	0x0001
#define AR5K_TXDESC_NOACK	0x0002	/*[5211+]*/
#define AR5K_TXDESC_RTSENA	0x0004
#define AR5K_TXDESC_CTSENA	0x0008
#define AR5K_TXDESC_INTREQ	0x0010
#define AR5K_TXDESC_VEOL	0x0020	/*[5211+]*/

