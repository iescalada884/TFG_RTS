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
 * RF Gain optimization
 *
 * Copyright (c) 2004-2009 Reyk Floeter <reyk@openbsd.org>
 * Copyright (c) 2006-2009 Nick Kossifidis <mickflemm@gmail.com>
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
 * Mode-specific RF Gain table (64bytes) for RF5111/5112
 * (RF5110 only comes with AR5210 and only supports a/turbo a mode so initial
 * RF Gain values are included in AR5K_AR5210_INI)
 */
struct ath5k_ini_rfgain {
	u16	rfg_register;	/* RF Gain register address */
	u32	rfg_value[2];	/* [freq (see below)] */
};

/* Initial RF Gain settings for RF5111 */
static const struct ath5k_ini_rfgain rfgain_5111[] = {
	/*			      5Ghz	2Ghz	*/
	{ AR5K_RF_GAIN(0),	{ 0x000001a9, 0x00000000 } },
	{ AR5K_RF_GAIN(1),	{ 0x000001e9, 0x00000040 } },
	{ AR5K_RF_GAIN(2),	{ 0x00000029, 0x00000080 } },
	{ AR5K_RF_GAIN(3),	{ 0x00000069, 0x00000150 } },
	{ AR5K_RF_GAIN(4),	{ 0x00000199, 0x00000190 } },
	{ AR5K_RF_GAIN(5),	{ 0x000001d9, 0x000001d0 } },
	{ AR5K_RF_GAIN(6),	{ 0x00000019, 0x00000010 } },
	{ AR5K_RF_GAIN(7),	{ 0x00000059, 0x00000044 } },
	{ AR5K_RF_GAIN(8),	{ 0x00000099, 0x00000084 } },
	{ AR5K_RF_GAIN(9),	{ 0x000001a5, 0x00000148 } },
	{ AR5K_RF_GAIN(10),	{ 0x000001e5, 0x00000188 } },
	{ AR5K_RF_GAIN(11),	{ 0x00000025, 0x000001c8 } },
	{ AR5K_RF_GAIN(12),	{ 0x000001c8, 0x00000014 } },
	{ AR5K_RF_GAIN(13),	{ 0x00000008, 0x00000042 } },
	{ AR5K_RF_GAIN(14),	{ 0x00000048, 0x00000082 } },
	{ AR5K_RF_GAIN(15),	{ 0x00000088, 0x00000178 } },
	{ AR5K_RF_GAIN(16),	{ 0x00000198, 0x000001b8 } },
	{ AR5K_RF_GAIN(17),	{ 0x000001d8, 0x000001f8 } },
	{ AR5K_RF_GAIN(18),	{ 0x00000018, 0x00000012 } },
	{ AR5K_RF_GAIN(19),	{ 0x00000058, 0x00000052 } },
	{ AR5K_RF_GAIN(20),	{ 0x00000098, 0x00000092 } },
	{ AR5K_RF_GAIN(21),	{ 0x000001a4, 0x0000017c } },
	{ AR5K_RF_GAIN(22),	{ 0x000001e4, 0x000001bc } },
	{ AR5K_RF_GAIN(23),	{ 0x00000024, 0x000001fc } },
	{ AR5K_RF_GAIN(24),	{ 0x00000064, 0x0000000a } },
	{ AR5K_RF_GAIN(25),	{ 0x000000a4, 0x0000004a } },
	{ AR5K_RF_GAIN(26),	{ 0x000000e4, 0x0000008a } },
	{ AR5K_RF_GAIN(27),	{ 0x0000010a, 0x0000015a } },
	{ AR5K_RF_GAIN(28),	{ 0x0000014a, 0x0000019a } },
	{ AR5K_RF_GAIN(29),	{ 0x0000018a, 0x000001da } },
	{ AR5K_RF_GAIN(30),	{ 0x000001ca, 0x0000000e } },
	{ AR5K_RF_GAIN(31),	{ 0x0000000a, 0x0000004e } },
	{ AR5K_RF_GAIN(32),	{ 0x0000004a, 0x0000008e } },
	{ AR5K_RF_GAIN(33),	{ 0x0000008a, 0x0000015e } },
	{ AR5K_RF_GAIN(34),	{ 0x000001ba, 0x0000019e } },
	{ AR5K_RF_GAIN(35),	{ 0x000001fa, 0x000001de } },
	{ AR5K_RF_GAIN(36),	{ 0x0000003a, 0x00000009 } },
	{ AR5K_RF_GAIN(37),	{ 0x0000007a, 0x00000049 } },
	{ AR5K_RF_GAIN(38),	{ 0x00000186, 0x00000089 } },
	{ AR5K_RF_GAIN(39),	{ 0x000001c6, 0x00000179 } },
	{ AR5K_RF_GAIN(40),	{ 0x00000006, 0x000001b9 } },
	{ AR5K_RF_GAIN(41),	{ 0x00000046, 0x000001f9 } },
	{ AR5K_RF_GAIN(42),	{ 0x00000086, 0x00000039 } },
	{ AR5K_RF_GAIN(43),	{ 0x000000c6, 0x00000079 } },
	{ AR5K_RF_GAIN(44),	{ 0x000000c6, 0x000000b9 } },
	{ AR5K_RF_GAIN(45),	{ 0x000000c6, 0x000001bd } },
	{ AR5K_RF_GAIN(46),	{ 0x000000c6, 0x000001fd } },
	{ AR5K_RF_GAIN(47),	{ 0x000000c6, 0x0000003d } },
	{ AR5K_RF_GAIN(48),	{ 0x000000c6, 0x0000007d } },
	{ AR5K_RF_GAIN(49),	{ 0x000000c6, 0x000000bd } },
	{ AR5K_RF_GAIN(50),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(51),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(52),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(53),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(54),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(55),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(56),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(57),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(58),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(59),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(60),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(61),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(62),	{ 0x000000c6, 0x000000fd } },
	{ AR5K_RF_GAIN(63),	{ 0x000000c6, 0x000000fd } },
};

/* Initial RF Gain settings for RF5112 */
static const struct ath5k_ini_rfgain rfgain_5112[] = {
	/*			      5Ghz	2Ghz	*/
	{ AR5K_RF_GAIN(0),	{ 0x00000007, 0x00000007 } },
	{ AR5K_RF_GAIN(1),	{ 0x00000047, 0x00000047 } },
	{ AR5K_RF_GAIN(2),	{ 0x00000087, 0x00000087 } },
	{ AR5K_RF_GAIN(3),	{ 0x000001a0, 0x000001a0 } },
	{ AR5K_RF_GAIN(4),	{ 0x000001e0, 0x000001e0 } },
	{ AR5K_RF_GAIN(5),	{ 0x00000020, 0x00000020 } },
	{ AR5K_RF_GAIN(6),	{ 0x00000060, 0x00000060 } },
	{ AR5K_RF_GAIN(7),	{ 0x000001a1, 0x000001a1 } },
	{ AR5K_RF_GAIN(8),	{ 0x000001e1, 0x000001e1 } },
	{ AR5K_RF_GAIN(9),	{ 0x00000021, 0x00000021 } },
	{ AR5K_RF_GAIN(10),	{ 0x00000061, 0x00000061 } },
	{ AR5K_RF_GAIN(11),	{ 0x00000162, 0x00000162 } },
	{ AR5K_RF_GAIN(12),	{ 0x000001a2, 0x000001a2 } },
	{ AR5K_RF_GAIN(13),	{ 0x000001e2, 0x000001e2 } },
	{ AR5K_RF_GAIN(14),	{ 0x00000022, 0x00000022 } },
	{ AR5K_RF_GAIN(15),	{ 0x00000062, 0x00000062 } },
	{ AR5K_RF_GAIN(16),	{ 0x00000163, 0x00000163 } },
	{ AR5K_RF_GAIN(17),	{ 0x000001a3, 0x000001a3 } },
	{ AR5K_RF_GAIN(18),	{ 0x000001e3, 0x000001e3 } },
	{ AR5K_RF_GAIN(19),	{ 0x00000023, 0x00000023 } },
	{ AR5K_RF_GAIN(20),	{ 0x00000063, 0x00000063 } },
	{ AR5K_RF_GAIN(21),	{ 0x00000184, 0x00000184 } },
	{ AR5K_RF_GAIN(22),	{ 0x000001c4, 0x000001c4 } },
	{ AR5K_RF_GAIN(23),	{ 0x00000004, 0x00000004 } },
	{ AR5K_RF_GAIN(24),	{ 0x000001ea, 0x0000000b } },
	{ AR5K_RF_GAIN(25),	{ 0x0000002a, 0x0000004b } },
	{ AR5K_RF_GAIN(26),	{ 0x0000006a, 0x0000008b } },
	{ AR5K_RF_GAIN(27),	{ 0x000000aa, 0x000001ac } },
	{ AR5K_RF_GAIN(28),	{ 0x000001ab, 0x000001ec } },
	{ AR5K_RF_GAIN(29),	{ 0x000001eb, 0x0000002c } },
	{ AR5K_RF_GAIN(30),	{ 0x0000002b, 0x00000012 } },
	{ AR5K_RF_GAIN(31),	{ 0x0000006b, 0x00000052 } },
	{ AR5K_RF_GAIN(32),	{ 0x000000ab, 0x00000092 } },
	{ AR5K_RF_GAIN(33),	{ 0x000001ac, 0x00000193 } },
	{ AR5K_RF_GAIN(34),	{ 0x000001ec, 0x000001d3 } },
	{ AR5K_RF_GAIN(35),	{ 0x0000002c, 0x00000013 } },
	{ AR5K_RF_GAIN(36),	{ 0x0000003a, 0x00000053 } },
	{ AR5K_RF_GAIN(37),	{ 0x0000007a, 0x00000093 } },
	{ AR5K_RF_GAIN(38),	{ 0x000000ba, 0x00000194 } },
	{ AR5K_RF_GAIN(39),	{ 0x000001bb, 0x000001d4 } },
	{ AR5K_RF_GAIN(40),	{ 0x000001fb, 0x00000014 } },
	{ AR5K_RF_GAIN(41),	{ 0x0000003b, 0x0000003a } },
	{ AR5K_RF_GAIN(42),	{ 0x0000007b, 0x0000007a } },
	{ AR5K_RF_GAIN(43),	{ 0x000000bb, 0x000000ba } },
	{ AR5K_RF_GAIN(44),	{ 0x000001bc, 0x000001bb } },
	{ AR5K_RF_GAIN(45),	{ 0x000001fc, 0x000001fb } },
	{ AR5K_RF_GAIN(46),	{ 0x0000003c, 0x0000003b } },
	{ AR5K_RF_GAIN(47),	{ 0x0000007c, 0x0000007b } },
	{ AR5K_RF_GAIN(48),	{ 0x000000bc, 0x000000bb } },
	{ AR5K_RF_GAIN(49),	{ 0x000000fc, 0x000001bc } },
	{ AR5K_RF_GAIN(50),	{ 0x000000fc, 0x000001fc } },
	{ AR5K_RF_GAIN(51),	{ 0x000000fc, 0x0000003c } },
	{ AR5K_RF_GAIN(52),	{ 0x000000fc, 0x0000007c } },
	{ AR5K_RF_GAIN(53),	{ 0x000000fc, 0x000000bc } },
	{ AR5K_RF_GAIN(54),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(55),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(56),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(57),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(58),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(59),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(60),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(61),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(62),	{ 0x000000fc, 0x000000fc } },
	{ AR5K_RF_GAIN(63),	{ 0x000000fc, 0x000000fc } },
};

/* Initial RF Gain settings for RF2413 */
static const struct ath5k_ini_rfgain rfgain_2413[] = {
	{ AR5K_RF_GAIN(0),	{ 0x00000000, 0x00000000 } },
	{ AR5K_RF_GAIN(1),	{ 0x00000000, 0x00000040 } },
	{ AR5K_RF_GAIN(2),	{ 0x00000000, 0x00000080 } },
	{ AR5K_RF_GAIN(3),	{ 0x00000000, 0x00000181 } },
	{ AR5K_RF_GAIN(4),	{ 0x00000000, 0x000001c1 } },
	{ AR5K_RF_GAIN(5),	{ 0x00000000, 0x00000001 } },
	{ AR5K_RF_GAIN(6),	{ 0x00000000, 0x00000041 } },
	{ AR5K_RF_GAIN(7),	{ 0x00000000, 0x00000081 } },
	{ AR5K_RF_GAIN(8),	{ 0x00000000, 0x00000168 } },
	{ AR5K_RF_GAIN(9),	{ 0x00000000, 0x000001a8 } },
	{ AR5K_RF_GAIN(10),	{ 0x00000000, 0x000001e8 } },
	{ AR5K_RF_GAIN(11),	{ 0x00000000, 0x00000028 } },
	{ AR5K_RF_GAIN(12),	{ 0x00000000, 0x00000068 } },
	{ AR5K_RF_GAIN(13),	{ 0x00000000, 0x00000189 } },
	{ AR5K_RF_GAIN(14),	{ 0x00000000, 0x000001c9 } },
	{ AR5K_RF_GAIN(15),	{ 0x00000000, 0x00000009 } },
	{ AR5K_RF_GAIN(16),	{ 0x00000000, 0x00000049 } },
	{ AR5K_RF_GAIN(17),	{ 0x00000000, 0x00000089 } },
	{ AR5K_RF_GAIN(18),	{ 0x00000000, 0x00000190 } },
	{ AR5K_RF_GAIN(19),	{ 0x00000000, 0x000001d0 } },
	{ AR5K_RF_GAIN(20),	{ 0x00000000, 0x00000010 } },
	{ AR5K_RF_GAIN(21),	{ 0x00000000, 0x00000050 } },
	{ AR5K_RF_GAIN(22),	{ 0x00000000, 0x00000090 } },
	{ AR5K_RF_GAIN(23),	{ 0x00000000, 0x00000191 } },
	{ AR5K_RF_GAIN(24),	{ 0x00000000, 0x000001d1 } },
	{ AR5K_RF_GAIN(25),	{ 0x00000000, 0x00000011 } },
	{ AR5K_RF_GAIN(26),	{ 0x00000000, 0x00000051 } },
	{ AR5K_RF_GAIN(27),	{ 0x00000000, 0x00000091 } },
	{ AR5K_RF_GAIN(28),	{ 0x00000000, 0x00000178 } },
	{ AR5K_RF_GAIN(29),	{ 0x00000000, 0x000001b8 } },
	{ AR5K_RF_GAIN(30),	{ 0x00000000, 0x000001f8 } },
	{ AR5K_RF_GAIN(31),	{ 0x00000000, 0x00000038 } },
	{ AR5K_RF_GAIN(32),	{ 0x00000000, 0x00000078 } },
	{ AR5K_RF_GAIN(33),	{ 0x00000000, 0x00000199 } },
	{ AR5K_RF_GAIN(34),	{ 0x00000000, 0x000001d9 } },
	{ AR5K_RF_GAIN(35),	{ 0x00000000, 0x00000019 } },
	{ AR5K_RF_GAIN(36),	{ 0x00000000, 0x00000059 } },
	{ AR5K_RF_GAIN(37),	{ 0x00000000, 0x00000099 } },
	{ AR5K_RF_GAIN(38),	{ 0x00000000, 0x000000d9 } },
	{ AR5K_RF_GAIN(39),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(40),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(41),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(42),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(43),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(44),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(45),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(46),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(47),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(48),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(49),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(50),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(51),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(52),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(53),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(54),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(55),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(56),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(57),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(58),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(59),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(60),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(61),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(62),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(63),	{ 0x00000000, 0x000000f9 } },
};

/* Initial RF Gain settings for AR2316 */
static const struct ath5k_ini_rfgain rfgain_2316[] = {
	{ AR5K_RF_GAIN(0),	{ 0x00000000, 0x00000000 } },
	{ AR5K_RF_GAIN(1),	{ 0x00000000, 0x00000040 } },
	{ AR5K_RF_GAIN(2),	{ 0x00000000, 0x00000080 } },
	{ AR5K_RF_GAIN(3),	{ 0x00000000, 0x000000c0 } },
	{ AR5K_RF_GAIN(4),	{ 0x00000000, 0x000000e0 } },
	{ AR5K_RF_GAIN(5),	{ 0x00000000, 0x000000e0 } },
	{ AR5K_RF_GAIN(6),	{ 0x00000000, 0x00000128 } },
	{ AR5K_RF_GAIN(7),	{ 0x00000000, 0x00000128 } },
	{ AR5K_RF_GAIN(8),	{ 0x00000000, 0x00000128 } },
	{ AR5K_RF_GAIN(9),	{ 0x00000000, 0x00000168 } },
	{ AR5K_RF_GAIN(10),	{ 0x00000000, 0x000001a8 } },
	{ AR5K_RF_GAIN(11),	{ 0x00000000, 0x000001e8 } },
	{ AR5K_RF_GAIN(12),	{ 0x00000000, 0x00000028 } },
	{ AR5K_RF_GAIN(13),	{ 0x00000000, 0x00000068 } },
	{ AR5K_RF_GAIN(14),	{ 0x00000000, 0x000000a8 } },
	{ AR5K_RF_GAIN(15),	{ 0x00000000, 0x000000e8 } },
	{ AR5K_RF_GAIN(16),	{ 0x00000000, 0x000000e8 } },
	{ AR5K_RF_GAIN(17),	{ 0x00000000, 0x00000130 } },
	{ AR5K_RF_GAIN(18),	{ 0x00000000, 0x00000130 } },
	{ AR5K_RF_GAIN(19),	{ 0x00000000, 0x00000170 } },
	{ AR5K_RF_GAIN(20),	{ 0x00000000, 0x000001b0 } },
	{ AR5K_RF_GAIN(21),	{ 0x00000000, 0x000001f0 } },
	{ AR5K_RF_GAIN(22),	{ 0x00000000, 0x00000030 } },
	{ AR5K_RF_GAIN(23),	{ 0x00000000, 0x00000070 } },
	{ AR5K_RF_GAIN(24),	{ 0x00000000, 0x000000b0 } },
	{ AR5K_RF_GAIN(25),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(26),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(27),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(28),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(29),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(30),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(31),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(32),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(33),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(34),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(35),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(36),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(37),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(38),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(39),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(40),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(41),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(42),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(43),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(44),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(45),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(46),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(47),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(48),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(49),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(50),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(51),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(52),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(53),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(54),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(55),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(56),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(57),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(58),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(59),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(60),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(61),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(62),	{ 0x00000000, 0x000000f0 } },
	{ AR5K_RF_GAIN(63),	{ 0x00000000, 0x000000f0 } },
};


/* Initial RF Gain settings for RF5413 */
static const struct ath5k_ini_rfgain rfgain_5413[] = {
	/*			      5Ghz	2Ghz	*/
	{ AR5K_RF_GAIN(0),	{ 0x00000000, 0x00000000 } },
	{ AR5K_RF_GAIN(1),	{ 0x00000040, 0x00000040 } },
	{ AR5K_RF_GAIN(2),	{ 0x00000080, 0x00000080 } },
	{ AR5K_RF_GAIN(3),	{ 0x000001a1, 0x00000161 } },
	{ AR5K_RF_GAIN(4),	{ 0x000001e1, 0x000001a1 } },
	{ AR5K_RF_GAIN(5),	{ 0x00000021, 0x000001e1 } },
	{ AR5K_RF_GAIN(6),	{ 0x00000061, 0x00000021 } },
	{ AR5K_RF_GAIN(7),	{ 0x00000188, 0x00000061 } },
	{ AR5K_RF_GAIN(8),	{ 0x000001c8, 0x00000188 } },
	{ AR5K_RF_GAIN(9),	{ 0x00000008, 0x000001c8 } },
	{ AR5K_RF_GAIN(10),	{ 0x00000048, 0x00000008 } },
	{ AR5K_RF_GAIN(11),	{ 0x00000088, 0x00000048 } },
	{ AR5K_RF_GAIN(12),	{ 0x000001a9, 0x00000088 } },
	{ AR5K_RF_GAIN(13),	{ 0x000001e9, 0x00000169 } },
	{ AR5K_RF_GAIN(14),	{ 0x00000029, 0x000001a9 } },
	{ AR5K_RF_GAIN(15),	{ 0x00000069, 0x000001e9 } },
	{ AR5K_RF_GAIN(16),	{ 0x000001d0, 0x00000029 } },
	{ AR5K_RF_GAIN(17),	{ 0x00000010, 0x00000069 } },
	{ AR5K_RF_GAIN(18),	{ 0x00000050, 0x00000190 } },
	{ AR5K_RF_GAIN(19),	{ 0x00000090, 0x000001d0 } },
	{ AR5K_RF_GAIN(20),	{ 0x000001b1, 0x00000010 } },
	{ AR5K_RF_GAIN(21),	{ 0x000001f1, 0x00000050 } },
	{ AR5K_RF_GAIN(22),	{ 0x00000031, 0x00000090 } },
	{ AR5K_RF_GAIN(23),	{ 0x00000071, 0x00000171 } },
	{ AR5K_RF_GAIN(24),	{ 0x000001b8, 0x000001b1 } },
	{ AR5K_RF_GAIN(25),	{ 0x000001f8, 0x000001f1 } },
	{ AR5K_RF_GAIN(26),	{ 0x00000038, 0x00000031 } },
	{ AR5K_RF_GAIN(27),	{ 0x00000078, 0x00000071 } },
	{ AR5K_RF_GAIN(28),	{ 0x00000199, 0x00000198 } },
	{ AR5K_RF_GAIN(29),	{ 0x000001d9, 0x000001d8 } },
	{ AR5K_RF_GAIN(30),	{ 0x00000019, 0x00000018 } },
	{ AR5K_RF_GAIN(31),	{ 0x00000059, 0x00000058 } },
	{ AR5K_RF_GAIN(32),	{ 0x00000099, 0x00000098 } },
	{ AR5K_RF_GAIN(33),	{ 0x000000d9, 0x00000179 } },
	{ AR5K_RF_GAIN(34),	{ 0x000000f9, 0x000001b9 } },
	{ AR5K_RF_GAIN(35),	{ 0x000000f9, 0x000001f9 } },
	{ AR5K_RF_GAIN(36),	{ 0x000000f9, 0x00000039 } },
	{ AR5K_RF_GAIN(37),	{ 0x000000f9, 0x00000079 } },
	{ AR5K_RF_GAIN(38),	{ 0x000000f9, 0x000000b9 } },
	{ AR5K_RF_GAIN(39),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(40),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(41),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(42),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(43),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(44),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(45),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(46),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(47),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(48),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(49),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(50),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(51),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(52),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(53),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(54),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(55),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(56),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(57),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(58),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(59),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(60),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(61),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(62),	{ 0x000000f9, 0x000000f9 } },
	{ AR5K_RF_GAIN(63),	{ 0x000000f9, 0x000000f9 } },
};


/* Initial RF Gain settings for RF2425 */
static const struct ath5k_ini_rfgain rfgain_2425[] = {
	{ AR5K_RF_GAIN(0),	{ 0x00000000, 0x00000000 } },
	{ AR5K_RF_GAIN(1),	{ 0x00000000, 0x00000040 } },
	{ AR5K_RF_GAIN(2),	{ 0x00000000, 0x00000080 } },
	{ AR5K_RF_GAIN(3),	{ 0x00000000, 0x00000181 } },
	{ AR5K_RF_GAIN(4),	{ 0x00000000, 0x000001c1 } },
	{ AR5K_RF_GAIN(5),	{ 0x00000000, 0x00000001 } },
	{ AR5K_RF_GAIN(6),	{ 0x00000000, 0x00000041 } },
	{ AR5K_RF_GAIN(7),	{ 0x00000000, 0x00000081 } },
	{ AR5K_RF_GAIN(8),	{ 0x00000000, 0x00000188 } },
	{ AR5K_RF_GAIN(9),	{ 0x00000000, 0x000001c8 } },
	{ AR5K_RF_GAIN(10),	{ 0x00000000, 0x00000008 } },
	{ AR5K_RF_GAIN(11),	{ 0x00000000, 0x00000048 } },
	{ AR5K_RF_GAIN(12),	{ 0x00000000, 0x00000088 } },
	{ AR5K_RF_GAIN(13),	{ 0x00000000, 0x00000189 } },
	{ AR5K_RF_GAIN(14),	{ 0x00000000, 0x000001c9 } },
	{ AR5K_RF_GAIN(15),	{ 0x00000000, 0x00000009 } },
	{ AR5K_RF_GAIN(16),	{ 0x00000000, 0x00000049 } },
	{ AR5K_RF_GAIN(17),	{ 0x00000000, 0x00000089 } },
	{ AR5K_RF_GAIN(18),	{ 0x00000000, 0x000001b0 } },
	{ AR5K_RF_GAIN(19),	{ 0x00000000, 0x000001f0 } },
	{ AR5K_RF_GAIN(20),	{ 0x00000000, 0x00000030 } },
	{ AR5K_RF_GAIN(21),	{ 0x00000000, 0x00000070 } },
	{ AR5K_RF_GAIN(22),	{ 0x00000000, 0x00000171 } },
	{ AR5K_RF_GAIN(23),	{ 0x00000000, 0x000001b1 } },
	{ AR5K_RF_GAIN(24),	{ 0x00000000, 0x000001f1 } },
	{ AR5K_RF_GAIN(25),	{ 0x00000000, 0x00000031 } },
	{ AR5K_RF_GAIN(26),	{ 0x00000000, 0x00000071 } },
	{ AR5K_RF_GAIN(27),	{ 0x00000000, 0x000001b8 } },
	{ AR5K_RF_GAIN(28),	{ 0x00000000, 0x000001f8 } },
	{ AR5K_RF_GAIN(29),	{ 0x00000000, 0x00000038 } },
	{ AR5K_RF_GAIN(30),	{ 0x00000000, 0x00000078 } },
	{ AR5K_RF_GAIN(31),	{ 0x00000000, 0x000000b8 } },
	{ AR5K_RF_GAIN(32),	{ 0x00000000, 0x000001b9 } },
	{ AR5K_RF_GAIN(33),	{ 0x00000000, 0x000001f9 } },
	{ AR5K_RF_GAIN(34),	{ 0x00000000, 0x00000039 } },
	{ AR5K_RF_GAIN(35),	{ 0x00000000, 0x00000079 } },
	{ AR5K_RF_GAIN(36),	{ 0x00000000, 0x000000b9 } },
	{ AR5K_RF_GAIN(37),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(38),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(39),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(40),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(41),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(42),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(43),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(44),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(45),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(46),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(47),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(48),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(49),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(50),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(51),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(52),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(53),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(54),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(55),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(56),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(57),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(58),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(59),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(60),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(61),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(62),	{ 0x00000000, 0x000000f9 } },
	{ AR5K_RF_GAIN(63),	{ 0x00000000, 0x000000f9 } },
};

#define AR5K_GAIN_CRN_FIX_BITS_5111		4
#define AR5K_GAIN_CRN_FIX_BITS_5112		7
#define AR5K_GAIN_CRN_MAX_FIX_BITS		AR5K_GAIN_CRN_FIX_BITS_5112
#define AR5K_GAIN_DYN_ADJUST_HI_MARGIN		15
#define AR5K_GAIN_DYN_ADJUST_LO_MARGIN		20
#define AR5K_GAIN_CCK_PROBE_CORR		5
#define AR5K_GAIN_CCK_OFDM_GAIN_DELTA		15
#define AR5K_GAIN_STEP_COUNT			10

/* Check if our current measurement is inside our
 * current variable attenuation window */
#define AR5K_GAIN_CHECK_ADJUST(_g) 		\
	((_g)->g_current <= (_g)->g_low || (_g)->g_current >= (_g)->g_high)

struct ath5k_gain_opt_step {
	s8				gos_param[AR5K_GAIN_CRN_MAX_FIX_BITS];
	s8				gos_gain;
};

struct ath5k_gain_opt {
	u8				go_default;
	u8				go_steps_count;
	const struct ath5k_gain_opt_step	go_step[AR5K_GAIN_STEP_COUNT];
};

/*
 * Parameters on gos_param:
 * 1) Tx clip PHY register
 * 2) PWD 90 RF register
 * 3) PWD 84 RF register
 * 4) RFGainSel RF register
 */
static const struct ath5k_gain_opt rfgain_opt_5111 = {
	4,
	9,
	{
		{ { 4, 1, 1, 1 }, 6 },
		{ { 4, 0, 1, 1 }, 4 },
		{ { 3, 1, 1, 1 }, 3 },
		{ { 4, 0, 0, 1 }, 1 },
		{ { 4, 1, 1, 0 }, 0 },
		{ { 4, 0, 1, 0 }, -2 },
		{ { 3, 1, 1, 0 }, -3 },
		{ { 4, 0, 0, 0 }, -4 },
		{ { 2, 1, 1, 0 }, -6 }
	}
};

/*
 * Parameters on gos_param:
 * 1) Mixgain ovr RF register
 * 2) PWD 138 RF register
 * 3) PWD 137 RF register
 * 4) PWD 136 RF register
 * 5) PWD 132 RF register
 * 6) PWD 131 RF register
 * 7) PWD 130 RF register
 */
static const struct ath5k_gain_opt rfgain_opt_5112 = {
	1,
	8,
	{
		{ { 3, 0, 0, 0, 0, 0, 0 }, 6 },
		{ { 2, 0, 0, 0, 0, 0, 0 }, 0 },
		{ { 1, 0, 0, 0, 0, 0, 0 }, -3 },
		{ { 0, 0, 0, 0, 0, 0, 0 }, -6 },
		{ { 0, 1, 1, 0, 0, 0, 0 }, -8 },
		{ { 0, 1, 1, 0, 1, 1, 0 }, -10 },
		{ { 0, 1, 0, 1, 1, 1, 0 }, -13 },
		{ { 0, 1, 0, 1, 1, 0, 1 }, -16 },
	}
};

