/*
    bttv-cards.c

    this file has configuration informations - card-specific stuff
    like the big tvcards array for the most part

    Copyright (C) 1996,97,98 Ralph  Metzler (rjkm@thp.uni-koeln.de)
                           & Marcus Metzler (mocm@thp.uni-koeln.de)
    (c) 1999-2001 Gerd Knorr <kraxel@goldbach.in-berlin.de>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include <compbttv.h>

#include <linux/delay.h>
#include <linux/ioctl.h>
#include <linux/pci.h>

#include <asm/io.h>

#include "bttvp.h"
#include "bt832.h"

/* fwd decl */
static void boot_msp34xx(struct bttv *btv, int pin);
static void boot_bt832(struct bttv *btv);
static void hauppauge_eeprom(struct bttv *btv);
static void avermedia_eeprom(struct bttv *btv);
static void osprey_eeprom(struct bttv *btv);
static void modtec_eeprom(struct bttv *btv);
static void init_PXC200(struct bttv *btv);

static void winview_audio(struct bttv *btv, struct video_audio *v, int set);
static void lt9415_audio(struct bttv *btv, struct video_audio *v, int set);
static void avermedia_tvphone_audio(struct bttv *btv, struct video_audio *v,
				    int set);
static void terratv_audio(struct bttv *btv, struct video_audio *v, int set);
static void gvbctv3pci_audio(struct bttv *btv, struct video_audio *v, int set);
static void winfast2000_audio(struct bttv *btv, struct video_audio *v, int set);
static void pvbt878p9b_audio(struct bttv *btv, struct video_audio *v, int set);
static void fv2000s_audio(struct bttv *btv, struct video_audio *v, int set);
static void windvr_audio(struct bttv *btv, struct video_audio *v, int set);
static void adtvk503_audio(struct bttv *btv, struct video_audio *v, int set);
static void rv605_muxsel(struct bttv *btv, unsigned int input);
static void eagle_muxsel(struct bttv *btv, unsigned int input);
static void xguard_muxsel(struct bttv *btv, unsigned int input);
static void ivc120_muxsel(struct bttv *btv, unsigned int input);

static int terratec_active_radio_upgrade(struct bttv *btv);
static int tea5757_read(struct bttv *btv);
static int tea5757_write(struct bttv *btv, int value);
static void identify_by_eeprom(struct bttv *btv,
			       unsigned char eeprom_data[256]);

/* config variables */
static unsigned int triton1=0;
static unsigned int vsfx=0;
static unsigned int latency = UNSET;
unsigned int no_overlay=-1;

static unsigned int card[BTTV_MAX]  = { [ 0 ... (BTTV_MAX-1) ] = UNSET};
static unsigned int pll[BTTV_MAX]   = { [ 0 ... (BTTV_MAX-1) ] = UNSET};
static unsigned int tuner[BTTV_MAX] = { [ 0 ... (BTTV_MAX-1) ] = UNSET};
static unsigned int svhs[BTTV_MAX]  = { [ 0 ... (BTTV_MAX-1) ] = UNSET};
static unsigned int gpiomask = UNSET;
static unsigned int audioall = UNSET;
static unsigned int audiomux[5] = { [ 0 ... 4 ] = UNSET };

/* ----------------------------------------------------------------------- */
/* list of card IDs for bt878+ cards                                       */

static struct CARD {
	unsigned id;
	int cardnr;
	char *name;
} cards[] = {
	{ 0x13eb0070, BTTV_HAUPPAUGE878,  "Hauppauge WinTV" },
	{ 0x39000070, BTTV_HAUPPAUGE878,  "Hauppauge WinTV-D" },
	{ 0x45000070, BTTV_HAUPPAUGEPVR,  "Hauppauge WinTV/PVR" },
	{ 0xff000070, BTTV_OSPREY1x0,     "Osprey-100" },
	{ 0xff010070, BTTV_OSPREY2x0_SVID,"Osprey-200" },
	{ 0xff020070, BTTV_OSPREY500,     "Osprey-500" },
	{ 0xff030070, BTTV_OSPREY2000,    "Osprey-2000" },
	{ 0xff040070, BTTV_OSPREY540,     "Osprey-540" },
	{ 0x00011002, BTTV_ATI_TVWONDER,  "ATI TV Wonder" },
	{ 0x00031002, BTTV_ATI_TVWONDERVE,"ATI TV Wonder/VE" },
	{ 0x6606107d, BTTV_WINFAST2000,   "Leadtek WinFast TV 2000" },
	{ 0x6607107d, BTTV_WINFAST2000,   "Leadtek WinFast VC 100" },
	{ 0x263610b4, BTTV_STB2,          "STB TV PCI FM, Gateway P/N 6000704" },
	{ 0x264510b4, BTTV_STB2,          "STB TV PCI FM, Gateway P/N 6000704" },
 	{ 0x402010fc, BTTV_GVBCTV3PCI,    "I-O Data Co. GV-BCTV3/PCI" },
	{ 0x405010fc, BTTV_GVBCTV4PCI,    "I-O Data Co. GV-BCTV4/PCI" },
	{ 0x407010fc, BTTV_GVBCTV5PCI,    "I-O Data Co. GV-BCTV5/PCI" },
	{ 0x001211bd, BTTV_PINNACLE,      "Pinnacle PCTV" },
	{ 0x001c11bd, BTTV_PINNACLESAT,   "Pinnacle PCTV Sat" },
	{ 0x1200bd11, BTTV_PINNACLE,      "Pinnacle PCTV [bswap]" },
	{ 0xff00bd11, BTTV_PINNACLE,      "Pinnacle PCTV [bswap]" },
	{ 0x3000121a, BTTV_VOODOOTV_FM,   "3Dfx VoodooTV FM/ VoodooTV 200" },
	{ 0x3060121a, BTTV_STB2,	  "3Dfx VoodooTV 100/ STB OEM" },
	{ 0x3000144f, BTTV_MAGICTVIEW063, "(Askey Magic/others) TView99 CPH06x" },
	{ 0x3002144f, BTTV_MAGICTVIEW061, "(Askey Magic/others) TView99 CPH05x" },
	{ 0x3005144f, BTTV_MAGICTVIEW061, "(Askey Magic/others) TView99 CPH061/06L (T1/LC)" },
	{ 0x5000144f, BTTV_MAGICTVIEW061, "Askey CPH050" },
	{ 0x00011461, BTTV_AVPHONE98,     "AVerMedia TVPhone98" },
	{ 0x00021461, BTTV_AVERMEDIA98,   "AVermedia TVCapture 98" },
	{ 0x00031461, BTTV_AVPHONE98,     "AVerMedia TVPhone98" },
	{ 0x00041461, BTTV_AVERMEDIA98,   "AVerMedia TVCapture 98" },
	{ 0x03001461, BTTV_AVERMEDIA98,   "VDOMATE TV TUNER CARD" },
	{ 0x300014ff, BTTV_MAGICTVIEW061, "TView 99 (CPH061)" },
	{ 0x300214ff, BTTV_PHOEBE_TVMAS,  "Phoebe TV Master (CPH060)" },
	{ 0x1117153b, BTTV_TERRATVALUE,   "Terratec TValue (Philips PAL B/G)" },
	{ 0x1118153b, BTTV_TERRATVALUE,   "Terratec TValue (Temic PAL B/G)" },
	{ 0x1119153b, BTTV_TERRATVALUE,   "Terratec TValue (Philips PAL I)" },
	{ 0x111a153b, BTTV_TERRATVALUE,   "Terratec TValue (Temic PAL I)" },
	{ 0x1123153b, BTTV_TERRATVRADIO,  "Terratec TV Radio+" },
	{ 0x1127153b, BTTV_TERRATV,       "Terratec TV+ (V1.05)"    },
	{ 0x1134153b, BTTV_TERRATVALUE,   "Terratec TValue (LR102)" },
	{ 0x1135153b, BTTV_TERRATVALUER,  "Terratec TValue Radio" }, // LR102
	{ 0x5018153b, BTTV_TERRATVALUE,   "Terratec TValue" }, // ??
	{ 0x400015b0, BTTV_ZOLTRIX_GENIE, "Zoltrix Genie TV" },
	{ 0x400a15b0, BTTV_ZOLTRIX_GENIE, "Zoltrix Genie TV" },
	{ 0x400d15b0, BTTV_ZOLTRIX_GENIE, "Zoltrix Genie TV / Radio" },
	{ 0x401015b0, BTTV_ZOLTRIX_GENIE, "Zoltrix Genie TV / Radio" },
	{ 0x401615b0, BTTV_ZOLTRIX_GENIE, "Zoltrix Genie TV / Radio" },
	{ 0x1430aa00, BTTV_PV143,         "Provideo PV143A" },
	{ 0x1431aa00, BTTV_PV143,         "Provideo PV143B" },
	{ 0x1432aa00, BTTV_PV143,         "Provideo PV143C" },
	{ 0x1433aa00, BTTV_PV143,         "Provideo PV143D" },
	{ 0x1460aa00, BTTV_PV150,         "Provideo PV150A-1" },
	{ 0x1461aa01, BTTV_PV150,         "Provideo PV150A-2" },
	{ 0x1462aa02, BTTV_PV150,         "Provideo PV150A-3" },
	{ 0x1463aa03, BTTV_PV150,         "Provideo PV150A-4" },
	{ 0x1464aa04, BTTV_PV150,         "Provideo PV150B-1" },
	{ 0x1465aa05, BTTV_PV150,         "Provideo PV150B-2" },
	{ 0x1466aa06, BTTV_PV150,         "Provideo PV150B-3" },
	{ 0x1467aa07, BTTV_PV150,         "Provideo PV150B-4" },
	{ 0xa132ff00, BTTV_IVC100,        "IVC-100"  },
	{ 0xa1550000, BTTV_IVC200,        "IVC-200"  },
	{ 0xa1550001, BTTV_IVC200,        "IVC-200"  },
	{ 0xa1550002, BTTV_IVC200,        "IVC-200"  },
	{ 0xa1550003, BTTV_IVC200,        "IVC-200"  },	
	{ 0xa1550100, BTTV_IVC200,        "IVC-200G" },
	{ 0xa1550101, BTTV_IVC200,        "IVC-200G" },
	{ 0xa1550102, BTTV_IVC200,        "IVC-200G" },
	{ 0xa1550103, BTTV_IVC200,        "IVC-200G" },
	{ 0xa182ff00, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff01, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff02, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff03, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff04, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff05, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff06, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff07, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff08, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff09, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff0a, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff0b, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff0c, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff0d, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff0e, BTTV_IVC120,        "IVC-120G" },
	{ 0xa182ff0f, BTTV_IVC120,        "IVC-120G" },
	{ 0x41424344, BTTV_GRANDTEC,      "GrandTec Multi Capture" },
	{ 0x01020304, BTTV_XGUARD,        "Grandtec Grand X-Guard" },
    	{ 0x010115cb, BTTV_GMV1,          "AG GMV1" },
	{ 0x010114c7, BTTV_MODTEC_205,    "Modular Technology MM201/MM202/MM205/MM210/MM215 PCTV" },
	{ 0x18501851, BTTV_CHRONOS_VS2,   "FlyVideo 98 (LR50)/ Chronos Video Shuttle II" },
	{ 0x18511851, BTTV_FLYVIDEO98EZ,  "FlyVideo 98EZ (LR51)/ CyberMail AV" },
	{ 0x18521852, BTTV_TYPHOON_TVIEW, "FlyVideo 98FM (LR50)/ Typhoon TView TV/FM Tuner" },
	{ 0x10b42636, BTTV_HAUPPAUGE878,  "STB ???" },
	{ 0x217d6606, BTTV_WINFAST2000,   "Leadtek WinFast TV 2000" },
	{ 0x03116000, BTTV_SENSORAY311,   "Sensoray 311" },
	{ 0x00790e11, BTTV_WINDVR,        "Canopus WinDVR PCI" },
	{ 0xa0fca1a0, BTTV_ZOLTRIX,       "Face to Face Tvmax" },
	{ 0x01010071, BTTV_NEBULA_DIGITV, "Nebula Electronics DigiTV" },
	{ 0x200A1295, BTTV_PXC200,	  "Imagenation PXC200"},   
	{ 0, -1, NULL }
};

/* ----------------------------------------------------------------------- */
/* array with description for bt848 / bt878 tv/grabber cards               */

struct tvcard bttv_tvcards[] = {
{
/* ---- card 0x00 ---------------------------------- */
	.name		= " *** UNKNOWN/GENERIC *** ",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.muxsel		= { 2, 3, 1, 0},
	.tuner_type	= -1,
},{
	.name		= "MIRO PCTV",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 15,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 2, 0, 0, 0, 10},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{
	.name		= "Hauppauge (bt848)",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 7,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 1, 2, 3, 4},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{
	.name		= "STB, Gateway P/N 6000699 (bt848)",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 7,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 4, 0, 2, 3, 1},
	.no_msp34xx	= 1,
	.needs_tvaudio	= 1,
	.tuner_type     = TUNER_PHILIPS_NTSC,
	.pll            = PLL_28,
	.has_radio      = 1,
},{

/* ---- card 0x04 ---------------------------------- */
	.name		= "Intel Create and Share PCI/ Smart Video Recorder III",
	.video_inputs	= 4,
	.audio_inputs	= 0,
	.tuner		= -1,
	.svhs		= 2,
	.gpiomask	= 0,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0 },
	.needs_tvaudio	= 0,
	.tuner_type	= 4,
},{
	.name		= "Diamond DTV2000",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 3,
	.muxsel		= { 2, 3, 1, 0},
	.audiomux	= { 0, 1, 0, 1, 3},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{
	.name		= "AVerMedia TVPhone",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 3,
	.muxsel		= { 2, 3, 1, 1},
	.gpiomask	= 0x0f,
	.audiomux	= { 0x0c, 0x04, 0x08, 0x04, 0},
	/*                0x04 for some cards ?? */
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
	.audio_hook	= avermedia_tvphone_audio,
},{
	.name		= "MATRIX-Vision MV-Delta",
	.video_inputs	= 5,
	.audio_inputs	= 1,
	.tuner		= -1,
	.svhs		= 3,
	.gpiomask	= 0,
	.muxsel		= { 2, 3, 1, 0, 0},
	.audiomux	= {0 },
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{

/* ---- card 0x08 ---------------------------------- */
	.name		= "Lifeview FlyVideo II (Bt848) LR26 / MAXI TV Video PCI2 LR26",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xc00,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 0xc00, 0x800, 0x400, 0xc00, 0},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name		= "IMS/IXmicro TurboTV",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 3,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 1, 1, 2, 3, 0},
	.needs_tvaudio	= 0,
	.pll		= PLL_28,
	.tuner_type	= TUNER_TEMIC_PAL,
},{
	.name		= "Hauppauge (bt878)",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x0f, /* old: 7 */
	.muxsel		= { 2, 0, 1, 1},
	.audiomux	= { 0, 1, 2, 3, 4},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name		= "MIRO PCTV pro",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x3014f,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0x20001,0x10001, 0, 0,10},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{

/* ---- card 0x0c ---------------------------------- */
	.name		= "ADS Technologies Channel Surfer TV (bt848)",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 15,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 13, 14, 11, 7, 0, 0},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{
	.name		= "AVerMedia TVCapture 98",
	.video_inputs	= 3,
	.audio_inputs	= 4,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 15,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 13, 14, 11, 7, 0, 0},
	.needs_tvaudio	= 1,
	.msp34xx_alt    = 1,
	.pll		= PLL_28,
	.tuner_type	= TUNER_PHILIPS_PAL,
},{
	.name		= "Aimslab Video Highway Xtreme (VHX)",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 7,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 2, 1, 3, 4}, /* old: { 0, 1, 2, 3, 4} */
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name		= "Zoltrix TV-Max",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 15,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= {0 , 0, 1 , 0, 10},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{

/* ---- card 0x10 ---------------------------------- */
	.name		= "Prolink Pixelview PlayTV (bt878)",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x01fe00,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0x01c000, 0, 0x018000, 0x014000, 0x002000, 0 },
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name		= "Leadtek WinView 601",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x8300f8,
	.muxsel		= { 2, 3, 1, 1,0},
	.audiomux	= { 0x4fa007,0xcfa007,0xcfa007,0xcfa007,0xcfa007,0xcfa007},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
	.audio_hook	= winview_audio,
	.has_radio	= 1,
},{
	.name		= "AVEC Intercapture",
	.video_inputs	= 3,
	.audio_inputs	= 2,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0,
	.muxsel		= {2, 3, 1, 1},
	.audiomux	= {1, 0, 0, 0, 0},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{
	.name		= "Lifeview FlyVideo II EZ /FlyKit LR38 Bt848 (capture only)",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= -1,
	.svhs		= -1,
	.gpiomask	= 0x8dff00,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0 },
	.no_msp34xx	= 1,
	.tuner_type	= -1,
},{

/* ---- card 0x14 ---------------------------------- */
	.name		= "CEI Raffles Card",
	.video_inputs	= 3,
	.audio_inputs	= 3,
	.tuner		= 0,
	.svhs		= 2,
	.muxsel		= {2, 3, 1, 1},
	.tuner_type	= -1,
},{
	.name		= "Lifeview FlyVideo 98/ Lucky Star Image World ConferenceTV LR50",
	.video_inputs	= 4,
	.audio_inputs	= 2,  // tuner, line in
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x1800,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 0x800, 0x1000, 0x1000, 0x1800},
	.pll		= PLL_28,
	.tuner_type	= TUNER_PHILIPS_PAL_I,
},{
	.name		= "Askey CPH050/ Phoebe Tv Master + FM",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xc00,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= {0, 1, 0x800, 0x400, 0xc00, 0},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name		= "Modular Technology MM201/MM202/MM205/MM210/MM215 PCTV, bt878",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= -1,
	.gpiomask	= 7,
	.muxsel		= { 2, 3, -1 },
        .digital_mode   = DIGITAL_MODE_CAMERA,
	.audiomux	= { 0, 0, 0, 0, 0 },
	.no_msp34xx	= 1,
	.pll            = PLL_28,
	.tuner_type     = TUNER_ALPS_TSBB5_PAL_I,
},{

/* ---- card 0x18 ---------------------------------- */
	.name		= "Askey CPH05X/06X (bt878) [many vendors]",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xe00,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= {0x400, 0x400, 0x400, 0x400, 0xc00},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name           = "Terratec TerraTV+ Version 1.0 (Bt848)/ Terra TValue Version 1.0/ Vobis TV-Boostar",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask       = 0x1f0fff,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux       = { 0x20000, 0x30000, 0x10000, 0, 0x40000},
	.needs_tvaudio	= 0,
	.tuner_type	= TUNER_PHILIPS_PAL,
	.audio_hook     = terratv_audio,
},{
	.name		= "Hauppauge WinCam newer (bt878)",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 3,
	.gpiomask	= 7,
	.muxsel		= { 2, 0, 1, 1},
	.audiomux	= { 0, 1, 2, 3, 4},
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{
	.name		= "Lifeview FlyVideo 98/ MAXI TV Video PCI2 LR50",
	.video_inputs	= 4,
	.audio_inputs	= 2,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x1800,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 0x800, 0x1000, 0x1000, 0x1800},
	.pll            = PLL_28,
	.tuner_type	= TUNER_PHILIPS_SECAM,
},{

/* ---- card 0x1c ---------------------------------- */
	.name           = "Terratec TerraTV+ Version 1.1 (bt878)",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x1f0fff,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0x20000, 0x30000, 0x10000, 0x00000, 0x40000},
	.needs_tvaudio	= 0,
	.tuner_type	= TUNER_PHILIPS_PAL,
	.audio_hook	= terratv_audio,
	/* GPIO wiring:
               External 20 pin connector (for Active Radio Upgrade board)
               gpio00: i2c-sda
               gpio01: i2c-scl
               gpio02: om5610-data
               gpio03: om5610-clk
               gpio04: om5610-wre
               gpio05: om5610-stereo
               gpio06: rds6588-davn
               gpio07: Pin 7 n.c.
               gpio08: nIOW
               gpio09+10: nIOR, nSEL ?? (bt878)
                gpio09: nIOR (bt848)
                gpio10: nSEL (bt848)
              Sound Routing:
               gpio16: u2-A0 (1st 4052bt)
               gpio17: u2-A1
               gpio18: u2-nEN
               gpio19: u4-A0 (2nd 4052)
               gpio20: u4-A1
                       u4-nEN - GND
	    Btspy:
	  	00000 : Cdrom (internal audio input)
		10000 : ext. Video audio input
		20000 : TV Mono
		a0000 : TV Mono/2
	       1a0000 : TV Stereo	
		30000 : Radio
		40000 : Mute
       */

},{
	/* Jannik Fritsch <jannik@techfak.uni-bielefeld.de> */
	.name		= "Imagenation PXC200",
	.video_inputs	= 5,
	.audio_inputs	= 1,
	.tuner		= -1,
	.svhs		= 1, /* was: 4 */
	.gpiomask	= 0,
	.muxsel		= { 2, 3, 1, 0, 0},
	.audiomux	= { 0 },
	.needs_tvaudio	= 1,
	.tuner_type	= -1,
},{
	.name		= "Lifeview FlyVideo 98 LR50",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x1800,  //0x8dfe00
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 0x0800, 0x1000, 0x1000, 0x1800, 0 },
	.pll            = PLL_28,
	.tuner_type	= -1,
},{
	.name		= "Formac iProTV, Formac ProTV I (bt848)",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 3,
	.gpiomask	= 1,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 1, 0, 0, 0, 0 },
	.pll            = PLL_28,
	.tuner_type	= TUNER_PHILIPS_PAL,
},{

/* ---- card 0x20 ---------------------------------- */
	.name		= "Intel Create and Share PCI/ Smart Video Recorder III",
	.video_inputs	= 4,
	.audio_inputs	= 0,
	.tuner		= -1,
	.svhs		= 2,
	.gpiomask	= 0,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0 },
	.needs_tvaudio	= 0,
	.tuner_type	= 4,
},{
	.name           = "Terratec TerraTValue Version Bt878",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xffff00,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0x500, 0, 0x300, 0x900, 0x900},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= TUNER_PHILIPS_PAL,
},{
	.name		= "Leadtek WinFast 2000/ WinFast 2000 XP",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xc33000,
	.muxsel		= { 2, 3, 1, 1, 0}, // TV, CVid, SVid, CVid over SVid connector
	.audiomux	= { 0x422000,0x1000,0x0000,0x620000,0x800000},
	/* Audio Routing for "WinFast 2000 XP" (no tv stereo !)
		gpio23 -- hef4052:nEnable (0x800000)
		gpio12 -- hef4052:A1
	        gpio13 -- hef4052:A0
	    0x0000: external audio
	    0x1000: FM
	    0x2000: TV
	    0x3000: n.c.
          Note: There exists another variant "Winfast 2000" with tv stereo !?
	  Note: eeprom only contains FF and pci subsystem id 107d:6606
	 */
	.needs_tvaudio	= 0,
	.pll		= PLL_28,
	.has_radio	= 1,
	.tuner_type	= 5, // default for now, gpio reads BFFF06 for Pal bg+dk
	.audio_hook	= winfast2000_audio,
},{
	.name		= "Lifeview FlyVideo 98 LR50 / Chronos Video Shuttle II",
	.video_inputs	= 4,
	.audio_inputs	= 3,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x1800,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 0x800, 0x1000, 0x1000, 0x1800},
	.pll		= PLL_28,
	.tuner_type	= -1,
},{

/* ---- card 0x24 ---------------------------------- */
	.name		= "Lifeview FlyVideo 98FM LR50 / Typhoon TView TV/FM Tuner",
	.video_inputs	= 4,
	.audio_inputs	= 3,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x1800,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 0x800, 0x1000, 0x1000, 0x1800, 0 },
	.pll		= PLL_28,
	.tuner_type	= -1,
	.has_radio	= 1,
},{
	.name		= "Prolink PixelView PlayTV pro",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xff,
	.muxsel		= { 2, 3, 1, 1 },
	.audiomux	= { 0x21, 0x20, 0x24, 0x2c, 0x29, 0x29 },
	.no_msp34xx	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name		= "Askey CPH06X TView99",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x551e00,
	.muxsel		= { 2, 3, 1, 0},
	.audiomux	= { 0x551400, 0x551200, 0, 0, 0x551c00, 0x551200 },
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= 1,
},{
	.name		= "Pinnacle PCTV Studio/Rave",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x03000F,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 2, 0, 0, 0, 1},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{

/* ---- card 0x28 ---------------------------------- */
	.name		= "STB TV PCI FM, Gateway P/N 6000704 (bt878), 3Dfx VoodooTV 100",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 7,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 4, 0, 2, 3, 1},
	.no_msp34xx	= 1,
	.needs_tvaudio	= 1,
	.tuner_type     = TUNER_PHILIPS_NTSC,
	.pll            = PLL_28,
	.has_radio      = 1,
},{
	.name		= "AVerMedia TVPhone 98",
	.video_inputs	= 3,
	.audio_inputs	= 4,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 15,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 13, 4, 11, 7, 0, 0},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
	.has_radio	= 1,
	.audio_hook	= avermedia_tvphone_audio,
},{
	.name		= "ProVideo PV951", /* pic16c54 */
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 0, 0, 0, 0},
	.needs_tvaudio	= 1,
	.no_msp34xx	= 1,
	.pll		= PLL_28,
	.tuner_type	= 1,
},{
	.name		= "Little OnAir TV",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xe00b,
	.muxsel		= {2, 3, 1, 1},
	.audiomux	= {0xff9ff6, 0xff9ff6, 0xff1ff7, 0, 0xff3ffc},
	.no_msp34xx	= 1,
	.tuner_type	= -1,
},{

/* ---- card 0x2c ---------------------------------- */
	.name		= "Sigma TVII-FM",
	.video_inputs	= 2,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= -1,
	.gpiomask	= 3,
	.muxsel		= {2, 3, 1, 1},
	.audiomux	= {1, 1, 0, 2, 3},
	.no_msp34xx	= 1,
	.pll		= PLL_NONE,
	.tuner_type	= -1,
},{
	.name		= "MATRIX-Vision MV-Delta 2",
	.video_inputs	= 5,
	.audio_inputs	= 1,
	.tuner		= -1,
	.svhs		= 3,
	.gpiomask	= 0,
	.muxsel		= { 2, 3, 1, 0, 0},
	.audiomux	= {0 },
	.no_msp34xx	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name		= "Zoltrix Genie TV/FM",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xbcf03f,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0xbc803f, 0xbc903f, 0xbcb03f, 0, 0xbcb03f},
	.no_msp34xx	= 1,
	.pll		= PLL_28,
	.tuner_type	= 21,
},{
	.name		= "Terratec TV/Radio+",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x70000,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0x20000, 0x30000, 0x10000, 0, 0x40000, 0x20000 },
	.needs_tvaudio	= 1,
	.no_msp34xx	= 1,
	.pll		= PLL_35,
	.tuner_type	= 1,
	.has_radio	= 1,
},{

/* ---- card 0x30 ---------------------------------- */
	.name		= "Askey CPH03x/ Dynalink Magic TView",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 15,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= {2,0,0,0,1},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	.name		= "IODATA GV-BCTV3/PCI",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x010f00,
	.muxsel		= {2, 3, 0, 0},
	.audiomux	= {0x10000, 0, 0x10000, 0, 0, 0},
	.no_msp34xx	= 1,
	.pll		= PLL_28,
	.tuner_type	= TUNER_ALPS_TSHC6_NTSC,
	.audio_hook	= gvbctv3pci_audio,
},{
	.name		= "Prolink PV-BT878P+4E / PixelView PlayTV PAK / Lenco MXTV-9578 CP",
	.video_inputs	= 5,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 3,
	.gpiomask	= 0xAA0000,
	.muxsel		= { 2,3,1,1,-1 },
	.digital_mode   = DIGITAL_MODE_CAMERA,
	.audiomux	= { 0x20000, 0, 0x80000, 0x80000, 0xa8000, 0x46000  },
	.no_msp34xx	= 1,
	.pll		= PLL_28,
	.tuner_type	= TUNER_PHILIPS_PAL_I,
	/* GPIO wiring: (different from Rev.4C !)
		GPIO17: U4.A0 (first hef4052bt)
		GPIO19: U4.A1
		GPIO20: U5.A1 (second hef4052bt)
		GPIO21: U4.nEN
		GPIO22: BT832 Reset Line
		GPIO23: A5,A0, U5,nEN
	   Note: At i2c=0x8a is a Bt832 chip, which changes to 0x88 after being reset via GPIO22
	 */
},{
	.name           = "Eagle Wireless Capricorn2 (bt878A)",
	.video_inputs   = 4,
	.audio_inputs   = 1,
	.tuner          = 0,
	.svhs           = 2,
	.gpiomask       = 7,
	.muxsel         = { 2, 0, 1, 1},
	.audiomux       = { 0, 1, 2, 3, 4},
	.pll            = PLL_28,
	.tuner_type     = -1 /* TUNER_ALPS_TMDH2_NTSC */,
},{

/* ---- card 0x34 ---------------------------------- */
	/* David H�rdeman <david@2gen.com> */
	.name           = "Pinnacle PCTV Studio Pro",
	.video_inputs   = 4,
	.audio_inputs   = 1,
	.tuner          = 0,
	.svhs           = 3,
	.gpiomask       = 0x03000F,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 1, 0xd0001, 0, 0, 10},
			/* sound path (5 sources):
			   MUX1 (mask 0x03), Enable Pin 0x08 (0=enable, 1=disable)
				0= ext. Audio IN
				1= from MUX2
				2= Mono TV sound from Tuner
				3= not connected
			   MUX2 (mask 0x30000):
				0,2,3= from MSP34xx
				1= FM stereo Radio from Tuner */
	.needs_tvaudio  = 1,
	.pll            = PLL_28,
	.tuner_type     = -1,
},{
	/* Claas Langbehn <claas@bigfoot.com>,
	   Sven Grothklags <sven@upb.de> */
	.name		= "Typhoon TView RDS + FM Stereo / KNC1 TV Station RDS",
	.video_inputs	= 3,
	.audio_inputs	= 3,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x1c,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0, 0, 0x10, 8, 4 },
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= TUNER_PHILIPS_PAL_I,
	.has_radio	= 1,
},{
	/* Tim R�stermundt <rosterm@uni-muenster.de>
	   in de.comp.os.unix.linux.hardware:
		options bttv card=0 pll=1 radio=1 gpiomask=0x18e0
		audiomux=0x44c71f,0x44d71f,0,0x44d71f,0x44dfff
		options tuner type=5 */
	.name		= "Lifeview FlyVideo 2000 /FlyVideo A2/ Lifetec LT 9415 TV [LR90]",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x18e0,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0x0000,0x0800,0x1000,0x1000,0x18e0 },
		       /* For cards with tda9820/tda9821:
			  0x0000: Tuner normal stereo
			  0x0080: Tuner A2 SAP (second audio program = Zweikanalton)
			  0x0880: Tuner A2 stereo */
	.pll		= PLL_28,
	.tuner_type	= -1,
},{
	/* Miguel Angel Alvarez <maacruz@navegalia.com>
	   old Easy TV BT848 version (model CPH031) */
	.name           = "Askey CPH031/ BESTBUY Easy TV",
	.video_inputs	= 4,
	.audio_inputs   = 1,
	.tuner          = 0,
	.svhs           = 2,
	.gpiomask       = 0xF,
	.muxsel         = { 2, 3, 1, 0},
	.audiomux       = { 2, 0, 0, 0, 10},
	.needs_tvaudio  = 0,
	.pll		= PLL_28,
	.tuner_type	= TUNER_TEMIC_PAL,
},{

/* ---- card 0x38 ---------------------------------- */
	/* Gordon Heydon <gjheydon@bigfoot.com ('98) */
	.name           = "Lifeview FlyVideo 98FM LR50",
	.video_inputs   = 4,
	.audio_inputs   = 3,
	.tuner          = 0,
	.svhs           = 2,
	.gpiomask       = 0x1800,
	.muxsel         = { 2, 3, 1, 1},
	.audiomux       = { 0, 0x800, 0x1000, 0x1000, 0x1800, 0 },
	.pll            = PLL_28,
	.tuner_type     = 5,
},{
	/* This is the ultimate cheapo capture card 
	 * just a BT848A on a small PCB!
	 * Steve Hosgood <steve@equiinet.com> */
	.name           = "GrandTec 'Grand Video Capture' (Bt848)",
	.video_inputs   = 2,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = 1,
	.gpiomask       = 0,
	.muxsel         = { 3, 1 },
	.audiomux       = { 0 },
	.needs_tvaudio  = 0,
	.no_msp34xx     = 1,
	.pll            = PLL_35,
	.tuner_type     = -1,
},{
        /* Daniel Herrington <daniel.herrington@home.com> */
        .name           = "Askey CPH060/ Phoebe TV Master Only (No FM)",
        .video_inputs   = 3,
        .audio_inputs   = 1,
        .tuner          = 0,
        .svhs           = 2,
        .gpiomask       = 0xe00,
        .muxsel         = { 2, 3, 1, 1},
        .audiomux       = { 0x400, 0x400, 0x400, 0x400, 0x800, 0x400 },
        .needs_tvaudio  = 1,
        .pll            = PLL_28,
        .tuner_type     = TUNER_TEMIC_4036FY5_NTSC,
},{
	/* Matti Mottus <mottus@physic.ut.ee> */
	.name		= "Askey CPH03x TV Capturer",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
        .gpiomask       = 0x03000F,
	.muxsel		= { 2, 3, 1, 0},
        .audiomux       = { 2,0,0,0,1 },
	.pll            = PLL_28,
	.tuner_type	= 0,
},{

/* ---- card 0x3c ---------------------------------- */
	/* Philip Blundell <philb@gnu.org> */
	.name           = "Modular Technology MM100PCTV",
	.video_inputs   = 2,
	.audio_inputs   = 2,
	.tuner		= 0,
	.svhs		= -1,
	.gpiomask       = 11,
	.muxsel         = { 2, 3, 1, 1},
	.audiomux       = { 2, 0, 0, 1, 8},
	.pll            = PLL_35,
	.tuner_type     = TUNER_TEMIC_PAL,

},{
	/* Adrian Cox <adrian@humboldt.co.uk */
	.name	        = "AG Electronics GMV1",
	.video_inputs   = 2,
	.audio_inputs   = 0,
	.tuner	        = -1,
	.svhs	        = 1,
	.gpiomask       = 0xF,
	.muxsel	        = { 2, 2},
	.audiomux       = { },
	.no_msp34xx     = 1,
	.needs_tvaudio  = 0,
	.pll	        = PLL_28,
	.tuner_type     = -1,
},{
	/* Miguel Angel Alvarez <maacruz@navegalia.com>
	   new Easy TV BT878 version (model CPH061) 
	   special thanks to Informatica Mieres for providing the card */
	.name           = "Askey CPH061/ BESTBUY Easy TV (bt878)",
	.video_inputs	= 3,
	.audio_inputs   = 2,
	.tuner          = 0,
	.svhs           = 2,
	.gpiomask       = 0xFF,
	.muxsel         = { 2, 3, 1, 0},
	.audiomux       = { 1, 0, 4, 4, 9},
	.needs_tvaudio  = 0,
	.pll		= PLL_28,
	.tuner_type	= TUNER_PHILIPS_PAL,
},{
	/* Lukas Gebauer <geby@volny.cz> */
	.name		= "ATI TV-Wonder",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xf03f,
	.muxsel		= { 2, 3, 1, 0 },
	.audiomux	= { 0xbffe, 0, 0xbfff, 0, 0xbffe},
	.pll		= PLL_28,
	.tuner_type	= TUNER_TEMIC_4006FN5_MULTI_PAL,
},{

/* ---- card 0x40 ---------------------------------- */
	/* Lukas Gebauer <geby@volny.cz> */
	.name		= "ATI TV-Wonder VE",
	.video_inputs	= 2,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= -1,
	.gpiomask	= 1,
	.muxsel		= { 2, 3, 0, 1},
	.audiomux	= { 0, 0, 1, 0, 0},
	.no_msp34xx	= 1,
	.pll		= PLL_28,
	.tuner_type	= TUNER_TEMIC_4006FN5_MULTI_PAL,
},{
	/* DeeJay <deejay@westel900.net (2000S) */
	.name           = "Lifeview FlyVideo 2000S LR90",
	.video_inputs   = 3,
	.audio_inputs   = 3,
	.tuner          = 0,
	.svhs           = 2,
	.gpiomask	= 0x18e0,
	.muxsel		= { 2, 3, 0, 1},
			/* Radio changed from 1e80 to 0x800 to make
			   FlyVideo2000S in .hu happy (gm)*/
			/* -dk-???: set mute=0x1800 for tda9874h daughterboard */
	.audiomux	= { 0x0000,0x0800,0x1000,0x1000,0x1800, 0x1080 },
	.audio_hook	= fv2000s_audio,
	.no_msp34xx	= 1,
	.no_tda9875	= 1,
	.needs_tvaudio  = 1,
	.pll            = PLL_28,
	.tuner_type     = 5,
},{
	.name		= "Terratec TValueRadio",
	.video_inputs	= 3,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0xffff00,
	.muxsel		= { 2, 3, 1, 1},
	.audiomux	= { 0x500, 0x500, 0x300, 0x900, 0x900},
	.needs_tvaudio	= 1,
	.pll		= PLL_28,
	.tuner_type	= TUNER_PHILIPS_PAL,
	.has_radio	= 1,
},{
	/* TANAKA Kei <peg00625@nifty.com> */
	.name           = "IODATA GV-BCTV4/PCI",
	.video_inputs   = 3,
	.audio_inputs   = 1,
	.tuner          = 0,
	.svhs           = 2,
	.gpiomask       = 0x010f00,
	.muxsel         = {2, 3, 0, 0},
	.audiomux       = {0x10000, 0, 0x10000, 0, 0, 0},
	.no_msp34xx     = 1,
	.pll            = PLL_28,
	.tuner_type     = TUNER_SHARP_2U5JF5540_NTSC,
	.audio_hook     = gvbctv3pci_audio,
},{

/* ---- card 0x44 ---------------------------------- */
        .name           = "3Dfx VoodooTV FM (Euro), VoodooTV 200 (USA)",
	// try "insmod msp3400 simple=0" if you have
	// sound problems with this card.
        .video_inputs   = 4,
        .audio_inputs   = 1,
        .tuner          = 0,
        .svhs           = -1,
        .gpiomask       = 0x4f8a00,
	// 0x100000: 1=MSP enabled (0=disable again)
	// 0x010000: Connected to "S0" on tda9880 (0=Pal/BG, 1=NTSC)
        .audiomux       = {0x947fff, 0x987fff,0x947fff,0x947fff, 0x947fff},
	// tvtuner, radio,   external,internal, mute,  stereo
	/* tuner, Composit, SVid, Composit-on-Svid-adapter*/
        .muxsel         = { 2, 3 ,0 ,1},
        .tuner_type     = TUNER_MT2032,
	.pll		= PLL_28,
	.has_radio	= 1,
},{
	/* Philip Blundell <pb@nexus.co.uk> */
	.name           = "Active Imaging AIMMS",
	.video_inputs   = 1,
	.audio_inputs   = 0,
	.tuner          = -1,
	.tuner_type     = -1,
	.pll            = PLL_28,
	.muxsel         = { 2 },
	.gpiomask       = 0
},{
        /* Tomasz Pyra <hellfire@sedez.iq.pl> */
        .name           = "Prolink Pixelview PV-BT878P+ (Rev.4C,8E)",
        .video_inputs   = 3,
        .audio_inputs   = 4,
        .tuner          = 0,
        .svhs           = 2,
        .gpiomask       = 15,
        .muxsel         = { 2, 3, 1, 1},
        .audiomux       = { 0, 0, 11, 7, 13, 0}, // TV and Radio with same GPIO !
        .needs_tvaudio  = 1,
        .pll            = PLL_28,
        .tuner_type     = 25,
	/* GPIO wiring:
		GPIO0: U4.A0 (hef4052bt)
		GPIO1: U4.A1
		GPIO2: U4.A1 (second hef4052bt)
		GPIO3: U4.nEN, U5.A0, A5.nEN
		GPIO8-15: vrd866b ?
	 */
},{
	.name		= "Lifeview FlyVideo 98EZ (capture only) LR51",
	.video_inputs	= 4,
	.audio_inputs   = 0,
	.tuner		= -1,
	.svhs		= 2,
	.muxsel		= { 2, 3, 1, 1}, // AV1, AV2, SVHS, CVid adapter on SVHS
	.pll		= PLL_28,
	.no_msp34xx	= 1,
},{

/* ---- card 0x48 ---------------------------------- */
	/* Dariusz Kowalewski <darekk@automex.pl> */
	.name		= "Prolink Pixelview PV-BT878P+9B (PlayTV Pro rev.9B FM+NICAM)",
	.video_inputs	= 4,
	.audio_inputs	= 1,
	.tuner		= 0,
	.svhs		= 2,
	.gpiomask	= 0x3f,
	.muxsel		= { 2, 3, 1, 1 },
	.audiomux	= { 0x01, 0x00, 0x03, 0x03, 0x09, 0x02 },
	.needs_tvaudio  = 1,
	.no_msp34xx	= 1,
	.no_tda9875	= 1,
	.pll		= PLL_28,
	.tuner_type	= 5,
	.audio_hook	= pvbt878p9b_audio, // Note: not all cards have stereo
	.has_radio	= 1,  // Note: not all cards have radio
	/* GPIO wiring:
		GPIO0: A0 hef4052
		GPIO1: A1 hef4052
		GPIO3: nEN hef4052
		GPIO8-15: vrd866b
		GPIO20,22,23: R30,R29,R28
	 */
},{
	/* Clay Kunz <ckunz@mail.arc.nasa.gov> */
	/* you must jumper JP5 for the card to work */
	.name           = "Sensoray 311",
	.video_inputs   = 5,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = 4,
	.gpiomask       = 0,
	.muxsel         = { 2, 3, 1, 0, 0},
	.audiomux       = { 0 },
	.needs_tvaudio  = 0,
	.tuner_type     = -1,
},{
	/* Miguel Freitas <miguel@cetuc.puc-rio.br> */
	.name           = "RemoteVision MX (RV605)",
	.video_inputs   = 16,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = -1,
	.gpiomask       = 0x00,
	.gpiomask2      = 0x07ff,
	.muxsel         = { 0x33, 0x13, 0x23, 0x43, 0xf3, 0x73, 0xe3, 0x03,
			  0xd3, 0xb3, 0xc3, 0x63, 0x93, 0x53, 0x83, 0xa3 },
	.no_msp34xx     = 1,
	.no_tda9875     = 1,
	.tuner_type     = -1,
	.muxsel_hook    = rv605_muxsel,
},{
        .name           = "Powercolor MTV878/ MTV878R/ MTV878F",
        .video_inputs   = 3,
        .audio_inputs   = 2, 
	.tuner		= 0,
        .svhs           = 2,
        .gpiomask       = 0x1C800F,  // Bit0-2: Audio select, 8-12:remote control 14:remote valid 15:remote reset
        .muxsel         = { 2, 1, 1, },
        .audiomux       = { 0, 1, 2, 2, 4 },
        .needs_tvaudio  = 0,
        .tuner_type     = TUNER_PHILIPS_PAL,
	.pll		= PLL_28,
	.has_radio	= 1,
},{

/* ---- card 0x4c ---------------------------------- */
        /* Masaki Suzuki <masaki@btree.org> */
        .name           = "Canopus WinDVR PCI (COMPAQ Presario 3524JP, 5112JP)",
        .video_inputs   = 3,
        .audio_inputs   = 1,
        .tuner          = 0,
        .svhs           = 2,
        .gpiomask       = 0x140007,
        .muxsel         = { 2, 3, 1, 1 },
        .audiomux       = { 0, 1, 2, 3, 4, 0 },
        .tuner_type     = TUNER_PHILIPS_NTSC,
        .audio_hook     = windvr_audio,
},{
        .name           = "GrandTec Multi Capture Card (Bt878)",
        .video_inputs   = 4,
        .audio_inputs   = 0,
        .tuner          = -1,
        .svhs           = -1,
        .gpiomask       = 0,
        .muxsel         = { 2, 3, 1, 0 },
        .audiomux       = { 0 },
        .needs_tvaudio  = 0,
        .no_msp34xx     = 1,
        .pll            = PLL_28,
        .tuner_type     = -1,
},{
        .name           = "Jetway TV/Capture JW-TV878-FBK, Kworld KW-TV878RF",
        .video_inputs   = 4,
        .audio_inputs   = 3,
        .tuner          = 0,
        .svhs           = 2,
        .gpiomask       = 7,
        .muxsel         = { 2, 3, 1, 1 },   // Tuner, SVid, SVHS, SVid to SVHS connector
        .audiomux       = { 0 ,0 ,4, 4,4,4},// Yes, this tuner uses the same audio output for TV and FM radio!
					  // This card lacks external Audio In, so we mute it on Ext. & Int.
					  // The PCB can take a sbx1637/sbx1673, wiring unknown.
					  // This card lacks PCI subsystem ID, sigh.
					  // audiomux=1: lower volume, 2+3: mute
					  // btwincap uses 0x80000/0x80003
        .needs_tvaudio  = 0,
        .no_msp34xx     = 1,
        .pll            = PLL_28,
        .tuner_type     = 5, // Samsung TCPA9095PC27A (BG+DK), philips compatible, w/FM, stereo and
			   // radio signal strength indicators work fine.
	.has_radio		= 1,
	/* GPIO Info:
		GPIO0,1:   HEF4052 A0,A1
		GPIO2:     HEF4052 nENABLE
		GPIO3-7:   n.c.
		GPIO8-13:  IRDC357 data0-5 (data6 n.c. ?) [chip not present on my card]
		GPIO14,15: ??
		GPIO16-21: n.c.
		GPIO22,23: ??
		??       : mtu8b56ep microcontroller for IR (GPIO wiring unknown)*/
},{
        /* Arthur Tetzlaff-Deas, DSP Design Ltd <software@dspdesign.com> */
        .name           = "DSP Design TCVIDEO",
        .video_inputs   = 4,
        .svhs           = -1,
        .muxsel         = { 2, 3, 1, 0},
        .pll            = PLL_28,
        .tuner_type     = -1,
},{

        /* ---- card 0x50 ---------------------------------- */
	.name           = "Hauppauge WinTV PVR",
        .video_inputs   = 4,
        .audio_inputs   = 1,
        .tuner          = 0,
        .svhs           = 2,
        .muxsel         = { 2, 0, 1, 1},
        .needs_tvaudio  = 1,
        .pll            = PLL_28,
        .tuner_type     = -1,

	.gpiomask       = 7,
	.audiomux       = {7},
},{
	.name           = "IODATA GV-BCTV5/PCI",
	.video_inputs   = 3,
	.audio_inputs   = 1,
	.tuner          = 0,
	.svhs           = 2,
	.gpiomask       = 0x0f0f80,
	.muxsel         = {2, 3, 1, 0},
	.audiomux       = {0x030000, 0x010000, 0x030000, 0, 0x020000, 0},
	.no_msp34xx     = 1,
	.pll            = PLL_28,
	.tuner_type     = TUNER_PHILIPS_NTSC_M,
	.audio_hook     = gvbctv3pci_audio,
	.has_radio      = 1,
},{
	.name           = "Osprey 100/150 (878)", /* 0x1(2|3)-45C6-C1 */
	.video_inputs   = 4,                  /* id-inputs-clock */
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = 3,
	.muxsel         = { 3, 2, 0, 1 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{
	.name           = "Osprey 100/150 (848)", /* 0x04-54C0-C1 & older boards */
	.video_inputs   = 3,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = 2,
	.muxsel         = { 2, 3, 1 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{

	/* ---- card 0x54 ---------------------------------- */
	.name           = "Osprey 101 (848)", /* 0x05-40C0-C1 */
	.video_inputs   = 2,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = 1,
	.muxsel         = { 3, 1 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{
	.name           = "Osprey 101/151",       /* 0x1(4|5)-0004-C4 */
	.video_inputs   = 1,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = -1,
	.muxsel         = { 0 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{
	.name           = "Osprey 101/151 w/ svid",  /* 0x(16|17|20)-00C4-C1 */
	.video_inputs   = 2,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = 1,
	.muxsel         = { 0, 1 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{
	.name           = "Osprey 200/201/250/251",  /* 0x1(8|9|E|F)-0004-C4 */
	.video_inputs   = 1,
	.audio_inputs   = 1,
	.tuner          = -1,
	.svhs           = -1,
	.muxsel         = { 0 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{
	
	/* ---- card 0x58 ---------------------------------- */
	.name           = "Osprey 200/250",   /* 0x1(A|B)-00C4-C1 */
	.video_inputs   = 2,
	.audio_inputs   = 1,
	.tuner          = -1,
	.svhs           = 1,
	.muxsel         = { 0, 1 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{
	.name           = "Osprey 210/220",   /* 0x1(A|B)-04C0-C1 */
	.video_inputs   = 2,
	.audio_inputs   = 1,
	.tuner          = -1,
	.svhs           = 1,
	.muxsel         = { 2, 3 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{
	.name           = "Osprey 500",   /* 500 */
	.video_inputs   = 2,
	.audio_inputs   = 1,
	.tuner          = -1,
	.svhs           = 1,
	.muxsel         = { 2, 3 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
},{
       .name           = "Osprey 540",   /* 540 */
       .video_inputs   = 4,
       .audio_inputs   = 1,
       .tuner          = -1,
       .pll            = PLL_28,
       .tuner_type     = -1,
       .no_msp34xx     = 1,
       .no_tda9875     = 1,
       .no_tda7432     = 1,
},{

	/* ---- card 0x5C ---------------------------------- */
	.name           = "Osprey 2000",  /* 2000 */
	.video_inputs   = 2,
	.audio_inputs   = 1,
	.tuner          = -1,
	.svhs           = 1,
	.muxsel         = { 2, 3 },
	.pll            = PLL_28,
	.tuner_type     = -1,
        .no_msp34xx     = 1,
        .no_tda9875     = 1,
        .no_tda7432     = 1,      /* must avoid, conflicts with the bt860 */
},{
	/* M G Berberich <berberic@forwiss.uni-passau.de> */
	.name           = "IDS Eagle",
	.video_inputs   = 4,
	.audio_inputs   = 0,
	.tuner          = -1,
	.tuner_type     = -1,
	.svhs           = -1,
	.gpiomask       = 0,
	.muxsel         = { 0, 1, 2, 3 },
	.muxsel_hook    = eagle_muxsel,
	.no_msp34xx     = 1,
	.no_tda9875     = 1,
	.pll            = PLL_28,
},{
	.name           = "Pinnacle PCTV Sat",
	.video_inputs   = 2,
	.audio_inputs   = 0,
	.svhs           = 1,
	.tuner          = -1,
	.tuner_type     = -1,
	.no_msp34xx     = 1,
	.no_tda9875     = 1,
	.no_tda7432     = 1,   
	.gpiomask       = 0x01,
	.audiomux       = { 0, 0, 0, 0, 1 },
	.muxsel         = { 3, 0, 1, 2},
	.needs_tvaudio  = 0, 
	.pll            = PLL_28,
},{
        .name           = "Formac ProTV II (bt878)",
        .video_inputs   = 4,
        .audio_inputs   = 1,
        .tuner          = 0,
        .svhs           = 3,
        .gpiomask       = 2,
	// TV, Comp1, Composite over SVID con, SVID
        .muxsel         = { 2, 3, 1, 1},
        .audiomux       = { 2, 2, 0, 0, 0 }, 
        .pll            = PLL_28,
	.has_radio      = 1,
        .tuner_type     = TUNER_PHILIPS_PAL,
      /* sound routing:
           GPIO=0x00,0x01,0x03: mute (?)
              0x02: both TV and radio (tuner: FM1216/I)
         The card has onboard audio connectors labeled "cdrom" and "board",
	 not soldered here, though unknown wiring.
         Card lacks: external audio in, pci subsystem id.
       */
},{

	/* ---- card 0x60 ---------------------------------- */
	.name           = "MachTV",
        .video_inputs   = 3,
        .audio_inputs   = 1,
        .tuner          = 0,
        .svhs           = -1,
        .gpiomask       = 7,
        .muxsel         = { 2, 3, 1, 1},
        .audiomux       = { 0, 1, 2, 3, 4},
        .needs_tvaudio  = 1,
        .tuner_type     = 5,
	.pll            = 1,
},{
	.name           = "Euresys Picolo",
	.video_inputs   = 3,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = 2,
	.gpiomask       = 0,
	.no_msp34xx     = 1,
	.no_tda9875     = 1,
	.no_tda7432     = 1,
	.muxsel         = { 2, 0, 1},
	.pll            = PLL_28,
},{
	/* Luc Van Hoeylandt <luc@e-magic.be> */
	.name           = "ProVideo PV150", /* 0x4f */
	.video_inputs   = 2,
	.audio_inputs   = 0,
	.tuner          = -1,
	.svhs           = -1,
	.gpiomask       = 0,
	.muxsel         = { 2, 3 },
	.audiomux       = { 0 },
	.needs_tvaudio  = 0,
	.no_msp34xx     = 1,
	.pll            = PLL_28,
	.tuner_type     = -1,
},{
	/* Hiroshi Takekawa <sian@big.or.jp> */
	/* This card lacks subsystem ID */
	.name           = "AD-TVK503", /* 0x63 */
	.video_inputs   = 4,
	.audio_inputs   = 1,
	.tuner          = 0,
	.svhs           = 2,
	.gpiomask       = 0x001e8007,
	.muxsel         = { 2, 3, 1, 0 },
	/*                  Tuner, Radio, external, internal, off,  on */
	.audiomux       = { 0x08,  0x0f,  0x0a,     0x08,     0x0f, 0x08 },
	.needs_tvaudio  = 0,
	.no_msp34xx     = 1,
	.pll            = PLL_28,
	.tuner_type     = 2,
	.audio_hook	= adtvk503_audio,
},{

	/* ---- card 0x64 ---------------------------------- */
        .name           = "Hercules Smart TV Stereo",
        .video_inputs   = 4,
        .audio_inputs   = 1,
        .tuner          = 0,
        .svhs           = 2,
        .gpiomask       = 0x00,
        .muxsel         = { 2, 3, 1, 1 },
        .needs_tvaudio  = 1,
        .no_msp34xx     = 1,
        .pll            = PLL_28,
        .tuner_type     = 5,
	/* Notes:
	   - card lacks subsystem ID
	   - stereo variant w/ daughter board with tda9874a @0xb0
	   - Audio Routing: 
		always from tda9874 independent of GPIO (?)
		external line in: unknown
	   - Other chips: em78p156elp @ 0x96 (probably IR remote control)
	              hef4053 (instead 4052) for unknown function
	*/
},{
        .name           = "Pace TV & Radio Card",
        .video_inputs   = 4,
        .audio_inputs   = 1,
        .tuner          = 0,
        .svhs           = 2,
        .muxsel         = { 2, 3, 1, 1}, // Tuner, CVid, SVid, CVid over SVid connector
        .gpiomask       = 0,
        .no_tda9875     = 1,
        .no_tda7432     = 1,
        .tuner_type     = 1,
        .has_radio      = 1,
        .pll            = PLL_28,
        /* Bt878, Bt832, FI1246 tuner; no pci subsystem id
           only internal line out: (4pin header) RGGL
           Radio must be decoded by msp3410d (not routed through)*/
        //         .digital_mode   = DIGITAL_MODE_CAMERA, // todo!
},{
        /* Chris Willing <chris@vislab.usyd.edu.au> */
        .name           = "IVC-200",
        .video_inputs   = 1,
        .audio_inputs   = 0,
        .tuner          = -1,
        .tuner_type     = -1,
        .svhs           = -1,
        .gpiomask       = 0xdf,
        .muxsel         = { 2 },
        .pll            = PLL_28,
},{
	.name           = "Grand X-Guard / Trust 814PCI",
	.video_inputs   = 16,
        .audio_inputs   = 0,
        .tuner          = -1,
        .svhs           = -1,
	.tuner_type     = 4,
        .gpiomask2      = 0xff,
	.muxsel         = { 2,2,2,2, 3,3,3,3, 1,1,1,1, 0,0,0,0 },
	.muxsel_hook    = xguard_muxsel,
	.no_msp34xx     = 1,
	.no_tda9875     = 1,
        .no_tda7432     = 1,
	.pll            = PLL_28,
},{

	/* ---- card 0x68 ---------------------------------- */
	.name           = "Nebula Electronics DigiTV",
	.svhs           = -1,
	.muxsel         = { 2, 3, 1, 0},
	.no_msp34xx     = 1,
	.no_tda9875     = 1,
	.no_tda7432     = 1,
	.pll            = PLL_28,
	.tuner_type     = -1,
	.no_video       = 1,
},{
	/* Jorge Boncompte - DTI2 <jorge@dti2.net> */
	.name           = "ProVideo PV143",
        .video_inputs   = 4,
        .audio_inputs   = 0,
        .tuner          = -1,
        .svhs           = -1,
        .gpiomask       = 0,
        .muxsel         = { 2, 3, 1, 0 },
        .audiomux       = { 0 },
        .needs_tvaudio  = 0,
        .no_msp34xx     = 1,
        .pll            = PLL_28,
        .tuner_type     = -1,
},{
	/* M.Klahr@phytec.de */
	.name           = "PHYTEC VD-009-X1 MiniDIN (bt878)",
	.video_inputs   = 4,
	.audio_inputs   = 0,
	.tuner          = -1, /* card has no tuner */
	.svhs           = 3,
	.gpiomask       = 0x00, 
	.muxsel         = { 2, 3, 1, 0},
	.audiomux       = { 0, 0, 0, 0, 0, 0 }, /* card has no audio */
	.needs_tvaudio  = 1,
	.pll            = PLL_28,
	.tuner_type     = -1,
},{
	.name           = "PHYTEC VD-009-X1 Combi (bt878)",
	.video_inputs   = 4,
	.audio_inputs   = 0,
	.tuner          = -1, /* card has no tuner */
	.svhs           = 3,
	.gpiomask       = 0x00,
	.muxsel         = { 2, 3, 1, 1},
	.audiomux       = { 0, 0, 0, 0, 0, 0 }, /* card has no audio */
	.needs_tvaudio  = 1,
	.pll            = PLL_28,
	.tuner_type     = -1,
},{

	/* ---- card 0x6c ---------------------------------- */
	.name           = "PHYTEC VD-009 MiniDIN (bt878)",
	.video_inputs   = 10,
	.audio_inputs   = 0,
	.tuner          = -1, /* card has no tuner */
	.svhs           = 9,
	.gpiomask       = 0x00,
	.gpiomask2      = 0x03, /* gpiomask2 defines the bits used to switch audio
				   via the upper nibble of muxsel. here: used for
				   xternal video-mux */
	.muxsel         = { 0x02, 0x12, 0x22, 0x32, 0x03, 0x13, 0x23, 0x33, 0x01, 0x00 },
	.audiomux       = { 0, 0, 0, 0, 0, 0 }, /* card has no audio */
	.needs_tvaudio  = 1,
	.pll            = PLL_28,
	.tuner_type     = -1,
},{
	.name           = "PHYTEC VD-009 Combi (bt878)",
	.video_inputs   = 10,
	.audio_inputs   = 0,
	.tuner          = -1, /* card has no tuner */
	.svhs           = 9,
	.gpiomask       = 0x00,
	.gpiomask2      = 0x03, /* gpiomask2 defines the bits used to switch audio
				   via the upper nibble of muxsel. here: used for
				   xternal video-mux */
	.muxsel         = { 0x02, 0x12, 0x22, 0x32, 0x03, 0x13, 0x23, 0x33, 0x01, 0x01 },
	.audiomux       = { 0, 0, 0, 0, 0, 0 }, /* card has no audio */
	.needs_tvaudio  = 1,
	.pll            = PLL_28,
	.tuner_type     = -1,
},{
        .name           = "IVC-100",
        .video_inputs   = 4,
        .audio_inputs   = 0,
        .tuner          = -1,
        .tuner_type     = -1,
        .svhs           = -1,
        .gpiomask       = 0xdf,
        .muxsel         = { 2, 3, 1, 0 },
        .pll            = PLL_28,
},{
	/* IVC-120G - Alan Garfield <alan@fromorbit.com> */
	.name           = "IVC-120G",
	.video_inputs   = 16,
	.audio_inputs   = 0,    /* card has no audio */
	.tuner          = -1,   /* card has no tuner */
	.tuner_type     = -1,
	.svhs           = -1,   /* card has no svhs */
	.needs_tvaudio  = 0,
	.no_msp34xx     = 1,
	.no_tda9875     = 1,
	.no_tda7432     = 1,
	.gpiomask       = 0x00,
	.muxsel         = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 
			    0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10 },
	.muxsel_hook    = ivc120_muxsel,
	.pll            = PLL_28,
}};

const unsigned int bttv_num_tvcards = ARRAY_SIZE(bttv_tvcards);

/* ----------------------------------------------------------------------- */

static unsigned char eeprom_data[256];

/*
 * identify card
 */
void bttv_idcard(struct bttv *btv)
{
	unsigned int gpiobits;
	int i,type;
	short int tmp;

	/* read PCI subsystem ID */
	pci_read_config_word(btv->bt_dev, PCI_SUBSYSTEM_ID, &tmp);
	btv->cardid = tmp << 16;
	pci_read_config_word(btv->bt_dev, PCI_SUBSYSTEM_VENDOR_ID, &tmp);
	btv->cardid |= tmp;

	if (0 != btv->cardid && 0xffffffff != btv->cardid) {
		/* look for the card */
		for (type = -1, i = 0; cards[i].id != 0; i++){
			if (cards[i].id  == btv->cardid){
				type = i;
				break;
			}
		}
		
		if (type != -1) {
			/* found it */
			printk(KERN_INFO "bttv%d: detected: %s [card=%d], "
			       "PCI subsystem ID is %04x:%04x\n",
			       btv->nr,cards[type].name,cards[type].cardnr,
			       btv->cardid & 0xffff,
			       (btv->cardid >> 16) & 0xffff);
			btv->type = cards[type].cardnr;
		} else {
			/* 404 */
			printk(KERN_INFO "bttv%d: subsystem: %04x:%04x (UNKNOWN)\n",
			       btv->nr, btv->cardid & 0xffff,
			       (btv->cardid >> 16) & 0xffff);
			printk(KERN_DEBUG "please mail id, board name and "
			       "the correct card= insmod option to kraxel@bytesex.org\n");
		}
	} 

	/* let the user override the autodetected type */
	if (card[btv->nr] < bttv_num_tvcards)
		btv->type=card[btv->nr];
	
	/* print which card config we are using */
	printk(KERN_INFO "bttv%d: using: %s [card=%d,%s]\n",btv->nr,
	       bttv_tvcards[btv->type].name, btv->type,
	       card[btv->nr] < bttv_num_tvcards
	       ? "insmod option" : "autodetected");

	/* overwrite gpio stuff ?? */
	if (UNSET == audioall && UNSET == audiomux[0])
		return;

	if (UNSET != audiomux[0]) {
		gpiobits = 0;
		for (i = 0; i < 5; i++) {
			bttv_tvcards[btv->type].audiomux[i] = audiomux[i];
			gpiobits |= audiomux[i];
		}
	} else {
		gpiobits = audioall;
		for (i = 0; i < 5; i++) {
			bttv_tvcards[btv->type].audiomux[i] = audioall;
		}
	}
	bttv_tvcards[btv->type].gpiomask = (UNSET != gpiomask) ? gpiomask : gpiobits;
	printk(KERN_INFO "bttv%d: gpio config override: mask=0x%lx, mux=",
	       btv->nr,bttv_tvcards[btv->type].gpiomask);
	for (i = 0; i < 5; i++) {
		printk("%s0x%lx", i ? "," : "", bttv_tvcards[btv->type].audiomux[i]);
	}
	printk("\n");
}

/*
 * (most) board specific initialisations goes here
 */

/* Some Modular Technology cards have an eeprom, but no subsystem ID */
void identify_by_eeprom(struct bttv *btv, unsigned char eeprom_data[256])
{
	int type = -1;
	
	if (0 == strncmp(eeprom_data,"GET.MM20xPCTV",13))
		type = BTTV_MODTEC_205;
	else if (0 == strncmp(eeprom_data+20,"Picolo",7))
		type = BTTV_EURESYS_PICOLO;
	else if (eeprom_data[0] == 0x84 && eeprom_data[2]== 0)
                type = BTTV_HAUPPAUGE; /* old bt848 */

	if (-1 != type) {
		btv->type = type;
		printk("bttv%d: detected by eeprom: %s [card=%d]\n",
		       btv->nr, bttv_tvcards[btv->type].name, btv->type);
	}
}

static void flyvideo_gpio(struct bttv *btv)
{ 
	int gpio,outbits,has_remote,has_radio,is_capture_only,is_lr90,has_tda9820_tda9821;
	int tuner=-1,ttype;
	
	outbits = btread(BT848_GPIO_OUT_EN);
	btwrite(0x00, BT848_GPIO_OUT_EN);
	udelay(8);  // without this we would see the 0x1800 mask
	gpio=btread(BT848_GPIO_DATA);
	btwrite(outbits, BT848_GPIO_OUT_EN);
	// all cards provide GPIO info, some have an additional eeprom
	// LR50: GPIO coding can be found lower right CP1 .. CP9
	//       CP9=GPIO23 .. CP1=GPIO15; when OPEN, the corresponding GPIO reads 1.
	//       GPIO14-12: n.c.
	// LR90: GP9=GPIO23 .. GP1=GPIO15 (right above the bt878)
	
	// lowest 3 bytes are remote control codes (no handshake needed)
        // xxxFFF: No remote control chip soldered
        // xxxF00(LR26/LR50), xxxFE0(LR90): Remote control chip (LVA001 or CF45) soldered 
	// Note: Some bits are Audio_Mask !

	ttype=(gpio&0x0f0000)>>16;
	switch(ttype) {
	case 0x0: tuner=2; // NTSC, e.g. TPI8NSR11P
		break;
        case 0x2: tuner=39;// LG NTSC (newer TAPC series) TAPC-H701P
		break;
	case 0x4: tuner=5; // Philips PAL TPI8PSB02P, TPI8PSB12P, TPI8PSB12D or FI1216, FM1216
		break;
	case 0x6: tuner=37; // LG PAL (newer TAPC series) TAPC-G702P
		break;
	case 0xC: tuner=3; // Philips SECAM(+PAL) FQ1216ME or FI1216MF
		break;
	default:
		printk(KERN_INFO "bttv%d: FlyVideo_gpio: unknown tuner type.\n", btv->nr);
	}

	has_remote          =   gpio & 0x800000;
	has_radio	    =   gpio & 0x400000;
	//   unknown                   0x200000;
	//   unknown2                  0x100000;
        is_capture_only     = !(gpio & 0x008000); //GPIO15
	has_tda9820_tda9821 = !(gpio & 0x004000);
	is_lr90             = !(gpio & 0x002000); // else LR26/LR50 (LR38/LR51 f. capture only)
        //		        gpio & 0x001000 // output bit for audio routing

	if(is_capture_only) 
		tuner=4; // No tuner present 

	printk(KERN_INFO "bttv%d: FlyVideo Radio=%s RemoteControl=%s Tuner=%d gpio=0x%06x\n", 
	       btv->nr, has_radio? "yes":"no ", has_remote? "yes":"no ", tuner, gpio); 
	printk(KERN_INFO "bttv%d: FlyVideo  LR90=%s tda9821/tda9820=%s capture_only=%s\n",
		btv->nr, is_lr90?"yes":"no ", has_tda9820_tda9821?"yes":"no ", 
		is_capture_only?"yes":"no ");

	if(tuner!= -1) // only set if known tuner autodetected, else let insmod option through
		btv->tuner_type = tuner;
	btv->has_radio = has_radio;  

	// LR90 Audio Routing is done by 2 hef4052, so Audio_Mask has 4 bits: 0x001c80
        // LR26/LR50 only has 1 hef4052, Audio_Mask 0x000c00
	// Audio options: from tuner, from tda9821/tda9821(mono,stereo,sap), from tda9874, ext., mute
	if(has_tda9820_tda9821) btv->audio_hook = lt9415_audio;
	//todo: if(has_tda9874) btv->audio_hook = fv2000s_audio;
}

int miro_tunermap[] = { 0,6,2,3,   4,5,6,0,  3,0,4,5,  5,2,16,1,
			14,2,17,1, 4,1,4,3,  1,2,16,1, 4,4,4,4 };
int miro_fmtuner[]  = { 0,0,0,0,   0,0,0,0,  0,0,0,0,  0,0,0,1,
			1,1,1,1,   1,1,1,0,  0,0,0,0,  0,1,0,0 };

static void miro_pinnacle_gpio(struct bttv *btv)
{
	int id,msp,gpio;
	char *info;

	btwrite(0,BT848_GPIO_OUT_EN);
        gpio = btread(BT848_GPIO_DATA);
	id   = ((gpio>>10) & 63) -1;
	msp  = bttv_I2CRead(btv, I2C_MSP3400, "MSP34xx");
	if (id < 32) {
		btv->tuner_type = miro_tunermap[id];
		if (0 == (gpio & 0x20)) {
			btv->has_radio = 1;
			if (!miro_fmtuner[id]) {
				btv->has_matchbox = 1;
				btv->mbox_we    = (1<<6);
				btv->mbox_most  = (1<<7);
				btv->mbox_clk   = (1<<8);
				btv->mbox_data  = (1<<9);
				btv->mbox_mask  = (1<<6)|(1<<7)|(1<<8)|(1<<9);
			}
		} else {
			btv->has_radio = 0;
		}
		if (-1 != msp) {
			if (btv->type == BTTV_MIRO)
				btv->type = BTTV_MIROPRO;
			if (btv->type == BTTV_PINNACLE)
				btv->type = BTTV_PINNACLEPRO;
		}
		printk(KERN_INFO
		       "bttv%d: miro: id=%d tuner=%d radio=%s stereo=%s\n",
		       btv->nr, id+1, btv->tuner_type,
		       !btv->has_radio ? "no" :
		       (btv->has_matchbox ? "matchbox" : "fmtuner"),
		       (-1 == msp) ? "no" : "yes");
	} else {
		/* new cards with microtune tuner */
		id = 63 - id;
		btv->has_radio = 0;
		switch (id) {
		case 1:
			info = "PAL / mono";
			break;
		case 2:
			info = "PAL+SECAM / stereo";
			btv->has_radio = 1;
			break;
		case 3:
			info = "NTSC / stereo";
			btv->has_radio = 1;
			break;
		case 4:
			info = "PAL+SECAM / mono";
			break;
		case 5:
			info = "NTSC / mono";
			break;
		case 6:
			info = "NTSC / stereo";
			break;
		default:
			info = "oops: unknown card";
			break;
		}
		if (-1 != msp)
			btv->type = BTTV_PINNACLEPRO;
		printk(KERN_INFO
		       "bttv%d: pinnacle/mt: id=%d info=\"%s\" radio=%s\n",
		       btv->nr, id, info, btv->has_radio ? "yes" : "no");
		btv->tuner_type  = 33;
		btv->pinnacle_id = id;
	}
}

/* GPIO21   L: Buffer aktiv, H: Buffer inaktiv */
#define LM1882_SYNC_DRIVE     0x200000L

static void init_ids_eagle(struct bttv *btv)
{
	btwrite(0xFFFF37, BT848_GPIO_OUT_EN);
	btwrite(0x000000, BT848_GPIO_REG_INP);
	
	btwrite(0x200020, BT848_GPIO_DATA);
	
	/* flash strobe inverter ?! */
	btwrite(0x200024, BT848_GPIO_DATA);
	
	/* switch sync drive off */
	btor(LM1882_SYNC_DRIVE, BT848_GPIO_DATA);
	
	/* set BT848 muxel to 2 */
	btaor((2)<<5, ~(2<<5), BT848_IFORM);
}

/* Muxsel helper for the IDS Eagle.
 * the eagles does not use the standard muxsel-bits but
 * has its own multiplexer */
static void eagle_muxsel(struct bttv *btv, unsigned int input)
{
	btaor((2)<<5, ~(3<<5), BT848_IFORM);
	btaor((bttv_tvcards[btv->type].muxsel[input&7]&3),
	      ~3, BT848_GPIO_DATA);

       /* composite */
       /* set chroma ADC to sleep */
       btor(BT848_ADC_C_SLEEP, BT848_ADC);
       /* set to composite video */
       btand(~BT848_CONTROL_COMP, BT848_E_CONTROL);
       btand(~BT848_CONTROL_COMP, BT848_O_CONTROL);

       /* switch sync drive off */
       btor(LM1882_SYNC_DRIVE, BT848_GPIO_DATA);
}

/* ----------------------------------------------------------------------- */

void bttv_reset_audio(struct bttv *btv)
{
	/*
	 * BT878A has a audio-reset register.
	 * 1. This register is an audio reset function but it is in
	 *    function-0 (video capture) address space.
	 * 2. It is enough to do this once per power-up of the card.
	 * 3. There is a typo in the Conexant doc -- it is not at
	 *    0x5B, but at 0x058. (B is an odd-number, obviously a typo!).
	 * --//Shrikumar 030609
	 */
	if (btv->id != 878)
		return;
	
	if (bttv_debug)
		printk("bttv%d: BT878A ARESET\n",btv->nr);
	btwrite((1<<7), 0x058);
	udelay(10);
	btwrite(     0, 0x058);
}

/* initialization part one -- before registering i2c bus */
void bttv_init_card1(struct bttv *btv)
{
	switch (btv->type) {
	case BTTV_HAUPPAUGE:
	case BTTV_HAUPPAUGE878:
                boot_msp34xx(btv,5);
		break;
	case BTTV_VOODOOTV_FM:
                boot_msp34xx(btv,20);
		break;
	case BTTV_AVERMEDIA98:
		boot_msp34xx(btv,11);
		break;
	case BTTV_HAUPPAUGEPVR:
		pvr_boot(btv);
		break;
	}
}

/* initialization part two -- after registering i2c bus */
void bttv_init_card2(struct bttv *btv)
{
        btv->tuner_type = -1;

	if (BTTV_UNKNOWN == btv->type) {
		bttv_readee(btv,eeprom_data,0xa0);
		identify_by_eeprom(btv,eeprom_data);
	}

	switch (btv->type) {
	case BTTV_MIRO:
	case BTTV_MIROPRO:
	case BTTV_PINNACLE:
	case BTTV_PINNACLEPRO: 
		/* miro/pinnacle */
		miro_pinnacle_gpio(btv);
		break;
	case BTTV_FLYVIDEO_98:
	case BTTV_MAXI:
	case BTTV_LIFE_FLYKIT:
	case BTTV_FLYVIDEO:
	case BTTV_TYPHOON_TVIEW:
	case BTTV_CHRONOS_VS2:
	case BTTV_FLYVIDEO_98FM:
	case BTTV_FLYVIDEO2000:
	case BTTV_FLYVIDEO98EZ:
	case BTTV_CONFERENCETV:
	case BTTV_LIFETEC_9415:
		flyvideo_gpio(btv);
		break;
	case BTTV_HAUPPAUGE:
	case BTTV_HAUPPAUGE878:
	case BTTV_HAUPPAUGEPVR:
		/* pick up some config infos from the eeprom */
		bttv_readee(btv,eeprom_data,0xa0);
                hauppauge_eeprom(btv);
		break;
	case BTTV_AVERMEDIA98:
	case BTTV_AVPHONE98:
		bttv_readee(btv,eeprom_data,0xa0);
		avermedia_eeprom(btv);
		break;
	case BTTV_PXC200:
		init_PXC200(btv);
		break;
	case BTTV_VHX:
		btv->has_radio    = 1;
		btv->has_matchbox = 1;
		btv->mbox_we      = 0x20;
		btv->mbox_most    = 0;
		btv->mbox_clk     = 0x08;
		btv->mbox_data    = 0x10;
		btv->mbox_mask    = 0x38;
		break;
	case BTTV_VOBIS_BOOSTAR:
	case BTTV_TERRATV:
		terratec_active_radio_upgrade(btv);
		break;
	case BTTV_MAGICTVIEW061:
		if (btv->cardid == 0x3002144f) {
			btv->has_radio=1;
			printk("bttv%d: radio detected by subsystem id (CPH05x)\n",btv->nr);
		}
		break;
       case BTTV_STB2:
                if (btv->cardid == 0x3060121a) {
			/* Fix up entry for 3DFX VoodooTV 100,
			   which is an OEM STB card variant. */
			btv->has_radio=0;
			btv->tuner_type=TUNER_TEMIC_NTSC;
		}
		break;
	case BTTV_OSPREY1x0:
	case BTTV_OSPREY1x0_848:
	case BTTV_OSPREY101_848:
	case BTTV_OSPREY1x1:
	case BTTV_OSPREY1x1_SVID:
	case BTTV_OSPREY2xx:
	case BTTV_OSPREY2x0_SVID:
	case BTTV_OSPREY2x0:
	case BTTV_OSPREY500:
	case BTTV_OSPREY540:
	case BTTV_OSPREY2000:
		bttv_readee(btv,eeprom_data,0xa0);
                osprey_eeprom(btv);
		break;
	case BTTV_IDS_EAGLE:
		init_ids_eagle(btv);
		break;
	case BTTV_MODTEC_205:
		bttv_readee(btv,eeprom_data,0xa0);
		modtec_eeprom(btv);
		break;
	}

	/* pll configuration */
        if (!(btv->id==848 && btv->revision==0x11)) {
		/* defaults from card list */
		if (PLL_28 == bttv_tvcards[btv->type].pll) {
			btv->pll.pll_ifreq=28636363;
			btv->pll.pll_crystal=BT848_IFORM_XT0;
		}
		if (PLL_35 == bttv_tvcards[btv->type].pll) {
			btv->pll.pll_ifreq=35468950;
			btv->pll.pll_crystal=BT848_IFORM_XT1;
		}
		/* insmod options can override */
                switch (pll[btv->nr]) {
                case 0: /* none */
			btv->pll.pll_crystal = 0;
			btv->pll.pll_ifreq   = 0;
			btv->pll.pll_ofreq   = 0;
                        break;
                case 1: /* 28 MHz */
		case 28:
                        btv->pll.pll_ifreq   = 28636363;
			btv->pll.pll_ofreq   = 0;
                        btv->pll.pll_crystal = BT848_IFORM_XT0;
                        break;
                case 2: /* 35 MHz */
		case 35:
                        btv->pll.pll_ifreq   = 35468950;
			btv->pll.pll_ofreq   = 0;
                        btv->pll.pll_crystal = BT848_IFORM_XT1;
                        break;
                }
        }
	btv->pll.pll_current = -1;

	/* tuner configuration (from card list / autodetect / insmod option) */
 	if (UNSET != bttv_tvcards[btv->type].tuner_type)
		if(UNSET == btv->tuner_type) 
                	btv->tuner_type = bttv_tvcards[btv->type].tuner_type;
	if (UNSET != tuner[btv->nr])
		btv->tuner_type = tuner[btv->nr];
	printk("bttv%d: using tuner=%d\n",btv->nr,btv->tuner_type);
	if (btv->pinnacle_id != UNSET){
		set_break_point_here;
		bttv_call_i2c_clients(btv,AUDC_CONFIG_PINNACLE,
				      &btv->pinnacle_id);
	}

	if (btv->tuner_type != UNSET)
		bttv_call_i2c_clients(btv,TUNER_SET_TYPE,&btv->tuner_type);
	btv->svhs = bttv_tvcards[btv->type].svhs;
	if (svhs[btv->nr] != UNSET)
		btv->svhs = svhs[btv->nr];

	if (bttv_tvcards[btv->type].has_radio)
		btv->has_radio=1;
	if (bttv_tvcards[btv->type].audio_hook)
		btv->audio_hook=bttv_tvcards[btv->type].audio_hook;

	if (bttv_tvcards[btv->type].digital_mode == DIGITAL_MODE_CAMERA) {
		/* detect Bt832 chip for quartzsight digital camera */
		if ((bttv_I2CRead(btv, I2C_BT832_ALT1, "Bt832") >=0) ||
		    (bttv_I2CRead(btv, I2C_BT832_ALT2, "Bt832") >=0))
			boot_bt832(btv);
	}

	/* try to detect audio/fader chips */
	/*MaRTE OS*/
	/*	if (!bttv_tvcards[btv->type].no_msp34xx &&
	    bttv_I2CRead(btv, I2C_MSP3400, "MSP34xx") >=0) {
	}

	if (bttv_tvcards[btv->type].msp34xx_alt &&
	    bttv_I2CRead(btv, I2C_MSP3400_ALT, "MSP34xx (alternate address)") >=0) {
	}

	if (!bttv_tvcards[btv->type].no_tda9875 &&
	    bttv_I2CRead(btv, I2C_TDA9875, "TDA9875") >=0) {
	}

	if (!bttv_tvcards[btv->type].no_tda7432 && 
	    bttv_I2CRead(btv, I2C_TDA7432, "TDA7432") >=0) {
	}
	if (bttv_tvcards[btv->type].needs_tvaudio) {
	}*/
	/* tuner modules */
	/*	if (btv->pinnacle_id != UNSET) {
	}
	if (btv->tuner_type != UNSET) {
	}*/
}


/* ----------------------------------------------------------------------- */
/* some hauppauge specific stuff                                           */

static struct HAUPPAUGE_TUNER 
{
        int  id;
        char *name;
} 
hauppauge_tuner[] = 
{
        { TUNER_ABSENT,        "" },
        { TUNER_ABSENT,        "External" },
        { TUNER_ABSENT,        "Unspecified" },
        { TUNER_PHILIPS_PAL,   "Philips FI1216" },
        { TUNER_PHILIPS_SECAM, "Philips FI1216MF" },
        { TUNER_PHILIPS_NTSC,  "Philips FI1236" },
        { TUNER_PHILIPS_PAL_I, "Philips FI1246" },
        { TUNER_PHILIPS_PAL_DK,"Philips FI1256" },
        { TUNER_PHILIPS_PAL,   "Philips FI1216 MK2" },
        { TUNER_PHILIPS_SECAM, "Philips FI1216MF MK2" },
        { TUNER_PHILIPS_NTSC,  "Philips FI1236 MK2" },
        { TUNER_PHILIPS_PAL_I, "Philips FI1246 MK2" },
        { TUNER_PHILIPS_PAL_DK,"Philips FI1256 MK2" },
        { TUNER_TEMIC_NTSC,    "Temic 4032FY5" },
        { TUNER_TEMIC_PAL,     "Temic 4002FH5" },
        { TUNER_TEMIC_PAL_I,   "Temic 4062FY5" },
        { TUNER_PHILIPS_PAL,   "Philips FR1216 MK2" },
        { TUNER_PHILIPS_SECAM, "Philips FR1216MF MK2" },
        { TUNER_PHILIPS_NTSC,  "Philips FR1236 MK2" },
        { TUNER_PHILIPS_PAL_I, "Philips FR1246 MK2" },
        { TUNER_PHILIPS_PAL_DK,"Philips FR1256 MK2" },
        { TUNER_PHILIPS_PAL,   "Philips FM1216" },
        { TUNER_PHILIPS_SECAM, "Philips FM1216MF" },
        { TUNER_PHILIPS_NTSC,  "Philips FM1236" },
        { TUNER_PHILIPS_PAL_I, "Philips FM1246" },
        { TUNER_PHILIPS_PAL_DK,"Philips FM1256" },
        { TUNER_TEMIC_4036FY5_NTSC, "Temic 4036FY5" },
        { TUNER_ABSENT,        "Samsung TCPN9082D" },
        { TUNER_ABSENT,        "Samsung TCPM9092P" },
        { TUNER_TEMIC_4006FH5_PAL, "Temic 4006FH5" },
        { TUNER_ABSENT,        "Samsung TCPN9085D" },
        { TUNER_ABSENT,        "Samsung TCPB9085P" },
        { TUNER_ABSENT,        "Samsung TCPL9091P" },
        { TUNER_TEMIC_4039FR5_NTSC, "Temic 4039FR5" },
        { TUNER_PHILIPS_FQ1216ME,   "Philips FQ1216 ME" },
        { TUNER_TEMIC_4066FY5_PAL_I, "Temic 4066FY5" },
        { TUNER_PHILIPS_NTSC,        "Philips TD1536" },
        { TUNER_PHILIPS_NTSC,        "Philips TD1536D" },
	{ TUNER_PHILIPS_NTSC,  "Philips FMR1236" }, /* mono radio */
        { TUNER_ABSENT,        "Philips FI1256MP" },
        { TUNER_ABSENT,        "Samsung TCPQ9091P" },
        { TUNER_TEMIC_4006FN5_MULTI_PAL, "Temic 4006FN5" },
        { TUNER_TEMIC_4009FR5_PAL, "Temic 4009FR5" },
        { TUNER_TEMIC_4046FM5,     "Temic 4046FM5" },
	{ TUNER_TEMIC_4009FN5_MULTI_PAL_FM, "Temic 4009FN5" },
	{ TUNER_ABSENT,        "Philips TD1536D_FH_44"},
	{ TUNER_LG_NTSC_FM,    "LG TPI8NSR01F"},
	{ TUNER_LG_PAL_FM,     "LG TPI8PSB01D"},
	{ TUNER_LG_PAL,        "LG TPI8PSB11D"},	
	{ TUNER_LG_PAL_I_FM,   "LG TAPC-I001D"},
	{ TUNER_LG_PAL_I,      "LG TAPC-I701D"}
};

static void modtec_eeprom(struct bttv *btv)
{
	if( strncmp(&(eeprom_data[0x1e]),"Temic 4066 FY5",14) ==0) {
		btv->tuner_type=TUNER_TEMIC_4066FY5_PAL_I;
		printk("bttv Modtec: Tuner autodetected %s\n",
		       &eeprom_data[0x1e]);
	} else {
		printk("bttv Modtec: Unknown TunerString:%s\n",
		       &eeprom_data[0x1e]);
	}
}

static void hauppauge_eeprom(struct bttv *btv)
{
	unsigned int blk2,tuner,radio,model;

	if (eeprom_data[0] != 0x84 || eeprom_data[2] != 0)
		printk(KERN_WARNING "bttv%d: Hauppauge eeprom: invalid\n",
		       btv->nr);

	/* Block 2 starts after len+3 bytes header */
	blk2 = eeprom_data[1] + 3;

	/* decode + use some config infos */
	model = eeprom_data[12] << 8 | eeprom_data[11];
	tuner = eeprom_data[9];
	radio = eeprom_data[blk2-1] & 0x01;
	
        if (tuner < ARRAY_SIZE(hauppauge_tuner))
                btv->tuner_type = hauppauge_tuner[tuner].id;
	if (radio)
		btv->has_radio = 1;
	
	if (bttv_verbose)
		printk(KERN_INFO "bttv%d: Hauppauge eeprom: model=%d, "
		       "tuner=%s (%d), radio=%s\n",
		       btv->nr, model, hauppauge_tuner[tuner].name,
		       btv->tuner_type, radio ? "yes" : "no");
}

static int terratec_active_radio_upgrade(struct bttv *btv)
{
	int freq;

	btv->has_radio    = 1;
	btv->has_matchbox = 1;
	btv->mbox_we      = 0x10;
	btv->mbox_most    = 0x20;
	btv->mbox_clk     = 0x08;
	btv->mbox_data    = 0x04;
	btv->mbox_mask    = 0x3c;

	btv->mbox_iow     = 1 <<  8;
	btv->mbox_ior     = 1 <<  9;
	btv->mbox_csel    = 1 << 10;

	freq=88000/62.5;
	tea5757_write(btv, 5 * freq + 0x358); // write 0x1ed8
	if (0x1ed8 == tea5757_read(btv)) {
		printk("bttv%d: Terratec Active Radio Upgrade found.\n",
		       btv->nr);
		btv->has_radio    = 1;
		btv->has_matchbox = 1;
	} else {
		btv->has_radio    = 0;
		btv->has_matchbox = 0;
	}
	return 0;
}	


/* ----------------------------------------------------------------------- */

/*
 * minimal bootstrap for the WinTV/PVR -- upload altera firmware.
 *
 * The hcwamc.rbf firmware file is on the Hauppauge driver CD.  Have
 * a look at Pvr/pvr45xxx.EXE (self-extracting zip archive, can be
 * unpacked with unzip).
 */
#define PVR_GPIO_DELAY		10

#define BTTV_ALT_DATA		0x000001
#define BTTV_ALT_DCLK		0x100000
#define BTTV_ALT_NCONFIG	0x800000

static int pvr_altera_load(struct bttv *btv, u8 *micro, u32 microlen)
{
	u32 n;
  	u8 bits;
	int i;
 
	btwrite(BTTV_ALT_DATA|BTTV_ALT_DCLK|BTTV_ALT_NCONFIG,
		BT848_GPIO_OUT_EN);
	btwrite(0,BT848_GPIO_DATA);
	udelay(PVR_GPIO_DELAY);
	
	btwrite(BTTV_ALT_NCONFIG,BT848_GPIO_DATA);
	udelay(PVR_GPIO_DELAY);

	for (n = 0; n < microlen; n++) {
		bits = micro[n];
		for ( i = 0 ; i < 8 ; i++ ) {
			btand(~BTTV_ALT_DCLK,BT848_GPIO_DATA);
			if (bits & 0x01) 
				btor(BTTV_ALT_DATA,BT848_GPIO_DATA);
			else 
				btand(~BTTV_ALT_DATA,BT848_GPIO_DATA);
			btor(BTTV_ALT_DCLK,BT848_GPIO_DATA);
			bits >>= 1;
		}
	}
	btand(~BTTV_ALT_DCLK,BT848_GPIO_DATA);
	udelay(PVR_GPIO_DELAY);
	
	/* begin Altera init loop (Not necessary,but doesn't hurt) */
	for (i = 0 ; i < 30 ; i++) {
		btand(~BTTV_ALT_DCLK,BT848_GPIO_DATA);
		btor(BTTV_ALT_DCLK,BT848_GPIO_DATA);
	}
	btand(~BTTV_ALT_DCLK,BT848_GPIO_DATA);
	return 0;
}

#ifndef CONFIG_FW_LOADER
/* old 2.4.x way -- via soundcore's mod_firmware_load */
   
static char *firm_altera = "/usr/lib/video4linux/hcwamc.rbf";

extern int mod_firmware_load(const char *fn, char **fp);

int pvr_boot(struct bttv *btv)
{
	u32 microlen;
	u8 *micro;
	int result;

	microlen = 0;//mod_firmware_load(firm_altera, (char**) &micro);
	if (!microlen) {
		printk(KERN_WARNING "bttv%d: altera firmware not found [%s]\n",
		       btv->nr, firm_altera);
		return -1;
	}
	
	printk(KERN_INFO "bttv%d: uploading altera firmware [%s] ...\n",
	       btv->nr, firm_altera);
	result = pvr_altera_load(btv, micro, microlen);
	printk(KERN_INFO "bttv%d: ... upload %s\n",
	       btv->nr, (result < 0) ? "failed" : "ok");
	vfree(micro);
	return result;
}
#else
/* new 2.5.x way -- via hotplug firmware loader */

int pvr_boot(struct bttv *btv)
{
        const struct firmware *fw_entry;
	int rc;

	rc = request_firmware(&fw_entry, "hcwamc.rbf", &btv->dev->dev);
	if (rc != 0) {
		printk(KERN_WARNING "bttv%d: no altera firmware [via hotplug]\n",
		       btv->nr);
                return rc;
        }
	rc = pvr_altera_load(btv, fw_entry->data, fw_entry->size);
	printk(KERN_INFO "bttv%d: altera firmware upload %s\n",
	       btv->nr, (rc < 0) ? "failed" : "ok");
        release_firmware(fw_entry);
	return rc;
}	
#endif

/* ----------------------------------------------------------------------- */
/* some osprey specific stuff                                              */

static void osprey_eeprom(struct bttv *btv)
{
       int i = 0;
       unsigned char *ee = eeprom_data;
       unsigned long serial = 0;
    
       if (btv->type == 0) {
               /* this might be an antique... check for MMAC label in eeprom */
               if ((ee[0]=='M') && (ee[1]=='M') && (ee[2]=='A') && (ee[3]=='C')) {
                       unsigned char checksum = 0;
                       for (i =0; i<21; i++)
			       checksum += ee[i];
                       if (checksum != ee[21])
			       return;
		       btv->type = BTTV_OSPREY1x0_848;
		       for (i = 12; i < 21; i++)
			       serial *= 10, serial += ee[i] - '0';
               }
       } else {
	       unsigned short type;
               int offset = 4*16;
        
               for(; offset < 8*16; offset += 16) {
                       unsigned short checksum = 0;
                       /* verify the checksum */
                       for(i = 0; i<14; i++) checksum += ee[i+offset];
                               checksum = ~checksum;  /* no idea why */
                               if ((((checksum>>8)&0x0FF) == ee[offset+14]) &&
                                   ((checksum & 0x0FF) == ee[offset+15])) {
                               break;
                       }
               }
        
               if (offset >= 8*16)
		       return;

	       /* found a valid descriptor */
	       type = (ee[offset+4]<<8) | (ee[offset+5]);
	       
	       switch(type) {

	       /* 848 based */
	       case 0x0004:
		       btv->type = BTTV_OSPREY1x0_848;
		       break;
	       case 0x0005:
		       btv->type = BTTV_OSPREY101_848;
		       break;
		       
               /* 878 based */
	       case 0x0012:
	       case 0x0013:
		       btv->type = BTTV_OSPREY1x0;
		       break;
	       case 0x0014:
	       case 0x0015:
		       btv->type = BTTV_OSPREY1x1;
		       break;
	       case 0x0016:
	       case 0x0017:
	       case 0x0020:
		       btv->type = BTTV_OSPREY1x1_SVID;
		       break;
	       case 0x0018:
	       case 0x0019:
	       case 0x001E:
	       case 0x001F:
		       btv->type = BTTV_OSPREY2xx;
		       break;
	       case 0x001A:
	       case 0x001B:
		       btv->type = BTTV_OSPREY2x0_SVID;
		       break;
	       case 0x0040:
		       btv->type = BTTV_OSPREY500;
		       break;
	       case 0x0050:
	       case 0x0056:
		       btv->type = BTTV_OSPREY540;
		       /* bttv_osprey_540_init(btv); */
		       break;
	       case 0x0060:
	       case 0x0070:
		       btv->type = BTTV_OSPREY2x0;
		       //enable output on select control lines
		       btwrite(0x000303, BT848_GPIO_OUT_EN);
		       break;
	       default:
		       /* unknown...leave generic, but get serial # */
		       break;
	       }
	       serial =  (ee[offset+6] << 24)
		       | (ee[offset+7] << 16)
		       | (ee[offset+8] <<  8)
		       | (ee[offset+9]);
       }
       
       printk(KERN_INFO "bttv%d: osprey eeprom: card=%d name=%s serial=%ld\n",
	      btv->nr, btv->type, bttv_tvcards[btv->type].name,serial);
}	

/* ----------------------------------------------------------------------- */
/* AVermedia specific stuff, from  bktr_card.c                             */

int tuner_0_table[] = {
        TUNER_PHILIPS_NTSC,  TUNER_PHILIPS_PAL /* PAL-BG*/,
        TUNER_PHILIPS_PAL,   TUNER_PHILIPS_PAL /* PAL-I*/,
        TUNER_PHILIPS_PAL,   TUNER_PHILIPS_PAL,
        TUNER_PHILIPS_SECAM, TUNER_PHILIPS_SECAM,
        TUNER_PHILIPS_SECAM, TUNER_PHILIPS_PAL};

int tuner_1_table[] = {
        TUNER_TEMIC_NTSC,  TUNER_TEMIC_PAL,
	TUNER_TEMIC_PAL,   TUNER_TEMIC_PAL,
	TUNER_TEMIC_PAL,   TUNER_TEMIC_PAL,
        TUNER_TEMIC_4012FY5, TUNER_TEMIC_4012FY5, //TUNER_TEMIC_SECAM
        TUNER_TEMIC_4012FY5, TUNER_TEMIC_PAL};

static void avermedia_eeprom(struct bttv *btv)
{
        int tuner_make,tuner_tv_fm,tuner_format,tuner=0;

	tuner_make      = (eeprom_data[0x41] & 0x7);
        tuner_tv_fm     = (eeprom_data[0x41] & 0x18) >> 3;
        tuner_format    = (eeprom_data[0x42] & 0xf0) >> 4;
	btv->has_remote = (eeprom_data[0x42] & 0x01);

	if (tuner_make == 0 || tuner_make == 2)
		if(tuner_format <=9)
			tuner = tuner_0_table[tuner_format];
	if (tuner_make == 1)
		if(tuner_format <=9)
			tuner = tuner_1_table[tuner_format];
	
	printk(KERN_INFO "bttv%d: Avermedia eeprom[0x%02x%02x]: tuner=",
		btv->nr,eeprom_data[0x41],eeprom_data[0x42]);
	if(tuner) {
		btv->tuner_type=tuner;
		printk("%d",tuner);
	} else
		printk("Unknown type");
	printk(" radio:%s remote control:%s\n",
	       tuner_tv_fm     ? "yes" : "no",
	       btv->has_remote ? "yes" : "no");
}

/* used on Voodoo TV/FM (Voodoo 200), S0 wired to 0x10000 */
void bttv_tda9880_setnorm(struct bttv *btv, int norm)
{
	// fix up our card entry
	if(norm==VIDEO_MODE_NTSC) {
		bttv_tvcards[BTTV_VOODOOTV_FM].audiomux[0]=0x957fff;
		bttv_tvcards[BTTV_VOODOOTV_FM].audiomux[4]=0x957fff;
		dprintk("bttv_tda9880_setnorm to NTSC\n");
	}
	else {
	        bttv_tvcards[BTTV_VOODOOTV_FM].audiomux[0]=0x947fff;
                bttv_tvcards[BTTV_VOODOOTV_FM].audiomux[4]=0x947fff;
		dprintk("bttv_tda9880_setnorm to PAL\n");
	}
	// set GPIO according
	btaor(bttv_tvcards[btv->type].audiomux[btv->audio],
              ~bttv_tvcards[btv->type].gpiomask, BT848_GPIO_DATA);
}


/*
 * reset/enable the MSP on some Hauppauge cards
 * Thanks to Ky�sti M�lkki (kmalkki@cc.hut.fi)!
 *
 * Hauppauge:  pin  5
 * Voodoo:     pin 20
 */
static void boot_msp34xx(struct bttv *btv, int pin)
{
	int mask = (1 << pin);

        btaor(mask, ~mask, BT848_GPIO_OUT_EN);
        btaor(0, ~mask, BT848_GPIO_DATA);
        udelay(2500);
        btaor(mask, ~mask, BT848_GPIO_DATA);
	if (bttv_gpio)
		bttv_gpio_tracking(btv,"msp34xx");

	if (bttv_verbose)
		printk(KERN_INFO "bttv%d: Hauppauge/Voodoo msp34xx: reset line "
		       "init [%d]\n", btv->nr, pin);
}

static void boot_bt832(struct bttv *btv)
{
	int outbits,databits,resetbit=0;

	switch (btv->type) {
	case BTTV_PXELVWPLTVPAK:
		resetbit = 0x400000;
		break;
	case BTTV_MODTEC_205:
		resetbit = 1<<9;
		break;
	default:
		BUG();
	}

	bttv_call_i2c_clients(btv, BT832_HEXDUMP, NULL);

	printk("bttv%d: Reset Bt832 [line=0x%x]\n",btv->nr,resetbit);
	btwrite(0, BT848_GPIO_DATA);
	outbits = btread(BT848_GPIO_OUT_EN);
	databits= btread(BT848_GPIO_DATA);
	btwrite(resetbit, BT848_GPIO_OUT_EN);
	udelay(5);
	btwrite(resetbit, BT848_GPIO_DATA);
	udelay(5);
	btwrite(0, BT848_GPIO_DATA);
	udelay(5);
	btwrite(outbits, BT848_GPIO_OUT_EN);
	btwrite(databits, BT848_GPIO_DATA);

	// bt832 on pixelview changes from i2c 0x8a to 0x88 after
	// being reset as above. So we must follow by this:
	bttv_call_i2c_clients(btv, BT832_REATTACH, NULL);
}

/* ----------------------------------------------------------------------- */
/*  Imagenation L-Model PXC200 Framegrabber */
/*  This is basically the same procedure as 
 *  used by Alessandro Rubini in his pxc200 
 *  driver, but using BTTV functions */

static void init_PXC200(struct bttv *btv)
{
/*
	static int vals[] __devinitdata = { 0x08, 0x09, 0x0a, 0x0b, 0x0d, 0x0d,
					    0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
					    0x00 };
*/
	int val;

	pll[btv->nr] = 0;

	/* Initialise GPIO-connevted stuff */
	btwrite(1<<13,BT848_GPIO_OUT_EN); /* Reset pin only */
	btwrite(0,BT848_GPIO_DATA);
	udelay(3);
	btwrite(1<<13,BT848_GPIO_DATA);
	/* GPIO inputs are pulled up, so no need to drive 
	 * reset pin any longer */
	btwrite(0,BT848_GPIO_OUT_EN);
	if (bttv_gpio)
		bttv_gpio_tracking(btv,"pxc200");

	/*  we could/should try and reset/control the AD pots? but
	    right now  we simply  turned off the crushing.  Without
	    this the AGC drifts drifts
	    remember the EN is reverse logic -->
	    setting BT848_ADC_AGC_EN disable the AGC
	    tboult@eecs.lehigh.edu
	*/

	btwrite(BT848_ADC_RESERVED|BT848_ADC_AGC_EN, BT848_ADC);
	
	/*	Initialise MAX517 DAC */
	printk(KERN_INFO "Setting DAC reference voltage level ...\n");
	bttv_I2CWrite(btv,0x5E,0,0x80,1);
	
	/*	Initialise 12C508 PIC */
	/*	The I2CWrite and I2CRead commmands are actually to the 
	 *	same chips - but the R/W bit is included in the address
	 *	argument so the numbers are different */
	

	printk(KERN_INFO "Initialising 12C508 PIC chip ...\n");

	/* First of all, enable the clock line. This is used in the PXC200-F */
	val = btread(BT848_GPIO_DMA_CTL);
	val |= BT848_GPIO_DMA_CTL_GPCLKMODE;
	btwrite(val, BT848_GPIO_DMA_CTL);
	
	/* Then, push to 0 the reset pin long enough to reset the *
	 * device same as above for the reset line, but not the same
	 * value sent to the GPIO-connected stuff
	 * which one is the good one? */
	btwrite( (1<<2), BT848_GPIO_OUT_EN); /* only the reset pin */
	btwrite(0, BT848_GPIO_DATA);
	udelay(10);
	btwrite(1<<2, BT848_GPIO_DATA);
/*
       	for (i = 0; i < ARRAY_SIZE(vals); i++) {
		tmp=bttv_I2CWrite(btv,0x1E,0,vals[i],1);
		if (tmp != -1) {
			printk(KERN_INFO
			       "I2C Write(%2.2x) = %i\nI2C Read () = %2.2x\n\n",
			       vals[i],tmp,bttv_I2CRead(btv,0x1F,NULL));
		}
	}
*/
	printk(KERN_INFO "PXC200 Initialised.\n");
}


/* ----------------------------------------------------------------------- */
/* Miro Pro radio stuff -- the tea5757 is connected to some GPIO ports     */
/*
 * Copyright (c) 1999 Csaba Halasz <qgehali@uni-miskolc.hu>
 * This code is placed under the terms of the GNU General Public License
 *
 * Brutally hacked by Dan Sheridan <dan.sheridan@contact.org.uk> djs52 8/3/00
 */

void bus_low(struct bttv *btv, int bit)
{
	if (btv->mbox_ior) {
		btor(btv->mbox_ior | btv->mbox_iow | btv->mbox_csel,
		     BT848_GPIO_DATA);
		udelay(5);
	}

	btand(~(bit), BT848_GPIO_DATA);
	udelay(5);

	if (btv->mbox_ior) {
		btand(~(btv->mbox_iow | btv->mbox_csel),
		      BT848_GPIO_DATA);
		udelay(5);
	}
}

void bus_high(struct bttv *btv, int bit)
{
	if (btv->mbox_ior) {
		btor(btv->mbox_ior | btv->mbox_iow | btv->mbox_csel,
		     BT848_GPIO_DATA);
		udelay(5);
	}

	btor((bit), BT848_GPIO_DATA);
	udelay(5);

	if (btv->mbox_ior) {
		btand(~(btv->mbox_iow | btv->mbox_csel),
		      BT848_GPIO_DATA);
		udelay(5);
	}
}

int bus_in(struct bttv *btv, int bit)
{
	if (btv->mbox_ior) {
		btor(btv->mbox_ior | btv->mbox_iow | btv->mbox_csel,
		     BT848_GPIO_DATA);
		udelay(5);

		btand(~(btv->mbox_ior | btv->mbox_csel),
		      BT848_GPIO_DATA);
		udelay(5);
	}
	return btread(BT848_GPIO_DATA) & (bit);
}

/* TEA5757 register bits */
#define TEA_FREQ		0:14
#define TEA_BUFFER		15:15

#define TEA_SIGNAL_STRENGTH	16:17

#define TEA_PORT1		18:18
#define TEA_PORT0		19:19

#define TEA_BAND		20:21
#define TEA_BAND_FM		0
#define TEA_BAND_MW		1
#define TEA_BAND_LW		2
#define TEA_BAND_SW		3

#define TEA_MONO		22:22
#define TEA_ALLOW_STEREO	0
#define TEA_FORCE_MONO		1

#define TEA_SEARCH_DIRECTION	23:23
#define TEA_SEARCH_DOWN		0
#define TEA_SEARCH_UP		1

#define TEA_STATUS		24:24
#define TEA_STATUS_TUNED	0
#define TEA_STATUS_SEARCHING	1

extern unsigned long read_time(void);

/* Low-level stuff */
static int tea5757_read(struct bttv *btv)
{
	unsigned long timeout;
	int value = 0;
	int i;
	
	/* better safe than sorry */
	btaor((btv->mbox_clk | btv->mbox_we),
	      ~btv->mbox_mask, BT848_GPIO_OUT_EN);

	if (btv->mbox_ior) {
		btor(btv->mbox_ior | btv->mbox_iow | btv->mbox_csel,
		     BT848_GPIO_DATA);
		udelay(5);
	}

	if (bttv_gpio)
		bttv_gpio_tracking(btv,"tea5757 read");
	
	bus_low(btv,btv->mbox_we);
	bus_low(btv,btv->mbox_clk);
	
	udelay(10);
	timeout= read_time() + 1000000;

	// wait for DATA line to go low; error if it doesn't
	while (bus_in(btv,btv->mbox_data) && timeout > read_time());
	if (bus_in(btv,btv->mbox_data)) {
		printk(KERN_WARNING "bttv%d: tea5757: read timeout\n",btv->nr);
		return -1;
	}

	dprintk("bttv%d: tea5757:",btv->nr);
	for(i = 0; i < 24; i++)
	{
		udelay(5);
		bus_high(btv,btv->mbox_clk);
		udelay(5);
		dprintk("%c",(bus_in(btv,btv->mbox_most) == 0)?'T':'-');
		bus_low(btv,btv->mbox_clk);
		value <<= 1;
		value |= (bus_in(btv,btv->mbox_data) == 0)?0:1;  /* MSB first */
		dprintk("%c", (bus_in(btv,btv->mbox_most) == 0)?'S':'M');
	}
	dprintk("\nbttv%d: tea5757: read 0x%X\n", btv->nr, value);
	return value;
}

static int tea5757_write(struct bttv *btv, int value)
{
	int i;
	int reg = value;
	
	btaor(btv->mbox_clk | btv->mbox_we | btv->mbox_data,
	      ~btv->mbox_mask, BT848_GPIO_OUT_EN);

	if (btv->mbox_ior) {
		btor(btv->mbox_ior | btv->mbox_iow | btv->mbox_csel,
		     BT848_GPIO_DATA);
		udelay(5);
	}
	if (bttv_gpio)
		bttv_gpio_tracking(btv,"tea5757 write");

	dprintk("bttv%d: tea5757: write 0x%X\n", btv->nr, value);
	bus_low(btv,btv->mbox_clk);
	bus_high(btv,btv->mbox_we);
	for(i = 0; i < 25; i++)
	{
		if (reg & 0x1000000)
			bus_high(btv,btv->mbox_data);
		else
			bus_low(btv,btv->mbox_data);
		reg <<= 1;
		bus_high(btv,btv->mbox_clk);
		udelay(10);
		bus_low(btv,btv->mbox_clk);
		udelay(10);
	}
	bus_low(btv,btv->mbox_we);  /* unmute !!! */
	return 0;
}

void tea5757_set_freq(struct bttv *btv, unsigned short freq)
{
	dprintk("tea5757_set_freq %d\n",freq);
	tea5757_write(btv, 5 * freq + 0x358); /* add 10.7MHz (see docs) */
}


/* ----------------------------------------------------------------------- */
/* winview                                                                 */

void winview_audio(struct bttv *btv, struct video_audio *v, int set)
{
	/* PT2254A programming Jon Tombs, jon@gte.esi.us.es */
	int bits_out, loops, vol, data;

	if (!set) {
		/* Fixed by Leandro Lucarella <luca@linuxmendoza.org.ar (07/31/01) */
		v->flags |= VIDEO_AUDIO_VOLUME;
		return;
	}
	
	/* 32 levels logarithmic */
	vol = 32 - ((v->volume>>11));
	/* units */
	bits_out = (PT2254_DBS_IN_2>>(vol%5));
	/* tens */
	bits_out |= (PT2254_DBS_IN_10>>(vol/5));
	bits_out |= PT2254_L_CHANNEL | PT2254_R_CHANNEL;
	data = btread(BT848_GPIO_DATA);
	data &= ~(WINVIEW_PT2254_CLK| WINVIEW_PT2254_DATA|
		  WINVIEW_PT2254_STROBE);
	for (loops = 17; loops >= 0 ; loops--) {
		if (bits_out & (1<<loops))
			data |=  WINVIEW_PT2254_DATA;
		else
			data &= ~WINVIEW_PT2254_DATA;
		btwrite(data, BT848_GPIO_DATA);
		udelay(5);
		data |= WINVIEW_PT2254_CLK;
		btwrite(data, BT848_GPIO_DATA);
		udelay(5);
		data &= ~WINVIEW_PT2254_CLK;
		btwrite(data, BT848_GPIO_DATA);
	}
	data |=  WINVIEW_PT2254_STROBE;
	data &= ~WINVIEW_PT2254_DATA;
	btwrite(data, BT848_GPIO_DATA);
	udelay(10);                     
	data &= ~WINVIEW_PT2254_STROBE;
	btwrite(data, BT848_GPIO_DATA);
}

/* ----------------------------------------------------------------------- */
/* mono/stereo control for various cards (which don't use i2c chips but    */
/* connect something to the GPIO pins                                      */

static void
gvbctv3pci_audio(struct bttv *btv, struct video_audio *v, int set)
{
	unsigned int con = 0;

	if (set) {
		btor(0x300, BT848_GPIO_OUT_EN);
		if (v->mode & VIDEO_SOUND_LANG1)
			con = 0x000;
		if (v->mode & VIDEO_SOUND_LANG2)
			con = 0x300;
		if (v->mode & VIDEO_SOUND_STEREO)
			con = 0x200;
//		if (v->mode & VIDEO_SOUND_MONO)
//			con = 0x100;
		btaor(con, ~0x300, BT848_GPIO_DATA);
	} else {
		v->mode = VIDEO_SOUND_STEREO |
			  VIDEO_SOUND_LANG1  | VIDEO_SOUND_LANG2;
	}
}

/*
 * Mario Medina Nussbaum <medisoft@alohabbs.org.mx>
 *  I discover that on BT848_GPIO_DATA address a byte 0xcce enable stereo,
 *  0xdde enables mono and 0xccd enables sap
 *
 * Petr Vandrovec <VANDROVE@vc.cvut.cz>
 *  P.S.: At least mask in line above is wrong - GPIO pins 3,2 select
 *  input/output sound connection, so both must be set for output mode.
 *
 * Looks like it's needed only for the "tvphone", the "tvphone 98"
 * handles this with a tda9840
 *
 */
static void
avermedia_tvphone_audio(struct bttv *btv, struct video_audio *v, int set)
{
	int val = 0;

	if (set) {
		if (v->mode & VIDEO_SOUND_LANG1)   /* SAP */
			val = 0x02;
		if (v->mode & VIDEO_SOUND_STEREO)
			val = 0x01;
		if (val) {
			btaor(val, ~0x03, BT848_GPIO_DATA);
			if (bttv_gpio)
				bttv_gpio_tracking(btv,"avermedia");
		}
	} else {
		v->mode = VIDEO_SOUND_MONO | VIDEO_SOUND_STEREO |
			VIDEO_SOUND_LANG1;
		return;
	}
}

/* Lifetec 9415 handling */
static void
lt9415_audio(struct bttv *btv, struct video_audio *v, int set)
{
        int val = 0;

        if (btread(BT848_GPIO_DATA) & 0x4000) {
		v->mode = VIDEO_SOUND_MONO;
		return;
	}

        if (set) {
                if (v->mode & VIDEO_SOUND_LANG2)  /* A2 SAP */
                        val = 0x0080;
		if (v->mode & VIDEO_SOUND_STEREO) /* A2 stereo */
                        val = 0x0880;
                if ((v->mode & VIDEO_SOUND_LANG1) ||
		    (v->mode & VIDEO_SOUND_MONO))
			val = 0;
                btaor(val, ~0x0880, BT848_GPIO_DATA);
                if (bttv_gpio)
                        bttv_gpio_tracking(btv,"lt9415");
        } else {
		/* autodetect doesn't work with this card :-( */
                v->mode = VIDEO_SOUND_MONO | VIDEO_SOUND_STEREO |
			VIDEO_SOUND_LANG1 | VIDEO_SOUND_LANG2;
                return;
        }
}

// TDA9821 on TerraTV+ Bt848, Bt878
static void
terratv_audio(struct bttv *btv, struct video_audio *v, int set)
{
	unsigned int con = 0;

	if (set) {
		btor(0x180000, BT848_GPIO_OUT_EN);
		if (v->mode & VIDEO_SOUND_LANG2)
			con = 0x080000;
		if (v->mode & VIDEO_SOUND_STEREO)
			con = 0x180000;
		btaor(con, ~0x180000, BT848_GPIO_DATA);
		if (bttv_gpio)
			bttv_gpio_tracking(btv,"terratv");
	} else {
		v->mode = VIDEO_SOUND_MONO | VIDEO_SOUND_STEREO |
			VIDEO_SOUND_LANG1 | VIDEO_SOUND_LANG2;
	}
}

static void
winfast2000_audio(struct bttv *btv, struct video_audio *v, int set)
{
	unsigned long val = 0;

	if (set) {
		/*btor (0xc32000, BT848_GPIO_OUT_EN);*/
		if (v->mode & VIDEO_SOUND_MONO)		/* Mono */
			val = 0x420000;
		if (v->mode & VIDEO_SOUND_LANG1)	/* Mono */
			val = 0x420000;
		if (v->mode & VIDEO_SOUND_LANG2)	/* SAP */
			val = 0x410000;
		if (v->mode & VIDEO_SOUND_STEREO)	/* Stereo */
			val = 0x020000;
		if (val) {
			btaor(val, ~0x430000, BT848_GPIO_DATA);
			if (bttv_gpio)
				bttv_gpio_tracking(btv,"winfast2000");
		}
	} else {
		v->mode = VIDEO_SOUND_MONO | VIDEO_SOUND_STEREO |
			  VIDEO_SOUND_LANG1 | VIDEO_SOUND_LANG2;
	}
}

/*
 * Dariusz Kowalewski <darekk@automex.pl>
 * sound control for Prolink PV-BT878P+9B (PixelView PlayTV Pro FM+NICAM
 * revision 9B has on-board TDA9874A sound decoder).
 *
 * Note: There are card variants without tda9874a. Forcing the "stereo sound route"
 *       will mute this cards.
 */
static void
pvbt878p9b_audio(struct bttv *btv, struct video_audio *v, int set)
{
	unsigned int val = 0;

	if (btv->radio_user)
		return;

	if (set) {
		if (v->mode & VIDEO_SOUND_MONO)	{
			val = 0x01;
		}
		if ((v->mode & (VIDEO_SOUND_LANG1 | VIDEO_SOUND_LANG2))
		    || (v->mode & VIDEO_SOUND_STEREO)) {
			val = 0x02;
		}
		if (val) {
			btaor(val, ~0x03, BT848_GPIO_DATA);
			if (bttv_gpio)
				bttv_gpio_tracking(btv,"pvbt878p9b");
		}
	} else {
		v->mode = VIDEO_SOUND_MONO | VIDEO_SOUND_STEREO |
			VIDEO_SOUND_LANG1 | VIDEO_SOUND_LANG2;
	}
}

/*
 * Dariusz Kowalewski <darekk@automex.pl>
 * sound control for FlyVideo 2000S (with tda9874 decoder) 
 * based on pvbt878p9b_audio() - this is not tested, please fix!!!
 */
static void
fv2000s_audio(struct bttv *btv, struct video_audio *v, int set)
{
	unsigned int val = 0xffff;

	if (btv->radio_user)
		return;
	if (set) {
		if (v->mode & VIDEO_SOUND_MONO)	{
			val = 0x0000;
		}
		if ((v->mode & (VIDEO_SOUND_LANG1 | VIDEO_SOUND_LANG2))
		    || (v->mode & VIDEO_SOUND_STEREO)) {
			val = 0x1080; //-dk-???: 0x0880, 0x0080, 0x1800 ...
		}
		if (val != 0xffff) {
			btaor(val, ~0x1800, BT848_GPIO_DATA);
			if (bttv_gpio)
				bttv_gpio_tracking(btv,"fv2000s");
		}
	} else {
		v->mode = VIDEO_SOUND_MONO | VIDEO_SOUND_STEREO |
			VIDEO_SOUND_LANG1 | VIDEO_SOUND_LANG2;
	}
}

/*
 * sound control for Canopus WinDVR PCI
 * Masaki Suzuki <masaki@btree.org>
 */
static void
windvr_audio(struct bttv *btv, struct video_audio *v, int set)
{
        unsigned long val = 0;

        if (set) {
                if (v->mode & VIDEO_SOUND_MONO)
                        val = 0x040000;
                if (v->mode & VIDEO_SOUND_LANG1)
                        val = 0;
                if (v->mode & VIDEO_SOUND_LANG2)
                        val = 0x100000;
                if (v->mode & VIDEO_SOUND_STEREO)
                        val = 0;
                if (val) {
                        btaor(val, ~0x140000, BT848_GPIO_DATA);
                        if (bttv_gpio)
                                bttv_gpio_tracking(btv,"windvr");
                }
        } else {
                v->mode = VIDEO_SOUND_MONO | VIDEO_SOUND_STEREO |
                          VIDEO_SOUND_LANG1 | VIDEO_SOUND_LANG2;
        }
}

/*
 * sound control for AD-TVK503
 * Hiroshi Takekawa <sian@big.or.jp>
 */
static void
adtvk503_audio(struct bttv *btv, struct video_audio *v, int set)
{
	unsigned int con = 0xffffff;

	//btaor(0x1e0000, ~0x1e0000, BT848_GPIO_OUT_EN);

	if (set) {
		//btor(***, BT848_GPIO_OUT_EN);
		if (v->mode & VIDEO_SOUND_LANG1)
			con = 0x00000000;
		if (v->mode & VIDEO_SOUND_LANG2)
			con = 0x00180000;
		if (v->mode & VIDEO_SOUND_STEREO)
			con = 0x00000000;
		if (v->mode & VIDEO_SOUND_MONO)
			con = 0x00060000;
		if (con != 0xffffff) {
			btaor(con, ~0x1e0000, BT848_GPIO_DATA);
			if (bttv_gpio)
				bttv_gpio_tracking(btv, "adtvk503");
		}
	} else {
		v->mode = VIDEO_SOUND_MONO | VIDEO_SOUND_STEREO |
			  VIDEO_SOUND_LANG1  | VIDEO_SOUND_LANG2;
	}
}

/* RemoteVision MX (rv605) muxsel helper [Miguel Freitas]
 *
 * This is needed because rv605 don't use a normal multiplex, but a crosspoint
 * switch instead (CD22M3494E). This IC can have multiple active connections
 * between Xn (input) and Yn (output) pins. We need to clear any existing
 * connection prior to establish a new one, pulsing the STROBE pin.
 *
 * The board hardwire Y0 (xpoint) to MUX1 and MUXOUT to Yin.
 * GPIO pins are wired as:
 *  GPIO[0:3] - AX[0:3] (xpoint) - P1[0:3] (microcontroler)
 *  GPIO[4:6] - AY[0:2] (xpoint) - P1[4:6] (microcontroler)
 *  GPIO[7]   - DATA (xpoint)    - P1[7] (microcontroler)
 *  GPIO[8]   -                  - P3[5] (microcontroler)
 *  GPIO[9]   - RESET (xpoint)   - P3[6] (microcontroler)
 *  GPIO[10]  - STROBE (xpoint)  - P3[7] (microcontroler)
 *  GPINTR    -                  - P3[4] (microcontroler)
 *
 * The microcontroler is a 80C32 like. It should be possible to change xpoint
 * configuration either directly (as we are doing) or using the microcontroler
 * which is also wired to I2C interface. I have no further info on the
 * microcontroler features, one would need to disassembly the firmware.
 * note: the vendor refused to give any information on this product, all
 *       that stuff was found using a multimeter! :)
 */
static void rv605_muxsel(struct bttv *btv, unsigned int input)
{
	/* reset all conections */
	btaor(0x200,~0x200, BT848_GPIO_DATA);
	mdelay(1);
	btaor(0x000,~0x200, BT848_GPIO_DATA);
	mdelay(1);

	/* create a new conection */
	btaor(0x080,~0x480, BT848_GPIO_DATA);
	btaor(0x480,~0x480, BT848_GPIO_DATA);
	mdelay(1);
	btaor(0x080,~0x480, BT848_GPIO_DATA);
	mdelay(1);
}

// The Grandtec X-Guard framegrabber card uses two Dual 4-channel
// video multiplexers to provide up to 16 video inputs. These
// multiplexers are controlled by the lower 8 GPIO pins of the
// bt878. The multiplexers probably Pericom PI5V331Q or similar.

// xxx0 is pin xxx of multiplexer U5,
// yyy1 is pin yyy of multiplexer U2

#define ENA0    0x01
#define ENB0    0x02
#define ENA1    0x04
#define ENB1    0x08

#define IN10    0x10
#define IN00    0x20
#define IN11    0x40
#define IN01    0x80

static void xguard_muxsel(struct bttv *btv, unsigned int input)
{
	static const int masks[] = {
                ENB0, ENB0|IN00, ENB0|IN10, ENB0|IN00|IN10,
                ENA0, ENA0|IN00, ENA0|IN10, ENA0|IN00|IN10,
                ENB1, ENB1|IN01, ENB1|IN11, ENB1|IN01|IN11,
                ENA1, ENA1|IN01, ENA1|IN11, ENA1|IN01|IN11,
	};
        btwrite(masks[input%16], BT848_GPIO_DATA);
}

/*
 * ivc120_muxsel [Added by Alan Garfield <alan@fromorbit.com>]
 *
 * The IVC120G security card has 4 i2c controlled TDA8540 matrix
 * swichers to provide 16 channels to MUX0. The TDA8540's have 
 * 4 indepedant outputs and as such the IVC120G also has the 
 * optional "Monitor Out" bus. This allows the card to be looking 
 * at one input while the monitor is looking at another.
 *
 * Since I've couldn't be bothered figuring out how to add an
 * independant muxsel for the monitor bus, I've just set it to 
 * whatever the card is looking at.
 *
 *  OUT0 of the TDA8540's is connected to MUX0         (0x03)
 *  OUT1 of the TDA8540's is connected to "Monitor Out"        (0x0C)
 *
 *  TDA8540_ALT3 IN0-3 = Channel 13 - 16       (0x03)
 *  TDA8540_ALT4 IN0-3 = Channel 1 - 4         (0x03)
 *  TDA8540_ALT5 IN0-3 = Channel 5 - 8         (0x03)
 *  TDA8540_ALT6 IN0-3 = Channel 9 - 12                (0x03)
 *
 */

/* All 7 possible sub-ids for the TDA8540 Matrix Switcher */
#define I2C_TDA8540        0x90
#define I2C_TDA8540_ALT1   0x92
#define I2C_TDA8540_ALT2   0x94
#define I2C_TDA8540_ALT3   0x96
#define I2C_TDA8540_ALT4   0x98
#define I2C_TDA8540_ALT5   0x9a
#define I2C_TDA8540_ALT6   0x9c

static void ivc120_muxsel(struct bttv *btv, unsigned int input)
{
	// Simple maths
	int key = input % 4;    
	int matrix = input / 4;
	
	dprintk("bttv%d: ivc120_muxsel: Input - %02d | TDA - %02d | In - %02d\n",
		btv->nr, input, matrix, key);
	
	// Handles the input selection on the TDA8540's
	bttv_I2CWrite(btv, I2C_TDA8540_ALT3, 0x00,
		      ((matrix == 3) ? (key | key << 2) : 0x00), 1);
	bttv_I2CWrite(btv, I2C_TDA8540_ALT4, 0x00,
		      ((matrix == 0) ? (key | key << 2) : 0x00), 1);
	bttv_I2CWrite(btv, I2C_TDA8540_ALT5, 0x00,
		      ((matrix == 1) ? (key | key << 2) : 0x00), 1);
	bttv_I2CWrite(btv, I2C_TDA8540_ALT6, 0x00,
		      ((matrix == 2) ? (key | key << 2) : 0x00), 1);
	
	// Handles the output enables on the TDA8540's
	bttv_I2CWrite(btv, I2C_TDA8540_ALT3, 0x02,
		      ((matrix == 3) ? 0x03 : 0x00), 1);  // 13 - 16
	bttv_I2CWrite(btv, I2C_TDA8540_ALT4, 0x02,
		      ((matrix == 0) ? 0x03 : 0x00), 1);  // 1-4
	bttv_I2CWrite(btv, I2C_TDA8540_ALT5, 0x02,
		      ((matrix == 1) ? 0x03 : 0x00), 1);  // 5-8 
	bttv_I2CWrite(btv, I2C_TDA8540_ALT6, 0x02,
		      ((matrix == 2) ? 0x03 : 0x00), 1);  // 9-12
	
	// Selects MUX0 for input on the 878
	btaor((0)<<5, ~(3<<5), BT848_IFORM);
}


/* ----------------------------------------------------------------------- */
/* motherboard chipset specific stuff                                      */

void bttv_check_chipset(void)
{
	int pcipci_fail = 0;
	struct pci_device dev;
	int pci_pci_problems = 0;

	if (pci_pci_problems & PCIPCI_FAIL)
		pcipci_fail = 1;
	if (pci_pci_problems & (PCIPCI_TRITON|PCIPCI_NATOMA|PCIPCI_VIAETBF))
		triton1 = 1;
	if (pci_pci_problems & PCIPCI_VSFX)
		vsfx = 1;

	/* print warnings about any quirks found */
	if (triton1)
		printk(KERN_INFO "bttv: Host bridge needs ETBF enabled.\n");
	if (vsfx)
		printk(KERN_INFO "bttv: Host bridge needs VSFX enabled.\n");
	if (pcipci_fail) {
		printk(KERN_WARNING "bttv: BT848 and your chipset may not work together.\n");
		if (UNSET == no_overlay) {
			printk(KERN_WARNING "bttv: going to disable overlay.\n");
			no_overlay = 1;
		}
	}
	if (UNSET != latency)
		printk(KERN_INFO "bttv: pci latency fixup [%d]\n",latency);

	if(0==pci_find_device(PCI_VENDOR_ID_INTEL,PCI_DEVICE_ID_INTEL_82441,NULL,&dev)) {
                unsigned char b;
		pci_read_config_byte(&dev, 0x53, &b);
		if (bttv_debug)
			printk(KERN_INFO "bttv: Host bridge: 82441FX Natoma, "
			       "bufcon=0x%02x\n",b);
	}
}

int bttv_handle_chipset(struct bttv *btv)
{
 	unsigned char command;

	if (!triton1 && !vsfx && UNSET == latency)
		return 0;

	if (bttv_verbose) {
		if (triton1)
			printk(KERN_INFO "bttv%d: enabling ETBF (430FX/VP3 compatibilty)\n",btv->nr);
		if (vsfx && btv->id >= 878)
			printk(KERN_INFO "bttv%d: enabling VSFX\n",btv->nr);
		if (UNSET != latency)
			printk(KERN_INFO "bttv%d: setting pci timer to %d\n",
			       btv->nr,latency);
	}

	if (btv->id < 878) {
		/* bt848 (mis)uses a bit in the irq mask for etbf */
		if (triton1)
			btv->triton1 = BT848_INT_ETBF;
	} else {
		/* bt878 has a bit in the pci config space for it */
                pci_read_config_byte(btv->bt_dev, BT878_DEVCTRL, &command);
		if (triton1)
			command |= BT878_EN_TBFX;
		if (vsfx)
			command |= BT878_EN_VSFX;
                pci_write_config_byte(btv->bt_dev, BT878_DEVCTRL, command);
        }
	if (UNSET != latency)
		pci_write_config_byte(btv->bt_dev, PCI_LATENCY_TIMER, latency);
	return 0;
}


/*
 * Local variables:
 * c-basic-offset: 8
 * End:
 */
