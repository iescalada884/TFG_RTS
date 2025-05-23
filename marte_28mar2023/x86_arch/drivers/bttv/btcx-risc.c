/*
    btcx-risc.c

    bt848/bt878/cx2388x risc code generator.

    (c) 2000-03 Gerd Knorr <kraxel@bytesex.org> [SuSE Labs]

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

#include <linux/pci.h>
#include <linux/videodev2.h>
#include <linux/errno.h>
#include "btcx-risc.h"

static unsigned int debug = 0;

/* ---------------------------------------------------------- */
/* allocate/free risc memory                                  */

static int memcnt;

extern void vfree_32(void *t);

void btcx_riscmem_free(struct pci_device *pci,
		       struct btcx_riscmem *risc)
{
	if (NULL == risc)
		return;
	if (NULL == risc->cpu)
		return;
	pci_free_consistent(pci, risc->size, risc->cpu, risc->dma);
	vfree((void *)risc->cpu);
	memset(risc,0,sizeof(*risc));
	if (debug) {
		memcnt--;
		printk("btcx: riscmem free [%d]\n",memcnt);
	}
}

int btcx_riscmem_alloc(struct pci_device *pci,
		       struct btcx_riscmem *risc,
		       unsigned int size)
{
	u32 *cpu;
	dma_addr_t dma;

	if (NULL != risc->cpu && risc->size < size)
		btcx_riscmem_free(pci,risc);
	if (NULL == risc->cpu) {
		cpu = pci_alloc_consistent(pci, size, &dma);
		if (NULL == cpu)
			return -ENOMEM;
		risc->cpu  = cpu;
		risc->dma  = dma;
		risc->size = size;
		if (debug) {
			memcnt++;
			printk("btcx: riscmem alloc size=%d [%d]\n",size,memcnt);
		}
	}
	memset(risc->cpu,0,risc->size);
	return 0;
}

/* ---------------------------------------------------------- */
/* screen overlay helpers                                     */

int
btcx_screen_clips(int swidth, int sheight, struct v4l2_rect *win,
		  struct v4l2_clip *clips, unsigned int n)
{
	if (win->left < 0) {
		/* left */
		clips[n].c.left = 0;
		clips[n].c.top = 0;
		clips[n].c.width  = -win->left;
		clips[n].c.height = win->height;
		n++;
	}
	if (win->left + win->width > swidth) {
		/* right */
		clips[n].c.left   = swidth - win->left;
		clips[n].c.top    = 0;
		clips[n].c.width  = win->width - clips[n].c.left;
		clips[n].c.height = win->height;
		n++;
	}
	if (win->top < 0) {
		/* top */
		clips[n].c.left = 0;
		clips[n].c.top = 0;
		clips[n].c.width  = win->width;
		clips[n].c.height = -win->top;
		n++;
	}
	if (win->top + win->height > sheight) {
		/* bottom */
		clips[n].c.left = 0;
		clips[n].c.top = sheight - win->top;
		clips[n].c.width  = win->width;
		clips[n].c.height = win->height - clips[n].c.top;
		n++;
	}
	return n;
}

int
btcx_align(struct v4l2_rect *win, struct v4l2_clip *clips, unsigned int n, int mask)
{
	s32 nx,nw,dx;
	unsigned int i;

	/* fixup window */
	nx = (win->left + mask) & ~mask;
	nw = (win->width) & ~mask;
	if (nx + nw > win->left + win->width)
		nw -= mask+1;
	dx = nx - win->left;
	win->left  = nx;
	win->width = nw;
	if (debug)
		printk(KERN_DEBUG "btcx: window align %lx%ld+%ld+%ld [dx=%ld]\n",
		       win->width, win->height, win->left, win->top, dx);

	/* fixup clips */
	for (i = 0; i < n; i++) {
		nx = (clips[i].c.left-dx) & ~mask;
		nw = (clips[i].c.width) & ~mask;
		if (nx + nw < clips[i].c.left-dx + clips[i].c.width)
			nw += mask+1;
		clips[i].c.left  = nx;
		clips[i].c.width = nw;
		if (debug)
			printk(KERN_DEBUG "btcx:   clip align %lx%ld+%ld+%ld\n",
			       clips[i].c.width, clips[i].c.height,
			       clips[i].c.left, clips[i].c.top);
	}
	return 0;
}

void
btcx_sort_clips(struct v4l2_clip *clips, unsigned int nclips)
{
	struct v4l2_clip swap;
	int i,j,n;

	if (nclips < 2)
		return;
	for (i = nclips-2; i >= 0; i--) {
		for (n = 0, j = 0; j <= i; j++) {
			if (clips[j].c.left > clips[j+1].c.left) {
				swap = clips[j];
				clips[j] = clips[j+1];
				clips[j+1] = swap;
				n++;
			}
		}
		if (0 == n)
			break;
	}
}

void
btcx_calc_skips(int line, int width, unsigned int *maxy,
		struct btcx_skiplist *skips, unsigned int *nskips,
		const struct v4l2_clip *clips, unsigned int nclips)
{
	unsigned int clip,skip;
	int end,maxline;
	
	skip=0;
	maxline = 9999;
	for (clip = 0; clip < nclips; clip++) {

		/* sanity checks */
		if (clips[clip].c.left + clips[clip].c.width <= 0)
			continue;
		if (clips[clip].c.left > (signed)width)
			break;
		
		/* vertical range */
		if (line > clips[clip].c.top+clips[clip].c.height-1)
			continue;
		if (line < clips[clip].c.top) {
			if (maxline > clips[clip].c.top-1)
				maxline = clips[clip].c.top-1;
			continue;
		}
		if (maxline > clips[clip].c.top+clips[clip].c.height-1)
			maxline = clips[clip].c.top+clips[clip].c.height-1;

		/* horizontal range */
		if (0 == skip || clips[clip].c.left > skips[skip-1].end) {
			/* new one */
			skips[skip].start = clips[clip].c.left;
			if (skips[skip].start < 0)
				skips[skip].start = 0;
			skips[skip].end = clips[clip].c.left + clips[clip].c.width;
			if (skips[skip].end > width)
				skips[skip].end = width;
			skip++;
		} else {
			/* overlaps -- expand last one */
			end = clips[clip].c.left + clips[clip].c.width;
			if (skips[skip-1].end < end)
				skips[skip-1].end = end;
			if (skips[skip-1].end > width)
				skips[skip-1].end = width;
		}
	}
	*nskips = skip;
	*maxy = maxline;

	if (debug) {
		printk(KERN_DEBUG "btcx: skips line %d-%d:",line,maxline);
		for (skip = 0; skip < *nskips; skip++) {
			printk(" %d-%d",skips[skip].start,skips[skip].end);
		}
		printk("\n");
	}
}

/*
 * Local variables:
 * c-basic-offset: 8
 * End:
 */
