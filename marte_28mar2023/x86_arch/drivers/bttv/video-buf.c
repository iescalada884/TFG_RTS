/*
 * generic helper functions for video4linux capture buffers, to handle
 * memory management and PCI DMA.  Right now bttv + saa7134 use it.
 *
 * The functions expect the hardware being able to scatter gatter
 * (i.e. the buffers are not linear in physical memory, but fragmented
 * into PAGE_SIZE chunks).  They also assume the driver does not need
 * to touch the video data (thus it is probably not useful for USB as
 * data often must be uncompressed by the drivers).
 * 
 * (c) 2001,02 Gerd Knorr <kraxel@bytesex.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include <compbttv.h>

#include <linux/pci.h>
#include <linux/errno.h>
//#include <linux/compiler.h>
#include <asm/page.h>
#include <debug_marte.h>

#include "video-buf.h"

static int debug = 1;

#define dprintk(level, fmt, arg...)	if (debug >= level) \
	printf("vbuf: " fmt, ## arg)

/*
struct scatterlist*
videobuf_vmalloc_to_sg(unsigned char *virt, int nr_pages)
{
	struct scatterlist *sglist;
	struct page *pg;
	int i;

	sglist = kmalloc(sizeof(struct scatterlist)*nr_pages, GFP_KERNEL);
	if (NULL == sglist)
		return NULL;
	memset(sglist,0,sizeof(struct scatterlist)*nr_pages);
	for (i = 0; i < nr_pages; i++, virt += PAGE_SIZE) {
		pg = vmalloc_to_page(virt);
		if (NULL == pg)
			goto err;
		if (PageHighMem(pg))
			BUG();
		sglist[i].page   = pg;
		sglist[i].length = PAGE_SIZE;
	}
	return sglist;
	
 err:
	kfree(sglist);
	return NULL;
}

struct scatterlist*
videobuf_pages_to_sg(struct page **pages, int nr_pages, int offset)
{
	struct scatterlist *sglist;
	int i = 0;

	if (NULL == pages[0])
		return NULL;
	sglist = kmalloc(sizeof(*sglist) * nr_pages, GFP_KERNEL);
	if (NULL == sglist)
		return NULL;
	memset(sglist, 0, sizeof(*sglist) * nr_pages);

	if (NULL == pages[0])
		goto nopage;
	if (PageHighMem(pages[0]))
		goto highmem;
	sglist[0].page   = pages[0];
	sglist[0].offset = offset;
	sglist[0].length = PAGE_SIZE - offset;
	for (i = 1; i < nr_pages; i++) {
		if (NULL == pages[i])
			goto nopage;
		if (PageHighMem(pages[i]))
			goto highmem;
		sglist[i].page   = pages[i];
		sglist[i].length = PAGE_SIZE;
	}
	return sglist;

 nopage:
	dprintk(2,"sgl: oops - no page\n");
	kfree(sglist);
	return NULL;

 highmem:
	dprintk(2,"sgl: oops - highmem page\n");
	kfree(sglist);
	return NULL;
}
*/
int videobuf_lock(struct page **pages, int nr_pages)
{
	int i = 0;
  set_break_point_here;

	dprintk(2,"lock start ...\n");
	//for (i = 0; i < nr_pages; i++)
	//	if (TryLockPage(pages[i]))
	//		goto err;
	dprintk(2,"lock ok [%d pages]\n",nr_pages);
	return 0;

// err:
	dprintk(2,"lock failed, unlock ...\n");
	while (i > 0)
		//unlock_page(pages[--i]);
	dprintk(2,"lock quit\n");
	return -EINVAL;
}

int videobuf_unlock(struct page **pages, int nr_pages)
{
	//int i;
  set_break_point_here;

	dprintk(2,"unlock start ...\n");
	//for (i = 0; i < nr_pages; i++)
	//	unlock_page(pages[i]);
	dprintk(2,"unlock ok [%d pages]\n",nr_pages);
	return 0;
}

/* --------------------------------------------------------------------- */

int videobuf_dma_init_user(struct videobuf_dmabuf *dma, int direction,
			   unsigned long data, unsigned long size)
{
	unsigned long first,last;
	int err;//, rw = 0;
  set_break_point_here;

	dma->direction = direction;
	switch (dma->direction) {
		//	case PCI_DMA_FROMDEVICE: rw = READ;  break;
		//	case PCI_DMA_TODEVICE:   rw = WRITE; break;
	default:                 BUG();
	}

	first = (data          & PAGE_MASK) >> PAGE_SHIFT;
	last  = ((data+size-1) & PAGE_MASK) >> PAGE_SHIFT;
	dma->offset   = data & ~PAGE_MASK;
	dma->nr_pages = last-first+1;
	dma->pages = kmalloc(dma->nr_pages * sizeof(struct page*),
			     GFP_KERNEL);
	if (NULL == dma->pages)
		return -ENOMEM;
	dprintk(1,"init user [0x%lx+0x%lx => %d pages]\n",
		data,size,dma->nr_pages);

	//down_read(&current->mm->mmap_sem);
        err = dma->nr_pages;//get_user_pages(current,current->mm,
		//	     data & PAGE_MASK, dma->nr_pages,
		//	     rw == READ, 1, /* force */
		//	     dma->pages, NULL);
	//up_read(&current->mm->mmap_sem);
	if (err != dma->nr_pages) {
		dma->nr_pages = (err >= 0) ? err : 0;
		dprintk(1,"get_user_pages: err=%d [%d]\n",err,dma->nr_pages);
		return err < 0 ? err : -EINVAL;
	}
	return 0;
}

int videobuf_dma_init_kernel(struct videobuf_dmabuf *dma, int direction,
			     int nr_pages)
{
  set_break_point_here;
	dprintk(1,"init kernel [%d pages]\n",nr_pages);
	dma->direction = direction;
	dma->vmalloc = vmalloc_32(nr_pages << PAGE_SHIFT);
	if (NULL == dma->vmalloc) {
		dprintk(1,"vmalloc_32(%d pages) failed\n",nr_pages);
		return -ENOMEM;
	}
	memset(dma->vmalloc,0,nr_pages << PAGE_SHIFT);
	dma->nr_pages = nr_pages;
	return 0;
}

int videobuf_dma_init_overlay(struct videobuf_dmabuf *dma, int direction,
			      dma_addr_t addr, int nr_pages)
{
  set_break_point_here;
	dprintk(1,"init overlay [%d pages @ bus 0x%lx]\n",
		nr_pages,(unsigned long)addr);
	dma->direction = direction;
	if (0 == addr)
		return -EINVAL;

	dma->bus_addr = addr;
	dma->nr_pages = nr_pages;
	return 0;
}
/*
int videobuf_dma_pci_map(struct pci_dev *dev, struct videobuf_dmabuf *dma)
{
	int err;

	if (0 == dma->nr_pages)
		BUG();
	
	if (dma->pages) {
		if (0 != (err = videobuf_lock(dma->pages, dma->nr_pages))) {
			dprintk(1,"videobuf_lock: %d\n",err);
			return err;
		}
		dma->sglist = videobuf_pages_to_sg(dma->pages, dma->nr_pages,
						   dma->offset);
		if (NULL == dma->sglist)
			videobuf_unlock(dma->pages, dma->nr_pages);
	}
	if (dma->vmalloc) {
		dma->sglist = videobuf_vmalloc_to_sg
			(dma->vmalloc,dma->nr_pages);
	}
	if (dma->bus_addr) {
		dma->sglist = kmalloc(sizeof(struct scatterlist), GFP_KERNEL);
		if (NULL != dma->sglist) {
			dma->sglen  = 1;
			sg_dma_address(&dma->sglist[0]) = dma->bus_addr & ~PAGE_MASK;
			dma->sglist[0].offset           = dma->bus_addr & PAGE_MASK;
			sg_dma_len(&dma->sglist[0])     = dma->nr_pages * PAGE_SIZE;
		}
	}
	if (NULL == dma->sglist) {
		dprintk(1,"scatterlist is NULL\n");
		return -ENOMEM;
	}

	if (!dma->bus_addr)
		dma->sglen = pci_map_sg(dev,dma->sglist,dma->nr_pages,
					dma->direction);
	return 0;
}

int videobuf_dma_pci_sync(struct pci_dev *dev, struct videobuf_dmabuf *dma)
{
	if (!dma->sglen)
		BUG();

	if (!dma->bus_addr)
		pci_dma_sync_sg(dev,dma->sglist,dma->nr_pages,dma->direction);
	return 0;
}

int videobuf_dma_pci_unmap(struct pci_dev *dev, struct videobuf_dmabuf *dma)
{
	if (!dma->sglen)
		return 0;

	if (!dma->bus_addr)
		pci_unmap_sg(dev,dma->sglist,dma->nr_pages,dma->direction);
	kfree(dma->sglist);
	dma->sglist = NULL;
	dma->sglen = 0;
	if (dma->pages)
		videobuf_unlock(dma->pages, dma->nr_pages);
	return 0;
}

int videobuf_dma_free(struct videobuf_dmabuf *dma)
{
	if (dma->sglen)
		BUG();

	if (dma->pages) {
		int i;
		for (i=0; i < dma->nr_pages; i++)
			page_cache_release(dma->pages[i]);
		kfree(dma->pages);
		dma->pages = NULL;
	}
	if (dma->vmalloc) {
		vfree(dma->vmalloc);
		dma->vmalloc = NULL;
	}
	if (dma->bus_addr) {
		dma->bus_addr = 0;
	}
	dma->direction = PCI_DMA_NONE;
	return 0;
}
*/
/* --------------------------------------------------------------------- */

void* videobuf_alloc(unsigned int size)
{

	struct videobuf_buffer *vb;
	//  set_break_point_here;

	vb = kmalloc(size,GFP_KERNEL);
	if (NULL != vb) {
		memset(vb,0,size);
		//init_waitqueue_head(&vb->done);
	}
	return vb;
}

int videobuf_waiton(struct videobuf_buffer *vb, int intr)
{
	int retval = 0;
	/*
	DECLARE_WAITQUEUE(wait, current);
	
	add_wait_queue(&vb->done, &wait);
	while (vb->state == STATE_ACTIVE || vb->state == STATE_QUEUED) {
		if (non_blocking) {
			retval = -EAGAIN;
			break;
		}
		set_current_state(intr ? TASK_INTERRUPTIBLE :
					TASK_UNINTERRUPTIBLE);
		if (vb->state == STATE_ACTIVE || vb->state == STATE_QUEUED)
			schedule();
		set_current_state(TASK_RUNNING);
		if (intr && signal_pending(current)) {
			dprintk(1,"buffer waiton: -EINTR\n");
			retval = -EINTR;
			break;
		}
	}
	remove_wait_queue(&vb->done, &wait);
	*/
  set_break_point_here;
	return retval;
}

int
videobuf_iolock(struct pci_device *pci, struct videobuf_buffer *vb,
		struct v4l2_multiple_framebuffer *mfbuf)
{
	int err,pages;
	dma_addr_t bus;

  set_break_point_here;
	switch (vb->memory) {
	case V4L2_MEMORY_MMAP:
	case V4L2_MEMORY_USERPTR:
		if (0 == vb->baddr) {
			/* no userspace addr -- kernel bounce buffer */
			pages = PAGE_ALIGN(vb->size) >> PAGE_SHIFT;
			err = 0;//videobuf_dma_init_kernel(&vb->dma,PCI_DMA_FROMDEVICE,
				//		       pages);
			if (0 != err)
				return err;
		} else {
			/* dma directly to userspace */
			err = 0;//videobuf_dma_init_user(&vb->dma,PCI_DMA_FROMDEVICE,
				//		     vb->baddr,vb->bsize);
			if (0 != err)
				return err;
		}
		break;
	case V4L2_MEMORY_OVERLAY:
		if (NULL == mfbuf)
			return -EINVAL;
		/* FIXME: need sanity checks for vb->boff */
		bus   = (dma_addr_t)mfbuf->base[0] + vb->boff;
		pages = PAGE_ALIGN(vb->size) >> PAGE_SHIFT;
		err = 0;//videobuf_dma_init_overlay(&vb->dma,PCI_DMA_FROMDEVICE,
			//			bus, pages);
		if (0 != err)
			return err;
		break;
	default:
		BUG();
	}
	err = 0;//videobuf_dma_pci_map(pci,&vb->dma);
	if (0 != err)
		return err;
		
	return 0;
}

/* --------------------------------------------------------------------- */

void
videobuf_queue_init(struct videobuf_queue *q,
		    struct videobuf_queue_ops *ops,
		    struct pci_device *pci,   //    spinlock_t *irqlock,
		    enum v4l2_buf_type type,
		    enum v4l2_field field,
		    unsigned int msize)
{
	//  set_break_point_here;
	memset(q,0,sizeof(*q));

	//	q->irqlock = irqlock;
	q->pci     = pci;
	q->type    = type;
	q->field   = field;
	q->msize   = msize;
	q->ops     = ops;

	//init_MUTEX(&q->lock);
	INIT_LIST_HEAD(&q->stream);
}

int 
videobuf_queue_is_busy(struct videobuf_queue *q)
{
	int i;
	set_break_point_here;
	if (q->streaming) {
		dprintk(1,"busy: streaming active\n");
		return 1;
	}
	if (q->reading) {
		dprintk(1,"busy: pending read #1\n");
		return 1;
	}
	if (q->read_buf) {
		dprintk(1,"busy: pending read #2\n");
		return 1;
	}
	for (i = 0; i < VIDEO_MAX_FRAME; i++) {
		if (NULL == q->bufs[i])
			continue;
		if (q->bufs[i]->map) {
			dprintk(1,"busy: buffer #%d mapped\n",i);
			return 1;
		}
		if (q->bufs[i]->state == STATE_QUEUED) {
			dprintk(1,"busy: buffer #%d queued\n",i);
			return 1;
		}
		if (q->bufs[i]->state == STATE_ACTIVE) {
			dprintk(1,"busy: buffer #%d avtive\n",i);
			return 1;
		}
	}
	return 0;
}

void
videobuf_queue_cancel(int num, struct videobuf_queue *q)
{
	//	unsigned long flags;
	int i;
  set_break_point_here;

	/* remove queued buffers from list */
  //	spin_lock_irqsave(q->irqlock,flags);
	for (i = 0; i < VIDEO_MAX_FRAME; i++) {
		if (NULL == q->bufs[i])
			continue;
		if (q->bufs[i]->state == STATE_QUEUED) {
			list_del(&q->bufs[i]->queue);
			q->bufs[i]->state = STATE_ERROR;
		}
	}
	//	spin_unlock_irqrestore(q->irqlock,flags);

	/* free all buffers + clear queue */
	for (i = 0; i < VIDEO_MAX_FRAME; i++) {
		if (NULL == q->bufs[i])
			continue;
		q->ops->buf_release(num,q->bufs[i]);
	}
	INIT_LIST_HEAD(&q->stream);
}

/* --------------------------------------------------------------------- */

enum v4l2_field
videobuf_next_field(struct videobuf_queue *q)
{
	enum v4l2_field field = q->field;
  set_break_point_here;

	BUG_ON(V4L2_FIELD_ANY == field);

	if (V4L2_FIELD_ALTERNATE == field) {
		if (V4L2_FIELD_TOP == q->last) {
			field   = V4L2_FIELD_BOTTOM;
			q->last = V4L2_FIELD_BOTTOM;
		} else {
			field   = V4L2_FIELD_TOP;
			q->last = V4L2_FIELD_TOP;
		}
	}
	return field;
}

void
videobuf_status(struct v4l2_buffer *b, struct videobuf_buffer *vb,
		enum v4l2_buf_type type)
{
  set_break_point_here;
	b->index    = vb->i;
	b->type     = type;

	b->memory   = vb->memory;
	switch (b->memory) {
	case V4L2_MEMORY_MMAP:
		b->m.offset  = vb->boff;
		b->length    = vb->bsize;
		break;
	case V4L2_MEMORY_USERPTR:
		b->m.userptr = vb->baddr;
		b->length    = vb->bsize;
		break;
	case V4L2_MEMORY_OVERLAY:
		b->m.offset  = vb->boff;
		break;
	}

	b->flags    = 0;
	if (vb->map)
		b->flags |= V4L2_BUF_FLAG_MAPPED;

	switch (vb->state) {
	case STATE_PREPARED:
	case STATE_QUEUED:
	case STATE_ACTIVE:
		b->flags |= V4L2_BUF_FLAG_QUEUED;
		break;
	case STATE_DONE:
	case STATE_ERROR:
		b->flags |= V4L2_BUF_FLAG_DONE;
		break;
	case STATE_NEEDS_INIT:
	case STATE_IDLE:
		/* nothing */
		break;
	}

	b->field     = vb->field;
	b->timestamp = vb->ts;
	b->bytesused = vb->size;
	b->sequence  = vb->field_count >> 1;
}

int
videobuf_reqbufs(int num, struct videobuf_queue *q,
		 struct v4l2_requestbuffers *req)
{
	unsigned int size,count;
	int retval;
  set_break_point_here;

	if (req->type != q->type)
		return -EINVAL;
	if (req->count < 1)
		return -EINVAL;
	if (req->memory != V4L2_MEMORY_MMAP     &&
	    req->memory != V4L2_MEMORY_USERPTR  &&
	    req->memory != V4L2_MEMORY_OVERLAY)
		return -EINVAL;

	//down(&q->lock);
	count = req->count;
	if (count > VIDEO_MAX_FRAME)
		count = VIDEO_MAX_FRAME;
	size = 0;
	q->ops->buf_setup(num,&count,&size);
	size = PAGE_ALIGN(size);
	dprintk(1,"reqbufs: bufs=%d, size=0x%x [%d pages total]\n",
		count, size, (count*size)>>PAGE_SHIFT);

	retval = videobuf_mmap_setup(num,q,count,size,req->memory);
	if (retval < 0)
		goto done;

	req->count = count;

 done:
	//up(&q->lock);
	return retval;
}

int
videobuf_querybuf(struct videobuf_queue *q, struct v4l2_buffer *b)
{
  set_break_point_here;
	if (!!(b->type != q->type))
		return -EINVAL;
	if (!!(b->index < 0 || b->index >= VIDEO_MAX_FRAME))
		return -EINVAL;
	if (!!(NULL == q->bufs[b->index]))
		return -EINVAL;
	videobuf_status(b,q->bufs[b->index],q->type);
	return 0;
}

int
videobuf_qbuf(int num, struct videobuf_queue *q,
	      struct v4l2_buffer *b)
{
	struct videobuf_buffer *buf;
	enum v4l2_field field;
	//	unsigned long flags;
	int retval;
  set_break_point_here;

	//down(&q->lock);
	retval = -EBUSY;
	if (q->reading)
		goto done;
	retval = -EINVAL;
	if (b->type != q->type)
		goto done;
	if (b->index < 0 || b->index >= VIDEO_MAX_FRAME)
		goto done;
	buf = q->bufs[b->index];
	if (NULL == buf)
		goto done;
	if (buf->memory != b->memory)
		goto done;
	if (buf->state == STATE_QUEUED ||
	    buf->state == STATE_ACTIVE)
		goto done;

	switch (b->memory) {
	case V4L2_MEMORY_MMAP:
		if (0 == buf->baddr)
			goto done;
		break;
	case V4L2_MEMORY_USERPTR:
		if (b->length < buf->bsize)
			goto done;
		buf->baddr = b->m.userptr;
		break;
	case V4L2_MEMORY_OVERLAY:
		buf->boff = b->m.offset;
		break;
	default:
		goto done;
	}

	field = videobuf_next_field(q);
	retval = q->ops->buf_prepare(num,buf,field);
	if (0 != retval)
		goto done;
	
	list_add_tail(&buf->stream,&q->stream);
	if (q->streaming) {
		//		spin_lock_irqsave(q->irqlock,flags);
		q->ops->buf_queue(num,buf);
		//		spin_unlock_irqrestore(q->irqlock,flags);
	}
	retval = 0;
	
 done:
	//up(&q->lock);
	return retval;
}

int
videobuf_dqbuf(int num, struct videobuf_queue *q,
	       struct v4l2_buffer *b)
{
 	struct videobuf_buffer *buf;
	int retval;
	set_break_point_here;	
	//down(&q->lock);
	retval = -EBUSY;
	if (q->reading)
		goto done;
	retval = -EINVAL;
	if (b->type != q->type)
		goto done;
	if (list_empty(&q->stream))
		goto done;
	buf = list_entry(q->stream.next, struct videobuf_buffer, stream);
	retval = videobuf_waiton(buf, 1);
	if (retval < 0)
		goto done;
	switch (buf->state) {
	case STATE_ERROR:
		retval = -EIO;
		/* fall through */
	case STATE_DONE:
		//videobuf_dma_pci_sync(q->pci,&buf->dma);
		buf->state = STATE_IDLE;
		break;
	default:
		retval = -EINVAL;
		goto done;
	}
	list_del(&buf->stream);
	memset(b,0,sizeof(*b));
	videobuf_status(b,buf,q->type);

 done:
	//up(&q->lock);
	return retval;
}

int videobuf_streamon(int num, struct videobuf_queue *q)
{
	struct videobuf_buffer *buf;
	struct list_head *list;
	//	unsigned long flags;
	int retval;
	set_break_point_here;	
	//down(&q->lock);
	retval = -EBUSY;
	if (q->reading)
		goto done;
	retval = 0;
	if (q->streaming)
		goto done;
	q->streaming = 1;
	//	spin_lock_irqsave(q->irqlock,flags);
	list_for_each(list,&q->stream) {
		buf = list_entry(list, struct videobuf_buffer, stream);
		if (buf->state == STATE_PREPARED)
			q->ops->buf_queue(num,buf);
	}
	//	spin_unlock_irqrestore(q->irqlock,flags);

 done:
	//up(&q->lock);
	return retval;
}

int videobuf_streamoff(int num, struct videobuf_queue *q)
{
	int retval = -EINVAL;
	set_break_point_here;
	//down(&q->lock);
	if (!q->streaming)
		goto done;
	videobuf_queue_cancel(num,q);
	q->streaming = 0;
	retval = 0;

 done:
	//up(&q->lock);
	return retval;
}

static ssize_t
videobuf_read_zerocopy(int num, struct videobuf_queue *q,
		       char *data, size_t count, loff_t *ppos)
{
	enum v4l2_field field;
	//	unsigned long flags;
        int retval;
	set_break_point_here;
        /* setup stuff */
	retval = -ENOMEM;
	q->read_buf = videobuf_alloc(q->msize);
	if (NULL == q->read_buf)
		goto done;

	q->read_buf->memory = V4L2_MEMORY_USERPTR;
	q->read_buf->baddr  = (unsigned long)data;
        q->read_buf->bsize  = count;
	field = videobuf_next_field(q);
	retval = q->ops->buf_prepare(num,q->read_buf,field);
	if (0 != retval)
		goto done;
	
        /* start capture & wait */
	//	spin_lock_irqsave(q->irqlock,flags);
	q->ops->buf_queue(num,q->read_buf);
	//	spin_unlock_irqrestore(q->irqlock,flags);
        retval = videobuf_waiton(q->read_buf,0);
        if (0 == retval) {
		//videobuf_dma_pci_sync(q->pci,&q->read_buf->dma);
		if (STATE_ERROR == q->read_buf->state)
			retval = -EIO;
		else
			retval = q->read_buf->size;
	}

 done:
	/* cleanup */
	q->ops->buf_release(num,q->read_buf);
	kfree(q->read_buf);
	q->read_buf = NULL;
	return retval;
}

ssize_t videobuf_read_one(int num, struct videobuf_queue *q,
			  char *data, size_t count, loff_t *ppos)
{
	enum v4l2_field field;
	//	unsigned long flags;
	unsigned size, nbufs, bytes;
	int retval;

	//down(&q->lock);

	set_break_point_here;
	nbufs = 1; size = 0;
	q->ops->buf_setup(num,&nbufs,&size);
	if (NULL == q->read_buf  &&
	    count >= size) {/*        &&
				       !(file->f_flags & O_NONBLOCK)) {*/
		retval = videobuf_read_zerocopy(num,q,data,count,ppos);
		if (retval >= 0  ||  retval == -EIO)
			/* ok, all done */
			goto done;
		/* fallback to kernel bounce buffer on failures */
	}

	if (NULL == q->read_buf) {
		/* need to capture a new frame */
		retval = -ENOMEM;
		q->read_buf = videobuf_alloc(q->msize);
		if (NULL == q->read_buf)
			goto done;
		q->read_buf->memory = V4L2_MEMORY_USERPTR;
		field = videobuf_next_field(q);
		retval = q->ops->buf_prepare(num,q->read_buf,field);
		if (0 != retval)
			goto done;
		//		spin_lock_irqsave(q->irqlock,flags);
		q->ops->buf_queue(num,q->read_buf);
		//		spin_unlock_irqrestore(q->irqlock,flags);
		q->read_off = 0;
	}

	/* wait until capture is done */
        retval = videobuf_waiton(q->read_buf,1);
	if (0 != retval)
		goto done;
	//videobuf_dma_pci_sync(q->pci,&q->read_buf->dma);

	if (STATE_ERROR == q->read_buf->state) {
		/* catch I/O errors */
		q->ops->buf_release(num,q->read_buf);
		kfree(q->read_buf);
		q->read_buf = NULL;
		retval = -EIO;
		goto done;
	}

	/* copy to userspace */
	bytes = count;
	if (bytes > q->read_buf->size - q->read_off)
		bytes = q->read_buf->size - q->read_off;
	retval = -EFAULT;
	if (copy_to_user(data, q->read_buf->dma.vmalloc+q->read_off, bytes))
		goto done;

	retval = bytes;
	q->read_off += bytes;
	if (q->read_off == q->read_buf->size) {
		/* all data copied, cleanup */
		q->ops->buf_release(num,q->read_buf);
		kfree(q->read_buf);
		q->read_buf = NULL;
	}

 done:
	//up(&q->lock);
	return retval;
}

int videobuf_read_start(int num, struct videobuf_queue *q)
{
	enum v4l2_field field;
	//	unsigned long flags;
	int count = 0, size = 0;
	int err, i;
  set_break_point_here;

	q->ops->buf_setup(num,&count,&size);
	if (count < 2)
		count = 2;
	if (count > VIDEO_MAX_FRAME)
		count = VIDEO_MAX_FRAME;
	size = PAGE_ALIGN(size);

	err = videobuf_mmap_setup(num, q, count, size, V4L2_MEMORY_USERPTR);
	if (err)
		return err;
	for (i = 0; i < count; i++) {
		field = videobuf_next_field(q);
		err = q->ops->buf_prepare(num,q->bufs[i],field);
		if (err)
			return err;
		list_add_tail(&q->bufs[i]->stream, &q->stream);
	}
	//	spin_lock_irqsave(q->irqlock,flags);
	for (i = 0; i < count; i++)
		q->ops->buf_queue(num,q->bufs[i]);
	//	spin_unlock_irqrestore(q->irqlock,flags);
	q->reading = 1;
	return 0;
}

void videobuf_read_stop(int num, struct videobuf_queue *q)
{
	int i;
  set_break_point_here;
	
	videobuf_queue_cancel(num,q);
	INIT_LIST_HEAD(&q->stream);
	for (i = 0; i < VIDEO_MAX_FRAME; i++) {
		if (NULL == q->bufs[i])
			continue;
		kfree(q->bufs[i]);
		q->bufs[i] = NULL;
	}
	q->read_buf = NULL;
	q->reading  = 0;
}

ssize_t videobuf_read_stream(int num, struct videobuf_queue *q,
			     char *data, size_t count, loff_t *ppos,
			     int vbihack)
{
  	unsigned int *fc, bytes;
	int err, retval;
	//	unsigned long flags;
	set_break_point_here;	
	//down(&q->lock);
	retval = -EBUSY;
	if (q->streaming)
		goto done;
	if (!q->reading) {
		retval = videobuf_read_start(num,q);
		if (retval < 0)
			goto done;
	}

	retval = 0;
	while (count > 0) {
		/* get / wait for data */
		if (NULL == q->read_buf) {
			q->read_buf = list_entry(q->stream.next,
						 struct videobuf_buffer,
						 stream);
			list_del(&q->read_buf->stream);
			q->read_off = 0;
		}
		err = videobuf_waiton(q->read_buf,1);
		if (err < 0) {
			if (0 == retval)
				retval = err;
			break;
		}

		if (q->read_buf->state == STATE_DONE) {
			if (vbihack) {
				/* dirty, undocumented hack -- pass the frame counter
				 * within the last four bytes of each vbi data block.
				 * We need that one to maintain backward compatibility
				 * to all vbi decoding software out there ... */
				fc  = (unsigned int*)q->read_buf->dma.vmalloc;
				fc += (q->read_buf->size>>2) -1;
				*fc = q->read_buf->field_count >> 1;
				dprintk(1,"vbihack: %d\n",*fc);
			}
			
			/* copy stuff */
			bytes = count;
			if (bytes > q->read_buf->size - q->read_off)
				bytes = q->read_buf->size - q->read_off;
			if (copy_to_user(data + retval,
					 q->read_buf->dma.vmalloc + q->read_off,
					 bytes)) {
				if (0 == retval)
					retval = -EFAULT;
				break;
			}
			count       -= bytes;
			retval      += bytes;
			q->read_off += bytes;
		} else {
			/* some error -- skip buffer */
			q->read_off = q->read_buf->size;
		}

		/* requeue buffer when done with copying */
		if (q->read_off == q->read_buf->size) {
			list_add_tail(&q->read_buf->stream,
				      &q->stream);
			//			spin_lock_irqsave(q->irqlock,flags);
			q->ops->buf_queue(num,q->read_buf);
			//			spin_unlock_irqrestore(q->irqlock,flags);
			q->read_buf = NULL;
		}
	}

 done:
	//up(&q->lock);
	return retval;
}

unsigned int videobuf_poll_stream(int num,
				  struct videobuf_queue *q)
				  //				  poll_table *wait)
{
#if 0
	struct videobuf_buffer *buf = NULL;
	unsigned int rc = 0;

	//down(&q->lock);
	if (q->streaming) {
		if (!list_empty(&q->stream))
			buf = list_entry(q->stream.next,
					 struct videobuf_buffer, stream);
	} else {
		if (!q->reading)
			videobuf_read_start(num,q);
		if (!q->reading) {
			rc = POLLERR;
		} else if (NULL == q->read_buf) {
			q->read_buf = list_entry(q->stream.next,
						 struct videobuf_buffer,
						 stream);
			list_del(&q->read_buf->stream);
			q->read_off = 0;
		}
		buf = q->read_buf;
	}
	if (!buf)
		rc = POLLERR;

	if (0 == rc) {
		poll_wait(num, &buf->done, wait);
		if (buf->state == STATE_DONE ||
		    buf->state == STATE_ERROR)
			rc = POLLIN|POLLRDNORM;
	}
	//up(&q->lock);
	return rc;
#endif
	return -1;
}

int videobuf_mmap_setup(int num, struct videobuf_queue *q,
			unsigned int bcount, unsigned int bsize,
			enum v4l2_memory memory)
{
	unsigned int i;
	int err;

  set_break_point_here;
	err = videobuf_mmap_free(num,q);
	if (0 != err)
		return err;

	for (i = 0; i < bcount; i++) {
		q->bufs[i] = videobuf_alloc(q->msize);
		q->bufs[i]->i      = i;
		q->bufs[i]->memory = memory;
		q->bufs[i]->bsize  = bsize;
		switch (memory) {
		case V4L2_MEMORY_MMAP:
			q->bufs[i]->boff  = bsize * i;
			break;
		case V4L2_MEMORY_USERPTR:
		case V4L2_MEMORY_OVERLAY:
			/* nothing */
			break;
		};
	}
	dprintk(1,"mmap setup: %d buffers, %d bytes each\n",
		bcount,bsize);
	return 0;
}

int videobuf_mmap_free(int num, struct videobuf_queue *q)
{
	int i;

  set_break_point_here;
	for (i = 0; i < VIDEO_MAX_FRAME; i++)
		if (q->bufs[i] && q->bufs[i]->map)
			return -EBUSY;
	for (i = 0; i < VIDEO_MAX_FRAME; i++) {
		if (NULL == q->bufs[i])
			continue;
		q->ops->buf_release(num,q->bufs[i]);
		kfree(q->bufs[i]);
		q->bufs[i] = NULL;
	}
	return 0;
}

int videobuf_mmap_mapper(struct vm_area_struct *vma,
			 struct videobuf_queue *q)
{
#if 0
	struct videobuf_mapping *map;
	unsigned int first,last,size,i;
	int retval;

	//down(&q->lock);

	retval = -EINVAL;
	if (!(vma->vm_flags & VM_WRITE)) {
		dprintk(1,"mmap app bug: PROT_WRITE please\n");
		goto done;
	}
	if (!(vma->vm_flags & VM_SHARED)) {
		dprintk(1,"mmap app bug: MAP_SHARED please\n");
		goto done;
	}

	/* look for first buffer to map */
	for (first = 0; first < VIDEO_MAX_FRAME; first++) {
		if (NULL == q->bufs[first])
			continue;
		if (V4L2_MEMORY_MMAP != q->bufs[first]->memory)
			continue;
		if (q->bufs[first]->boff == (vma->vm_pgoff << PAGE_SHIFT))
			break;
	}
	if (VIDEO_MAX_FRAME == first) {
		dprintk(1,"mmap app bug: offset invalid [offset=0x%lx]\n",
			(vma->vm_pgoff << PAGE_SHIFT));
		goto done;
	}

	/* look for last buffer to map */
	for (size = 0, last = first; last < VIDEO_MAX_FRAME; last++) {
		if (NULL == q->bufs[last])
			continue;
		if (V4L2_MEMORY_MMAP != q->bufs[last]->memory)
			continue;
		if (q->bufs[last]->map) {
			retval = -EBUSY;
			goto done;
		}
		size += q->bufs[last]->bsize;
		if (size == (vma->vm_end - vma->vm_start))
			break;
	}
	if (VIDEO_MAX_FRAME == last) {
		dprintk(1,"mmap app bug: size invalid [size=0x%lx]\n",
			(vma->vm_end - vma->vm_start));
		goto done;
	}

	/* create mapping + update buffer list */
	retval = -ENOMEM;
	map = kmalloc(sizeof(struct videobuf_mapping),GFP_KERNEL);
	if (NULL == map)
		goto done;
	for (size = 0, i = first; i <= last; size += q->bufs[i++]->bsize) {
		q->bufs[i]->map   = map;
		q->bufs[i]->baddr = vma->vm_start + size;
	}
	map->count    = 1;
	map->start    = vma->vm_start;
	map->end      = vma->vm_end;
	map->q        = q;
	vma->vm_ops   = &videobuf_vm_ops;
	vma->vm_flags |= VM_DONTEXPAND;
	vma->vm_flags &= ~VM_IO; /* using shared anonymous pages */
	vma->vm_private_data = map;
	dprintk(1,"mmap %p: %08lx-%08lx pgoff %08lx bufs %d-%d\n",
		map,vma->vm_start,vma->vm_end,vma->vm_pgoff,first,last);
	retval = 0;

 done:
	//up(&q->lock);
	return retval;
#endif
	return -1;
}

/*
 * Local variables:
 * c-basic-offset: 8
 * End:
 */
