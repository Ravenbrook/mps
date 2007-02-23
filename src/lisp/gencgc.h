/* 
 * Generational Conservative Garbage Collector for CMUCL x86.
 *
 * This code was written by Douglas T. Crosher, based on Public Domain
 * codes from Carnegie Mellon University. This code has been placed in
 * the public domain, and is provided 'as is'.
 *
 * Douglas Crosher, 1996, 1997.
 *
 * $Header: /project/cmucl/cvsroot/src/lisp/gencgc.h,v 1.13 2006/01/18 15:21:26 rtoy Exp $
 *
 */

#ifndef _GENCGC_H_
#define _GENCGC_H_

void gc_free_heap(void);
inline int find_page_index(void *);
inline char *page_address(int);



/*
 * Set when the page is write protected. If it is writen into it is
 * made writable and this flag is cleared. This should always reflect
 * the actual write_protect status of a page.
 */

#define PAGE_WRITE_PROTECTED_MASK	0x00000010
#define PAGE_WRITE_PROTECTED(page) \
	(page_table[page].flags & PAGE_WRITE_PROTECTED_MASK)

/*
 * This flag is set when the above write protect flag is clear by the
 * sigbus handler. This is useful for re-scavenging pages that are
 * written during a GC.
 */

#define PAGE_WRITE_PROTECT_CLEARED_MASK	0x00000020
#define PAGE_WRITE_PROTECT_CLEARED(page) \
	(page_table[page].flags & PAGE_WRITE_PROTECT_CLEARED_MASK)

/*
 * Page allocated flag: 0 for a free page; 1 when allocated. If
 * the page is free then the following slots are invalid - well
 * the bytes_used must be 0.
 */

#define PAGE_ALLOCATED_MASK	0x00000040
#define PAGE_ALLOCATED(page)	(page_table[page].flags & PAGE_ALLOCATED_MASK)

/*
 * Unboxed region flag: 1 for unboxed objects, 0 for boxed objects.
 */
#define PAGE_UNBOXED_MASK		0x00000080
#define PAGE_UNBOXED_SHIFT		7
#define PAGE_UNBOXED(page)	(page_table[page].flags & PAGE_UNBOXED_MASK)
#define PAGE_UNBOXED_VAL(page)	(PAGE_UNBOXED(page) >> PAGE_UNBOXED_SHIFT)

/*
 * If this page should not be moved during a GC then this flag is
 * set. It's only valid during a GC for allocated pages.
 */

#define PAGE_DONT_MOVE_MASK		0x00000100
#define PAGE_DONT_MOVE(page) \
	(page_table[page].flags & PAGE_DONT_MOVE_MASK)

/*
 * If the page is part of a large object then this flag is set. No
 * other objects should be allocated to these pages. This is only
 * valid when the page is allocated.
 */

#define PAGE_LARGE_OBJECT_MASK		0x00000200
#define PAGE_LARGE_OBJECT_SHIFT		9
#define PAGE_LARGE_OBJECT(page) \
	(page_table[page].flags & PAGE_LARGE_OBJECT_MASK)
#define PAGE_LARGE_OBJECT_VAL(page) \
	(PAGE_LARGE_OBJECT(page) >> PAGE_LARGE_OBJECT_SHIFT)

/*
 * The generation that this page belongs to. This should be valid for
 * all pages that may have objects allocated, even current allocation
 * region pages - this allows the space of an object to be easily
 * determined.
 */

#define PAGE_GENERATION_MASK		0x0000000f
#define PAGE_GENERATION(page) \
	(page_table[page].flags & PAGE_GENERATION_MASK)

#define PAGE_FLAGS(page, mask) (page_table[page].flags & (mask))
#define PAGE_FLAGS_UPDATE(page, mmask, mflags) \
     (page_table[page].flags = (page_table[page].flags & ~(mmask)) | (mflags))

struct page {
    /*
     * Page flags.
     */

    unsigned flags;

    /*
     * It is important to know the offset to the first object in the
     * page. Currently it's only important to know if an object starts
     * at the begining of the page in which case the offset would be 0
     */
    int first_object_offset;

    /*
     * The number of bytes of this page that are used. This may be less
     * than the actual bytes used for pages within the current
     * allocation regions. It should be 0 for all unallocated pages (not
     * hard to achieve).
     */
    int bytes_used;
};



/*
 * The smallest page size that can be independently allocated and
 * write protected.
 */

#if defined(i386)
#define PAGE_SIZE 4096
#elif defined(sparc)
/*
 * For sparc, the minimum page size (physical page size) is 8K.
 * However, some experiments indicate that this gives worse
 * performance in some cases than the non-gencgc version.  By upping
 * the page size to 32K, we are as good as before, so let us use a 32K
 * page size.  (I'm assuming the gain is because we do the kernel
 * allocation trap less often.)
 */
#define PAGE_SIZE (4*8192)
#elif defined(DARWIN)
/*
 * The physical page size is 4K.  Like sparc, this appears to be
 * somewhat slow (but need to verify that), so let's make the page
 * size 32K so we hit the allocation trap less often.
 */
/*#define PAGE_SIZE 4096*/
/*#define PAGE_SIZE (2*4096)*/
#define PAGE_SIZE (4*4096)
/*#define PAGE_SIZE (8*4096)*/
#endif

extern unsigned dynamic_space_pages;
extern struct page *page_table;


/*
 * Abstract out the data for an allocation region allowing a single
 * routine to be used for allocation and closing.
 */
struct alloc_region {
    /* These two are needed for quick allocation */
    char *free_pointer;
    char *end_addr;		/* Pointer to the byte after the last usable byte */

    /* Needed when closing the region. */
    int first_page;
    int last_page;
    char *start_addr;
};

extern struct alloc_region boxed_region;
extern struct alloc_region unboxed_region;


void gencgc_pickup_dynamic(void);

#ifdef i386
void sniff_code_object(struct code *code, unsigned displacement);
#endif
lispobj *search_dynamic_space(lispobj * pointer);
void update_dynamic_space_free_pointer(void);

lispobj *component_ptr_from_pc(lispobj * pc);

void gc_alloc_update_page_tables(int unboxed,

				 struct alloc_region *alloc_region);

extern char *alloc(int);

#endif /* _GENCGC_H_ */
