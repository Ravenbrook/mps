/*
 * $Header: /project/cmucl/cvsroot/src/lisp/sunos-os.c,v 1.7 2005/09/15 18:26:52 rtoy Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the SunOS version.
 * March 1991, Miles Bader <miles@cogsci.ed.ack.uk> & ted <ted@edu.NMSU>
 *
 */

/* #define DEBUG */

#include <stdio.h>

#include <signal.h>
#include <sys/file.h>

#ifdef SOLARIS
#include <unistd.h>
#include <errno.h>
#include <sys/param.h>
#define OS_PROTERR		SEGV_ACCERR
#define OS_MAPERR		SEGV_MAPERR
#define OS_HASERRNO(code)	((code)->si_errno != 0)
#define OS_ERRNO(code)		((code)->si_errno)
#else
#define OS_PROTERR		SEGV_PROT
#define OS_MAPERR		SEGV_NOMAP
#define OS_HASERRNO(code)	(SEGV_CODE(code)==SEGV_OBJERR)
#define OS_ERRNO(code)		SEGV_ERRNO(code)
extern int errno;
#endif /* SOLARIS */

#include "os.h"
/* To get dynamic_0_space and friends */
#include "globals.h"
/* To get memory map */
#include "sparc-validate.h"

/* block size must be larger than the system page size */
#define SPARSE_BLOCK_SIZE (1<<15)
#define SPARSE_SIZE_MASK (SPARSE_BLOCK_SIZE-1)

#define PROT_DEFAULT OS_VM_PROT_ALL

#define OFFSET_NONE ((os_vm_offset_t)(~0))

#define EMPTYFILE "/tmp/empty"
#define ZEROFILE "/dev/zero"

#define INITIAL_MAX_SEGS 32
#define GROW_MAX_SEGS 16

extern char *getenv();

/* ---------------------------------------------------------------- */

#define ADJ_OFFSET(off,adj) (((off)==OFFSET_NONE) ? OFFSET_NONE : ((off)+(adj)))

long os_vm_page_size = (-1);
static long os_real_page_size = (-1);

static struct segment {
    os_vm_address_t start;	/* note: start & length are expected to be on page */
    os_vm_size_t length;	/*       boundaries */
    long file_offset;
    short mapped_fd;
    short protection;
} *segments;

static int n_segments = 0, max_segments = 0;

static int zero_fd = (-1), empty_fd = (-1);

static os_vm_address_t last_fault = 0;
static os_vm_size_t real_page_size_difference = 0;

static void
os_init_bailout(arg)
     char *arg;
{
    char buf[500];

    sprintf(buf, "os_init: %s", arg);
    perror(buf);
    exit(1);
}

void
os_init()
{
    char *empty_file = getenv("CMUCL_EMPTYFILE");

    if (empty_file == NULL)
	empty_file = EMPTYFILE;

    empty_fd = open(empty_file, O_RDONLY | O_CREAT);
    if (empty_fd < 0)
	os_init_bailout(empty_file);
    unlink(empty_file);

    zero_fd = open(ZEROFILE, O_RDONLY);
    if (zero_fd < 0)
	os_init_bailout(ZEROFILE);


#ifdef SOLARIS
    os_vm_page_size = os_real_page_size = sysconf(_SC_PAGESIZE);
#else
    os_vm_page_size = os_real_page_size = getpagesize();
#endif

    max_segments = INITIAL_MAX_SEGS;
    segments = (struct segment *) malloc(sizeof(struct segment) * max_segments);

    if (segments == NULL) {
	fprintf(stderr, "os_init: Couldn't allocate %d segment descriptors\n",
		max_segments);
	exit(1);
    }

    if (os_vm_page_size > OS_VM_DEFAULT_PAGESIZE) {
	fprintf(stderr, "os_init: Pagesize too large (%d > %d)\n",
		os_vm_page_size, OS_VM_DEFAULT_PAGESIZE);
	exit(1);
    } else {
	/*
	 * we do this because there are apparently dependencies on
	 * the pagesize being OS_VM_DEFAULT_PAGESIZE somewhere...
	 * but since the OS doesn't know we're using this restriction,
	 * we have to grovel around a bit to enforce it, thus anything
	 * that uses real_page_size_difference.
	 */
	real_page_size_difference = OS_VM_DEFAULT_PAGESIZE - os_vm_page_size;
	os_vm_page_size = OS_VM_DEFAULT_PAGESIZE;
    }
}

/* ---------------------------------------------------------------- */

void
seg_force_resident(struct segment *seg, os_vm_address_t addr, os_vm_size_t len)
{
    int prot = seg->protection;

    if (prot != 0) {
	os_vm_address_t end = addr + len, touch = addr;

	while (touch < end) {
	    int contents = (*(char *) touch);

	    if (prot & OS_VM_PROT_WRITE)
		(*(char *) touch) = contents;
	    touch =
		(os_vm_address_t) (((long) touch + SPARSE_BLOCK_SIZE) &
				   ~SPARSE_SIZE_MASK);
	}
    }
}

static struct segment *
seg_create_nomerge(addr, len, protection, mapped_fd, file_offset)
     os_vm_address_t addr;
     os_vm_size_t len;
     int protection;
     int mapped_fd;
{
    int n;
    struct segment *seg;

    if (len == 0)
	return NULL;

    if (n_segments == max_segments) {
	struct segment *new_segs;

	max_segments += GROW_MAX_SEGS;

	new_segs = (struct segment *)
	    realloc(segments, max_segments * sizeof(struct segment));

	if (new_segs == NULL) {
	    fprintf(stderr,
		    "seg_create_nomerge: Couldn't grow segment descriptor table to %s segments\n",
		    max_segments);
	    max_segments -= GROW_MAX_SEGS;
	    return NULL;
	}

	segments = new_segs;
    }

    for (n = n_segments, seg = segments; n > 0; n--, seg++)
	if (addr < seg->start) {
	    seg = (&segments[n_segments]);
	    while (n-- > 0) {
		seg[0] = seg[-1];
		seg--;
	    }
	    break;
	}

    n_segments++;

    seg->start = addr;
    seg->length = len;
    seg->protection = protection;
    seg->mapped_fd = mapped_fd;
    seg->file_offset = file_offset;

    return seg;
}

#if 1
/* returns the first segment containing addr */
static struct segment *
seg_find(addr)
     os_vm_address_t addr;
{
    int n;
    struct segment *seg;

    for (n = n_segments, seg = segments; n > 0; n--, seg++)
	if (seg->start <= addr && seg->start + seg->length > addr)
	    return seg;

    return NULL;
}
#else
/* returns the first segment containing addr */
static struct segment *
seg_find(addr)
     os_vm_address_t addr;
{
    /* does a binary search */
    struct segment *lo = segments, *hi = segments + n_segments;

    while (hi > lo) {
	struct segment *mid = lo + ((hi - lo) >> 1);
	os_vm_address_t start = mid->start;

	if (addr >= start && addr < start + mid->length)
	    return mid;
	else if (addr < start)
	    hi = mid;
	else
	    lo = mid + 1;
    }

    return NULL;
}
#endif

/* returns TRUE if the range from addr to addr+len intersects with any segment */
static boolean
collides_with_seg_p(addr, len)
     os_vm_address_t addr;
     os_vm_size_t len;
{
    int n;
    struct segment *seg;
    os_vm_address_t end = addr + len;

    for (n = n_segments, seg = segments; n > 0; n--, seg++)
	if (seg->start >= end)
	    return FALSE;
	else if (seg->start + seg->length > addr)
	    return TRUE;

    return FALSE;
}

#if 0				/* WAY to SLOW */
/* returns TRUE if the range from addr to addr+len is a valid mapping
 * (that we don't know about) */
static boolean
mem_in_use(addr, len)
     os_vm_address_t addr;
     os_vm_size_t len;
{
    os_vm_address_t p;

    for (p = addr; addr < addr + len; p += os_real_page_size) {
	char c;

	if (mincore((caddr_t) p, os_real_page_size, &c) == 0 || errno != ENOMEM)
	    return TRUE;
    }
    return FALSE;
}
#endif

#define seg_last_p(seg) (((seg)-segments)>=n_segments-1)

static void
seg_destroy(seg)
     struct segment *seg;
{
    if (seg != NULL) {
	int n;

	for (n = seg - segments + 1; n < n_segments; n++) {
	    seg[0] = seg[1];
	    seg++;
	}

	n_segments--;
    }
}

static void
seg_try_merge_next(seg)
     struct segment *seg;
{
    struct segment *nseg = seg + 1;

    if (!seg_last_p(seg)
	&& seg->start + seg->length == nseg->start
	&& seg->protection == nseg->protection
	&& seg->mapped_fd == nseg->mapped_fd
	&& ADJ_OFFSET(seg->file_offset, seg->length) == nseg->file_offset) {
	/* can merge with the next segment */
#ifdef DEBUG
	fprintf(stderr,
		";;; seg_try_merge: Merged 0x%08x[0x%08x] with 0x%08x[0x%08x]\n",
		seg->start, seg->length, nseg->start, nseg->length);
#endif

	if (((long) nseg->start & SPARSE_SIZE_MASK) != 0) {
	    /*
	     * if not on a block boundary, we have to ensure both parts
	     * of a common block are in a known state
	     */
	    seg_force_resident(seg, nseg->start - 1, 1);
	    seg_force_resident(nseg, nseg->start, 1);
	}

	seg->length += nseg->length;
	seg_destroy(nseg);
    }
}


/*
 * Try to merge seg with adjacent segments.
 */
static void
seg_try_merge_adjacent(seg)
     struct segment *seg;
{
    if (!seg_last_p(seg))
	seg_try_merge_next(seg);
    if (seg > segments)
	seg_try_merge_next(seg - 1);
}

static struct segment *
seg_create(addr, len, protection, mapped_fd, file_offset)
     os_vm_address_t addr;
     os_vm_size_t len;
     int protection;
     int mapped_fd;
{
    struct segment *seg =

	seg_create_nomerge(addr, len, protection, mapped_fd, file_offset);
    if (seg != NULL)
	seg_try_merge_adjacent(seg);
    return seg;
}

/* 
 * Change the attributes of the given range of an existing segment, and return
 * a segment corresponding to the new bit.
 */
static struct segment *
seg_change_range(seg, addr, len, protection, mapped_fd, file_offset)
     struct segment *seg;
     os_vm_address_t addr;
     os_vm_size_t len;
     int protection;
     int mapped_fd;
{
    os_vm_address_t end = addr + len;

    if (len == 0)
	return NULL;

    if (protection != seg->protection
	|| mapped_fd != seg->mapped_fd
	|| file_offset != ADJ_OFFSET(seg->file_offset, addr - seg->start)) {
	os_vm_size_t old_len = seg->length, seg_offset = (addr - seg->start);

	if (old_len < len + seg_offset) {
	    struct segment *next = seg + 1;

#ifdef DEBUG
	    fprintf(stderr,
		    ";;; seg_change_range: region 0x%08x[0x%08x] overflows 0x%08x[0x%08x]\n",
		    addr, len, seg->start, old_len);
#endif

	    while (!seg_last_p(seg) && next->start + next->length <= end) {
#ifdef DEBUG
		fprintf(stderr,
			";;; seg_change_range: merging extra segment 0x%08x[0x%08x]\n",
			next->start, next->length);
#endif
		seg_destroy(next);
	    }

	    if (!seg_last_p(seg) && next->start < end) {
		next->length -= end - next->start;
		next->start = end;
		old_len = next->start - seg->start;
	    } else
		old_len = len + seg_offset;

#ifdef DEBUG
	    fprintf(stderr,
		    ";;; seg_change_range: extended first seg to 0x%08x[0x%08x]\n",
		    seg->start, old_len);
#endif
	}

	if (seg_offset + len < old_len) {
	    /* add second part of old segment */
	    seg_create_nomerge(end,
			       old_len - (seg_offset + len),
			       seg->protection,
			       seg->mapped_fd,
			       ADJ_OFFSET(seg->file_offset, seg_offset + len));

#ifdef DEBUG
	    fprintf(stderr,
		    ";;; seg_change_range: Split off end of 0x%08x[0x%08x]: 0x%08x[0x%08x]\n",
		    seg->start, old_len, end, old_len - (seg_offset + len));
#endif
	}

	if (seg_offset == 0) {
	    seg->length = len;
	    seg->protection = protection;
	    seg->mapped_fd = mapped_fd;
	    seg->file_offset = file_offset;
	} else {
	    /* adjust first part of remaining old segment */
	    seg->length = seg_offset;

#ifdef DEBUG
	    fprintf(stderr,
		    ";;; seg_change_range: Split off beginning of 0x%08x[0x%08x]: 0x%08x[0x%08x]\n",
		    seg->start, old_len, seg->start, seg_offset);
#endif

	    /* add new middle segment for new protected region */
	    seg =
		seg_create_nomerge(addr, len, protection, mapped_fd,
				   file_offset);
	}

	seg_try_merge_adjacent(seg);

	last_fault = 0;
    }

    return seg;
}

/* ---------------------------------------------------------------- */

static os_vm_address_t
mapin(addr, len, protection, map_fd, offset, is_readable)
     os_vm_address_t addr;
     os_vm_size_t len;
     int protection;
     int map_fd;
     long offset;
     int is_readable;
{
    os_vm_address_t real;
    boolean sparse = (len >= SPARSE_BLOCK_SIZE);

    if (offset != OFFSET_NONE
	&& (offset < os_vm_page_size || (offset & (os_vm_page_size - 1)) != 0)) {
	fprintf(stderr,
		"mapin: file offset (%d) not multiple of pagesize (%d)\n",
		offset, os_vm_page_size);
    }

    if (addr == NULL)
	len += real_page_size_difference;	/* futz around to get an aligned region */

    last_fault = 0;
    real = (os_vm_address_t)
	mmap((caddr_t) addr,
	     (long) len,
	     sparse ? (is_readable ? PROT_READ | PROT_EXEC : 0) : protection,
	     (addr == NULL ? 0 : MAP_FIXED) | MAP_PRIVATE,
	     (is_readable || !sparse) ? map_fd : empty_fd,
	     (off_t) (offset == OFFSET_NONE ? 0 : offset));

    if ((long) real == -1) {
	perror("mapin: mmap");
	return NULL;
    }

    if (addr == NULL) {
	/*
	 * now play around with what the os gave us to make it align by
	 * our standards (which is why we overallocated)
	 */
	os_vm_size_t overflow;

	addr = os_round_up_to_page(real);
	if (addr != real)
	    munmap((caddr_t) real, addr - real);

	overflow = real_page_size_difference - (addr - real);
	if (overflow != 0)
	    munmap((caddr_t) (addr + len - real_page_size_difference),
		   overflow);

	real = addr;
    }


    return real;
}

static os_vm_address_t
map_and_remember(addr, len, protection, map_fd, offset, is_readable)
     os_vm_address_t addr;
     os_vm_size_t len;
     int protection;
     int map_fd;
     long offset;
     int is_readable;
{
    os_vm_address_t real =

	mapin(addr, len, protection, map_fd, offset, is_readable);

    if (real != NULL) {
	struct segment *seg = seg_find(real);

	if (seg != NULL)
	    seg = seg_change_range(seg, real, len, protection, map_fd, offset);
	else
	    seg = seg_create(real, len, protection, map_fd, offset);

	if (seg == NULL) {
	    munmap((caddr_t) real, len);
	    return NULL;
	}
    }
#ifdef DEBUG
    fprintf(stderr,
	    ";;; map_and_remember: 0x%08x[0x%08x] offset: %d, mapped to: %d\n",
	    real, len, offset, map_fd);
#endif

    return real;
}

/* ---------------------------------------------------------------- */

os_vm_address_t
os_validate(addr, len)
     os_vm_address_t addr;
     os_vm_size_t len;
{
    addr = os_trunc_to_page(addr);
    len = os_round_up_size_to_page(len);

#ifdef DEBUG
    fprintf(stderr, ";;; os_validate: 0x%08x[0x%08x]\n", addr, len);
#endif

    if (addr != NULL && collides_with_seg_p(addr, len))
	return NULL;

    return map_and_remember(addr, len, PROT_DEFAULT, zero_fd, OFFSET_NONE,
			    FALSE);
}

void
os_invalidate(addr, len)
     os_vm_address_t addr;
     os_vm_size_t len;
{
    struct segment *seg = seg_find(addr);

    addr = os_trunc_to_page(addr);
    len = os_round_up_size_to_page(len);

#ifdef DEBUG
    fprintf(stderr, ";;; os_invalidate: 0x%08x[0x%08x]\n", addr, len);
#endif

    if (seg == NULL)
	fprintf(stderr, "os_invalidate: Unknown segment: 0x%08x[0x%08x]\n",
		addr, len);
    else {
	seg = seg_change_range(seg, addr, len, 0, 0, OFFSET_NONE);
	if (seg != NULL)
	    seg_destroy(seg);

	last_fault = 0;
	if (munmap((caddr_t) addr, len) != 0)
	    perror("os_invalidate: munmap");
    }
}

os_vm_address_t
os_map(fd, offset, addr, len)
     int fd;
     int offset;
     os_vm_address_t addr;
     long len;
{
    addr = os_trunc_to_page(addr);
    len = os_round_up_size_to_page(len);

#ifdef DEBUG
    fprintf(stderr, ";;; os_map: 0x%08x[0x%08x]\n", addr, len);
#endif

    return map_and_remember(addr, len, PROT_DEFAULT, fd, offset, TRUE);
}

void
os_flush_icache(address, length)
     os_vm_address_t address;
     os_vm_size_t length;
{
#if defined(MACH) && defined(mips)
    vm_machine_attribute_val_t flush;
    kern_return_t kr;

    flush = MATTR_VAL_ICACHE_FLUSH;

    kr = vm_machine_attribute(task_self(), address, length,
			      MATTR_CACHE, &flush);
    if (kr != KERN_SUCCESS)
	mach_error("Could not flush the instruction cache", kr);
#endif
#ifdef SOLARIS			/* also SunOS ?? */
    static int flushit = -1;

    /*
     * On some systems, iflush needs to be emulated in the kernel
     * On those systems, it isn't necessary
     * Call getenv() only once.
     */
    if (flushit == -1)
	flushit = getenv("CMUCL_NO_SPARC_IFLUSH") == 0;

    if (flushit) {
	static int traceit = -1;

	if (traceit == -1)
	    traceit = getenv("CMUCL_TRACE_SPARC_IFLUSH") != 0;

	if (traceit)
	    fprintf(stderr, ";;;iflush %p - %x\n", address, length);
	flush_icache(address, length);
    }
#endif
}

void
os_protect(addr, len, prot)
     os_vm_address_t addr;
     os_vm_size_t len;
     int prot;
{
    struct segment *seg = seg_find(addr);

    addr = os_trunc_to_page(addr);
    len = os_round_up_size_to_page(len);

#ifdef DEBUG
    fprintf(stderr, ";;; os_protect: 0x%08x[0x%08x]\n", addr, len);
#endif

    if (seg != NULL) {
	int old_prot = seg->protection;

	if (prot != old_prot) {
	    /*
	     * oooooh, sick: we have to make sure all the pages being protected have
	     * faulted in, so they're in a known state...
	     */
	    seg_force_resident(seg, addr, len);

	    seg_change_range(seg, addr, len, prot, seg->mapped_fd,
			     seg->file_offset);

	    if (mprotect((caddr_t) addr, (long) len, prot) != 0)
		perror("os_unprotect: mprotect");
	}
    } else
	fprintf(stderr, "os_protect: Unknown segment: 0x%08x[0x%08x]\n", addr,
		len);
}

boolean
valid_addr(test)
     os_vm_address_t test;
{
    return seg_find(test) != NULL;
}

/* ---------------------------------------------------------------- */

static boolean
maybe_gc(HANDLER_ARGS)
{
    /*
     * It's necessary to enable recursive SEGVs, since the handle is
     * used for multiple things (e.g., both gc-trigger & faulting in pages).
     * We check against recursive gc's though...
     */

    boolean did_gc;
    static already_trying = 0;

    if (already_trying)
	return FALSE;

    SAVE_CONTEXT();

#ifdef POSIX_SIGS
    sigprocmask(SIG_SETMASK, &context->uc_sigmask, 0);
#else
    sigsetmask(context->sc_mask);
#endif

    already_trying = TRUE;
    did_gc = interrupt_maybe_gc(signal, code, context);
    already_trying = FALSE;

    return did_gc;
}

/*
 * The primary point of catching segmentation violations is to allow 
 * read only memory to be re-mapped with more permissions when a write
 * is attempted.  this greatly decreases the residency of the program
 * in swap space since read only areas don't take up room
 *
 * Running into the gc trigger page will also end up here...
 */
#ifndef SOLARIS
void
segv_handler(HANDLER_ARGS, caddr_t addr)
#else
void
segv_handler(HANDLER_ARGS)
#endif				/* SOLARIS */
{
#ifdef SOLARIS
    caddr_t addr = code->si_addr;
#endif

    SAVE_CONTEXT();

    if (CODE(code) == OS_PROTERR) {	/* allow writes to this chunk */
	struct segment *seg = seg_find(addr);

	if ((caddr_t) last_fault == addr) {
	    if (seg != NULL && maybe_gc(signal, code, context))
		/* we just garbage collected */
		return;
	    else {
		/* a *real* protection fault */
		fprintf(stderr,
			"segv_handler: Real protection violation: 0x%08x\n",
			addr);
		interrupt_handle_now(signal, code, context);
	    }
	} else
	    last_fault = (os_vm_address_t) addr;

	if (seg != NULL) {
	    int err;

	    /* round down to a page */
	    os_vm_address_t block =

		(os_vm_address_t) ((long) addr & ~SPARSE_SIZE_MASK);
	    os_vm_size_t length = SPARSE_BLOCK_SIZE;

	    if (block < seg->start) {
		length -= (seg->start - block);
		block = seg->start;
	    }
	    if (block + length > seg->start + seg->length)
		length = seg->start + seg->length - block;

#if 0
	    /* unmap it.  probably redundant. */
	    if (munmap((caddr_t) block, length) == -1)
		perror("segv_handler: munmap");
#endif

	    /* and remap it with more permissions */
	    err = (int)
		mmap((caddr_t) block,
		     length,
		     seg->protection,
		     MAP_PRIVATE | MAP_FIXED,
		     seg->mapped_fd,
		     seg->file_offset == OFFSET_NONE
		     ? 0 : seg->file_offset + (block - seg->start));

	    if (err == -1) {
		perror("segv_handler: mmap");
		interrupt_handle_now(signal, code, context);
	    }
	} else {
	    fprintf(stderr, "segv_handler: 0x%08x not in any segment\n", addr);
	    interrupt_handle_now(signal, code, context);
	}
    }
    /*
     * note that we check for a gc-trigger hit even if it's not a PROT error
     */
    else if (!maybe_gc(signal, code, context)) {
	static int nomap_count = 0;

	if (CODE(code) == OS_MAPERR) {
	    if (nomap_count == 0) {
		fprintf(stderr,
			"segv_handler: No mapping fault: 0x%08x\n", addr);
		nomap_count++;
	    } else {
		/*
		 * There should be higher-level protection against stack
		 * overflow somewhere, but at least this prevents infinite
		 * puking of error messages...
		 */
		fprintf(stderr,
			"segv_handler: Recursive no mapping fault (stack overflow?)\n");
		exit(-1);
	    }
	} else if (OS_HASERRNO(code)) {
	    errno = OS_ERRNO(code);
	    perror("segv_handler: Object error");
	}

	interrupt_handle_now(signal, code, context);

	if (CODE(code) == OS_MAPERR)
	    nomap_count--;
    }
}

void
os_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGSEGV, segv_handler);
}


#ifdef SOLARIS


/* function defintions for register lvalues */

int *
solaris_register_address(struct ucontext *context, int reg)
{
    if (reg == 0) {
	static int zero;

	zero = 0;

	return &zero;
    } else if (reg < 16) {
	return &context->uc_mcontext.gregs[reg + 3];
    } else if (reg < 32) {
	int *sp = (int *) context->uc_mcontext.gregs[REG_SP];

	return &sp[reg - 16];
    } else
	return 0;
}

/* function defintions for backward compatibilty and static linking */

#if 0
void *
dlopen(const char *file, int flag)
{
    return 0;
}

void *
dlsym(void *obj, const char *sym)
{
    return 0;
}

int
dlclose(void *obj)
{
    return 0;
}

char *
dlerror(void)
{
    return "no dynamic linking";
}
#endif

/* For now we put in some porting functions */

#ifndef SOLARIS25
int
getdtablesize(void)
{
    return sysconf(_SC_OPEN_MAX);
}

char *
getwd(char *path)
{
    return getcwd(path, MAXPATHLEN);
}

int
getpagesize(void)
{
    return sysconf(_SC_PAGESIZE);
}


#include <sys/procfs.h>
/* Old rusage definition */
struct rusage {
    struct timeval ru_utime;	/* user time used */
    struct timeval ru_stime;	/* system time used */
    long ru_maxrss;
#define ru_first        ru_ixrss
    long ru_ixrss;		/* XXX: 0 */
    long ru_idrss;		/* XXX: sum of rm_asrss */
    long ru_isrss;		/* XXX: 0 */
    long ru_minflt;		/* any page faults not requiring I/O */
    long ru_majflt;		/* any page faults requiring I/O */
    long ru_nswap;		/* swaps */
    long ru_inblock;		/* block input operations */
    long ru_oublock;		/* block output operations */
    long ru_msgsnd;		/* messages sent */
    long ru_msgrcv;		/* messages received */
    long ru_nsignals;		/* signals received */
    long ru_nvcsw;		/* voluntary context switches */
    long ru_nivcsw;		/* involuntary " */
#define ru_last         ru_nivcsw
};


int
getrusage(int who, struct rusage *rp)
{
    memset(rp, 0, sizeof(struct rusage));

    return 0;
}

int
setreuid()
{
    fprintf(stderr, "setreuid unimplemented\n");
    errno = ENOSYS;
    return -1;
}

int
setregid()
{
    fprintf(stderr, "setregid unimplemented\n");
    errno = ENOSYS;
    return -1;
}

int
gethostid()
{
    fprintf(stderr, "gethostid unimplemented\n");
    errno = ENOSYS;
    return -1;
}

int
killpg(int pgrp, int sig)
{
    if (pgrp < 0) {
	errno = ESRCH;
	return -1;
    }
    return kill(-pgrp, sig);
}
#endif

int
sigblock(int mask)
{
    sigset_t old, new;

    sigemptyset(&new);
    new.__sigbits[0] = mask;

    sigprocmask(SIG_BLOCK, &new, &old);

    return old.__sigbits[0];
}

#ifndef SOLARIS25
int
wait3(int *status, int options, struct rusage *rp)
{
    if (rp)
	memset(rp, 0, sizeof(struct rusage));

    return waitpid(-1, status, options);
}
#endif

int
sigsetmask(int mask)
{
    sigset_t old, new;

    sigemptyset(&new);
    new.__sigbits[0] = mask;

    sigprocmask(SIG_SETMASK, &new, &old);

    return old.__sigbits[0];

}

#endif /* SOLARIS */

os_vm_address_t round_up_sparse_size(os_vm_address_t addr)
{
    return (addr + SPARSE_BLOCK_SIZE - 1) & ~SPARSE_SIZE_MASK;
}

/*
 * An array of the start of the spaces which should have holes placed
 * after them.  Must not include the dynamic spaces because the size
 * of the dynamic space can be controlled from the command line.
 */
static os_vm_address_t spaces[] = {
    READ_ONLY_SPACE_START, STATIC_SPACE_START,
    BINDING_STACK_START, CONTROL_STACK_START
};

/*
  
 * The corresponding array for the size of each space.  Be sure that
 * the spaces and holes don't overlap!  The sizes MUST be on
 * SPARSE_BLOCK_SIZE boundaries.
 
 */
static unsigned long space_size[] = {
    READ_ONLY_SPACE_SIZE, STATIC_SPACE_SIZE,
    BINDING_STACK_SIZE, CONTROL_STACK_SIZE
};

/*
 * The size of the hole to make.  It should be strictly smaller than
 * SPARSE_BLOCK_SIZE.
 */

#define HOLE_SIZE 0x2000

void
make_holes(void)
{
    int k;
    os_vm_address_t hole;

    /* Make holes of the appropriate size for desired spaces */

    for (k = 0; k < sizeof(spaces) / sizeof(spaces[0]); ++k) {

	hole = spaces[k] + space_size[k];

	if (os_validate(hole, HOLE_SIZE) == NULL) {
	    fprintf(stderr,
		    "ensure_space: Failed to validate hole of %ld bytes at 0x%08X\n",
		    HOLE_SIZE, (unsigned long) hole);
	    exit(1);
	}
	/* Make it inaccessible */
	os_protect(hole, HOLE_SIZE, 0);
    }

    /* Round up the dynamic_space_size to the nearest SPARSE_BLOCK_SIZE */
    dynamic_space_size = round_up_sparse_size(dynamic_space_size);

    /* Now make a hole for the dynamic spaces */
    hole = dynamic_space_size + (os_vm_address_t) dynamic_0_space;

    if (os_validate(hole, HOLE_SIZE) == NULL) {
	fprintf(stderr,
		"ensure_space: Failed to validate hold of %ld bytes at 0x%08X\n",
		HOLE_SIZE, (unsigned long) hole);
	exit(1);
    }
    os_protect(hole, HOLE_SIZE, 0);

    hole = dynamic_space_size + (os_vm_address_t) dynamic_1_space;
    if (os_validate(hole, HOLE_SIZE) == NULL) {
	fprintf(stderr,
		"ensure_space: Failed to validate hole of %ld bytes at 0x%08X\n",
		HOLE_SIZE, (unsigned long) hole);
	exit(1);
    }
    os_protect(hole, HOLE_SIZE, 0);
}
