/*
 * $Header: /project/cmucl/cvsroot/src/lisp/hpux-os.c,v 1.7 2005/09/15 18:26:51 rtoy Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 */

/* Assumptions these routines are based on:
os_validate: Not called with NULL for an addr.
os_invalidate: Never called.
os_map: Files are only mapped at the beginning of one of the areas passed
	to os_validate.
os_protect: Only called on an entire region when giving permissions and only
	    called from some point in a segment to the end of the segment
	    when removing them.
	    Only called with all protections or no protections.
os_zero: Only ever zeroed from some point in a segment to the end of the
	 segment.
os_allocate_at: Calls to here are disjoint from those around it (the others
	in os-common.c) since it calls os_validate and the others (in 
	os-common.c) use malloc, etc.
Note that os_validate does not actually allocate memory until it has to map
the particular section in.
*/

/* #define DEBUG */

#include <stdio.h>
#include <assert.h>
#include <signal.h>
#include <sys/file.h>
#include <unistd.h>
#include "os.h"
#include <sys/resource.h>
#include "interrupt.h"
#include <netdb.h>
#include <sys/times.h>
#include <errno.h>

os_vm_size_t os_vm_page_size = (-1);

#define MAX_SEGMENTS 20
#define ALLOC_SIZE 0x10000
static struct segment {
    os_vm_address_t base;
    os_vm_size_t len;
    os_vm_address_t valid;
    os_vm_address_t protected;
} segments[MAX_SEGMENTS];

void
os_init()
{
    int i;

    os_vm_page_size = sysconf(_SC_PAGE_SIZE);
    for (i = 0; i < MAX_SEGMENTS; i++)
	segments[i].len = 0;
}


os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int fd, i;
    caddr_t ret;

    addr = os_trunc_to_page(addr);
    len = os_round_up_size_to_page(len);

#ifdef DEBUG
    printf("os_validate: addr: 0x%X, len: 0x%X, end: 0x%X\n", addr, len,
	   addr + len);
#endif
    assert(addr != NULL);
    assert(len != 0);

    for (i = 0; i < MAX_SEGMENTS; i++)
	if (segments[i].len == 0)
	    break;

    assert(i != MAX_SEGMENTS);

    segments[i].base = addr;
    segments[i].len = len;
    segments[i].valid = addr;
    segments[i].protected = addr + len;
    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    assert(FALSE);
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    int i;

    addr = os_trunc_to_page(addr);
    len = os_round_up_size_to_page(len);

    if (mmap(addr, len, OS_VM_PROT_ALL, MAP_FILE | MAP_FIXED | MAP_PRIVATE, fd,
	     (off_t) offset) == (os_vm_address_t) - 1) {
	perror("mmap");
	return NULL;
    }

    for (i = 0; i < MAX_SEGMENTS; i++)
	if (segments[i].len != 0 && segments[i].base == addr)
	    break;

    assert(i != MAX_SEGMENTS);
    assert(segments[i].valid == addr);

    segments[i].valid = addr + len;
#ifdef DEBUG
    printf("os_map: addr: 0x%X, len: 0x%X, end: 0x%X\n", addr, len, addr + len);
#endif
    return addr;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    sanctify_for_execution(address, length);
}

void
os_protect(os_vm_address_t addr, os_vm_size_t len, os_vm_prot_t prot)
{
    int i;

    addr = os_trunc_to_page(addr);
    len = os_round_up_size_to_page(len);

    for (i = 0; i < MAX_SEGMENTS; i++)
	if (segments[i].base <= addr
	    && addr < segments[i].base + segments[i].len) break;

    assert(i != MAX_SEGMENTS);

    if (prot) {
	assert(segments[i].base == addr && segments[i].len == len);
	segments[i].protected = addr + len;
    } else {
	assert(segments[i].protected == addr + len);
	segments[i].protected = addr;
    }

    if (addr < segments[i].valid)
	if (mprotect(addr, segments[i].valid - addr, prot) == -1) {
	    perror("mprotect");
	    printf("segments[i].base: 0x%X\n", segments[i].base);
	    printf("segments[i].len: 0x%X\n", segments[i].len);
	    printf("segments[i].valid: 0x%X\n", segments[i].valid);
	    printf("addr: 0x%X, segments[i].valid-addr: 0x%X\n", addr,
		   segments[i].valid - addr);
	    printf("prot: 0x%X, len 0x%X\n", prot, len);
	    assert(FALSE);
	}
#ifdef DEBUG
    printf("os_protect: addr: 0x%X, len: 0x%X, end: 0x%X, prot 0x%X\n",
	   addr, len, addr + len, prot);
#endif
}

boolean valid_addr(os_vm_address_t addr)
{
    int i;

    for (i = 0; i < MAX_SEGMENTS; i++)
	if (segments[i].base <= addr
	    && addr < segments[i].base + segments[i].len) return TRUE;
    return FALSE;
}

void
segv_handler(int signal, int code, struct sigcontext *context)
{
    int i;
    os_vm_address_t addr, nvalid;

    sigsetmask(BLOCKABLE);

    addr = (os_vm_address_t) context->sc_sl.sl_ss.ss_cr21;	/* verify this!!! */
    for (i = 0; i < MAX_SEGMENTS; i++)
	if (segments[i].len != 0 && segments[i].base <= addr &&
	    addr < segments[i].base + segments[i].len)
	    break;
    if (i == MAX_SEGMENTS || addr < segments[i].valid)
	interrupt_handle_now(signal, code, context);
    else if (segments[i].protected <= addr) {
	if (!interrupt_maybe_gc(signal, code, context))
	    interrupt_handle_now(signal, code, context);
    } else {
	nvalid =
	    ((os_vm_address_t)
	     (((long) (addr + ALLOC_SIZE)) & ~((long) (ALLOC_SIZE - 1))));
	if (nvalid > segments[i].protected)
	    nvalid = segments[i].protected;

#ifdef DEBUG
	printf("Mapping: addr: 0x%08x, old: 0x%08x, new: 0x%08x\n",
	       addr, segments[i].valid, nvalid);
#endif

	if (mmap(segments[i].valid, nvalid - segments[i].valid,
		 OS_VM_PROT_ALL, MAP_ANONYMOUS | MAP_FIXED | MAP_PRIVATE, -1,
		 0) == (os_vm_address_t) - 1) {
	    perror("mmap");
	    printf("segments[i].base: 0x%X\n", segments[i].base);
	    printf("segments[i].len: 0x%X\n", segments[i].len);
	    printf("segments[i].valid: 0x%X\n", segments[i].valid);
	    printf("segments[i].protected: 0x%X\n", segments[i].protected);
	    printf("nvalid: 0x%X\n", nvalid);
	    printf("nvalid-segments[i].valid: 0x%X\n",
		   nvalid - segments[i].valid);
	    assert(FALSE);
	}
	segments[i].valid = nvalid;
    }
}

void
sigbus_handler(int signal, int code, struct sigcontext *context)
{
#ifdef DEBUG
    printf("Bus Error at 0x%X\n", context->sc_sl.sl_ss.ss_cr21);
#endif

    if (!interrupt_maybe_gc(signal, code, context))
	interrupt_handle_now(signal, code, context);
}

void
os_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGSEGV, segv_handler);
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
}

void
os_zero(os_vm_address_t addr, os_vm_size_t length)
{
    os_vm_address_t block_start;
    os_vm_size_t block_size;
    int i;

#ifdef PRINTNOISE
    fprintf(stderr, "os_zero: addr: 0x%08x, len: 0x%08x, end: 0x%X\n", addr,
	    length, addr + length);
#endif

    block_start = os_round_up_to_page(addr);

    length -= block_start - addr;
    block_size = os_trunc_size_to_page(length);

    if (block_start > addr)
	memset((char *) addr, 0, block_start - addr);
    if (block_size < length)
	assert(FALSE);

    if (block_size != 0) {
	/* Now deallocate and allocate the block so that it */
	/* faults in  zero-filled. */

	for (i = 0; i < MAX_SEGMENTS; i++)
	    if (segments[i].base <= block_start &&
		block_start < segments[i].base + segments[i].len)
		break;
	assert(i != MAX_SEGMENTS);
	assert(block_start + block_size == segments[i].base + segments[i].len);

	if (segments[i].valid > block_start) {
	    if (munmap(block_start, segments[i].valid - block_start) == -1) {
		perror("munmap");
		return;
	    }
	    segments[i].valid = block_start;
	}
    }
}

os_vm_address_t os_allocate(os_vm_size_t len)
{
    return (os_vm_address_t) malloc(len);
}

os_vm_address_t os_allocate_at(os_vm_address_t addr, os_vm_size_t len)
{
    return os_validate(addr, len);
}

void
os_deallocate(os_vm_address_t addr, os_vm_size_t len)
{
    free(addr);
}

os_vm_address_t
os_reallocate(os_vm_address_t addr, os_vm_size_t old_len, os_vm_size_t len)
{
    addr = (os_vm_address_t) realloc(addr, len);
    assert(addr != NULL);
    return addr;
}

int
getrusage(int who, struct rusage *rusage)
{
    static long ticks_per_sec = 0;
    static long usec_per_tick = 0;
    struct tms buf;
    clock_t uticks, sticks;

    memset(rusage, 0, sizeof(struct rusage));

    if (ticks_per_sec == 0) {
	ticks_per_sec = sysconf(_SC_CLK_TCK);
	usec_per_tick = 1000000 / ticks_per_sec;
    }

    if (times(&buf) == -1)
	return -1;

    if (who == RUSAGE_SELF) {
	uticks = buf.tms_utime;
	sticks = buf.tms_stime;
    } else if (who == RUSAGE_CHILDREN) {
	uticks = buf.tms_utime;
	sticks = buf.tms_stime;
    } else {
	errno = EINVAL;
	return -1;
    }

    rusage->ru_utime.tv_sec = uticks / ticks_per_sec;
    rusage->ru_utime.tv_usec = (uticks % ticks_per_sec) * usec_per_tick;
    rusage->ru_stime.tv_sec = sticks / ticks_per_sec;
    rusage->ru_stime.tv_usec = (sticks % ticks_per_sec) * usec_per_tick;

    return 0;
}

int
getdtablesize(void)
{
    struct rlimit rlp;

    assert(getrlimit(RLIMIT_NOFILE, &rlp) == 0);
    return rlp.rlim_cur;
}

unsigned long
gethostid(void)
{
    char hostname[256];
    struct hostent *hostent;
    static unsigned long addr = NULL;

    if (addr)
	return addr;

    if (gethostname(hostname, sizeof(hostname)) == -1) {
	perror("gethostname");
	return 0;
    }

    hostent = gethostbyname(hostname);
    if (hostent == NULL) {
	perror("gethostbyname");
	return 0;
    }

    addr = ((unsigned char *) (hostent->h_addr))[0] << 24 |
	((unsigned char *) (hostent->h_addr))[1] << 16 |
	((unsigned char *) (hostent->h_addr))[2] << 8 |
	((unsigned char *) (hostent->h_addr))[3];

    return addr;
}

int
getpagesize(void)
{
    return os_vm_page_size;
}
