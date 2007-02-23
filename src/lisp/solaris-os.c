/*
 * $Header: /project/cmucl/cvsroot/src/lisp/solaris-os.c,v 1.18 2006/07/17 15:50:52 rtoy Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is an experimental Solaris version based on sunos-os.c but
 * without the VM hack of unmapped pages for the GC trigger which
 * caused trouble when system calls were passed unmapped pages.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/file.h>

#include <unistd.h>
#include <errno.h>
#include <sys/param.h>

#include <dlfcn.h>

#include <sys/resource.h>

#if defined(GENCGC)
#include "lisp.h"
#include "gencgc.h"
#endif

#include "os.h"

#include "interrupt.h"

/* To get dynamic_0_space and friends */
#include "globals.h"
/* To get memory map */
#include "sparc-validate.h"

/* For type_ListPointer and NIL */
#include "internals.h"

#define EMPTYFILE "/tmp/empty"
#define ZEROFILE "/dev/zero"

/* ---------------------------------------------------------------- */


long os_vm_page_size = (-1);
static long os_real_page_size = (-1);

static int zero_fd = (-1);

static os_vm_size_t real_page_size_difference = 0;

static void
os_init_bailout(char *arg)
{
    char buf[500];

    sprintf(buf, "os_init: %s", arg);
    perror(buf);
    exit(1);
}

void
os_init(void)
{
    zero_fd = open(ZEROFILE, O_RDONLY);
    if (zero_fd < 0)
	os_init_bailout(ZEROFILE);

    os_vm_page_size = os_real_page_size = sysconf(_SC_PAGESIZE);

    if (os_vm_page_size > OS_VM_DEFAULT_PAGESIZE) {
	fprintf(stderr, "os_init: Pagesize too large (%ld > %d)\n",
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

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_NORESERVE;

    if (addr)
	flags |= MAP_FIXED;

    if (
	(addr =
	 (os_vm_address_t) mmap((void *) addr, len, OS_VM_PROT_ALL, flags,
				zero_fd, 0)) == (os_vm_address_t) - 1)
	perror("mmap");

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap((void *) addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    if (
	(addr =
	 (os_vm_address_t) mmap((void *) addr, len, OS_VM_PROT_ALL,
				MAP_PRIVATE | MAP_FIXED, fd,
				(off_t) offset)) == (os_vm_address_t) - 1)
	perror("mmap");

    return addr;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
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
	    fprintf(stderr, ";;;iflush %p - %lx\n", (void *) address, length);
	flush_icache((unsigned int *) address, length);
    }
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect((void *) address, length, prot) == -1)
	perror("mprotect");
}

static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char *beg = (char *) sbeg;
    char *end = (char *) sbeg + slen;
    char *adr = (char *) a;

    return (adr >= beg && adr < end);
}

boolean valid_addr(os_vm_address_t addr)
{
    /* Stolen from Linux-os.c */
    os_vm_address_t newaddr;

    newaddr = os_trunc_to_page(addr);

    /* Just assume address is valid if it lies within one of the known
       spaces.  (Unlike sunos-os which keeps track of every valid page.) */
    return (in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
	    || in_range_p(addr, STATIC_SPACE_START, STATIC_SPACE_SIZE)
	    || in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size)
#ifndef GENCGC
	    || in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
#endif
	    || in_range_p(addr, CONTROL_STACK_START, CONTROL_STACK_SIZE)
	    || in_range_p(addr, BINDING_STACK_START, BINDING_STACK_SIZE));
}

/* ---------------------------------------------------------------- */

/*
 * Running into the gc trigger page will end up here...
 */
#if defined(GENCGC)

void
segv_handle_now(HANDLER_ARGS)
{
    interrupt_handle_now(signal, code, context);
}

void
segv_handler(HANDLER_ARGS)
{
    caddr_t addr = code->si_addr;
    int page_index;

    SAVE_CONTEXT();

    page_index = find_page_index((void *) addr);

#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow(addr, context))
	return;
#endif

    /* check if the fault is within the dynamic space. */
    if (page_index != -1) {
	/* Un-protect the page */

	/* The page should have been marked write protected */
	if (!PAGE_WRITE_PROTECTED(page_index))
	    fprintf(stderr,
		    "*** Sigsegv in page not marked as write protected\n");
	os_protect((os_vm_address_t) page_address(page_index), PAGE_SIZE,
		   OS_VM_PROT_ALL);
	page_table[page_index].flags &= ~PAGE_WRITE_PROTECTED_MASK;
	page_table[page_index].flags |= PAGE_WRITE_PROTECT_CLEARED_MASK;

	return;
    }

    /*
     * Could be a C stack overflow.  Let's check
     */

    {
	struct rlimit rlimit;

	if (getrlimit(RLIMIT_STACK, &rlimit) == 0) {
	    /* The stack top here is based on the notes in sparc-validate.h */
	    char *stack_top = (char *) 0xffbf0000;
	    char *stack_bottom;

	    stack_bottom = stack_top - rlimit.rlim_cur;

	    /*
	     * Add a fudge factor.  Don't know why, but we get the signal
	     * sometime after the bottom of the stack, as computed above,
	     * has been reached.  (It seems to be 8K, so we use something
	     * larger.)
	     */

	    stack_bottom -= 16384;

	    if ((stack_bottom <= addr) && (addr <= stack_top)) {
		fprintf(stderr,
			"\nsegv_handler:  C stack overflow.  Try increasing stack limit (%ld).\n",
			rlimit.rlim_cur);

		segv_handle_now(signal, code, context);
	    }
	} else {
	    perror("getrlimit");
	}
    }

    /* a *real* protection fault */
    fprintf(stderr, "segv_handler: Real protection violation: %p\n", addr);
    segv_handle_now(signal, code, context);
}
#else
void
segv_handler(HANDLER_ARGS)
{
    caddr_t addr = code->si_addr;

    SAVE_CONTEXT();

#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow(addr, context))
	return;
#endif

    if (!interrupt_maybe_gc(signal, code, context)) {
	/* a *real* protection fault */
	fprintf(stderr, "segv_handler: Real protection violation: 0x%08x\n",
		addr);
	interrupt_handle_now(signal, code, context);
    }
}
#endif

void
os_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGSEGV, segv_handler);
}


/* function definitions for register lvalues */

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
		    "ensure_space: Failed to validate hole of %d bytes at 0x%08lX\n",
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
		"ensure_space: Failed to validate hold of %d bytes at 0x%08lX\n",
		HOLE_SIZE, (unsigned long) hole);
	exit(1);
    }
    os_protect(hole, HOLE_SIZE, 0);

#ifndef GENCGC
    hole = dynamic_space_size + (os_vm_address_t) dynamic_1_space;
    if (os_validate(hole, HOLE_SIZE) == NULL) {
	fprintf(stderr,
		"ensure_space: Failed to validate hole of %ld bytes at 0x%08X\n",
		HOLE_SIZE, (unsigned long) hole);
	exit(1);
    }
    os_protect(hole, HOLE_SIZE, 0);
#endif
}

void *
os_dlsym(const char *sym_name, lispobj lib_list)
{
    static void *program_handle;
    void *sym_addr = 0;

    if (!program_handle)
	program_handle = dlopen((void *) 0, RTLD_LAZY | RTLD_GLOBAL);
    if (lib_list != NIL) {
	lispobj lib_list_head;

	for (lib_list_head = lib_list;
	     lib_list_head != NIL; lib_list_head = (CONS(lib_list_head))->cdr) {
	    struct cons *lib_cons = CONS(CONS(lib_list_head)->car);
	    struct sap *dlhandle = (struct sap *) PTR(lib_cons->car);

	    sym_addr = dlsym((void *) dlhandle->pointer, sym_name);
	    if (sym_addr)
		return sym_addr;
	}
    }
    sym_addr = dlsym(program_handle, sym_name);

    return sym_addr;
}
