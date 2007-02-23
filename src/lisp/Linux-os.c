/*
 * Linux-os.c. 
 * From FreeBSD-os.c
 * From osf1-os.c,v 1.1 94/03/27 15:30:51 hallgren Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the OSF1 version.  By Sean Hallgren.
 * Much hacked by Paul Werkowski
 * Morfed from the FreeBSD file by Peter Van Eynde (July 1996)
 * GENCGC support by Douglas Crosher, 1996, 1997.
 * Alpha support by Julian Dolby, 1999.
 *
 * $Header: /project/cmucl/cvsroot/src/lisp/Linux-os.c,v 1.26 2006/03/17 02:56:45 rtoy Exp $
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include <errno.h>
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "internals.h"
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <netdb.h>
#include <link.h>
#include <dlfcn.h>
#include <fpu_control.h>
#include <assert.h>

#include "validate.h"
size_t os_vm_page_size;

#define DPRINTF(t,a) {if (t) fprintf a;}

#if defined GENCGC
#include "gencgc.h"
#endif

#if ((LINUX_VERSION_CODE >= linuxversion(2,1,0)) || (__GNU_LIBRARY__ >= 6))
int PVE_stub_errno;
#endif

#if ((LINUX_VERSION_CODE >= linuxversion(2,1,0)) || (__GNU_LIBRARY__ >= 6))
void
update_errno(void)
{
    PVE_stub_errno = errno;
}
#endif


void
os_init(void)
{
    struct utsname name;

    uname(&name);

    /* We need this for mmap */

    if (name.release[0] < '2') {
	printf("Linux version must be later then 2.0.0!\n");
	exit(2);
    }

    os_vm_page_size = getpagesize();

#if defined(i386) || defined(__x86_64)
#if 0
    setfpucw(0x1272 | 4 | 8 | 16 | 32);	/* No interrupts */
#else
    /*
     * Round to nearest, double-float precision (not extended!), disable
     * interrupts for everything except invalid.
     */
    setfpucw(_FPU_RC_NEAREST | _FPU_DOUBLE | _FPU_MASK_PM | _FPU_MASK_UM |
	     _FPU_MASK_OM | _FPU_MASK_ZM | _FPU_MASK_DM);
#endif
#endif
}

#ifdef i386
#if (LINUX_VERSION_CODE >= linuxversion(2,1,0)) || (__GNU_LIBRARY__ >= 6)
int
sc_reg(struct sigcontext *c, int offset)
#else
int
sc_reg(struct sigcontext_struct *c, int offset)
#endif
{
    switch (offset) {
      case 0:
	  return c->eax;
      case 2:
	  return c->ecx;
      case 4:
	  return c->edx;
      case 6:
	  return c->ebx;
      case 8:
	  return c->esp;
      case 10:
	  return c->ebp;
      case 12:
	  return c->esi;
      case 14:
	  return c->edi;
    }
    return 0;
}
#endif

#ifdef __x86_64
int
sc_reg(struct sigcontext *c, int offset)
{
    switch (offset) {
      case 0:
	  return c->rax;
      case 2:
	  return c->rcx;
      case 4:
	  return c->rdx;
      case 6:
	  return c->rbx;
      case 8:
	  return c->rsp;
      case 10:
	  return c->rbp;
      case 12:
	  return c->rsi;
      case 14:
	  return c->rdi;
      case 16:
	  return c->r8;
      case 18:
	  return c->r9;
      case 20:
	  return c->r10;
      case 22:
	  return c->r11;
      case 24:
	  return c->r12;
      case 26:
	  return c->r13;
      case 28:
	  return c->r14;
      case 30:
	  return c->r15;
    }
    return 0;
}
#endif

void
os_save_context(void)
{
    /*
     * Called from interrupt handlers so C stuff knows things set in Lisp.
     */
}

void
os_set_context(void)
{
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
    os_vm_address_t out_addr;

    DPRINTF(0, (stderr, "os_validate %p %d => ", addr, len));

    out_addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (out_addr == (os_vm_address_t) - 1) {
	perror("mmap");
	return NULL;
    }

    if (addr && (addr != out_addr)) {
	fprintf(stderr,
		"Couldn't mmap at %p, len %d; got mapping at %p instead", addr,
		len, out_addr);
	munmap(addr, len);
	return NULL;
    }

    DPRINTF(0, (stderr, "%p\n", out_addr));

    return out_addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    DPRINTF(0, (stderr, "os_invalidate %p %d\n", addr, len));

    if (munmap(addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    addr = mmap(addr, len,
		OS_VM_PROT_ALL,
		MAP_PRIVATE | MAP_FILE | MAP_FIXED, fd, (off_t) offset);

    if (addr == (os_vm_address_t) - 1)
	perror("mmap");

    return addr;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot) == -1)
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

boolean
valid_addr(os_vm_address_t addr)
{
    os_vm_address_t newaddr;

    newaddr = os_trunc_to_page(addr);

    if (in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
	|| in_range_p(addr, STATIC_SPACE_START, STATIC_SPACE_SIZE)
	|| in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size)
	|| in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
	|| in_range_p(addr, CONTROL_STACK_START, CONTROL_STACK_SIZE)
	|| in_range_p(addr, BINDING_STACK_START, BINDING_STACK_SIZE))
	return TRUE;
    return FALSE;
}


#if defined GENCGC

static void
sigsegv_handle_now(HANDLER_ARGS)
{
    interrupt_handle_now(signal, contextstruct);
}

static int tramp_signal;
static struct sigcontext tramp_contextstruct;

static void
sigsegv_handler_tramp(void)
{
    sigsegv_handle_now(tramp_signal, tramp_contextstruct);
    assert(0);
}

void
sigsegv_handler(HANDLER_ARGS)
{
    GET_CONTEXT
#if (LINUX_VERSION_CODE >= linuxversion(2,1,0)) || (__GNU_LIBRARY__ >= 6)
    int fault_addr = ((struct sigcontext *) (&contextstruct))->cr2;
#else
    int fault_addr = ((struct sigcontext_struct *) (&contextstruct))->cr2;
#endif
    int page_index = find_page_index((void *) fault_addr);

#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow((void *) fault_addr, &contextstruct))
	return;
#endif

    /* check if the fault is within the dynamic space. */
    if (page_index != -1) {
	/* Un-protect the page */

	/* The page should have been marked write protected */
	if (!PAGE_WRITE_PROTECTED(page_index))
	    fprintf(stderr,
		    "*** Sigsegv in page not marked as write protected\n");
	os_protect(page_address(page_index), 4096, OS_VM_PROT_ALL);
	page_table[page_index].flags &= ~PAGE_WRITE_PROTECTED_MASK;
	page_table[page_index].flags |= PAGE_WRITE_PROTECT_CLEARED_MASK;

	return;
    }
#if defined(__x86_64)
    DPRINTF(0, (stderr, "sigsegv: rip: %p\n", context->rip));
#else
    DPRINTF(0, (stderr, "sigsegv: eip: %lx\n", context->eip));
#endif

#ifdef RED_ZONE_HIT
    {
	/* Switch back to the normal stack and invoke the Lisp signal
	   handler there.  Global variables are used to pass the context
	   to the other stack. */
	tramp_signal = signal;
	tramp_contextstruct = contextstruct;
	SC_PC(context) = sigsegv_handler_tramp;
	return;
    }
#endif

    sigsegv_handle_now(signal, contextstruct);
}
#else
static void
sigsegv_handler(HANDLER_ARGS)
{
    os_vm_address_t addr;

#ifdef i386
    GET_CONTEXT
#endif
	DPRINTF(0, (stderr, "sigsegv\n"));
#ifdef i386
    interrupt_handle_now(signal, contextstruct);
#else
#define CONTROL_STACK_TOP (((char*) CONTROL_STACK_START) + CONTROL_STACK_SIZE)

    addr = arch_get_bad_addr(signal, code, context);

    if (addr != NULL && context->sc_regs[reg_ALLOC] & (1 << 63)) {
	context->sc_regs[reg_ALLOC] -= (1 << 63);
	interrupt_handle_pending(context);
    } else if (addr > CONTROL_STACK_TOP && addr < BINDING_STACK_START) {
	fprintf(stderr, "Possible stack overflow at 0x%08lX!\n", addr);
	/* try to fix control frame pointer */
	while (!(CONTROL_STACK_START <= *current_control_frame_pointer &&
		 *current_control_frame_pointer <= CONTROL_STACK_TOP))
	    ((char *) current_control_frame_pointer) -= sizeof(lispobj);
	ldb_monitor();
    } else if (!interrupt_maybe_gc(signal, code, context))
	interrupt_handle_now(signal, code, context);
#endif
}
#endif

static void
sigbus_handler(HANDLER_ARGS)
{
#if defined(i386) || defined(__x86_64)
    GET_CONTEXT
#endif
	DPRINTF(1, (stderr, "sigbus:\n"));	/* there is no sigbus in linux??? */
#if defined(i386) || defined(__x86_64)
    interrupt_handle_now(signal, contextstruct);
#else
    interrupt_handle_now(signal, code, context);
#endif
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGSEGV, sigsegv_handler);
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
}

/* Some symbols, most notably stat and lstat, don't appear at all in
   the glibc .so files as a result of preprocessor and linker magic /
   braindamage.  So, try falling back to a stub in linux-stubs.S that
   will call the proper function if it's one of those. */

static void *
dlsym_fallback(void *handle, const char *name)
{
    char newsym[1024];
    void *sym_addr;

    strcpy(newsym, "PVE_stub_");
    strcat(newsym, name);
    sym_addr = dlsym(handle, newsym);
#ifdef DEBUG
    if (sym_addr == 0) {
	fputs(dlerror(), stderr);
    }
#endif
    return sym_addr;
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
    if (!sym_addr && dlerror()) {
	return dlsym_fallback(program_handle, sym_name);
    } else {
	return sym_addr;
    }
}

int
get_h_errno()
{
    return h_errno;
}
