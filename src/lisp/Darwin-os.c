/*
 * Darwin-os.c.
 * From FreeBSD-os.c 1.6 2000/10/24 13:32:30 dtc Exp
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the OSF1 version.  By Sean Hallgren.
 * Much hacked by Paul Werkowski
 * GENCGC support by Douglas Crosher, 1996, 1997.
 * Frobbed for OpenBSD by Pierre R. Mai, 2001.
 * Frobbed for Darwin by Pierre R. Mai, 2003.
 *
 * $Header: /project/cmucl/cvsroot/src/lisp/Darwin-os.c,v 1.7 2006/01/31 03:16:39 rtoy Exp $
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
#ifdef GENCGC
#include "gencgc.h"
#endif

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
/* #include <sys/proc.h> */
/* For timebase info */
#include <sys/param.h>
#include <sys/sysctl.h>


/* Need this to define ppc_saved_state_t (for 10.4) */
#include <mach/thread_status.h>

#include "validate.h"
vm_size_t os_vm_page_size;

#define DPRINTF(t,a) {if (t) fprintf a;}


/*
 * This is accessed by Lisp!  Make this a static symbol?
 */
int cycles_per_tick = 1;

/*
 * Compute the conversion factor from the numbef of time base ticks to
 * clock frequency.  The timebase counter on PPC is not a cycle
 * counter; we have to derive the relationship ourselves.
 */

void
timebase_init()
{
    int mib[2];
    int tbfrequency;
    int cpufrequency;
    unsigned int miblen;
    size_t len;

    mib[0] = CTL_HW;
#ifndef HW_TB_FREQ
    /*
     * Mac OS X 10.2.8 doesn't have this, so we take it from 10.3.8,
     * which does.
     */
#define HW_TB_FREQ	23
#endif
    mib[1] = HW_TB_FREQ;
    miblen = 2;
    len = sizeof(tbfrequency);

    if (sysctl(mib, miblen, &tbfrequency, &len, NULL, 0) == -1) {
	perror("Error getting HW_TB_FREQ from sysctl: ");
    }

    mib[0] = CTL_HW;
    mib[1] = HW_CPU_FREQ;
    miblen = 2;
    len = sizeof(cpufrequency);
    if (sysctl(mib, miblen, &cpufrequency, &len, NULL, 0) == -1) {
	perror("Error getting HW_CPU_FREQ from sysctl: ");
    }

    cycles_per_tick = cpufrequency / tbfrequency;
}


void
os_init(void)
{
    os_vm_page_size = OS_VM_DEFAULT_PAGESIZE;
    timebase_init();
}

int
os_get_errno(void)
{
    return errno;
}

int
os_set_errno(int newvalue)
{
    errno = newvalue;
    return errno;
}


int *
sc_reg(os_context_t * context, int offset)
{
    ppc_saved_state_t *state = &context->uc_mcontext->ss;

    switch (offset) {
      case 0:
	  return &state->r0;
      case 1:
	  return &state->r1;
      case 2:
	  return &state->r2;
      case 3:
	  return &state->r3;
      case 4:
	  return &state->r4;
      case 5:
	  return &state->r5;
      case 6:
	  return &state->r6;
      case 7:
	  return &state->r7;
      case 8:
	  return &state->r8;
      case 9:
	  return &state->r9;
      case 10:
	  return &state->r10;
      case 11:
	  return &state->r11;
      case 12:
	  return &state->r12;
      case 13:
	  return &state->r13;
      case 14:
	  return &state->r14;
      case 15:
	  return &state->r15;
      case 16:
	  return &state->r16;
      case 17:
	  return &state->r17;
      case 18:
	  return &state->r18;
      case 19:
	  return &state->r19;
      case 20:
	  return &state->r20;
      case 21:
	  return &state->r21;
      case 22:
	  return &state->r22;
      case 23:
	  return &state->r23;
      case 24:
	  return &state->r24;
      case 25:
	  return &state->r25;
      case 26:
	  return &state->r26;
      case 27:
	  return &state->r27;
      case 28:
	  return &state->r28;
      case 29:
	  return &state->r29;
      case 30:
	  return &state->r30;
      case 31:
	  return &state->r31;
      case 34:
	  /*
	   * Not sure if this is really defined anywhere, but after
	   * r31 is cr, xer, lr, and ctr.  So we let 34 be lr.
	   */
	  return &state->lr;
      case 35:
	  return &state->ctr;
      case 41:
	  return &context->uc_mcontext->es.dar;
      case 42:
	  return &context->uc_mcontext->es.dsisr;
    }

    return (int *) 0;
}

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
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;

    if (addr)
	flags |= MAP_FIXED;
    else
	flags |= MAP_VARIABLE;

    DPRINTF(0, (stderr, "os_validate %x %d => ", addr, len));

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == (os_vm_address_t) - 1) {
	perror("mmap");
	return NULL;
    }

    DPRINTF(0, (stderr, "%x\n", addr));

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    DPRINTF(0, (stderr, "os_invalidate %x %d\n", addr, len));

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
    /* see ppc-arch.c */
    ppc_flush_icache(address, length);
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
    int ret;
    os_vm_address_t newaddr;

    newaddr = os_trunc_to_page(addr);

    if (in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
	|| in_range_p(addr, STATIC_SPACE_START, STATIC_SPACE_SIZE)
	|| in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size)
#ifndef GENCGC
	|| in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
#endif
	|| in_range_p(addr, CONTROL_STACK_START, CONTROL_STACK_SIZE)
	|| in_range_p(addr, BINDING_STACK_START, BINDING_STACK_SIZE))
	return TRUE;
    return FALSE;
}


static void
sigbus_handler(HANDLER_ARGS)
{
#if defined(GENCGC)
    caddr_t fault_addr = code->si_addr;
    int page_index = find_page_index((void *) fault_addr);
#endif
    
    SAVE_CONTEXT();

    DPRINTF(0, (stderr, "sigbus:\n"));
    DPRINTF(0, (stderr, " PC       = %p\n", SC_PC(context)));
    DPRINTF(0, (stderr, " ALLOC-TN = %p\n", SC_REG(context, reg_ALLOC)));
    DPRINTF(0, (stderr, " CODE-TN  = %p\n", SC_REG(context, reg_CODE)));
    DPRINTF(0, (stderr, " LRA-TN   = %p\n", SC_REG(context, reg_LRA)));
    DPRINTF(0, (stderr, " CFP-TN   = %p\n", SC_REG(context, reg_CFP)));
    DPRINTF(0, (stderr, " FDEFN-TN = %p\n", SC_REG(context, reg_FDEFN)));
    DPRINTF(0, (stderr, " foreign_function_call = %d\n", foreign_function_call_active));
    
#if defined(GENCGC)
#if defined(SIGSEGV_VERBOSE)
    fprintf(stderr, "Signal %d, fault_addr=%x, page_index=%d:\n",
	    signal, fault_addr, page_index);
#endif
    
    /* Check if the fault is within the dynamic space. */
    if (page_index != -1) {
	/* Un-protect the page */

	/* The page should have been marked write protected */
	if (!PAGE_WRITE_PROTECTED(page_index))
	    fprintf(stderr,
		    "*** Sigsegv in page not marked as write protected\n");

	os_protect(page_address(page_index), PAGE_SIZE, OS_VM_PROT_ALL);
	page_table[page_index].flags &= ~PAGE_WRITE_PROTECTED_MASK;
	page_table[page_index].flags |= PAGE_WRITE_PROTECT_CLEARED_MASK;

	return;
    }
#endif

    if (!interrupt_maybe_gc(signal, code, context)) {
	interrupt_handle_now(signal, code, context);
    }

    /* Work around G5 bug; fix courtesy gbyers via chandler */
    sigreturn(context);
}

void
os_install_interrupt_handlers(void)
{
/*  interrupt_install_low_level_handler(SIGSEGV, sigsegv_handler); */
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
}

void
map_core_sections(char *exec_name)
{
    fprintf(stderr, "Linked cores not supported for Darwin!\n");
    exit(1);
}

#define RTLD_LAZY 1
#define RTLD_NOW 2
#define RTLD_GLOBAL 0x100


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
