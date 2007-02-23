/*
 * FreeBSD-os.c. Maybe could be just BSD-os.c
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
 * GENCGC support by Douglas Crosher, 1996, 1997.
 *
 * $Header: /project/cmucl/cvsroot/src/lisp/FreeBSD-os.c,v 1.12 2006/05/30 22:42:05 fgilham Exp $
 *
 */

#include "os.h"
#include <sys/file.h>
#include <errno.h>
#include "./signal.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "internals.h"

#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/proc.h>
#include <dlfcn.h>
#include "validate.h"

#if defined GENCGC
#include "gencgc.h"
#endif

#if __FreeBSD_version > 400000
/* The lisp runtime is dynamically linked, but we need a definition of
   errno for genesis. */
#undef errno
int errno;
#endif

vm_size_t os_vm_page_size;


void
os_init(void)
{
    os_vm_page_size = getpagesize();
}

int
sc_reg(struct sigcontext *c, int offset)
{
    switch (offset) {
      case 0:
	  return c->sc_eax;
      case 2:
	  return c->sc_ecx;
      case 4:
	  return c->sc_edx;
      case 6:
	  return c->sc_ebx;
      case 8:
	  return c->sc_esp;
      case 10:
	  return c->sc_ebp;
      case 12:
	  return c->sc_esi;
      case 14:
	  return c->sc_edi;
    }

    return 0;
}

void
os_save_context(void)
{
    /* Called from interrupt handlers so C stuff knows things set in
       Lisp.  */
}

void
os_set_context(void)
{
}

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;

    if (addr)
	flags |= MAP_FIXED;
    else
	flags |= MAP_VARIABLE;

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == (os_vm_address_t) - 1) {
	perror("mmap");
	return NULL;
    }

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    addr = mmap(addr, len, OS_VM_PROT_ALL,
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

    return adr >= beg && adr < end;
}

boolean valid_addr(os_vm_address_t addr)
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
sigbus_handler(int signal, int code, struct sigcontext *context,
	       void *fault_addr)
{
    int page_index;

#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow(fault_addr, context))
	return;
#endif

#if defined GENCGC
    page_index = find_page_index(fault_addr);

    /* Check if the fault is within the dynamic space. */
    if (page_index != -1) {
	/* Un-protect the page */

	/* The page should have been marked write protected */
	if (!PAGE_WRITE_PROTECTED(page_index))
	    fprintf(stderr,
		    "*** Sigbus in page not marked as write protected\n");

	os_protect(page_address(page_index), 4096, OS_VM_PROT_ALL);
	page_table[page_index].flags &= ~PAGE_WRITE_PROTECTED_MASK;
	page_table[page_index].flags |= PAGE_WRITE_PROTECT_CLEARED_MASK;

	return;
    }
#endif /* GENCGC */

    interrupt_handle_now(signal, code, context);
}

static void
sigsegv_handler(int signal, int code, struct sigcontext *context)
{
    interrupt_handle_now(signal, code, context);
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler
	(SIGSEGV, (void (*)(HANDLER_ARGS)) sigsegv_handler);
    interrupt_install_low_level_handler
	(SIGBUS, (void (*)(HANDLER_ARGS)) sigbus_handler);
}

void *
os_dlsym(const char *sym_name, lispobj lib_list)
{
    if (lib_list != NIL) {
	lispobj lib_list_head;

	for (lib_list_head = lib_list;
	     lib_list_head != NIL; lib_list_head = CONS(lib_list_head)->cdr) {
	    struct cons *lib_cons = CONS(CONS(lib_list_head)->car);
	    struct sap *dlhandle = (struct sap *) PTR(lib_cons->car);
	    void *sym_addr = dlsym((void *) dlhandle->pointer, sym_name);

	    if (sym_addr)
		return sym_addr;
	}
    }

    return dlsym(RTLD_DEFAULT, sym_name);
}
