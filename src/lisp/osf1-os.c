/*
 * $Header: /project/cmucl/cvsroot/src/lisp/osf1-os.c,v 1.2 2005/09/15 18:26:52 rtoy Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the OSF1 version.  By Sean Hallgren.
 *
 */

#include <stdio.h>
#include <sys/file.h>
#include <errno.h>
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "interrupt.h"
#include "lispregs.h"
#include <sys/types.h>
#include <sys/sysinfo.h>
#include <sys/proc.h>

vm_size_t os_vm_page_size;

void
os_init()
{
    int buf[2] = { SSIN_UACPROC, UAC_SIGBUS | UAC_NOPRINT };
    int error;

    os_vm_page_size = OS_VM_DEFAULT_PAGESIZE;
    if (setsysinfo(SSI_NVPAIRS, buf, 1, NULL, NULL) == -1)
	perror("setsysinfo");
}

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;

    if (addr)
	flags |= MAP_FIXED;
    else
	flags |= MAP_VARIABLE;

    if ((addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0)) ==
	(os_vm_address_t) - 1)
	perror("mmap");

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
    if (
	(addr =
	 mmap(addr, len, OS_VM_PROT_ALL, MAP_PRIVATE | MAP_FILE | MAP_FIXED, fd,
	      (off_t) offset)) == (os_vm_address_t) - 1)
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

boolean valid_addr(os_vm_address_t addr)
{
    int ret;
    os_vm_address_t newaddr;

    newaddr = os_trunc_to_page(addr);
    if ((ret = mvalid(newaddr, newaddr - addr + 4, OS_VM_PROT_ALL)) == 0)
	return TRUE;
    else if (errno == EINVAL)
	perror("mvalid");
    return FALSE;
}

static void
sigbus_handler(int signal, int code, struct sigcontext *context)
{
    context->sc_pc -= 4;	/* pc is +4 on bus error!?!!? */
    if (arch_get_bad_addr(signal, code, context) &&
	context->sc_regs[reg_ALLOC] & 2) {
	context->sc_regs[reg_ALLOC] -= 2;
	interrupt_handle_pending(context);
    } else
	interrupt_handle_now(signal, code, context);
}

static void
sigsegv_handler(int signal, int code, struct sigcontext *context)
{
    if (!interrupt_maybe_gc(signal, code, context))
	interrupt_handle_now(signal, code, context);
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGSEGV, sigsegv_handler);
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
}
