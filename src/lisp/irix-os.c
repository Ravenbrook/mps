/*
 * $Header: /project/cmucl/cvsroot/src/lisp/irix-os.c,v 1.2 2005/09/15 18:26:52 rtoy Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the IRIX version.  By Sean Hallgren.
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

/* #define DEBUG */

os_vm_size_t os_vm_page_size = (-1);

int zero_fd;

void
os_init()
{
    zero_fd = open("/dev/zero", O_RDONLY);
    os_vm_page_size = getpagesize();
}

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_AUTORESRV;

#ifdef DEBUG
    printf("os_validate: addr = %x, len = %x\n", addr, len);
#endif

    if (addr)
	flags |= MAP_FIXED;

    if ((addr = mmap(addr, len, OS_VM_PROT_ALL, flags, zero_fd, 0)) ==
	(os_vm_address_t) - 1)
	perror("mmap");

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
#ifdef DEBUG
    printf("os_invalidate: addr = %x, len = %x\n", addr, len);
#endif

    if (munmap(addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
#ifdef DEBUG
    printf("os_map: fd = %d, offset = %d, addr = %x, len = %x\n", fd, offset,
	   addr, len);
#endif

    if ((addr = mmap(addr, len, OS_VM_PROT_ALL, MAP_PRIVATE | MAP_FIXED, fd,
		     (off_t) offset)) == (os_vm_address_t) - 1)
	perror("mmap");

    return addr;
}

void
sanctify_for_execution(os_vm_address_t addr, os_vm_size_t len)
{
    char *end_addr = addr + len;

    addr = os_trunc_to_page(addr);
    len = end_addr - addr;

    if (mprotect(addr, len, OS_VM_PROT_ALL) == -1)
	perror("mprotect");
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    sanctify_for_execution(address, length);
}

void
os_protect(os_vm_address_t addr, os_vm_size_t len, os_vm_prot_t prot)
{
    if (mprotect(addr, len, prot) == -1)
	perror("mprotect");
}

boolean valid_addr(os_vm_address_t addr)
{
}

static void
sigbus_handler(int signal, int code, struct sigcontext *context)
{
    if (!interrupt_maybe_gc(signal, code, context))
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
