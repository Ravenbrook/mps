/*
 * $Header: /project/cmucl/cvsroot/src/lisp/mach-os.c,v 1.4 2005/09/15 18:26:52 rtoy Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the Mach version.
 *
 */

#include <stdio.h>
#include <mach.h>
#include <sys/file.h>
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "interrupt.h"

#define MAX_SEGS 32

static struct segment {
    vm_address_t start;
    vm_size_t length;
} addr_map[MAX_SEGS];
static int segments = -1;

vm_size_t os_vm_page_size;

#if defined(i386) || defined(parisc)
mach_port_t
task_self()
{
    return mach_task_self();
}
#endif

void
os_init()
{
    os_vm_page_size = vm_page_size;
}

os_vm_address_t
os_validate(vm_address_t addr, vm_size_t len)
{
    kern_return_t res;

    res = vm_allocate(task_self(), &addr, len, addr == NULL);

    if (res != KERN_SUCCESS)
	return 0;

    segments = -1;

    vm_protect(task_self(), addr, len, FALSE,
	       VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE);

    return addr;
}

void
os_invalidate(vm_address_t addr, vm_size_t len)
{
    kern_return_t res;

    res = vm_deallocate(task_self(), addr, len);

    if (res != KERN_SUCCESS)
	mach_error("Could not vm_allocate memory: ", res);

    segments = -1;
}

vm_address_t
os_map(int fd, int offset, vm_address_t addr, vm_size_t len)
{
    kern_return_t res;

    res = map_fd(fd, offset, &addr, 0, len);

    if (res != KERN_SUCCESS) {
	char buf[256];

	sprintf(buf, "Could not map_fd(%d, %d, 0x%08x, 0x%08x): ",
		fd, offset, addr, len);
	mach_error(buf, res);

	lseek(fd, offset, L_SET);
	read(fd, addr, len);
    }

    segments = -1;

    vm_protect(task_self(), addr, len, FALSE,
	       VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE);

    return addr;
}

void
os_flush_icache(vm_address_t address, vm_size_t length)
{
#ifdef mips
    vm_machine_attribute_val_t flush;
    kern_return_t kr;

    flush = MATTR_VAL_ICACHE_FLUSH;

    kr = vm_machine_attribute(task_self(), address, length,
			      MATTR_CACHE, &flush);
    if (kr != KERN_SUCCESS)
	mach_error("Could not flush the instruction cache", kr);
#endif
}

void
os_protect(vm_address_t address, vm_size_t length, vm_prot_t protection)
{
    vm_protect(task_self(), address, length, FALSE, protection);
}

boolean
valid_addr(test)
     vm_address_t test;
{
    vm_address_t addr;
    vm_size_t size;
    int bullshit;
    int curseg;

    if (segments == -1) {
	addr = 0;
	curseg = 0;

	while (1) {
	    if (vm_region
		(task_self(), &addr, &size, &bullshit, &bullshit, &bullshit,
		 &bullshit, &bullshit, &bullshit) != KERN_SUCCESS)
		break;

	    if (curseg > 0
		&& addr_map[curseg - 1].start + addr_map[curseg - 1].length ==
		addr) addr_map[curseg - 1].length += size;
	    else {
		addr_map[curseg].start = addr;
		addr_map[curseg].length = size;
		curseg++;
	    }

	    addr += size;
	}

	segments = curseg;
    }

    for (curseg = 0; curseg < segments; curseg++)
	if (addr_map[curseg].start <= test
	    && test < addr_map[curseg].start + addr_map[curseg].length)
	    return TRUE;
    return FALSE;
}

#ifndef ibmrt
static void
sigbus_handler(int signal, int code, struct sigcontext *context)
{
    if (!interrupt_maybe_gc(signal, code, context))
	interrupt_handle_now(signal, code, context);
}
#endif

void
os_install_interrupt_handlers(void)
{
#ifndef ibmrt
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
#endif
#ifdef mips
    interrupt_install_low_level_handler(SIGSEGV, sigbus_handler);
#endif
}
