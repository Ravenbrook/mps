/*

 $Header: /project/cmucl/cvsroot/src/lisp/NetBSD-os.h,v 1.4 2005/09/15 18:26:50 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _NETBSD_OS_H_
#define _NETBSD_OS_H_

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/signal.h>
#include <ucontext.h>

#define MAP_ANONYMOUS MAP_ANON
#define MAP_VARIABLE 0

typedef caddr_t os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096

#define POSIX_SIGS
#define HANDLER_ARGS int signal, siginfo_t *code, ucontext_t *context
#define CODE(code)  ((code) ? code->si_code : 0)
#define os_context_t ucontext_t

int sc_reg(ucontext_t *, int);

#define PROTECTION_VIOLATION_SIGNAL SIGSEGV

#endif /* _NETBSD_OS_H_ */
