/*

 $Header: /project/cmucl/cvsroot/src/lisp/mach-os.h,v 1.3 2005/01/13 19:55:00 fgilham Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _MACH_OS_H_
#define _MACH_OS_H_

#include <mach.h>

typedef vm_address_t os_vm_address_t;
typedef vm_size_t os_vm_size_t;
typedef vm_offset_t os_vm_offset_t;
typedef vm_prot_t os_vm_prot_t;

#define OS_VM_PROT_READ VM_PROT_READ
#define OS_VM_PROT_WRITE VM_PROT_WRITE
#define OS_VM_PROT_EXECUTE VM_PROT_EXECUTE

#define OS_VM_DEFAULT_PAGESIZE	4096

#endif /* _MACH_OS_H_ */
