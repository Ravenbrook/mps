/*

 $Header: /project/cmucl/cvsroot/src/lisp/hpux-os.h,v 1.3 2005/01/13 19:55:00 fgilham Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _HPUX_OS_H_
#define _HPUX_OS_H_

#include <sys/mman.h>

typedef caddr_t os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096

#endif /* _HPUX_OS_H_ */
