/* $Header: /project/cmucl/cvsroot/src/lisp/core.h,v 1.6 2005/09/15 18:26:51 rtoy Exp $ */

#ifndef _CORE_H_
#define _CORE_H_

#include "lisp.h"

#define CORE_PAGESIZE OS_VM_DEFAULT_PAGESIZE
#define CORE_MAGIC (('C' << 24) | ('O' << 16) | ('R' << 8) | 'E')
#define CORE_END 3840
#define CORE_NDIRECTORY 3861
#define CORE_VALIDATE 3845
#define CORE_VERSION 3860
#define CORE_MACHINE_STATE 3862
#define CORE_INITIAL_FUNCTION 3863

#define DYNAMIC_SPACE_ID (1)
#define STATIC_SPACE_ID (2)
#define READ_ONLY_SPACE_ID (3)

struct ndir_entry {
#if !(defined(alpha) || defined(__x86_64))
    long identifier;
    long nwords;
    long data_page;
    long address;
    long page_count;
#else
    u32 identifier;
    u32 nwords;
    u32 data_page;
    u32 address;
    u32 page_count;
#endif
};

extern lispobj load_core_file(char *file);

#endif /* _CORE_H_ */
