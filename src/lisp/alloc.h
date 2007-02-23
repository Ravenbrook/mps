/* $Header: /project/cmucl/cvsroot/src/lisp/alloc.h,v 1.2 2002/05/02 21:10:52 toy Exp $ */

#ifndef _ALLOC_H_
#define _ALLOC_H_

#include "lisp.h"

extern lispobj alloc_cons(lispobj car, lispobj cdr);
extern lispobj alloc_number(long n);
extern lispobj alloc_string(char *str);
extern lispobj alloc_sap(void *ptr);

#endif /* _ALLOC_H_ */
