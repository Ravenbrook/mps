/* $Header: /project/cmucl/cvsroot/src/lisp/validate.h,v 1.6 2005/01/13 19:55:01 fgilham Exp $ */

#ifndef _VALIDATE_H_
#define _VALIDATE_H_

#ifdef parisc
#include "hppa-validate.h"
#endif

#ifdef mips
#include "mips-validate.h"
#endif

#ifdef ibmrt
#include "rt-validate.h"
#endif

#ifdef sparc
#include "sparc-validate.h"
#endif

#if defined(i386) || defined(__x86_64)
#include "x86-validate.h"
#endif

#ifdef alpha
#include "alpha-validate.h"
#endif

#ifdef ppc
#include "ppc-validate.h"
#endif

extern void validate(void);

#endif /* _VALIDATE_H_ */
