/*
 * $Header: /project/cmucl/cvsroot/src/lisp/purify.h,v 1.2 2005/01/13 19:55:00 fgilham Exp $
 */

#ifndef _PURIFY_H_
#define _PURIFY_H_

extern int purify(lispobj static_roots, lispobj read_only_roots);

#endif /* _PURIFY_H_ */
