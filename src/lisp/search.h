/*
 * $Header: /project/cmucl/cvsroot/src/lisp/search.h,v 1.2 2005/09/15 18:26:52 rtoy Exp $
 */

#ifndef _SEARCH_H_
#define _SEARCH_H_

extern boolean search_for_type(int type, lispobj ** start, int *count);
extern boolean search_for_symbol(char *name, lispobj ** start, int *count);

#endif
