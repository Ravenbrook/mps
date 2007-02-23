/* Routines that must be linked into the core for lisp to work. */
/* $Header: /project/cmucl/cvsroot/src/lisp/undefineds.c,v 1.7 2005/09/15 18:26:53 rtoy Exp $ */

#ifdef sun
#ifndef MACH
#if !defined(SUNOS) && !defined(SOLARIS)
#define SUNOS
#endif
#endif
#endif

typedef int func();

extern func
#define F(x) x,
#if !(defined(irix) || defined(SOLARIS))
/* XXXfixme next line probably wrong; was previous behavior */
#define D(x) x,
#else
#define D(x)
#endif
#include "undefineds.h"
#undef F
#undef D
  exit;				/* just a random function known to exist */

#if defined(SOLARIS) || defined(irix)

#ifdef irix
int errno;			/* hack to be sure works with newer libc without having to redump */

	   /* causes libc to be relocated to match cmucl rather than vice
	      versa */
#endif

extern int
#define F(x)
#define D(x) x,
#include "undefineds.h"
#undef F
#undef D
  errno;			/* a random variable known to exist */

int
reference_random_symbols(void)
{
    int a;

#if defined(SOLARIS) && defined(__GNUC__)
    /*
     * For some reason, gcc 3.0 still deletes function calls here, even
     * with -O0.  I don't know why.  So, we don't define the functions
     * here, but define them in the table below.
     */
#define F(x)
#else
#define F(x) x(0);
#endif
#define D(x) a+=x;
#include "undefineds.h"
#undef F
#undef D
    return a;
}

#if defined(SOLARIS) && defined(__GNUC__)
/*
 * If we reference the functions here, gcc 3.0 will leave them in the
 * object file.  However, don't try to put the data symbols here.  It
 * will cause CMUCL to crash doing a get_timezone.
 */

func *reference_random_symbols_table[] = {
#define F(x) x,
#define D(x)
#include "undefineds.h"
#undef F
#undef D
    exit			/* a random function known to exist */
};
#endif

#else

#if defined(SVR4)
extern char *tzname[];
extern int daylight;
extern long altzone;
extern long timezone;
#endif
func *reference_random_symbols[] = {
#define F(x) x,
    /* XXXfixme next line is probably wrong but was previous behavior */
#define D(x) &x,
#include "undefineds.h"
#undef F
#undef D
    exit			/* a random function known to exist */
};

#endif
