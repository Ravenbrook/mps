/* impl.h.check: ASSERTION INTERFACE
 *
 * $HopeName: MMsrc!check.h(MMdevel_assertid.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This header defines a family of AVER and NOTREACHED macros. The
 * macros should be used to instrument and annotate code with
 * invariants, and so provide both interface and internal consistency
 * checks.
 *
 * Non-obvious AVER statements should always be accompanied by a
 * comment.
 *
 * .disable: When assertions are disabled, AVER expands to something
 * which evaluates the condition but discards the result. Compilers
 * will throw the code away, but check its syntax.
 */

#ifndef check_h
#define check_h

#include "config.h"
#include "misc.h"
#include "mpslib.h"


typedef void (*AssertHandler)(const char *cond,
                              const char *hopename,
                              const char *file,
                              unsigned line,
                              unsigned id);
extern AssertHandler AssertInstall(AssertHandler handler);
extern AssertHandler AssertDefault(void);

extern void AssertFail(const char *cond,
                       const char *hopename,
                       const char *file,
                       unsigned line,
                       unsigned id);

#define ASSERT(id, cond) \
  BEGIN \
    if(cond) NOOP; else \
      AssertFail(#cond, \
                 FileSrcIdStruct.hopename, \
                 FileSrcIdStruct.file, \
                 __LINE__, \
                 id); \
  END

#define NOCHECK(cond) \
  BEGIN \
    (void)sizeof(cond); \
  END

#define NOTREACHED \
  BEGIN \
    AssertFail("unreachable statement", \
               FileSrcIdStruct.hopename, \
               FileSrcIdStruct.file, \
               __LINE__, \
               id); \
  END

#ifdef CHECK_ASSERT
#define CHECKC(id, cond)        ASSERT(id, cond)
#else
#define CHECKC(id, cond) \
  BEGIN \
    if(cond) \
      NOOP; \
    else \
      return FALSE; \
  END
#endif


/* CHECKT -- check type simply
 *
 * Must be thread safe.  See design.mps.interface.c.thread-safety
 * and design.mps.interface.c.check.space.
 */

#define CHECKT(type, val) \
  ((val) != NULL && (val)->sig == type ## Sig)

#if defined(CHECK_SHALLOW)
#define CHECKS(id, type, val)   CHECKC(id, CHECKT(type, val))
#define CHECKL(id, cond)        CHECKC(id, cond)
#define CHECKD(id, type, val)   CHECKC(id, CHECKT(type, val))
#define CHECKU(id, type, val)   CHECKC(id, CHECKT(type, val))
#elif defined(CHECK_DEEP)
#define CHECKS(id, type, val)   CHECKC(id, CHECKT(type, val))
#define CHECKL(id, cond)        CHECKC(id, cond)
#define CHECKD(id, type, val)   CHECKC(id, type ## Check(val))
#define CHECKU(id, type, val)   CHECKC(id, CHECKT(type, val))
#else /* neither CHECK_DEEP nor CHECK_SHALLOW */
#define CHECKS(id, type, val)   CHECKC(id, CHECKT(type, val))
#define CHECKL(id, cond)        NOCHECK(cond)
#define CHECKD(id, type, val)   NOCHECK(type ## Check(val))
#define CHECKU(id, type, val)   NOCHECK(CHECKT(type, val))
#endif /* CHECK_SHALLOW or CHECK_DEEP */

#endif /* check_h */
