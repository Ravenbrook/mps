/*  impl.c.protso
 *
 *                  PROTECTION FOR Solaris
 *
 *  $HopeName: MMsrc!protso.c(MMdevel_protoposm_1.1) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 */

#include "std.h"
#include "prot.h"
#include "fault.h"
#include "lock.h"
#include "lockst.h"
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <signal.h>
#include <siginfo.h>
#include <sys/mman.h>

#ifndef OS_SOLARIS
#error "protso.c is Solaris specific, but OS_SOLARIS is not set"
#endif

/* Fix up unprototyped system calls.  */
/* Note that these are not fixed up by std.h because that only fixes */
/* up discrepancies with ANSI. */

extern int getpagesize(void);
extern pid_t getpid(void);
extern int kill(pid_t, int);

/* Crap that can't be included via "open sesame" */
/* definitions for the sa_flags field */
#define	SA_SIGINFO	0x00000008
/*
 * SIGSEGV signal codes
 */

#define	SEGV_MAPERR	1	/* address not mapped to object */
#define	SEGV_ACCERR	2	/* invalid permissions */
#define	NSIGSEGV	2



/*  == Protection Granularity ==
 *
 *  The granularity of protection is one page under Solaris.
 */

Addr ProtGrain(void)
{
  Addr grain;

  grain = (Addr)getpagesize();
  AVER(IsPoT(grain));

  return(grain);
}


/* The previously-installed signal action, as returned by */
/* sigaction(3).  See ProtSetup. */

static struct sigaction sigNext;


/*  == Protection Signal Handler ==
 *
 *  This is the signal handler installed by ProtSetup to deal with
 *  protection faults.  It is installed on the SIGSEGV signal.
 *  It decodes the protection fault details from the signal context
 *  and passes them to FaultDispatch, which attempts to handle the
 *  fault and remove its cause.  If the fault is handled, then
 *  the handler returns and execution resumes.  If it isn't handled,
 *  then sigHandle does its best to pass the signal on to the
 *  previously installed signal handler (sigNext).
 */
 
static void sigHandle(int sig, siginfo_t *info, void *context)
{
  AVER(sig == SIGSEGV);
  AVER(info != NULL);

  if(info->si_code == SEGV_ACCERR) {
    ProtMode mode;
    Addr base, limit;

    /* We can't determine the access mode (read, write, etc.) */
    /* under Solaris without decoding the faulting instruction. */
    /* Don't bother, yet.  We can do this if necessary. */

    mode = ProtREAD | ProtWRITE;

    /* We assume that the access is for one word at the address. */
    /* @@@@ What happens to ldd across a page boundary? */
    /* ldd has to be dword aligned */

    base = (Addr)info->si_addr;
    limit = base + sizeof(Addr);

    /* Offer each protection structure the opportunity to handle the */
    /* exception.  If it succeeds, then allow the mutator to continue. */

    if(FaultDispatch(base, limit, mode))
      return;
  }

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler. */

  /* --- This is really weak.  Need to implement rest of the contract of sigaction */
  (*sigNext.sa_handler)(sig, info, context);
}


/*  == Global Protection Setup ==
 *
 *  Under Solaris, the global setup involves installing a signal handler
 *  on SIGSEGV to catch and handle protection faults (see sigHandle).
 *  The previous handler is recorded so that it can be reached from
 *  sigHandle if it fails to handle the fault.
 *
 *  NOTE: There are problems with this approach:
 *    1. we can't honor the wishes of the sigvec(2) entry for the
 *       previous handler,
 *    2. what if this thread is suspended just after calling signal(3)?
 *       The sigNext variable will never be initialized!
 */

void ProtSetup(void)
{
  struct sigaction sa;
  int result;

  sa.sa_handler = sigHandle;
  sa.sa_flags = SA_SIGINFO;

  result = sigaction(SIGSEGV, &sa, &sigNext);
  AVER(result == 0);
}


/*  == Set Protection ==
 *
 *  This is just a thin veneer on top of mprotect(2).
 */

void ProtSet(Addr base, Addr limit, ProtMode mode)
{
#ifdef DEBUG_ASSERT
  Addr grain = ProtGrain();
#endif
  int flags;

  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base != 0);
  AVER((limit - base) <= INT_MAX);	/* should be redundant */
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));

  flags = PROT_READ | PROT_WRITE | PROT_EXEC;
  if((mode & ProtREAD) != 0)
    flags &= ~PROT_READ;
  if((mode & ProtWRITE) != 0)
    flags &= ~PROT_WRITE;

  if(mprotect((caddr_t)base, (int)(limit - base), flags) != 0)
    NOTREACHED;
}


/*  == Protection Trampoline ==
 *
 *  The protection trampoline is trivial under Solaris, as there is nothing
 *  that needs to be done in the dynamic context of the mutator in order
 *  to catch faults.  (Contrast this with Win32 Structured Exception
 *  Handling.)
 */

void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  AVER(f != NULL);
  AVER(resultReturn != NULL);

  *resultReturn = (*f)(p, s);
}

/*  == Protection Shield Entry ==
 */

void ProtShieldEnter(Space space)
{
  AVER(ISVALID(Space, space));

  /* Since we don't know how to give per-thread access */
  ThreadDequeSuspend(SpaceThreadDeque(space));
}

/*  == Protection Shield Exit ==
 */

void ProtShieldLeave(Space space)
{
  AVER(ISVALID(Space, space));

  /* Since we don't know how to give per-thread access */
  ThreadDequeResume(SpaceThreadDeque(space));
}

