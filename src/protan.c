/*  impl.c.protan
 */

#include "std.h"
#include "space.h"
#include "th.h"
#include "prot.h"
#include "fault.h"
#include "lock.h"
#include "lockst.h"
#include "vm.h"
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <signal.h>

#ifndef ANSI_VM
#error "protan.c is ANSI specific, but ANSI_VM is not set"
#endif

/*  == Protection Granularity ==
 *
 *  The granularity of protection is one page under SunOS.
 */

Addr ProtGrain(void)
{
  return(VMGrain());
}

static Bool fault_pending = FALSE;
static Bool in_progress = FALSE;


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

static void sigHandle(int sig, char *addr)
{
	Addr base, limit;
    ProtMode mode;
    
    AVER(sig == SIGSEGV);

	
	AVER(addr != NULL);  /* Assume protection address was decoded. */
	
	/* We can't determine the access mode (read, write, etc.) */
	/* under SunOS without decoding the faulting instruction. */
	/* Don't bother, yet.  We can do this if necessary. */
	
	mode = ProtREAD | ProtWRITE;
	
	/* We assume that the access is for one word at the address. */
	/* @@@@ What happens to ldd across a page boundary? */
	/* ldd has to be dword aligned */
	
	base = (Addr)addr;
	limit = (Addr)addr + sizeof(Addr);
	
	/* Offer each protection structure the opportunity to handle the */
	/* exception.  If it succeeds, then allow the mutator to continue. */
	
	if(FaultDispatch(base, limit, mode))
	  return;

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler. */

  NOTREACHED;
}



/*  == Global Protection Setup ==
 *
 *  Under SunOS, the global setup involves installing a signal handler
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

  AVER(base < limit);
  AVER(base != 0);
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));

  /* Just handle it now please! */
  if (mode != ProtNONE)
    fault_pending |= TRUE;

}


/*  == Protection Trampoline ==
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

static void mapFn(Env env, Addr base, Addr size)
{
  /* @@@ Handle multi-page segments */
  if (ArenaProtMode((Arena) env, base) != ProtNONE)
    sigHandle(SIGSEGV, (char *)base);
}
   

/*  == Protection Shield Exit ==
 */

void ProtShieldLeave(Space space)
{
  AVER(ISVALID(Space, space));

  /* So, hack me:  touch all protected pages when you leave, but don't recurse */
  while (fault_pending != FALSE  && in_progress == FALSE) {
     Arena arena = SpaceArena(space);
     
     fault_pending = FALSE;
     in_progress = TRUE;
     /* @@@ brute force */
     ArenaMapSegs(arena, mapFn, (Env)arena);
     in_progress = FALSE;
  }
  /* Since we don't know how to give per-thread access */
  ThreadDequeResume(SpaceThreadDeque(space));
}

