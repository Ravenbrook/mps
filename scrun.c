/* scrun.c -- run the abstract machine
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"

#include <stdlib.h>


/* no_entry -- unreachable entry point for dummy functions */

extern void no_entry(state_t state)
{
  UNUSED(state);
  ASSERT(0);			/* not reached */
  abort();
}


/* run -- run the abstract machine
 *
 * Runs the procedure in T0 in the abstract machine.  If the procedure
 * returns normally, run() returns zero, and the results are in ARGL.
 * If there's an uncaught error, run() returns one, and the error
 * object is in A0.
 */

extern int run(state_t);
static void run_return(state_t);
static void run_error(state_t);

extern int run(state_t state)
{
  int code;

  /* Hand-build procedure objects to call the procedure in T0 and */
  /* an error continuation which causes the run loop to exit. */

  /* Note that the continuations of these top level frames are */
  /* set to themselves.	 This fact is used by "backtrace" to detect */
  /* them, and also means that RET can always set the "read_only" flag */
  /* of the caller's caller.  In effect, these frames are like */
  /* stack sentinels. */

  MAKE_PROC(state->errproc, sym_fatal, run_error);
  state->errproc->proc.cont = state->errproc;

  MAKE_PROC(state->here, sym_run, no_entry);
  state->here->proc.cont = state->here;

  /* Invoke the procedure in T0. */
  CALL_TRANS(T0, run_return);

  /* Run the abstract machine.	When the T0 procedure returns, */
  /* or there's an uncaught error, control will emerge from longjmp. */
  code = setjmp(state->driver_loop);
  if(code == 0 || code == LJ_ERROR)
    repeat {
      ASSERT(state->here != NULL && TYPE(state->here) == TYPE_PROC);
      ASSERT(state->here->proc.entry != NULL);
      ASSERT(!state->here->proc.read_only);

      if(state->trace) {
	if(state->here->proc.entry == eval_entry) {
	  write_string(state, stdout, "eval ");
	  write_obj(state, A0, WRITE_DEPTH, stdout);
	  write_char(state, stdout, '\n');
	} else if(state->here->proc.entry == define_entry) {
	  write_string(state, stdout, "define ");
	  write_obj(state, CAR(A1), WRITE_DEPTH, stdout);
	  write_char(state, stdout, '\n');
	}
      }

      if(state->heap_checking)
	heap_check(state);

      (*state->here->proc.entry)(state);

      ++state->step;
    }

  if(code == LJ_RETURN)
    return 0;
  else if(code == LJ_FATAL)
    return 1;

  ASSERT(0);			/* illegal LJ code */
  abort();
}

static void run_return(state_t) __attribute__((noreturn));

static void run_return(state_t state)
{
  longjmp(state->driver_loop, LJ_RETURN);
}

static void run_error(state_t) __attribute__((noreturn));

static void run_error(state_t state)
{
  longjmp(state->driver_loop, LJ_FATAL);
}
