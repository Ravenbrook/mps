/* scexception.c -- exception handling extensions
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* make_exception_entry -- make an exception object
 *
 * (make-exception <object>)
 */

static void make_exception_entry(state_t state)
{
  check_args(state, 1, 0);
  MAKE_EXCEPTION(T0, A0);
  RET1(T0);
}


/* throw exception
 * throw exception continuation (for re-throwing)
 */

static void throw_entry(state_t state)
{
  if(ARGS == 1) {
    check_args(state, 1, TYPE_EXCEPTION);
    T1 = state->cont;
  } else {
    check_args(state, 2, TYPE_EXCEPTION, TYPE_PROC);
    T1 = A1;
  }
  T0 = A0;
  NEW_ARGS();
  PUSH_ARG(T1);
  PUSH_ARG(T0);
  state->cont = state->errproc;
  RET();
}


/* catch
 *
 * (catch (lambda (exception cont) <recovery>)
 *        (lambda () <exp>))
 *
 * L0	environment
 * L1	operands: (<handler> <exp>)
 * L2	previous error handler
 * L3	current error handler
 */

static void catch_entry(state_t state);
static void catch_uninstall(state_t state);
static void catch_error(state_t state);

static void catch_entry(state_t state)
{
  check_args(state, 2, TYPE_PROC, TYPE_PROC);

  ENTER(3);  
  L0 = A0;                      /* handler */
  L1 = A1;                      /* body thunk */

  /* Install the error handler */
  L2 = state->errproc;		/* previous error handler */
  DUP_PROC(state->errproc, state->here);
  state->errproc->proc.entry = catch_error;
  state->errproc->proc.read_only = 1; /* @@@@ why? */

  CALL0(L1, catch_uninstall);
}

static void catch_uninstall(state_t state)
{
  state->errproc = L2;
  LEAVE();
  RET();			/* leave results of thunk intact */
}

static void catch_error(state_t state)
{
  ASSERT(ARGS == 2);
  ASSERT(TYPE(A0) == TYPE_EXCEPTION);
  ASSERT(TYPE(A1) == TYPE_PROC);

  /* Uninstall the error handler while it's running. */
  state->errproc = L2;

  T0 = A0;
  T1 = A1;
  LEAVE();
  TAIL2(L0, T0, T1);            /* to the handler */
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"make-exception",		make_exception_entry},
  {"throw",			throw_entry},
  {"catch",		        catch_entry},
  {NULL,                        NULL}
};


/* syntab -- syntax table
 *
 * This table used by "unit_entry" to create the initial syntax environment.
 */

static const proctab_s syntab[] = {
  {NULL, NULL}
};


static void unit_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR); /* the environment */
  bind_procs(state, proctab, syntab);
  RET0();
}


const label_t sc_unit_scexception = &unit_entry;
