/* scproc.c -- procedure operations
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* apply1 proc list
 *
 * Applies procedure to list as an argument list.  This is not a full
 * implementation of R5RS apply, but that can be built on top.
 *
 * Note that calling "length" should also check the well-formedness
 * of the argument list.
 */

static void apply1_entry(state_t);
static void apply1_apply(state_t);

static void apply1_entry(state_t state)
{
  check_args(state, 2, TYPE_PROC, 0);
  ENTER(2);
  L0 = A0;			/* the procedure to apply */
  L1 = A1;			/* the argument list for it */
  CALL1(proc_length, L1, apply1_apply);
}

static void apply1_apply(state_t state)
{
  check_results(state, 1, TYPE_INTEGER);/* expect one result from length */
  ARGS = INT(A0);
  ARGL = L1;
  LEAVE();
  TAIL(L0);
}


/* call/cc -- call procedure with current continuation
 *
 * (call/cc <procedure>)
 */

static void call_cc_entry(state_t);
static void call_cc_cont(state_t);

static void call_cc_entry(state_t state)
{
  check_args(state, 1, TYPE_PROC);

  /* Set the read only flag on our caller.  This also makes all the */
  /* "stack frames" above here (i.e. the continuation) read-only */
  /* (see RET_TRANS) so that the contination will work.	 Also, */
  /* the read only flag is a bit like a reference count, and */
  /* we've just created a new reference. */
  state->cont->proc.read_only = 1;

  /* Build a procedure which, when called, discards it's return */
  /* address and instead returns to L0, as if it were coming */
  /* back from the original call to call/cc. */
  MAKE_PROC_REGS(T1, sym_continuation, call_cc_cont, 1);
  LOC(T1, 0) = state->cont;

  /* We can't just pass state->cont to the argument function, because */
  /* it's RET which duplicates read-only stack frames, not CALL or TAIL. */
  T0 = A0;
  TAIL1(T0, T1);
}

static void call_cc_cont(state_t state)
{
  state->cont = L0;
  RET();			/* pass all values untouched */
}


/* values obj ...
 *
 * "Delivers all of its arguments to its continuation. Except for
 *  continuations created by the call-with-values procedure, all
 *  continuations take exactly one value." -- R5RS
 */

static void values_entry(state_t state)
{
  RET();			/* return arguments unchanged */
}


/* call-with-values producer consumer
 *
 * "Calls its producer argument with no values and a continuation
 *  that, when passed some values, calls the consumer procedure with
 *  those values as arguments. The continuation for the call to
 *  consumer is the continuation of the call to call-with-values." -- R5RS
 *
 * This is a monsterous way to do this, but R5RS has it, so I'll include
 * it.
 *
 * @@@@ Could be cleverer here.	 We could build a frames for the producer
 * and consumer.  The producer would continue to the consumer, and the
 * consumer would continue to our continuation.	 Then we just transfer
 * to the producer.
 */

static void call_with_values_entry(state_t);
static void call_with_values_apply(state_t);

static void call_with_values_entry(state_t state)
{
  check_args(state, 2, TYPE_PROC, TYPE_PROC);
  ENTER(1);
  L0 = A1;
  T0 = A0;
  CALL0(T0, call_with_values_apply);
}

static void call_with_values_apply(state_t state)
{
  LEAVE();
  TAIL(L0);			/* leave the arguments intact */
}


/* procedure-name procedure */

static void proc_name_entry(state_t state)
{
  check_args(state, 1, TYPE_PROC);
  T0 = A0->proc.name;
  RET1(T0);
}

/* set-procedure-name procedure symbol
 *
 * Sets the internal procedure name of the procedure to the symbol.
 * The internal name is displayed when the procedure is written,
 * and by debugging routines such as backtrace.	 It has no semantics
 * in Scheme.  This procedure can be used to give names to procedures
 * created by lambda.
 */

static void set_proc_name_entry(state_t state)
{
  check_args(state, 2, TYPE_PROC, TYPE_SYMBOL);
  A0->proc.name = A1;
  RET0();
}

/* procedure-continuation procedure */

static void proc_cont_entry(state_t state)
{
  check_args(state, 1, TYPE_PROC);
  T0 = A0->proc.cont;
  RET1(T0);
}

/* procedure-local procedure index */

static void proc_loc_entry(state_t state)
{
  check_args(state, 2, TYPE_PROC, TYPE_INTEGER);
  unless(0 <= INT(A1) && (size_t)INT(A1) < A0->proc.regs)
    error(state, "reference to local %ld is out of range of locals [0, %lu)",
	  (ulong)INT(A1), (ulong)(A0->proc.regs));
  T0 = LOC(A0, INT(A1));
  RET1(T0);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"apply1",			apply1_entry},
  {"call/cc",			call_cc_entry},
  {"call-with-values",		call_with_values_entry},
  {"values",			values_entry},
  {"procedure-continuation",	proc_cont_entry},
  {"procedure-local",		proc_loc_entry},
  {"procedure-name",		proc_name_entry},
  {"set-procedure-name!",	set_proc_name_entry},
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


const label_t sc_unit_scproc = &unit_entry;
