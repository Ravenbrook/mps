/* scload.c -- "load" operator
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* load -- load scheme source code from a file
 *
 * optional procedure: load filename
 *
 * "Filename should be a string naming an existing file containing
 *  Scheme source code. The `load' procedure reads expressions and
 *  definitions from the file and evaluates them sequentially. It is
 *  unspecified whether the results of the expressions are printed. The
 *  `load' procedure does not affect the values returned by
 *  `current-input-port' and `current-output-port'. `Load' returns an
 *  unspecified value." -- R5RS
 *
 * Unlike R5RS, load is a syntactic operator, not a procedure, because
 * it is essentially a declaration: it can bind symbol in the environment.
 *
 * L0	environment
 * L1	inport port to from which to load
 */

static void load_entry(state_t);
static void load_string(state_t);
static void load_symbol(state_t);
static void load_file(state_t);
static void load_while(state_t);
static void load_loop(state_t);

static void load_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);

  unless(TYPE(A1) == TYPE_PAIR && CDR(A1) == obj_empty)
    error(state, "takes one argument");

  ENTER(2);

  switch(TYPE(CAR(A1))) {
  case TYPE_STRING:
    JUMP(load_string);
  case TYPE_SYMBOL:
    JUMP(load_symbol);
  default:
    error(state, "takes a string or symbol, not a %s", type_name(TYPE(CAR(A1))));
  }
}

static void load_string(state_t state)
{
  L0 = A0;			/* environment */
  T0 = CAR(A1);			/* file name */
  CALL1(proc_open_in, T0, load_file);
}

static void load_file(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from "open-input-file" */
  L1 = A0;			/* the input port */
  JUMP(load_while);
}

static void load_while(state_t state)
{
  CALL1(proc_read, L1, load_loop);
}

static void load_loop(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from "read" */
  T0 = A0;
  if(T0 == obj_eof) {		/* end of file? */
    LEAVE();
    TAIL1(proc_close_in, L1);
  }
  CALL2(proc_eval, T0, L0, load_while);
}

static void load_symbol(state_t state)
{
  T0 = lookup(state, state->units, CAR(A1));
  if(T0 == obj_undef)
    error(state, "couldn't find unit \"%s\"", CAR(A1)->symbol.string);
  T1 = CDR(T0);                 /* unit top-level procedure */
  T2 = A0;                      /* the environment */
  LEAVE();
  TAIL1(T1, T2);
}


/* the-unit-environment
 *
 * Returns the unit environment object from the state.
 */

static void units_entry(state_t state)
{
  check_args(state, 0);
  RET1(state->units);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"the-unit-environment",      units_entry},
  {NULL, NULL}
};


/* syntab -- syntax table
 *
 * This table used by "unit_entry" to create the initial syntax environment.
 */

static const proctab_s syntab[] = {
  {"load",		load_entry},
  {NULL, NULL}
};


static void unit_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR); /* the environment */
  bind_procs(state, proctab, syntab);
  RET0();
}


const label_t sc_unit_scload = &unit_entry;
