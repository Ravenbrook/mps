/* scenv.c -- environment operations
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* the_env -- return the current environment
 *
 * (the-environment)
 */

static void the_env_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  unless(A1 == obj_empty)
    error(state, "takes no arguments");
  T0 = A0;
  RET1(T0);
}


/* bind_entry -- bind a symbol in the top frame of the environment
 *
 * (bind <environment> <symbol> <value>)
 */

static void bind_entry(state_t state)
{
  check_args(state, 3, TYPE_PAIR, TYPE_SYMBOL, 0);
  unless(TYPE(A0) == TYPE_PAIR &&
	 (CAR(A0) == obj_empty ||
	  TYPE(CAR(A0)) == TYPE_PAIR))
    error(state, "first argument must be an environment");
  BIND(A0, A1, A2, T0);
  RET0();
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"bind",			bind_entry},
  {NULL,                        NULL}
};


/* syntab -- syntax table
 *
 * This table used by "unit_entry" to create the initial syntax environment.
 */

static const proctab_s syntab[] = {
  {"the-environment",	the_env_entry},
  {NULL, NULL}
};


static void unit_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR); /* the environment */
  bind_procs(state, proctab, syntab);
  RET0();
}


const label_t sc_unit_scenv = &unit_entry;
