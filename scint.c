/* scint.c -- integer operations
 *
 * Richard Brooksby, 2004-08-03
 *
 * $Id$
 */

#include "sc.h"


/* = z1 z2 z3 ...
 *
 * "These procedures return #t if their arguments are (respectively):
 *  equal, monotonically increasing, monotonically decreasing,
 *  monotonically nondecreasing, or monotonically nonincreasing." -- R5RS
 */

static void number_equalp_entry(state_t state)
{
  unless(ARGS >= 1)
    error(state, "requires at least one argument");
  T0 = CAR(ARGL);
  unless(TYPE(T0) == TYPE_INTEGER)
    error(state, "all arguments must be integers");
  T1 = CDR(ARGL);
  while(TYPE(T1) == TYPE_PAIR) {
    T2 = CAR(T1);
    unless(TYPE(T2) == TYPE_INTEGER)
      error(state, "all arguments must be integers");
    unless(INT(T0) == INT(T2))
      RET1(obj_false);
    T1 = CDR(T1);
  }
  unless(T1 == obj_empty)
    error(state, "applied to malformed argument list");
  RET1(obj_true);
}

static void lessp_entry(state_t state)
{
  unless(ARGS >= 1)
    error(state, "requires at least one argument");
  T0 = CAR(ARGL);
  unless(TYPE(T0) == TYPE_INTEGER)
    error(state, "all arguments must be integers");
  T1 = CDR(ARGL);
  while(TYPE(T1) == TYPE_PAIR) {
    T2 = CAR(T1);
    unless(TYPE(T2) == TYPE_INTEGER)
      error(state, "all arguments must be integers");
    unless(INT(T0) < INT(T2))
      RET1(obj_false);
    T0 = T2;
    T1 = CDR(T1);
  }
  unless(T1 == obj_empty)
    error(state, "applied to malformed argument list");
  RET1(obj_true);
}

static void greaterp_entry(state_t state)
{
  unless(ARGS >= 1)
    error(state, "requires at least one argument");
  T0 = CAR(ARGL);
  unless(TYPE(T0) == TYPE_INTEGER)
    error(state, "all arguments must be integers");
  T1 = CDR(ARGL);
  while(TYPE(T1) == TYPE_PAIR) {
    T2 = CAR(T1);
    unless(TYPE(T2) == TYPE_INTEGER)
      error(state, "all arguments must be integers");
    unless(INT(T0) > INT(T2))
      RET1(obj_false);
    T0 = T2;
    T1 = CDR(T1);
  }
  unless(T1 == obj_empty)
    error(state, "applied to malformed argument list");
  RET1(obj_true);
}


/* + z1 ... 
 * * z1 ... 
 *
 * "These procedures return the sum or product of their arguments." -- R5RS 6.2.5
 *
 * @@@@ Might be better to provide binary operators as primitives and generalize them
 * in Scheme.
 */

static void add_entry(state_t state)
{
  int_t sum = 0;
  while(TYPE(ARGL) == TYPE_PAIR) {
    unless(TYPE(A0) == TYPE_INTEGER)
      error(state, "arguments must be integers");
    sum += INT(A0);
    ARGL = CDR(ARGL);
  }
  ASSERT(ARGL == obj_empty);
  MAKE_INTEGER(T0, sum);
  RET1(T0);
}

static void multiply_entry(state_t state)
{
  int_t product = 1;
  while(TYPE(ARGL) == TYPE_PAIR) {
    unless(TYPE(A0) == TYPE_INTEGER)
      error(state, "arguments must be integers");
    product *= INT(A0);
    ARGL = CDR(ARGL);
  }
  ASSERT(ARGL == obj_empty);
  MAKE_INTEGER(T0, product);
  RET1(T0);
}


/* - z1 z2 
 * - z 
 * - z1 z2 ... 
 * / z1 z2 
 * / z 
 * / z1 z2 ... 
 *
 * "With two or more arguments, these procedures return the difference
 *  or quotient of their arguments, associating to the left. With one
 *  argument, however, they return the additive or multiplicative
 *  inverse of their argument." -- R5RS 6.2.5
 */

static void subtract_entry(state_t state)
{
  int_t diff;
  unless(ARGS >= 1)
    error(state, "takes at least one argument");
  unless(TYPE(A0) == TYPE_INTEGER)
    error(state, "arguments must be integers");
  if(ARGS == 1)
    diff = -INT(A0);
  else {
    diff = INT(A0);
    ARGL = CDR(ARGL);
    while(TYPE(ARGL) == TYPE_PAIR) {
      unless(TYPE(A0) == TYPE_INTEGER)
	error(state, "arguments must be integers");
      diff -= INT(A0);
      ARGL = CDR(ARGL);
    }
    ASSERT(ARGL == obj_empty);
  }
  MAKE_INTEGER(T0, diff);
  RET1(T0);
}

static void divide_entry(state_t state)
{
  int_t quot;
  unless(ARGS >= 1)
    error(state, "takes at least one argument");
  unless(TYPE(A0) == TYPE_INTEGER)
    error(state, "arguments must be integers");
  if(ARGS == 1)
    quot = 1/INT(A0);		/* @@@@ meaningless of ints */
  else {
    quot = INT(A0);
    ARGL = CDR(ARGL);
    while(TYPE(ARGL) == TYPE_PAIR) {
      unless(TYPE(A0) == TYPE_INTEGER)
	error(state, "arguments must be integers");
      quot /= INT(A0);
      ARGL = CDR(ARGL);
    }
    ASSERT(ARGL == obj_empty);
  }
  MAKE_INTEGER(T0, quot);
  RET1(T0);
}


/* proctab -- procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "start".
 */

static const proctab_s proctab[] = {
  {"*",  multiply_entry},
  {"+",  add_entry},
  {"-",  subtract_entry},
  {"/",  divide_entry},
  {"<",  lessp_entry},
  {"=",  number_equalp_entry},
  {">",  greaterp_entry},
  {NULL, NULL},
};


/* syntab -- syntax table
 *
 * This table used by "start" to create the initial syntax environment.
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


const label_t sc_unit_scint = &unit_entry;
