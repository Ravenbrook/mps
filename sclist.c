/* sclist.c -- list operations
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* cons obj1 obj2
 *
 * "Returns a newly allocated pair whose car is obj1 and whose cdr is
 *  obj2. The pair is guaranteed to be different (in the sense of
 *  `eqv?') from every existing object." -- R5RS
 */

static void cons_entry(state_t state)
{
  check_args(state, 2, 0, 0);
  MAKE_PAIR(T0, A0, A1);
  RET1(T0);
}


/* car pair
 *
 * "Returns the contents of the car field of pair. Note that it is an
 *  error to take the car of the empty list." -- R5RS
 */

static void car_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR);
  T0 = CAR(A0);
  RET1(T0);
}


/* cdr pair
 *
 * "Returns the contents of the cdr field of pair. Note that it is an
 *  error to take the cdr of the empty list." -- R5RS
 */

static void cdr_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR);
  T0 = CDR(A0);
  RET1(T0);
}


/* set-car! pair obj
 *
 * "Stores obj in the car field of pair. The value returned by
 *  `set-car!' is unspecified." -- R5RS
 */

static void setcar_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  SETCAR(A0, A1);
  RET0();
}


/* set-cdr! pair obj
 *
 * "Stores obj in the cdr field of pair. The value returned by
 *  `set-cdr!' is unspecified." -- R5RS
 */

static void setcdr_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  SETCDR(A0, A1);
  RET0();
}


/* length list 
 *
 * "Returns the length of list." -- R5RS
 *
 * This implementation also detects circular lists, and is used for safety
 * within the interpreter for this purpose.
 */

static void length_entry(state_t state)
{
  check_args(state, 1, 0);
  T0 = A0;			/* hare */
  T1 = A0;			/* tortoise */
  MAKE_INTEGER(T2, 0);		/* length */
  do {
    if(TYPE(T0) != TYPE_PAIR)
      break;
    T0 = CDR(T0);
    SETINT(T2, INT(T2) + 1);
    if(TYPE(T0) != TYPE_PAIR)
      break;
    T0 = CDR(T0);
    SETINT(T2, INT(T2) + 1);
    T1 = CDR(T1);
  } while(T0 != T1);
  unless(T0 == obj_empty)
    error(state, "applied to non-list");
  RET1(T2);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"car",			car_entry},
  {"cdr",			cdr_entry},
  {"cons",			cons_entry},
  {"length",			length_entry},
  {"set-car!",			setcar_entry},
  {"set-cdr!",			setcdr_entry},
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


const label_t sc_unit_sclist = &unit_entry;
