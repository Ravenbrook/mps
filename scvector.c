/* scvector.c -- vector operations
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* make-vector k 
 * make-vector k fill 
 *
 * "Returns a newly allocated vector of k elements. If a second
 *  argument is given, then each element is initialized to fill.
 *  Otherwise the initial contents of each element is unspecified." -- R5RS
 */

static void make_vector_entry(state_t state)
{
  unless(1 <= ARGS && ARGS < 3)
    error(state, "takes one or two arguments, not %lu", (ulong)ARGS);
  unless(TYPE(A0) == TYPE_INTEGER)
    error(state, "first argument must be an integer");
  unless(0 <= INT(A0))
    error(state, "vector length can't be negative");
  T0 = ARGS == 2 ? A1 : obj_unspec;
  MAKE_VECTOR(T1, INT(A0), T0);
  RET1(T1);
}


/* vector-length vector
 *
 * "Returns the number of elements in vector as an exact integer." -- R5RS
 */

static void vector_length_entry(state_t state)
{
  check_args(state, 1, TYPE_VECTOR);
  MAKE_INTEGER(T0, VECLEN(A0));
  RET1(T0);
}


/* vector-ref vector k 
 *
 * "k must be a valid index of vector. `Vector-ref' returns the
 *  contents of element k of vector." -- R5RS
 */

static void vector_ref_entry(state_t state)
{
  veclen_t i;
  check_args(state, 2, TYPE_VECTOR, TYPE_INTEGER);
  i = INT(A1);
  T0 = A0;
  unless(0 <= i && i < VECLEN(T0))
    error(state, "reference to %ld is out of bounds of vector of length %lu", i, (ulong)VECLEN(T0));
  RET1(VECREF(T0, i));
}


/* vector-set! vector k obj
 *
 * "k must be a valid index of vector. `Vector-set!' stores obj in
 *  element k of vector. The value returned by `vector-set!' is
 *  unspecified." -- R5RS
 */

static void vector_set_entry(state_t state)
{
  veclen_t i;
  check_args(state, 3, TYPE_VECTOR, TYPE_INTEGER, 0);
  i = INT(A1);
  T0 = A0;
  unless(0 <= i && i < VECLEN(T0))
    error(state, "reference to %ld is out of bounds of vector of length %lu", i, (ulong)VECLEN(T0));
  VECSET(T0, i, A2);
  RET0();
}


/* list->vector list 
 *
 * "`List->vector' returns a newly created vector initialized to
 *  the elements of the list list." -- R5RS
 *
 * This function is only built in because is used by "read".
 *
 * L0	the list to be converted
 */

static void list_to_vector_entry(state_t);
static void list_to_vector_fill(state_t);

static void list_to_vector_entry(state_t state)
{
  check_args(state, 1, 0);

  ENTER(1);
  L0 = A0;

  /* Calculate the length of the list.	This also ensures it's a */
  /* valid list and not circular. */
  CALL1(proc_length, L0, list_to_vector_fill);
}

static void list_to_vector_fill(state_t state)
{
  veclen_t i;

  check_args(state, 1, TYPE_INTEGER);/* expect one argument from length */
  T0 = A0;

  MAKE_VECTOR(T1, INT(T0), obj_uninit);

  /* Fill in the vector with the list elements */
  for(i = 0; i < INT(T0); ++i) {
    VECSET(T1, i, CAR(L0));
    L0 = CDR(L0);
  }

  LEAVE();
  RET1(T1);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"make-vector",		make_vector_entry},
  {"list->vector",		list_to_vector_entry},
  {"vector-length",		vector_length_entry},
  {"vector-ref",		vector_ref_entry},
  {"vector-set!",		vector_set_entry},
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

const label_t sc_unit_scvector = &unit_entry;
