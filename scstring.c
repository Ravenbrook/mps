/* scstring.c -- string operations
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* make-string k char
 *
 * "`Make-string' returns a newly allocated string of length k. If
 *  char is given, then all elements of the string are initialized to
 *  char, otherwise the contents of the string are unspecified." -- R5RS
 */

static void make_string_entry(state_t state)
{
  unless(1 <= ARGS && ARGS < 3)
    error(state, "takes one or two arguments, not %lu", (ulong)ARGS);
  unless(TYPE(A0) == TYPE_INTEGER)
    error(state, "first argument must be an integer");
  unless(0 <= INT(A0))
    error(state, "string length can't be negative");
  unless(ARGS == 1 || TYPE(A1) == TYPE_CHARACTER)
    error(state, "second argument must be a character");

  MAKE_STRING_UNINIT(T0, INT(A0));
  if(ARGS == 2)
    memset(T0->string.string, A1->character.c, INT(A0));
  else
    memset(T0->string.string, '?', INT(A0));
  T0->string.string[INT(A0)] = '\0';

  RET1(T0);
}


/* string-set! string k char
 *
 * "k must be a valid index of string . `String-set!' stores char in
 *  element k of string and returns an unspecified value." -- R5RS
 */

static void string_set_entry(state_t state)
{
  strlen_t i;
  check_args(state, 3, TYPE_STRING, TYPE_INTEGER, TYPE_CHARACTER);
  i = INT(A1);
  T0 = A0;
  unless(0 <= i && i <= STRLEN(T0))
    error(state, "reference to %ld is out of bounds of string of length %lu",
	  i, (ulong)STRLEN(T0));
  STR(T0)[i] = A2->character.c;
  RET0();
}


/* string-ref string k
 *
 * "k must be a valid index of string. `String-ref' returns character
 *  k of string using zero-origin indexing." -- R5RS
 */

static void string_ref_entry(state_t state)
{
  strlen_t i;
  check_args(state, 2, TYPE_STRING, TYPE_INTEGER);
  i = INT(A1);
  T0 = A0;
  unless(0 <= i && i <= STRLEN(T0))
    error(state, "reference to %ld is out of bounds of string of length %lu",
	  i, (ulong)STRLEN(T0));
  MAKE_CHARACTER(T1, STR(T0)[i]);
  RET1(T1);
}


/* string-length string
 *
 * "Returns the number of characters in the given string." -- R5RS
 */

static void string_length_entry(state_t state)
{
  check_args(state, 1, TYPE_STRING);
  MAKE_INTEGER(T0, STRLEN(A0));
  RET1(T0);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"make-string",		make_string_entry},
  {"string-length",		string_length_entry},
  {"string-ref",		string_ref_entry},
  {"string-set!",		string_set_entry},
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


const label_t sc_unit_scstring = &unit_entry;
