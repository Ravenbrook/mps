/* scchar.c -- character operations
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"

#include <ctype.h>
#include <limits.h>


/* char->integer char
 * integer->char n
 *
 * "Given a character, `char->integer' returns an exact integer
 *  representation of the character. Given an exact integer that is
 *  the image of a character under `char->integer', `integer->char'
 *  returns that character. These procedures implement
 *  order-preserving isomorphisms between the set of characters under
 *  the char<=? ordering and some subset of the integers under the
 *  `<=' ordering." -- R5RS
 */

static void char_to_integer_entry(state_t state)
{
  check_args(state, 1, TYPE_CHARACTER);
  MAKE_INTEGER(T0, A0->character.c);
  RET1(T0);
}

static void integer_to_char_entry(state_t state)
{
  check_args(state, 1, TYPE_INTEGER);
  unless(CHAR_MIN <= INT(A0) && INT(A0) <= CHAR_MAX)
    error(state, "integer %ld is out of legal range of characters [%d, %d]",
	  (ulong)INT(A0), CHAR_MIN, CHAR_MAX);
  MAKE_CHARACTER(T0, INT(A0));
  RET1(T0);
}


/* char-alphabetic? char
 * char-numeric? char
 * char-whitespace? char
 * char-upper-case? letter
 * char-lower-case? letter
 *
 * "These procedures return #t if their arguments are alphabetic,
 *  numeric, whitespace, upper case, or lower case characters,
 *  respectively, otherwise they return #f. The following remarks,
 *  which are specific to the ASCII character set, are intended only
 *  as a guide: The alphabetic characters are the 52 upper and lower
 *  case letters. The numeric characters are the ten decimal digits.
 *  The whitespace characters are space, tab, line feed, form feed,
 *  and carriage return." -- R5RS
 *
 * We just use the C library's definitions.  These could be written in
 * Scheme, but it's nice to use the C ones so that we don't have to
 * worry about the locale's definitions of character classes.
 */

static void char_alpha_p_entry(state_t state)
{
  check_args(state, 1, TYPE_CHARACTER);
  T0 = isalpha(A0->character.c) ? obj_true : obj_false;
  RET1(T0);
}

static void char_numeric_p_entry(state_t state)
{
  check_args(state, 1, TYPE_CHARACTER);
  T0 = isdigit(A0->character.c) ? obj_true : obj_false;
  RET1(T0);
}

static void char_whitespace_p_entry(state_t state)
{
  check_args(state, 1, TYPE_CHARACTER);
  T0 = isspace(A0->character.c) ? obj_true : obj_false;
  RET1(T0);
}

static void char_upper_p_entry(state_t state)
{
  check_args(state, 1, TYPE_CHARACTER);
  T0 = isupper(A0->character.c) ? obj_true : obj_false;
  RET1(T0);
}

static void char_lower_p_entry(state_t state)
{
  check_args(state, 1, TYPE_CHARACTER);
  T0 = islower(A0->character.c) ? obj_true : obj_false;
  RET1(T0);
}


/* char-upcase char
 * char-downcase char
 *
 * "These procedures return a character char2 such that `(char-ci=?
 *  char char2)'. In addition, if char is alphabetic, then the result
 *  of `char-upcase' is upper case and the result of `char-downcase'
 *  is lower case." -- R5RS
 *
 * We just use C's definitions.
 */

static void char_upcase_entry(state_t state)
{
  check_args(state, 1, TYPE_CHARACTER);
  MAKE_CHARACTER(T0, toupper(A0->character.c));
  RET1(T0);
}

static void char_downcase_entry(state_t state)
{
  check_args(state, 1, TYPE_CHARACTER);
  MAKE_CHARACTER(T0, tolower(A0->character.c));
  RET1(T0);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"char->integer",		char_to_integer_entry},
  {"char-alphabetic?",		char_alpha_p_entry},
  {"char-downcase",		char_downcase_entry},
  {"char-lower-case?",		char_lower_p_entry},
  {"char-numeric?",		char_numeric_p_entry},
  {"char-upcase",		char_upcase_entry},
  {"char-upper-case?",		char_upper_p_entry},
  {"char-whitespace?",		char_whitespace_p_entry},
  {"integer->char",		integer_to_char_entry},
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


const label_t sc_unit_scchar = &unit_entry;
