/* scport.c -- port operations
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* string->symbol string 
 *
 * "Returns the symbol whose name is string. This procedure can create
 *  symbols with names containing special characters or letters in the
 *  non-standard case, but it is usually a bad idea to create such
 *  symbols because in some implementations of Scheme they cannot be
 *  read as themselves. See `symbol->string'." -- R5RS
 *
 * @@@@ Ideally the symbol table would be a vector of weak pointers,
 * so that unused symbols can be GC'd away.
 *
 * @@@@ "intern" is called by compiled code -- see opcodes.h
 */

static void string_to_symbol_entry(state_t state)
{
  check_args(state, 1, TYPE_STRING);
  T0 = A0;
  intern(state);
  RET1(T0);
}


/* symbol->string symbol
 *
 * "Returns the name of symbol as a string. If the symbol was part of
 *  an object returned as the value of a literal expression (section
 *  see section Literal expressions) or by a call to the `read'
 *  procedure, and its name contains alphabetic characters, then the
 *  string returned will contain characters in the implementation's
 *  preferred standard case--some implementations will prefer upper
 *  case, others lower case. If the symbol was returned by
 *  `string->symbol', the case of characters in the string returned
 *  will be the same as the case in the string that was passed to
 *  `string->symbol'. It is an error to apply mutation procedures like
 *  string-set! to strings returned by this procedure." -- R5RS
 */

static void symbol_to_string_entry(state_t state)
{
  check_args(state, 1, TYPE_SYMBOL);
  MAKE_STRING(T0, SYMLEN(A0), SYMSTR(A0));
  RET1(T0);
}


/* the-symbol-table
 *
 * Returns the symbol table object.
 */

static void the_symtab_entry(state_t state)
{
  check_args(state, 0);
  RET1(state->obj_symtab);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"string->symbol",		string_to_symbol_entry},
  {"symbol->string",		symbol_to_string_entry},
  {"the-symbol-table",		the_symtab_entry},
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


const label_t sc_unit_scsymbol = &unit_entry;
