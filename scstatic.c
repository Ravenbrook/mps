/* scport.c -- static data
 *
 * Richard Brooksby, 2004-08-04
 *
 * I used to create these objects on the heap during initialization, which made
 * some things simple, because then _everything_ was on the heap.  But that makes
 * it hard for an embedded multi-threaded interpreter to find the objects, because
 * it can't share them between machines, and must therefore keep pointers to them
 * in the machine state.  That's cumbersome to set up, so now they're allocated
 * statically.
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* const */ struct special_s static_specials[] = {
  {{TYPE_SPECIAL}, "()"},		/* the empty list */
  {{TYPE_SPECIAL}, "#[end of file]"},	/* end of file */
  {{TYPE_SPECIAL}, "#t"},		/* boolean true */
  {{TYPE_SPECIAL}, "#f"},		/* boolean false */
  {{TYPE_SPECIAL}, "#[unspecified]"},	/* unspecified result indicator */
  {{TYPE_SPECIAL}, "#[undefined]"},	/* undefined value indicator */
  {{TYPE_SPECIAL}, "#[uninitialized]"}, /* uninitialized value indicator */
  {{TYPE_SPECIAL}, "#[unbound]"},	/* unbound variable indicator */
};

const size_t static_specials_length = ARRAYLEN(static_specials);


/* const */ struct symbol_s static_symbols[] = {
  {{TYPE_SYMBOL},  9, "anonymous"},	/* the procedure name of anonymous functions */
  {{TYPE_SYMBOL},  2, "=>"},
  {{TYPE_SYMBOL},  5, "begin"},
  {{TYPE_SYMBOL}, 12, "continuation"},
  {{TYPE_SYMBOL},  6, "define"},
  {{TYPE_SYMBOL},  4, "else"},
  {{TYPE_SYMBOL},  6, "lambda"},
  {{TYPE_SYMBOL}, 10, "quasiquote"},
  {{TYPE_SYMBOL},  5, "quote"},
  {{TYPE_SYMBOL},  7, "unquote"},
  {{TYPE_SYMBOL}, 16, "unquote-splicing"},
  {{TYPE_SYMBOL},  5, "(run)"},		/* procedure name used in run() */
  {{TYPE_SYMBOL},  7, "(fatal)"},	/* ditto */
  {{TYPE_SYMBOL},  7, "(start)"},	/* ditto */
  {{TYPE_SYMBOL},  4, "eval"},
  {{TYPE_SYMBOL},  1, "+"},		/* These three are "peculiar identifiers" */
  {{TYPE_SYMBOL},  1, "-"},		/* (R5RS 7.1.1) and must be read specially. */
  {{TYPE_SYMBOL},  3, "..."},		/* See "read". */
};

const size_t static_symbols_length = ARRAYLEN(static_symbols);