/* sc.c -- scheme-like language abstract machine and interpreter
 *
 * Richard Brooksby, 1996-08-30
 *
 * $Id$
 *
 *
 * INTRODUCTION
 *
 * This is an interpreter and compiled-code run-time system for SC,
 * a language almost conforming to "Revised(5) Report on the
 * Algorithmic Language Scheme" [R5RS].
 *
 * Differences from R5RS:
 *
 * - Internal definitions [R5RS 5.2.2] may occur anywhere, and may refer
 *   directly to the values of earlier definitions, or indirectly to later
 *   definitions.  This is a compatible extension of R5RS.
 *
 * - "Dynamic-wind" [R5RS 6.4] is not supported.
 *
 * - "Funny symbols", delimited by vertical bars, are supported.
 * 
 * - C escape characters [C99, 6.4.4.4] are allowed in strings, funny
 *   symbols, and characters.
 *
 * - "Load" is not a function, but an operator, like "define".
 *
 * CONFORMANCE
 *
 * This is a strictly conforming hosted ISO C program [C90].
 *
 * LICENCE
 *
 * Copyright (C) 1997-2004 Richard Brooksby.  This document is provided
 * "as is", without any express or implied warranty.  In no event will
 * the authors be held liable for any damages arising from the use of
 * this document.  You may not duplicate or reproduce this document in
 * any form without the express permission of the copyright holder.
 *
 * REFERENCES
 *
 * [R5RS]	"Revised(5) Report on the Algorithmic Language Scheme";
 *		Richard Kelsey, William Clinger, Jonathan Rees (Eds).
 *
 * [C90]	"American National Standard for Programming Languages -- C";
 *		ANSI/ISO 9899:1990.
 *
 * [C99]	"Internation Standard for Programming Languages -- C";
 *		ISO/IEC 9899:1999(E).
 *
 * NOTES
 *
 * [1] The length cannot exceed the size of an integer, otherwise it'll be
 * unreferenceable.
 *
 * [2] This is the maximum number of local registers used by any procedure
 * in the interpreter (actually, in "eval").
 */

#include "sc.h"
#include "mpsavm.h"
#include "mpscmv.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <errno.h>


/* Allocation re-entry check */

int allocating = 0;


/* ASSERTION HANDLER -- see sc.h */

#ifndef NDEBUG

extern void assert_fail(const char *condition, const char *file, ulong line)
{
  (void)fprintf(stderr,
	        "\n*** ASSERTION FAILURE\n\n"
	        "Condition: %s\n"
	        "File:      %s\n"
	        "Line:      %lu\n",
	        condition, file, line);
  abort();
}

#endif /* NDEBUG, not */


/* ERROR CHECKING */


/* type_name -- return a string describing a type */

extern const char *type_name(type_t type)
{
  ASSERT(!ISNTTYPE(type));
  switch(type) {
  case TYPE_PROC: return "procedure";
  case TYPE_SYMBOL: return "symbol";
  case TYPE_PAIR: return "pair";
  case TYPE_INTEGER: return "integer";
  case TYPE_SPECIAL: return "special";
  case TYPE_INPORT: return "input-port";
  case TYPE_OUTPORT: return "output-port";
  case TYPE_STRING: return "string";
  case TYPE_VECTOR: return "vector";
  case TYPE_EXCEPTION: return "exception";
  case TYPE_CHARACTER: return "character";
  case TYPE_FORWARD: return "forward";
  }
  ASSERT(0);			/* not reached */
  return "[unknown type]";
}


/* error -- throw an error condition
 *
 * The "error" procedure takes a printf-style format string
 * and arguments, writes the message into a string in an
 * exception, throws the exception, and longjmps to driver_loop,
 * which restarts the run loop, causing the error continuation
 * to be executed.  The current continuation is passed as an
 * argument to the error handler, for debugging.
 *
 * The error handler is called with the exception and the continuation
 * where the error happened as arguments.  That's "here".  The error
 * procedure's continuation is it's own.
 */

extern void error(state_t state, const char *format, ...)
{
  static int in = 0;
  char message[ERRMAX+1];
  va_list vargs;

  ASSERT(!in);
  in = 1;

  /* We can assume that the procedure symbol isn't funny, because */
  /* "error" is only called within the interpreter, and so we can */
  /* safely use "strcpy" on it. */
  va_start(vargs, format);
  strcpy(message, SYMSTR(state->here->proc.name));
  strcat(message, ": ");
  vsprintf(message + state->here->proc.name->symbol.length + 2,
	   format,
	   vargs);
  va_end(vargs);

  /* Make an exception containing the error message. */
  MAKE_STRING(T0, strlen(message), message);
  MAKE_EXCEPTION(T0, T0);

  /* Make the current procedure non-leaf, so that there's an unbroken */
  /* chain of continuations back up the stack for backtracing and */
  /* debugging. */
  if(state->here->proc.cont == obj_undef)
    ENTER(0);

  /* @@@@ It would be nice if the error handler could come back here and */
  /* resume execution somehow.	Perhaps it could re-start just before the */
  /* error checking code.  Some sort of loop would be needed around the */
  /* code that discovered the error condition. */

  NEW_ARGS();
  PUSH_ARG(T0);
  TAIL_TRANS(proc_throw);
  in = 0;
  longjmp(state->driver_loop, LJ_ERROR);
}


/* check_arg_list -- check number and type of arguments or results */

static void check_arg_list(state_t state, const char *what, ulong n, va_list vargs)
{
  ulong i;
  obj_t list;

  unless(ARGS == n)
    error(state, "takes %lu %s%s, not %lu",
	         (ulong)n,
	         what,
	         n == 1 ? "" : "s",	/* pluralize "what" */
	         (ulong)ARGS);

  list = ARGL;
  for(i = 0; i < n; ++i) {
    type_t type = va_arg(vargs, type_t);
    if(type != 0 && TYPE(CAR(list)) != type)
      error(state, "%s %lu must be of type %s not type %s",
	           what,
	           i,
	           type_name(type),
	           type_name(TYPE(CAR(list))));
    list = CDR(list);
  }
}

extern void check_args(state_t state, ulong n, ...)
{
  va_list vargs;
  va_start(vargs, n);
  check_arg_list(state, "argument", n, vargs);
  va_end(vargs);
}

extern void check_results(state_t state, ulong n, ...)
{
  va_list vargs;
  va_start(vargs, n);
  check_arg_list(state, "result", n, vargs);
  va_end(vargs);
}


/* hash -- hash a string to an unsigned long
 *
 * This hash function was derived (with permission) from
 * Paul Haahr's hash in the most excellent rc 1.4.
 */

static unsigned long hash(const char *s, size_t l)
{
  char c;
  ulong h = 0;
  size_t i = 0;
  repeat {
    if(i == l) break; c = s[i++]; h += (c<<17) ^ (c<<11) ^ (c<<5) ^ (c>>1);
    if(i == l) break; c = s[i++]; h ^= (c<<14) + (c<<7) + (c<<4) + c;
    if(i == l) break; c = s[i++]; h ^= (~c<<11) | ((c<<3) ^ (c>>1));
    if(i == l) break; c = s[i++]; h -= (c<<16) | (c<<9) | (c<<2) | (c&3);
  }
  return h;
}


/* eqvp -- eqv? equality
 *
 * The R5RS definition of eqv? is rather long, so I haven't pasted it in here.
 */

extern int eqvp(obj_t obj1, obj_t obj2)
{
  if(obj1 == obj2)
    return 1;
  if(TYPE(obj1) == TYPE(obj2))
    switch(TYPE(obj1)) {
    case TYPE_INTEGER: return INT(obj1) == INT(obj2);
    case TYPE_CHARACTER: return obj1->character.c == obj2->character.c;
    }
  return 0;
}


/* ENVIRONMENT */


/* lookup_in_frame -- look up a symbol in single frame
 *
 * Search a single frame of the environment for a symbol binding.
 */

extern obj_t lookup_in_frame(state_t state, obj_t frame, obj_t symbol)
{
  COUNT(ENV_LOOKUP_IN_FRAME);
  while(frame != obj_empty) {
    COUNT(ENV_LOOKUP_SCAN);
    ASSERT(TYPE(frame) == TYPE_PAIR);
    ASSERT(TYPE(CAR(frame)) == TYPE_PAIR);
    ASSERT(TYPE(CAAR(frame)) == TYPE_SYMBOL);
    if(CAAR(frame) == symbol)
      return CAR(frame);
    frame = CDR(frame);
  }
  return obj_undef;
}


/* lookup -- look up symbol in environment
 *
 * Search an entire environment for a binding of a symbol.
 */

extern obj_t lookup(state_t state, obj_t env, obj_t symbol)
{
  obj_t binding;
  COUNT(ENV_LOOKUP);
  while(env != obj_empty) {
    ASSERT(TYPE(env) == TYPE_PAIR);
    binding = lookup_in_frame(state, CAR(env), symbol);
    if(binding != obj_undef)
      return binding;
    env = CDR(env);
  }
  return obj_undef;
}

/* eq? obj1 obj2 
 *
 * "`Eq?' is similar to `eqv?' except that in some cases it is capable
 *  of discerning distinctions finer than those detectable by `eqv?'.
 *  
 * "`Eq?' and `eqv?' are guaranteed to have the same behavior on
 *  symbols, booleans, the empty list, pairs, procedures, and
 *  non-empty strings and vectors. `Eq?''s behavior on numbers and
 *  characters is implementation-dependent, but it will always return
 *  either true or false, and will return true only when `eqv?' would
 *  also return true. `Eq?' may also behave differently from `eqv?' on
 *  empty vectors and empty strings." -- R5RS 6.1
 */

static void eqp_entry(state_t state)
{
  check_args(state, 2, 0, 0);
  T0 = A0 == A1 ? obj_true : obj_false;
  RET1(T0);
}


/* eqv? -- "useful" equivalence relation
 *
 * (eqv? <obj1> <obj2>)
 */

static void eqvp_entry(state_t state)
{
  check_args(state, 2, 0, 0);
  T0 = eqvp(A0, A1) ? obj_true : obj_false;
  RET1(T0);
}


/* type obj
 *
 * Returns the type code from the header of the object.	 This is used
 * in Scheme to build the type test predicates, "integer?" etc.
 */

static void type_entry(state_t state)
{
  check_args(state, 1, 0);
  MAKE_INTEGER(T0, TYPE(A0));
  RET1(T0);
}


/* find -- find entry for symbol in symbol table
 *
 * Look for a symbol matching the string in the symbol table.
 * If the symbol was found, returns the address of the symbol
 * table entry which points to the symbol.  Otherwise it
 * either returns the address of a NULL entry into which the
 * new symbol should be inserted, or NULL if the symbol table
 * is full.
 */

static veclen_t find(obj_t symtab,
		     char *string,
		     size_t string_length) {
  ulong h;
  veclen_t i, j;
  veclen_t length = VECLEN(symtab);

  h = hash(string, string_length);
  i = h & (length-1);
  j = i;
  do {
    obj_t symbol = VECREF(symtab, i);
    if(symbol == obj_undef ||
       (string_length == SYMLEN(symbol) &&
	memcmp(string, SYMSTR(symbol), string_length) == 0))
      return i;
    i = (i+(h|1)) & (length-1);
  } while(i != j);

  return length;
}


/* intern -- low-level string->symbol
 *
 * Looks up or creates a symbol from the string in T0.  The symbol is
 * returned in T0.
 */

extern void intern(state_t state)
{
  veclen_t where;
  veclen_t i;

  ASSERT(TYPE(T0) == TYPE_STRING);

  where = find(state->obj_symtab, STR(T0), STRLEN(T0));

  /* Is the symbol table full?	If so, double its size, rehash all the */
  /* symbols, and try again. */

  if(where == VECLEN(state->obj_symtab)) {

    MAKE_VECTOR(T1, VECLEN(state->obj_symtab) * 2, obj_undef);

    for(i = 0; i < VECLEN(state->obj_symtab); ++i) {
      T2 = VECREF(state->obj_symtab, i);
      ASSERT(VECREF(state->obj_symtab, i) != obj_undef);
      where = find(T1, SYMSTR(T2), SYMLEN(T2));
      ASSERT(where != VECLEN(T1));
      ASSERT(VECREF(T1, where) == obj_undef);
      VECSET(T1, where, T2);
    }

    state->obj_symtab = T1;

    where = find(state->obj_symtab, STR(T0), STRLEN(T0));
    ASSERT(where != VECLEN(T1));
    ASSERT(VECREF(T1, where) == obj_undef);
  }

  /* The symbol wasn't found, but an empty slot for it was. */
  if(VECREF(state->obj_symtab, where) == obj_undef) {
    MAKE_SYMBOL(T1, STRLEN(T0), STR(T0));
    VECSET(state->obj_symtab, where, T1);
  }
  
  T0 = VECREF(state->obj_symtab, where);
}


/* gc -- force a garabge collection now */

static void gc_entry(state_t state)
{
  check_args(state, 0);
  gc(state, 0);
  RET0();
}


/* stats -- print heap stats */

static void stats_entry(state_t state)
{
  veclen_t i;
  check_args(state, 0);
  stats(state, state->heap_base, state->heap_next);
#ifdef COUNT_STATS
  MAKE_VECTOR(T0, COUNT_MAX, obj_uninit);
  for(i = 0; i < COUNT_MAX; ++i) {
    MAKE_INTEGER(T1, state->count[i]);
    VECSET(T0, i, T1);
  }
  RET1(T0);
#else /* COUNT_STATS not */
  RET0();
#endif /* COUNT_STATS */
}


/* trace! -- switch tracing on or off */

static void trace_entry(state_t state)
{
  check_args(state, 1, TYPE_SPECIAL);
  state->trace = A0 != obj_false;
  RET0();
}


/* heap_check! -- switch heap checking on or off */

static void heap_check_entry(state_t state)
{
  check_args(state, 1, TYPE_SPECIAL);
  state->heap_checking = A0 != obj_false;
  RET0();
}


/* INITIALIZATION */


/* syntab -- initial syntax table
 *
 * This table used by "start" to create the initial syntax environment.
 *
 * "let*" and "letrec" can share an implementation because of the way
 * that environments are implemented with frames.
 */

static const proctab_s syntab[] = {
  {NULL,                NULL}
};


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "start".
 */

static const proctab_s proctab[] = {
  {"eq?",			eqp_entry},
  {"eqv?",			eqvp_entry},
  {"gc",			gc_entry},
  {"heap-check!",		heap_check_entry},
  {"stats",			stats_entry},
  {"trace!",			trace_entry},
  {"type",			type_entry},
  {NULL,                        NULL}
};


/* global_procs -- procedures accessible to all functions
 *
 * The procs table of the state structure are initialized with the binding
 * pairs of these procedures, so that they can be called from anywhere.
 *
 * IMPORTANT: They must therefore appear in exactly the same order as the
 * "proc_*" macros in "sc.h".
 */

static const char *global_procs[] = {
  "read-eval-print",
  "read",
  "eval",
  "write",
  "list->vector",
  "string->symbol",
  "open-input-file",
  "length",
  "close-input-port",
  "throw",
};


/* init_units -- other units to be loaded into the initial environment */

extern const label_t sc_unit_scint, sc_unit_scchar, sc_unit_scport, sc_unit_scrw,
                     sc_unit_screp, sc_unit_sceval, sc_unit_sclist, sc_unit_scstring,
                     sc_unit_scvector, sc_unit_scsymbol, sc_unit_scsyntax,
                     sc_unit_scload, sc_unit_scexception, sc_unit_scdynload,
                     sc_unit_scenv, sc_unit_scproc, sc_unit_init;

static const struct {
  const char *name;
  const label_t *const entry;
} init_units[] = {
  {"scsyntax", &sc_unit_scsyntax},
  {"scint", &sc_unit_scint},
  {"scchar", &sc_unit_scchar},
  {"scport", &sc_unit_scport},
  {"scrw", &sc_unit_scrw},
  {"screp", &sc_unit_screp},
  {"sceval", &sc_unit_sceval},
  {"sclist", &sc_unit_sclist},
  {"scstring", &sc_unit_scstring},
  {"scvector", &sc_unit_scvector},
  {"scsymbol", &sc_unit_scsymbol},
  {"scload", &sc_unit_scload},
  {"scexception", &sc_unit_scexception},
  {"scdynload", &sc_unit_scdynload},
  {"scenv", &sc_unit_scenv},
  {"scproc", &sc_unit_scproc},
  {"init", &sc_unit_init},
};


/* bind_procs -- bind procedures and operators from table into environment
 *
 * Expects the environment in A0, and returns with A0 intact.
 */

extern void bind_procs(state_t state,
                       const proctab_s procs[],
                       const proctab_s ops[])
{
  size_t i;

  check_args(state, 1, TYPE_PAIR); /* the environment */

  for(i = 0; procs[i].name != NULL; ++i) {
    MAKE_STRING(T0, strlen(procs[i].name), procs[i].name);
    intern(state);
    MAKE_PROC(T1, T0, procs[i].entry);
    BIND(CAR(A0), T0, T1, T2);
  }

  for(i = 0; ops[i].name != NULL; ++i) {
    MAKE_STRING(T0, strlen(ops[i].name), ops[i].name);
    intern(state);
    MAKE_PROC(T1, T0, ops[i].entry);
    BIND(CDR(A0), T0, T1, T2);
  }
}


static state_t state_create(void)
{
  state_t state;
  void *heap, *p;
  size_t i;
  mps_res_t res;
  mps_arena_t arena;
  mps_pool_t state_pool, obj_pool;

  /* What on earth is 64<<20? */
  res = mps_arena_create(&arena, mps_arena_class_vm(), 64<<20);
  if(res != MPS_RES_OK)
    goto fail_arena;

  obj_pool = create_pool(arena);
  if(obj_pool == NULL)
    goto fail_pool;
  
  /* What do these parameters mean? */
  res = mps_pool_create(&state_pool, arena, mps_class_mv(),
                        sizeof(state_s), sizeof(state_s), sizeof(state_s));
  if(res != MPS_RES_OK)
    goto fail_pool;

  res = mps_alloc(&p, state_pool, sizeof(state_s));
  if(res != MPS_RES_OK)
    goto fail_state;
  state = (state_t)p;

  res = mps_alloc(&heap, state_pool, HEAP);
  if(res != MPS_RES_OK)
    goto fail_heap;

  state->arena = arena;
  state->state_pool = state_pool;
  state->obj_pool = obj_pool;

  res = mps_ap_create(&state->obj_ap, obj_pool, MPS_RANK_EXACT);
  if(res != MPS_RES_OK)
    goto fail_ap;

  state->trace = 0;		/* don't trace by default */
  state->heap_checking = 0;	/* don't heap check by default */
  state->step = 0;		/* the machine step is zero */
  state->inited = 0;		/* the state is not yet initialized */
  state->heap_size = HEAP;
  state->heap_total = 0;
  state->heap_last = 0;
  state->heap_base = heap;
  state->heap_next = heap;
  state->heap_limit = (void *)((char *)heap + state->heap_size);
  state->old_base = NULL;
  state->old_limit = NULL;
  for(i = 0; i < ARRAYLEN(state->procs); ++i)
    state->procs[i] = obj_uninit;
  state->here = obj_uninit;
  state->cont = obj_uninit;
  state->argl = obj_uninit;
  state->args = 0;
  for(i = 0; i < ARRAYLEN(state->temp); ++i)
    state->temp[i] = obj_uninit;
  state->errproc = obj_uninit;
  state->inport = obj_uninit;
  state->outport = obj_uninit;
  state->errport = obj_uninit;
  state->obj_symtab = obj_uninit;
  state->units = obj_uninit;

#ifdef COUNT_STATS
  for(i = 0; i < ARRAYLEN(state->count); ++i)
    state->count[i] = 0;
#endif

  /* The roots should all now be initialized. */
  state->inited = 1;

  if(create_root(arena, state) == NULL)
    goto fail_root;

  heap_check(state);

  return state;

fail_root:
  mps_ap_destroy(state->obj_ap);
fail_ap:
  mps_free(state_pool, state_pool, HEAP);
fail_heap:
  mps_free(state_pool, state, sizeof(state_s)); /* @@@@ won't pool_destroy do this? */
fail_state:
  mps_pool_destroy(state_pool); /* @@@@ won't arena_destroy do this? */
fail_pool:
  mps_arena_destroy(arena);
fail_arena:
  return NULL;
}

static void state_destroy(state_t state)
{
  ASSERT(state != NULL);
  free(state->heap_base);
  free(state);
}


/* make_state -- initialize a new machine
 *
 * Creates a new state object, independent of all others, with the initial
 * environment in A0.
 */

static void start_entry(state_t state);
static void start_units_while(state_t state);
static void start_units_loop(state_t state);
static void start_units_done(state_t state);

static void start_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR); /* the environment */

  ENTER(2);

  /* Bind the initial set of procedures and operators. */
  bind_procs(state, proctab, syntab);

  /* Load the initial units.  To make sure this happens in the order */
  /* specified in the init_units array, we look them up one by one */
  /* rather than looping over state->units. */
  L0 = A0;
  MAKE_INTEGER(L1, 0);
  JUMP(start_units_while);
}

static void start_units_while(state_t state)
{
  const char *string;
  ASSERT(0 <= INT(L1));
  unless((size_t)INT(L1) < ARRAYLEN(init_units))
    JUMP(start_units_done);
  string = init_units[INT(L1)].name;
  MAKE_STRING(T0, strlen(string), string);
  intern(state);
  T1 = lookup(state, state->units, T0);
  ASSERT(T1 != obj_undef);      /* it was bound in make_state */
  CALL1(CDR(T1), L0, start_units_loop);
}

static void start_units_loop(state_t state)
{
  check_results(state, 0);      /* expect no results from unit linker */
  SETINT(L1, INT(L1) + 1);
  JUMP(start_units_while);
}

static void start_units_done(state_t state)
{
  size_t i;

  /* Initialize the global procedures. */
  ASSERT(ARRAYLEN(global_procs) == ARRAYLEN(state->procs));
  for(i = 0; i < ARRAYLEN(global_procs); ++i) {
    MAKE_STRING(T0, strlen(global_procs[i]), global_procs[i]);
    intern(state);
    T1 = lookup(state, CAR(L0), T0);
    ASSERT(T1 != obj_undef); /* bind_procs defined it */
    state->procs[i] = T1;
  }
  
  LEAVE();
  RET1(L0); /* the environment */
}


extern state_t make_state(void)
{
  state_t state;
  size_t i;
  int res;
  
  state = state_create();
  if(state == NULL)
    return NULL;

  /* At this point the state is sufficiently set up to allow allocation */
  /* and other abstract machine stuff that can be done in one basic */
  /* block. */

  if(setjmp(state->driver_loop) != 0)
    goto fail_init; /* initialization code below failed somehow */

  /* Initialize the symbol table. */
  MAKE_VECTOR(state->obj_symtab, SYMTAB, obj_undef);

  /* Intern the static initial symbols.	 This assumes that there */
  /* will be room in the initial symbol table. */
  for(i = 0; i < static_symbols_length; ++i) {
    veclen_t where;
    T0 = STATIC_SYMBOL(i);
    where = find(state->obj_symtab, SYMSTR(T0), SYMLEN(T0));
    ASSERT(where != VECLEN(state->obj_symtab));
    ASSERT(VECREF(state->obj_symtab, where) == obj_undef);
    VECSET(state->obj_symtab, where, T0);
  }

  /* Initialize the global ports. */
  MAKE_INPORT(state->inport, stdin);
  MAKE_OUTPORT(state->outport, stdout);
  MAKE_OUTPORT(state->errport, stderr);

  /* Create an empty set of units. */
  MAKE_PAIR(state->units, obj_empty, obj_empty);
  for(i = 0; i < ARRAYLEN(init_units); ++i) {
    MAKE_STRING(T0, strlen(init_units[i].name), init_units[i].name);
    intern(state);
    MAKE_PROC(T1, T0, *init_units[i].entry);
    BIND(state->units, T0, T1, T2);
  }

  /* Create an empty environment into which everything will be bound. */
  MAKE_PAIR(T0, obj_empty, obj_empty);
  MAKE_PAIR(T1, obj_empty, obj_empty);
  MAKE_PAIR(T2, T0, T1);

  MAKE_PROC(T0, sym_start, start_entry);
  NEW_ARGS();
  PUSH_ARG(T2);
  res = run(state);
  if(res != 0)
    goto fail_run;

  return state;

fail_run:
fail_init:
  state_destroy(state);
  return NULL;
}
