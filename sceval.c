/* sceval.c -- evaluator
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */


#include "sc.h"


/* eval expression environment-specifier 
 *
 * "Evaluates expression in the specified environment and returns its
 *  value. Expression must be a valid Scheme expression represented as
 *  data, and environment-specifier must be a value returned by one of
 *  the three procedures described below. Implementations may extend
 *  `eval' to allow non-expression programs (definitions) as the first
 *  argument and to allow other values as environments, with the
 *  restriction that `eval' is not allowed to create new bindings in
 *  the environments associated with `null-environment' or
 *  `scheme-report-environment'." -- R5RS
 *
 * The environment is represented as a pair (venv . senv) where venv
 * is the value environment and senv is the syntactic environment
 * (of operators such as "if", "define", etc.).
 *
 * L0	environment for the evaluation
 * L1	expression being evaluated (used by backtrace)
 * L2	operands of procedure application
 * L3	the procedure to apply (result of evaluating operator)
 * L4	evaluated argument list
 * L5	the last pair in the argument list (to make appending easy)
 * L6	argument count (length of argument list)
 */

extern void eval_entry(state_t);
static void eval_fun(state_t);
static void eval_arg(state_t);
static void eval_arg2(state_t);

extern void eval_entry(state_t state)
{
  check_args(state, 2, 0, TYPE_PAIR);

  /* Note that we defer ENTER until as late as possible, to avoid allocating */
  /* and hopefully make evaluation of simple cases faster. */

  switch(TYPE(A0)) {
  case TYPE_SPECIAL:		/* self-evaluating object */
    if(A0 == obj_empty)		/* the empty list isn't self-evaluating */
      break;
  case TYPE_INTEGER:
  case TYPE_STRING:
  case TYPE_CHARACTER:
    T0 = A0;
    RET1(T0);

  case TYPE_SYMBOL:		/* variable lookup */
    T0 = lookup(state, CAR(A1), A0);
    unless(T0 == obj_undef)
      RET1(CDR(T0));
    /* We ENTER here purely so that the backtrace will be able to display */
    /* the expression that caused the error. */
    ENTER(2);
    L1 = A0;			/* used by backtrace */
    L0 = A1;			/* environment */
    error(state, "unbound symbol \"%s\"", SYMSTR(A0)); /* @@@@ assumes symbol not funny */

  case TYPE_PAIR:		/* application */
    /* If the application is of the form (<symbol> <operands>...) */
    /* then see if the symbol is in the syntax environment.  If it */
    /* is, then the application is a "special form", so call the */
    /* operator to evaluate it. */
    if(TYPE(CAR(A0)) == TYPE_SYMBOL) {
      T0 = lookup(state, CDR(A1), CAR(A0));
      if(T0 != obj_undef) {
	T1 = CDR(T0);			/* the operator procedure */
	ASSERT(TYPE(T1) == TYPE_PROC);
	T0 = CDR(A0);			/* pass the operands unevaluated */
	T2 = A1;
	TAIL2(T1, T2, T0);
      }
    }

    ENTER(7);                   /* [2] */
    L1 = A0;			/* used by backtrace */
    L0 = A1;			/* environment */

    /* Otherwise, the expression is an ordinary procedure */
    /* application, so evaluate the CAR to get the procedure */
    /* to apply, then evaluate the arguments to create an */
    /* argument list for the procedure, then call it. */
    L2 = CDR(A0);		/* remember operands for later */
    T0 = CAR(A0);		/* the unevaluated operator */
    CALL2(proc_eval, T0, L0, eval_fun);
  }

  /* We ENTER here purely so that the backtrace will be able to display */
  /* the expression that caused the error. */
  ENTER(2);
  L1 = A0;			/* used by backtrace */
  L0 = A1;			/* environment */
  error(state, "unknown syntax");
}

static void eval_fun(state_t state)
{
  check_results(state, 1, TYPE_PROC); /* expect one result from eval */
/*
  unless(TYPE(A0) == TYPE_PROC)
    error(state, "application of non-procedure");
 */
  L3 = A0;			/* L3 is the procedure to apply */
  L4 = obj_empty;		/* L4 will be the argument list */
  MAKE_INTEGER(L6, 0);		/* L6 is the count of arguments */
  JUMP(eval_arg);
}

static void eval_arg(state_t state)
{
  /* If we've reached the end of the argument list L2 then the */
  /* list L4 contains the evaluated arguments, so just tail */
  /* call the procedure in L3. */
  if(TYPE(L2) != TYPE_PAIR) {
    unless(L2 == obj_empty)
      error(state, "malformed argument list");
    ARGL = L4;
    ARGS = INT(L6);
    LEAVE();
    TAIL(L3);
  }

  /* Evaluate the next argument in the operands list. */
  CALL2(proc_eval, CAR(L2), L0, eval_arg2);
}

static void eval_arg2(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from eval */
  /* Append the evaluated argument to the argument list. */
  MAKE_PAIR(T0, A0, obj_empty);
  if(L4 == obj_empty)
    L4 = T0;
  else
    SETCDR(L5, T0);
  L5 = T0;
  SETINT(L6, INT(L6) + 1);		/* count the argument */
  L2 = CDR(L2);			/* move on to the next operand */
  JUMP(eval_arg);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"eval", eval_entry},
  {NULL, NULL}
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


const label_t sc_unit_sceval = &unit_entry;
