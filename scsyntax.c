/* scsyntax.c -- basic operators and syntax
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* lambda -- create a function closure
 *
 * (lambda <formals> <exp1> <exp2> ...)
 *
 * The lambda operator creates a new procedure which enters "exec"
 * in order to bind the formals to the actual arguments and evaluate
 * the expressions in the lambda body.
 *
 * @@@@ If we were compiling (even partially) then lambda would build
 * a special environment for execution here, containing only the free
 * variables of the body.  Ideally, we'd replace the variable references
 * in the body with direct O(1) references to this environment, which
 * would be the procedure's "closure".
 *
 * L0	environment in which lambda is being evaluated
 * L1	formal argument list
 * L2	procedure body (expression list)
 */

static void exec_entry(state_t);

static void lambda_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, TYPE_PAIR);

  unless(TYPE(A1) == TYPE_PAIR &&
	 TYPE(CDR(A1)) == TYPE_PAIR)
    error(state, "illegal syntax");

  /* Check syntax of argument list to save time when applying */
  /* the procedure. */
  T0 = CAR(A1);
  while(TYPE(T0) == TYPE_PAIR) {
    unless(TYPE(CAR(T0)) == TYPE_SYMBOL)
      error(state, "non-symbol in argument list");
    T0 = CDR(T0);
  }
  unless(T0 == obj_empty || TYPE(T0) == TYPE_SYMBOL)
    error(state, "badly terminated argument list");

  /* Make the procedure by building a procedure which will enter "exec" */
  /* with the environment, formals, and body in its local variables. */
  MAKE_PROC_REGS(T0, sym_anonymous, exec_entry, 3);
  LOC(T0, 0) = A0;
  LOC(T0, 1) = CAR(A1);		/* <formals> */
  LOC(T0, 2) = CDR(A1);		/* <exp1> <exp2> ... */

  RET1(T0);
}


/* exec -- execute a procedure body
 *
 * This is the entry point for interpreted functions, as set up
 * by "lambda".	 On entry, ARGL is the actual argument list of
 * the procedure, and "lambda" has set up the local variables:
 *
 * L0	closure environment
 * L1	formal argument list
 * L2	procedure body
 *
 * Note that exec_entry is also used by named let (see let_named()).
 */

static void begin_while(state_t);

static void exec_entry(state_t state)
{
  ENTER(3);

  /* Add a new frame to each of the value and syntax environments */
  /* so that new bindings are created locally. */
  MAKE_PAIR(T0, obj_empty, CAR(L0));
  MAKE_PAIR(T1, obj_empty, CDR(L0));
  MAKE_PAIR(L0, T0, T1);

  /* Bind the formal arguments to the actual arguments by */
  /* "zipping" the two lists together. */
  T0 = L1;
  T1 = ARGL;
  while(TYPE(T0) == TYPE_PAIR) {
    if(TYPE(T1) != TYPE_PAIR)
      error(state, "too few arguments");
    ASSERT(TYPE(CAR(T0)) == TYPE_SYMBOL);
    BIND(CAR(L0), CAR(T0), CAR(T1), T2);
    T0 = CDR(T0);
    T1 = CDR(T1);
  }

  /* If the formal argument list is terminated with a symbol */
  /* then there is a "rest arg" which binds to the rest of */
  /* the arguments, however many there are. */
  if(TYPE(T0) == TYPE_SYMBOL)
    BIND(CAR(L0), T0, T1, T2);
  else {
    ASSERT(T0 == obj_empty); /* lambda should've checked this */
    if(T1 != obj_empty)
      error(state, "too many arguments");
  }

  /* Run the procedure body by jumping to begin_while. */
  L1 = L2;
  JUMP(begin_while);
}


/* quote -- return operands unevaluated
 *
 * In Scheme, (quote foo) evaluates to foo (i.e. foo is not evaluated).
 * See R4RS 4.1.2.  The reader expands "'x" to "(quote x)".
 */

static void quote_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  unless(TYPE(A1) == TYPE_PAIR &&
	 CDR(A1) == obj_empty)
    error(state, "illegal syntax");
  T0 = CAR(A1);
  RET1(T0);
}


/* define -- bind a symbol in the top frame of the environment
 *
 * In Scheme, "(define <symbol> <expression>)" evaluates expressions
 * and binds it to symbol in the top frame of the environment (see
 * R4RS 5.2).  This code also allows the non-essential syntax for
 * define, "(define (<symbol> <formals>) <body>)" as a short-hand for
 * "(define <symbol> (lambda (<formals>) <body>))".
 *
 * L0	environment
 * L1	the symbol to be defined (used by "backtrace")
 *
 * @@@@ It would be nice to allow multi-value binding.
 */

extern void define_entry(state_t);
static void define_bind(state_t);
static void define_bind_fun(state_t);

extern void define_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  ENTER(2);
  L0 = A0;			/* environment */
  T0 = A1;			/* operands */
  unless(TYPE(T0) == TYPE_PAIR &&
	 TYPE(CDR(T0)) == TYPE_PAIR)
    error(state, "illegal syntax");

  /* (define <symbol> <expression>) */
  if(TYPE(CAR(T0)) == TYPE_SYMBOL) {
    unless(CDDR(T0) == obj_empty)
      error(state, "illegal syntax");
    L1 = CAR(T0);		/* the symbol */
    CALL2(proc_eval, CADR(T0), L0, define_bind);
  } else if(TYPE(CAR(T0)) == TYPE_PAIR &&
	    TYPE(CAAR(T0)) == TYPE_SYMBOL) {
    L1 = CAAR(T0);		/* the symbol */
    /* Build an expression of the form (lambda (<formals>) <body>) */
    /* and evaluate it to get the procedure to bind the symbol to. */
    MAKE_PAIR(T0, CDAR(T0), CDR(T0));
    MAKE_PAIR(T0, sym_lambda, T0);
    CALL2(proc_eval, T0, L0, define_bind_fun);
  } else
    error(state, "illegal syntax");
}

static void define_bind(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from eval */
  BIND(CAR(L0), L1, A0, T0);
  LEAVE();
  RET0();
}

static void define_bind_fun(state_t state)
{
  check_results(state, 1, TYPE_PROC);/* expect one result from eval */
  A0->proc.name = L1;		/* set the printable procedure name */
  BIND(CAR(L0), L1, A0, T0);
  LEAVE();
  RET0();
}


/* set! -- assignment
 *
 * (set! <variable> <expression>)
 * See R4RS 4.1.6.
 *
 * L0	the binding pair of the variable
 *
 * @@@@ It would be nice to allow multi-value set.
 */

static void set_entry(state_t);
static void set_set(state_t);

static void set_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  ENTER(1);
  T0 = A0;			/* environment */
  T1 = A1;			/* operands */
  unless(TYPE(T1) == TYPE_PAIR &&
	 TYPE(CDR(T1)) == TYPE_PAIR &&
	 CDDR(T1) == obj_empty)
    error(state, "illegal syntax");
  unless(TYPE(CAR(T1)) == TYPE_SYMBOL)
    error(state, "applied to non-symbol");
  L0 = lookup(state, CAR(T0), CAR(T1));
  if(L0 == obj_undef)
    error(state, "applied to unbound symbol \"%s\"", CAR(T1)->symbol.string);
  CALL2(proc_eval, CADR(T1), T0, set_set);
}

static void set_set(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from eval */
  SETCDR(L0, A0);		/* set cdr of binding to result */
  LEAVE();
  RET0();
}


/* begin -- sequencing
 *
 * (begin <expression1> <expression2> ...)
 * See R4RS 4.2.3.
 * R5RS requires the last expression be tail-called (r5rs_5.html#SEC23).
 *
 * L0	the environment
 * L1	the (remaining) sequence of expressions
 */

static void begin_entry(state_t);
static void begin_while(state_t);
static void begin_loop(state_t);

static void begin_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  ENTER(2);
  L0 = A0;			/* environment */
  L1 = A1;			/* operands */
  JUMP(begin_while);
}

/* Note: begin_while is shared code between several operators that execute */
/* a sequence of instructions. */

static void begin_while(state_t state)
{
  unless(TYPE(L1) == TYPE_PAIR)
    error(state, "illegal syntax");

  if(CDR(L1) == obj_empty) {	/* last expression? */
    LEAVE();
    TAIL2(proc_eval, CAR(L1), L0);
  }

  CALL2(proc_eval, CAR(L1), L0, begin_loop);
}

static void begin_loop(state_t state)
{
  /* NOTE: The results from eval are ignored. */
  L1 = CDR(L1);
  JUMP(begin_while);
}


/* cond -- general conditional
 *
 * "(cond (<test1> <exp1.1> ...) (<test2> <exp2.1> ...) ...
 *  [(else <expe.1> ...)])"
 *
 * R5RS requires that the last expression of a cond clause be
 * tail called (r5rs_5.html#SEC23).
 *
 * L0	the environment
 * L1	the (remaining) list of clauses
 * L2	the first clause
 */

static void cond_entry(state_t);
static void cond_while(state_t);
static void cond_test(state_t);

static void cond_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  ENTER(3);
  L0 = A0;			/* environment */
  L1 = A1;			/* operands */
  unless(TYPE(L1) == TYPE_PAIR)
    error(state, "illegal syntax");
  JUMP(cond_while);
}

static void cond_while(state_t state)
{
  unless(TYPE(L1) == TYPE_PAIR) {
    unless(L1 == obj_empty)
      error(state, "illegal syntax in last clause");
    LEAVE();
    RET0();
  }
  L2 = CAR(L1);			/* clause */
  unless(TYPE(L2) == TYPE_PAIR)
    error(state, "illegal clause syntax");
  if(CAR(L2) == sym_else) {
    unless(CDR(L1) == obj_empty)
      error(state, "else clause must come last");
    L1 = CDR(L2);		/* skip the "else" */
    JUMP(begin_while);		/* execute the clause */
  }
  CALL2(proc_eval, CAR(L2), L0, cond_test);
}

static void cond_test(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from eval */
  if(A0 != obj_false) {		/* if test is not false */
    L1 = CDR(L2);
    if(TYPE(L1) == TYPE_PAIR) { /* if the clause is non-empty ... */
      if(CAR(L1) == sym_arrow)	/* (test => expression) */
	L1 = CDR(L1);		/* skip the "=>" */
      JUMP(begin_while);	/* ... execute the clause */
    }
    T0 = A0;			/* otherwise return the test result */
    LEAVE();
    RET1(T0);
  }
  L1 = CDR(L1);			/* otherwise try the next clause */
  JUMP(cond_while);
}


/* if -- conditional
 *
 * (if <test> <consequent> <alternate>)
 * (if <test> <consequent>)
 *
 * R5RS requires both the consequent and the alternate be tail-called.
 * (r5rs_5.html#SEC23).
 *
 * L0	the environment
 * L1	the operands
 */

static void if_entry(state_t);
static void if_test(state_t);

static void if_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  ENTER(3);
  L0 = A0;		/* environment */
  L1 = A1;		/* operands */
  unless(TYPE(L1) == TYPE_PAIR &&
	 TYPE(CDR(L1)) == TYPE_PAIR &&
	 (CDDR(L1) == obj_empty ||
	  (TYPE(CDDR(L1)) == TYPE_PAIR &&
	   CDDDR(L1) == obj_empty)))
    error(state, "illegal syntax");
  CALL2(proc_eval, CAR(L1), L0, if_test);
}

static void if_test(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from eval */
  LEAVE();
  if(A0 != obj_false)		/* if test is not false, eval consequent */
    TAIL2(proc_eval, CADR(L1), L0);
  if(TYPE(CDDR(L1)) == TYPE_PAIR)
    TAIL2(proc_eval, CADDR(L1), L0);
  RET0();
}


/* and -- conjunction conditional
 *
 * (and <test1> ...)
 *
 * R5RS requires that the last expression be tail-called (r5rs_5.html#SEC23).
 *
 * L0	the environment
 * L1	the (remaining) list of tests
 */

static void and_entry(state_t);
static void and_while(state_t);
static void and_loop(state_t);

static void and_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  ENTER(2);
  L0 = A0;		/* environment */
  L1 = A1;		/* operands */
  if(L1 == obj_empty) {
    LEAVE();
    RET1(obj_true);
  }
  JUMP(and_while);
}

static void and_while(state_t state)
{
  unless(TYPE(L1) == TYPE_PAIR)
    error(state, "illegal syntax");

  if(CDR(L1) == obj_empty) {	/* last expression? */
    LEAVE();
    TAIL2(proc_eval, CAR(L1), L0);
  }

  CALL2(proc_eval, CAR(L1), L0, and_loop);
}

static void and_loop(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from eval */
  if(A0 == obj_false) {
    T0 = A0;
    LEAVE();
    RET1(T0);
  }
  L1 = CDR(L1);
  JUMP(and_while);
}


/* or -- disjunction conditional
 *
 * (or <test1> ...)
 *
 * R5RS requires that the last expression be tail-called (r5rs_5.html#SEC23).
 *
 * L0	the environment
 * L1	the (remaining) list of tests
 */

static void or_entry(state_t);
static void or_while(state_t);
static void or_loop(state_t);

static void or_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  ENTER(2);
  L0 = A0;		/* environment */
  L1 = A1;		/* operands */
  if(L1 == obj_empty) {
    LEAVE();
    RET1(obj_false);
  }
  JUMP(or_while);
}

static void or_while(state_t state)
{
  unless(TYPE(L1) == TYPE_PAIR)
    error(state, "illegal syntax");

  if(CDR(L1) == obj_empty) {	/* last expression? */
    LEAVE();
    TAIL2(proc_eval, CAR(L1), L0);
  }

  CALL2(proc_eval, CAR(L1), L0, or_loop);
}

static void or_loop(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from eval */
  if(A0 != obj_false) {
    T0 = A0;
    LEAVE();
    RET1(T0);
  }
  L1 = CDR(L1);
  JUMP(or_while);
}


/* let -- (let <bindings> <body>)
 * let* -- (let <bindings> <body>)
 *
 * @@@@ It would be nice to allow multi-value bind.
 * @@@@ It would be nice to spot (let ((foo (lambda ...))) ...)
 * and set the procedure name to "foo".
 *
 * L0	the environment
 * L1	the next binding / formal args for named let
 * L2	the operands
 * L3	the new local environment for the body
 * L4	the bindings or name in the case of named let
 */

static void let_entry(state_t);
static void let_named(state_t);
static void let_star_entry(state_t);
static void let_while(state_t);
static void let_bind(state_t);

static void let_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  unless(TYPE(A1) == TYPE_PAIR &&
	 TYPE(CDR(A1)) == TYPE_PAIR)
    error(state, "illegal syntax");
  ENTER(5);
  L0 = A0;			/* environment for bindings is outer env */
  L2 = A1;			/* operands */
  MAKE_PAIR(T0, obj_empty, CAR(A0));
  MAKE_PAIR(L3, T0, CDR(A0));	/* local environment for let body */
  L4 = CAR(A1);			/* bindings or name */
  if(TYPE(L4) == TYPE_SYMBOL)
    JUMP(let_named);
  else
    JUMP(let_while);
}

static void let_named(state_t state)
{
  unless(TYPE(CDDR(A1)) == TYPE_PAIR)
    error(state, "illegal syntax");

  /* Build a formal argument list made up of the local variable */
  /* names in the bindings list. */
  T0 = CADR(L2);		/* bindings */
  T1 = obj_empty;		/* last pair in args list */
  L1 = obj_empty;		/* new formals list */
  while(TYPE(T0) == TYPE_PAIR) {
    unless(TYPE(CAR(T0)) == TYPE_PAIR && TYPE(CAAR(T0)) == TYPE_SYMBOL)
      error(state, "illegal binding");
    MAKE_PAIR(T2, CAAR(T0), obj_empty);
    if(T1 == obj_empty)
      L1 = T2;
    else
      SETCDR(T1, T2);
    T1 = T2;
    T0 = CDR(T0);
  }
  unless(T0 == obj_empty)
    error(state, "illegal bindings list");

  /* Make a procedure which, when called, binds the local variable names */
  /* to its arguments, and executes the let body as if it were a procedure */
  /* body.  This is essentially doing the same job as "lambda_entry". */
  MAKE_PROC_REGS(T0, L4, exec_entry, 3);
  LOC(T0, 0) = L3;		/* proc runs in new local environment */
  LOC(T0, 1) = L1;		/* <formals> */
  LOC(T0, 2) = CDDR(L2);	/* <exp1> ... */

  /* Bind the procedure the named let name in the new local environment. */
  BIND(CAR(L3), L4, T0, T1);

  /* Now get on with the let. */
  L2 = CDR(L2);			/* operands less the name */
  L4 = CAR(L2);			/* bindings */
  JUMP(let_while);
}

static void let_star_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  unless(TYPE(A1) == TYPE_PAIR &&
	 TYPE(CDR(A1)) == TYPE_PAIR)
    error(state, "illegal syntax");
  ENTER(6);
  L2 = A1;			/* operands */
  MAKE_PAIR(T0, obj_empty, CAR(A0));
  MAKE_PAIR(L3, T0, CDR(A0));	/* local environment for let body */
  L0 = L3;			/* environment for bindings is local env */
  L4 = CAR(A1);			/* bindings */
  JUMP(let_while);
}

static void let_while(state_t state)
{
  unless(TYPE(L4) == TYPE_PAIR) {
    unless(L4 == obj_empty)
      error(state, "illegal bindings list");
    L0 = L3;			/* replace environment with new bindings */
    L1 = CDR(L2);		/* set operands to let body */
    JUMP(begin_while);		/* execute body */
  }
  L1 = CAR(L4);			/* next binding */
  unless(TYPE(L1) == TYPE_PAIR &&
	 TYPE(CAR(L1)) == TYPE_SYMBOL &&
	 TYPE(CDR(L1)) == TYPE_PAIR &&
	 CDDR(L1) == obj_empty)
    error(state, "illegal binding");
  CALL2(proc_eval, CADR(L1), L0, let_bind);
}

static void let_bind(state_t state)
{
  check_results(state, 1, 0);	/* except one result from eval */
  BIND(CAR(L3), CAR(L1), A0, T0);
  L4 = CDR(L4);
  JUMP(let_while);
}


/* case
 *
 * "(case <exp>
 *    ((<datum1.1> ...) <exp1.1> ...)
 *    ((<datum2.1> ...> <exp2.1> ...)
 *   [(else <expe.1> ...)"
 *
 * L0	environment
 * L1	operands
 */

static void case_entry(state_t);
static void case_while(state_t);

static void case_entry(state_t state)
{
  check_args(state, 2, TYPE_PAIR, 0);
  ENTER(2);
  L0 = A0;			/* environment */
  L1 = A1;			/* operands */
  unless(TYPE(L1) == TYPE_PAIR && TYPE(CDR(L1)) == TYPE_PAIR)
    error(state, "illegal syntax");

  /* Evaluate the key expression, to be compared with the data. */
  CALL2(proc_eval, CAR(L1), L0, case_while);
}

static void case_while(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from eval */
  T0 = A0;			/* the key */
  L1 = CDR(L1);			/* the clauses */

  while(TYPE(L1) == TYPE_PAIR) {
    T2 = CAR(L1);		/* clause */

    unless(TYPE(T2) == TYPE_PAIR &&
	   TYPE(CDR(T2)) == TYPE_PAIR)
      error(state, "illegal clause syntax");

    if(CAR(T2) == sym_else) {
      unless(CDR(L1) == obj_empty)
	error(state, "else clause must come last");
      L1 = CDR(T2);
      JUMP(begin_while);	/* execute the clause */
    }

    T1 = CAR(T2);		/* the list of data */
    while(TYPE(T1) == TYPE_PAIR) {
      if(eqvp(T0, CAR(T1))) {	/* does the key match the datum? */
	L1 = CDR(T2);
	JUMP(begin_while);	/* execute the clause */
      }
      T1 = CDR(T1);
    }
    if(T1 != obj_empty)
      error(state, "illegal data list syntax");

    L1 = CDR(L1);		/* try the next clause */
  }

  unless(L1 == obj_empty)
    error(state, "illegal syntax in last clause");
  LEAVE();
  RET0();
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {NULL, NULL}
};


/* syntab -- syntax table
 *
 * This table used by "unit_entry" to create the initial syntax environment.
 */

static const proctab_s syntab[] = {
  {"and",		and_entry},
  {"begin",		begin_entry},
  {"case",		case_entry},
  {"cond",		cond_entry},
  {"define",		define_entry},
  {"if",		if_entry},
  {"lambda",		lambda_entry},
  {"let",		let_entry},
  {"let*",		let_star_entry},
  {"letrec",		let_star_entry},
  {"or",		or_entry},
  {"quote",		quote_entry},
  {"set!",		set_entry},
  {NULL,                NULL}
};


static void unit_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR); /* the environment */
  bind_procs(state, proctab, syntab);
  RET0();
}

const label_t sc_unit_scsyntax = &unit_entry;
