/* screp.c -- read-eval-print loop
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"


/* read-eval-print -- interactive read-eval-print loop
 *
 * (read-eval-print <env>)
 *
 * L0	the environment
 * L1	the input port
 * L2	the output port
 * L3	the error port
 * L4	form to evaluate / result to print
 * L5	the previous error handler
 */

static void rep_entry(state_t);
static void rep_read(state_t);
static void rep_eval(state_t);
static void rep_print(state_t);
static void rep_error(state_t);

static void rep_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR);
  ENTER(6);
  L0 = A0;			/* environment */
  /* @@@@ The ports used to go in locals because they were args. */
  /* Do we need to make copies of them any more?  What effect would */
  /* with-output-port etc. have on read-eval-print otherwise? */
  L1 = state->inport;		/* input port */
  L2 = state->outport;		/* output port */
  L3 = state->errport;		/* error port */

  if(INPORTSTREAM(L1) == NULL ||
     OUTPORTSTREAM(L2) == NULL ||
     OUTPORTSTREAM(L3) == NULL)
    error(state, "ports must all be open");

  /* Install error handler. */
  L5 = state->errproc;
  DUP_PROC(state->errproc, state->here);
  state->errproc->proc.entry = rep_error;
  state->errproc->proc.read_only = 1;

  JUMP(rep_read);
}

/* Print the prompt and read an input form. */

static void rep_read(state_t state)
{
/*  write_formatted(state, INPORTSTREAM(L2), "%lu> ", state->step); */
  CALL1(proc_read, L1, rep_eval);
}

/* If there was an input form, evaluate it. */

static void rep_eval(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from "read" */
  if(A0 == obj_eof) {
    state->errproc = L5;	/* uninstall the error handler */
    LEAVE();
    RET0();
  }
  L4 = A0;
  CALL2(proc_eval, L4, L0, rep_print);
}

/* Print the result of evaluation and go back. */

static void rep_print(state_t state)
{
  ulong i;

  L4 = ARGL;			/* the results (usually one) */
  i = 0;
  while(TYPE(L4) == TYPE_PAIR) {
    write_formatted(state, OUTPORTSTREAM(L2), "=%lu> ", i);
    write_obj(state, CAR(L4), WRITE_DEPTH, OUTPORTSTREAM(L2));
    write_char(state, OUTPORTSTREAM(L2), '\n');
    ++i;
    L4 = CDR(L4);
  }
  ASSERT(L4 == obj_empty);

  JUMP(rep_read);
}

/* Handle an error by printing the message and going back to */
/* the beginning of the loop. */

#ifdef REP_BACKTRACE
static void backtrace(state_t state, obj_t cont, FILE *stream)
{
  ASSERT(TYPE(cont) == TYPE_PROC);
  while(cont->proc.cont != cont) {
    write_string(state, stream, "backtrace ");
    write_string(state, stream, SYMSTR(cont->proc.name)); /* @@@@ assumes symbol not funny */
    if(cont->proc.name == sym_eval || cont->proc.name == sym_define) {
      write_char(state, stream, ' ');
      write_obj(state, LOC(cont, 1), WRITE_DEPTH, stream);
    }
    write_char(state, stream, '\n');
    cont = cont->proc.cont;
  }
}
#endif /* REP_BACKTRACE */

static void rep_error(state_t state)
{
  FILE *stream;

  /* @@@@ Can't really call check_args here because of recursion */
  /* problems if there's an error.  "catch" avoids this by */
  /* deinstalling the handler.  Could do the same? */
  ASSERT(ARGS == 2);

#if 0
  /* Discard the rest of the current input line. */
  {
    int c;
    do
      c = read_char(state, INPORTSTREAM(L1));
    until(c == EOF || c == '\n');
  }
#endif

  stream = OUTPORTSTREAM(L2);

  write_string(state, stream, "exception> ");
  write_obj(state, A0, WRITE_DEPTH, stream);
  write_char(state, stream, '\n');

#ifdef REP_BACKTRACE
  backtrace(state, A1, stream);
#endif /* REP_BACKTRACE */

  JUMP(rep_read);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"read-eval-print",		rep_entry},
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


const label_t sc_unit_screp = &unit_entry;
