/* =========
 * Fix Forth
 * =========
 *
 * :Author: Richard Brooksby
 * :Date: 2023-05-16
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>


/* Abstract machine core
   ===================== */


/* Objects
 * -------
 *
 * An object is any structure with a prefix compatible with obj_s, the
 * first field of which is a pointer to a type object that describes
 * it.  Types, being objects, have a prefix compatible with obj_s
 * whose first field points to the type of types.
 */

typedef struct type_s *type_t;
typedef struct type_s {
  type_t type;          /* == &type_type */
  const char *name;     /* printable name of type */
} type_s;

typedef struct obj_s *obj_t;
typedef struct obj_s {
  type_t type;          /* object type */
} obj_s;

static type_s type_type = {
  &type_type,
  "type"
};


/* Abstract machine state objects
 *
 * A state is all that is required to run the abstract machine.  It is
 * equivalent to processor registers of a real machine.
 *
 * A state cannot be allocated on the garbage collected heap for two
 * reasons:
 *
 *   1. it is the root for garbage collection
 *
 *   2. the compiled C code must be able to make transfers between
 *      state fields freely and safely, without the risk of losing a
 *      root to an incremental GC, just as a real processor can
 *      transfer values between registers without hitting a memory
 *      protection barrier.
 *
 * To ensure that access to state fields can't be optimised away and
 * hidden from the GC, all state objects are volatile:
 *
 *   Since variables marked as volatile are prone to change outside
 *   the standard flow of code, the compiler has to perform every read
 *   and write to the variable as indicated by the code. Any access to
 *   volatile variables cannot be optimised away, e.g. by use of
 *   registers for storage of intermediate values.
 *
 *   -- Wikipedia
 */

#define STATE_NR 3

typedef volatile struct state_s *state_t;
typedef void (*entry_t)(state_t);
typedef struct state_s {
  type_t type;          /* == &type_state */
  obj_t rands;          /* operand stack, a list of objects */
  obj_t rators;         /* operator (return) stack, a list of closures */
  obj_t dictionary;     /* dictonary of words (environment) */
  entry_t pc;           /* program counter */
  obj_t reg[STATE_NR];  /* registers */
} state_s;

static struct type_s type_state = {
  &type_type,
  "state"
};

/* run -- run the abstract machine */

static void run(state_t state)
{
  for (;;)
    state->pc(state);
}


/* Special objects
 *
 * Special objects are singleton types used for various special
 * purposes.  They contain their own name -- their printed
 * representation.
 *
 * An example is the empty list, list_empty, printed "()".
 */

typedef struct special_s *special_t;
typedef struct special_s {
  type_t type;          /* == &type_special */
  const char *name;     /* printable name of special object */
} special_s;

static type_s type_special = {
  &type_type,
  "special"
};

static special_s list_empty = {
  &type_special,
  "()"
};


/* Pair objects, used to make Lisp-style lists */

typedef struct pair_s *pair_t;
typedef struct pair_s {
  type_t type;          /* == &type_pair */
  obj_t car;            /* left / head of list */
  obj_t cdr;            /* right / tail of list */
} pair_s;

static type_s type_pair = {
  &type_type,
  "pair"
};


/* Function objects */

typedef struct fun_s *fun_t;
typedef struct fun_s {
  type_t type;		/* == &type_fun */
  entry_t entry;        /* entry point of function code */
  obj_t closure;        /* whatever the function code needs */
} fun_s;

static type_s type_fun = {
  &type_type,
  "fun"
};

/* op_jump -- jump to a function
 *
 * Jumps to the function in register zero.  Register zero can then be
 * used by the function to access its own closure.
 */

static void op_jump(state_t state)
{
  assert(state->reg[0]->type == &type_fun);
  state->pc = ((fun_t)state->reg[0])->entry;
}

/* op_call -- call a function
 *
 * Calls the function in register zero.
 *
 * op_call's ``link`` argument is where execution should continue when
 * the function returns.
 *
 * Calling consists of constructing a continuation function that will
 * continue at ``link`` when called, and pushing it on the operator
 * stack for use by ``op_ret``, then jumping to the function.
 */

static void op_call(state_t state, entry_t link)
{
  state->reg[1] = malloc(sizeof(fun_s)); /* reserve */
  if (state->reg[1] == NULL) exit(99);
  ((fun_t)state->reg[1])->type = &type_fun;
  ((fun_t)state->reg[1])->entry = link;
  ((fun_t)state->reg[1])->closure = state->reg[0];
  /* commit */

  state->reg[2] = malloc(sizeof(pair_s)); /* reserve */
  if (state->reg[2] == NULL) exit(99);
  ((pair_t)state->reg[2])->type = &type_pair;
  ((pair_t)state->reg[2])->car = (obj_t)state->reg[1];
  ((pair_t)state->reg[2])->cdr = state->rators;
  /* commit */

  state->rators = state->reg[2];

  op_jump(state);
}

/* op_ret -- return from a function
 *
 * op_ret pops a function from the operator stack, presumably put
 * there by ``op_call``, and jumps to it.
 */

static void op_ret(state_t state)
{
  assert(state->rators != (obj_t)&list_empty);
  assert(state->rators->type == &type_pair);
  state->reg[0] = ((pair_t)state->rators)->car;
  state->rators = ((pair_t)state->rators)->cdr;
  assert(state->reg[0]->type == &type_fun);
  state->pc = ((fun_t)state->reg[0])->entry;
}


/* Operand stack */

/* op_push -- push a value on to the operand stack
 *
 * Pushes the contents of register 1 on to the operand stack by
 * prepending to the list.
 *
 * Corrupts register 2.
 */

static void op_push(state_t state)
{
  state->reg[2] = malloc(sizeof(pair_s)); /* reserve */
  if (state->reg[2] == NULL) exit(99);
  ((pair_t)state->reg[2])->type = &type_pair;
  ((pair_t)state->reg[2])->car = state->reg[1];
  ((pair_t)state->reg[2])->cdr = state->rands;
  state->rands = state->reg[2];
}

/* op_pop -- pop a value from the operand stack
 *
 * Pops the top value from the operand stack into register 1.
 *
 * FIXME: What about popping the empty stack?
 */

static void op_pop(state_t state)
{
  assert(state->rands->type == &type_pair);
  state->reg[1] = ((pair_t)state->rands)->car;
  state->rands = ((pair_t)state->rands)->cdr;
}


/* Character strings */

typedef struct string_s *string_t;
typedef struct string_s {
  type_t type;		/* == &type_string */
  size_t length;        /* length of c array */
  char c[1];            /* multibyte-encoded C string */
} string_s;

static type_s type_string = {
  &type_type,
  "string"
};


/* Print function */

static void print_entry(state_t state)
{
  op_pop(state);
  assert(state->reg[1]->type == &type_string);
  fputs(((string_t)state->reg[1])->c, stdout);
  op_ret(state);
}

static struct fun_s fun_print = {
  &type_fun,
  print_entry,
  (obj_t)&list_empty /* FIXME: should be a special unused value */
};


/* Exit continuations */

static void exit_entry(state_t state)
{
  (void)state;
  exit(EXIT_SUCCESS);
}

static void abort_entry(state_t state)
{
  (void)state;
  abort();
}


/* Make a state */

static void state_init(state_s *state)
{
  size_t i;
  state->type = &type_state;
  state->rands = (obj_t)&list_empty;
  state->rators = (obj_t)&list_empty;
  state->dictionary = (obj_t)&list_empty;
  state->pc = abort_entry;
  for (i = 0; i < sizeof(state->reg) / sizeof(state->reg[0]); ++i)
    state->reg[i] = NULL;
}  


/* Make a string
 *
 * Makes a string object out of a C string and pushes it on to the
 * operand stack.
 *
 * Corrupts register 1.
 */

static void make_string(state_t state, const char *s)
{
  size_t size = strlen(s) + 1;
  state->reg[1] = malloc(offsetof(string_s, c) + size);
  ((string_t)state->reg[1])->type = &type_string;
  ((string_t)state->reg[1])->length = size;
  memcpy(((string_t)state->reg[1])->c, s, size);
  op_push(state);
}  


int main(void)
{
  state_s state_s;
  state_t state;

  state_init(&state_s);
  state = &state_s;

  make_string(state, "Hello, world!\n");

  state->reg[0] = (obj_t)&fun_print;
  op_call(state, exit_entry);
  run(state);

  assert(0);
  return EXIT_FAILURE;
}
