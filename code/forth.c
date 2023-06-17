/* =========
 * Fix Forth
 * =========
 *
 * :Author: Richard Brooksby
 * :Date: 2023-05-16
 */

#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "testlib.h"

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

#define OBJ_ALIGN sizeof(obj_t)

typedef struct obj_s *obj_t;

typedef struct type_s *type_t;
typedef struct type_s {
  type_t type;          /* == &type_type */
  const char *name;     /* printable name of type */
  mps_res_t (*scan)(mps_ss_t, obj_t);
  mps_addr_t (*skip)(mps_addr_t);
} type_s;

typedef struct obj_s {
  type_t type;          /* object type */
} obj_s;

static mps_res_t obj_scan(mps_ss_t ss, obj_t obj)
{
  MPS_SCAN_BEGIN(ss) {
    mps_res_t res;
    mps_addr_t addr = obj->type;
    res = MPS_FIX12(ss, &addr);
    if (res != MPS_RES_OK) return res;
    obj->type = addr;
  } MPS_SCAN_END(ss);
  return MPS_RES_OK;
}

static mps_addr_t type_skip(mps_addr_t addr)
{
  return (char *)addr + sizeof(type_s);
}

static type_s type_type = {
  &type_type,
  "type",
  obj_scan, /* type_s has no other scannable fields */
  type_skip
};


/* Object format for MPS */

static mps_addr_t fmt_skip(mps_addr_t addr)
{
  return ((obj_t)addr)->type->skip(addr);
}

static mps_res_t fmt_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  while (base < limit) {
    mps_res_t res = ((obj_t)base)->type->scan(ss, base);
    if (res != MPS_RES_OK)
      return res;
    base = fmt_skip(base);
  }
  return MPS_RES_OK;
}

typedef struct fwd_s *fwd_t;
typedef struct fwd_s {
  type_t type;          /* == &type_fwd */
  mps_addr_t new;       /* where the object has been moved */
  size_t size;          /* size of this fwd object */
} fwd_s;

static mps_addr_t fwd_skip(mps_addr_t addr)
{
  return (char *)addr + ((fwd_t)addr)->size;
}

static type_s fwd_type = {
  &type_type,
  "fwd",
  obj_scan, /* fwd has no scannable fields */
  fwd_skip
};

typedef struct fwd2_s *fwd2_t;
typedef struct fwd2_s {
  type_t type;          /* == &type_fwd */
  mps_addr_t new;       /* where the object has been moved */
} fwd2_s;

static mps_addr_t fwd2_skip(mps_addr_t addr)
{
  return (char *)addr + sizeof(fwd2_s);
}

static type_s fwd2_type = {
  &type_type,
  "fwd2",
  obj_scan, /* fwd has no scannable fields */
  fwd2_skip
};

static void fmt_fwd(mps_addr_t old, mps_addr_t new)
{
  obj_t obj = old;
  mps_addr_t limit = fmt_skip(old);
  size_t size = (size_t)((char *)limit - (char *)old);
  assert(size >= alignUp(sizeof(fwd2_s), OBJ_ALIGN));
  if (size == alignUp(sizeof(fwd2_s), OBJ_ALIGN)) {
    obj->type = &fwd2_type;
    ((fwd2_t)obj)->new = new;
  } else {
    obj->type = &fwd_type;
    ((fwd_t)obj)->new = new;
    ((fwd_t)obj)->size = size;
  }
}

static mps_addr_t fmt_isfwd(mps_addr_t addr)
{
  obj_t obj = addr;
  if (obj->type == &fwd_type)
    return ((fwd_t)obj)->new;
  else if (obj->type == &fwd2_type)
    return ((fwd2_t)obj)->new;
  else
    return NULL;
}

typedef struct pad_s *pad_t;
typedef struct pad_s {
  type_t type;          /* == &type_pad */
  size_t size;          /* size of padding object */
} pad_s;

static mps_addr_t pad_skip(mps_addr_t addr)
{
  return (char *)addr + ((pad_t)addr)->size;
}

static type_s type_pad = {
  &type_type,
  "pad",
  obj_scan, /* padding does not have scannable fields */
  pad_skip
};

static void fmt_pad(mps_addr_t addr, size_t size)
{
  obj_t obj = addr;
  assert(size >= alignUp(sizeof(pad_s), OBJ_ALIGN));
  obj->type = &type_pad;
  ((pad_t)obj)->size = size;
}


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
 * hidden from the GC, all state objects should be volatile:
 *
 *   Since variables marked as volatile are prone to change outside
 *   the standard flow of code, the compiler has to perform every read
 *   and write to the variable as indicated by the code. Any access to
 *   volatile variables cannot be optimised away, e.g. by use of
 *   registers for storage of intermediate values.
 *
 *   -- Wikipedia
 *
 * FIXME: mps_reserve discards the volatile qualifier.
 */

#define STATE_NR 3

typedef /* volatile */ struct state_s *state_t;
typedef void (*entry_t)(state_t);
typedef struct state_s {
  type_t type;          /* == &type_state */
  obj_t rands;          /* operand stack, a list of objects */
  obj_t rators;         /* operator (return) stack, a list of closures */
  obj_t dictionary;     /* dictonary of words (environment) */
  entry_t pc;           /* program counter */
  mps_pool_t pool;      /* heap memory pool */
  mps_ap_t ap;          /* allocation point */
  void *baby;           /* newly-born object pointer TODO: explain purpose */
  obj_t reg[STATE_NR];  /* registers */
} state_s;

static mps_res_t state_scan(mps_ss_t ss, obj_t obj)
{
  state_t state = (state_t)obj;

#define FIX(ref) \
  do { \
    mps_addr_t _addr = (ref); \
    mps_res_t res = MPS_FIX12(ss, &_addr); \
    if (res != MPS_RES_OK) return res; \
    (ref) = _addr; \
  } while(0)

  MPS_SCAN_BEGIN(ss) {
    size_t i;
    FIX(state->type);
    FIX(state->rands);
    FIX(state->rators);
    FIX(state->dictionary);
    FIX(state->baby);
    for (i = 0; i < sizeof(state->reg) / sizeof(state->reg[0]); ++i)
      FIX(state->reg[i]);
  } MPS_SCAN_END(ss);

#undef FIX

  return MPS_RES_OK;
}

static mps_addr_t state_skip(mps_addr_t addr)
{
  return (char *)addr + sizeof(state_s);
}

static struct type_s type_state = {
  &type_type,
  "state",
  state_scan,
  state_skip
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

static mps_addr_t special_skip(mps_addr_t addr)
{
  return (char *)addr + sizeof(special_s);
}

static type_s type_special = {
  &type_type,
  "special",
  obj_scan, /* special_s has no other scannable fields */
  special_skip
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

static mps_res_t pair_scan(mps_ss_t ss, obj_t obj)
{
  pair_t pair = (pair_t)obj;
  
#define FIX(ref) \
  do { \
    mps_addr_t _addr = (ref); \
    mps_res_t res = MPS_FIX12(ss, &_addr); \
    if (res != MPS_RES_OK) return res; \
    (ref) = _addr; \
  } while(0)

  MPS_SCAN_BEGIN(ss) {
    FIX(pair->type);
    FIX(pair->car);
    FIX(pair->cdr);
  } MPS_SCAN_END(ss);

#undef FIX

  return MPS_RES_OK;
}

static mps_addr_t pair_skip(mps_addr_t addr)
{
  return (char *)addr + sizeof(pair_s);
}

static type_s type_pair = {
  &type_type,
  "pair",
  pair_scan,
  pair_skip
};


/* Function objects */

typedef struct fun_s *fun_t;
typedef struct fun_s {
  type_t type;		/* == &type_fun */
  entry_t entry;        /* entry point of function code */
  obj_t closure;        /* whatever the function code needs */
} fun_s;

static mps_res_t fun_scan(mps_ss_t ss, obj_t obj)
{
  fun_t fun = (fun_t)obj;

#define FIX(ref) \
  do { \
    mps_addr_t _addr = (ref); \
    mps_res_t res = MPS_FIX12(ss, &_addr); \
    if (res != MPS_RES_OK) return res; \
    (ref) = _addr; \
  } while(0)

  MPS_SCAN_BEGIN(ss) {
    FIX(fun->type);
    FIX(fun->closure);
  } MPS_SCAN_END(ss);

#undef FIX

  return MPS_RES_OK;
}

static mps_addr_t fun_skip(mps_addr_t addr)
{
  return (char *)addr + sizeof(fun_s);
}

static type_s type_fun = {
  &type_type,
  "fun",
  fun_scan,
  fun_skip
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
  do {
    die(mps_reserve(&state->baby, state->ap, sizeof(fun_s)),
        "op_call / mps_reserve(fun)");
    state->reg[1] = state->baby;
    ((fun_t)state->reg[1])->type = &type_fun;
    ((fun_t)state->reg[1])->entry = link;
    ((fun_t)state->reg[1])->closure = state->reg[0];
  } while (!mps_commit(state->ap, state->baby, sizeof(fun_s)));

  do {
    die(mps_reserve(&state->baby, state->ap, sizeof(pair_s)),
        "op_call / mps_reserve(pair)");
    state->reg[2] = state->baby;
    ((pair_t)state->reg[2])->type = &type_pair;
    ((pair_t)state->reg[2])->car = (obj_t)state->reg[1];
    ((pair_t)state->reg[2])->cdr = state->rators;
  } while (!mps_commit(state->ap, state->baby, sizeof(pair_s)));

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
  do {
    die(mps_reserve(&state->baby, state->ap, sizeof(pair_s)),
        "op_push / mps_reserve");
    state->reg[2] = state->baby;
    ((pair_t)state->reg[2])->type = &type_pair;
    ((pair_t)state->reg[2])->car = state->reg[1];
    ((pair_t)state->reg[2])->cdr = state->rands;
  } while(!mps_commit(state->ap, state->baby, sizeof(pair_s)));
  
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
  size_t n;             /* length of c array (including NUL) */
  char c[1];            /* multibyte-encoded C string */
} string_s;

static mps_addr_t string_skip(mps_addr_t addr)
{
  string_t string = (string_t)addr;
  size_t size = alignUp(offsetof(string_s, c) + string->n, OBJ_ALIGN);
  return (char *)addr + size;
}

static type_s type_string = {
  &type_type,
  "string",
  obj_scan, /* string_s has no other scannable fields */
  string_skip
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

static void state_init(state_s *state, mps_arena_t arena, mps_fmt_t fmt)
{
  size_t i;

  state->type = &type_state;
  state->rands = (obj_t)&list_empty;
  state->rators = (obj_t)&list_empty;
  state->dictionary = (obj_t)&list_empty;
  state->pc = abort_entry;
  for (i = 0; i < sizeof(state->reg) / sizeof(state->reg[0]); ++i)
    state->reg[i] = NULL;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
    die(mps_pool_create_k(&state->pool, arena, mps_class_amc(), args),
        "state_init / mps_pool_create_k");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    die(mps_ap_create_k(&state->ap, state->pool, args),
        "state_init / mps_ap_create_k");
  } MPS_ARGS_END(args);
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
  size_t length = strlen(s);
  size_t size = alignUp(offsetof(string_s, c) + length + 1, OBJ_ALIGN);

  do {
    die(mps_reserve(&state->baby, state->ap, size),
        "make_string / mps_reserve");
    state->reg[1] = state->baby;
    ((string_t)state->reg[1])->type = &type_string;
    ((string_t)state->reg[1])->n = length + 1; /* includes NUL */
    memcpy(((string_t)state->reg[1])->c, s, length + 1);
  } while(!mps_commit(state->ap, state->baby, size));

  op_push(state);
}  


int main(void)
{
  state_s state_s;
  state_t state;
  mps_arena_t arena;
  mps_fmt_t fmt;
  mps_root_t root;

  MPS_ARGS_BEGIN(args) {
    die(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
        "main / mps_arena_create_k");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, OBJ_ALIGN);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, fmt_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, fmt_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, fmt_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, fmt_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, fmt_pad);
    die(mps_fmt_create_k(&fmt, arena, args),
        "main / mps_fmt_create_k");
  } MPS_ARGS_END(args);

  state_init(&state_s, arena, fmt);
  state = &state_s;

  die(mps_root_create_fmt(&root,
                          arena,
                          mps_rank_exact(),
                          0,
                          fmt_scan,
                          &state,
                          &state + 1),
      "main / mps_root_create_fmt");

  make_string(state, "Hello, world!\n");

  state->reg[0] = (obj_t)&fun_print;
  op_call(state, exit_entry);
  run(state);

  assert(0);
  return EXIT_FAILURE;
}
