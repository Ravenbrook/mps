/* scmm.c -- memory manager: allocator, garbage collector, etc.
 *
 * This is a simple copying garbage collector.
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"

#include <stddef.h>
#include <stdlib.h>


typedef obj_t (*fix_t)(state_t, obj_t);

static size_t obj_size(obj_t obj)
{
  switch(TYPE(obj)) {
  case TYPE_PROC:
    return ALIGN_UP(offsetof(proc_s, locs[obj->proc.regs]));
  case TYPE_PAIR:
    return ALIGN_UP(sizeof(pair_s));
  case TYPE_INTEGER:
    return ALIGN_UP(sizeof(integer_s));
  case TYPE_SPECIAL:
    return ALIGN_UP(sizeof(special_s));
  case TYPE_INPORT:
  case TYPE_OUTPORT:
    return ALIGN_UP(sizeof(port_s));
  case TYPE_CHARACTER:
    return ALIGN_UP(sizeof(character_s));
  case TYPE_FORWARD:
    return ALIGN_UP(sizeof(forward_s));
  case TYPE_EXCEPTION:
    return ALIGN_UP(sizeof(exception_s));
  case TYPE_SYMBOL:
    return ALIGN_UP(offsetof(symbol_s, string[obj->symbol.length + 1]));
  case TYPE_STRING:
    return ALIGN_UP(offsetof(string_s, string[obj->string.length + 1]));
  case TYPE_VECTOR:
    return ALIGN_UP(offsetof(vector_s, elements[VECLEN(obj)]));
  }

  ASSERT(0);		/* unknown object type */
#ifdef NDEBUG           /* avoids "never executed" warning */
(void)fprintf(stderr,
	        "\n*** GC discovered corrupt heap during forwarding.	"
	        "Object at 0x%lx has unknown type 0x%lX.\n",
	        (ulong)obj, (ulong)TYPE(obj));
  abort();
#endif
}

static obj_t obj_scan(state_t state, fix_t fix, obj_t obj)
{
  size_t size;

  switch(TYPE(obj)) {
  case TYPE_INTEGER:
    size = sizeof(integer_s);
    break;

  case TYPE_SPECIAL:
    size = sizeof(special_s);
    break;

  case TYPE_INPORT:
  case TYPE_OUTPORT:
    size = sizeof(port_s);
    break;

  case TYPE_CHARACTER:
    size = sizeof(character_s);
    break;

  case TYPE_SYMBOL:
    size = offsetof(symbol_s, string[obj->symbol.length + 1]);
    break;

  case TYPE_STRING:
    size = offsetof(string_s, string[obj->string.length + 1]);
    break;

  case TYPE_PROC:
    {
      size_t i, regs = obj->proc.regs;
      obj->proc.name = (*fix)(state, obj->proc.name);
      obj->proc.cont = (*fix)(state, obj->proc.cont);
      for(i = 0; i < regs; ++i)
	LOC(obj, i) = (*fix)(state, LOC(obj, i));
      size = offsetof(proc_s, locs[regs]);
    }
    break;

  case TYPE_PAIR:
    SETCAR(obj, (*fix)(state, CAR(obj)));
    SETCDR(obj, (*fix)(state, CDR(obj)));
    size = sizeof(pair_s);
    break;

  case TYPE_EXCEPTION:
    obj->exception.object = (*fix)(state, obj->exception.object);
    size = sizeof(exception_s);
    break;

  case TYPE_VECTOR:
    {
      veclen_t i, length = VECLEN(obj);
      for(i = 0; i < length; ++i)
	VECSET(obj, i, (*fix)(state, VECREF(obj, i)));
      size = offsetof(vector_s, elements[length]);
    }
    break;

  /* The scan function shouldn't be applied to forwarding */
  /* pointers, because they should only exist in from-space. */
  case TYPE_FORWARD:
    ASSERT(0);
    break;

  default:			/* unknown object type */
    ASSERT(0);
#ifdef NDEBUG                   /* avoids "never executed" warning */
    (void)fprintf(stderr,
	          "\n*** GC discovered corrupt heap during scanning.  "
	          "Object at 0x%lx has unknown type 0x%lX.\n",
	          (ulong)obj, (ulong)TYPE(obj));
    abort();
#endif
}

  return (obj_t)((ulong)obj + ALIGN_UP(size));
}

static void state_scan(state_t state, fix_t fix)
{
  size_t i;

  for(i = 0; i < ARRAYLEN(state->procs); ++i)
    state->procs[i] = (*fix)(state, state->procs[i]);

  state->here = (*fix)(state, state->here);
  state->cont = (*fix)(state, state->cont);
  state->argl = (*fix)(state, state->argl);

  for(i = 0; i < TEMPS; ++i)
    state->temp[i] = (*fix)(state, state->temp[i]);

  state->errproc = (*fix)(state, state->errproc);

  state->inport = (*fix)(state, state->inport);
  state->outport = (*fix)(state, state->outport);
  state->errport = (*fix)(state, state->errport);

  state->obj_symtab = (*fix)(state, state->obj_symtab);

  state->units = (*fix)(state, state->units);
}

static int is_dynamic(state_t state, obj_t obj)
{
  return state->heap_base <= (void *)obj && (void *)obj < state->heap_next;
}

static int is_static(obj_t obj)
{
  void *p = (void *)obj;
  return ((void *)static_symbols <= p &&
	  p < (void *)&static_symbols[static_symbols_length]) ||
	 ((void *)static_specials <= p &&
	  p < (void *)&static_specials[static_specials_length]);
}

static obj_t pointer_check(state_t state, obj_t obj)
{
  ASSERT(is_dynamic(state, obj) || is_static(obj));
  ASSERT(ALIGN_UP((ulong)obj) == (ulong)obj);
  ASSERT(!ISNTTYPE(TYPE(obj)));
  return obj;
}

extern void heap_check(state_t state)
{
  obj_t obj;

  if(state->inited)
    state_scan(state, pointer_check);

  obj = (obj_t)state->heap_base;
  while((void *)obj < state->heap_next) {
    (void)pointer_check(state, obj);
    obj = obj_scan(state, pointer_check, obj);
  }
}

extern void stats(state_t state, void *base, void *limit)
{
  obj_t obj;
  ulong total;
  size_t i;
  struct {type_t type; ulong count;} counts[] = {
    {TYPE_PROC, 0},
    {TYPE_SYMBOL, 0},
    {TYPE_PAIR, 0},
    {TYPE_INTEGER, 0},
    {TYPE_SPECIAL, 0},
    {TYPE_INPORT, 0},
    {TYPE_OUTPORT, 0},
    {TYPE_STRING, 0},
    {TYPE_VECTOR, 0},
    {TYPE_EXCEPTION, 0},
    {TYPE_CHARACTER, 0},
    {TYPE_FORWARD, 0},
  };
#ifdef AGE_STATS
  size_t ages[10] = {0, };
#endif

  total = 0;
  for(obj = base; (void *)obj < limit; obj = (obj_t)((ulong)obj + obj_size(obj))) {
#ifdef AGE_STATS
    size_t j;
    ulong age;
#endif
    for(i = 0; i < ARRAYLEN(counts); ++i) {
      if(TYPE(obj) == counts[i].type)
	++counts[i].count;
    }

#ifdef AGE_STATS
    age = state->step - obj->header.birthday;
    j = 0;
    while(age != 0) {
      age /= 10;
      ++j;
    }
    ASSERT(j < ARRAYLEN(ages));
    ++ages[j];
#endif

    ++total;
  }

  for(i = 0; i < ARRAYLEN(counts); ++i) {
    ulong j;
    (void)fprintf(stdout,
                  "stats %12s: %8lu (%2ld%%)  ",
	          type_name(counts[i].type),
	          counts[i].count,
	          counts[i].count * 100 / total);
    for(j = 0; j < counts[i].count * 50 / total; ++j)
      putchar('#');
    write_char(state, stdout, '\n');
  }

#ifdef AGE_STATS
  for(i = 0; i < ARRAYLEN(ages); ++i) {
    size_t j;
    (void)fprintf(stdout,
	          "age 10^%-2lu: %8lu (%2ld%%) ",
	          i,
	          ages[i],
	          ages[i] * 100 / total);
    for(j = 0; j < ages[i] * 50 / total; ++j)
      putchar('#');
    putchar('\n');
  }
#endif
}

static obj_t gc_fix(state_t state, obj_t obj)
{
  if(state->old_base <= (void *)obj && (void *)obj < state->old_limit)
    if(TYPE(obj) == TYPE_FORWARD) {
      obj = obj->forward.object;
      COUNT(GC_SNAP);
    } else {
      size_t size = obj_size(obj);
      obj_t copy = alloc(state, size);
      memcpy(copy, obj, size);
      TYPE(obj) = TYPE_FORWARD;
      obj->forward.object = copy;
      obj = copy;
      COUNT(GC_COPY);
    }
  else
    ASSERT(is_dynamic(state, obj) || is_static(obj));
  return obj;
}

extern void gc(state_t state, size_t size)
{
  static int in_gc = 0;
  obj_t obj;
  size_t old_size;
  mps_res_t res;
  void *p;

  UNUSED(size);

  ASSERT(state->inited);

  ASSERT(!in_gc);		/* Prevent recursive GC */
  in_gc = 1;

  (void)fprintf(stdout,
	        "gc start step %lu size %lub\n",
	        state->step,
	        (ulong)state->heap_next - (ulong)state->heap_base);

#ifdef GC_CHECKING
  heap_check(state);
#endif

#ifdef GC_STATS
  stats(state, state->heap_base, state->heap_next);
#endif

  state->old_base = state->heap_base;
  state->old_limit = state->heap_limit;
  old_size = state->heap_size;

  /* Simple growth policy: We allow twice the amount of space in use at */
  /* the last GC, provided that's more than the current usage. */
  /* @@@@ This means the heap never shrinks. */
  if(state->heap_last * 2 > state->heap_size) {
    state->heap_size = state->heap_last * 2;
    (void)fprintf(stdout, "gc new size %lub\n", state->heap_size);
  }

  res = mps_alloc(&p, state->pool, state->heap_size);
  if(res != MPS_RES_OK) {
    (void)fprintf(stdout, "Can't allocate new heap.\n");
    exit(EXIT_FAILURE);
  }
  state->heap_base = p;
  state->heap_next = state->heap_base;
  state->heap_limit = (void *)((ulong)state->heap_base + state->heap_size);

#ifdef GC_CHECKING
  memset(state->heap_base, 0x25, state->heap_size);	/* debugging help */
#endif

  /* Scan the roots */
  state_scan(state, gc_fix);

  /* Scan the to-space */
  obj = (obj_t)state->heap_base;
  while((void *)obj < state->heap_next)
    obj = obj_scan(state, gc_fix, obj);
  ASSERT(obj == state->heap_next);

  state->heap_last = (ulong)state->heap_next - (ulong)state->heap_base;

  /* Delete the from-space */
#ifdef GC_CHECKING
  memset(state->old_base, 0xF5, old_size);
#endif
  mps_free(state->pool, state->old_base, old_size);

#ifdef GC_CHECKING
  heap_check(state);
#endif

#ifdef GC_STATS
  stats(state, state->heap_base, state->heap_next);
#endif

  (void)fprintf(stdout, "gc end %lub\n", state->heap_last);

  in_gc = 0;

  COUNT(GC_GC);
}

extern obj_t alloc(state_t state, size_t size)
{
  void *after;
  obj_t obj;

  size = ALIGN_UP(size);

  after = (void *)((ulong)state->heap_next + size);

  if(after > state->heap_limit || after < state->heap_next) {
    gc(state, size);
    after = (void *)((ulong)state->heap_next + size);
    if(after > state->heap_limit || after < state->heap_next) {
      /* @@@@ Not very satisfactory outcome. */
      (void)fprintf(stdout, "GC failed to release enough memory\n");
      exit(EXIT_FAILURE);
    }
  }

  obj = (obj_t)state->heap_next;
  state->heap_next = after;
  state->heap_total += size;

  COUNT(GC_ALLOC);

  return obj;
}
