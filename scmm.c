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
#include "mps.h"
#include "mpscamc.h"

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
  if(!is_static(obj)) /* @@@@ because we had to align too much */
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

  res = mps_alloc(&p, state->state_pool, state->heap_size);
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
  mps_free(state->state_pool, state->old_base, old_size);

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


/* MPS format */

#define TYPE_PAD        ((type_t)0x21BEBADD)

typedef struct pad_s {
  header_s header;      /* @@@@ too big if AGE_STATS defined? */
  size_t size;
} pad_s;
  

/* Copied from <mps/version/1.105/manual/reference/index.html#mps_fmt_scan_t> */

static mps_res_t my_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  mps_res_t res;
  mps_addr_t next;

  MPS_SCAN_BEGIN(ss) {
    next = base;
    while(next < limit) {
      obj_t obj = (obj_t)next;
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
          res = MPS_FIX12(ss, (mps_addr_t *)&obj->proc.name);
          if(res != MPS_RES_OK) return res;
          res = MPS_FIX12(ss, (mps_addr_t *)&obj->proc.cont);
          if(res != MPS_RES_OK) return res;
          for(i = 0; i < regs; ++i) {
            res = MPS_FIX12(ss, (mps_addr_t *)&obj->proc.locs[i]);
            if(res != MPS_RES_OK) return res;
          }
          size = offsetof(proc_s, locs[regs]);
        }
        break;
    
      case TYPE_PAIR:
        res = MPS_FIX12(ss, (mps_addr_t *)&obj->pair.car);
        if(res != MPS_RES_OK) return res;
        res = MPS_FIX12(ss, (mps_addr_t *)&obj->pair.cdr);
        if(res != MPS_RES_OK) return res;
        size = sizeof(pair_s);
        break;
    
      case TYPE_EXCEPTION:
        res = MPS_FIX12(ss, (mps_addr_t *)&obj->exception.object);
        if(res != MPS_RES_OK) return res;
        size = sizeof(exception_s);
        break;
    
      case TYPE_VECTOR:
        {
          veclen_t i, length = VECLEN(obj);
          for(i = 0; i < length; ++i) {
            res = MPS_FIX12(ss, (mps_addr_t *)&obj->vector.elements[i]);
            if(res != MPS_RES_OK) return res;
          }
          size = offsetof(vector_s, elements[length]);
        }
        break;
    
      /* The scan function shouldn't be applied to forwarding */
      /* pointers, because they should only exist in from-space. */
      case TYPE_FORWARD:
        ASSERT(0);
        break;

      case TYPE_PAD:
        size = ((pad_s *)obj)->size;
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

      next = (mps_addr_t)((ulong)obj + ALIGN_UP(size));
    }
  } MPS_SCAN_END(ss);

  return MPS_RES_OK;
}

static size_t my_size(mps_addr_t base)
{
  obj_t obj = (obj_t)base;
  if(TYPE(obj) == TYPE_PAD)
    return ((pad_s *)obj)->size;
  else
    return obj_size(obj);
}

static mps_addr_t my_skip(mps_addr_t base)
{
  return (mps_addr_t)((ulong)base + my_size(base));
}

static void my_copy(mps_addr_t old, mps_addr_t new)
{
  size_t size = obj_size((obj_t)old);
  memcpy(new, old, size);
}

static void my_fwd(mps_addr_t old, mps_addr_t new)
{
  obj_t obj = (obj_t)old;
  TYPE(obj) = TYPE_FORWARD;
  obj->forward.object = (obj_t)new;
}

static mps_addr_t my_isfwd(mps_addr_t base)
{
  obj_t obj = (obj_t)base;
  if(TYPE(obj) == TYPE_FORWARD)
    return obj->forward.object;
  else
    return NULL;
}

static void my_pad(mps_addr_t base, size_t size)
{
  obj_t obj = (obj_t)base;
  ASSERT(size >= sizeof(header_s));     /* @@@@ what if AGE_STATS defined? */
  TYPE(obj) = TYPE_PAD;
  ((pad_s *)obj)->size = size;
}


/* Copied from <mps/version/1.105/manual/reference/index.html#mps_fmt_A_s> */

static mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_t my_format;
  mps_res_t res;
  mps_fmt_A_s my_format_A = {
    8, /* @@@@ */
    &my_scan,
    &my_skip,
    &my_copy,
    &my_fwd,
    &my_isfwd,
    &my_pad
  };

  res = mps_fmt_create_A(&my_format, arena, &my_format_A);
  if(res != MPS_RES_OK)
    return NULL;

  return my_format;
}

extern mps_pool_t create_pool(mps_arena_t arena)
{
  mps_pool_t pool;
  mps_fmt_t format;
  mps_res_t res;
  mps_chain_t chain;
  static mps_gen_param_s testChain[] = {
    { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };
  
  format = create_format(arena);
  if(format == NULL)
    return NULL;
  
  res = mps_chain_create(&chain,
                         arena,
                         sizeof(testChain) / sizeof(testChain[0]),
                         testChain);
  if(res != MPS_RES_OK)
    return NULL;

  res = mps_pool_create(&pool, arena, mps_class_amc(), format, chain);
  if(res != MPS_RES_OK)
    return NULL;

  return pool;
}

static void fail_alloc(void) __attribute__((noreturn));

static void fail_alloc(void)
{
  (void)fprintf(stderr, "Failed to allocate!\n");
  exit(EXIT_FAILURE);
}

extern void make_pair(state_t state)
{
  void *p;
  size_t size = ALIGN_UP(sizeof(pair_s));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_PAIR;
    obj->pair.car = obj_uninit;
    obj->pair.cdr = obj_uninit;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_PAIR);
}

extern void make_integer(state_t state)
{
  void *p;
  size_t size = ALIGN_UP(sizeof(integer_s));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_INTEGER;
    obj->integer.integer = 0;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_INTEGER);
}

extern void make_vector(state_t state, size_t length)
{
  void *p;
  size_t size = ALIGN_UP(offsetof(vector_s, elements[length]));
  do {
    obj_t obj;
    size_t i;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_VECTOR;
    obj->vector.length = length;
    for(i = 0; i < length; ++i)
      obj->vector.elements[i] = obj_uninit;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_VECTOR);
}

extern void make_symbol(state_t state, size_t length, char *string)
{
  void *p;
  size_t size = ALIGN_UP(offsetof(symbol_s, string[length + 1]));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_SYMBOL;
    obj->symbol.length = length;
    memcpy(obj->symbol.string, string, length + 1);
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_SYMBOL);
}

extern void make_string_uninit(state_t state, size_t length)
{
  void *p;
  size_t size = ALIGN_UP(offsetof(symbol_s, string[length + 1]));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_STRING;
    obj->string.length = length;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_STRING);
}

extern void make_proc_regs(state_t state, size_t length)
{
  void *p;
  size_t size = ALIGN_UP(offsetof(proc_s, locs[length]));
  do {
    obj_t obj;
    size_t i;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_PROC;
    obj->proc.name = obj_uninit;
    obj->proc.entry = &no_entry;
    obj->proc.cont = obj_uninit;
    obj->proc.cont = obj_uninit;
    obj->proc.read_only = 0;
    obj->proc.regs = length;
    for(i = 0; i < length; ++i)
      obj->proc.locs[i] = obj_uninit;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_PROC);
}

extern void make_special(state_t state, char *string)
{
  void *p;
  size_t size = ALIGN_UP(sizeof(special_s));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_SPECIAL;
    obj->special.name = string;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_SPECIAL);
}

extern void make_inport(state_t state, FILE *stream)
{
  void *p;
  size_t size = ALIGN_UP(sizeof(port_s));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_INPORT;
    obj->port.stream = stream;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_INPORT);
}

extern void make_outport(state_t state, FILE *stream)
{
  void *p;
  size_t size = ALIGN_UP(sizeof(port_s));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_OUTPORT;
    obj->port.stream = stream;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_OUTPORT);
}

extern void make_exception(state_t state)
{
  void *p;
  size_t size = ALIGN_UP(sizeof(exception_s));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_EXCEPTION;
    obj->exception.object = obj_uninit;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_EXCEPTION);
}

extern void make_character(state_t state, char c)
{
  void *p;
  size_t size = ALIGN_UP(sizeof(special_s));
  do {
    obj_t obj;
    mps_res_t res = mps_reserve(&p, state->obj_ap, size);
    if(res != MPS_RES_OK) fail_alloc();
    obj = p;
    obj->header.type = TYPE_CHARACTER;
    obj->character.c = c;
    state->baby = obj;
  } while(!mps_commit(state->obj_ap, p, size));
  COUNT(MAKE_CHARACTER);
}

static mps_res_t my_state_scan(mps_ss_t ss, void *p, size_t size)
{
  state_t state;
  size_t i;
  mps_res_t res;

  ASSERT(size == sizeof(state_s));

  state = p;

  MPS_SCAN_BEGIN(ss) {
  
    for(i = 0; i < ARRAYLEN(state->procs); ++i) {
      res = MPS_FIX12(ss, (mps_addr_t *)&state->procs[i]);
      if(res != MPS_RES_OK) goto fail_fix;
    }
  
    res = MPS_FIX12(ss, (mps_addr_t *)&state->here);
    if(res != MPS_RES_OK) goto fail_fix;
    res = MPS_FIX12(ss, (mps_addr_t *)&state->cont);
    if(res != MPS_RES_OK) goto fail_fix;
    res = MPS_FIX12(ss, (mps_addr_t *)&state->argl);
    if(res != MPS_RES_OK) goto fail_fix;
  
    for(i = 0; i < TEMPS; ++i) {
      res = MPS_FIX12(ss, (mps_addr_t *)&state->temp[i]);
      if(res != MPS_RES_OK) goto fail_fix;
    }
  
    res = MPS_FIX12(ss, (mps_addr_t *)&state->errproc);
    if(res != MPS_RES_OK) goto fail_fix;
  
    res = MPS_FIX12(ss, (mps_addr_t *)&state->inport);
    if(res != MPS_RES_OK) goto fail_fix;
    res = MPS_FIX12(ss, (mps_addr_t *)&state->outport);
    if(res != MPS_RES_OK) goto fail_fix;
    res = MPS_FIX12(ss, (mps_addr_t *)&state->errport);
    if(res != MPS_RES_OK) goto fail_fix;
  
    res = MPS_FIX12(ss, (mps_addr_t *)&state->obj_symtab);
    if(res != MPS_RES_OK) goto fail_fix;
  
    res = MPS_FIX12(ss, (mps_addr_t *)&state->units);
    if(res != MPS_RES_OK) goto fail_fix;

  } MPS_SCAN_END(ss);

  return MPS_RES_OK;

fail_fix:
  return res;
}

extern mps_root_t create_root(mps_arena_t arena, state_t state)
{
  mps_res_t res;
  mps_root_t root;

  /* @@@@ Anything other than (mps_rm_t)0 asserts out! */
  res = mps_root_create(&root, arena,
                        MPS_RANK_EXACT, (mps_rm_t)0,
                        my_state_scan, &state, sizeof(state));
  if(res != MPS_RES_OK)
    return NULL;

  return root;
}

