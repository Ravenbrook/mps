#include <stdlib.h>  /* for malloc */
#include <stdio.h>  /* for printf */

#include "mps.h"
#include "mpsacl.h"   /* for mps_arena_class_cl */
#include "mpscamc.h"   /* for mps_class_amc */

mps_res_t res;

static void checkres(char *text)
{
  printf("%s -- res: %d\n", text, (int)res);
  if(res != MPS_RES_OK) {
    exit(1);
  }
}

static void nyi(char *text)
{
  printf("Not Yet Implemented: %s\n", text);
  exit(1);
}

static void error(char *text)
{
  printf("ERROR: %s, res=%d\n", text, res);
  exit(1);
}

/* XF -- an example format
 *
 * Our format must allow:
 *   - our objects (aka "client objects");
 *   - padding objects (for MPS use);
 *   - forwarding objects (for MPS use).
 */

enum {
  xf_typecode_atom = 'a',
  xf_typecode_vect = 'v',
  xf_typecode_pad =  'p',
  xf_typecode_fwdShort = 's',
  xf_typecode_fwdLong  = 'l'
};

/* Object: the common part of all objects */
typedef struct xf_object_s {
  uint32_t typecode;
  /* ... will have further bytes ... */
} *xf_object;

/* Atom: a single scalar value */
typedef struct xf_atom_s {
  uint32_t typecode;
  uint32_t value;
} *xf_atom;

/* Vector: a sequence of references to other objects */
typedef struct xf_vect_s {
  uint32_t typecode;
  uint32_t level;
  void *refs[1000];
} *xf_vect;

/* Pad: we must be able to create a pad of any valid length.
 * We must also be able to turn any client object into a pad.
 */
typedef struct xf_pad_s {
  uint32_t typecode;
  uint32_t size;  /* overall length of pad, in bytes */
  /* ... 0 or more additional bytes ... */
} *xf_pad;

/* Fwd: we must be able to turn any client object into a fwd.
 * We need two types: fwdShort or fwdLong.
 */
typedef struct xf_fwdShort_s {
  uint32_t typecode;
  mps_addr_t fwd_pointer;
} *xf_fwdShort;
typedef struct xf_fwdLong_s {
  uint32_t typecode;
  mps_addr_t fwd_pointer;
  uint32_t size;  /* overall length of fwd, in bytes */
  /* ... 0 or more additional bytes ... */
} *xf_fwdLong;

#define XF_ALIGN 8
mps_align_t XF_align = XF_ALIGN;

static mps_res_t XF_scan(mps_ss_t ss, mps_addr_t a1, mps_addr_t a2)
{
  nyi("XF_scan");
}

static mps_addr_t XF_skip(mps_addr_t a1)
{
  xf_object obj = (xf_object)a1;
  uint32_t size = 0;
  
  switch(obj->typecode) {
    case xf_typecode_atom: {
      size = sizeof(struct xf_atom_s);
      break;
    }
    case xf_typecode_vect: {
      size = sizeof(struct xf_vect_s);
      break;
    }
    case xf_typecode_pad: {
      xf_pad pad = (xf_pad)a1;
      size = pad->size;
      break;
    }
    case xf_typecode_fwdShort: {
      size = sizeof(struct xf_fwdShort_s);
      break;
    }
    case xf_typecode_fwdLong: {
      xf_fwdLong fwdLong = (xf_fwdLong)a1;
      size = fwdLong->size;
      break;
    }
    default: {
      error("XF_skip: unrecognised typecode");
      break;
    }
  }
  return (char *)a1 + size;
}

static void XF_copy(mps_addr_t a1, mps_addr_t a2)
{
  nyi("XF_copy");
}

static void XF_fwd(mps_addr_t a1, mps_addr_t a2)
{
  nyi("XF_fwd");
}

static mps_addr_t XF_isfwd(mps_addr_t a1)
{
  return NULL;
}

static void XF_pad(mps_addr_t a1, size_t size)
{
  xf_pad p = (xf_pad)a1;
  
  p->typecode = xf_typecode_pad;
  p->size = size;
}

static mps_addr_t XF_mps_class(mps_addr_t a1)
{
  return NULL;
}

mps_fmt_B_s fmt_B_parts = {
  XF_ALIGN,
  XF_scan,
  XF_skip,
  XF_copy,
  XF_fwd,
  XF_isfwd,
  XF_pad,
  XF_mps_class
};

static xf_atom make_atom(mps_ap_t ap)
{
  xf_atom neo = NULL;
  void *p;
  size_t cb = sizeof(struct xf_atom_s);
  do {
    res = mps_reserve(&p, ap, cb);
    if(res != MPS_RES_OK)
      goto fail_Reserve;
    neo = p;
  
    /* initialize new object */
    neo->typecode = xf_typecode_atom;
    neo->value = 0;
  } while(!mps_commit(ap, p, cb));
  
  return neo;

fail_Reserve:
  /* no more memory */
  error("make_atom: mps_reserve failed");
  return NULL;
}

static show_usage(mps_arena_t arena)
{
  size_t size = mps_arena_committed(arena);
  printf("Usage: committed memory: %X.\n", size);
}

struct mps_gen_param_s gens[] = {
  /* Generation 0 */
  { 10  /* x 1024 bytes (!) */,
    0.85  /* % mortality */
  }
};
#define GEN_COUNT 1

int main(int argc, char **argv)
{
  void *pBlock = NULL;
  size_t cbBlock = 4 * 1024 * 1024;
  
  mps_arena_t arena;
  mps_fmt_t fmt;
  mps_chain_t chain;
  mps_pool_t pool;
  mps_ap_t ap;
  xf_atom atom;
  
  /* arena */
  pBlock = malloc(cbBlock);
  res = mps_arena_create(&arena, mps_arena_class_cl(), cbBlock, pBlock);
  checkres("mps_arena_create");
  
  /* declare stack and regs as ambiguous roots */
  
  /* format */
  res = mps_fmt_create_B(&fmt, arena, &fmt_B_parts);
  checkres("mps_fmt_create_B");
  
  /* chain */
  res = mps_chain_create(&chain, arena, GEN_COUNT, gens);
  checkres("mps_chain_create");
  
  /* pool */
  res = mps_pool_create(&pool, arena, mps_class_amc(), fmt, chain);
  checkres("mps_pool_create");
  
  /* ap */
  res = mps_ap_create(&ap, pool);
  checkres("mps_ap_create");

  /* make some stuff */
  show_usage(arena);
  {
    int i;
    for(i = 0; i < 1000; i += 1) {
      atom = make_atom(ap);
      atom->value = i;
    }
    printf("Created atom at address: %p.\n", (void*)atom);
  }
  show_usage(arena);

  /* try a collection! */
  res = mps_arena_collect(arena);
  checkres("mps_arena_collect");

  /* tidy */
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);
  mps_arena_destroy(arena);
    
  free(pBlock);
  return 0;
}
