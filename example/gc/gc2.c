
#include <stdlib.h>  /* for malloc */
#include <stdio.h>  /* for printf */

#include "mps.h"
#include "mpsacl.h"   /* for mps_arena_class_cl */
#include "mpscamc.h"   /* for mps_class_amc */

mps_res_t res;


static void outres(char *text)
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
 */

enum {
  xf_type_atom = 'a',
  xf_type_apad = 'p',
  xf_type_vect = 'V',
  xf_type_vpad = 'P',
  xf_type_LAST = 100000  /* not a type */
};

enum {
  xf_value_atom = 'a',
  xf_value_apad = ('a'<<24) + ('p'<<16) + ('a'<<8) + 'd',
  xf_value_vect = 'V',
  xf_value_vpad = ('V'<<24) + ('P'<<16) + ('A'<<8) + 'D',
  xf_value_LAST = 100000  /* not a type */
};

typedef struct xf_atom_s {
  uint32_t typecode;
  uint32_t value;
} *xf_atom;

typedef struct xf_vect_s {
  uint32_t typecode;
  uint32_t size;  /* overall, in bytes */
  uint8_t bytes[1];
} *xf_vect;

#define XF_ALIGN 8
mps_align_t XF_align = XF_ALIGN;

static mps_res_t XF_scan(mps_ss_t ss, mps_addr_t a1, mps_addr_t a2)
{
  return MPS_RES_OK;
}

static mps_addr_t XF_skip(mps_addr_t a1)
{
  return (char *)a1 + XF_align;
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
  if(size == 8) {
    xf_atom a = (xf_atom)a1;
    a->typecode = xf_type_apad;
    a->value = xf_value_apad;
  } else {
    xf_vect v = (xf_vect)a1;
    if(size <= 9) {
      error("XF_pad: wrong size to pad (too small)");
    }
    v->typecode = xf_type_vpad;
    v->size = size;
  }
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
    outres("mps_reserve");
    if(res != MPS_RES_OK)
      goto fail_Reserve;
    neo = p;
  
    /* initialize new object */
    neo->typecode = 0;
    neo->value = 0;
  } while(!mps_commit(ap, p, cb));
  
  return neo;

fail_Reserve:
  /* no more memory */
  error("make_atom: mps_reserve failed");
  return NULL;
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
  outres("mps_arena_create");
  
  /* declare stack and regs as ambiguous roots */
  
  /* format */
  res = mps_fmt_create_B(&fmt, arena, &fmt_B_parts);
  outres("mps_fmt_create_B");
  
  /* chain */
  res = mps_chain_create(&chain, arena, GEN_COUNT, gens);
  outres("mps_chain_create");
  
  /* pool */
  res = mps_pool_create(&pool, arena, mps_class_amc(), fmt, chain);
  outres("mps_pool_create");
  
  /* ap */
  res = mps_ap_create(&ap, pool);
  outres("mps_ap_create");

  /* make some stuff */
  atom = make_atom(ap);
  printf("Created atom at address: %p.\n", (void*)atom);

  /* tidy */
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);
  mps_arena_destroy(arena);
    
  free(pBlock);
  printf("hello, world!\n");
  return 0;
}
