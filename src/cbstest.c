/*  impl.c.cbstest: COALESCING BLOCK STRUCTURE TEST
 *
 *  $HopeName$
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#include "mpm.h"
#include "mps.h"
#include "mpsaan.h" /* ANSI arena for BTCreate and BTDestroy */
#include "testlib.h"

#ifdef MPS_OS_SU
#include "ossu.h"
#endif /* MPS_OS_SU */


SRCID(cbstest, "$HopeName$");

#define ArraySize ((size_t)4096)
#define nOperations ((size_t)10000)
#define addr_of_index(i) ((mps_addr_t)((char *)block + (i)))
#define index_of_addr(a) ((long)((char *)(a) - (char *)block))
#define ERROR(message) MPS_BEGIN printf(message "\n"); exit(1); MPS_END

static BT alloc; /* the BT which we will use for alloc */
static Arena arena; /* the ANSI arena which we use to allocate the BT */
static CBSRootStruct cbs;
static void *block;

/* Not very uniform, but never mind. */
static long random(long limit) {
  return rnd() % limit;
}

static void random_range(mps_addr_t *base_return, mps_addr_t *limit_return) {
  long i1, i2;

  i1 = random(ArraySize);
  do {
    i2 = random(ArraySize);
  } while (i1 == i2);

  if(i1 > i2) {
    *base_return = addr_of_index(i2);
    *limit_return = addr_of_index(i1);
  } else {
    *base_return = addr_of_index(i1);
    *limit_return = addr_of_index(i2);
  }
}

static void allocate(mps_addr_t base, mps_addr_t limit) {
  mps_res_t res;
  long ib, il;

  ib = index_of_addr(base);
  il = index_of_addr(limit);

  res = CBSDelete(&cbs, base, limit);

  if(res == ResOK) {
    printf("> allocate %p%p\n", base, limit);
    if(!BTIsResRange(alloc, ib, il))  
      ERROR("succeeded in deleting non-alloced block");
    else
      BTSetRange(alloc, ib, il);
  } else if(res == ResFAIL) {
    if(BTIsResRange(alloc, ib, il))
      ERROR("failed to delete alloced block");
  } else {
    ERROR("Unexpected result");
  }
}

static void deallocate(mps_addr_t base, mps_addr_t limit) {
  mps_res_t res;
  long ib, il;

  ib = index_of_addr(base);
  il = index_of_addr(limit);

  res = CBSInsert(&cbs, base, limit);

  if(res == ResOK) {
    printf("> deallocate %p%p\n", base, limit);
    if(!BTIsSetRange(alloc, ib, il))  
      ERROR("succeeded in inserting non-free block");
    else
      BTResRange(alloc, ib, il);
  } else if(res == ResFAIL) {
    if(BTIsSetRange(alloc, ib, il))
      ERROR("failed to insert free block");
  } else {
    ERROR("Unexpected result");
  }
}

extern int main(int argc, char *argv[])
{
  mps_res_t res;
  int i;
  mps_addr_t base, limit;

  testlib_unused(argc); testlib_unused(argv);

  res = mps_arena_create((mps_arena_t *)&arena,
                         mps_arena_class_an());
  if (res != MPS_RES_OK) 
    ERROR("failed to create ANSI arena.");
  res = BTCreate(&alloc, arena, ArraySize);
  if (res != MPS_RES_OK) 
    ERROR("failed to create bit table.");

  res = CBSRootInit(arena, &cbs, NULL, NULL, NULL, NULL, 0);
  if(res != MPS_RES_OK)
    ERROR("failed to initialise CBS.");

  BTSetRange(alloc, 0, ArraySize); /* Initially all allocated */
  /* We're not going to use this block, but I feel unhappy just */
  /* inventing addresses. */
  res = ArenaAlloc(&block, arena, ArraySize);
  if(res != MPS_RES_OK)
    ERROR("failed to allocate block");
  printf("block %p %p\n", block, (char *)block + ArraySize);

  for(i = 0; i < nOperations; i++) {
    random_range(&base, &limit);
    if(random(2) == 1) {
      allocate(base, limit);
    } else {
      deallocate(base, limit);
    }
    if(i % (nOperations / 10) == 0)
      CBSDescribe(&cbs, mps_lib_get_stdout());
  }

  return 0;
}
