/*  impl.c.cbstest: COALESCING BLOCK STRUCTURE TEST
 *
 *  $HopeName: MMsrc!cbstest.c(MMdevel_gavinm_splay.2) $
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


SRCID(cbstest, "$HopeName: MMsrc!cbstest.c(MMdevel_gavinm_splay.2) $");

#define ArraySize ((size_t)123456)
#define nOperations ((size_t)125000)
#define SuccessRatio ((long)200) /* Ratio of failures attempted */
#define addr_of_index(i) ((mps_addr_t)((char *)block + (i)))
#define index_of_addr(a) ((long)((char *)(a) - (char *)block))
#define ErrorExit(message) MPS_BEGIN printf(message "\n"); exit(1); MPS_END

static BT alloc; /* the BT which we will use for alloc */
static Arena arena; /* the ANSI arena which we use to allocate the BT */
static CBSStruct cbs;
static void *block;
static long nAllocateTried, nAllocateSucceeded, nDeallocateTried,
  nDeallocateSucceeded;

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
  Bool isRes;

  ib = index_of_addr(base);
  il = index_of_addr(limit);

  isRes = BTIsResRange(alloc, ib, il);

  if(!isRes && random(SuccessRatio) != 0)
    return;

  nAllocateTried++;

  res = CBSDelete(&cbs, base, limit);

  if(res == ResOK) {
    if(!isRes)  
      ErrorExit("succeeded in deleting non-alloced block");
    else {
      nAllocateSucceeded++;
      BTSetRange(alloc, ib, il);
    }
  } else if(res == ResFAIL) {
    if(isRes)
      ErrorExit("failed to delete alloced block");
  } else {
    ErrorExit("Unexpected result");
  }
}

static void deallocate(mps_addr_t base, mps_addr_t limit) {
  mps_res_t res;
  long ib, il;
  Bool isSet;

  ib = index_of_addr(base);
  il = index_of_addr(limit);

  isSet = BTIsSetRange(alloc, ib, il);

  if(!isSet && random(SuccessRatio) != 0)
    return;

  nDeallocateTried++;

  res = CBSInsert(&cbs, base, limit);

  if(res == ResOK) {
    if(!isSet)  
      ErrorExit("succeeded in inserting non-free block");
    else {
      nDeallocateSucceeded++;
      BTResRange(alloc, ib, il);
    }
  } else if(res == ResFAIL) {
    if(isSet)
      ErrorExit("failed to insert free block");
  } else {
    ErrorExit("Unexpected result");
  }
}

extern int main(int argc, char *argv[])
{
  mps_res_t res;
  int i;
  mps_addr_t base, limit;

  testlib_unused(argc); testlib_unused(argv);

  nAllocateTried = nAllocateSucceeded = nDeallocateTried = 
    nDeallocateSucceeded = 0;

  res = mps_arena_create((mps_arena_t *)&arena,
                         mps_arena_class_an());
  if (res != MPS_RES_OK) 
    ErrorExit("failed to create ANSI arena.");
  res = BTCreate(&alloc, arena, ArraySize);
  if (res != MPS_RES_OK) 
    ErrorExit("failed to create bit table.");

  res = CBSInit(arena, &cbs, NULL, NULL, NULL, NULL, 0);
  if(res != MPS_RES_OK)
    ErrorExit("failed to initialise CBS.");

  BTSetRange(alloc, 0, ArraySize); /* Initially all allocated */
  /* We're not going to use this block, but I feel unhappy just */
  /* inventing addresses. */
  res = ArenaAlloc(&block, arena, ArraySize);
  if(res != MPS_RES_OK)
    ErrorExit("failed to allocate block");
  printf("Allocated block [%p, %p)\n", block, (char *)block + ArraySize);

  for(i = 0; i < nOperations; i++) {
    random_range(&base, &limit);
    if(random(2) == 1) {
      allocate(base, limit);
    } else {
      deallocate(base, limit);
    }
  }

  CBSDescribe(&cbs, mps_lib_get_stdout());

  printf("Number of allocations attempted: %ld\n", nAllocateTried);
  printf("Number of allocations succeeded: %ld\n", nAllocateSucceeded);
  printf("Number of deallocations attempted: %ld\n", nDeallocateTried);
  printf("Number of deallocations succeeded: %ld\n", nDeallocateSucceeded);
  printf("\nNo problems detected.\n");
  return 0;
}
