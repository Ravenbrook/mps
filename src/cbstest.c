/*  impl.c.cbstest: COALESCING BLOCK STRUCTURE TEST
 *
 *  $HopeName: MMsrc!cbstest.c(MMdevel_mv2_rework.1) $
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


SRCID(cbstest, "$HopeName: MMsrc!cbstest.c(MMdevel_mv2_rework.1) $");

#define ArraySize ((Size)123456)
#define NOperations ((Size)125000)
#define MinSize ((Size)120) /* Arbitrary size */

#define AddrOfIndex(block, i) AddrAdd(block, i)
#define IndexOfAddr(block, a) AddrOffset(block, a)

static Count NAllocateTried, NAllocateSucceeded, NDeallocateTried,
  NDeallocateSucceeded, NNewBlocks;


/* These globals are used to communicate with the new and delete */
/* callbacks.  The first two booleans indicate which (if either) */
/* may be called.  The third boolean indicates whether the block */
/* being deleted will exist.  The base-limit pair is the right */
/* size of the new block */
static Bool ShouldCallDelete = FALSE;
static Bool ShouldCallNew = FALSE;
static Bool CallbackExists = FALSE;
static Addr CallbackBase, CallbackLimit;

typedef struct CheckCBSClosureStruct {
  BT allocTable;
  Addr base;
  Addr limit;
  Addr oldLimit;
} CheckCBSClosureStruct, *CheckCBSClosure;


static void cbsNewCallback(CBS cbs, CBSBlock cbsBlock) {
  AVERT(CBS, cbs);
  AVERT(CBSBlock, cbsBlock);
  AVER(ShouldCallNew);

  AVER(CBSBlockSize(cbsBlock) >= MinSize);
  AVER(CBSBlockBase(cbsBlock) == CallbackBase);
  AVER(CBSBlockLimit(cbsBlock) == CallbackLimit);

  ShouldCallNew = FALSE;
  NNewBlocks++;
}

static void cbsDeleteCallback(CBS cbs, CBSBlock cbsBlock) {
  AVERT(CBS, cbs);
  AVER(ShouldCallDelete);
  AVER(CallbackExists == CBSBlockExists(cbsBlock));
  if(CallbackExists) {
    AVERT(CBSBlock, cbsBlock);
    AVER(CBSBlockSize(cbsBlock) < MinSize);
    AVER(CBSBlockBase(cbsBlock) == CallbackBase);
    AVER(CBSBlockLimit(cbsBlock) == CallbackLimit);
  }

  ShouldCallDelete = FALSE;
  NNewBlocks++;
}

static Bool checkCBSAction(CBS cbs, CBSBlock cbsBlock,
                           void *p, unsigned long s) {
  Addr base, limit;
  CheckCBSClosure closure = (CheckCBSClosure)p;

  /* Don't need to check cbs every time */
  UNUSED(cbs);
  AVER(closure != NULL);
  AVER(s == 0);

  base = CBSBlockBase(cbsBlock);
  limit = CBSBlockLimit(cbsBlock);

  if(base > closure->oldLimit) {
    AVER(BTIsSetRange(closure->allocTable, 
                      IndexOfAddr(closure->base, closure->oldLimit), 
                      IndexOfAddr(closure->base, base)));
  } else { /* must be at start of table */
    AVER(base == closure->oldLimit);
    AVER(closure->oldLimit == closure->base);
  }
  
  AVER(BTIsResRange(closure->allocTable, 
                    IndexOfAddr(closure->base, base), 
                    IndexOfAddr(closure->base, limit)));


  closure->oldLimit = limit;

  return TRUE;
}

static void checkCBS(CBS cbs, BT allocTable, Addr dummyBlock) {
  CheckCBSClosureStruct closure;

  closure.allocTable = allocTable;
  closure.base = dummyBlock;
  closure.limit = AddrOfIndex(closure.base, ArraySize);
  closure.oldLimit = closure.base;

  CBSIterate(cbs, checkCBSAction, (void *)&closure, (unsigned long)0);

  if(closure.oldLimit == closure.base)
    AVER(BTIsSetRange(allocTable, 0, 
                      IndexOfAddr(dummyBlock, closure.limit)));
  else if(closure.limit > closure.oldLimit)
    AVER(BTIsSetRange(allocTable, 
                      IndexOfAddr(dummyBlock, closure.oldLimit),
                      IndexOfAddr(dummyBlock, closure.limit)));
  else
    AVER(closure.oldLimit == closure.limit);
}

/* Not very uniform, but never mind. */
static Word random(Word limit) {
  return (Word)rnd() % limit;
}


/* nextEdge -- Finds the next transition in the bit table 
 *
 * Returns the index greater than <base> such that the
 * range [<base>, <return>) has the same value in the bit table,
 * and <return> has a different value or does not exist.
 */

static Index nextEdge(BT bt, Size size, Index base) {
  Index end;
  Bool baseValue;

  AVER(bt != NULL);
  AVER(base < size);

  baseValue = BTGet(bt, base);

  for(end = base + 1; end < size && BTGet(bt, end) == baseValue; end++) 
    NOOP;

  return end;
}


/* lastEdge -- Finds the previous transition in the bit table 
 *
 * Returns the index less than <base> such that the range
 * [<return>, <base>] has the same value in the bit table,
 * and <return>-1 has a different value or does not exist.
 */

static Index lastEdge(BT bt, Size size, Index base) {
  Index end;
  Bool baseValue;

  AVER(bt != NULL);
  AVER(base < size);

  baseValue = BTGet(bt, base);

  for(end = base; end > (Index)0 && BTGet(bt, end - 1) == baseValue; end--) 
    NOOP;

  return end;
}


/* randomRange -- picks random range within table
 *
 * The function first picks a uniformly distributed <base> within the table.
 *
 * It then scans forward a binary exponentially distributed
 * number of "edges" in the table (that is, transitions between set and 
 * reset) to get <end>.  Note that there is a 50% chance that <end> will 
 * be the next edge, a 25% chance it will be the edge after, etc., until
 * the end of the table.
 *
 * Finally it picks a <limit> uniformly distributed in the range
 * [base+1, limit].
 *
 * Hence there is a somewhat better than 50% chance that the range will be
 * all either set or reset.
 */
 
static void randomRange(Addr *baseReturn, 
                        Addr *limitReturn,
                        BT allocTable,
                        Addr block) {
  Index base;   /* the start of our range */
  Index end;    /* an edge (i.e. different from its predecessor) */
                /* after base */
  Index limit;  /* a randomly chosen value in (base, limit]. */

  base = random(ArraySize);

  do {
    end = nextEdge(allocTable, ArraySize, base);
  } while(end < ArraySize && random(2) == 0); /* p=0.5 exponential */

  AVER(end > base);

  limit = base + 1 + random(end - base);

  *baseReturn = AddrOfIndex(block, base);
  *limitReturn = AddrOfIndex(block, limit);
}


/* expect_* -- Set callback expectations */

static void expect_nothing(void) {
  AVER(!ShouldCallNew);
  AVER(!ShouldCallDelete);
}

static void expect_new(Addr base, Addr limit) {
  AVER(!ShouldCallNew);
  AVER(!ShouldCallDelete);

  ShouldCallNew = TRUE;
  CallbackBase = base;
  CallbackLimit = limit;
}

static void expect_delete(Addr base, Addr limit) {
  AVER(!ShouldCallNew);
  AVER(!ShouldCallDelete);

  ShouldCallDelete = TRUE;
  CallbackExists = TRUE;
  CallbackBase = base;
  CallbackLimit = limit;
}

static void expect_delete_entirely(void) {
  AVER(!ShouldCallNew);
  AVER(!ShouldCallDelete);

  ShouldCallDelete = TRUE;
  CallbackExists = FALSE;
}

static void allocate(CBS cbs, Addr block, BT allocTable, 
                     Addr base, Addr limit) {
  Res res;
  Index ib, il;                  /* Indexed for base and limit */
  Bool isFree;

  ib = IndexOfAddr(block, base);
  il = IndexOfAddr(block, limit);

  isFree = BTIsResRange(allocTable, ib, il);
  
  /*
  printf("allocate: [%p, %p) -- %s\n", 
         base, limit, isFree ? "succeed" : "fail");
  */

  NAllocateTried++;

  if(isFree) {
    Addr outerBase, outerLimit;    /* interval containing [ib, il) */
    Size left, right, total;       /* Sizes of block and two fragments */

    outerBase = 
      AddrOfIndex(block, lastEdge(allocTable, ArraySize, ib));
    outerLimit = 
      AddrOfIndex(block, nextEdge(allocTable, ArraySize, il - 1));

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* based on detailed knowledge of CBS behaviour */
    if(total >= MinSize && left < MinSize && right < MinSize) {
      if(left == (Size)0 && right == (Size)0) {
        expect_delete_entirely();
      } else if(left >= right) {
        expect_delete(outerBase, base);
      } else {
        expect_delete(limit, outerLimit);
      }
    } else if(left >= MinSize && right >= MinSize) {
      if(left >= right) {
        expect_new(limit, outerLimit);
      } else {
        expect_new(outerBase, base);
      }
    } else {
      expect_nothing();
    }
  }

  res = CBSDelete(cbs, base, limit);

  if(!isFree) {
    die_expect((mps_res_t)res, MPS_RES_FAIL, 
               "Succeeded in deleting allocated block");
  } else { /* isFree */
    die_expect((mps_res_t)res, MPS_RES_OK, 
               "failed to delete free block");
    NAllocateSucceeded++;
    BTSetRange(allocTable, ib, il);
  }
}

static void deallocate(CBS cbs, Addr block, BT allocTable,
                       Addr base, Addr limit) {
  Res res;
  Index ib, il;
  Bool isAllocated;

  ib = IndexOfAddr(block, base);
  il = IndexOfAddr(block, limit);

  isAllocated = BTIsSetRange(allocTable, ib, il);

  /*
  printf("deallocate: [%p, %p) -- %s\n", 
         base, limit, isAllocated ? "succeed" : "fail");
  */

  NDeallocateTried++;

  if(isAllocated) {
    Addr outerBase, outerLimit;    /* interval containing [ib, il) */
    Size left, right, total;       /* Sizes of block and two fragments */

    /* Find the free blocks adjacent to the allocated block */
    if(ib > 0 && !BTGet(allocTable, ib - 1)) {
      outerBase = 
        AddrOfIndex(block, lastEdge(allocTable, ArraySize, ib - 1));
    } else {
      outerBase = base;
     }

    if(il < ArraySize && !BTGet(allocTable, il)) {
      outerLimit = 
        AddrOfIndex(block, nextEdge(allocTable, ArraySize, il));
    } else {
      outerLimit = limit;
    }

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* based on detailed knowledge of CBS behaviour */
    if(total >= MinSize && left < MinSize && right < MinSize) {
      expect_new(outerBase, outerLimit);
    } else if(left >= MinSize && right >= MinSize) {
      if(left >= right) {
        expect_delete(limit, outerLimit);
      } else {
        expect_delete(outerBase, base);
      }
    } else {
      expect_nothing();
    }
  }

  res = CBSInsert(cbs, base, limit);

  if(!isAllocated) { 
    die_expect((mps_res_t)res, MPS_RES_FAIL,
               "succeeded in inserting non-allocated block");
  } else { /* isAllocated */
    die_expect((mps_res_t)res, MPS_RES_OK,
               "failed to insert allocated block");

    NDeallocateSucceeded++;
    BTResRange(allocTable, ib, il);
  }
}

extern int main(int argc, char *argv[])
{
  int i;
  Addr base, limit;
  mps_arena_t mpsArena;
  Arena arena; /* the ANSI arena which we use to allocate the BT */
  CBSStruct cbsStruct;
  CBS cbs;
  void *p;
  Addr dummyBlock;
  BT allocTable;

  testlib_unused(argc); 
  testlib_unused(argv);

  NAllocateTried = NAllocateSucceeded = NDeallocateTried = 
    NDeallocateSucceeded = NNewBlocks = 0;

  die((mps_res_t)mps_arena_create(&mpsArena,
                                  mps_arena_class_an()),
      "Failed to create arena");
  arena = (Arena)mpsArena; /* avoid pun */

  die((mps_res_t)BTCreate(&allocTable, arena, ArraySize), 
      "failed to create alloc table");

  die((mps_res_t)CBSInit(arena, &cbsStruct, &cbsNewCallback, 
                         &cbsDeleteCallback, MinSize, TRUE),
    "failed to initialise CBS");
  cbs = &cbsStruct;

  BTSetRange(allocTable, 0, ArraySize); /* Initially all allocated */

  /* We're not going to use this block, but I feel unhappy just */
  /* inventing addresses. */
  die((mps_res_t)ArenaAlloc(&p, arena, ArraySize), 
      "failed to allocate block");
  dummyBlock = (Addr)p; /* avoid pun */

  printf("Allocated block [%p, %p)\n", dummyBlock, 
         (char *)dummyBlock + ArraySize);

  checkCBS(cbs, allocTable, dummyBlock);
  for(i = 0; i < NOperations; i++) {
    randomRange(&base, &limit, allocTable, dummyBlock);

    if(random(2) == 1) {
      allocate(cbs, dummyBlock, allocTable, base, limit);
    } else {
      deallocate(cbs, dummyBlock, allocTable, base, limit);
    }
    if(i % 5000 == 0)
      checkCBS(cbs, allocTable, dummyBlock);
  }

  /* CBSDescribe prints a very long line. */
  /* CBSDescribe(cbs, mps_lib_get_stdout()); */

  printf("\nNumber of allocations attempted: %ld\n", NAllocateTried);
  printf("Number of allocations succeeded: %ld\n", NAllocateSucceeded);
  printf("Number of deallocations attempted: %ld\n", NDeallocateTried);
  printf("Number of deallocations succeeded: %ld\n", NDeallocateSucceeded);
  printf("Number of new large blocks: %ld\n", NNewBlocks);
  printf("\nNo problems detected.\n");
  return 0;
}
