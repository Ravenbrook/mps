/* impl.c.arenacv: ARENA COVERAGE TEST
 *
 * $HopeName: MMsrc!arenacv.c(MMdevel_tony_sunset.1) $
 * Copyright (C) 1997, 1998 Harlequin Group plc.  All rights reserved.
 *
 * .readership: MPS developers
 * .coverage: At the moment, we're only trying to cover the new code
 * (partial mapping of the page table and vm overflow).
 * .note.tract-size: If the page size is divisible by sizeof(TractStruct), many
 * test cases end up being essentially identical -- there just aren't that
 * many different cases then.
 * .improve.gap-below: Could test different-sized gaps below the tract
 * being allocated; this requires using two adjacent zones.
 */

#include <stdio.h>
#include <stdlib.h>
#include "mpstd.h"

#include "mpm.h"
#include "testlib.h"
#include "mpsavm.h"
#include "mpsacl.h"
#include "mpsaan.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


#define tractsSIZE 500


static void testPageTable(ArenaClass class, ...)
{
  Arena arena; Pool pool;
  Addr offsetBase, gapBase, newBase, topBase;
  Size pageSize;
  Count tractsPerPage, offset, gap, new;
  int i;
  SegPrefStruct pref = *SegPrefDefault();
  RefSet refSet = (RefSet)2;
  va_list args;

  va_start(args, class);
  die(ArenaCreateV(&arena, class, args), "ArenaCreate");
  va_end(args);

  die(PoolCreate(&pool, arena, PoolClassMV(),
		 (Size)65536, (Size)32, (Size)65536),
      "PoolCreate");

  pageSize = ArenaAlign(arena);
  tractsPerPage = pageSize / sizeof(TractStruct);
  printf("%ld tracts per page in the page table.\n", (long)tractsPerPage);

  /* Testing the behaviour with various sizes of gaps in the page table. */

  /* Assume the allocation strategy is first-fit.  The idea of the tests is */
  /* to allocate a region of memory, then deallocate a gap in the middle, */
  /* then allocate a new region that fits in the gap with various amounts */
  /* left over.  Like this: */
  /* |-offsetRegion-||----gapRegion----||-topRegion-| */
  /* |-offsetRegion-||-newRegion-|      |-topRegion-| */
  /* This is done with three different sizes of offsetRegion, in two */
  /*  different zones to ensure that all page boundary cases are tested. */
  for(i = 0; i < 2; ++i) { /* zone loop */
    for(offset = 0; offset <= 2*tractsPerPage; offset += tractsPerPage) {
      if(offset != 0)
        die(ArenaAlloc(&offsetBase, &pref, offset * pageSize, pool,
                       /* withReservoirPermit */ FALSE),
            "offsetRegion");
      for(gap = tractsPerPage+1; gap <= 3 * (tractsPerPage+1);
          gap += (tractsPerPage+1)) {
        die(ArenaAlloc(&gapBase, &pref, gap * pageSize, pool,
                       /* withReservoirPermit */ FALSE),
            "gapRegion");
        die(ArenaAlloc(&topBase, &pref, pageSize, pool,
                       /* withReservoirPermit */ FALSE),
            "topRegion");
        ArenaFree(gapBase, gap * pageSize, pool);
        for(new = 1; new <= gap; new += tractsPerPage) {
          Tract tract;
          Count tractNum, expected;
          Res enoughTracts;

          die(ArenaAlloc(&newBase, &pref, new * pageSize, pool,
                         /* withReservoirPermit */ FALSE),
              "newRegion");

          /* Test tract iterators */
          die(TractFirst(&tract, arena) ? ResOK : ResFAIL, "first");
          tractNum = 1;
          while (TractNext(&tract, arena, TractBase(tract)))
            tractNum++;

          /* There are at least offset+new+top tracts */
          expected = offset + new + 1;
          if (tractNum >= expected) 
            enoughTracts = ResOK;
          else
            enoughTracts = ResFAIL;

          die(enoughTracts, "Not enough tracts");

          ArenaFree(newBase, new * pageSize, pool);
        }

      ArenaFree(topBase, pageSize, pool);
      }
      if(offset != 0) {
	/* Test TractOfAddr */
        Tract tract;
	Addr base;
        Bool found;

        found = TractOfAddr(&tract, arena, offsetBase);
        die(found ? ResOK : ResFAIL, "TractOfAddr");
	base = TractBase(tract);
	die(base == offsetBase ? ResOK : ResFAIL, "base");

	ArenaFree(offsetBase, offset * pageSize, pool);
      }
    }
    SegPrefExpress(&pref, SegPrefRefSet, &refSet);
  }

  PoolDestroy(pool);
  ArenaDestroy(arena);
}


static Res makeArena(Arena *arenaOut, ArenaClass class, ...)
{
  va_list args;
  Res res;

  va_start(args, class);
  res = ArenaCreateV(arenaOut, class, args);
  va_end(args);
  return res;
}


/* testSize -- test arena size overflow
 *
 * Just try allocating larger arenas, doubling the size each time, until
 * it fails, then check the error code.
 */

static void testSize(Size size)
{
  ArenaClass class = (ArenaClass)mps_arena_class_vm();
  Arena arena;
  Res res;

  do {
    res = makeArena(&arena, class, size);
    if (res == ResOK)
      ArenaDestroy(arena);
    else
      die((res == ResRESOURCE) ? ResOK : res, "right error code");
    size *= 2;
  } while (size == 0);
}


#define TEST_ARENA_SIZE              ((Size)16<<22)


int main(void)
{
  void *block;

  testPageTable((ArenaClass)mps_arena_class_vm(), TEST_ARENA_SIZE);
  testPageTable((ArenaClass)mps_arena_class_vmnz(), TEST_ARENA_SIZE);

  testPageTable((ArenaClass)mps_arena_class_an(), TEST_ARENA_SIZE);

  block = malloc(TEST_ARENA_SIZE);
  die(block == NULL ? ResFAIL : ResOK, "malloc");
  testPageTable((ArenaClass)mps_arena_class_cl(), TEST_ARENA_SIZE,
                (Addr)block);

  testSize(TEST_ARENA_SIZE);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "Conclusion:  Failed to find any defects.\n");
  return 0;
}
