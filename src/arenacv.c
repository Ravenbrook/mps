/* impl.c.arenacv: ARENA COVERAGE TEST
 *
 * $HopeName$
 * Copyright (C) 1997 Harlequin Group, all rights reserved
 *
 * At the moment, we're only trying to cover the new code (partial mapping
 * of the page table).
 */

#include <stdio.h>
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif

#include "mpm.h"
#include "testlib.h"


#define segsSIZE 500


int main(void)
{
  Space space;
  Pool pool;
  Seg segs[segsSIZE];
  Size pageSize;
  Count segsPerPage, gap, i;

  die(SpaceCreate(&space, (Addr)0, ARENA_SIZE), "SpaceCreate");
  die(PoolCreate(&pool, space, PoolClassMV(),
		 (Size)65536, (Size)32, (Size)65536),
      "PoolCreate");

  pageSize = ArenaAlign(space);
  segsPerPage = pageSize / sizeof(SegStruct);
  printf("%ld segments per page in the page table.\n", (long)segsPerPage);

  /* Testing the behaviour with various sizes of gaps in the page table. */
  /* Assume the allocation strategy is first-fit.  Allocate a range of */
  /* segments, then deallocate a gap in the middle, then allocate a segment. */
  die((3 * segsPerPage + 5 > segsSIZE) ? ResFAIL : ResOK, "Table size");
  die(SegAlloc(&segs[0], SegPrefDefault(), space, pageSize, pool),
      "SegAlloc");
  for(gap = 1; gap < 3 * segsPerPage + 6; gap += 3) {
    for(i = 1; i < gap + 2; ++i) {
      die(SegAlloc(&segs[i], SegPrefDefault(), space, pageSize, pool),
	  "SegAlloc");
    }
    for(i = 1; i < gap + 1; ++i) {
      SegFree(space, segs[i]);
    }
    die(SegAlloc(&segs[1], SegPrefDefault(), space, pageSize, pool),
	"SegAlloc");
    SegFree(space, segs[1]);
    SegFree(space, segs[gap+1]);
  }
  SegFree(space, segs[0]);

  PoolDestroy(pool);
  SpaceDestroy(space);
  fprintf(stderr, "Conclusion:  Failed to find any defects.\n");
  return 0;
}
