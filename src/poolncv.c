/* impl.c.poolncv: NULL POOL COVERAGE TEST
 *
 * $HopeName: MMsrc!poolncv.c(MM_epcore_brisling.1) $
 * Copyright (C) 1997 Harlequin Limited.  All rights reserved.
 */

#include "pooln.h"
#include "mpsaan.h"
#include "mpm.h"
#include "testlib.h"

#include <stdio.h>


static Bool testit(ArenaClass class, ...)
{
  Bool eflag = FALSE;
  Arena arena;
  Pool pool;
  Res res;
  Addr p;
  va_list args;

  va_start(args, class);
  die(ArenaCreateV(&arena, class, args), "ArenaCreate");
  va_end(args);

  die(PoolCreate(&pool, arena, PoolClassN()), "PoolNCreate");
  res = PoolAlloc(&p, pool, 1, /* withReservoirPermit */ FALSE);
  if(res == ResOK) {
    fprintf(stderr,
            "Error:  Unexpectedly succeeded in"
            "allocating block from PoolN\n");
    eflag = TRUE;
  }
  PoolDestroy(pool);
  ArenaDestroy(arena);

  return eflag;
}


int main(void)
{
  if(testit((ArenaClass)mps_arena_class_an(), (Size)1000)) {
    fprintf(stderr, "Conclusion:  Defects found.\n");
  } else {
    fprintf(stderr, "Conclusion:  Failed to find any defects.\n");
  }
  return 0;
}
