/* impl.c.epdss: EPDL/R STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 */

#include "mpscepdl.h"
#include "mpslib.h"
#include "mpsavm.h"
#include "mpsacl.h"
#include "testlib.h"
#include "mps.h"
#include <stdlib.h>
#include <stdarg.h>


#define testArenaSIZE   ((((size_t)128)<<20) - 4)
#define smallArenaSIZE  ((((size_t)51)<<20) - 4)
#define testSetSIZE 200
#define testLOOPS 10
/* Make the range large enough to span three pages in the segment table: */
/* 160 segments/page, page size max 0x2000. */
#define maxSIZE (2 * 160 * (size_t)0x2000)
/* Average is about the size of large objects divided by total objects */
#define averageSIZE ((20 * maxSIZE) / (testSetSIZE * (1 + testLOOPS/2)))


/* stress -- create a pool of the requested type and allocate in it */

static mps_res_t stress(mps_class_t class, mps_arena_t arena,
                        size_t (*size)(int i), ...)
{
  mps_res_t res;
  mps_pool_t pool;
  va_list arg;
  int i, k;
  int *ps[testSetSIZE];
  size_t ss[testSetSIZE];

  va_start(arg, size);
  res = mps_pool_create_v(&pool, arena, class, arg);
  va_end(arg);
  if (res != MPS_RES_OK)
    return res;

  /* allocate a load of objects */
  for (i=0; i<testSetSIZE; ++i) {
    ss[i] = (*size)(i);

    res = mps_alloc((mps_addr_t *)&ps[i], pool, ss[i]);
    if (res != MPS_RES_OK)
      return res;
    if (ss[i] >= sizeof(ps[i]))
      *ps[i] = 1; /* Write something, so it gets swap. */
  }

  mps_pool_check_fenceposts(pool);

  for (k=0; k<testLOOPS; ++k) {
    /* shuffle all the objects */
    for (i=0; i<testSetSIZE; ++i) {
      int j = rnd()%(testSetSIZE-i);
      void *tp;
      size_t ts;
     
      tp = ps[j]; ts = ss[j];
      ps[j] = ps[i]; ss[j] = ss[i];
      ps[i] = tp; ss[i] = ts;
    }
    /* free half of the objects */
    /* upper half, as when allocating them again we want smaller objects */
    /* see randomSize() */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      mps_free(pool, (mps_addr_t)ps[i], ss[i]);
      /* if (i == testSetSIZE/2) */
      /*   PoolDescribe((Pool)pool, mps_lib_stdout); */
    }
    /* allocate some new objects */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      ss[i] = (*size)(i);
      res = mps_alloc((mps_addr_t *)&ps[i], pool, ss[i]);
      if (res != MPS_RES_OK) return res;
    }
  }
   
  mps_pool_destroy(pool);

  return MPS_RES_OK;
}


#define max(a, b) (((a) > (b)) ? (a) : (b))


/* randomSize -- produce sizes both latge and small */

static size_t randomSize(int i)
{
  /* Reduce by a factor of 2 every 10 cycles.  Total allocation about 40 MB. */
  return rnd() % max((maxSIZE >> (i / 10)), 2) + 1;
}


/* testInArena -- test all the pool classes in the given arena */

static mps_pool_debug_option_s debugOptions = { (void *)"postpost", 8 };

static int testInArena(mps_arena_t arena)
{
  printf("EPDL debug\n");
  die(stress(mps_class_epdl_debug(), arena, randomSize,
             &debugOptions, (size_t)65536, averageSIZE, (size_t)8),
      "stress EPDL debug");

  printf("EPDL\n");
  die(stress(mps_class_epdl(), arena, randomSize,
             (size_t)65536, averageSIZE, (size_t)8),
      "stress EPDL");

  printf("EPDR debug\n");
  die(stress(mps_class_epdr_debug(), arena, randomSize,
             &debugOptions, (size_t)65536, averageSIZE, (size_t)8),
      "stress EPDR debug");

  printf("EPDR\n");
  die(stress(mps_class_epdr(), arena, randomSize,
             (size_t)65536, averageSIZE, (size_t)8),
      "stress EPDR");

  return 0;
}


int main(int argc, char **argv)
{
  mps_arena_t arena;
  void *block;

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vmnz(), testArenaSIZE),
      "mps_arena_create");
  testInArena(arena);
  mps_arena_destroy(arena);

  block = malloc(testArenaSIZE);
  cdie(block != NULL, "malloc");
  die(mps_arena_create(&arena, mps_arena_class_cl(), testArenaSIZE, block),
      "mps_arena_create");
  testInArena(arena);
  mps_arena_destroy(arena);
  free(block);

  block = malloc(smallArenaSIZE);
  cdie(block != NULL, "malloc");
  die(mps_arena_create(&arena, mps_arena_class_cl(), smallArenaSIZE, block),
      "mps_arena_create");
  testInArena(arena);
  mps_arena_destroy(arena);
  free(block);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}
