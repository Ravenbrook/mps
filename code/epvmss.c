/* impl.c.epvmss: EPVM POOL CLASS STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .limitations: This tests GC on tangles of PS arrays and strings only.
 */

#include "testlib.h"
#include "fmtpstst.h"
#include "fmtpscon.h"
#include "mpscepvm.h"
#include "mpsavm.h"
#include <stdlib.h>
#include "misc.h" /* for FALSE and TRUE */


#define exactRootsCOUNT 10
#define totalSizeMAX    1000 * (size_t)1024
#define totalSizeSTEP   16 * (size_t)1024
#define objNULL         ((mps_addr_t)0)
#define testArenaSIZE   ((size_t)16<<20)


static OBJECT *exactRoots;
static size_t totalSize = 0;


static void makeObj(OBJECT *objOutput, mps_ap_t ap)
{
  mps_addr_t p;
  mps_res_t res;
  uint16 length;
  size_t size;

  length = (uint16)(rnd() % 4); size = length * sizeof(OBJECT);
  if(length > 0)
    do {
      MPS_RESERVE_BLOCK(res, p, ap, size);
      if(res)
        die(res, "MPS_RESERVE_BLOCK(obj)");
      ps_array_init(objOutput, p, length, exactRoots, exactRootsCOUNT);
    } while(!mps_commit(ap, p, size));
  else
    ps_array_init(objOutput, NULL, length, exactRoots, exactRootsCOUNT);
  totalSize += size;
}


static void makeStr(OBJECT *objOutput, mps_ap_t ap)
{
  mps_addr_t p;
  mps_res_t res;
  uint16 length;
  size_t size;

  length = (uint16)(rnd() % 32);
  size = (length + 7) & ~7; /* align to 8 */
  if(size > 0)
    do {
      MPS_RESERVE_BLOCK(res, p, ap, size);
      if(res)
        die(res, "MPS_RESERVE_BLOCK(str)");
      ps_string_init(objOutput, p, length);
    } while(!mps_commit(ap, p, size));
  else
    ps_string_init(objOutput, NULL, length);
  totalSize += size;
}


static void test(mps_arena_t arena)
{
  mps_fmt_t format;
  mps_root_t exactRoot;
  size_t lastStep = 0;
  unsigned long objects = 0;
  int collections = 0;
  OBJECT dummy;
  mps_pool_t pool[2];
  mps_ap_t strAP[2], objAP[2];

  die(mps_fmt_create_A(&format, arena, ps_fmt_A()), "fmt_create");

  die(mps_pool_create(&pool[0], arena, mps_class_epvm(), format, 1, 0),
      "pool_create(local)");
  die(mps_pool_create(&pool[1], arena, mps_class_epvm(), format, 1, 0),
      "pool_create(global)");

  die(mps_ap_create(&objAP[0], pool[0], TRUE), "ap_create(0, TRUE)");
  die(mps_ap_create(&objAP[1], pool[1], TRUE), "ap_create(1, TRUE)");
  die(mps_ap_create(&strAP[0], pool[0], FALSE), "ap_create(0, FALSE)");
  die(mps_ap_create(&strAP[1], pool[1], FALSE), "ap_create(1, FALSE)");

  /* Have to allocate dynamically to get alignment */
  exactRoots = calloc(exactRootsCOUNT, sizeof(OBJECT));
  die(exactRoots == NULL, "calloc");
  ps_array_init(&dummy, &exactRoots[0], exactRootsCOUNT, NULL, 0);
  die(mps_root_create_fmt(&exactRoot, arena, MPS_RANK_EXACT,
                          (mps_rm_t)0, ps_scan,
                          &exactRoots[0], &exactRoots[exactRootsCOUNT]),
      "root_create_fmt(exact)");

  while(totalSize < totalSizeMAX) {
    size_t r;

    if (totalSize > lastStep + totalSizeSTEP) {
      lastStep = totalSize; ++collections;
      printf("\nCollection %d, size %lu bytes, %lu objects.\n",
             collections, (unsigned long)totalSize, objects);
      fflush(stdout);
      die(mps_epvm_collect(pool[collections % 2]), "mps_epvm_collect");
      for (r = 0; r < exactRootsCOUNT; ++r)
        cdie(ps_check(&exactRoots[r]), "all roots check");
    }

    r = rnd() % exactRootsCOUNT;
    cdie(ps_check(&exactRoots[r]), "dying root check");
    switch (rnd() % 4) {
    case 0: makeObj(&exactRoots[r], objAP[0]); break;
    case 1: makeObj(&exactRoots[r], objAP[1]); break;
    case 2: makeStr(&exactRoots[r], strAP[0]); break;
    case 3: makeStr(&exactRoots[r], strAP[1]); break;
    }
    ps_write(&exactRoots[(exactRootsCOUNT - 1) - r],
             exactRoots, exactRootsCOUNT);

    ++objects;
    if (objects % 256 == 0) {
      printf(".");
      fflush(stdout);
    }
  }

  mps_ap_destroy(objAP[1]);
  mps_ap_destroy(objAP[0]);
  mps_ap_destroy(strAP[1]);
  mps_ap_destroy(strAP[0]);
  mps_root_destroy(exactRoot);
  free(exactRoots);
  mps_pool_destroy(pool[1]);
  mps_pool_destroy(pool[0]);
  mps_fmt_destroy(format);
}


int main(int argc, char **argv)
{
  mps_arena_t arena;

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vmnz(), testArenaSIZE),
      "arena_create");
  test(arena);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}
