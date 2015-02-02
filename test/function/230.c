/* 
TEST_HEADER
 id = $Id$
 summary = regression test for job003898
 language = c
 link = testlib.o rankfmt.o
 parameters = SIZE=(1u<<20)
OUTPUT_SPEC
 live1 > 80
 live2 > 40
 live3 = 0
 live4 > 90
END_HEADER
*/

#include "chain.h"
#include "mpm.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpscmvff.h"
#include "rankfmt.h"
#include "testlib.h"

static mps_gen_param_s testChain[] = {{64 * SIZE, 0.9}, {64 * SIZE, 0.1}};

#define genCOUNT (sizeof testChain / sizeof testChain[0])

static void *stackpointer;

#define LIVE_FRACTION(pool, size) \
  (100 * (mps_pool_total_size(pool) - mps_pool_free_size(pool)) / size)

#define FILL(ap)                                                \
  for (;;) {                                                    \
    mps_res_t res = allocrone(&q, ap, 1, mps_rank_exact());     \
    if (res == MPS_RES_OK) {                                    \
      setref(q, 0, p);                                          \
      p = q;                                                    \
    } else if (res == MPS_RES_COMMIT_LIMIT) {                   \
      break;                                                    \
    } else {                                                    \
      die(res, "expected COMMIT_LIMIT");                        \
    }                                                           \
  }

static void test(void)
{
  size_t size = SIZE;
  mps_arena_t arena;
  mps_pool_t pool_amc, pool_mvff;
  mps_thr_t thread;
  mps_root_t root;
  mps_fmt_t format;
  mps_chain_t chain;
  mps_ap_t ap_amc, ap_mvff;
  mycell *p = NULL, *q;

  /* Create an arena with a spare commit limit that's the same as its
   * commit limit, and a generation chain all of whose generations are
   * larger than the commit limit (so that collections are never
   * forced by the generations).
   */
  die(mps_arena_create(&arena, mps_arena_class_vm(), 64 * size), "arena");
  die(mps_arena_commit_limit_set(arena, size), "commit_limit");
  mps_arena_spare_commit_limit_set(arena, 64 * size);

  die(mps_thread_reg(&thread, arena), "register thread");
  die(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
                          mps_stack_scan_ambig, stackpointer, 0), "root");
  die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");
  die(mps_pool_create(&pool_amc, arena, mps_class_amc(), format, chain),
      "create pool amc");
  die(mps_ap_create(&ap_amc, pool_amc), "create ap amc");
  die(mps_pool_create_k(&pool_mvff, arena, mps_class_mvff(), mps_args_none),
      "create pool mvff");
  die(mps_ap_create(&ap_mvff, pool_mvff), "create ap mvff");

  /* 1. Allocate objects in the AMC pool until the commit limit is reached. */
  FILL(ap_amc);
  report("live1", "%d", LIVE_FRACTION(pool_amc, size));
  report("commit1", "%lu", (unsigned long)mps_arena_committed(arena));
  report("spare1", "%lu", (unsigned long)mps_arena_spare_committed(arena));
  report("chunks1", "%lu", (unsigned long)RingLength(&arena->chunkRing));
  report("zones10", "%lx", (unsigned long)chain->gens[0].zones);
  report("zones11", "%lx", (unsigned long)chain->gens[1].zones);
  report("total10", "%lu", (unsigned long)PARENT(PoolGenStruct, genRing, chain->gens[0].locusRing.next)->totalSize);
  report("total11", "%lu", (unsigned long)PARENT(PoolGenStruct, genRing, chain->gens[1].locusRing.next)->totalSize);

  
  mps_arena_park(arena);
  mps_ap_destroy(ap_mvff);
  mps_pool_destroy(pool_mvff);
  mps_ap_destroy(ap_amc);
  mps_pool_destroy(pool_amc);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
  mps_root_destroy(root);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);
}


int main(int argc, char **argv)
{
  void *m;
  stackpointer = &m;
  easy_tramp(test);
  pass();
  return 0;
}

