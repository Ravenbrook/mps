/* 
TEST_HEADER
 id = $Id$
 summary = (regression test) Allocate a big object (>64M, perhaps up to 1G) in an AMC pool
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "newfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(make_format(&format, arena), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 (void)allocdumb(ap, 1024*1024*80);

 mps_arena_park(arena);
 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}


int main(void)
{
 run_test(test);
 pass();
 return 0;
}

