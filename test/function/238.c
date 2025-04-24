/*
TEST_HEADER
 id = $Id$
 summary = regression test for GitHub issue #306
 language = c
 link = testlib.o newfmt.o
 parameters =
END_HEADER
*/

#include "testlib.h"

#if MPS_WORD_WIDTH > 32

#include "mpsavm.h"
#include "mpscamc.h"
#include "newfmt.h"

#include <stdio.h>

/* On UNIX systems, the protection ProtSet in code/protix.c contained
 * an assert that AddrOffset(base, limit) <= INT_MAX (also with the
 * comment "should be redundant". An allocation larger than INT_MAX
 * therefore triggers the assert, even though the MPS happily allows
 * the allocation otherwise.
 *
 * Note that this is only a problem on platforms where sizeof(int) <
 * sizeof(size_t). If sizeof(int) == sizeof(size_t) then everything
 * works as expected.
 *
 * To trigger the assertion, it is usually enough to just ask the MPS
 * to do a garbage collection cycle.
 *
 * As such, this test simply allocates a large block of memory and
 * forces the MPS to do a collection. The test is enabled for other
 * 64-bit platforms to detect other potential problems there as well.
 */

/* Number of allocations to do to encourage a collection. */
#define NUM_ALLOCS 1000

/* Number of times to allocate objects with a mps_arena_step in
 * between. */
#define NUM_STEPS 2

/* Allocate an object that is >= 2 GiB. In a separate function to avoid
 * accidentally pinning the pointer to the large object just in case. */
static void create_large(mps_ap_t ap, mycell *store)
{
  mycell *created;
  /* Allocate an object with a size that overflows a signed 32-bit integer. */
  size_t target_bytes = 0x80000000;
  /* This is not exact, which is why there is a slight margin. */
  size_t elements = (target_bytes / sizeof(struct refitem)) + 1;
  /* Make sure the number of elements fits in the int in newfmt. */
  asserts(elements < INT_MAX, "INT_MAX is too small");

  created = allocone(ap, (int)elements);

  /* Add some pointers to make the MPS try to protect the object. */
  setref(created, 0, created);
  setref(created, 1, store);

  /* Store it in 'store' so that it is kept alive. */
  setref(store, 0, created);
}


static void test(void *stack_pointer)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_pool_t pool;
  mps_thr_t thread;
  mps_root_t root;
  mps_chain_t chain;
  mps_ap_t ap;

  mycell *object;

  int i, j;

  mps_gen_param_s gen_params[2] = {
    { 512, 0.9 },
    { 10 * 1024, 0.5 }
  };

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "mps_arena_create_k");
  die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
  die(mps_chain_create(&chain, arena, 2, gen_params), "mps_chain_create");
  die(mps_thread_reg(&thread, arena), "mps_thread_reg");
  die(mps_root_create_thread(&root, arena, thread, stack_pointer), "mps_root_create_thread");
  MPS_ARGS_BEGIN(args) {
     MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
     MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
     die(mps_pool_create_k(&pool, arena, mps_class_amc(), args), "mps_pool_create_k");
  } MPS_ARGS_END(args);

  die(mps_ap_create_k(&ap, pool, mps_args_none), "mps_ap_create_k");

  /* Allocate an object that we store the big object inside. To make
   * it less likely that pinning it affects how the MPS behaves. */
  object = allocone(ap, 1);
  create_large(ap, object);

  /* Allocate some other objects and ask for a small amount of
   * collection work. This to encourage the MPS to raise a barrier on
   * the large allocation. */
  for (i = 0; i < NUM_STEPS; i++) {
    for (j = 0; j < NUM_ALLOCS; j++)
      allocone(ap, 100);

    /* Ask the MPS to do a small amount of GC work. */
    mps_arena_step(arena, 0.0001, 10000);
  }

  /* Tear down MPS data structures. */
  mps_arena_park(arena);
  mps_ap_destroy(ap);
  mps_root_destroy(root);
  mps_thread_dereg(thread);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
  mps_arena_destroy(arena);
}

#else

static void test(void *stack_pointer)
{
  /* nothing to do on 32-bit systems */
}

#endif

int main(void)
{
  run_test(test);
  pass();
  return 0;
}
