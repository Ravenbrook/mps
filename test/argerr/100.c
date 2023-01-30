/* 
TEST_HEADER
 id = $Id$
 summary = null &root_t for mps_root_create_table
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = mps_root_o != NULL
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_addr_t a[30];

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_table(NULL, arena,
        mps_rank_ambig(), 0, a, sizeof a),
      "root create");

}

int main(void)
{
 run_test(test);
 return 0;
}
