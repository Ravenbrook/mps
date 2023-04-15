/* 
TEST_HEADER
 id = $Id$
 summary = UNALIGNED 2nd arg to mps_fmt_create_k
 language = c
 link = testlib.o newfmt.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "arg.h"
#include "newfmt.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;
 mps_fmt_t format;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(make_format(&format, UNALIGNED), "create format");

}

int main(void)
{
 run_test(test);
 return 0;
}
