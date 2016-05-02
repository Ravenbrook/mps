/* arenath.c: MULTI-ARENA MULTI-THREAD TEST
 *
 * $Id$
 * Copyright (c) 2016 Ravenbrook Limited.  See end of file for license.
 *
 * .intro: This test case checks the behaviour of multiple arenas
 * running in multiple threads.
 *
 * .access: Can one arena handle a protection fault while another
 * arena is holding its lock? (See job004019.)
 */

#include "fmtscheme.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "testlib.h"
#include "testthr.h"

#include <stdio.h> /* printf */

#define THREAD_COUNT 2
#define OBJ_COUNT 1000
#define COLLECTIONS 4
#define VECTOR_LENGTH 2

static mps_res_t scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  mps_res_t res;
  res = scheme_scan(ss, base, limit);
  return res;
}

ATTRIBUTE_NOINLINE
static void test(void *marker)
{
  mps_arena_t arena;
  mps_thr_t thr;
  mps_root_t root;
  mps_fmt_t fmt;
  mps_pool_t pool;
  mps_ap_t ap;
  obj_t v;

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
      "arena_create");
  die(mps_thread_reg(&thr, arena), "thread_reg");
  die(mps_root_create_thread(&root, arena, thr, marker), "root_create_thread");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, sizeof(mps_word_t));
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, scheme_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, scheme_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, scheme_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, scheme_pad);
    die(mps_fmt_create_k(&fmt, arena, args), "fmt_create");
  } MPS_ARGS_END(args);
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
    die(mps_pool_create_k(&pool, arena, mps_class_amc(), args), "pool_create");
  } MPS_ARGS_END(args);
  die(mps_ap_create_k(&ap, pool, mps_args_none), "ap_create");
  v = scheme_make_vector(ap, VECTOR_LENGTH, NULL);

  while (mps_collections(arena) < COLLECTIONS) {
    size_t i;
    for (i = 0; i < OBJ_COUNT; ++i) {
      size_t slot = rnd() % VECTOR_LENGTH;
      obj_t w = scheme_make_vector(ap, VECTOR_LENGTH, NULL);
      if (v->vector.vector[slot] == NULL) {
        v->vector.vector[slot] = w;
      } else if (rnd() % 2 == 0) {
        w->vector.vector[rnd() % VECTOR_LENGTH] = v->vector.vector[slot];
        v->vector.vector[slot] = w;
      } else {
        v->vector.vector[slot] = NULL;
      }
    }
  }

  mps_arena_park(arena);
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_fmt_destroy(fmt);
  mps_root_destroy(root);
  mps_thread_dereg(thr);
  mps_arena_destroy(arena);
}

static void *thread(void *arg)
{
  void *marker = &marker;
  test(marker);
  return arg;
}

int main(int argc, char *argv[])
{
  size_t i;
  mps_arena_t arena;
  testthr_t kids[THREAD_COUNT];  
  testlib_init(argc, argv);

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
      "arena_create");

  for (i = 0; i < NELEMS(kids); ++i)
    testthr_create(&kids[i], thread, NULL);

  for (i = 0; i < NELEMS(kids); ++i)
    testthr_join(&kids[i], NULL);

  mps_arena_destroy(arena);
  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
