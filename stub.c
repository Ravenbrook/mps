/* stub.c -- driver for compiled code
 *
 * Richard Brooksby, 2004-07-28
 *
 * $Id$
 *
 *
 * CONFORMANCE
 *
 * This is a strictly conforming hosted ISO C program [C90].
 *
 *
 * LICENCE
 *
 * Copyright (C) 1997-2004 Richard Brooksby.  This document is provided
 * "as is", without any express or implied warranty.  In no event will
 * the authors be held liable for any damages arising from the use of
 * this document.  You may not duplicate or reproduce this document in
 * any form without the express permission of the copyright holder.
 */

#include "sc.h"

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>


extern label_t unit; /* @@@@ see opcodes.h */


/* main -- progam entry point
 *
 * We don't want to hang around in boring old C world any longer than
 * necessary, so main does the minimum amount of initialization and
 * starts the abstract machine.
 */

int main(void)
{
  int res;
  state_t state;

  state = make_state();
  if(state == NULL) {
    (void)fprintf(stderr, "Can't allocate initial state.\n");
    return EXIT_FAILURE;
  }

  /* @@@@ Load up "init.sc" here? */

  /* make_state leaves the environment in A0.  We just pass it to */
  /* the compiled code unit entry point. */
  MAKE_PROC(T0, sym_start, *unit);
  res = run(state);
  if(res != 0)
    goto fail_run;

  /* @@@@ Print the final argument list, even though it's going to be empty. */
  /* Perhaps the compiled unit should return the environment. */
  if(setjmp(state->driver_loop) == 0)
    write_obj(state, ARGL, WRITE_DEPTH, stdout);
  (void)fputc('\n', stdout);

  return EXIT_SUCCESS;

fail_run:
  (void)fputs("Fatal error.\n", stdout);
  if(setjmp(state->driver_loop) == 0)
    write_obj(state, A0, WRITE_DEPTH, stdout);
  (void)fputc('\n', stdout);
  return EXIT_FAILURE;
}
