/* impl.c.abqtest: AVAILABLE BLOCK QUEUE TEST
 *
 * $HopeName: MMsrc!abqtest.c(trunk.2) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>


#include "mpm.h"
#include "mps.h"
#include "mpsaan.h" /* ANSI arena for ABQCreate and ABQDestroy */
#include "testlib.h"


SRCID(abqtest, "$HopeName: MMsrc!abqtest.c(trunk.2) $");

#include "abq.h"


static ABQStruct abq; /* the ABQ which we will use */
static Size abqSize; /* the size of the current ABQ */

#define ABQ_SIZE 10
#define TEST_ITER 10000


static unsigned long random(unsigned long n)
{
  return rnd()%n;
}

static int pushee = 1;
static int popee = 1;
static int deleted = 0;

/* --- Override this for the test */
Bool CBSBlockCheck(CBSBlock block) {
  UNUSED(block);
  CHECKL(block != NULL);
  /* Don't check block->splayNode? */
  /* CHECKL(block->base <= block->limit); */
  return TRUE;
}


static void step()
{
  Res res;
  CBSBlock a;
  
  switch (random(9)) {
  push:
    case 0: case 1: case 2: case 3:
      res = ABQPush(&abq, (CBSBlock)pushee);
      if (res != ResOK) {
        goto pop;
      }
      pushee++;
      break;
  pop:
    case 5: case 6: case 7: case 8:
      res = ABQPop(&abq, &a);
      if (res != ResOK){
        goto push;
      }
      if (popee == deleted) {
      	popee++;
        deleted = 0;
      }
      AVER(a == (CBSBlock)popee);
      popee++;
      break;
    default:
      if (!deleted & pushee > popee) {
        deleted = random (pushee - popee) + popee;
        res = ABQDelete(&abq, (CBSBlock)deleted);
        AVER(res == ResOK);
      }
  }
}

extern int main(void)
{
  mps_res_t res;
  mps_arena_t arena;

  int i;
  
  abqSize = 0;
  
  res = mps_arena_create(&arena,
			 mps_arena_class_an());
  if (res != MPS_RES_OK) {
    printf("failed to create ANSI arena.\n");
    return 1;
  }
  
  res = ABQInit((Arena)arena, &abq, ABQ_SIZE);
  if (res == ResOK) {
    abqSize = ABQ_SIZE;
  } else {
    printf("ABQCreate returned %d\n",res);
    return 1;
  }

  for (i = 0; i < TEST_ITER; i++) {
    step();
  }
  
  printf("All tests passed.\n");
  
  return 0;
}
