/* impl.c.action: STRATEGIC ACTION
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!action.c(MMdevel_gens2.2) $
 */

#include "mpm.h"
#include <math.h>
#include <float.h>
#include <stdio.h> /* @@@@ for fprintf in debugging */

SRCID(action, "$HopeName: MMsrc!action.c(MMdevel_gens2.2) $");


/* ActionCheck -- check consistency of an Action structure */

Bool ActionCheck(Action action)
{
  CHECKS(Action, action);
  CHECKU(Pool, action->pool);
  CHECKL(RingCheck(&action->poolRing));
  CHECKL(action->serial <= action->pool->actionSerial);
  CHECKL(action->sigmaBirthTime <= action->size * action->pool->space->allocTime);
  return TRUE;
}


/* ActionInit -- initialize an action structure */

void ActionInit(Action action, Pool pool)
{
  AVER(action != NULL);
  AVERT(Pool, pool);

  action->pool = pool;
  RingInit(&action->poolRing);
  action->size = (Size)0;
  action->sigmaBirthTime = 0;

  action->sig = ActionSig;
  action->serial = pool->actionSerial;
  ++pool->actionSerial;

  AVERT(Action, action);

  RingAppend(&pool->actionRing, &action->poolRing);
}


/* ActionFinish -- finish an action structure */

void ActionFinish(Action action)
{
  AVERT(Action, action);

  RingRemove(&action->poolRing);

  action->sig = SigInvalid;

  RingFinish(&action->poolRing);
}


/* ActionPredict -- predict the net benefit of taking an action
 *
 * This formula is tied in to what the AMC pool class does, i.e.
 * a copying collection.
 *
 * In equilibrium, with a steady heap size, reclaim rate is equal
 * to the allocation rate of the mutator.
 *
 * In practice, the mutator will want to control the reclaim rate
 * by making it a function of the amount of memory
 * used and the amount of memory available -- zero at zero used, and
 * infinity when all memory is used, such that the collector tries to
 * balance with the mutator somewhere in-between.
 *
 * The RECLAIM_BENEFIT is the amount the mutator is willing to pay
 * to manage each byte it allocates (in the same units as the COSTs).
 *
 * How do we think about pause times?
 *
 * Could do with a factor for the temporary space cost of copying collection.
 * What about memory touching costs and swap costs?  What about number of faults?
 * What about the balance between ambiguous and exact references?
 *
 * Perhaps these figures should be determined empirically.
 */

#define COLLECTION_COST	((double)1000)	/* @@@@ approx 68k instructions per collection */
#define SCAN_COST	((double)50)	/* @@@@ approx 68k instructions per byte */
#define COPY_COST	((double)20)	/* @@@@ approx 68k instructions per byte */
#define STEADY_BENEFIT	((double)100)	/* @@@@ steady-state reclaim benefit */

static double ReclaimBenefit(Space space)
{
  double usage = (double)ArenaCommitted(space) / ARENA_SIZE;
  return STEADY_BENEFIT * usage / (1 - usage);
}

static double ActionPredict(Action action)
{
  Space space;
  double averageAge;			/* average age of data */
  double survival;			/* fraction estimated to survive collection */
  double survivalSize;			/* bytes that survive */
  double grey;				/* estimate of what must be scanned */
  double cost, benefit;			/* cost and benefit */

  AVERT(Action, action);

  if(action->size == 0)
    return -COLLECTION_COST;

  space = action->pool->space;
  averageAge = space->allocTime - action->sigmaBirthTime / action->size;
  
  /* Estimate the survival rate based on the average age. */
  /* @@@@ This currently uses a bogus radioactive decay model. */
  survival = pow(0.1, averageAge / 1e6);
  survivalSize = survival * action->size;

  /* Estimate the size of the grey set that will have to be scanned. */
  /* @@@@ This currently assumes that it's everything else in the world. */
  /* @@@@ Should factor in the size of the roots, especially if the stack */
  /* is currently very deep. */
  grey = ArenaCommitted(space) - action->size + survivalSize;
  
  cost = COLLECTION_COST + grey * SCAN_COST + survivalSize * COPY_COST;

  benefit = (action->size - survivalSize) * ReclaimBenefit(space);

  fprintf(stderr, "action %p  averageAge %g  survival %g  cost %g  benefit %g  net %g\n",
          action, averageAge, survival, cost, benefit, benefit - cost);

  return benefit - cost;
}


/* Noddy collection policy -- condemn first pool found */

static Res ActionCollect(Action action)
{
  Trace trace;
  Res res;
  Space space;

  space = PoolSpace(action->pool);

  res = TraceCreate(&trace, space, action);
  if(res != ResOK) return res;

  return ResOK;
}


/* ActionPoll -- decide what to do next
 *
 * This is the brain of the system.  The function weighs up the
 * costs and benefits of the various actions exhibited by the pools,
 * and takes those which are worthwhile.
 */

void ActionPoll(Space space)
{
  Ring poolNode;
  double bestBenefit;
  Action bestAction;
  
  AVERT(Space, space);
  
  bestBenefit = -DBL_MAX;
  bestAction = NULL;

  RING_FOR(poolNode, &space->poolRing) {
    Pool pool = RING_ELT(Pool, spaceRing, poolNode);
    Ring actionNode;

    RING_FOR(actionNode, &pool->actionRing) {
      Action action = RING_ELT(Action, poolRing, actionNode);
      double benefit;
      AVERT(Action, action);
      
      benefit = ActionPredict(action);
      if(benefit >= bestBenefit) {
        bestBenefit = benefit;
        bestAction = action;
      }
    }
  }

  /* @@@@ ignores failure */
  if(bestBenefit > 0) {
    AVER(bestAction != NULL);
    (void)ActionCollect(bestAction);
  }
}
