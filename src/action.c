/* impl.c.action: STRATEGIC ACTION
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!action.c(MMdevel_control.1) $
 */

#include "mpm.h"
#include <float.h>	/* @@@@ for DBL_MAX */

SRCID(action, "$HopeName: MMsrc!action.c(MMdevel_control.1) $");


/* ActionCheck -- check consistency of an Action structure */

Bool ActionCheck(Action action)
{
  CHECKS(Action, action);
  CHECKU(Pool, action->pool);
  CHECKL(RingCheck(&action->poolRing));
  CHECKL(action->serial <= action->pool->actionSerial);
  return TRUE;
}


/* ActionInit -- initialize an action structure */

void ActionInit(Action action, Pool pool)
{
  AVER(action != NULL);
  AVERT(Pool, pool);

  action->pool = pool;
  RingInit(&action->poolRing);

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


/* Noddy collection policy -- condemn first pool found */

static Res ActionCollect(Action action)
{
  Trace trace;
  Res res;
  Space space;

  space = PoolSpace(action->pool);

  res = TraceCreate(&trace, space, action);
  if(res != ResOK) return res;

/* @@@@ temporary hack to remove incrementality */
  while(space->busyTraces != TraceSetEMPTY) {
    TraceId ti;

    /* Poll active traces to make progress. */
    for(ti = 0; ti < TRACE_MAX; ++ti)
      if(TraceSetIsMember(space->busyTraces, ti)) {
	trace = SpaceTrace(space, ti);

	res = TracePoll(trace);
	AVER(res == ResOK); /* @@@@ */

	/* @@@@ Pick up results and use for prediction. */
	if(trace->state == TraceFINISHED)
	  TraceDestroy(trace);
      }
  }

  return ResOK;
}


/* ReclaimBenefit -- calculate the benefit of reclaiming a byte */

static double ReclaimBenefit(Space space)
{
  Ring node;
  Size sizeMin;         /* min. to which memory might be reduced */
  Size sizeCurrent;     /* current memory usage */
  static double maxOverhead = 1.3;
                        /* maximum acceptable relative overhead */
  static Size minOverhead = (Size)1024*1024;
                        /* minimum acceptable absolute overhead */
  Size overhead;        /* calculated size overhead */
  Size sizeMax;         /* calculated user's limit on memory usage */
  double benefit;       /* benefit per byte of reducing usage */

  AVERT(Space, space);

  sizeMin = (Size)0;
  sizeCurrent = (Size)0;
  RING_FOR(node, SpacePoolRing(space)) {
    Pool pool = RING_ELT(Pool, spaceRing, node);
    sizeMin += PoolSizeMin(pool);
    sizeCurrent += PoolSizeCurrent(pool);
  }

  AVER(sizeCurrent >= sizeMin);

  /* This is a fairly abitrary function which is zero when */
  /* sizeCurrent is equal to sizeMin (i.e. no bytes can be reclaimed) */
  /* and approaches infinity as sizeCurrent approaches sizeMax. */
  overhead = sizeCurrent - sizeMin;
  if(overhead <= minOverhead)   /* also copes with sizeCurrent == 0 */
    benefit = 0;
  else {
    sizeMax = maxOverhead * sizeMin;
    if(sizeCurrent > sizeMax)
      benefit = DBL_MAX;
    else
      benefit =
        (double)(sizeMax - sizeMin) / (sizeMax - sizeCurrent) - 1;
  }

  return benefit;
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
  double reclaimBenefit;
  
  AVERT(Space, space);

  /* @@@@ Should combine all actions with positive benefit */
  for(;;) {
    reclaimBenefit = ReclaimBenefit(space);
    bestBenefit = -DBL_MAX;
    bestAction = NULL;

    RING_FOR(poolNode, &space->poolRing) {
      Pool pool = RING_ELT(Pool, spaceRing, poolNode);
      Ring actionNode;

      RING_FOR(actionNode, &pool->actionRing) {
	Action action = RING_ELT(Action, poolRing, actionNode);
	double benefit;
	AVERT(Action, action);
	
	benefit = PoolBenefit(action->pool, action, reclaimBenefit);
	if(benefit >= bestBenefit) {
	  bestBenefit = benefit;
	  bestAction = action;
	}
      }
    }

    if(bestBenefit <= 0)
      break;

    /* @@@@ ignores failure */
    AVER(bestAction != NULL);
    (void)ActionCollect(bestAction);
  }
}
