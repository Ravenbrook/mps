/* impl.c.action: STRATEGIC ACTION
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!action.c(MMdevel_gens.2) $
 */

#include "mpm.h"

SRCID(action, "$HopeName: MMsrc!action.c(MMdevel_gens.2) $");


/* ActionCheck -- check consistency of an Action structure */

Bool ActionCheck(Action action)
{
  CHECKS(Action, action);
  CHECKU(Pool, action->pool);
  CHECKL(RingCheck(&action->poolRing));
  CHECKL(FUNCHECK(action->f));
  /* Can't check w as it is interpreted by client (pool) */
  CHECKL(action->serial <= action->pool->actionSerial);
  return TRUE;
}


/* ActionInit -- initialize an action structure */

void ActionInit(Action action, Pool pool, ActionClosureMethod f, Word w)
{
  AVER(action != NULL);
  AVERT(Pool, pool);
  AVER(FUNCHECK(f));
  /* Can't check w as it is interpreted by client (pool) */

  action->pool = pool;
  RingInit(&action->poolRing);

  action->f = f;
  action->w = w;

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


static Res ActionCollect(Action action)
{
  Trace trace;
  Res res;
  Space space;

  space = PoolSpace(action->pool);

  res = TraceCreate(&trace, space);
  if(res != ResOK) goto failTraceCreate;

  res = TraceStart(trace, action);
  if(res != ResOK) goto failStart;

  return ResOK;

failStart:
  /* .improve.undo-condemn: This is unsatisfactory as pools which
   * successfully completed a condemn aren't given a chance to
   * release any resources they may have allocated. */
  TraceDestroy(trace);
failTraceCreate:
  return res;
}


/* ActionPoll -- decide what to do next
 *
 * This is the brain of the system.  The function weighs up the
 * costs and benefits of the various actions exhibited by the pools,
 * and takes those which are worthwhile.
 *
 * @@@@ At the moment, it just launches a collection whenever it
 * can.
 *
 * @@ At the moment, if there is no collection in progress then all the
 * actions are prodded in turn to see whether any action should be
 * taken.  The last action that can be taken is taken.
 */

void ActionPoll(Space space)
{
  Ring poolnode;
  Action chosen = NULL;

  AVERT(Space, space);

  if(space->busyTraces != TraceSetEMPTY) {
    return;
  }

  RING_FOR(poolnode, &space->poolRing) {
    Pool pool = RING_ELT(Pool, spaceRing, poolnode);
    Ring actionnode;

    RING_FOR(actionnode, &pool->actionRing) {
      Action action = RING_ELT(Action, poolRing, actionnode);
      AVERT(Action, action);

      if(action->pool->class->attr & AttrGC) {
	/* Call the pool to decide whether to act */
	if((action->f)(action, action->w)) {
	  chosen = action;
	}
      }
    }
  }
  if(chosen != NULL) {
    (void)ActionCollect(chosen);
  }
}
