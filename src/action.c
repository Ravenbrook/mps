/* impl.c.action: STRATEGIC ACTION
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!action.c(trunk.2) $
 */

#include "mpm.h"

SRCID(action, "$HopeName: MMsrc!action.c(trunk.2) $");


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

static Res ActionCollect(Arena arena)
{
  Ring ring, node;
  Trace trace;
  Res res;
  Pool pool;

  ring = ArenaPoolRing(arena);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);

    pool = RING_ELT(Pool, arenaRing, node);
    if((pool->class->attr & AttrGC) != 0)
      goto found;

    node = next;
  }

  /* No GC-able pool found. */
  return ResOK;

found:
  res = TraceCreate(&trace, arena);
  if(res != ResOK) goto failTraceCreate;

  res = TraceStart(trace, pool);
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
 */

void ActionPoll(Arena arena)
{
  AVERT(Arena, arena);

  if(arena->busyTraces == TraceSetEMPTY)
    (void)ActionCollect(arena);
}
