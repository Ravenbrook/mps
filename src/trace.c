/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(MMdevel_action2.4) $
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(MMdevel_action2.4) $");

Bool ScanStateCheck(ScanState ss)
{
  CHECKS(ScanState, ss);
  CHECKU(Space, ss->space);
  CHECKL(ss->zoneShift == ss->space->zoneShift);
  CHECKL(RankCheck(ss->rank));
  CHECKL(ss->white == ss->space->trace[ss->traceId].white);
  return TRUE;
}

Bool TraceIdCheck(TraceId ti)
{
  CHECKL(ti == TraceIdNONE || ti < TRACE_MAX);
  return TRUE;
}

Bool TraceSetCheck(TraceSet ts)
{
  CHECKL(ts < (1uL << TRACE_MAX));
  return TRUE;
}

/* TraceCheck -- check consistency of Trace object */

Bool TraceCheck(Trace trace)
{
  CHECKS(Trace, trace);
  CHECKU(Space, trace->space);
  CHECKL(TraceIdCheck(trace->ti));
  CHECKL(trace == &trace->space->trace[trace->ti]);
  CHECKL(TraceSetIsMember(trace->space->busyTraces, trace->ti));
  /* Can't check trace->white -- not in O(1) anyway. */
  return TRUE;
}


/* TraceCreate -- create a Trace object
 *
 * Allocates and initializes a new Trace object with a TraceId
 * which is not currently active.
 *
 * Returns ResLIMIT if there aren't any available trace IDs.
 *
 * Trace objects are allocated directly from a small array in the
 * space structure which is indexed by the TraceId.  This is so
 * that it's always possible to start a trace (provided there's
 * a free TraceId) even if there's no available memory.
 *
 * This code is written to be adaptable to allocating Trace
 * objects dynamically.
 */

Res TraceCreate(Trace *traceReturn, Space space)
{
  TraceId ti;
  Trace trace;

  /* .single-collection */
  AVER(TRACE_MAX == 1);

  AVER(traceReturn != NULL);
  AVERT(Space, space);

  /* allocate free TraceId */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(!TraceSetIsMember(space->busyTraces, ti))
      goto found;
  return ResLIMIT;

found:
  trace = SpaceTrace(space, ti);
  space->busyTraces = TraceSetAdd(space->busyTraces, ti);

  /* @@@@ Everything should be black for ti.  Could check. */

  trace->space = space;
  trace->white = RefSetEMPTY;
  trace->ti = ti;
  trace->state = TraceINIT;

  trace->sig = TraceSig;
  AVERT(Trace, trace);

  *traceReturn = trace;
  return ResOK;
}


/* TraceDestroy -- destroy a trace object
 *
 * Finish and deallocate a Trace object, freeing up a TraceId.
 *
 * This code does not allow a Trace to be destroyed while it is
 * active.  It would be possible to allow this, but the colours
 * of segments etc. would need to be reset to black.
 */

void TraceDestroy(Trace trace)
{
  AVERT(Trace, trace);
  AVER(trace->state == TraceFINISHED);
  trace->sig = SigInvalid;
  trace->space->busyTraces =
    TraceSetDel(trace->space->busyTraces, trace->ti);
}


Res TraceCondemn(RefSet *whiteReturn, Trace trace, Pool pool)
{
/* @@@@ This will iterate over all segments, greying them, and */
/* whitening all those in the condemned set.  To begin with */
/* it just takes over from PoolCondemn by iterating over the */
/* segments in a pool. */
  AVER(whiteReturn != NULL);
  AVERT(Trace, trace);
  AVERT(Pool, pool);

  return PoolCondemn(whiteReturn, pool, trace);
}


Res TraceFlip(Trace trace, RefSet white)
{
  Ring ring;
  Ring node;
  Space space;
  ScanStateStruct ss;
  Res res;

  AVERT(Trace, trace);

  space = trace->space;
  ShieldSuspend(space);

  AVER(trace->state == TraceUNFLIPPED);
  AVER(trace->white == RefSetEMPTY);

  trace->white = white;

  /* Update location dependency structures.  white is */
  /* a conservative approximation of the refset of refs which */
  /* may move during this collection. */
  /* @@@@ It is too conservative.  Not everything white will */
  /* necessarily move. */
  LDAge(space, white);

  /* Grey all the roots and pools. */
  ring = SpacePoolRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Pool pool = RING_ELT(Pool, spaceRing, node);

    if((pool->class->attr & AttrSCAN) != 0)
      PoolGrey(pool, trace);  /* implicitly excludes white set */

    node = next;
  }

  ring = SpaceRootRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Root root = RING_ELT(Root, spaceRing, node);

    RootGrey(root, trace);

    node = next;
  }

  ss.fix = TraceFix;
  ss.zoneShift = SpaceZoneShift(space);
  ss.white = trace->white;
  ss.summary = RefSetEMPTY;
  ss.space = space;
  ss.traceId = trace->ti;
  ss.weakSplat = (Addr)0xadd4badd;
  ss.sig = ScanStateSig;

  /* At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or */
  /* final roots either, since we must protect these in order to */
  /* avoid scanning them too early, before the pool contents. */

  /* @@@@ This isn't correct if there are higher ranking roots than */
  /* data in pools. */

  for(ss.rank = RankAMBIG; ss.rank <= RankEXACT; ++ss.rank) {
    ring = SpaceRootRing(space);
    node = RingNext(ring);

    while(node != ring) {
      Ring next = RingNext(node);
      Root root = RING_ELT(Root, spaceRing, node);

      AVER(RootRank(root) <= RankEXACT); /* see above */

      if(RootRank(root) == ss.rank) {
        res = RootScan(&ss, root);
        if(res != ResOK) {
          return res;
        }
      }

      node = next;
    }
  }

  ss.sig = SigInvalid;  /* just in case */

  ShieldResume(space);

  return ResOK;
}

static void TraceReclaim(Trace trace)
{
  Ring ring, node;
  Space space;

  AVERT(Trace, trace);

  space = trace->space;
  ring = SpacePoolRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Pool pool = RING_ELT(Pool, spaceRing, node);

    if((pool->class->attr & AttrGC) != 0)
      PoolReclaim(pool, trace);

    node = next;
  }
}

Size TracePoll(Trace trace)
{
  Res res;
  Bool finished;

  AVERT(Trace, trace);

  if(trace->white != RefSetEMPTY) {
    res = TraceRun(trace, &finished);
    AVER(res == ResOK); /* @@@@ */
    if(finished) {
      TraceReclaim(trace);
      TraceDestroy(trace);
      return SPACE_POLL_MAX;
    }
  }

  /* We need to calculate a rate depending on the amount of work */
  /* remaining and the deadline for the collection to finish. */
  return (Size)4096;            /* @@@@ */
}

Res TraceFix(ScanState ss, Ref *refIO)
{
  Ref ref;
  Seg seg;
  Pool pool;

  AVERT(ScanState, ss);
  AVER(refIO != NULL);

  ref = *refIO;
  if(SegOfAddr(&seg, ss->space, ref))
    if(ss->traceId == seg->white) {
      pool = seg->pool;
      return PoolFix(pool, ss, seg, refIO);
    }

  return ResOK;
}

/*  == Scan Area ==
 *
 *  This is a convenience function for scanning the contiguous area
 *  [base, limit).  i.e. it calls fix on all words from base up
 *  to limit, inclusive of base and exclusive of limit.
 */

Res TraceScanArea(ScanState ss, Addr *base, Addr *limit)
{
  Res res;
  Addr *p;
  Ref ref;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  TRACE_SCAN_BEGIN(ss) {
    p = base;
  loop:
    if(p >= limit) goto out;
    ref = *p++;
    if(!TRACE_FIX1(ss, ref)) goto loop;
    res = TRACE_FIX2(ss, p-1);
    if(res == ResOK) goto loop;
    return res;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);

  return ResOK;
}

/*  == Scan Area Tagged ==
 *
 *  This is as TraceScanArea except words are only fixed if they
 *  are multiples of four. i.e. look like 4-byte aligned pointers.
 */

Res TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit)
{
  Res res;
  Addr *p;
  Ref ref;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  TRACE_SCAN_BEGIN(ss) {
    p = base;
  loop:
    if(p >= limit) goto out;
    ref = *p++;
    if(((Word)ref&3) != 0)   /* only fix 4-aligned pointers */
      goto loop;             /* not a pointer */
    if(!TRACE_FIX1(ss, ref)) goto loop;
    res = TRACE_FIX2(ss, p-1);
    if(res == ResOK) goto loop;
    return res;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);

  return ResOK;
}

Res TraceRun(Trace trace, Bool *finishedReturn)
{
  Res res;
  ScanStateStruct ss;
  Space space;

  AVERT(Trace, trace);
  AVER(finishedReturn != NULL);

  space = trace->space;

  ss.fix = TraceFix;
  ss.zoneShift = SpaceZoneShift(space);
  ss.white = trace->white;
  ss.summary = RefSetEMPTY;
  ss.space = space;
  ss.traceId = trace->ti;
  ss.sig = ScanStateSig;

  for(ss.rank = 0; ss.rank < RankMAX; ++ss.rank) {
    Ring ring;
    Ring node;

    if(ss.rank == RankWEAK) {
      ss.weakSplat = (Addr)0;
    } else {
      ss.weakSplat = (Addr)0xadd4badd;
    }

    ring = SpacePoolRing(space);
    node = RingNext(ring);
    while(node != ring) {
      Ring next = RingNext(node);
      Pool pool = RING_ELT(Pool, spaceRing, node);
      Bool finished;

      if((pool->class->attr & AttrSCAN) != 0) {
        res = PoolScan(&ss, pool, &finished);
        if(res != ResOK) return res;

        if(!finished) {
          *finishedReturn = FALSE;
          return ResOK;
        }
      }

      node = next;
    }
  }

  ss.sig = SigInvalid;  /* just in case */

  *finishedReturn = TRUE;
  return ResOK;
}
