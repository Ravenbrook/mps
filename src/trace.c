/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(MMdevel_action2.11) $
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(MMdevel_action2.11) $");

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
  /* @@@@ Use trace->state to check more invarients. */
  /* @@@@ Check trace->interval? */
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
  trace->interval = (Size)4096; /* @@@@ should be progress control */

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


Res TraceStart(Trace trace, Pool pool)
{
  Res res;
  Ring ring, node;
  Space space;
  Seg seg;

  AVERT(Trace, trace);
  AVERT(Pool, pool);
  AVER((pool->class->attr & AttrGC) != 0);
  AVER(trace->state == TraceINIT);
  AVER(trace->white == RefSetEMPTY);

  /* Identify the condemned set and turn it white. */
  space = trace->space;
  ring = PoolSegRing(pool);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Seg seg = RING_ELT(Seg, poolRing, node);

    /* Segments should start out black w.r.t. the trace. */
    /* i.e. the reference partition is (B, 0, 0). */
    AVER(seg->white == TraceIdNONE);

    res = PoolCondemn(pool, trace, seg);
    if(res != ResOK) {
      /* @@@@ Ought to unwind. */
      return res;
    }

    /* Add the segment to the white set if it's now white. */
    if(seg->white == trace->ti)
      trace->white = RefSetUnion(trace->white, RefSetOfSeg(space, seg));

    node = next;
  }

  /* If there is nothing white then there can be nothing grey, */
  /* so everything is black and we can proceed straight to */
  /* reclaim.  We have to reclaim because we want to guarantee */
  /* to the pool that for every condemn there will be a reclaim. */
  /* @@@@ We can also shortcut if there is nothing grey. */
  /* @@@@ This should be in design. */
  if(trace->white == RefSetEMPTY) {
    trace->state = TraceRECLAIM;
    return ResOK;
  }

  /* Turn everything else grey. */

  /* @@@@ Instead of iterating over all the segments, we could */
  /* iterate over all pools which are scannable and thence over */
  /* all their segments.  This might be better if the minority */
  /* of segments are scannable.  Perhaps we should choose */
  /* dynamically which method to use. */

  seg = SegFirst(space);
  while(seg != NULL) {
    /* Segment should be either black or white by now. */
    AVER(!TraceSetIsMember(seg->grey, trace->ti));

    /* A segment can only be grey if it contains some references. */
    /* This is indicated by the rankSet begin non-empty.  Such */
    /* segments may only belong to scannable pools. */
    if(seg->rankSet != RankSetEMPTY) {
      /* Segments with ranks may only belong to scannable pools. */
      AVER((seg->pool->class->attr & AttrSCAN) != 0);

      /* @@@@ This is where we should look at the remembered set */
      /* (summary) stored in the segment to see if it might */
      /* possibly refer to the condemned set. */

      PoolGrey(pool, trace, seg);
    }

    seg = SegNext(space, seg);
  }

  ring = SpaceRootRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Root root = RING_ELT(Root, spaceRing, node);

    /* @@@@ This is where we should look at the remembered set */
    /* (summary) stored in the root to see if it might */
    /* possibly refer to the condemned set. */

    RootGrey(root, trace);

    node = next;
  }

  trace->state = TraceUNFLIPPED;

  return ResOK;
}


static Res TraceFlip(Trace trace)
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

  /* @@@@ MUST TRIP ALL BUFFERS to make sure they don't */
  /* contain unscanned objects. */

  /* Update location dependency structures.  white is */
  /* a conservative approximation of the refset of refs which */
  /* may move during this collection. */
  /* @@@@ It is too conservative.  Not everything white will */
  /* necessarily move. */
  LDAge(space, trace->white);

  /* At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or */
  /* final roots either, since we must protect these in order to */
  /* avoid scanning them too early, before the pool contents. */

  /* @@@@ This isn't correct if there are higher ranking roots than */
  /* data in pools. */

  ss.fix = TraceFix;
  ss.zoneShift = SpaceZoneShift(space);
  ss.white = trace->white;
  ss.summary = RefSetEMPTY;
  ss.space = space;
  ss.traceId = trace->ti;
  ss.weakSplat = (Addr)0xadd4badd;
  ss.sig = ScanStateSig;

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

  trace->state = TraceFLIPPED;

  ShieldResume(space);

  return ResOK;
}


static void TraceReclaim(Trace trace)
{
  Space space;
  Seg seg;

  AVERT(Trace, trace);
  AVER(trace->state == TraceRECLAIM);

  space = trace->space;
  seg = SegFirst(space);
  while(seg != NULL) {
    /* There shouldn't be any grey stuff left for this trace. */
    AVER(!TraceSetIsMember(seg->grey, trace->ti));

    if(seg->white == trace->ti) {
      AVER((seg->pool->class->attr & AttrGC) != 0);

      PoolReclaim(seg->pool, trace, seg);
    }

    seg = SegNext(space, seg);
  }

  trace->state = TraceFINISHED;
}


/* FindGrey -- find a grey segment
 *
 * This function finds a segment which is grey for any of the traces
 * in ts and which does not have a higher rank than any other such
 * segment (i.e. a next segment to scan).
 *
 * This is equivalent to choosing a grey node from the grey set
 * of a partition.
 *
 * @@@@ This must be optimised by using better data structures at
 * the cost of some bookkeeping elsewhere, esp. during fix.
 */

static Bool FindGrey(Seg *segReturn, Rank *rankReturn,
                     Space space, TraceId ti)
{
  Rank rank;
  Seg seg;

  AVER(segReturn != NULL);
  AVERT(Space, space);
  AVER(TraceIdCheck(ti));
  
  for(rank = 0; rank < RankMAX; ++rank)
    for(seg = SegFirst(space); seg != NULL; seg = SegNext(space, seg))
      if(RankSetIsMember(seg->rankSet, rank) &&
         TraceSetIsMember(seg->grey, ti)) {
	*segReturn = seg;
        *rankReturn = rank;
	return TRUE;
      }

  return FALSE;
}


void TraceAccess(Space space, Seg seg, AccessSet mode)
{
  Res res;
  ScanStateStruct ss;
  Pool pool;

  AVERT(Space, space);
  AVERT(Seg, seg);
  UNUSED(mode);

  pool = seg->pool;

  ss.fix = TraceFix;
  ss.zoneShift = space->zoneShift;
  ss.summary = RefSetEMPTY;
  ss.space = space;
  ss.sig = ScanStateSig;
  ss.rank = RankEXACT;  /* Surely this is conservative?  @@ */
  ss.weakSplat = (Addr)0xadd4badd;

  /* design.mps.poolamc.access.multi */
  /* @@@@ Change ss to hold a trace set. */
  for(ss.traceId = 0; ss.traceId < TRACE_MAX; ++ss.traceId)
    if(TraceSetIsMember(space->busyTraces, ss.traceId)) {
      ss.white = space->trace[ss.traceId].white;
      res = PoolScan(&ss, pool, seg);
      AVER(res == ResOK);       /* design.mps.poolamc.access.error */
    }

  ss.sig = SigInvalid;		/* just in case */
}


static Res TraceRun(Trace trace)
{
  Res res;
  ScanStateStruct ss;
  Space space;
  Seg seg;
  Rank rank;
  Pool pool;

  AVERT(Trace, trace);
  AVER(trace->state == TraceFLIPPED);

  space = trace->space;

  if(FindGrey(&seg, &rank, space, trace->ti)) {
    ss.fix = TraceFix;
    ss.zoneShift = SpaceZoneShift(space);
    ss.white = trace->white;
    ss.summary = RefSetEMPTY;
    ss.space = space;
    ss.traceId = trace->ti;
    ss.rank = rank;
    ss.sig = ScanStateSig;

    /* @@@@ This must go. */
    if(ss.rank == RankWEAK) {
      ss.weakSplat = (Addr)0;
    } else {
      ss.weakSplat = (Addr)0xadd4badd;
    }

    pool = seg->pool;

    AVER((pool->class->attr & AttrSCAN) != 0);

    res = PoolScan(&ss, pool, seg);
    if(res != ResOK) return res;

    ss.sig = SigInvalid;  /* just in case */
  } else
    trace->state = TraceRECLAIM;

  return ResOK;
}


/* TracePoll -- make some progress in tracing
 *
 * @@@@ This should accept some sort of progress control.
 */

Res TracePoll(Trace trace)
{
  Space space;
  Res res;

  AVERT(Trace, trace);

  space = trace->space;

  switch(trace->state) {
    case TraceUNFLIPPED: {
      res = TraceFlip(trace);
      if(res != ResOK) return res;
    } break;

    case TraceFLIPPED: {
      res = TraceRun(trace);
      if(res != ResOK) return res;
    } break;

    case TraceRECLAIM: {
      TraceReclaim(trace);
    } break;

    case TraceFINISHED:
    case TraceINIT:
    NOOP;
    break;

    default:
    NOTREACHED;
    break;
  }

  return ResOK;
}


#if 0
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
#endif /* 0 */


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

