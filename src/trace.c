/* impl.c.trace: TRACER
 *
 * $HopeName: MMsrc!trace.c(MMdevel_trace2.2) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * NOTES
 *
 * Scanning the roots shouldn't be so special.  All that's
 * needed is to make sure they're not white at the start, then treat
 * them like segments.  The only problem is protection.
 *
 * The mutator colour is the colour of all unprotectable objects, i.e.
 * those objects between which the mutator can move references at will.
 * They therefore all have the same colour, and other objects are
 * protected from them.  The mutator colour is _not_ necessarily the
 * colour of the Root objects.
 *
 * This implementation treats the roots as unprotectable and everything
 * else as protectable, but that could be changed.
 *
 * The sequence of events is:
 *  1. create a Trace, allocating a TraceId and a reference partition
 *  2. start the trace by colouring some objects white and others grey
 *     (in particular the mutator is grey)
 *  3. scan segments containing ambiguous references
 *  4. scan the unprotectable references, turning the mutator black
 *  5. scan remaining grey segments in rank order
 *  6. reclaim white objects
 *  7. destroy the Trace
 *
 * The protection mode of a segment may need to be adjusted whenever
 * the colour changes relative to the mutator.  The colour is changed
 * by scanning and fixing, and the mutator colour by flip.
 *
 * The correct protection can be determined from the colour bits, so
 * there is no need for a separate Shield mechanism, at least not for
 * the benefit of the Tracer.  (It might be useful for other reasons.)
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(MMdevel_trace2.2) $");


/* TraceCheck -- check consistency of Trace object */

Bool TraceCheck(Trace trace)
{
  CHECKS(Trace, trace);
  CHECKU(Space, trace->space);
  CHECKL(TraceIdCheck(trace->ti));
  CHECKL(trace == &trace->space->trace[trace->ti]);
  CHECKL(TraceSetMember(trace->space->busyTraces, trace->ti));
  /* Can't check trace->white -- not in O(1) anyway. */
  /* See TraceFullCheck. */
  return TRUE;
}


/* TraceIdCheck -- check validity of a TraceId */

Bool TraceIdCheck(TraceId ti)
{
  CHECKL(ti < TRACE_MAX);
  return TRUE;
}


/* TraceSetCheck -- check validity of a TraceSet */

Bool TraceSetCheck(TraceSet ts)
{
/*  CHECKL(ts < (1uL << TRACE_MAX)); @@@@ */
  return TRUE;
}


/* FixCheck -- check the validity of a Fix */

Bool FixCheck(Fix fix)
{
  RefSet white;
  TraceId ti;
  CHECKS(Fix, fix);
  CHECKU(Space, fix->space);
  CHECKL(fix->zoneShift == fix->space->zoneShift);
  CHECKL(RankCheck(fix->rank));
  white = RefSetEMPTY;
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetMember(fix->ts, ti))
      white = RefSetUnion(white, TraceWhite(SpaceTrace(fix->space, ti)));
  CHECKL(fix->white == white);
  return TRUE;
}


/* TraceFullCheck -- consistency check of tracer data structures
 *
 * This is _not_ an O(1) check of the Trace data structure, but
 * a thorough going-over of the heap.
 */

static Bool TraceFullCheck(Space space)
{
  TraceId ti;
  RefSet whites[TRACE_MAX];
  Seg seg;
  TraceSet ts;

  /* Check that the white reference sets in each of the trace */
  /* structures are supersets of the sets of white segments. */
  
  for(ti = 0; ti < TRACE_MAX; ++ti)
    whites[ti] = RefSetEMPTY;

  for(seg = SegFirst(space); seg != NULL; seg = SegNext(space, seg)) {
    AVER(ColCheck(SegCol(seg))); /* in passing */

    for(ti = 0; ti < TRACE_MAX; ++ti)
      if(ColIsWhite(SegCol(seg), TraceSetSingle(ti)))
        whites[ti] = TraceSetUnion(whites[ti], RefSetOfSeg(space, seg));
  }
  
  for(ti = 0; ti < TRACE_MAX; ++ti)
    AVER(TraceSetSuper(SpaceTrace(space, ti)->white, whites[ti]));

  /* @@@@ Should scan segments accumulating colour accurately */
  /* and make sure SegCol(seg) is OK.  This will be a pretty */
  /* thorough test. */

/* @@@@
  for(seg = SegFirst(space); seg != NULL; seg = SegNext(space, seg))
    AVER(ColSuper(SegCol(seg), SegCalculateCol(seg)));
 */
 
  /* The mutator should only be grey for unflipped traces. */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetMember(space->busyTraces, ti))
      AVER(SpaceTrace(space, ti)->state != TraceUNFLIPPED ||
           ColIsGrey(SpaceMutCol(space), TraceSetSingle(ti)));

  /* The mutator is never white. */
  AVER(!ColIsWhite(SpaceMutCol(space), TraceSetUNIV));

  /* All segments, and the mutator, are black for inactive segments. */
  ts = TraceSetComp(space->busyTraces);
  AVER(!ColIsGrey(SpaceMutCol(space), ts));
  for(seg = SegFirst(space); seg != NULL; seg = SegNext(space, seg)) {
    AVER(!ColIsWhite(SegCol(seg), ts));
    AVER(!ColIsGrey(SegCol(seg), ts));
  }

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

  AVER(traceReturn != NULL);
  AVERT(Space, space);

  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(!TraceSetMember(space->busyTraces, ti))
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


/* TraceWhite -- return the approximation to the white set */

RefSet TraceWhite(Trace trace)
{
  AVERT(Trace, trace);
  return trace->white;
}


/* UnionWhite -- calculate the approx. to whites of traces
 *
 * UnionWhite calculates a RefSet which is the approximation
 * to the union of the white sets of a set of traces, ts, so
 * that it's possible to scan for more than one.
 */

static RefSet UnionWhite(Space space, TraceSet ts)
{
  RefSet rs;
  TraceId ti;
  
  AVERT(Space, space);
  AVER(TraceSetCheck(ts));
  
  rs = RefSetEMPTY;
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetMember(ts, ti))
      rs = RefSetUnion(rs, TraceWhite(SpaceTrace(space, ti)));

  return rs;
}
  

/* TraceFix -- fix up a reference
 *
 * TraceFix is called from a scanner, which is called from TraceScan.
 * If *refIO is white for any trace being scanned for (fix->ts), it makes
 * it non-white by asking the pool to sort it out.
 */

static Res TraceFix(Ref *refIO, Fix fix)
{
  Ref ref;
  Seg seg;
  Pool pool;
  Res res;

  AVERT(Fix, fix);
  AVER(refIO != NULL);

  ref = *refIO;
  if(SegOfAddr(&seg, fix->space, ref))
    if(ColIsWhite(SegCol(seg), fix->ts)) {
      pool = SegPool(seg);
      res = PoolFix(refIO, pool, fix, seg);
      if(res != ResOK) return res;
    }

/* @@@@
  AVER(!PoolIsWhite(pool, *refIO));
 */

  return ResOK;
}


/* TraceScanSeg -- scan a segment for a set of traces
 *
 * .scan.seg.purpose: The purpose of scanning a segment is to remove
 * greyness from it for a set of traces.  This is achieved by
 * applying the generic fix function, TraceFix, to all of the
 * references in the segment.  The Fix object is a closure
 * for TraceFix.
 */

static Res TraceScanSeg(Space space, Seg seg, TraceSet ts)
{
  Res res;
  FixStruct fixStruct;
  
  AVERT(Space, space);
  AVERT(Seg, seg);
  AVER(TraceSetCheck(ts));
  
  AVER(ColIsGrey(SegCol(seg), ts));

  fixStruct.f = TraceFix;
  fixStruct.zoneShift = SpaceZoneShift(space);
  fixStruct.white = UnionWhite(space, ts);
  fixStruct.summary = RefSetEMPTY;
  fixStruct.space = space;
  fixStruct.ts = ts;
  fixStruct.rank = seg->rank;
  fixStruct.sig = FixSig;

  res = PoolScan(SegPool(seg), &fixStruct, seg);
  if(res != ResOK) return res;

  fixStruct.sig = SigInvalid; /* make sure it can't be used again */

  ColBlacken(SegCol(seg), ts);

  return ResOK;
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

static Seg FindGrey(Space space, TraceSet ts)
{
  Rank rank;
  Seg seg;

  AVERT(Space, space);
  AVER(TraceSetCheck(ts));
  
  for(rank = 0; rank < RankMAX; ++rank)
    for(seg = SegFirst(space); seg != NULL; seg = SegNext(space, seg))
      if(seg->rank == rank && ColIsGrey(SegCol(seg), ts))
        return seg;

  return NULL;
}


/* TraceScan -- find and scan a grey segment */

static Res TraceScan(Trace trace)
{
  TraceSet ts;
  Seg seg;
  Res res;
  Space space;

  AVERT(Trace, trace);
  AVER(trace->state == TraceUNFLIPPED | trace->state == TraceFLIPPED);

  space = trace->space;
  ts = TraceSetSingle(trace->ti);
  
  seg = FindGrey(space, ts);
  if(seg == NULL) {
    trace->state = TraceRECLAIM;
    return ResOK;
  }
  
  res = TraceScanSeg(space, seg, ts);
  if(res != ResOK) return res;
  
  return ResOK;
}


/* TraceAccess -- handle a mutator access to a shielded segment */

void TraceAccess(Space space, Seg seg, AccessSet mode)
{
  NOTREACHED;
}


/* TraceFlip -- blacken the mutator for some traces
 *
 * Blackening the mutator involves scanning the roots.
 *
 * (It should really involve scanning all the unprotectable objects
 *  which the mutator can see, i.e. the real root node, rather than
 *  the Root objects, which may be able to be scanned incrementally.)
 *
 * @@@@ Must deal with half-initialized objects in allocation buffers
 * here.
 */

static Res TraceFlip(Space space, TraceSet ts)
{
  Ring node;
  FixStruct fixStruct;
  TraceId ti;
  Res res;
  
  AVER(ColIsGrey(SpaceMutCol(space), ts));

  fixStruct.f = TraceFix;
  fixStruct.zoneShift = SpaceZoneShift(space);
  fixStruct.white = UnionWhite(space, ts);
  fixStruct.summary = RefSetEMPTY;
  fixStruct.space = space;
  fixStruct.ts = ts;
  fixStruct.rank = RankAMBIG; /* all roots ambig at moment @@@@ */
  fixStruct.sig = FixSig;

  RING_FOR(node, SpaceRootRing(space)) {
    Root root = RING_ELT(Root, spaceRing, node);
    res = RootScan(root, &fixStruct);
    if(res != ResOK) return res;
  }

  fixStruct.sig = SigInvalid; /* make sure it can't be used again */

  ColBlacken(SpaceMutCol(space), ts);

  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetMember(ts, ti))
      SpaceTrace(space, ti)->state = TraceFLIPPED;

  return ResOK;    
}


/* TraceReclaim -- reclaim some segments in a trace
 *
 * This can be done incrementally, but it isn't yet.
 */

static void TraceReclaim(Space space, TraceSet ts)
{
  Seg seg;
  TraceId ti;

  AVERT(Space, space);
  AVER(TraceSetCheck(ts));

  /* The mutator mustn't be grey with respect to any of the traces. */
  AVER(!ColIsGrey(SpaceMutCol(space), ts));

  /* The traces must all be finished, i.e. believe that there */
  /* are no more grey objects. */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetMember(ts, ti))
      AVER(SpaceTrace(space, ti)->state == TraceRECLAIM);

  /* Scan over all the segments and tell the pool about any which */
  /* are white. */
  /* @@@@ This might be better done by telling the pool whose */
  /* condemned set it was? */

  for(seg = SegFirst(space); seg != NULL; seg = SegNext(space, seg)) {

    /* There shouldn't be any grey stuff left. */
    AVER(!ColIsGrey(SegCol(seg), ts));

    if(ColIsWhite(SegCol(seg), ts))
      PoolReclaim(SegPool(seg), seg, ts);
  }

  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetMember(ts, ti))
      SpaceTrace(space, ti)->state = TraceFINISHED;
}


/* TraceComplete -- non-incremental GC driver
 *
 * Creates and starts a trace, then forces it to make progress by
 * calling TraceSet until it is finished.  Then destroys it.
 */

Res TraceComplete(Space space, Option option)
{
  Trace trace;
  Res res;

  AVERT(Space, space);
  AVERT(Option, option);
  
  /* Allocate a trace object from the space.  The only possible */
  /* failure is ResLIMIT, indicating that there aren't any available */
  /* TraceIds left. */  
  res = TraceCreate(&trace, space);
  if(res != ResOK) goto failCreate;

  AVER(TraceFullCheck(space));

  res = TraceStart(trace, option);
  if(res != ResOK) goto failStart;

  while(trace->state != TraceFINISHED) {
    AVER(TraceFullCheck(space));
    res = TraceStep(trace);
    if(res != ResOK) goto failStep;
  }
  
  AVER(TraceFullCheck(space));

  TraceDestroy(trace);

  AVER(TraceFullCheck(space));

  return ResOK;

failStep:
  TraceDestroy(trace);
failStart:
failCreate:
  return res;
}


/* TraceStep -- make some progress in tracing
 *
 * This should accept some sort of progress controls.
 */

Res TraceStep(Trace trace)
{
  Space space;
  TraceSet ts;
  Res res;

  AVERT(Trace, trace);

  space = trace->space;
  ts = TraceSetSingle(trace->ti);

  switch(trace->state) {
    case TraceUNFLIPPED:
    res = TraceFlip(space, ts);
    if(res != ResOK) return res;
    break;

    case TraceFLIPPED:
    res = TraceScan(trace);
    if(res != ResOK) return res;
    break;

    case TraceRECLAIM:
    TraceReclaim(space, ts);
    break;

    case TraceFINISHED:
    case TraceINIT:
    NOTREACHED;
  }

  return ResOK;
}


/* TraceStart -- start a new trace
 *
 * Starts a new trace using option to locate the initial
 * reference partition's white set.
 */

Res TraceStart(Trace trace, Option option)
{
  Space space;
  TraceId ti;			/* id of the new trace */
  TraceSet ts;			/* the singleton {id} */
  RefSet white;			/* superset of refs to white set */
  Seg seg;
  
  AVERT(Trace, trace);
  AVERT(Option, option);
  AVER(trace->state == TraceINIT);

  space = trace->space;
  ti = trace->ti;
  ts = TraceSetSingle(ti);

  /* Set the mutator (all roots) to grey. */
  ColSetGrey(SpaceMutCol(space), ts);

  /* Set all segments to grey.  Add white to condemned segments. */
  /* Accumulate an approximation to the white set. */
  /* @@@@ The owning pool should almost certainly be involved in */
  /* both these operations.  Also, refinement has not been impl'd. */

  white = RefSetEMPTY;

  for(seg = SegFirst(space); seg != NULL; seg = SegNext(space, seg)) {
    Col col = SegCol(seg);
    
    /* Traces should start out black. */
    AVER(!ColIsGrey(col, ts));
    AVER(!ColIsWhite(col, ts));

    /* A segment can only contain grey objects if it contains some references. */
    if(seg->rank != RankNONE)
      ColSetGrey(SegCol(seg), ts);

    /* Allow the option to condemn the segment. */
    OptionCondemn(option, seg, ti);

    /* Add it to the white refset if it's now white. */
    if(ColIsWhite(SegCol(seg), ts))
      white = RefSetUnion(white, RefSetOfSeg(space, seg));
  }

  trace->white = white;

  /* refine here */
  
  trace->state = TraceUNFLIPPED;

  AVER(TraceFullCheck(space));

  return ResOK;
}


/* TraceScanAreaTagged -- scan aligned words
 *
 * The words between base and limit are fixed if they are multiples
 * of four, i.e. look like 4-byte aligned pointers.
 *
 * This function shouldn't be here.  It is used by stack scanners
 * and is rather Dylan specific.
 */

Res TraceScanAreaTagged(Fix fix, Addr *base, Addr *limit)
{
  Res res;
  Addr *p;
  Ref ref;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  TRACE_SCAN_BEGIN(fix) {
    p = base;
  loop:
    if(p >= limit) goto out;
    ref = *p++;
    if(((Word)ref&3) != 0)   /* only fix 4-aligned pointers */
      goto loop;             /* not a pointer */
    if(!TRACE_FIX1(fix, ref)) goto loop;
    res = TRACE_FIX2(p-1, fix);
    if(res == ResOK) goto loop;
    return res;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(fix);

  return ResOK;
}

