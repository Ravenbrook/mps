/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(MMdevel_remem.7) $
 *
 * .flipped-traces: flipped traces is the set of traces for which
 * the mutator always sees black.
 *
 * .flipped-white: a seg is flipped white if it is white for any
 * flipped trace.
 *
 * .flipped-black: a seg is flipped black if it is black (neither grey
 * nor white) for each flipped trace.
 *
 * .flipped-grey: a seg is flipped grey if it is neither flipped-black
 * nor flipped-white.
 *
 * Any flipped-grey segment or root must be read-protected while the
 * mutator is not suspended.
 *
 * A flipped-grey segment should be write-protected during scanning.
 * In fact, all grey segs are maintained as write protected.
 *
 * The pools expect old-space segments to be readable and writeable
 * after the trace it is white for is flipped.
 * Flipped-white segments are maintained as unprotected.
 * 
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(MMdevel_remem.7) $");

Bool ScanStateCheck(ScanState ss)
{
  CHECKS(ScanState, ss);
  CHECKU(Space, ss->space);
  CHECKL(ss->zoneShift == ss->space->zoneShift);
  CHECKL(RankCheck(ss->rank));
/* CHECKL(ss->condemned == ss->space->trace[ss->traceId].condemned); */
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

Res TraceCreate(TraceId *tiReturn, Space space)
{
  TraceId ti;
  Trace trace;

  AVER(tiReturn != NULL);
  AVERT(Space, space);

  /* allocate free TraceId */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(!TraceSetIsMember(space->busyTraces, ti))
      goto found;
  return ResLIMIT;

found:
  trace = &space->trace[ti];
  trace->condemned = RefSetEmpty;
  trace->white = 0;
  trace->grey = 0;
  trace->todo = 0;
  trace->done = 0;
  trace->rate = 2500;
  space->busyTraces = TraceSetAdd(space->busyTraces, ti);

  *tiReturn = ti;
  return ResOK;
}

void TraceDestroy(Space space, TraceId ti)
{
  AVERT(Space, space);
  space->busyTraces = TraceSetDelete(space->busyTraces, ti);
  space->flippedTraces = TraceSetDelete(space->flippedTraces, ti);
}

/* TraceCondemn
 *
 * Condemns all segs in the condemned RefSet by setting their seg's
 * condemned value to that of the TraceId.  This also accumulates a
 * RefSet which approximates the condemned set.  This might be smaller
 * than the passed condemned set.
 *
 * .condemn.buf: If a seg is buffered we'd like to detach it,
 * but we can't because the mutator might be half-way through
 * allocating an object.  So instead, we trip the buffer so that
 * the (next) allocation fails and detaches the buffer (see
 * .trip).  The seg _can_ be condemned, but we must be careful
 * not to destroy it if it is still buffered at reclaim time,
 * because the mutator might still be mucking about in the
 * buffer.  (See .reclaim.buf.)
 *
 * .condemn.buf.grey: When we trip the buffer, we also add its
 * greyness to the seg's, so that fixExact doesn't need to look
 * at the buffer.  See .fixExact.grey.old.
 */
static Res TraceCondemn(RefSet *condemnedIO, Space space, TraceId ti)
{
  Ring node;
  RefSet condemnedIn, condemnedOut;
  Trace trace;

  AVER(condemnedIO != NULL);

  condemnedIn = *condemnedIO;
  trace = &space->trace[ti];

  condemnedOut = RefSetEmpty;

  node = RingNext(&space->traceSegRing);
  while(node != &space->traceSegRing) {
    Ring next = RingNext(node);
    Seg seg = RING_ELT(Seg, traceRing, node);
    Pool pool = seg->pool;
    Buffer buffer;

    AVERT(Seg, seg);
    AVERT(Pool, pool);

    /* .condemn.buf, .condemn.buf.grey */
    buffer = seg->buffer;
    if(buffer != NULL) { /* TraceBufferTrap?? @@@@ */
      if(BufferIsSet(buffer))
        BufferTrap(buffer);
      /* buffer sync */
      seg->grey = TraceSetUnion(seg->grey, buffer->grey);
    }

    /* @@@@ Condemn if segment falls entirely within condemned RefSet */
    /* intersects? lowest zone in and not already condemned */
    if(RefSetDiff(RefSetOfSeg(space, seg), condemnedIn) ==
        RefSetEmpty && seg->white == TraceSetEMPTY)
    {
      AVER((seg->white & (1<<ti)) == TraceSetEMPTY);
      AVER(!TraceSetIsMember(seg->grey, ti));
      seg->white = 1<<ti;

      if(seg->buffer != NULL) {
        if(!BufferIsTrapped(seg->buffer))
          BufferTrap(seg->buffer);
      }

    /* @@@@
    res = PoolCondemn(&c, pool, ti, seg);
    if(res != ResOK) goto failCondemn;
    */

      condemnedOut = RefSetUnion(condemnedOut, RefSetOfSeg(space, seg));

      trace->white += SegSize(space, seg);
    }

    node = next;
  }
  *condemnedIO = condemnedOut;
  return ResOK;
}

Res TraceCollect(Space space)
{
  Ring node, ring;
  TraceId ti;
  Trace trace;
  RefSet condemnedIn, condemnedOut;
  unsigned zone; 
  Res res;

  res = TraceCreate(&ti, space);
  if(res != ResOK) goto failTraceCreate;
  trace = &space->trace[ti];

  ring = SpacePoolRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Pool pool = RING_ELT(Pool, spaceRing, node);

    PoolTraceStart(pool, ti);  /* inform pool of trace creation */

    node = next;
  }

  zone = RefSetZone(space, AddrAdd((Addr)&space, sizeof(SpaceStruct)));

  condemnedIn = (RefSet)0x00030003 << ((space->epoch & 7)<<1);

/* move to flip */
  condemnedOut = condemnedIn;
  TraceCondemn(&condemnedOut, space, ti);

  res = TraceFlip(space, ti, condemnedOut);
  if(res != ResOK) goto failTraceFlip;

  return ResOK;

failTraceFlip:
  /* Undo condemnation by aborting? @@@@ */
  TraceDestroy(space, ti);
failTraceCreate:
  return res;
}

/* @@@@ cover exposed fwding buffers */
static void TraceCover(Space space)
{
  Ring node;

  node = RingNext(&space->exposedRing);
  while(node != &space->exposedRing) {
    Ring next = RingNext(node);
    Buffer buffer = RING_ELT(Buffer, traceRing, node);

    ShieldCover(space, buffer->seg);
    BufferTrap(buffer);
    RingRemove(&buffer->traceRing);  /* removing while iterating  @@@@? */

    node = next;
  }
}

Res TraceFlip(Space space, TraceId ti, RefSet condemned)
{
  Ring ring;
  Ring node;
  Trace trace;
  ScanStateStruct ss;
  Res res;

  AVERT(Space, space);

  ShieldSuspend(space);

  trace = &space->trace[ti];
  trace->condemned = condemned;

  /* Update location dependency structures.  condemned is
   * a conservative approximation of the refset of refs which
   * may move during this collection.
   * @@@@ It is conservative.  Not everything condemned will
   * necessarily move.
   */
  LDAge(space, condemned);

  space->flippedTraces = TraceSetAdd(space->flippedTraces, ti);

  /* Grey the relevant segs */

  node = RingNext(&space->traceSegRing);
  while(node != &space->traceSegRing) {
    Ring next = RingNext(node);
    Seg seg = RING_ELT(Seg, traceRing, node);

    /* If the seg is not white grey it if we cannot
     * deduce it does not refer to the condemned set
     */
    if((seg->white & (1<<ti)) == 0 &&
       RefSetInter(seg->summary, condemned) != RefSetEmpty) {
      AccessSet mode;

    /* @@@@ 
    PoolGrey(seg->pool, seg, ti);
    */
      /* @@@@ to SegGrey??
      */
      seg->grey = TraceSetAdd(seg->grey, ti);
      mode = (AccessREAD | AccessWRITE) & ~seg->sm;
      if(mode != 0) {
        if((space->flippedTraces & seg->white) == 0)
            /* not flipped-white */
          ShieldRaise(space, seg, mode);
      }
      trace->grey += SegSize(space, seg);
    }
    if(seg->white & space->flippedTraces) { /* flipped-white */
      /* Remove barrier from any flipped traces
       * The reference set summary does not need to change
       * as the segment is not mutator writeable.
       */
      if(seg->sm != 0)
        ShieldLower(space, seg, seg->sm);
    }


    node = next;
  }

  /* Grey all the roots. */

  ring = SpaceRootRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Root root = RING_ELT(Root, spaceRing, node);

    RootGrey(root, space, ti);

    node = next;
  }

  ss.fix = TraceFix;
  ss.zoneShift = space->zoneShift;
  ss.condemned = space->trace[ti].condemned;
  ss.summary = RefSetEmpty;
  ss.fixed = RefSetEmpty;
  ss.space = space;
  ss.traceSet = TraceSetAdd(TraceSetEMPTY, ti);
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

  TraceCover(space);

  ss.sig = SigInvalid;  /* just in case */

  /* Assume that 3/4 of white dies, then calculate rate to collect
   * allowing allocation during collection to be the expected amount
   * reclaimed.
   * Will need to scan all grey stuff and amount of living white
   * a net amount of 3/4 of the white space will be reclaimed.
   * So (G + W/4)/(W - W/4) gives the scanning rate.
   * Formula below has one added to top and bottom of fraction to avoid
   * 0 and infinity.
   *
   * divide this rate by number of traces allowed
   */
  trace->rate = ((trace->grey + trace->white/4)/TRACE_MAX + 4096)/
                  ((trace->white - trace->white/4)/4096 +1);

  ShieldResume(space);

  return ResOK;
}

/* TraceReclaim
 *
 * After a trace, reclaim any segs which are still condemned for the
 * trace, because they must be dead.
 *
 * .reclaim.grey: Note that this might delete things which are grey
 * for other collections.  This is OK, because we have conclusively
 * proved that they are dead and since the seg cannot be white for a
 * different trace -- the other collection must have assumed they were
 * alive.  There might be a problem with the accounting of grey segs,
 * however.
 *
 * .reclaim.buf: If a condemned seg still has a buffer attached, we
 * can't destroy it, even though we know that there are no live objects
 * there.  Even the object the mutator is allocating is dead, because
 * the buffer is tripped.  (See .condemn.buf.)
 */

static void TraceReclaim(Space space, TraceId ti)
{
  Ring node, ring;

  space->flippedTraces = TraceSetDelete(space->flippedTraces, ti);

  node = RingNext(&space->traceSegRing);
  while(node != &space->traceSegRing) {
    Ring next = RingNext(node);
    Seg seg = RING_ELT(Seg, traceRing, node);

    /* There shouldn't be any grey things left for this trace. */
    AVER(!TraceSetIsMember(seg->grey, ti));

    if(seg->white & (1<<ti)) {
      AVER(seg->white == (1<<ti));
      PoolReclaim(seg->pool, seg, ti);
    }
    node = next;
  }

  ring = SpacePoolRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Pool pool = RING_ELT(Pool, spaceRing, node);

    PoolTraceEnd(pool, ti);  /* inform pool tracing has finished */

    node = next;
  }
}

Size TracePoll(Space space, TraceId ti)
{
  Res res;
  Bool finished;
  Trace trace;

  trace = &space->trace[ti];

  trace->todo += trace->rate;
  if(trace->done <= trace->todo) {
    res = TraceRun(space, ti, &finished);
    AVER(res == ResOK); /* @@@@ */
    if(finished) {
      TraceReclaim(space, ti);
      TraceDestroy(space, ti);
      return SPACE_POLL_MAX;
    }
  }

  return (Size)4096;            /* @@@@ */
}

/* TraceSegScan -- scan a single seg, turning it black
 */

static Res TraceSegScan(Space space, ScanState ss, Seg seg)
{
  TraceId ti;
  Res res;

  AVERT(ScanState, ss);

  AVER(seg->white != TraceSetEMPTY ||
    seg->sm == (AccessREAD | AccessWRITE));

  ShieldExpose(space, seg);

  /* Want to synchronise with buffer here
   * could detach/attach but relies on pool being clever
   * pool might want to decree a buffer is less grey than it seems
   */
  if(seg->buffer != NULL)
    seg->grey = TraceSetUnion(seg->grey, seg->buffer->grey);
  /* it would be logical to move buffer->base up */

  /* PoolScan should
   * fix references in that seg until there are no references left that
   * are grey for ss->traceSet.
   */
  res = PoolScan(seg->pool, ss, seg);
  if(res) {
    ShieldCover(space, seg); /* Cover seg before returning error */
    return res;
  }

  if(seg->buffer != NULL)
    seg->buffer->grey = TraceSetEMPTY;

  seg->grey &= ~ss->traceSet;

  /* The segment is now scanned and the summary should contain
   * the refset of what is referenced by this seg.  The seg
   * will contain the unfixed references (old summary \ condemned)
   * unioned with the new ones (fixed).  See .fix.condemned.
   */
  seg->summary = RefSetUnion(ss->fixed,
      RefSetDiff(ss->summary, ss->condemned));
  
  /* If the seg is marked grey for a flipped trace, we may be
   * be able to deduce that it doesn't in fact point to that
   * traces white set and remove the greyness even though
   * we were not scanning for that trace.
   */
  for(ti = 0; ti <TRACE_MAX; ti++) {
    Trace trace = &space->trace[ti];
    if(TraceSetIsMember(space->flippedTraces & seg->grey , ti))
      if(RefSetInter(trace->condemned, seg->summary) ==
	  RefSetEmpty)
	seg->grey &= ~(1<<ti);
  }

  /* The segment is now less grey than it was.
   * @@@@
   * it no longer needs to be shielded. .barrier.read-write 
   * however, we can now use the write barrier for the
   * remembered set. .barrier.write
   */

  if((space->flippedTraces & seg->white) == 0) {
    /* not flipped-white
     * lower read-barrier if now flipped-black
     */
    if((seg->grey & space->flippedTraces) == 0)
      /* not grey for any flipped trace */
      ShieldLower(space, seg, AccessREAD);
    AVER(seg->sm & AccessWRITE);
  }

  /* Cover the segment again, now it's been scanned. */
  ShieldCover(space, seg);

  return ResOK;
}

/* TraceAccess -- deal with a shield access to a traced pool
 *
 * This is effectively the barrier fault handler.
 *
 * .barrier: There are barriers on some segments.  seg->sm can be
 * read to determine what barrier is in place.
 *
 * .barrier.read-write: There is a read and write barrier on all grey
 * segments.
 * This is rather conservative as we only need a write barrier
 * on grey segments as they are scanned (as fix is not atomic).
 *
 * .barrier.white.none: There is no barrier on flipped white segments
 *
 @@@@
 * .access.buffer: If the page accessed had and still has the
 * forwarding buffer attached, then trip it.  The seg will now
 * be black, and the mutator needs to access it.  The forwarding
 * buffer will be moved onto a fresh grey page.
 *
 * .access.error: @@@@ There really ought to be some error recovery.
 *
 * .access.multi: @@@@ It shouldn't be necessary to scan more than
 * once.  Instead, should use a multiple-fix thingy.  This would
 * require the ScanState to carry a _set_ of traces rather than
 * just one.
 */

void TraceAccess(Pool pool, Seg seg, AccessSet mode)
{
  Space space;
  ScanStateStruct ss;
  Res res;
  TraceId ti;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(seg->pool == pool);

  space = PoolSpace(pool);

  /* sync buffer */
  if(seg->buffer)
    seg->grey = TraceSetUnion(seg->grey, seg->buffer->grey);

  if(seg->grey != TraceSetEMPTY) {
    ss.fix = TraceFix;
    ss.zoneShift = space->zoneShift;
    ss.summary = RefSetEmpty;
    ss.fixed = RefSetEmpty;
    ss.space = space;
    ss.sig = ScanStateSig;
    ss.rank = RankEXACT;
    ss.traceSet = space->flippedTraces;

    /* .access.multi */
    ss.condemned = TraceSetEMPTY;
    for(ti = 0; ti < TRACE_MAX; ++ti)
      if(ss.traceSet & (1<<ti))
        ss.condemned |= space->trace[ti].condemned;
    res = TraceSegScan(space, &ss, seg);
    AVER(res == ResOK);       /* .access.error */

    /* @@@@ There used to be a check here to see if it was
     * the forwarding buffer being scanned. 
     * but this could not be the case (at the time)
     * as it would be detached after scanning, being a buffered seg
     * by this point
     */

    TraceCover(space);
  }
  mode &= seg->sm; /* set of remaining barriers */

  /* seg should now be flipped black and therefore
   * any remaining barriers may be removed.
   */

  if((mode & AccessWRITE) != 0) {
    /* seg will become mutator writeable so update summary
     * conservatively
     */
    seg->summary = RefSetUniv;
  }

  if(mode != 0) {
    ShieldLower(space, seg, mode & seg->sm);
  }

}

/* TraceBufferExpose - perhaps should be called UnTrap
 *
 * Readies the buffer for forwarding, exposing if necessary.
 * Seg is read and write protected if not already.
 * .exposedRing: exposed buffers this is a list of all buffers
 * that have been exposed as a result of forwarding.
 */

static void TraceBufferExpose(Space space, Buffer buffer)
{
  AccessSet mode;
  Seg seg = buffer->seg;

  AVER(BufferIsSet(buffer));

  if((buffer->prop & PropFWD) != 0) { /* PropExposed ?? */
    ShieldExpose(space, seg);
    /* sync buffer */
    seg->grey = TraceSetUnion(seg->grey, buffer->grey);

    buffer->grey = TraceSetEMPTY;

    mode = (AccessREAD | AccessWRITE) & ~seg->sm;
    if(mode != 0) {
      if((space->flippedTraces & seg->white) == 0)
          /* not flipped-white */
        ShieldRaise(space, seg, mode);
      else NOTREACHED; /* @@@@ ?? detach buffer */
    }
    RingAppend(&space->exposedRing, &buffer->traceRing);
  }
}

void TraceBufferDetach(Space space, Buffer buffer)
{
  Seg seg;

  AVER(BufferIsSet(buffer));

  seg = buffer->seg;

  if((buffer->prop & PropFWD) == 0) /* not fwding buf PropExpose@@@@ */
    ShieldExpose(space, seg); /* for flip-trapped @@@@ */
  else
    RingRemove(&buffer->traceRing);

  PoolBufferDetach(buffer->pool, buffer); /* expects set buffer */

  /* @@@@.flush.grey */
  seg->grey = TraceSetUnion(seg->grey, buffer->grey);

  seg->buffer = NULL;           /* detach buffer from seg */ 
  BufferReset(buffer);          /* put into reset state */

  ShieldCover(space, seg);
}

/* TraceBufferFill -- refill an allocation buffer
 *
 * Reserve was called on an allocation buffer which was reset, trapped,
 * or there wasn't enough room left in the buffer.
 * A buffer can be trapped for two different reasons:
 *   The buffer's seg was condemned.
 *     detach buffer is this case
 *   The buffer is a forwarding buffer.
 *     call TraceBufferExpose after untrapping before attempting
 *     allocation of object.
 * Except in the case of a non-full forwarding buffer the request
 * is passed through to the pool.
 *
 */
Res TraceBufferFill(Pool pool, Buffer buffer, Size size)
{
  Space space;
  Res res;

  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);

  space = PoolSpace(pool);

  if(BufferIsSet(buffer))
    TraceBufferDetach(space, buffer);

  AVER(BufferIsReset(buffer));

  res = PoolBufferAttach(pool, buffer, size);
  if(res) return res;

  /* buffer now set up.  Protect & expose appropriately */

  TraceBufferExpose(space, buffer);

  return ResOK;
}


/* pcAMCBufferTrip -- deal with a tripped buffer
 *
 * The buffer has been trapped during an allocation.  It must therefore be
 * a non-forwarding buffer.
 * A flip occurred between a reserve and commit on a buffer, and
 * the buffer was "tripped" (limit set to zero).  The object wasn't
 * scanned, and must therefore be assumed to be invalid, so the
 * reservation must be rolled back. This function detaches the
 * buffer from the seg completely.  The next allocation in the
 * buffer will cause a refill, and reach pcAMCFill.
 */
void TraceBufferTrip(Pool pool, Buffer buffer)
{
  Space space;

  AVERT(Buffer, buffer);
  AVER(!BufferIsReset(buffer));
  AVER(BufferIsReady(buffer));

  space = PoolSpace(pool);

  BufferUntrap(buffer);
  if((buffer->prop & PropFWD) != 0) {
    TraceBufferExpose(space, buffer);
    /* don't detach seg from forwarding buffer unless it is white */
    if((space->flippedTraces & buffer->seg->white) == TraceSetEMPTY)
      return;
  }

  TraceBufferDetach(space, buffer);
}

void TraceBufferFinish(Buffer buffer)
{
  Space space = BufferSpace(buffer);

  if(BufferIsReset(buffer)) 
    return;

  if(BufferIsTrapped(buffer))
    BufferUntrap(buffer);
  TraceBufferExpose(space, buffer);
  TraceBufferDetach(space, buffer);
}

/* allocates unprotected seg */
Res TraceSegAlloc(Seg *segReturn, Pool pool, Size size, PropSet prop)
{
  Res res;
  Seg seg;
  Space space;
  RefSet pref;

  AVER(segReturn != NULL);
  AVERT(Pool, pool);
  space = PoolSpace(pool);
  AVER(SizeIsAligned(size, ArenaAlign(space)));

  pref = (RefSet)0x00030003 <<
      (((space->epoch-((prop & PropFWD) != 0)) & 7)<<1);
  res = SegAllocPref(&seg, space, size, pool, (RefSet)pref);
  if(res != ResOK)
    return res;

  seg->pool = pool;
  seg->grey = TraceSetEMPTY;

  RingAppend(&space->traceSegRing, &seg->traceRing);

  *segReturn = seg;
  return ResOK;
}

void TraceSegFree(Pool pool, Seg seg)
{
  Space space;

  AVERT(Pool, pool);

  space = PoolSpace(pool);
  ShieldFlush(space);
  RingRemove(&seg->traceRing);

  SegFree(space, seg);
}

/* TraceSegGrey - turn a segment that is white for the current trace
 * 
 */
void TraceSegGrey(Space space, ScanState ss, Seg seg)
{
  AVER((seg->white & ss->traceSet) != TraceSetEMPTY);

  seg->grey |= seg->white & ss->traceSet;    /* mark for scanning */
  seg->white &= ~ss->traceSet;       /* remove seg from old-space */

  /* Must now be white, but check no longer white for any trace
   * before raising shield
   */
  if((seg->white & space->flippedTraces) == 0)
    ShieldRaise(space, seg, AccessREAD | AccessWRITE);
}

/* TraceFix - fix a reference
 * .fix.condemn: This only gets called if the reference is in condemned.
 */
Res TraceFix(ScanState ss, Ref *refIO)
{
  Ref ref;
  Seg seg;
  Pool pool;
  Space space;

  AVERT(ScanState, ss);
  AVER(refIO != NULL);

  ref = *refIO;
  space = ss->space;
  if(SegOfAddr(&seg, space, ref)) {
    if(ss->traceSet & seg->white) {
      Res res;
      pool = seg->pool;
      res = PoolFix(pool, ss, seg, refIO);
      if(res) return res;
    }
    /* .fix.fixed: record the refset of the new reference */
    ss->fixed = RefSetAdd(space, ss->fixed, *refIO);
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

Res TraceRun(Space space, TraceId ti, Bool *finishedReturn)
{
  Res res;
  ScanStateStruct ss;
  Trace trace = &space->trace[ti];

  AVERT(Space, space);
  AVER(finishedReturn != NULL);

  ss.fix = TraceFix;
  ss.zoneShift = space->zoneShift;
  ss.condemned = space->trace[ti].condemned;
  ss.summary = RefSetEmpty;
  ss.fixed = RefSetEmpty;
  ss.space = space;
  ss.traceSet = TraceSetAdd(TraceSetEMPTY, ti);
  ss.sig = ScanStateSig;

  trace->done += (Size)4096;
  for(ss.rank = 0; ss.rank < RankMAX; ++ss.rank) {
    Ring ring;
    Ring node;
    TraceSet grey;

    ring = &space->traceSegRing;
    node = RingNext(ring);

    if(ss.rank == RankEXACT) { /* @@@@ only exact segs for now */
      while(node != ring) {
        Ring next = RingNext(node);
        Seg seg = RING_ELT(Seg, traceRing, node);

        grey = seg->grey;
        if(seg->buffer != NULL)
          grey = TraceSetUnion(grey, seg->buffer->grey);

        if(TraceSetIsMember(grey, ti)) {
          res = TraceSegScan(space, &ss, seg);
          if(res) return res; /* should TraceCover anyway @@@@ */

          /* @@@@ remove from grey-list here */

          TraceCover(space);
          *finishedReturn = FALSE;
          return ResOK;
        }

        node = next;
      }
    }
  }

  TraceCover(space);

  ss.sig = SigInvalid;  /* just in case */

  *finishedReturn = TRUE;
  return ResOK;
}
