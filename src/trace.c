/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(MMdevel_remem.6) $
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(MMdevel_remem.6) $");

Bool ScanStateCheck(ScanState ss)
{
  CHECKS(ScanState, ss);
  CHECKU(Space, ss->space);
  CHECKL(ss->zoneShift == ss->space->zoneShift);
  CHECKL(RankCheck(ss->rank));
  CHECKL(ss->condemned == ss->space->trace[ss->traceId].condemned);
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

  /* .single-collection */
  AVER(TRACE_MAX == 1);

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
}

/*
 * @@@@
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

/*
  t = space->epoch & 0xff;
  if(t == 0xff)
    t = 0x7f;
  condemnedIn = (RefSet)((((t+1) & ~t)|1) * 0x1010101);
*/ /*
  condemnedIn = (condemnedIn << space->zoneShift) |
      (condemnedIn >> (WORD_WIDTH - space->zoneShift));
 */
/* move to flip */
  condemnedOut = RefSetEmpty;

  node = RingNext(&space->traceSegRing);
  while(node != &space->traceSegRing) {
    Ring next = RingNext(node);
    Seg seg = RING_ELT(Seg, traceRing, node);
    Pool pool = seg->pool;
    Buffer buffer;

    AVERT(Seg, seg);
    AVERT(Pool, pool);

    /* @@@@ Condemn if segment falls entirely within condemned RefSet */
    /* intersects? lowest zone in */
    if(RefSetDiff(RefSetOfSeg(space, seg), condemnedIn) ==
        RefSetEmpty)
    {
      AVER(seg->condemned == TraceIdNONE);
      AVER(!TraceSetIsMember(seg->grey, ti));
      seg->condemned = ti;

      /* Segments in old space are not protected, so remove the
       * write barrier if there is one (see @@@@.barrier.write) */
      /* @@@@ this should happen at flip time */
      if(seg->summary != RefSetUniv)
        ShieldLower(space, seg, AccessWRITE);

      condemnedOut = RefSetUnion(condemnedOut, RefSetOfSeg(space, seg));

      trace->white += SegSize(space, seg);
    }

    /* .condemn.buf, .condemn.buf.grey */
    buffer = seg->buffer;
    if(buffer != NULL) { /* TraceBufferTrap?? @@@@ */
      buffer->ap.limit = 0;
      seg->grey = TraceSetUnion(seg->grey, buffer->grey);
    }

    /*
    res = PoolCondemn(&c, pool, ti, seg);
    if(res != ResOK) goto failCondemn;
    */

    node = next;
  }

  res = TraceFlip(space, ti, condemnedOut);
  if(res != ResOK) goto failTraceFlip;

  return ResOK;

failTraceFlip:
/*
failCondemn:
*/
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
    buffer->ap.limit = 0;
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
   * @@@@ It is too conservative.  Not everything condemned will
   * necessarily move.
   */
  LDAge(space, condemned);


  /* Grey the relevant segs */

  node = RingNext(&space->traceSegRing);
  while(node != &space->traceSegRing) {
    Ring next = RingNext(node);
    Seg seg = RING_ELT(Seg, traceRing, node);

    if(seg->condemned != ti &&
       RefSetInter(seg->summary, condemned) != RefSetEmpty) {
      AccessSet mode;

    /* @@@@ 
    PoolGrey(seg->pool, seg, ti);
    */

      seg->grey = TraceSetAdd(seg->grey, ti);
      /* .barrier.read-write */
      mode = AccessREAD;
      if(seg->summary == RefSetUniv)
        mode |= AccessWRITE;
      ShieldRaise(space, seg, mode);
      trace->grey += SegSize(space, seg);
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
  ss.traceId = ti;
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
   */
  trace->rate = (trace->grey + trace->white/4 + 4096)/
                  ((trace->white - trace->white/4)/4096 +1);

  ShieldResume(space);

  return ResOK;
}

/* TraceReclaim
 * @@@@
 *
 * After a trace, destroy any segs which are still condemned for the
 * trace, because they must be dead.
 *
 * .reclaim.grey: Note that this might delete things which are grey
 * for other collections.  This is OK, because we have conclusively
 * proved that they are dead -- the other collection must have
 * assumed they were alive.  There might be a problem with the
 * accounting of grey segs, however.
 *
 * .reclaim.buf: If a condemned seg still has a buffer attached, we
 * can't destroy it, even though we know that there are no live objects
 * there.  Even the object the mutator is allocating is dead, because
 * the buffer is tripped.  (See .condemn.buf.)
 */

static void TraceReclaim(Space space, TraceId ti)
{
  Ring node, ring;

  node = RingNext(&space->traceSegRing);
  while(node != &space->traceSegRing) {
    Ring next = RingNext(node);
    Seg seg = RING_ELT(Seg, traceRing, node);

    /* There shouldn't be any grey things left for this trace. */
    AVER(!TraceSetIsMember(seg->grey, ti));

    if(seg->condemned == ti) {
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
 *
 * @@@@.groupscan.blacken: One a group is scanned it is turned black, i.e.
 * the ti is removed from the grey TraceSet.  However, if the
 * forwarding buffer is still pointing at the group it could
 * make it grey again when something is fixed, and cause the
 * group to be scanned again.  We can't tolerate this at present,
 * the the buffer is flushed.  The solution might be to scan buffers
 * explicitly.
 */

static Res TraceSegScan(Space space, ScanState ss, Seg seg)
{
  Res res;

  AVERT(ScanState, ss);

  ShieldExpose(space, seg);

  res = PoolScan(seg->pool, ss, seg);
  if(res) {
    ShieldCover(space, seg); /* Cover seg before returning error */
    return res;
  }

  /* @@@@.groupscan.blacken */
  if(seg->buffer != NULL)
    TraceBufferDetach(space, seg->buffer);
  AVER(seg->buffer == NULL);

  /* The segment is no longer grey for this collection, so
   * it no longer needs to be shielded. .barrier.read-write 
   * however, we can now use the write barrier for the
   * remembered set. .barrier.write
   */

  seg->grey = TraceSetDelete(seg->grey, ss->traceId);
  ShieldLower(space, seg, AccessREAD);
  seg->summary = RefSetUnion(ss->fixed,
      RefSetDiff(ss->summary, ss->condemned));
  if(seg->summary == RefSetUniv)
    ShieldLower(space, seg, AccessWRITE);

  /* Cover the segment again, now it's been scanned. */
  ShieldCover(space, seg);

  return ResOK;
}

/* TraceAccess -- deal with a shield access to a traced pool
 *
 * This is effectively the barrier fault handler.
 *
 * .barrier.read-write: There is a read and write barrier on all grey
 * segments and forwarding buffers.
 * This is rather conservative as we only need a write barrier
 * on grey segments as they are scanned (as fix is not atomic).
 *
 * .barrier.write: There is also a write barrier on scanned segments
 * iff seg->summary != RefSetUniv
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

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(seg->pool == pool);

  space = PoolSpace(pool);

  if((seg->buffer && seg->buffer->grey != TraceSetEMPTY) ||
     seg->grey != TraceSetEMPTY) {
    ss.fix = TraceFix;
    ss.zoneShift = space->zoneShift;
    ss.summary = RefSetEmpty;
    ss.fixed = RefSetEmpty;
    ss.space = space;
    ss.sig = ScanStateSig;
    ss.rank = RankEXACT;

    /* .access.multi */
    for(ss.traceId = 0; ss.traceId < TRACE_MAX; ++ss.traceId)
      if(TraceSetIsMember(space->busyTraces, ss.traceId)) {
	ss.condemned = space->trace[ss.traceId].condemned;
	res = TraceSegScan(space, &ss, seg);
	AVER(res == ResOK);       /* .access.error */
      }

    /* @@@@ There used to be a check here to see if it was
     * the forwarding buffer being scanned. 
     * but this could not be the case (at the time)
     * as it would be detached after scanning, being a buffered seg
     * by this point
     */

    TraceCover(space);

    /* The segment is now scanned, and no longer has a read-barrier.
     * If the access was a write then a access may happen again
     * immediately. */
    return;
  }

  AVER((mode & AccessREAD) == 0);

  if(mode & AccessWRITE) {
    AVER(seg->summary != RefSetUniv);
    seg->summary = RefSetUniv;
    ShieldLower(space, seg, AccessWRITE);
  }
}

static void TraceBufferReady(Space space, Buffer buffer)
{
                     /* @@@@ buffer->limit ??? */
  buffer->ap.limit = SegLimit(space, buffer->seg);  /* @@@@ BufferSet?? */
  if((buffer->prop & PropFWD) != 0) {
    ShieldExpose(space, buffer->seg);
    RingAppend(&space->exposedRing, &buffer->traceRing);
  }
}

void TraceBufferDetach(Space space, Buffer buffer)
{
  Seg seg;

  AVER(buffer->ap.limit !=0); /* @@@@ BufferIsReady? */
  seg = buffer->seg;

  if((buffer->prop & PropFWD) == 0) /* not fwding buf */
    ShieldExpose(space, seg); /* for flip-trapped @@@@ */
  else
    RingRemove(&buffer->traceRing);

  PoolBufferDetach(buffer->pool, buffer);
  ShieldCover(space, seg);

  buffer->ap.limit = 0;
  /* @@@@ BufferReset ?? */
}

/* TraceBufferFill -- refill an allocation buffer
@@@@
  The forwarding buffer may have been scanned; but will not
   have been detached from the segment.

@@@@
 *
 * Reserve was called on an allocation buffer which was reset,
 * or there wasn't enough room left in the buffer.  Allocate a seg
 * for the new object and attach it to the buffer.
 *
 * @@@@.fill.expose: If the buffer is being used for forwarding it may
 * be exposed, in which case the seg attached to it should be
 * exposed.  See @@@@.flush.cover.
 *
 * @@@@ This takes no account of the generation that the buffer should
 *      allocate in.  Forwarding buffers would need to be differently
 *      treated.
 */
Res TraceBufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size)
{
  Addr p;
  Space space;
  Seg seg;
  Res res;

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);

  space = PoolSpace(pool);

  if(!BufferIsReset(buffer)) {  /* not reset  */
    /* is buffer (exposure) trapped or not */
    if(buffer->ap.limit == 0 && (buffer->prop & PropFWD) != 0) {
      /* @@@@ ready buffer: attach and expose */
      ShieldExpose(space, buffer->seg);
      RingAppend(&space->exposedRing, &buffer->traceRing);
      buffer->ap.limit = SegLimit(space, buffer->seg);

      if(size <= AddrOffset(buffer->ap.init, buffer->ap.limit))
        goto reserve;
    }
    /* ordinary fill or flip trapped: should detach */
    seg = buffer->seg;
    if((buffer->prop & PropFWD) == 0) /* not fwding buf */
      ShieldExpose(space, seg); /* for flip-trapped @@@@ */
    else
      RingRemove(&buffer->traceRing);

    PoolBufferDetach(pool, buffer); /* must be ready before detach */
    ShieldCover(space, seg);
  }

  space = BufferSpace(buffer);
  res = PoolBufferAttach(pool, buffer, size);

  if(res != ResOK) return res;
  /* buffer now set up.  Protect & expose appropriately */

  buffer->grey = TraceSetEMPTY;

  if(buffer->prop & PropFWD) { /* potential security problem @@@@ */
    ShieldExpose(space, buffer->seg);
    ShieldRaise(space, buffer->seg, AccessREAD | AccessWRITE);
    RingAppend(&space->exposedRing, &buffer->traceRing);
  }

reserve:
  p = buffer->ap.init;          /* actually do the reserve */
  buffer->ap.alloc = AddrAdd(buffer->ap.alloc, size);

  *pReturn = p;
  return ResOK;
}


/* pcAMCBufferTrip -- deal with a tripped buffer
@@@@
 *
 * A flip occurred between a reserve and commit on a buffer, and
 * the buffer was "tripped" (limit set to zero).  The object wasn't
 * scanned, and must therefore be assumed to be invalid, so the
 * reservation must be rolled back. This function detaches the
 * buffer from the seg completely.  The next allocation in the
 * buffer will cause a refill, and reach pcAMCFill.
 */
Bool TraceBufferTrip(Pool pool, Buffer buffer, Addr p, Size size)
{
  Space space;

  AVERT(Buffer, buffer);
  AVER(p != 0);                 /* address of object */
  AVER(size > 0);               /* size of object */
  AVER(PoolHasAddr(pool, p));
  AVER(!BufferIsReset(buffer));
  AVER(BufferIsReady(buffer));
  AVER(buffer->ap.limit == 0);  /* buffer has been tripped */

  space = PoolSpace(pool);

/*
  @@@@ should be in flipped state
  @@@@ could seg be protected??? yes?
  PoolDetach(@@@@);
*/
  TraceBufferReady(space, buffer);
  TraceBufferDetach(space, buffer);

  return FALSE;                 /* cause commit to fail */
}

void TraceBufferFinish(Buffer buffer)
{
  Space space = BufferSpace(buffer);

  if(BufferIsReset(buffer)) 
    return;

  TraceBufferReady(space, buffer);
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

/* TraceSegGrey - turn all white into all grey */
void TraceSegGrey(Space space, ScanState ss, Seg seg)
{
  seg->condemned = TraceIdNONE;     /* remove seg from old-space */
  seg->grey =                       /* mark it for later scanning */
    TraceSetAdd(seg->grey, ss->traceId);
  /* @@@@.barrier.read-write */
  /* @@@@ + only if not white for any trace */
  ShieldRaise(space, seg, AccessREAD | AccessWRITE);
}

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
    if(ss->traceId == seg->condemned) {
      Res res;
      pool = seg->pool;
      res = PoolFix(pool, ss, seg, refIO);
      if(res) return res;
    }
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
  ss.traceId = ti;
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
