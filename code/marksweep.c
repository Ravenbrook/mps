/* marksweep.c: MARK-AND-SWEEP SEGMENTS
 *
 * $Id$
 * Copyright (c) 2018-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .design: The design for this module is <design/seg>.
 *
 * .purpose: This is the implementation of the mark-and-sweep segment
 * class MarkSweepSeg.
 */

#include "mpm.h"


/* MarkSweepSegStruct -- leaf-only mark-and-sweep segment structure
 *
 * Suitable for objects with no references to automatically managed
 * memory, that must not be moved or protected.
 *
 * Colour is represented as follows:
 * Black: +alloc +mark
 * White: +alloc -mark
 * Grey: objects have no references so can't be grey
 * Free: -alloc ?mark
 */

typedef struct MarkSweepSegStruct *MarkSweepSeg;

#define MarkSweepSegSig      ((Sig)0x519355E9) /* SIGnature Mark Sweep SEG */

typedef struct MarkSweepSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  BT mark;                  /* mark bit table */
  BT alloc;                 /* alloc bit table */
  Count freeGrains;         /* free grains */
  Count bufferedGrains;     /* grains in buffers */
  Count newGrains;          /* grains allocated since last collection */
  Count oldGrains;          /* grains allocated prior to last collection */
  Sig sig;                  /* <code/misc.h#sig> */
} MarkSweepSegStruct;


/* MarkSweepSegCheck -- check a mark-and-sweep segment */

ATTRIBUTE_UNUSED
static Bool MarkSweepSegCheck(MarkSweepSeg msseg)
{
  Seg seg = MustBeA(Seg, msseg);
  Pool pool = SegPool(seg);
  CHECKS(MarkSweepSeg, msseg);
  CHECKD(GCSeg, &msseg->gcSegStruct);
  CHECKD_NOSIG(BT, msseg->mark);
  CHECKD_NOSIG(BT, msseg->alloc);
  CHECKL(msseg->freeGrains + msseg->bufferedGrains + msseg->newGrains
         + msseg->oldGrains == PoolSizeGrains(pool, SegSize(seg)));
  return TRUE;
}


/* markSweepSegInit -- initialize a mark-and-sweep segment */

static Res markSweepSegInit(Seg seg, Pool pool, Addr base, Size size,
                            ArgList args)
{
  MarkSweepSeg msseg;
  Res res;
  Size tableSize;      /* # bytes in each control array */
  Arena arena = PoolArena(pool);
  /* number of bits needed in each control array */
  Count segGrains;
  void *p;

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, MarkSweepSeg, init)(seg, pool, base, size, args);
  if (res != ResOK)
    goto failSuperInit;
  msseg = CouldBeA(MarkSweepSeg, seg);

  AVER(SegWhite(seg) == TraceSetEMPTY);

  segGrains = PoolSizeGrains(pool, size);
  tableSize = BTSize(segGrains);
  res = ControlAlloc(&p, arena, 2 * tableSize);
  if (res != ResOK)
    goto failControlAlloc;
  msseg->mark = p;
  msseg->alloc = PointerAdd(p, tableSize);
  BTResRange(msseg->alloc, 0, segGrains);
  msseg->freeGrains = segGrains;
  msseg->bufferedGrains = (Count)0;
  msseg->newGrains = (Count)0;
  msseg->oldGrains = (Count)0;

  SetClassOfPoly(seg, CLASS(MarkSweepSeg));
  msseg->sig = MarkSweepSegSig;
  AVERC(MarkSweepSeg, msseg);

  return ResOK;

failControlAlloc:
  NextMethod(Inst, MarkSweepSeg, finish)(MustBeA(Inst, seg));
failSuperInit:
  AVER(res != ResOK);
  return res;
}


/* markSweepSegFinish -- Finish method for mark-and-sweep segments */

static void markSweepSegFinish(Inst inst)
{
  Seg seg = MustBeA(Seg, inst);
  MarkSweepSeg msseg = MustBeA(MarkSweepSeg, seg);
  Pool pool = SegPool(seg);
  Arena arena = PoolArena(pool);
  Count segGrains = PoolSizeGrains(pool, SegSize(seg));
  Size tableSize = BTSize(segGrains);

  AVER(!SegHasBuffer(seg));
  AVER(msseg->bufferedGrains == 0);
  AVER(segGrains == msseg->freeGrains + msseg->oldGrains + msseg->newGrains);
  PoolGenFree(PoolSegPoolGen(pool, seg), seg,
              PoolGrainsSize(pool, msseg->freeGrains),
              PoolGrainsSize(pool, msseg->oldGrains),
              PoolGrainsSize(pool, msseg->newGrains),
              FALSE);

  msseg->sig = SigInvalid;
  ControlFree(arena, msseg->mark, 2 * tableSize);
  NextMethod(Inst, MarkSweepSeg, finish)(inst);
}


/* markSweepSegBufferFill -- try filling buffer from mark-and-sweep segment */

static Bool markSweepSegBufferFill(Addr *baseReturn, Addr *limitReturn,
                                   Seg seg, Size size, RankSet rankSet)
{
  Index baseIndex, limitIndex;
  MarkSweepSeg msseg = MustBeA_CRITICAL(MarkSweepSeg, seg);
  Pool pool = SegPool(seg);
  Count requestedGrains, segGrains, allocatedGrains;
  Addr segBase, base, limit;

  AVER_CRITICAL(baseReturn != NULL);
  AVER_CRITICAL(limitReturn != NULL);
  AVER_CRITICAL(SizeIsAligned(size, PoolAlignment(pool)));
  AVER_CRITICAL(size > 0);
  AVER_CRITICAL(rankSet == RankSetEMPTY);

  requestedGrains = PoolSizeGrains(pool, size);
  if (msseg->freeGrains < requestedGrains)
    /* Not enough space to satisfy the request. */
    return FALSE;

  if (SegHasBuffer(seg))
    /* Don't bother trying to allocate from a buffered segment */
    return FALSE;

  segGrains = PoolSizeGrains(pool, SegSize(seg));
  if (msseg->freeGrains == segGrains) {
    /* Whole segment is free: no need for a search. */
    baseIndex = 0;
    limitIndex = segGrains;
    goto found;
  }

  if (!BTFindLongResRange(&baseIndex, &limitIndex, msseg->alloc,
                          0, segGrains, requestedGrains))
    return FALSE;

found:
  AVER(baseIndex < limitIndex);
  allocatedGrains = limitIndex - baseIndex;
  AVER(requestedGrains <= allocatedGrains);
  AVER(BTIsResRange(msseg->alloc, baseIndex, limitIndex));
  /* Objects are allocated black. */
  /* TODO: This should depend on trace phase. */
  BTSetRange(msseg->alloc, baseIndex, limitIndex);
  BTSetRange(msseg->mark, baseIndex, limitIndex);
  AVER(msseg->freeGrains >= allocatedGrains);
  msseg->freeGrains -= allocatedGrains;
  msseg->bufferedGrains += allocatedGrains;

  segBase = SegBase(seg);
  base = PoolAddrOfIndex(segBase, pool, baseIndex);
  limit = PoolAddrOfIndex(segBase, pool, limitIndex);
  PoolGenAccountForFill(PoolSegPoolGen(pool, seg), AddrOffset(base, limit));

  *baseReturn = base;
  *limitReturn = limit;
  return TRUE;
}


/* markSweepSegBufferEmpty -- empty buffer to mark-and-sweep segment */

static void markSweepSegBufferEmpty(Seg seg, Buffer buffer)
{
  MarkSweepSeg msseg = MustBeA(MarkSweepSeg, seg);
  Pool pool = SegPool(seg);
  Addr segBase, bufferBase, init, limit;
  Index initIndex, limitIndex;
  Count unusedGrains, usedGrains;

  AVERT(Seg, seg);
  AVERT(Buffer, buffer);
  segBase = SegBase(seg);
  bufferBase = BufferBase(buffer);
  init = BufferGetInit(buffer);
  limit = BufferLimit(buffer);
  AVER(segBase <= bufferBase);
  AVER(bufferBase <= init);
  AVER(init <= limit);
  AVER(limit <= SegLimit(seg));

  initIndex = PoolIndexOfAddr(segBase, pool, init);
  limitIndex = PoolIndexOfAddr(segBase, pool, limit);

  if (initIndex < limitIndex)
    BTResRange(msseg->alloc, initIndex, limitIndex);

  unusedGrains = limitIndex - initIndex;
  AVER(unusedGrains <= msseg->bufferedGrains);
  usedGrains = msseg->bufferedGrains - unusedGrains;
  msseg->freeGrains += unusedGrains;
  msseg->bufferedGrains = 0;
  msseg->newGrains += usedGrains;

  PoolGenAccountForEmpty(PoolSegPoolGen(pool, seg),
                         PoolGrainsSize(pool, usedGrains),
                         PoolGrainsSize(pool, unusedGrains), FALSE);
}


/* markSweepSegWhiten -- whiten a mark-and-sweep segment */

static Res markSweepSegWhiten(Seg seg, Trace trace)
{
  MarkSweepSeg msseg = MustBeA(MarkSweepSeg, seg);
  Pool pool = SegPool(seg);
  PoolGen pgen = PoolSegPoolGen(pool, seg);
  Buffer buffer;
  Count segGrains, agedGrains, uncondemnedGrains;

  AVERT(Trace, trace);
  AVER(SegWhite(seg) == TraceSetEMPTY);

  segGrains = PoolSizeGrains(pool, SegSize(seg));

  /* Whiten allocated objects; leave free areas black. */
  if (SegBuffer(&buffer, seg)) {
    Addr base = SegBase(seg);
    Index scanLimitIndex = PoolIndexOfAddr(base, pool, BufferScanLimit(buffer));
    Index limitIndex = PoolIndexOfAddr(base, pool, BufferLimit(buffer));
    uncondemnedGrains = limitIndex - scanLimitIndex;
    if (0 < scanLimitIndex)
      BTCopyInvertRange(msseg->alloc, msseg->mark, 0, scanLimitIndex);
    if (limitIndex < segGrains)
      BTCopyInvertRange(msseg->alloc, msseg->mark, limitIndex, segGrains);
  } else {
    uncondemnedGrains = (Count)0;
    BTCopyInvertRange(msseg->alloc, msseg->mark, 0, segGrains);
  }

  /* The unused part of the buffer remains buffered: the rest becomes old. */
  AVER(msseg->bufferedGrains >= uncondemnedGrains);
  agedGrains = msseg->bufferedGrains - uncondemnedGrains;
  PoolGenAccountForAge(pgen, PoolGrainsSize(pool, agedGrains),
                       PoolGrainsSize(pool, msseg->newGrains), FALSE);
  msseg->oldGrains += agedGrains + msseg->newGrains;
  msseg->bufferedGrains = uncondemnedGrains;
  msseg->newGrains = 0;

  if (msseg->oldGrains > 0) {
    GenDescCondemned(pgen->gen, trace,
                     PoolGrainsSize(pool, msseg->oldGrains));
    SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  }

  return ResOK;
}


/* markSweepSegReclaim -- reclaim white objects in a mark-and-sweep segment */

static void markSweepSegReclaim(Seg seg, Trace trace)
{
  MarkSweepSeg msseg = MustBeA(MarkSweepSeg, seg);
  Pool pool = SegPool(seg);
  PoolGen pgen = PoolSegPoolGen(pool, seg);
  Addr base;
  Index i = 0, segGrains;
  Buffer buffer;
  Bool hasBuffer = SegBuffer(&buffer, seg);
  Count reclaimedGrains = (Count)0;
  Format format = NULL; /* suppress "may be used uninitialized" warning */
  Count preservedCount = (Count)0;
  Count preservedGrains = (Count)0;
  Bool b;

  AVERT(Trace, trace);

  b = PoolFormat(&format, pool);
  AVER(b);

  base = SegBase(seg);
  segGrains = PoolSizeGrains(pool, SegSize(seg));

  while (i < segGrains) {
    /* "object" is a misnomer as it may refer to a free grain */
    Addr object = PoolAddrOfIndex(base, pool, i);
    Addr next;
    Index j;

    if (hasBuffer) {
      if (object == BufferScanLimit(buffer)
          && BufferScanLimit(buffer) != BufferLimit(buffer))
      {
        /* Skip buffered area. */
        i = PoolIndexOfAddr(base, pool, BufferLimit(buffer));
        continue;
      }
      /* Since we skip over the buffered area we are always either
         before the buffer, or after it, never in it. */
      AVER(object < BufferGetInit(buffer) || BufferLimit(buffer) <= object);
    }
    if (!BTGet(msseg->alloc, i)) {
      /* This grain is free. */
      ++ i;
      continue;
    }
    object = AddrAdd(object, format->headerSize);
    next = (*format->skip)(object);
    next = AddrSub(next, format->headerSize);
    j = PoolIndexOfAddr(base, pool, next);
    AVER(i < j);
    if (BTGet(msseg->mark, i)) {
      /* object is marked, so preserve it */
      ++ preservedCount;
      preservedGrains += j - i;
    } else {
      /* object is not marked, so free it */
      BTResRange(msseg->alloc, i, j);
      reclaimedGrains += j - i;
    }
    i = j;
  }
  AVER(i == segGrains);

  AVER(reclaimedGrains <= segGrains);
  AVER(msseg->oldGrains >= reclaimedGrains);
  msseg->oldGrains -= reclaimedGrains;
  msseg->freeGrains += reclaimedGrains;
  PoolGenAccountForReclaim(pgen, PoolGrainsSize(pool, reclaimedGrains), FALSE);

  STATISTIC(trace->reclaimSize += PoolGrainsSize(pool, reclaimedGrains));
  STATISTIC(trace->preservedInPlaceCount += preservedCount);
  GenDescSurvived(pgen->gen, trace, 0, PoolGrainsSize(pool, preservedGrains));
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));

  if (msseg->freeGrains == segGrains && !hasBuffer) {
    SegFree(seg);
  }
}


/* markSweepSegWalk -- walk objects in a mark-and-sweep segment
 *
 * Walks over _all_ objects in the segment: whether they are black or
 * white, they are still validly formatted as this is a leaf pool, so
 * there can't be any dangling references.
 */
static void markSweepSegWalk(Seg seg, Format format, FormattedObjectsVisitor f,
                             void *p, size_t s)
{
  MarkSweepSeg msseg = MustBeA(MarkSweepSeg, seg);
  Pool pool = SegPool(seg);
  Addr base;
  Index i = 0, segGrains;
  Buffer buffer;
  Bool hasBuffer = SegBuffer(&buffer, seg);

  AVERT(Format, format);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  base = SegBase(seg);
  segGrains = PoolSizeGrains(pool, SegSize(seg));

  while (i < segGrains) {
    /* "object" is a misnomer as it may refer to a free grain */
    Addr object = PoolAddrOfIndex(base, pool, i);
    Addr next;
    Index j;

    if (hasBuffer) {
      if (object == BufferScanLimit(buffer)
          && BufferScanLimit(buffer) != BufferLimit(buffer))
      {
        /* Skip buffered area. */
        i = PoolIndexOfAddr(base, pool, BufferLimit(buffer));
        continue;
      }
      /* Since we skip over the buffered area we are always either
         before the buffer, or after it, never in it. */
      AVER(object < BufferGetInit(buffer) || BufferLimit(buffer) <= object);
    }
    if (!BTGet(msseg->alloc, i)) {
      /* This grain is free. */
      ++ i;
      continue;
    }
    object = AddrAdd(object, format->headerSize);
    next = (*format->skip)(object);
    next = AddrSub(next, format->headerSize);
    j = PoolIndexOfAddr(base, pool, next);
    AVER(i < j);
    (*f)(object, format, pool, p, s);
    i = j;
  }
  AVER(i == segGrains);
}


/* markSweepSegFix -- fix a reference to a mark-and-sweep segment */

static Res markSweepSegFix(Seg seg, ScanState ss, Ref *refIO)
{
  MarkSweepSeg msseg = MustBeA_CRITICAL(MarkSweepSeg, seg);
  Pool pool = SegPool(seg);
  Ref clientRef;
  Addr base;
  Index i;

  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER_CRITICAL(refIO != NULL);

  clientRef = *refIO;
  base = AddrSub((Addr)clientRef, pool->format->headerSize);

  /* Not a real reference if out of bounds. This can happen if an
     ambiguous reference is closer to the base of the segment than the
     header size. */
  if (base < SegBase(seg)) {
    AVER(ss->rank == RankAMBIG);
    return ResOK;
  }

  /* Not a real reference if unaligned. */
  if (!AddrIsAligned(base, PoolAlignment(pool))) {
    AVER(ss->rank == RankAMBIG);
    return ResOK;
  }

  i = PoolIndexOfAddr(SegBase(seg), pool, base);

  /* Not a real reference if unallocated. */
  if (!BTGet(msseg->alloc, i)) {
    AVER(ss->rank == RankAMBIG);
    return ResOK;
  }

  if (!BTGet(msseg->mark, i)) {
    ss->wasMarked = FALSE;  /* <design/fix#.was-marked.not> */
    if (ss->rank == RankWEAK) {
      *refIO = (Addr)0;
    } else {
      BTSet(msseg->mark, i);
    }
  }

  return ResOK;
}


/* MarkSweepSegClass -- class definition for mark-and-sweep segments */

DEFINE_CLASS(Seg, MarkSweepSeg, klass)
{
  INHERIT_CLASS(klass, MarkSweepSeg, MutatorSeg);
  SegClassMixInNoSplitMerge(klass);
  klass->size = sizeof(MarkSweepSegStruct);
  klass->init = markSweepSegInit;
  klass->instClassStruct.finish = markSweepSegFinish;
  klass->bufferFill = markSweepSegBufferFill;
  klass->bufferEmpty = markSweepSegBufferEmpty;
  klass->whiten = markSweepSegWhiten;
  klass->reclaim = markSweepSegReclaim;
  klass->walk = markSweepSegWalk;
  klass->fix = markSweepSegFix;
  klass->fixEmergency = markSweepSegFix;
  AVERT(SegClass, klass);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2018-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
