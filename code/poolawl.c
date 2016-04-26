/* poolawl.c: AUTOMATIC WEAK LINKED POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 *
 * DESIGN
 *
 * .design: See <design/poolawl/>.  This is Dylan-specific pool.
 *
 *
 * ASSUMPTIONS (about when to scan single references on accesses)
 *
 * .assume.purpose: The purpose of scanning refs singly is to limit the
 * amount of scanning of weak references which must be performed when
 * the mutator hits a barrier. Weak references which are scanned at this
 * time are not "weak splatted". Minimizing any loss of weak splats
 * potentially reduces conservatism in the collector.
 *
 * .assume.noweak: It follows (from .assume.purpose) that there is no
 * benefit from scanning single refs on barrier accesses for segments
 * which don't contain any weak references. However, if a segment
 * contains either all weak refs or a mixture of weak and non-weak
 * references then there is a potential benefit.
 *
 * .assume.mixedrank: If a segment contains a mixture of references
 * at different ranks (e.g. weak and strong references), there is
 * no way to determine whether or not references at a rank other than
 * the scan state rank will be  scanned as a result of normal
 * (non-barrier) scanning  activity. (@@@@ This is a deficiency in MPS).
 * Assume that such references will, in fact, be scanned at the
 * incorrect rank.
 *
 * .assume.samerank: The pool doesn't support segments with mixed
 * rank segments in any case (despite .assume.mixedrank).
 *
 * .assume.alltraceable: The pool assumes that all objects are entirely
 * traceable. This must be documented elsewhere for the benefit of the
 * client.
 */

#include "mpscawl.h"
#include "mpm.h"
#include "locus.h"

SRCID(poolawl, "$Id$");


#define AWLSig ((Sig)0x519B7A37) /* SIGnature PooL AWL */


/* awlStat* -- Statistics gathering about instruction emulation
 *
 * To support change.dylan.2.0.160044.
 */


/* Per-segment statistics maintained between segment scans */

typedef struct awlStatSegStruct {
  Count sameAccesses;  /* accesses involving same address as last access */
  Addr lastAccess;     /* the address of last access */
} awlStatSegStruct, *awlStatSeg;

/* Per-pool statistics updated at segment scans */

typedef struct awlStatTotalStruct {
  Count goodScans;     /* total times a segment scanned at proper rank */
  Count badScans;      /* total times a segment scanned at improper rank */
  Count savedScans;    /* total times an entire segment scan was avoided */
  Count savedAccesses; /* total single references leading to a saved scan */
  Count declined;      /* number of declined single accesses */
} awlStatTotalStruct, *awlStatTotal;

/* the type of a function to find an object's dependent object */

typedef Addr (*FindDependentFunction)(Addr object);

/* AWLStruct -- AWL pool structure
 *
 * See <design/poolawl/#poolstruct>
 */

typedef struct AWLPoolStruct {
  PoolStruct poolStruct;
  Shift alignShift;
  PoolGenStruct pgenStruct; /* generation representing the pool */
  PoolGen pgen;             /* NULL or pointer to pgenStruct */
  Count succAccesses;       /* number of successive single accesses */
  FindDependentFunction findDependent; /*  to find a dependent object */
  awlStatTotalStruct stats;
  Sig sig;
} AWLPoolStruct, *AWL;

#define AWLGrainsSize(awl, grains) ((grains) << (awl)->alignShift)


static Bool AWLCheck(AWL awl);


typedef AWL AWLPool;
#define AWLPoolCheck AWLCheck
DECLARE_CLASS(Pool, AWLPool, AbstractCollectPool);
typedef SegBuf AWLBuffer;
#define AWLBufferCheck SegBufCheck
DECLARE_CLASS(Buffer, AWLBuffer, RankBuf);


/* Conversion between indexes and Addrs */
#define awlIndexOfAddr(base, awl, p) \
  (AddrOffset((base), (p)) >> (awl)->alignShift)
#define awlAddrOfIndex(base, awl, i) \
  AddrAdd(base, AWLGrainsSize(awl, i))


/* AWLSegStruct -- AWL segment subclass
 *
 * Subclass of GCSeg
 */

#define AWLSegSig ((Sig)0x519A3759) /* SIGnature AWL SeG */

/* <design/poolawl/#seg> */
typedef struct AWLSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  BT mark;                  /* NULL or mark bit table on white segs */
  BT scanned;               /* NULL or scanned table on white segs */
  BT alloc;                 /* alloc bit table */
  Count freeGrains;         /* free grains */
  Count bufferedGrains;     /* grains in buffers */
  Count newGrains;          /* grains allocated since last collection */
  Count oldGrains;          /* grains allocated prior to last collection */
  Count singleAccesses;     /* number of accesses processed singly */
  awlStatSegStruct stats;
  Sig sig;
} AWLSegStruct, *AWLSeg;

DECLARE_CLASS(Seg, AWLSeg, GCSeg);


ATTRIBUTE_UNUSED
static Bool AWLSegCheck(AWLSeg awlseg)
{
  Seg seg = MustBeA(Seg, awlseg);
  AWL awl = MustBeA(AWLPool, SegPool(seg));
  CHECKS(AWLSeg, awlseg);
  CHECKD(GCSeg, &awlseg->gcSegStruct);
  CHECKL((SegWhite(seg) == TraceSetEMPTY) == (awlseg->mark == NULL));
  CHECKL((SegWhite(seg) == TraceSetEMPTY) == (awlseg->scanned == NULL));
  CHECKL(awlseg->alloc != NULL);
  CHECKL(awlseg->freeGrains + awlseg->bufferedGrains +
         awlseg->newGrains + awlseg->oldGrains ==
         SegSize(seg) >> awl->alignShift);
  return TRUE;
}


/* Management of statistics for monitoring protection-driven accesses */

static void awlStatSegInit(AWLSeg awlseg)
{
  awlseg->stats.sameAccesses = 0;
  awlseg->stats.lastAccess = NULL;
}

static void awlStatTotalInit(AWL awl)
{
  awl->stats.goodScans = 0;
  awl->stats.badScans = 0;
  awl->stats.savedAccesses = 0;
  awl->stats.savedScans = 0;
  awl->stats.declined = 0;
}


/* AWLSegInit -- Init method for AWL segments */

ARG_DEFINE_KEY(awl_seg_rank_set, RankSet);
#define awlKeySegRankSet (&_mps_key_awl_seg_rank_set)

static Res AWLSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  AWLSeg awlseg;
  AWL awl = MustBeA(AWLPool, pool);
  Arena arena;
  RankSet rankSet;
  Count grains;
  Res res;
  void *v;
  ArgStruct arg;

  ArgRequire(&arg, args, awlKeySegRankSet);
  rankSet = arg.val.u;
  AVERT(RankSet, rankSet);
  /* .assume.samerank */
  /* AWL only accepts two ranks */
  AVER(RankSetSingle(RankEXACT) == rankSet
       || RankSetSingle(RankWEAK) == rankSet);

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, AWLSeg, init)(seg, pool, base, size, args);
  if (res != ResOK)
    goto failSuperInit;
  awlseg = CouldBeA(AWLSeg, seg);

  AVERT(Pool, pool);
  arena = PoolArena(pool);
  /* no useful checks for base and size */

  grains = size >> awl->alignShift;
  res = ControlAlloc(&v, arena, BTSize(grains));
  if (res != ResOK)
    goto failAllocTable;
  awlseg->alloc = v;
  BTResRange(awlseg->alloc, 0, grains);
  awlseg->mark = NULL;
  awlseg->scanned = NULL;
  SegSetRankAndSummary(seg, rankSet, RefSetUNIV);
  awlseg->freeGrains = grains;
  awlseg->bufferedGrains = (Count)0;
  awlseg->newGrains = (Count)0;
  awlseg->oldGrains = (Count)0;
  awlseg->singleAccesses = 0;
  awlStatSegInit(awlseg);

  SetClassOfPoly(seg, CLASS(AWLSeg));
  awlseg->sig = AWLSegSig;
  AVERC(AWLSeg, awlseg);

  return ResOK;

failAllocTable:
  NextMethod(Seg, AWLSeg, finish)(seg);
failSuperInit:
  AVER(res != ResOK);
  return res;
}


/* AWLSegFinish -- Finish method for AWL segments */

static void AWLSegFinish(Seg seg)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Pool pool = SegPool(seg);
  AWL awl = MustBeA(AWLPool, pool);
  Arena arena = PoolArena(pool);
  Size tableSize;
  Count grains;

  /* This is one of the few places where it is easy to check */
  /* awlseg->grains, so we do */
  grains = SegSize(seg) >> awl->alignShift;
  tableSize = BTSize(grains);
  ControlFree(arena, awlseg->alloc, tableSize);
  if (awlseg->scanned != NULL)
    ControlFree(arena, awlseg->scanned, tableSize);
  if (awlseg->mark != NULL)
    ControlFree(arena, awlseg->mark, tableSize);
  awlseg->sig = SigInvalid;

  /* finish the superclass fields last */
  NextMethod(Seg, AWLSeg, finish)(seg);
}


/* AWLSegClass -- Class definition for AWL segments */

DEFINE_CLASS(Seg, AWLSeg, klass)
{
  INHERIT_CLASS(klass, AWLSeg, GCSeg);
  SegClassMixInNoSplitMerge(klass);  /* no support for this (yet) */
  klass->size = sizeof(AWLSegStruct);
  klass->init = AWLSegInit;
  klass->finish = AWLSegFinish;
}


/* TODO: Any way to avoid this having to exist? */
/* TODO: Duplicate of loBufferInit. */
static Res awlBufferInit(Buffer buffer, Pool pool, Bool isMutator, ArgList args)
{
  Res res = NextMethod(Buffer, AWLBuffer, init)(buffer, pool, isMutator, args);
  if (res != ResOK)
    return res;
  SetClassOfPoly(buffer, CLASS(AWLBuffer));
  return ResOK;
}


ATTRIBUTE_UNUSED
static Count awlSegGrains(AWLSeg awlseg)
{
  Seg seg = MustBeA(Seg, awlseg);
  AWL awl = MustBeA(AWLPool, SegPool(seg));
  Size size = SegSize(seg);
  return size >> awl->alignShift;
}


/* awlRangeBlacken -- helper function that works on a range */

static void awlRangeBlacken(AWLSeg awlseg, Index base, Index limit)
{
  if (base != limit) {
    AVER(base < limit);
    AVER(limit <= awlSegGrains(awlseg));
    BTSetRange(awlseg->mark, base, limit);
    BTSetRange(awlseg->scanned, base, limit);
  }
}


/* TODO: Duplicate of loBufferFlip, except that it sets "scanned". */
static void awlBufferFlip(Buffer buffer, Trace trace)
{
  Seg seg;
  AWLSeg awlseg;
  AWL awl;
  Addr segBase, init, limit;
  Index bufferBaseIndex, initIndex, limitIndex;
  Size wasBuffered;
  Count agedGrains;
  
  NextMethod(Buffer, AWLBuffer, flip)(buffer, trace);

  seg = BufferSeg(buffer);
  if (seg == NULL || !TraceSetIsMember(SegWhite(seg), trace))
    return;

  awlseg = MustBeA(AWLSeg, seg);
  awl = MustBeA(AWLPool, BufferPool(buffer));

  /* .flip.age: Any objects between the buffer base and init were
     allocated white, so account them as old. */
  /* FIXME: Common code with amcBufFlip. */
  init = BufferScanLimit(buffer);
  wasBuffered = AddrOffset(BufferBase(buffer), init);
  PoolGenAccountForAge(awl->pgen, wasBuffered, 0, FALSE);

  /* Repeat the above accounting locally. */
  segBase = SegBase(seg);
  bufferBaseIndex = awlIndexOfAddr(segBase, awl, BufferBase(buffer));
  initIndex = awlIndexOfAddr(segBase, awl, init);
  agedGrains = initIndex - bufferBaseIndex;
  awlseg->oldGrains += agedGrains;
  AVER(awlseg->bufferedGrains >= agedGrains);
  awlseg->bufferedGrains -= agedGrains;

  /* .flip.condemned: Account for these objects to trace->condemned
     for the traces for white they are now white.  TODO: Need to
     iterate over SegWhite(seg) if segments can be condemend for
     multiple traces. */
  AVER(SegWhite(seg) == TraceSetSingle(trace));
  trace->condemned += wasBuffered;

  /* .flip.base: Shift the buffer base up over them, to keep the total
     buffered account equal to the total size of the buffers. */
  /* FIXME: Common code with amcBufFlip. */
  buffer->base = init; /* FIXME: Abstract this */

  /* .flip.mark: After the flip, the mutator is allocating black.
     Mark the unused part of the buffer to ensure the objects there
     stay alive.  When the buffer is emptied, allocations in this area
     will be accounted for as new. */
  limit = BufferLimit(buffer);
  limitIndex = awlIndexOfAddr(segBase, awl, limit);
  if (initIndex < limitIndex) /* FIXME: Could detach if empty? */
    awlRangeBlacken(awlseg, initIndex, limitIndex);
}
  

DEFINE_CLASS(Buffer, AWLBuffer, klass)
{
  INHERIT_CLASS(klass, AWLBuffer, RankBuf);
  klass->init = awlBufferInit;
  klass->flip = awlBufferFlip;
}


/* Single access pattern control parameters
 *
 * These control the number of expensive emulated single-accesses we
 * allow before we give up and scan a segment at whatever rank,
 * possibly causing retention of weak objects.
 *
 * AWLSegSALimit is the number of accesses for a single segment in a
 * GC cycle.  AWLTotalSALimit is the total number of accesses during a
 * GC cycle.
 *
 * These should be set in config.h, but are here in static variables
 * so that it's possible to tweak them in a debugger.
 */

extern Count AWLSegSALimit;
Count AWLSegSALimit = AWL_SEG_SA_LIMIT;
extern Bool AWLHaveSegSALimit;
Bool AWLHaveSegSALimit = AWL_HAVE_SEG_SA_LIMIT;

extern Count AWLTotalSALimit;
Count AWLTotalSALimit = AWL_TOTAL_SA_LIMIT;
extern Bool AWLHaveTotalSALimit;
Bool AWLHaveTotalSALimit = AWL_HAVE_TOTAL_SA_LIMIT;


/* Determine whether to permit scanning a single ref. */

static Bool AWLCanTrySingleAccess(Arena arena, AWL awl, Seg seg, Addr addr)
{
  AWLSeg awlseg;

  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);

  /* .assume.noweak */
  /* .assume.alltraceable */
  if (!RankSetIsMember(SegRankSet(seg), RankWEAK))
    return FALSE;

  /* If there are no traces in progress then the segment isn't read
     protected and this is just an ordinary write barrier hit.  No need to
     scan at all. */
  if (arena->flippedTraces == TraceSetEMPTY) {
    AVER(!(SegSM(seg) & AccessREAD));
    return FALSE;
  }

  /* The trace is already in the weak band, so we can scan the whole
     segment without retention anyway.  Go for it. */
  if (TraceRankForAccess(arena, seg) == RankWEAK)
    return FALSE;

  awlseg = MustBeA(AWLSeg, seg);

  /* If there have been too many single accesses in a row then don't
     keep trying them, even if it means retaining objects. */
  if(AWLHaveTotalSALimit) {
    if(awl->succAccesses >= AWLTotalSALimit) {
      STATISTIC(awl->stats.declined++);
      EVENT2(AWLDeclineTotal, seg, (EventFU)awl->succAccesses);
      return FALSE; /* decline single access because of total limit */
    }
  }

  /* If there have been too many single accesses to this segment
     then don't keep trying them, even if it means retaining objects.
     (Observed behaviour in Open Dylan 2012-09-10 by RB.) */
  if(AWLHaveSegSALimit) {
    if(awlseg->singleAccesses >= AWLSegSALimit) {
      STATISTIC(awl->stats.declined++);
      EVENT2(AWLDeclineSeg, seg, (EventFU)awlseg->singleAccesses);
      return FALSE; /* decline single access because of segment limit */
    }
  }

  return TRUE;
}


/* Record an access to a segment which required scanning a single ref */

static void AWLNoteRefAccess(AWL awl, Seg seg, Addr addr)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);

  AVERT(AWL, awl);
  AVER(addr != NULL);

  awlseg->singleAccesses++; /* increment seg count of ref accesses */
  if (addr == awlseg->stats.lastAccess) {
    /* If this is a repeated access, increment count  */
    STATISTIC(awlseg->stats.sameAccesses++);
  }
  STATISTIC(awlseg->stats.lastAccess = addr);
  awl->succAccesses++;  /* Note a new successive access */
}


/* Record an access to a segment which required scanning the entire seg */

static void AWLNoteSegAccess(AWL awl, Seg seg, Addr addr)
{
  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);

  awl->succAccesses = 0; /* reset count of successive accesses */
}


/* Record a scan of a segment which wasn't provoked by an access */

static void AWLNoteScan(AWL awl, Seg seg, ScanState ss)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);

  AVERT(AWL, awl);

  /* .assume.mixedrank */
  /* .assume.samerank */
  /* If this segment has any RankWEAK references, then  */
  /* record statistics about whether weak splatting is being lost. */
  if (RankSetIsMember(SegRankSet(seg), RankWEAK)) {
    if (RankWEAK == ss->rank) {
      /* This is "successful" scan at proper rank. */
      STATISTIC(awl->stats.goodScans++);
      if (0 < awlseg->singleAccesses) {
        /* Accesses have been proceesed singly */
        /* Record that we genuinely did save a protection-provoked scan */
        STATISTIC(awl->stats.savedScans++);
        STATISTIC(awl->stats.savedAccesses += awlseg->singleAccesses);
      }
    } else {
      /* This is "failed" scan at improper rank. */
      STATISTIC(awl->stats.badScans++);
    }
    /* Reinitialize the segment statistics */
    awlseg->singleAccesses = 0;
    STATISTIC(awlStatSegInit(awlseg));
  }
}


/* AWLSegCreate -- Create a new segment of at least given size */

static Res AWLSegCreate(AWLSeg *awlsegReturn,
                        RankSet rankSet, Pool pool, Size size)
{
  AWL awl = MustBeA(AWLPool, pool);
  Arena arena = PoolArena(pool);
  Seg seg;
  Res res;

  AVER(awlsegReturn != NULL);
  AVERT(RankSet, rankSet);
  AVER(size > 0);

  size = SizeArenaGrains(size, arena);
  /* beware of large sizes overflowing upon rounding */
  if (size == 0)
    return ResMEMORY;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD_FIELD(args, awlKeySegRankSet, u, rankSet);
    res = PoolGenAlloc(&seg, awl->pgen, CLASS(AWLSeg), size, args);
  } MPS_ARGS_END(args);
  if (res != ResOK)
    return res;

  *awlsegReturn = MustBeA(AWLSeg, seg);
  return ResOK;
}


/* AWLSegAlloc -- allocate an object in a given segment */

static Bool AWLSegAlloc(Addr *baseReturn, Addr *limitReturn,
                        AWLSeg awlseg, AWL awl, Size size)
{
  Seg seg = MustBeA(Seg, awlseg);
  Count n;        /* number of grains equivalent to alloc size */
  Index i, j;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(AWL, awl);
  AVER(size > 0);
  AVER(AWLGrainsSize(awl, size) >= size);

  if (size > SegSize(seg))
    return FALSE;
  n = size >> awl->alignShift;
  if (!BTFindLongResRange(&i, &j, awlseg->alloc,
                          0, awlSegGrains(awlseg), n))
    return FALSE;
  *baseReturn = awlAddrOfIndex(SegBase(seg), awl, i);
  *limitReturn = awlAddrOfIndex(SegBase(seg),awl, j);
  return TRUE;
}


/* AWLVarargs -- decode obsolete varargs */

static void AWLVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_FORMAT;
  args[0].val.format = va_arg(varargs, Format);
  args[1].key = MPS_KEY_AWL_FIND_DEPENDENT;
  args[1].val.addr_method = va_arg(varargs, mps_awl_find_dependent_t);
  args[2].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}


/* awlNoDependent -- no dependent object */

static Addr awlNoDependent(Addr addr)
{
  UNUSED(addr);
  return NULL;
}


/* AWLInit -- initialize an AWL pool */

ARG_DEFINE_KEY(AWL_FIND_DEPENDENT, Fun);

static Res AWLInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  AWL awl;
  FindDependentFunction findDependent = awlNoDependent;
  Chain chain;
  Res res;
  ArgStruct arg;
  unsigned gen = AWL_GEN_DEFAULT;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);
  UNUSED(klass); /* used for debug pools only */

  if (ArgPick(&arg, args, MPS_KEY_AWL_FIND_DEPENDENT))
    findDependent = (FindDependentFunction)arg.val.addr_method;
  if (ArgPick(&arg, args, MPS_KEY_CHAIN))
    chain = arg.val.chain;
  else {
    chain = ArenaGlobals(arena)->defaultChain;
    gen = 1; /* avoid the nursery of the default chain by default */
  }
  if (ArgPick(&arg, args, MPS_KEY_GEN))
    gen = arg.val.u;

  res = PoolAbsInit(pool, arena, klass, args);
  if (res != ResOK)
    goto failAbsInit;
  awl = CouldBeA(AWLPool, pool);

  /* Ensure a format was supplied in the argument list. */
  AVER(pool->format != NULL);
  pool->alignment = pool->format->alignment;

  AVER(FUNCHECK(findDependent));
  awl->findDependent = findDependent;

  AVERT(Chain, chain);
  AVER(gen <= ChainGens(chain));
  AVER(chain->arena == PoolArena(pool));

  awl->pgen = NULL;

  awl->alignShift = SizeLog2(PoolAlignment(pool));
  awl->succAccesses = 0;
  awlStatTotalInit(awl);

  SetClassOfPoly(pool, CLASS(AWLPool));
  awl->sig = AWLSig;
  AVERC(AWLPool, awl);

  res = PoolGenInit(&awl->pgenStruct, ChainGen(chain, gen), pool);
  if (res != ResOK)
    goto failGenInit;
  awl->pgen = &awl->pgenStruct;

  EVENT2(PoolInitAWL, pool, pool->format);

  return ResOK;

failGenInit:
  PoolAbsFinish(pool);
failAbsInit:
  AVER(res != ResOK);
  return res;
}


/* AWLFinish -- finish an AWL pool */

static void AWLFinish(Pool pool)
{
  AWL awl = MustBeA(AWLPool, pool);
  Ring ring, node, nextNode;

  ring = &pool->segRing;
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    AWLSeg awlseg = MustBeA(AWLSeg, seg);
    AVER(SegBuffer(seg) == NULL);
    AVERT(AWLSeg, awlseg);
    AVER(awlseg->bufferedGrains == 0);
    PoolGenFree(awl->pgen, seg,
                AWLGrainsSize(awl, awlseg->freeGrains),
                AWLGrainsSize(awl, awlseg->oldGrains),
                AWLGrainsSize(awl, awlseg->newGrains),
                FALSE);
  }
  awl->sig = SigInvalid;
  PoolGenFinish(awl->pgen);
  PoolAbsFinish(pool);
}


/* AWLBufferFill -- BufferFill method for AWL
 *
 * TODO: Duplcate of LOBufferFill.
 */

static Res AWLBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  AWL awl = MustBeA(AWLPool, pool);
  Addr base, limit;
  Res res;
  Ring node, nextNode;
  AWLSeg awlseg;
  Seg seg;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERC(Buffer, buffer);
  AVER(size > 0);

  /* Try to find a segment with enough space already. */
  RING_FOR(node, &pool->segRing, nextNode) {
    seg = SegOfPoolRing(node);
    awlseg = MustBeA(AWLSeg, seg);

    /* Only try to allocate in the segment if it is not already */
    /* buffered, and has the same ranks as the buffer. */
    if (SegBuffer(seg) == NULL
        && SegRankSet(seg) == BufferRankSet(buffer)
        && AWLGrainsSize(awl, awlseg->freeGrains) >= size
        && AWLSegAlloc(&base, &limit, awlseg, awl, size))
      goto found;
  }

  /* No segment had enough space, so make a new one. */
  res = AWLSegCreate(&awlseg, BufferRankSet(buffer), pool, size);
  if (res != ResOK)
    return res;
  seg = MustBeA(Seg, awlseg);
  base = SegBase(MustBeA(Seg, awlseg));
  limit = SegLimit(MustBeA(Seg, awlseg));

found:
  {
    Addr segBase = SegBase(seg);
    Index baseIndex = awlIndexOfAddr(segBase, awl, base);
    Index limitIndex = awlIndexOfAddr(segBase, awl, limit);

    AVER(BTIsResRange(awlseg->alloc, baseIndex, limitIndex));
    BTSetRange(awlseg->alloc, baseIndex, limitIndex);

    /* If the segment is condemned, mark the newly buffered region as
       black.  FIXME: Pre-flip, should be white in condemned segments,
       and grey otherwise. */
    if (awlseg->mark != NULL)
      awlRangeBlacken(awlseg, baseIndex, limitIndex);

    AVER(awlseg->freeGrains >= limitIndex - baseIndex);
    awlseg->freeGrains -= limitIndex - baseIndex;
    awlseg->bufferedGrains += limitIndex - baseIndex;
  }

  PoolGenAccountForFill(awl->pgen, AddrOffset(base, limit));

  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;
}


/* AWLBufferEmpty -- BufferEmpty method for AWL */

static void AWLBufferEmpty(Pool pool, Buffer buffer, Addr init, Addr limit)
{
  AWL awl = MustBeA(AWLPool, pool);
  Seg seg = BufferSeg(buffer);
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Addr segBase = SegBase(seg);
  Index i, j;
  Count usedGrains, unusedGrains;

  AVER(init <= limit);

  i = awlIndexOfAddr(segBase, awl, init);
  j = awlIndexOfAddr(segBase, awl, limit);
  AVER(i <= j);
  if (i < j)
    BTResRange(awlseg->alloc, i, j);

  unusedGrains = j - i;
  AVER(awlseg->bufferedGrains >= unusedGrains);
  usedGrains = awlseg->bufferedGrains - unusedGrains;
  awlseg->freeGrains += unusedGrains;
  awlseg->bufferedGrains = 0;
  awlseg->newGrains += usedGrains;
  PoolGenAccountForEmpty(awl->pgen, AWLGrainsSize(awl, usedGrains),
                         AWLGrainsSize(awl, unusedGrains), FALSE);
}


/* AWLWhiten -- segment condemning method */

/* awlRangeWhiten -- helper function that works on a range.
 *
 * This function abstracts common code from AWLWhiten.
 */
static void awlRangeWhiten(AWLSeg awlseg, Index base, Index limit)
{
  if(base != limit) {
    AVER(base < limit);
    AVER(limit <= awlSegGrains(awlseg));
    BTResRange(awlseg->mark, base, limit);
    BTResRange(awlseg->scanned, base, limit);
  }
}


/* AWLWhiten -- whiten a segment
 *
 * FIXME: Common code with LOWhiten.
 */

static Res AWLWhiten(Pool pool, Trace trace, Seg seg)
{
  AWL awl = MustBeA(AWLPool, pool);
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Buffer buffer = SegBuffer(seg);
  Size condemnedSize;
  Size tableSize;
  Res res;
  void *p;

  /* All parameters checked by generic PoolWhiten. */

  /* Can only whiten for a single trace, */
  /* see <design/poolawl/#fun.condemn> */
  AVER(SegWhite(seg) == TraceSetEMPTY);

  /* Empty the initialzed part of any buffer into the segment.  This
     is valid at any time.  We do it here as an optimisation to helps
     to condemn as much as possible. */
  if (buffer != NULL) {
    Addr init = BufferScanLimit(buffer);
    Size wasBuffered = AddrOffset(BufferBase(buffer), init);
    Count used = wasBuffered >> awl->alignShift;
    PoolGenAccountForEmpty(awl->pgen, wasBuffered, 0, FALSE);
    AVER(awlseg->bufferedGrains >= used);
    awlseg->bufferedGrains -= used;
    awlseg->newGrains += used;
    buffer->base = init;
  }

  /* Account for the new and old areas as condemned.  Any buffered
     area is added to condemned at flip (.flip.condemned).  TODO: This
     may lead to cancellation of collections that could reclaim white
     objects in the buffer. */
  condemnedSize = AWLGrainsSize(awl, awlseg->newGrains + awlseg->oldGrains);
  if (condemnedSize == 0)
    return ResOK; /* FIXME: Mustn't fail, see impl.c.trace.whiten.fail. */

  tableSize = BTSize(awlSegGrains(awlseg));
  res = ControlAlloc(&p, PoolArena(pool), tableSize);
  if (res != ResOK)
    goto failMarkTable;
  awlseg->mark = p;
  res = ControlAlloc(&p, PoolArena(pool), tableSize);
  if (res != ResOK)
    goto failScanTable;
  awlseg->scanned = p;

  /* Whiten the whole segment, including any buffered areas.  Pre-flip
     buffered allocation is white, post-flip black (.flip.mark). */
  awlRangeWhiten(awlseg, 0, awlSegGrains(awlseg));

  /* The unused part of the buffer remains buffered: the rest becomes
     old. */
  PoolGenAccountForAge(awl->pgen, 0,
                       AWLGrainsSize(awl, awlseg->newGrains), FALSE);
  awlseg->oldGrains += awlseg->newGrains;
  awlseg->newGrains = 0;

  trace->condemned += condemnedSize;
  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  
  return ResOK;

failScanTable:
  ControlFree(PoolArena(pool), awlseg->mark, tableSize);
failMarkTable:
  return res;
}


/* AWLBlacken -- Blacken method for AWL pools */

static void AWLBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);

  UNUSED(pool);

  AVERT(TraceSet, traceSet);

  if (awlseg->scanned != NULL) /* segment is white */
    BTSetRange(awlseg->scanned, 0, awlSegGrains(awlseg));
}


/* awlScanObject -- scan a single object */

static Res awlScanObject(AWL awl, ScanState ss, Addr base, Addr limit)
{
  Res res;
  Bool dependent;       /* is there a dependent object? */
  Addr dependentObject; /* base address of dependent object */
  Seg dependentSeg = NULL; /* segment of dependent object */
  Pool pool = MustBeA(AbstractPool, awl);
  Arena arena = PoolArena(pool);
  Format format = pool->format;

  AVERT(AWL, awl);
  AVERT(ScanState, ss);
  AVER(base != 0);
  AVER(base < limit);

  dependentObject = awl->findDependent(AddrAdd(base, format->headerSize));
  dependent = SegOfAddr(&dependentSeg, arena, dependentObject);
  if (dependent) {
      /* <design/poolawl/#fun.scan.pass.object.dependent.expose> */
      ShieldExpose(arena, dependentSeg);
      /* <design/poolawl/#fun.scan.pass.object.dependent.summary> */
      SegSetSummary(dependentSeg, RefSetUNIV);
  }

  res = FormatScan(format, ss,
                   AddrAdd(base, format->headerSize),
                   AddrAdd(limit, format->headerSize));

  if (dependent)
    ShieldCover(arena, dependentSeg);

  return res;
}


/* awlSegTraverse -- apply a visitor to all objects in a segment */
/* FIXME: Duplicate of loSegTraverse */

typedef Res (*AWLSegVisitor)(AWLSeg awlseg, Index i, Index j, Addr p, Addr q, void *closure);
static Res awlSegTraverse(AWLSeg awlseg, AWLSegVisitor visit, void *closure)
{
  Seg seg = MustBeA(Seg, awlseg);
  Pool pool = SegPool(seg);
  AWL awl = MustBeA(AWLPool, pool);
  Addr base = SegBase(seg);
  Count grains = awlSegGrains(awlseg);
  Buffer buffer = SegBuffer(seg);
  Addr bufferSkip, bufferLimit;
  Format format = pool->format;
  Size headerSize = format->headerSize;
  Index i;

  bufferSkip = (Addr)0;
  bufferLimit = (Addr)0;
  if (buffer != NULL) {
    Addr scanLimit = BufferScanLimit(buffer);
    bufferLimit = BufferLimit(buffer);
    if (scanLimit != bufferLimit)
      bufferSkip = scanLimit;
  }

  i = 0;
  while(i < grains) {
    Addr p, q;
    Index j;
    Res res;

    /* TODO: It would be more efficient to use BTFind*Range here. */
    if (!BTGet(awlseg->alloc, i)) {
      ++i;
      continue;
    }

    p = awlAddrOfIndex(base, awl, i);
    
    /* <design/poolawl/#fun.scan.pass.buffer> */
    if (p == bufferSkip) {
      i = awlIndexOfAddr(base, awl, bufferLimit);
      continue;
    }

    q = format->skip(AddrAdd(p, headerSize));
    q = AddrSub(q, headerSize);
    j = awlIndexOfAddr(base, awl, q);
    
    AVER_CRITICAL(j <= grains);
    AVER_CRITICAL(AddrIsAligned(q, PoolAlignment(pool)));
    /* Object should be entirely allocated. */
    AVER_CRITICAL(BTIsSetRange(awlseg->alloc, i, j));

    res = visit(awlseg, i, j, p, q, closure);
    if (res != ResOK)
      return res;

    i = j;
  }
  AVER(i == grains);

  return ResOK;
}


/* awlScanSinglePass -- a single scan pass over a segment */

typedef struct AWLScanClosureStruct {
  Bool *anyScannedReturn;
  ScanState ss;
  Bool scanAllObjects;
  AWL awl;
} AWLScanClosureStruct, *AWLScanClosure;

static Res awlScanVisitor(AWLSeg awlseg, Index i, Index j, Addr p, Addr q, void *closure)
{
  AWLScanClosure my = closure;
  UNUSED(j);

  /* <design/poolawl/#fun.scan.pass.object> */
  if (my->scanAllObjects ||
      (BTGet(awlseg->mark, i) && !BTGet(awlseg->scanned, i))) {
    Res res = awlScanObject(my->awl, my->ss, p, q);
    if (res != ResOK)
      return res;

    *my->anyScannedReturn = TRUE;

    /* TODO: Extending the mark bits to the whole object could save
       a pass on reclaim.  See AMSScan and AMSReclaim. */
    if (awlseg->scanned != NULL)
      BTSet(awlseg->scanned, i);
  }

  return ResOK;
}

static Res awlScanSinglePass(Bool *anyScannedReturn,
                             ScanState ss, Pool pool,
                             Seg seg, Bool scanAllObjects)
{
  AWL awl = MustBeA(AWLPool, pool);
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  AWLScanClosureStruct scStruct;

  AVERT(ScanState, ss);
  AVERT(Bool, scanAllObjects);
  AVER(scanAllObjects || awlseg->mark != NULL);
  AVER(scanAllObjects || awlseg->scanned != NULL);

  *anyScannedReturn = FALSE;

  scStruct.anyScannedReturn = anyScannedReturn;
  scStruct.ss = ss;
  scStruct.scanAllObjects = scanAllObjects;
  scStruct.awl = awl;
  return awlSegTraverse(awlseg, awlScanVisitor, &scStruct);
}


/* AWLScan -- segment scan method for AWL */

static Res AWLScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  AWL awl = MustBeA(AWLPool, pool);
  Bool anyScanned; /* TODO: Calculate from a field in ss? */
  Bool scanAllObjects;
  Res res;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);

  /* If the scanner isn't going to scan all the objects then the */
  /* summary of the unscanned objects must be added into the scan */
  /* state summary, so that it's a valid summary of the entire */
  /* segment on return. */

  /* This pool assumes disjoint white sets and maintains mark and */
  /* scanned tables (effectively non-white and black tables) with */
  /* respect to the trace with respect to which the segment is */
  /* white.  For any other trace, we cannot tell which objects */
  /* are grey and must therefore scan them all. */

  scanAllObjects =
    (TraceSetDiff(ss->traces, SegWhite(seg)) != TraceSetEMPTY);

  do {
    res = awlScanSinglePass(&anyScanned, ss, pool, seg, scanAllObjects);
    if (res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  /* we are done if we scanned all the objects or if we did a pass */
  /* and didn't scan any objects (since then, no new object can have */
  /* gotten fixed) */
  } while (!scanAllObjects && anyScanned);

  *totalReturn = scanAllObjects;
  AWLNoteScan(awl, seg, ss);
  return ResOK;
}


/* AWLFix -- Fix method for AWL */

static Res AWLFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AWL awl = MustBeA(AWLPool, pool);
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Ref clientRef;
  Addr base;
  Index i;

  AVERT(ScanState, ss);
  AVER(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER(refIO != NULL);

  clientRef = *refIO;
  ss->wasMarked = TRUE;

  base = AddrSub((Addr)clientRef, pool->format->headerSize);
  /* can get an ambiguous reference to close to the base of the
   * segment, so when we subtract the header we are not in the
   * segment any longer.  This isn't a real reference,
   * so we can just skip it.  */
  if (base < SegBase(seg)) {
    return ResOK;
  }
  i = awlIndexOfAddr(SegBase(seg), awl, base);

  switch(ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if (!AddrIsAligned(base, sizeof(void *)) || !BTGet(awlseg->alloc, i))
      return ResOK;
    /* falls through */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    if (!BTGet(awlseg->mark, i)) {
      ss->wasMarked = FALSE;
      if (ss->rank == RankWEAK) {
        *refIO = (Ref)0;
      } else {
        BTSet(awlseg->mark, i);
        SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
      }
    }
    break;
  default:
    NOTREACHED;
    return ResUNIMPL;
  }

  return ResOK;
}


/* AWLReclaim -- reclaim dead objects in an AWL segment
 *
 * TODO: Duplicate of LOReclaim.
 */

typedef struct AWLSegReclaimClosureStruct {
  Trace trace;
  Size preservedInPlaceSize;
  Count reclaimedGrains;
} AWLSegReclaimClosureStruct, *AWLSegReclaimClosure;

static Res awlSegReclaimVisitor(AWLSeg awlseg, Index i, Index j, Addr p, Addr q, void *closure)
{
  AWLSegReclaimClosure my = closure;
  
  if (!BTIsResRange(awlseg->mark, i, j)) { /* object marked? */
    my->preservedInPlaceSize += AddrOffset(p, q);
    STATISTIC(++my->trace->preservedInPlaceCount);
  } else {
    /* This object is not marked, so free it */
    BTResRange(awlseg->alloc, i, j);
    my->reclaimedGrains += j - i;
  }

  return ResOK;
}

static void AWLReclaim(Pool pool, Trace trace, Seg seg)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  AWL awl = MustBeA(AWLPool, pool);
  AWLSegReclaimClosureStruct rcStruct;
  Count reclaimedGrains;
  Size preservedInPlaceSize;

  AVERT(Trace, trace);
  AVER(TraceSetIsMember(SegWhite(seg), trace));
  AVER(awlseg->mark != NULL);

  rcStruct.trace = trace;
  rcStruct.preservedInPlaceSize = 0;
  rcStruct.reclaimedGrains = 0;
  (void)awlSegTraverse(awlseg, awlSegReclaimVisitor, &rcStruct);
  reclaimedGrains = rcStruct.reclaimedGrains;
  preservedInPlaceSize = rcStruct.preservedInPlaceSize;

  AVER(reclaimedGrains <= awlSegGrains(awlseg));
  AVER(awlseg->oldGrains >= reclaimedGrains);
  awlseg->oldGrains -= reclaimedGrains;
  awlseg->freeGrains += reclaimedGrains;
  PoolGenAccountForReclaim(awl->pgen, AWLGrainsSize(awl, reclaimedGrains), FALSE);

  STATISTIC(trace->reclaimSize += AWLGrainsSize(awl, reclaimedGrains));
  trace->preservedInPlaceSize += preservedInPlaceSize;

  ControlFree(PoolArena(pool), awlseg->mark, BTSize(awlSegGrains(awlseg)));
  awlseg->mark = NULL;
  ControlFree(PoolArena(pool), awlseg->scanned, BTSize(awlSegGrains(awlseg)));
  awlseg->scanned = NULL;
              
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));

  /* Destroy entirely free segment. */
  /* TODO: Consider keeping spare segments. */
  if (awlseg->freeGrains == awlSegGrains(awlseg) && SegBuffer(seg) == NULL) {
    AVER(awlseg->oldGrains == 0);
    AVER(awlseg->newGrains == 0);
    AVER(awlseg->bufferedGrains == 0);
    PoolGenFree(awl->pgen, seg,
                AWLGrainsSize(awl, awlseg->freeGrains),
                AWLGrainsSize(awl, awlseg->oldGrains),
                AWLGrainsSize(awl, awlseg->newGrains),
                FALSE);
  }
}


/* AWLAccess -- handle a barrier hit */

static Res AWLAccess(Pool pool, Seg seg, Addr addr,
                     AccessSet mode, MutatorFaultContext context)
{
  AWL awl = MustBeA(AWLPool, pool);
  Res res;

  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVER(SegPool(seg) == pool);
  AVERT(AccessSet, mode);
  
  /* Attempt scanning a single reference if permitted */
  if(AWLCanTrySingleAccess(PoolArena(pool), awl, seg, addr)) {
    res = PoolSingleAccess(pool, seg, addr, mode, context);
    switch(res) {
      case ResOK:
        AWLNoteRefAccess(awl, seg, addr);
        return ResOK;
      case ResFAIL:
        /* Not all accesses can be managed singly. Default to segment */
        break;
      default:
        return res;
    }
  }

  /* Have to scan the entire seg anyway. */
  res = PoolSegAccess(pool, seg, addr, mode, context);
  if(ResOK == res) {
    AWLNoteSegAccess(awl, seg, addr);
  }

  return res;
}


/* AWLWalk -- walk all objects */

typedef struct AWLWalkClosureStruct {
  FormattedObjectsVisitor f;
  Pool pool;
  Format format;
  Size headerSize;
  void *p;
  size_t s;
} AWLWalkClosureStruct, *AWLWalkClosure;

static Res awlWalkVisitor(AWLSeg awlseg, Index i, Index j, Addr p, Addr q, void *closure)
{
  AWLWalkClosure my = closure;
  UNUSED(awlseg);
  UNUSED(i);
  UNUSED(j);
  UNUSED(q);
  (*my->f)(AddrAdd(p, my->headerSize), my->format, my->pool, my->p, my->s);
  return ResOK;
}

static void AWLWalk(Pool pool, Seg seg, FormattedObjectsVisitor f,
                    void *p, size_t s)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Bool b;
  AWLWalkClosureStruct wcStruct;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  wcStruct.f = f;
  wcStruct.pool = pool;
  b = PoolFormat(&wcStruct.format, pool);
  AVER(b);
  wcStruct.headerSize = wcStruct.format->headerSize;
  wcStruct.p = p;
  wcStruct.s = s;
  (void)awlSegTraverse(awlseg, awlWalkVisitor, &wcStruct);
}


/* AWLTotalSize -- total memory allocated from the arena */
/* TODO: This code is repeated in AMS */

static Size AWLTotalSize(Pool pool)
{
  AWL awl = MustBeA(AWLPool, pool);
  return awl->pgen->totalSize;
}


/* AWLFreeSize -- free memory (unused by client program) */
/* TODO: This code is repeated in AMS */

static Size AWLFreeSize(Pool pool)
{
  AWL awl = MustBeA(AWLPool, pool);
  return awl->pgen->freeSize;
}


/* AWLPoolClass -- the class definition */

DEFINE_CLASS(Pool, AWLPool, klass)
{
  INHERIT_CLASS(klass, AWLPool, AbstractCollectPool);
  PoolClassMixInFormat(klass);
  klass->size = sizeof(AWLPoolStruct);
  klass->varargs = AWLVarargs;
  klass->init = AWLInit;
  klass->finish = AWLFinish;
  klass->bufferClass = AWLBufferClassGet;
  klass->bufferFill = AWLBufferFill;
  klass->bufferEmpty = AWLBufferEmpty;
  klass->access = AWLAccess;
  klass->whiten = AWLWhiten;
  klass->blacken = AWLBlacken;
  klass->scan = AWLScan;
  klass->fix = AWLFix;
  klass->fixEmergency = AWLFix;
  klass->reclaim = AWLReclaim;
  klass->walk = AWLWalk;
  klass->totalSize = AWLTotalSize;
  klass->freeSize = AWLFreeSize;
}


mps_pool_class_t mps_class_awl(void)
{
  return (mps_pool_class_t)CLASS(AWLPool);
}


/* AWLCheck -- check an AWL pool */

ATTRIBUTE_UNUSED
static Bool AWLCheck(AWL awl)
{
  CHECKS(AWL, awl);
  CHECKC(AWLPool, awl);
  CHECKD(Pool, CouldBeA(Pool, awl));
  CHECKL(AWLGrainsSize(awl, (Count)1) == PoolAlignment(CouldBeA(Pool, awl)));
  /* Nothing to check about succAccesses. */
  CHECKL(FUNCHECK(awl->findDependent));
  /* Don't bother to check stats. */
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
