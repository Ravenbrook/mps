/* poollo.c: LEAF POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: See <design/poollo/>.  This is a leaf pool class.
 */

#include "mpsclo.h"
#include "mpm.h"
#include "mps.h"

SRCID(poollo, "$Id$");


/* LOStruct -- leaf object pool instance structure */

#define LOSig           ((Sig)0x51970B07) /* SIGnature LO POoL */

typedef struct LOStruct *LO;

typedef struct LOStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  Shift alignShift;             /* log_2 of pool alignment */
  PoolGenStruct pgenStruct;     /* generation representing the pool */
  PoolGen pgen;                 /* NULL or pointer to pgenStruct */
  Sig sig;
} LOStruct;

#define LOGrainsSize(lo, grains) ((grains) << (lo)->alignShift)

typedef LO LOPool;
#define LOPoolCheck LOCheck
DECLARE_CLASS(Pool, LOPool, AbstractSegBufPool);
DECLARE_CLASS(Seg, LOSeg, GCSeg);
typedef SegBuf LOBuffer;
#define LOBufferCheck SegBufCheck
DECLARE_CLASS(Buffer, LOBuffer, SegBuf);


/* forward declaration */
static Bool LOCheck(LO lo);


/* LOGSegStruct -- LO segment structure */

typedef struct LOSegStruct *LOSeg;

#define LOSegSig      ((Sig)0x519705E9) /* SIGnature LO SEG */

typedef struct LOSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  BT mark;                  /* NULL or mark bit table on white segs */
  BT alloc;                 /* alloc bit table */
  Count freeGrains;         /* free grains */
  Count bufferedGrains;     /* grains in buffers */
  Count newGrains;          /* grains allocated since last collection */
  Count oldGrains;          /* grains allocated prior to last collection */
  Sig sig;                  /* <code/misc.h#sig> */
} LOSegStruct;


/* forward decls */
static Res loSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args);
static void loSegFinish(Seg seg);
static Count loSegGrains(LOSeg loseg);
static void loBufferFlip(Buffer buffer, Trace trace);
static Res loBufferInit(Buffer buffer, Pool pool, Bool isMutator, ArgList args);


/* LOSegClass -- Class definition for LO segments */

DEFINE_CLASS(Seg, LOSeg, klass)
{
  INHERIT_CLASS(klass, LOSeg, GCSeg);
  SegClassMixInNoSplitMerge(klass);
  klass->size = sizeof(LOSegStruct);
  klass->init = loSegInit;
  klass->finish = loSegFinish;
}


DEFINE_CLASS(Buffer, LOBuffer, klass)
{
  INHERIT_CLASS(klass, LOBuffer, SegBuf);
  klass->init = loBufferInit;
  klass->flip = loBufferFlip;
}


/* TODO: Any way to avoid this having to exist? */
/* TODO: Duplicate of awlBufferInit. */
static Res loBufferInit(Buffer buffer, Pool pool, Bool isMutator, ArgList args)
{
  Res res = NextMethod(Buffer, LOBuffer, init)(buffer, pool, isMutator, args);
  if (res != ResOK)
    return res;
  SetClassOfPoly(buffer, CLASS(LOBuffer));
  return ResOK;
}


/* LOSegCheck -- check an LO segment */

ATTRIBUTE_UNUSED
static Bool LOSegCheck(LOSeg loseg)
{
  Seg seg = MustBeA(Seg, loseg);
  LO lo = MustBeA(LOPool, SegPool(seg));
  CHECKS(LOSeg, loseg);
  CHECKD(GCSeg, &loseg->gcSegStruct);
  CHECKL((SegWhite(seg) == TraceSetEMPTY) == (loseg->mark == NULL));
  CHECKL(loseg->alloc != NULL);
  /* Could check exactly how many bits are set in the alloc table. */
  /* CHECKL(BTCountResRange(loseg->alloc, 0, ...); */
  CHECKL(loseg->freeGrains + loseg->bufferedGrains + loseg->newGrains
         + loseg->oldGrains
         == SegSize(seg) >> lo->alignShift);
  return TRUE;
}


/* loSegInit -- Init method for LO segments */

static Res loSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  LOSeg loseg;
  LO lo = MustBeA(LOPool, pool);
  Res res;
  Arena arena = PoolArena(pool);
  Count grains;
  void *p;

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, LOSeg, init)(seg, pool, base, size, args);
  if(res != ResOK)
    goto failSuperInit;
  loseg = CouldBeA(LOSeg, seg);

  AVER(SegWhite(seg) == TraceSetEMPTY);

  grains = size >> lo->alignShift;
  loseg->mark = NULL;
  res = ControlAlloc(&p, arena, BTSize(grains));
  if(res != ResOK)
    goto failAllocTable;
  loseg->alloc = p;
  BTResRange(loseg->alloc, 0, grains);
  loseg->freeGrains = grains;
  loseg->bufferedGrains = (Count)0;
  loseg->newGrains = (Count)0;
  loseg->oldGrains = (Count)0;

  SetClassOfPoly(seg, CLASS(LOSeg));
  loseg->sig = LOSegSig;
  AVERC(LOSeg, loseg);

  return ResOK;

failAllocTable:
  NextMethod(Seg, LOSeg, finish)(seg);
failSuperInit:
  AVER(res != ResOK);
  return res;
}


/* loSegFinish -- Finish method for LO segments */

static void loSegFinish(Seg seg)
{
  LOSeg loseg = MustBeA(LOSeg, seg);
  Pool pool = SegPool(seg);
  Arena arena = PoolArena(pool);
  Size tablesize;
  Count grains;

  loseg->sig = SigInvalid;

  grains = loSegGrains(loseg);
  tablesize = BTSize(grains);
  ControlFree(arena, loseg->alloc, tablesize);
  if (loseg->mark != NULL) {
    ControlFree(arena, loseg->mark, tablesize);
  }

  NextMethod(Seg, LOSeg, finish)(seg);
}


ATTRIBUTE_UNUSED
static Count loSegGrains(LOSeg loseg)
{
  Seg seg = MustBeA(Seg, loseg);
  LO lo = MustBeA(LOPool, SegPool(seg));
  Size size = SegSize(seg);
  return size >> lo->alignShift;
}


/* Conversion between indexes and Addrs */
#define loIndexOfAddr(base, lo, p) \
  (AddrOffset((base), (p)) >> (lo)->alignShift)

#define loAddrOfIndex(base, lo, i) \
  (AddrAdd((base), LOGrainsSize((lo), (i))))


/* loSegFree -- mark block from baseIndex to limitIndex free */

static void loSegFree(LOSeg loseg, Index baseIndex, Index limitIndex)
{
  AVERT(LOSeg, loseg);
  AVER(baseIndex < limitIndex);
  AVER(limitIndex <= loSegGrains(loseg));

  AVER(BTIsSetRange(loseg->alloc, baseIndex, limitIndex));
  BTResRange(loseg->alloc, baseIndex, limitIndex);
}


/* Find a free block of size size in the segment.
 * Return pointer to base and limit of block (which may be
 * bigger than the requested size to accommodate buffering).
 */
static Bool loSegFindFree(Addr *bReturn, Addr *lReturn,
                          LOSeg loseg, Size size)
{
  Index baseIndex, limitIndex;
  Seg seg = MustBeA(Seg, loseg);
  Pool pool = SegPool(seg);
  LO lo = MustBeA(LOPool, pool);
  Count agrains;
  Addr segBase;

  AVER(bReturn != NULL);
  AVER(lReturn != NULL);
  AVERT(LOSeg, loseg);

  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* agrains is the number of grains corresponding to the size */
  /* of the allocation request */
  agrains = size >> lo->alignShift;
  AVER(agrains >= 1);
  AVER(agrains <= loseg->freeGrains);
  AVER(size <= SegSize(seg));

  /* Don't bother trying to allocate from a buffered segment */
  if (SegBuffer(seg) != NULL)
    return FALSE;

  if (!BTFindLongResRange(&baseIndex, &limitIndex, loseg->alloc,
                          0, loSegGrains(loseg), agrains))
    return FALSE;

  /* check that BTFindLongResRange really did find enough space */
  AVER(baseIndex < limitIndex);
  AVER(LOGrainsSize(lo, limitIndex - baseIndex) >= size);
  segBase = SegBase(seg);
  *bReturn = loAddrOfIndex(segBase, lo, baseIndex);
  *lReturn = loAddrOfIndex(segBase, lo, limitIndex);

  return TRUE;
}


/* loSegCreate -- Creates a segment of size at least size.
 *
 * Segments will be multiples of ArenaGrainSize.
 */

static Res loSegCreate(LOSeg *loSegReturn, Pool pool, Size size)
{
  LO lo = MustBeA(LOPool, pool);
  Seg seg;
  Res res;

  AVER(loSegReturn != NULL);
  AVER(size > 0);

  res = PoolGenAlloc(&seg, lo->pgen, CLASS(LOSeg),
                     SizeArenaGrains(size, PoolArena(pool)),
                     argsNone);
  if (res != ResOK)
    return res;

  *loSegReturn = MustBeA(LOSeg, seg);
  return ResOK;
}


/* loSegTraverse -- apply a visitor to all objects in a segment */
/* FIXME: Duplicate of awlSegTraverse */

typedef Bool (*LOSegVisitor)(LOSeg loseg, Index i, Index j, Addr p, Addr q, void *closure);
static Bool loSegTraverse(LOSeg loseg, LOSegVisitor visit, void *closure)
{
  Seg seg = MustBeA(Seg, loseg);
  Buffer buffer = SegBuffer(seg);
  Addr bufferSkip, bufferLimit;
  Pool pool = SegPool(seg);
  LO lo = MustBeA(LOPool, pool);
  Addr base = SegBase(seg);
  Count grains = loSegGrains(loseg);
  Format format = NULL; /* supress "may be used uninitialized" warning */
  Size headerSize;
  Index i;
  Bool b;

  AVERT(LOSeg, loseg);

  /* If the segment has a buffer we skip over the buffered area.
     Although this is potentially an asynchronous read of the
     allocation point init pointer, it's conservative. */
  bufferSkip = (Addr)0; /* can't match within the segment */
  bufferLimit = (Addr)0; /* suppress uninitialized warning */
  if (buffer != NULL) {
    Addr scanLimit = BufferScanLimit(buffer);
    bufferLimit = BufferLimit(buffer);
    if (scanLimit != bufferLimit)
      bufferSkip = scanLimit;

    AVER(loseg->bufferedGrains >=
         loIndexOfAddr(base, lo, bufferLimit) -
         loIndexOfAddr(base, lo, scanLimit));
  }

  b = PoolFormat(&format, pool);
  AVER(b);
  headerSize = format->headerSize;

  i = 0;
  while (i < grains) {
    Addr p, q;
    Index j;

    /* TODO: It would be more efficient to use BTFind*Range here. */
    if (!BTGet(loseg->alloc, i)) { /* grain is free? */
      ++i;
      continue;
    }

    p = loAddrOfIndex(base, lo, i);

    if (p == bufferSkip) {
      i = loIndexOfAddr(base, lo, bufferLimit);
      continue;
    }

    /* Find the limit of the object. */
    q = format->skip(AddrAdd(p, headerSize));
    q = AddrSub(q, headerSize);
    j = loIndexOfAddr(base, lo, q);

    AVER_CRITICAL(j <= grains);
    AVER_CRITICAL(AddrIsAligned(q, PoolAlignment(pool)));
    /* Object should be entirely allocated. */
    AVER_CRITICAL(BTIsSetRange(loseg->alloc, i, j));

    if (!visit(loseg, i, j, p, q, closure))
      return FALSE;

    i = j;
  }
  AVER(i == grains);

  return TRUE;
}


/* LOWalk -- apply a visitor to all objects in a segment
 *
 * This walks over _all_ objects in the heap, whether they are black
 * or white, they are still validly formatted as this is a leaf pool,
 * so there can't be any dangling references
 *
 * FIXME: Duplicate code with AWLWalk.
 */

typedef struct LOWalkClosureStruct {
  FormattedObjectsVisitor f;
  Pool pool;
  Format format;
  Size headerSize;
  void *p;
  size_t s;
} LOWalkClosureStruct, *LOWalkClosure;

static Bool loWalkVisitor(LOSeg loseg, Index i, Index j, Addr p, Addr q, void *closure)
{
  LOWalkClosure my = closure;
  UNUSED(loseg);
  UNUSED(i);
  UNUSED(j);
  UNUSED(q);
  my->f(AddrAdd(p, my->headerSize), my->format, my->pool, my->p, my->s);
  return TRUE;
}

static void LOWalk(Pool pool, Seg seg, FormattedObjectsVisitor f,
                   void *p, size_t s)
{
  LOSeg loseg = MustBeA(LOSeg, seg);
  Bool b;
  LOWalkClosureStruct wcStruct;

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
  (void)loSegTraverse(loseg, loWalkVisitor, &wcStruct);
}


/* LOVarargs -- decode obsolete varargs */

static void LOVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_FORMAT;
  args[0].val.format = va_arg(varargs, Format);
  args[1].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}


/* LOInit -- initialize an LO pool */

static Res LOInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  LO lo;
  Res res;
  ArgStruct arg;
  Chain chain;
  unsigned gen = LO_GEN_DEFAULT;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);
  UNUSED(klass); /* used for debug pools only */

  res = PoolAbsInit(pool, arena, klass, args);
  if (res != ResOK)
    goto failAbsInit;
  lo = CouldBeA(LOPool, pool);

  /* Ensure a format was supplied in the argument list. */
  AVER(pool->format != NULL);

  if (ArgPick(&arg, args, MPS_KEY_CHAIN))
    chain = arg.val.chain;
  else {
    chain = ArenaGlobals(arena)->defaultChain;
    gen = 1; /* avoid the nursery of the default chain by default */
  }
  if (ArgPick(&arg, args, MPS_KEY_GEN))
    gen = arg.val.u;
  
  AVERT(Format, pool->format);
  AVER(FormatArena(pool->format) == arena);
  AVERT(Chain, chain);
  AVER(gen <= ChainGens(chain));
  AVER(chain->arena == arena);

  pool->alignment = pool->format->alignment;
  lo->alignShift = SizeLog2((Size)PoolAlignment(pool));

  lo->pgen = NULL;

  SetClassOfPoly(pool, CLASS(LOPool));
  lo->sig = LOSig;
  AVERC(LOPool, lo);
  
  res = PoolGenInit(&lo->pgenStruct, ChainGen(chain, gen), pool);
  if (res != ResOK)
    goto failGenInit;
  lo->pgen = &lo->pgenStruct;

  EVENT2(PoolInitLO, pool, pool->format);

  return ResOK;

failGenInit:
  PoolAbsFinish(pool);
failAbsInit:
  AVER(res != ResOK);
  return res;
}


/* LOFinish -- finish an LO pool */

static void LOFinish(Pool pool)
{
  LO lo = MustBeA(LOPool, pool);
  Ring node, nextNode;

  RING_FOR(node, &pool->segRing, nextNode) {
    Seg seg = SegOfPoolRing(node);
    LOSeg loseg = MustBeA(LOSeg, seg);
    AVER(SegBuffer(seg) == NULL);
    AVERT(LOSeg, loseg);
    AVER(loseg->bufferedGrains == 0);
    PoolGenFree(lo->pgen, seg,
                LOGrainsSize(lo, loseg->freeGrains),
                LOGrainsSize(lo, loseg->oldGrains),
                LOGrainsSize(lo, loseg->newGrains),
                FALSE);
  }
  PoolGenFinish(lo->pgen);

  lo->sig = SigInvalid;
  PoolAbsFinish(pool);
}


/* LOBufferFill -- BufferFull method for LO
 *
 * TODO: Duplicate of AWLBufferFill.
 */

static Res LOBufferFill(Addr *baseReturn, Addr *limitReturn,
                        Pool pool, Buffer buffer,
                        Size size)
{
  Res res;
  Ring node, nextNode;
  LO lo = MustBeA(LOPool, pool);
  LOSeg loseg;
  Addr base, limit;
  Seg seg;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(BufferRankSet(buffer) == RankSetEMPTY);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* Try to find a segment with enough space already. */
  RING_FOR(node, PoolSegRing(pool), nextNode) {
    seg = SegOfPoolRing(node);
    loseg = MustBeA(LOSeg, seg);
    AVERT(LOSeg, loseg);
    if(LOGrainsSize(lo, loseg->freeGrains) >= size
       && loSegFindFree(&base, &limit, loseg, size))
      goto found;
  }

  /* No segment had enough space, so make a new one. */
  res = loSegCreate(&loseg, pool, size);
  if(res != ResOK)
    return res;
  seg = MustBeA(Seg, loseg);
  base = SegBase(seg);
  limit = SegLimit(seg);

found:
  {
    Addr segBase = SegBase(seg);
    Index baseIndex = loIndexOfAddr(segBase, lo, base);
    Index limitIndex = loIndexOfAddr(segBase, lo, limit);

    /* Mark the newly buffered region as allocated */
    AVER(BTIsResRange(loseg->alloc, baseIndex, limitIndex));
    BTSetRange(loseg->alloc, baseIndex, limitIndex);

    /* If the segment is condemned, mark the newly buffered region as
       black.  FIXME: Could be white pre-flip. */
    if (loseg->mark != NULL)
      BTSetRange(loseg->mark, baseIndex, limitIndex);

    AVER(loseg->freeGrains >= limitIndex - baseIndex);
    loseg->freeGrains -= limitIndex - baseIndex;
    loseg->bufferedGrains += limitIndex - baseIndex;
  }

  PoolGenAccountForFill(lo->pgen, AddrOffset(base, limit));

  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;
}


/* Synchronise the buffer with the alloc Bit Table in the segment. */

static void LOBufferEmpty(Pool pool, Buffer buffer, Addr init, Addr limit)
{
  LO lo = MustBeA(LOPool, pool);
  Addr base, segBase;
  Seg seg;
  LOSeg loseg;
  Index initIndex, limitIndex;
  Count usedGrains, unusedGrains;

  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  seg = BufferSeg(buffer);
  AVERT(Seg, seg);
  AVER(init <= limit);

  loseg = MustBeA(LOSeg, seg);

  base = BufferBase(buffer);
  segBase = SegBase(seg);

  AVER(AddrIsAligned(base, PoolAlignment(pool)));
  AVER(segBase <= base);
  AVER(base <= SegLimit(seg));
  AVER(segBase <= init);
  AVER(init <= SegLimit(seg));

  /* convert base, init, and limit, to quantum positions */
  initIndex = loIndexOfAddr(segBase, lo, init);
  limitIndex = loIndexOfAddr(segBase, lo, limit);

  AVER(initIndex <= limitIndex);
  if (initIndex < limitIndex)
    loSegFree(loseg, initIndex, limitIndex);

  /* Mark bits, if they exist, are left in a random state. */

  unusedGrains = limitIndex - initIndex;
  AVER(loseg->bufferedGrains >= unusedGrains);
  loseg->freeGrains += unusedGrains;
  usedGrains = loseg->bufferedGrains - unusedGrains;
  loseg->bufferedGrains = 0;
  loseg->newGrains += usedGrains;
  PoolGenAccountForEmpty(lo->pgen, LOGrainsSize(lo, usedGrains),
			 LOGrainsSize(lo, unusedGrains), FALSE);

  /* If we're emptying a buffer on a condemned segment before the flip
     then the buffer was allocating white, so age the objects so we
     must age the objects to account for them as old. */
  if (SegWhite(seg) != TraceSetEMPTY &&
      !TraceSetInter(SegWhite(seg), PoolArena(pool)->flippedTraces))
  {
    PoolGenAccountForAge(lo->pgen, 0, LOGrainsSize(lo, usedGrains), FALSE);
    /* The only new grains should be the ones we've just added. */
    AVER(loseg->newGrains == usedGrains);
    loseg->oldGrains += usedGrains;
    loseg->newGrains -= usedGrains;
    /* There is no pre-flip allocation yet.  Retain the above code for
       the future. RB 2016-04-26 */
    NOTREACHED;
  }
}


/* LOWhiten -- whiten a segment
 *
 * FIXME: Common code with AWLWhiten.
 */

static Res LOWhiten(Pool pool, Trace trace, Seg seg)
{
  LO lo = MustBeA(LOPool, pool);
  LOSeg loseg = MustBeA(LOSeg, seg);
  Size condemnedSize;
  Res res;
  void *p;

  AVERT(Trace, trace);
  AVER(SegWhite(seg) == TraceSetEMPTY);

  /* Account for the new and old areas as condemned.  Any buffered
     area is added to condemned at flip (.flip.condemned).  TODO: This
     may lead to cancellation of collections that could reclaim white
     objects in the buffer. */
  condemnedSize = LOGrainsSize(lo, loseg->newGrains + loseg->oldGrains);
  if (condemnedSize == 0)
    return ResOK;

  AVER(loseg->mark == NULL);
  res = ControlAlloc(&p, PoolArena(pool), BTSize(loSegGrains(loseg)));
  if (res != ResOK)
    return res; /* FIXME: Mustn't fail, see impl.c.trace.whiten.fail. */
  loseg->mark = p;

  /* Whiten the whole segment, including any buffered areas.  Pre-flip
     buffered allocation is white, post-flip black (.flip.mark). */
  BTResRange(loseg->mark, 0, loSegGrains(loseg));

  /* Age new objects.  Buffered areas aren't accounted until flip
     (.flip.age). */
  PoolGenAccountForAge(lo->pgen, 0,
                       LOGrainsSize(lo, loseg->newGrains), FALSE);
  loseg->oldGrains += loseg->newGrains;
  loseg->newGrains = 0;

  trace->condemned += condemnedSize;
  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));

  return ResOK;
}


static void loBufferFlip(Buffer buffer, Trace trace)
{
  Seg seg;
  LOSeg loseg;
  LO lo;
  Addr segBase, init, limit;
  Index bufferBaseIndex, initIndex, limitIndex;
  Size wasBuffered;
  Count agedGrains;
  
  NextMethod(Buffer, LOBuffer, flip)(buffer, trace);

  seg = BufferSeg(buffer);
  if (seg == NULL || !TraceSetIsMember(SegWhite(seg), trace))
    return;

  loseg = MustBeA(LOSeg, seg);
  lo = MustBeA(LOPool, BufferPool(buffer));

  /* .flip.age: Any objects between the buffer base and init were
     allocated white, so account them as old. */
  /* FIXME: Common code with amcBufFlip. */
  init = BufferScanLimit(buffer);
  wasBuffered = AddrOffset(BufferBase(buffer), init);
  PoolGenAccountForAge(lo->pgen, wasBuffered, 0, FALSE);

  /* Repeat the above accounting locally. */
  segBase = SegBase(seg);
  bufferBaseIndex = loIndexOfAddr(segBase, lo, BufferBase(buffer));
  initIndex = loIndexOfAddr(segBase, lo, init);
  agedGrains = initIndex - bufferBaseIndex;
  loseg->oldGrains += agedGrains;
  AVER(loseg->bufferedGrains >= agedGrains);
  loseg->bufferedGrains -= agedGrains;

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
  limitIndex = loIndexOfAddr(segBase, lo, limit);
  if (initIndex < limitIndex) /* FIXME: Could detach if empty? */
    BTSetRange(loseg->mark, initIndex, limitIndex);
}
  

static Res LOFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  LO lo = MustBeA_CRITICAL(LOPool, pool);
  LOSeg loseg = MustBeA_CRITICAL(LOSeg, seg);
  Ref clientRef;
  Addr base;

  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER_CRITICAL(refIO != NULL);

  ss->wasMarked = TRUE;         /* <design/fix/#protocol.was-marked> */

  clientRef = *refIO;
  base = AddrSub((Addr)clientRef, pool->format->headerSize);
  /* can get an ambiguous reference to close to the base of the
   * segment, so when we subtract the header we are not in the
   * segment any longer.  This isn't a real reference,
   * so we can just skip it.  */
  if (base < SegBase(seg)) {
    return ResOK;
  }

  switch(ss->rank) {
  case RankAMBIG:
    if(!AddrIsAligned(base, PoolAlignment(pool))) {
      return ResOK;
    }
  /* fall through */

  case RankEXACT:
  case RankFINAL:
  case RankWEAK: {
    Size i = AddrOffset(SegBase(seg), base) >> lo->alignShift;

    AVER_CRITICAL(loseg->mark != NULL);
    if(!BTGet(loseg->mark, i)) {
      ss->wasMarked = FALSE;  /* <design/fix/#protocol.was-marked> */
      if(ss->rank == RankWEAK) {
        *refIO = (Addr)0;
      } else {
        BTSet(loseg->mark, i);
      }
    }
  } break;

  default:
    NOTREACHED;
    break;
  }

  return ResOK;
}


/* LOReclaim -- reclaim white objects in an LO segment
 *
 * TODO: Duplicate of AWLReclaim.
 */

typedef struct LOSegReclaimClosureStruct {
  Trace trace;
  Size preservedInPlaceSize;
  Count reclaimedGrains;
} LOSegReclaimClosureStruct, *LOSegReclaimClosure;

static Bool loSegReclaimVisitor(LOSeg loseg, Index i, Index j, Addr p, Addr q, void *closure)
{
  LOSegReclaimClosure my = closure;
  
  if (!BTIsResRange(loseg->mark, i, j)) { /* object marked? */
    my->preservedInPlaceSize += AddrOffset(p, q);
    STATISTIC(++my->trace->preservedInPlaceCount);
  } else {
    /* This object is not marked, so free it */
    loSegFree(loseg, i, j);
    my->reclaimedGrains += j - i;
  }

  return TRUE;
}

static void LOReclaim(Pool pool, Trace trace, Seg seg)
{
  LOSeg loseg = MustBeA(LOSeg, seg);
  LO lo = MustBeA(LOPool, pool);
  LOSegReclaimClosureStruct rcStruct;
  Count reclaimedGrains;
  Size preservedInPlaceSize;

  AVERT(Trace, trace);
  AVER(TraceSetIsMember(SegWhite(seg), trace));
  AVER(loseg->mark != NULL);

  rcStruct.trace = trace;
  rcStruct.preservedInPlaceSize = 0;
  rcStruct.reclaimedGrains = 0;
  (void)loSegTraverse(loseg, loSegReclaimVisitor, &rcStruct);
  reclaimedGrains = rcStruct.reclaimedGrains;
  preservedInPlaceSize = rcStruct.preservedInPlaceSize;
  
  AVER(reclaimedGrains <= loSegGrains(loseg));
  AVER(loseg->oldGrains >= reclaimedGrains);
  loseg->oldGrains -= reclaimedGrains;
  loseg->freeGrains += reclaimedGrains;
  PoolGenAccountForReclaim(lo->pgen, LOGrainsSize(lo, reclaimedGrains), FALSE);

  STATISTIC(trace->reclaimSize += LOGrainsSize(lo, reclaimedGrains));
  trace->preservedInPlaceSize += preservedInPlaceSize;

  ControlFree(PoolArena(pool), loseg->mark, BTSize(loSegGrains(loseg)));
  loseg->mark = NULL;

  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));

  /* Destroy entirely free segment. */
  /* TODO: Consider keeping spare segments. */
  if (loseg->freeGrains == loSegGrains(loseg) && SegBuffer(seg) == NULL) {
    AVER(loseg->oldGrains == 0);
    AVER(loseg->newGrains == 0);
    AVER(loseg->bufferedGrains == 0);
    PoolGenFree(lo->pgen, seg,
                LOGrainsSize(lo, loseg->freeGrains),
                LOGrainsSize(lo, loseg->oldGrains),
                LOGrainsSize(lo, loseg->newGrains),
                FALSE);
  }
}

  
/* LOTotalSize -- total memory allocated from the arena */
/* TODO: This code is repeated in AMS */

static Size LOTotalSize(Pool pool)
{
  LO lo = MustBeA(LOPool, pool);
  return lo->pgen->totalSize;
}


/* LOFreeSize -- free memory (unused by client program) */
/* TODO: This code is repeated in AMS */

static Size LOFreeSize(Pool pool)
{
  LO lo = MustBeA(LOPool, pool);
  return lo->pgen->freeSize;
}


/* LOPoolClass -- the class definition */

DEFINE_CLASS(Pool, LOPool, klass)
{
  INHERIT_CLASS(klass, LOPool, AbstractSegBufPool);
  PoolClassMixInFormat(klass);
  PoolClassMixInCollect(klass);
  klass->size = sizeof(LOStruct);
  klass->varargs = LOVarargs;
  klass->init = LOInit;
  klass->finish = LOFinish;
  klass->bufferFill = LOBufferFill;
  klass->bufferEmpty = LOBufferEmpty;
  klass->whiten = LOWhiten;
  klass->fix = LOFix;
  klass->fixEmergency = LOFix;
  klass->reclaim = LOReclaim;
  klass->walk = LOWalk;
  klass->totalSize = LOTotalSize;
  klass->bufferClass = LOBufferClassGet;
  klass->freeSize = LOFreeSize;
}


/* mps_class_lo -- the external interface to get the LO pool class */

mps_pool_class_t mps_class_lo(void)
{
  return (mps_pool_class_t)CLASS(LOPool);
}


/* LOCheck -- check an LO pool */

ATTRIBUTE_UNUSED
static Bool LOCheck(LO lo)
{
  CHECKS(LO, lo);
  CHECKC(LOPool, lo);
  CHECKD(Pool, &lo->poolStruct);
  CHECKC(LOPool, lo);
  CHECKL(ShiftCheck(lo->alignShift));
  CHECKL(LOGrainsSize(lo, (Count)1) == PoolAlignment(MustBeA(AbstractPool, lo)));
  if (lo->pgen != NULL) {
    CHECKL(lo->pgen == &lo->pgenStruct);
    CHECKD(PoolGen, lo->pgen);
  }
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
