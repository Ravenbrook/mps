/* impl.c.poolmv2: MANUAL VARIABLE POOL, II
 *
 * $HopeName: MMsrc!poolmv2.c(MMdevel_gavinm_splay.6) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .limitation.seg : MV2BufferFill may fill a buffer with a range that
 * spans more than one segment.  SegBuffer will only mark the first
 * such segment as being buffered.  Using multiple segments as a
 * substitute for pages is a short-term solution.
 */

#include "mpm.h"

/* --- Meters should be a substrate available to all pools. */
typedef struct MeterStruct *Meter;

typedef struct MeterStruct 
{
  char *name;
  Count count;
  double mean;
  double meanSquared;
}MeterStruct;

static void MeterInit(Meter meter, char *name) 
{
  meter->name = name;
  meter->count = 0;
  meter->mean = 0.0;
  meter->meanSquared = 0.0;
}

static void MeterAccumulate(Meter meter, Size amount)
{
  Count count = meter->count + 1;
  double mean = meter->mean;
  double meanSquared = meter->meanSquared;
  double weight = (double)(count - 1) / (double)count;

  /* .limitation.variance: This computation accumulates a running mean
   * and mean^2, minimizing overflow, but sacrificing numerical
   * stablity for small variances.  For more accuracy, the data set
   * should be emitted using a telemetry stream and analyzed off
   * line.
   */
  meter->count = count;
  meter->mean = weight * mean + amount / count;
  meter->meanSquared = weight * meanSquared + amount * amount / count;
}

/* --- crude hack */
static Res WriteDouble(char *before, char* indent, double d,
                       char* after, mps_lib_FILE *stream)
{
  WriteFU i, f;

  i = (WriteFU)d;
  f = (WriteFU)((d - (double)i) * 1000);
  return
    WriteF(stream, "$S$U.$U$S", before, indent, i, f, after, NULL);
}

static Res MeterWrite(Meter meter, char *indent, mps_lib_FILE *stream)
{
  Res res;

  res = WriteF(stream,
               "$Smeter $S {\n", indent, meter->name,
               "$S  count $U\n", indent, meter->count,
               "$S  total $U\n", indent,
               (WriteFU)(meter->count * meter->mean),
               NULL);
  if (res != ResOK)
    return res;
  res = WriteDouble("$S  mean ", indent, meter->mean, "\n", stream);
  if (res != ResOK)
    return res;
  /* --- stddev = sqrt(meanSquared - mean^2), but see
   * .limitation.variance above
   */
  res = WriteDouble("$S  mean^2 ", indent, meter->meanSquared,
                    "\n", stream);
  if (res != ResOK)
    return res;
  res = WriteF(stream,"}\n", NULL);
  if (res != ResOK)
    return res;
  
  return ResOK;
}
  
/* Make metering optional */
#define METER_DECL(meter) struct MeterStruct meter
#define METER_INIT(meter, init) (MeterInit(&(meter), (init)))
#define METER_ACC(meter, delta) (MeterAccumulate(&(meter), (delta)))
#define METER_WRITE(meter, indent, stream) (MeterWrite(&(meter),  \
                                                       (indent),  \
                                                       (stream)))
     
#include "poolmv2.h"

#include "mpscmv2.h"

SRCID(poolmv2, "$HopeName: MMsrc!poolmv2.c(MMdevel_gavinm_splay.6) $");

/* Signatures */
#define MV2Sig ((Sig)0x5193F299) /* SIGnature MV2 */

/* Prototypes */
typedef struct MV2Struct *MV2;
typedef struct ABQStruct *ABQ;
static Bool MV2Check(MV2 mv2);
static Bool ABQCheck(ABQ abq);

/* Structures */

typedef struct ABQStruct
{
  Count count;
  Index head, tail;
  CBSBlock *queue;
}ABQStruct;

typedef struct MV2Struct 
{
  PoolStruct poolStruct;
  CBSStruct cbsStruct;          /* The coalescing block structure */
  ABQStruct abqStruct;          /* The available block queue */
  SegPrefStruct segPrefStruct;  /* The preferences for segments */
  Size reuseSize;               /* Size at which blocks are recycled */
  Bool abqOverflow;             /* ABQ dropped some candidates */
  Bool contingency;             /* High fragmentation mode */
  /* A splinter >= minSize returned from a buffer will be used ASAP */
  /* See design.mps.poolmv2.arch.ap.no-fit.* */
  Size minSize;
  Bool splinter;                /* Saved splinter */
  Seg splinterSeg;              /* Saved splinter seg */
  Addr splinterBase;            /* Saved splinter base */
  Addr splinterLimit;           /* Saved splinter size */

  /* pool accounting */
  Size size;                    /* size of segs in pool */
  Size available;               /* bytes available for allocation */
  
  /* meters*/
  METER_DECL(segAllocs);        /* segs allocated */
  METER_DECL(segFrees);         /* segs freed */
  METER_DECL(bufferFills);      /* buffer fills */
  METER_DECL(bufferEmpties);    /* buffer empties */
  METER_DECL(poolFrees);        /* block frees */
  METER_DECL(overflows);        /* abq overflows */
  METER_DECL(contingencies);    /* contingencies */
  Sig sig;
}MV2Struct;

/* Macros */
#define PoolPoolMV2(pool) PARENT(MV2Struct, poolStruct, (pool))
#define MV2Pool(mv2) (&(mv2)->poolStruct)
#define MV2ABQ(mv2) (&(mv2)->abqStruct)
#define CBSMV2(cbs) PARENT(MV2Struct, cbsStruct, (cbs))
#define MV2CBS(mv2)	(&(mv2)->cbsStruct)
#define MV2segPref(mv2) (&(mv2)->segPrefStruct)
/* .trans.something the C language sucks */
#define unless(cond) if (!(cond))
#define when(cond) if(cond)


/* Methods */
static Bool ABQCheck(ABQ abq)
{
  Index index;
  
  CHECKL(abq->count > 0);
  CHECKL(abq->head >= 0);
  CHECKL(abq->head < abq->count);
  CHECKL(abq->tail >= 0);
  CHECKL(abq->tail < abq->count);
  CHECKL(abq->queue != NULL);
  /* Is this really a local check? */
  for (index = abq->head; index != abq->tail; ) {
    CHECKL(CBSBlockCheck(abq->queue[index]));
    if (++index == abq->count)
      index = 0;
  }

  return TRUE;
}

static Res ABQDescribe(ABQ abq, mps_lib_FILE *stream)
{
  Res res;
  Index index;

  AVERT(ABQ, abq);

  AVER(stream != NULL);

  res = WriteF(stream,
	       "ABQ $P\n{\n", (WriteFP)abq,
	       "  count: $U \n", (WriteFU)abq->count,
	       "  head: $U \n", (WriteFU)abq->head,
	       "  tail: $U \n", (WriteFU)abq->tail,
               "  queue: \n",
	       NULL);
  if(res != ResOK)
    return res;

  for (index = abq->head; index != abq->tail; ) {
    res = CBSBlockDescribe(abq->queue[index], stream);
    if(res != ResOK)
      return res;
    if (++index == abq->count)
      index = 0;
  }

  res = WriteF(stream, "}\n", NULL);
  if(res != ResOK)
    return res;
  
  return ResOK;
}


static Size ABQqueueSize(Count count)
{
  /* strange but true: the sizeof expression calculates the size of a
     single queue element */
  return (Size)(sizeof(((ABQ)NULL)->queue[0]) * count);
}


/* ABQInit -- Initialize an ABQ */
static Res ABQInit(Arena arena, ABQ abq, Count count)
{
  void *p;
  Res res;

  AVERT(Arena, arena);
  AVER(abq != NULL);
  AVER(count > 0);

  res = ArenaAlloc(&p, arena, ABQqueueSize(count));
  if (res != ResOK)
    return res;

  abq->count = count;
  abq->head = 0;
  abq->tail = 0;
  abq->queue = (CBSBlock *)p;

  AVERT(ABQ, abq);
  return ResOK;
}

static Bool ABQIsEmpty(ABQ abq) 
{
  AVERT(ABQ, abq);

  return abq->head == abq->tail;
}


/* ABQFinish -- finish an ABQ */
static void ABQFinish(Arena arena, ABQ abq)
{
  AVERT(Arena, arena);
  AVERT(ABQ, abq);
  /* must be empty */
  AVER(ABQIsEmpty(abq));

  ArenaFree(arena, abq->queue, ABQqueueSize(abq->count));
  
  abq->count = 0;
  abq->queue = NULL;
}

/* ABQPop -- pop a block from the head of the ABQ */
static Res ABQPop(ABQ abq, CBSBlock *blockReturn)
{
  Index index;
  
  AVER(blockReturn != NULL);
  AVERT(ABQ, abq);

  index = abq->head;
  if (index == abq->tail)
    return ResFAIL;

  *blockReturn = abq->queue[index];
  AVERT(CBSBlock, *blockReturn);

  if (++index == abq->count)
    index = 0;
  abq->head = index;
  
  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQPeek -- peek at the head of the ABQ */
static Res ABQPeek(ABQ abq, CBSBlock *blockReturn)
{
  Index index;
  
  AVER(blockReturn != NULL);
  AVERT(ABQ, abq);

  index = abq->head;
  if (index == abq->tail)
    return ResFAIL;

  *blockReturn = abq->queue[index];
  AVERT(CBSBlock, *blockReturn);

  /* Identical to pop, but don't write index back into head */

  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQPush -- push a block onto the tail of the ABQ */
static Res ABQPush(ABQ abq, CBSBlock block)
{
  Index index;

  AVERT(ABQ, abq);
  AVERT(CBSBlock, block);

  index = abq->tail;
  if (++index == abq->count)
    index = 0;
  
  if (index == abq->head)
    return ResFAIL;

  abq->queue[abq->tail] = block;
  abq->tail = index;

  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQDelete -- delete a block from the ABQ */
static Res ABQDelete(ABQ abq, CBSBlock block)
{
  Index index, next, count, tail;
  CBSBlock *queue;
  Bool found = FALSE;

  AVERT(ABQ, abq);
  AVERT(CBSBlock, block);

  index = abq->head;
  tail = abq->tail;
  count = abq->count;
  queue = abq->queue;
  
  while (index != tail) {
    if (queue[index] == block) {
      goto found;
    }
    if (++index == count)
      index = 0;
  }

  return ResFAIL;

found:
  /* index points to the node to be removed */
  next = index;
  if (++next == count)
    next = 0;
  while (next != tail) {
    queue[index] = queue[next];
    index = next;
    if (++next == count)
      next = 0;
  }
  abq->tail = index;
  AVERT(ABQ, abq);
  return ResOK;
}

/*
 * MV2NoteNew -- Callback invoked when a block on the CBS >= reuseSize
 */
static void MV2NoteNew(CBS cbs, CBSBlock block) 
{
  Res res;
  MV2 mv2;
  
  AVERT(CBS, cbs);
  mv2 = CBSMV2(cbs);
  AVERT(MV2, mv2);
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) >= mv2->reuseSize);

  /* Free blocks => not in contingency mode */
  mv2->contingency = FALSE;
  
  res = ABQPush(MV2ABQ(mv2), block);
  if (res != ResOK) {
    /* See .impl.c.poolmv2.free.merge: */
    CBSBlock oldBlock;
    Addr base, limit;
    
    {
      Res res = ABQPeek(MV2ABQ(mv2), &oldBlock);
      AVER(res == ResOK);
    }
    base = CBSBlockBase(oldBlock);
    limit = CBSBlockLimit(oldBlock);

    while (base < limit) {
      Seg seg;
      Addr segBase, segLimit;
      
      {
        Bool b = SegOfAddr(&seg, PoolArena(MV2Pool(mv2)), base);
        AVER(b);
      }
      segBase = SegBase(seg);
      segLimit = SegLimit(seg);
      if (base <= segBase && limit >= segLimit) {
        Size size = AddrOffset(segBase, segLimit);
        
        {
          Res res = CBSDelete(MV2CBS(mv2), segBase, segLimit);
          AVER(res == ResOK);
        }
        mv2->available -= size;
        SegFree(seg);
        mv2->size -= size;
        METER_ACC(mv2->segFrees, size);
      }
      base = segLimit;
    }

    res = ABQPush(MV2ABQ(CBSMV2(cbs)), block);
    if (res != ResOK) {
      mv2->abqOverflow = TRUE;
      METER_ACC(mv2->overflows, CBSBlockSize(block));
    }
  }
}

/*
 * MV2NoteDelete -- Callback invoked when a block on the CBS <=
 * reuseSize
 */
static void MV2NoteDelete(CBS cbs, CBSBlock block)
{
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) < CBSMV2(cbs)->reuseSize);
  
  {
    Res res = ABQDelete(MV2ABQ(CBSMV2(cbs)), block);
    AVER(res == ResOK);
  }
}
  
/*
 * MV2Init -- Initialize an MV2 pool
 *
 * Parameters are:
 * minSize, medianSize, maxSize, reserveDepth
 */
static Res MV2Init(Pool pool, va_list arg)
{
  /* --- ABQ decay rate */
  Arena arena;
  Size minSize, medianSize, maxSize, reuseSize;
  Count reserveDepth, abqDepth;
  MV2 mv2;
  Res res;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  
  /* --- Should there be a ResBADARG ? */
  minSize = va_arg(arg, Size);
  unless (minSize > 0)
    return ResLIMIT;
  medianSize = va_arg(arg, Size);
  unless (medianSize >= minSize)
    return ResLIMIT;
  maxSize = va_arg(arg, Size);
  unless (maxSize >= medianSize)
    return ResLIMIT;
  reserveDepth = va_arg(arg, Count);
  unless (reserveDepth > 0)
    return ResLIMIT;
  /* --- check that maxSize is not too large */
  /* --- check that reserveDepth is not too large */

  /* see design.mps.poolmv2.arch.parameters */
  reuseSize = SizeAlignUp(maxSize, ArenaAlign(arena));
  abqDepth = (reserveDepth * medianSize + reuseSize - 1) / reuseSize;

  res = CBSInit(arena, MV2CBS(mv2), &MV2NoteNew, &MV2NoteDelete,
                reuseSize, TRUE);
  if (res != ResOK)
    goto failCBS;
  
  res = ABQInit(arena, MV2ABQ(mv2), abqDepth);
  if (res != ResOK)
    goto failABQ;

  {
    RefSet refset;
    /* --- Loci needed here, what should the pref be? */
    /* --- why not SegPrefDefault(MV2segPref)? */
    *MV2segPref(mv2) = *SegPrefDefault();
    /* +++ Get own RefSet */
    refset = RefSetComp(ARENA_DEFAULT_REFSET);
    SegPrefExpress(MV2segPref(mv2), SegPrefRefSet, (void *)&refset);
  }

  mv2->reuseSize = reuseSize;
  mv2->abqOverflow = FALSE;
  mv2->contingency = FALSE;
  mv2->minSize = minSize;
  mv2->splinter = FALSE;
  mv2->splinterSeg = NULL;
  mv2->splinterBase = (Addr)0;
  mv2->splinterLimit = (Addr)0;
  
  /* meters*/
  METER_INIT(mv2->segAllocs, "segment allocations");
  METER_INIT(mv2->segFrees, "segment frees");
  METER_INIT(mv2->bufferFills, "buffer fills");
  METER_INIT(mv2->bufferEmpties, "buffer empties");
  METER_INIT(mv2->poolFrees, "pool frees");
  METER_INIT(mv2->overflows, "ABQ overflows");
  METER_INIT(mv2->contingencies, "contingencies");

  mv2->sig = MV2Sig;

  AVERT(MV2, mv2);
  return ResOK;

failABQ:
  CBSFinish(MV2CBS(mv2));
failCBS:
  AVER(res != ResOK);
  return res;
}


static void MV2Finish(Pool pool)
{
  MV2 mv2;
  Arena arena;
  
  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);
  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* Free any splinter */
  if (mv2->splinter) {
    Res res = CBSInsert(MV2CBS(mv2), mv2->splinterBase,
                        mv2->splinterLimit);
    
    AVER(res == ResOK);
  }

  /* +++ Free up all the blocks in the ABQ and CBS */
  
  
  /* Finish the ABQ and CBS structures */
  CBSFinish(MV2CBS(mv2));
  ABQFinish(arena, MV2ABQ(mv2));

  /* --- Finish segPref? */

  mv2->sig = SigInvalid;
}

static Bool ABQRefillCallback(CBS cbs, CBSBlock block, void *closureP,
                              unsigned long closureS)
{
  Res res;
  
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVERT(ABQ, MV2ABQ(CBSMV2(cbs)));
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) >= CBSMV2(cbs)->reuseSize);
  UNUSED(closureP);
  UNUSED(closureS);

  res = ABQPush(MV2ABQ(CBSMV2(cbs)), block);
  return res == ResOK;
}
  

static void ABQRefillIfNecessary(MV2 mv2) 
{
  AVERT(MV2, mv2);

  if (mv2->abqOverflow && ABQIsEmpty(MV2ABQ(mv2))) {
    mv2->abqOverflow = FALSE;
    CBSIterateLarge(MV2CBS(mv2), ABQRefillCallback, NULL, 0);
  }
}


/* MV2BufferFill -- refill an allocation buffer
 *
 * See design.mps.poolmv2.impl.c.poolmv2.ap.fill
 */

static Res MV2BufferFill(Seg *segReturn,
                         Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size minSize)
{
  Seg seg;
  MV2 mv2;
  Res res;
  Addr base, limit;
  Arena arena;
  Size alignedSize, idealSize, reuseSize;
  CBSBlock block;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);
  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(minSize > 0);
  AVER(SizeIsAligned(minSize, pool->alignment));

  arena = PoolArena(pool);
  reuseSize = mv2->reuseSize;
  alignedSize = SizeAlignUp(minSize, ArenaAlign(arena));
  idealSize = reuseSize;

  /* design.mps.poolmv2.arch.ap.no-fit.oversize */
  /* Allocate oversize blocks exactly, directly from the arena */
  if (alignedSize > reuseSize) {
    idealSize = minSize;
    goto direct;
  }

  /* design.mps.poolmv2.arch.ap.no-fit.return */
  /* Use any splinter, if available */
  if (mv2->splinter) {
    base = mv2->splinterBase;
    limit = mv2->splinterLimit;
    if(AddrOffset(base, limit) >= minSize) {
      seg = mv2->splinterSeg;
      mv2->splinter = FALSE;
      goto done;
    }
  }
  
  /* Attempt to retrieve a free block from the ABQ */
  ABQRefillIfNecessary(mv2);
  res = ABQPeek(MV2ABQ(mv2), &block);
  if (res == ResOK) {
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);
    {
      Bool b = SegOfAddr(&seg, arena, base);
      AVER(b);
    }
    /* Use the whole block if the remnant would not stay in the ABQ */
    AVER(idealSize == reuseSize);
    if (AddrOffset(base, limit) - idealSize > reuseSize) {
      limit = AddrAdd(base, idealSize);
    }
    {
      Res res = CBSDelete(MV2CBS(mv2), base, limit);
      AVER(res == ResOK);
    }

    goto done;
  }
  
retry:
  /* If contingency mode, search the CBS using oldest-fit */
  if (mv2->contingency) {
    res = ContingencySearch(MV2CBS(mv2), &seg, &base, &limit, minSize);
    if (res == ResOK)
      goto done;
  }

direct:
  /* Attempt to request a block from the arena */
  res = SegAlloc(&seg, MV2segPref(mv2), idealSize, pool);
  if (res == ResOK) {
    base = SegBase(seg);
    limit = AddrAdd(base, idealSize);
    AVER(limit == SegLimit(seg) || idealSize > reuseSize);
    mv2->size += idealSize;
    METER_ACC(mv2->segAllocs, AddrOffset(base, SegLimit(seg)));
    goto donenew;
  }

  /* Enter contingency mode */
  if (!mv2->contingency) {
    mv2->contingency = TRUE;
    METER_ACC(mv2->contingencies, idealSize);
    goto retry;
  }

  /* Try minimum */
  if (idealSize > alignedSize) {
    idealSize = alignedSize;
    goto direct;
  }
  
fail:
  AVER(res != ResOK);
  return res;
  
done:
  mv2->available -= AddrOffset(base, limit);
donenew:
  /* --- base/limit may span more than one segment */
  *segReturn = seg;
  *baseReturn = base;
  *limitReturn = limit;
  AVER(AddrOffset(base, limit) >= minSize);
  METER_ACC(mv2->bufferFills, AddrOffset(base, limit));
  EVENT_PPWAW(MV2BufferFill, mv2, buffer, minSize, base,
              AddrOffset(base, limit));
  return ResOK;
}

/* MV2BufferEmpty -- detach a buffer from a segment
 *
 * See design.mps.poolmv2.impl.c.poolmv2.ap.empty
 */

static void MV2BufferEmpty(Pool pool, Buffer buffer)
{
  MV2 mv2;
  Seg seg;
  Addr base, limit;
  Size size;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);
  AVERT(Buffer, buffer);
  AVER(!BufferIsReset(buffer));
  AVER(BufferIsReady(buffer));

  seg = BufferSeg(buffer);
  base = BufferGetInit(buffer);
  limit = BufferLimit(buffer);
  size = AddrOffset(base, limit);
  
  EVENT_PPW(MV2BufferEmpty, mv2, buffer, size);

  if (size == 0)
    return;
  
  /* put splinter > minSize at (effective) head of ABQ */
  if (size >= mv2->minSize) {
    /* discard any previous splinter */
    if (mv2->splinter) {
      Res res = CBSInsert(MV2CBS(mv2), mv2->splinterBase,
                          mv2->splinterLimit);
      AVER(res == ResOK);
    }
    mv2->splinter = TRUE;
    mv2->splinterSeg = seg;
    mv2->splinterBase = base;
    mv2->splinterLimit = limit;
  }
  else {
    Res res = CBSInsert(MV2CBS(mv2), base, limit);
    AVER(res == ResOK);
  }
  mv2->available += size;
  METER_ACC(mv2->bufferEmpties, size);
}

/* MV2Free -- free a block
 *
 * see design.poolmv2.impl.c.poolmv2.free
 */

static void MV2Free(Pool pool, Addr base, Size size)
{ 
  MV2 mv2;
  Addr limit;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);

  AVER(base != (Addr)0);
  AVER(size > 0);

  /* We know the buffer observes pool->alignment  */
  size = SizeAlignUp(size, pool->alignment);
  limit = AddrAdd(base, size);
  
  /* --- return exceptional blocks directly to arena ??? */
  
  {
    Res res = CBSInsert(MV2CBS(mv2), base, limit);
    AVER(res == ResOK);
  }
  METER_ACC(mv2->poolFrees, size);
  mv2->available += size;
}

static Res MV2Describe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  MV2 mv2;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);

  AVER(stream != NULL);

  res = WriteF(stream,
	       "MV2 $P\n{\n", (WriteFP)mv2,
	       "  reuseSize: $U \n", (WriteFU)mv2->reuseSize,
	       "  minSize: $U \n", (WriteFU)mv2->minSize,
	       "  size: $U \n", (WriteFU)mv2->size,
	       "  available: $U \n", (WriteFU)mv2->available,
	       "  abqOverflow: $S \n", mv2->abqOverflow?"TRUE":"FALSE",
	       "  contingency: $S \n", mv2->contingency?"TRUE":"FALSE",
	       "  splinter: $S \n", mv2->splinter?"TRUE":"FALSE",
	       "  splinterSeg: $P \n", (WriteFP)mv2->splinterSeg,
	       "  splinterBase: $A \n", (WriteFA)mv2->splinterBase,
	       "  splinterLimit: $A \n", (WriteFU)mv2->splinterLimit,
	       NULL);
  if(res != ResOK)
    return res;

  res = CBSDescribe(MV2CBS(mv2), stream);
  if(res != ResOK)
    return res;

  res = ABQDescribe(MV2ABQ(mv2), stream);
  if(res != ResOK)
    return res;

  /* --- how to deal with non-Ok res's */
  METER_WRITE(mv2->segAllocs, "  ", stream);
  METER_WRITE(mv2->segFrees, "  ", stream);
  METER_WRITE(mv2->bufferFills, "  ", stream);
  METER_WRITE(mv2->bufferEmpties, "  ", stream);
  METER_WRITE(mv2->poolFrees, "  ", stream);
  METER_WRITE(mv2->overflows, "  ", stream);
  METER_WRITE(mv2->contingencies, "  ", stream);
  
  res = WriteF(stream, "}\n", NULL);
  if(res != ResOK)
    return res;

  return ResOK;               
}

static PoolClassStruct PoolClassMV2Struct =
{
  PoolClassSig,
  "MV2",                        /* name */
  sizeof(MV2Struct),            /* size */
  offsetof(MV2Struct, poolStruct), /* offset */
  /* --- should we implement AttrALLOC? */
  AttrFREE | AttrBUF | AttrBUF_RESERVE,/* attr */
  MV2Init,                      /* init */
  MV2Finish,                    /* finish */
  PoolNoAlloc,                  /* alloc */
  MV2Free,                      /* free */
  PoolTrivBufferInit,           /* bufferInit */
  MV2BufferFill,                /* bufferFill */
  MV2BufferEmpty,               /* bufferEmpty */
  PoolTrivBufferFinish,         /* bufferFinish */
  PoolNoTraceBegin,             /* traceBegin */
  PoolNoAccess,                 /* access */
  PoolNoWhiten,                 /* whiten */
  PoolNoGrey,                   /* mark */
  PoolNoBlacken,                /* blacken */
  PoolNoScan,                   /* scan */
  PoolNoFix,                    /* fix */
  PoolNoFix,                    /* emergency fix */
  PoolNoReclaim,                /* relcaim */
  PoolNoBenefit,                /* benefit */
  PoolNoAct,                    /* act */
  PoolNoWalk,                   /* walk */
  MV2Describe,                  /* describe */
  PoolClassSig                  /* impl.h.mpmst.class.end-sig */
};

static Bool MV2Check(MV2 mv2)
{
  CHECKS(MV2, mv2);
  CHECKD(Pool, &mv2->poolStruct);
  CHECKL(mv2->poolStruct.class == &PoolClassMV2Struct);
  /* --- CHECKD(CBS, &mv2->cbsStruct); */
  CHECKL(CBSCheck(MV2CBS(mv2)));
  /* --- CHECKD(ABQ, &mv2->abqStruct); */
  CHECKL(ABQCheck(MV2ABQ(mv2)));
  CHECKD(SegPref, &mv2->segPrefStruct);
  CHECKL(mv2->minSize > 0);
  CHECKL(mv2->reuseSize >= mv2->minSize);
  CHECKL(BoolCheck(mv2->abqOverflow));
  CHECKL(BoolCheck(mv2->contingency));
  CHECKL(BoolCheck(mv2->splinter));
  if (mv2->splinter) {
    CHECKL(AddrOffset(mv2->splinterBase, mv2->splinterLimit) >=
           mv2->minSize);
    /* --- How to check Seg ??? */
    /* CHECKT(Seg, mv2->splinterSeg); */
    CHECKL(SegCheck(mv2->splinterSeg));
    CHECKL(mv2->splinterBase >= SegBase(mv2->splinterSeg));
    CHECKL(mv2->splinterLimit <= SegLimit(mv2->splinterSeg));
  }
  /* --- check meters? */

  return TRUE;
}

PoolClass PoolClassMV2(void)
{
  return &PoolClassMV2Struct;
}


/* MPS Interface Extension */

mps_class_t mps_class_mv2(void)
{
  return (mps_class_t)(PoolClassMV2());
}

/* Free bytes */

size_t mps_mv2_free_size(mps_pool_t mps_pool)
{
  Pool pool;
  MV2 mv2;

  pool = (Pool)mps_pool;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);

  return (size_t)mv2->available;
}

size_t mps_mv2_size(mps_pool_t mps_pool)
{
  Pool pool;
  MV2 mv2;

  pool = (Pool)mps_pool;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);

  return (size_t)mv2->size;
} 

