/* impl.c.poolmv2: NEW MANUAL VARIABLE POOL
 *
 * $HopeName: MMsrc!poolmv2.c(MMdevel_gavinm_splay.2) $
 * Copyright (C) 1998 Harlequin Group plc. All rights reserved.
 *
 * .purpose: The implementation of the new manual-variable pool class
 *
 * .readership: Any MPS developer
 * 
 * .design: See design.mps.poolmv2
 */

#include "mpm.h"
/* Missing */
typedef struct CBSBlockStruct *CBSBlock;
extern Bool CBSCheck(CBS);
extern Bool CBSBlockCheck(CBSBlock);
extern Size CBSBlockSize(CBSBlock);
extern Addr CBSBlockBase(CBSBlock);
extern Addr CBSBlockLimit(CBSBlock);

/* For #include "poolmv2.h" */

/* For #include "mpscmv2.h" */

SRCID(poolmv2, "$HopeName: MMsrc!poolmv2.c(MMdevel_gavinm_splay.2) $");

/* Signatures */
#define MV2Sig ((Sig)0x5193F299) /* SIGnature MV2 */

/* Prototypes */
typedef struct MV2Struct *MV2;
typedef struct ABQStruct *ABQ;
static Bool MV2Check(MV2);
static Bool ABQCheck(ABQ);

/* Structures */

typedef struct ABQStruct
{
  Count count;
  int head, tail;
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
  /* A free > maxSize will be handeled exceptionally */
  Size maxSize;
  /* A splinter >= minSize returned from a buffer will be used ASAP */
  Size minSize;
  Seg splinterSeg;              /* Saved splinter seg */
  Addr splinterBase;            /* Saved splinter base */
  Size splinterSize;            /* Saved splinter size */
  Sig sig;
}MV2Struct;

/* Macros */
#define PoolPoolMV2(pool) PARENT(MV2Struct, poolStruct, (pool))
#define MV2Pool(mv2) (&(mv2)->poolStruct)
#define MV2ABQ(mv2) (&(mv2)->abqStruct)
#define CBSMV2(cbs) PARENT(MV2Struct, cbsStruct, (cbs))
#define MV2CBS(mv2) (&(mv2)->cbsStruct)
#define MV2segPref(mv2) (&(mv2)->segPrefStruct)

/* Like AVER, but side-effect never optimized away */
#define ENSURE(cond) ASSERT(cond, #cond)

/* Methods */
static Bool MV2Check(MV2 mv2)
{
  UNUSED(mv2);
  CHECKS(MV2, mv2);
  CHECKD(Pool, &mv2->poolStruct);
  CHECKL(mv2->poolStruct.class == &PoolClassMV2Struct);
  /* --- how to check these??? */
  /* CHECKD(CBS, &mv2->cbsStruct); */
  AVERT(CBS, &mv2->cbsStruct);
  /* CHECKD(ABQ, &mv2->abqStruct); */
  AVERT(ABQ, &mv2->abqStruct);
  CHECKD(SegPref, &mv2->segPrefStruct);
  CHECKL(mv2->reuseSize > 0);
  CHECKL(mv2->abqOverflow == FALSE || mv2->abqOverflow == TRUE);
  CHECKL(mv2->contingency == FALSE || mv2->contingency == TRUE);
  CHECKL(mv2->maxSize >= mv2->reuseSize);
  CHECKL(mv2->minSize <= mv2->reuseSize);
  if (mv2->splinterSize != 0) {
    CHECKL(mv2->splinterSize > mv2->minSize);
    /* --- How to check Seg ??? */
    /* CHECKT(Seg, mv2->splinterSeg); */
    AVERT(Seg, mv2->splinterSeg);
    CHECKL(mv2->splinterBase >= SegBase(mv2->splinterSeg));
    CHECKL(AddrAdd(mv2->splinterBase, mv2->splinterSize) <= SegLimit(mv2->splinterSeg));
  }

  return TRUE;
}

static Bool ABQCheck(ABQ abq)
{
  UNUSED(abq);

  CHECKL(abq->count > 0);
  CHECKL(abq->head >= 0);
  CHECKL(abq->head < abq->count);
  CHECKL(abq->tail >= 0);
  CHECKL(abq->tail < abq->count);
  CHECKL(abq->queue != NULL);

  return TRUE;
}

static Size ABQqueueSize(Count count)
{
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

/* ABQFinish -- finish an ABQ */
static void ABQFinish(Arena arena, ABQ abq)
{
  AVERT(Arena, arena);
  AVERT(ABQ, abq);
  /* must be empty */
  AVER(abq->head == abq->tail);

  ArenaFree(arena, abq->queue, ABQqueueSize(abq->count));
  
  abq->count = 0;
  abq->queue = NULL;
}

static Bool ABQEmpty(ABQ abq) 
{
  AVERT(ABQ, abq);

  return abq->head == abq->tail;
}


/* ABQPop -- pop a block from the head of the ABQ */
static Res ABQPop(ABQ abq, CBSBlock *blockReturn)
{
  int index;
  
  AVER(blockReturn != NULL);
  AVERT(ABQ, abq);

  index = abq->head;
  if (index == abq->tail)
    return ResFAIL;

  if (++index >= abq->count)
    index = 0;
  abq->head = index;

  *blockReturn = abq->queue[index];
  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQPeek -- peek at the head of the ABQ */
static Res ABQPeek(ABQ abq, CBSBlock *blockReturn)
{
  int index;
  
  AVER(blockReturn != NULL);
  AVERT(ABQ, abq);

  index = abq->head;
  if (index == abq->tail)
    return ResFAIL;

  if (++index >= abq->count)
    index = 0;
  /* Identical to pop, but don't write index back into head */

  *blockReturn = abq->queue[index];
  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQPush -- push a block onto the tail of the ABQ */
static Res ABQPush(ABQ abq, CBSBlock block)
{
  int index;

  AVERT(ABQ, abq);

  index = abq->tail;
  if (++index >= abq->count)
    index = 0;
  
  if (index == abq->head)
    return ResFAIL;
  abq->tail = index;

  abq->queue[index] = block;
  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQDelete -- delete a block from the ABQ */
static Res ABQDelete(ABQ abq, CBSBlock block)
{
  int index, last, count, done;
  CBSBlock *queue;
  Bool found = FALSE;

  AVERT(ABQ, abq);

  index = abq->head;
  done = abq->tail;
  count = abq->count;
  queue = abq->queue;
  
  while (index != done) {
    last = index;
    if (++index >= count)
      index = 0;
    
    if (queue[index] == block) {
      found = TRUE;
      break;
    }
  }

  /* index points to the node to be removed */
  while (index != done) {
    last = index;
    if (++index >= count)
      index = 0;

    queue[last] = queue[index];
  }
  
  if (found){
    abq->tail = last;
    AVERT(ABQ, abq);
    return ResOK;
  }
  
  AVERT(ABQ, abq);
  return ResFAIL;
}

/*
 * NoteNew -- Callback invoked when a block on the CBS >= reuseSize
 */
static void NoteNew(CBS cbs, CBSBlock block) 
{
  Res res;
  MV2 mv2;
  
  AVERT(CBS, cbs);
  mv2 = CBSMV2(cbs);
  AVERT(MV2, mv2);
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) >= mv2->reuseSize);
  
  res = ABQPush(MV2ABQ(mv2), block);
  if (res != ResOK) {
    /* See .impl.c.poolmv2.free.merge: */
    CBSBlock block;
    Addr base, limit;
    
    ENSURE(ABQPeek(MV2ABQ(mv2), &block) == ResOK);
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);

    while (base < limit) {
      Seg seg;
      Addr segBase, segLimit;
      
      ENSURE(SegOfAddr(&seg, PoolArena(MV2Pool(mv2)), base));
      segBase = SegBase(seg);
      segLimit = SegLimit(seg);
      if (base <= segBase && limit >= segLimit) {
        ENSURE(CBSDelete(MV2CBS(mv2), segBase, segLimit) == ResOK);
        SegFree(seg);
      }
      base = segLimit;
    }

    res = ABQPush(MV2ABQ(CBSMV2(cbs)), block);
    if (res != ResOK) {
      CBSMV2(cbs)->abqOverflow = TRUE;
    }
  }
}

/*
 * NoteDelete -- Callback invoked when a block on the CBS <= reuseSize
 */
static void NoteDelete(CBS cbs, CBSBlock block)
{
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) < CBSMV2(cbs)->reuseSize);

  ENSURE(ABQDelete(MV2ABQ(CBSMV2(cbs)), block) == ResOK);
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
  void* p;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  
  /* --- These should be ARGAVER's or something */
  minSize = va_arg(arg, Size);
  AVER(minSize > 0);
  medianSize = va_arg(arg, Size);
  AVER(medianSize >= minSize);
  maxSize = va_arg(arg, Size);
  AVER(maxSize >= medianSize);
  reserveDepth = va_arg(arg, Count);
  AVER(reserveDepth > 0);

  reuseSize = SizeAlignUp(maxSize, ArenaAlign(arena));
  abqDepth = (reserveDepth * medianSize + reuseSize - 1) / reuseSize;

  res = CBSInit(arena, MV2CBS(mv2), NoteNew, NoteDelete, reuseSize);
  if (res != ResOK)
    goto failCBS;
  
  res = ABQInit(arena, MV2ABQ(mv2), abqCount);
  if (res != ResOK)
    goto failABQ;

  {
    RefSet refset;
    /* --- Loci needed here, what should the pref be? */
    /* --- why non SegPrefDefault(MV2segPref)? */
    *MV2segPref(mv2) = *SegPrefDefault();
    /* +++ At least get me my own RefSet */
    refset = RefSetComp(ARENA_DEFAULT_REFSET);
    SegPrefExpress(MV2segPref(mv2), SegPrefRefSet, (void *)&refset);
  }

  mv2->reuseSize = reuseSize;
  mv2->abqOverflow = FALSE;
  mv2->contingency = FALSE;
  mv2->maxSize = maxSize;
  mv2->minSize = minSize;
  mv2->splinterBase = (Addr)0;
  mv2->splinterSeg = NULL;
  mv2->splinterSize = 0;

  mv2->sig = MV2Sig;

  AVERT(MV2, mv2);
  return ResOK;

failABQ:
  CBSFinish(MV2CBS(mv2));
failCBS:
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

  /* +++ Free up all the blocks in the ABQ and CBS */
  
  /* Free the ABQ and CBS structures */
  CBSFinish(MV2CBS(mv2));
  ABQFinish(arena, MV2ABQ(mv2));

  /* --- Finish segPref? */

  mv2->sig = SigInvalid;
}


static void EnsureABQ(MV2 mv2) 
{
  AVERT(MV2, mv2);

  if (mv2->abqOverflow && ABQEmpty(MV2ABQ(mv2))) {
    mv2->abqOverflow = FALSE;
    CBSSetMinSize(MV2CBS(mv2), mv2->reuseSize);
  }
}


/* MV2BufferFill -- refill an allocation buffer
 *
 * See design.mps.poolmv2.impl.c.poolmv2.ap.fill
 */

static Res MV2BufferFill(Seg *segReturn,
                         Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  Seg seg;
  MV2 mv2;
  Res res;
  Addr base, limit;
  Arena arena;
  Size minSize, idealSize, reuseSize;
  CBSBlock block;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);
  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(size >  0);

  arena = PoolArena(pool);
  reuseSize = mv2->reuseSize;
  minSize = SizeAlignUp(size, ArenaAlign(arena));
  idealSize = reuseSize;

  /* Allocate oversized blocks directly from areana */
  if (minSize > reuseSize) {
    idealSize = minSize;
    goto direct;
  }

  /* Use any splinter if available */
  if (mv2->splinterSize > minSize) {
    seg = mv2->splinterSeg;
    base = mv2->splinterBase;
    limit = AddrAdd(base, mv2->splinterSize);
    mv2->splinterSize = 0;
    
    goto done;
  }
  
  /* Attempt to retrieve a free block from the ABQ */
  EnsureABQ(mv2);
  res = ABQPeek(MV2ABQ(mv2), &block);
  if (res == ResOK) {
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);
    ENSURE(SegOfAddr(&seg, arena, base));
    /* Use the whole block if the remnant would not stay in the ABQ */
    if (AddrOffset(base, limit) - idealSize > reuseSize) {
      limit = AddrAdd(base, idealSize);
    }
    ENSURE(CBSDelete(MV2CBS(mv2), base, limit) == ResOK);
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
    goto done;
  }

  /* Enter contingency mode */
  if (!mv2->contingency) {
    mv2->contingency = TRUE;
    goto retry;
  }

  /* Try minimum */
  if (idealSize != minSize) {
    idealSize = minSize;
    goto direct;
  }
  
fail:
  return res;
  
done:
  /* Give the buffer the entire segment to allocate in. */
  *segReturn = seg;
  *baseReturn = base;
  *limitReturn = limit;
  EVENT_PPWAW(MV2BufferFill, mv2, buffer, size, base, AddrOffset(base, limit));
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
    Size splinterSize = mv2->splinterSize;
    
    /* discard any previous splinter */
    if (splinterSize != 0) {
      ENSURE(CBSInsert(MV2CBS(mv2), mv2->splinterBase, AddrAdd(mv2->splinterBase, splinterSize))
             == ResOK);
    }
    mv2->splinterSeg = seg;
    mv2->splinterBase = base;
    mv2->splinterSize = size;
  }
  else {
    ENSURE(CBSInsert(MV2CBS(mv2), base, limit) == ResOK);
  }
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

  /* We know the buffer observes pool->alignement  */
  size = SizeAlignUp(size, pool->alignment);
  limit = AddrAdd(base, size);
  
  /* --- return exceptional blocks directly to arena ??? */
  
  ENSURE(CBSInsert(MV2CBS(mv2), base, limit) != ResOK);
}
