/* impl.c.poolmv2: NEW MANUAL VARIABLE POOL
 *
 * $HopeName: MMsrc!poolmv2.c(MMdevel_gavinm_splay.1) $
 * Copyright (C) 1998 The Harlequin Group Limited.  All rights reserved.
 *
 * .purpose: The implementation of the new manual-variable pool class
 *
 * .readership: Any MPS developer
 * 
 * .design: See design.mps.poolmv2
 */

#include "mpm.h"
#include "poolmv2.h"
#include "mpscmv2.h"

#include "cbs.h"

SRCID(poolmv2, "$HopeName: MMsrc!poolmv2.c(MMdevel_gavinm_splay.1) $");

/* Signatures */
#define MV2Sig ((Sig)0x5193F299) /* SIGnature MV2 */

/* Prototypes */
typedef struct MV2Struct *MV2;
static Bool MV2Check(MV2)
static Bool ABQCheck(ABQ)

/* Structures */

typedef struct MV2Struct 
{
  PoolStruct poolStruct;
  CBSStruct cbsStruct;          /* The coalescing block structure */
  ABQStruct abqStruct;          /* The available block queue */
  SegPrefStruct segPrefStruct;  /* The preferences for segments */
  Size reuseSize;               /* Size at which blocks are recycled */
  Bool abqOverflow;             /* ABQ dropped some candidates */
  Bool contingency;             /* High fragmentation mode */
  Sig sig;
}MV2Struct;

static Bool MV2Check(MV2 mv2)
{
  UNUSED(mv2);
  CHECKS(MV2, mv2);
  CHECKD(Pool, &mv2->poolStruct);
  CHECKL(mv2->poolStruct.class == &PoolClassMV2Struct);
  CHECKD(CBS, &mv2->cbsStruct);
  CHECKD(ABQ, &mv2->abqStruct);
  CHECKD(SegPref, &mv2->segPrefStruct);
  CHECKL(mv2->reuseSize > 0);
  CHECKL(mv2->abqOverflow == FALSE || mv2->abqOverflow == TRUE);
  CHECKL(mv2->contingency == FALSE || mv2->contingency == TRUE);

  return TRUE;
}

typedef struct ABQStruct
{
  Count count;
  int head, tail;
  Addr *queue;
}ABQStruct;

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

/* Macros */
#define PoolPoolMV2(pool) PARENT(MV2Struct, poolStruct, (pool))
#define MV2Pool(mv2) (&(mv2)->poolStruct)
#define MV2ABQ(mv2) (&(mv2)->abqStruct)
#define CBSMV2(cbs) PARENT(cbsStruct, MV2Struct, (cbs))
#define MV2CBS(mv2) (&(mv2)->cbsStruct)
#define MV2segPref(mv2) (&(mv2)->segPrefStruct)

/* Methods */
static Size ABQqueueSize(Count count)
{
  return (Size)(sizeof((MV2Struct)0.queue[0]) * count);
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
  abq->queue = (Addr *)p;

  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQFinish -- finish an ABQ */
static void ABQFinish(Arena arena, ABQ abq)
{
  AVERT(Arena, arena);
  AVERT(ABQ, abq);
  /* must be empty */
  AVERT(abq->head == abq->tail);

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
static Res ABQPop(ABQ abq, Addr *addrReturn)
{
  int index;
  
  AVER(addrReturn != NULL);
  AVERT(ABQ, abq);

  index = abq->head;
  if (index == abq->tail)
    return ResFAIL;

  if (++index >= abq->count)
    index = 0;
  abq->head = index;

  *addrReturn = abq->queue[index];
  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQPeek -- peek at the head of the ABQ */
static Res ABQPop(ABQ abq, Addr *addrReturn)
{
  int index;
  
  AVER(addrReturn != NULL);
  AVERT(ABQ, abq);

  index = abq->head;
  if (index == abq->tail)
    return ResFAIL;

  if (++index >= abq->count)
    index = 0;
  /* Identical to pop, but don't write index back into head */

  *addrReturn = abq->queue[index];
  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQPush -- push a block onto the tail of the ABQ */
static Res ABQPush(ABQ abq, Addr addr)
{
  int index;

  AVERT(ABQ, abq);

  index = abq->tail;
  if (++index >= abq->count)
    index = 0;
  
  if (index == abq->head)
    return ResFAIL;
  abq->tail = index;

  abq->queue[index] = addr;
  AVERT(ABQ, abq);
  return ResOK;
}

/* ABQDelete -- delete a block from the ABQ */
static Res ABQDelete(ABQ abq, Addr addr)
{
  int index, last, count, done;
  Addr *queue;
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
    
    if (queue[index] == addr) {
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

static void noteNew(CBS cbs, CBSBlock block) 
{
  Res res;
  
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) >= CBSMV2(cbs)->reuseSize);
  
  res = ABQPush(MV2ABQ(CBSMV2(cbs)), block);
  if (res != ResOK) {
    /* +++ return segment policy */
    CBSMV2(cbs)->abqOverflow = TRUE;
  }
}

static void noteDelete(CBS cbs, CBSBlock block)
{
  Res res;
  
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) < CBSMV2(cbs)->reuseSize);

  res = ABQDelete(MV2ABQ(CBSMV2(cbs)), base);
  if (res == ResOK) {
    return;
  }
  NOTREACHED;
}
  
static Res MV2Init(Pool pool, va_list arg)
{
  /* --- ABQ decay rate */
  Arena arena;
  Size reuseSize;
  Count abqCount;
  MV2 mv2;
  Res res;
  void* p;

  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  
  reuseSize = va_arg(arg, Size);
  AVER(resuseSize > 0);
  abqCount = va_arg(arg, Count);
  AVER(abqCount > 0);

  res = CBSInit(arena, MV2CBS(mv2), noteNew, noteDelete, reuseSize);
  if (res != ResOk)
    goto failCBS;
  
  res = ABQInit(arena, MV2ABQ(mv2), abqCount);
  if (res != ResOk)
    goto failABQ;

  /* --- Loci needed here, what should the pref be? */
  /* --- why non SegPrefDefault(MV2segPref)? */
  *MV2segPref(mv2) = *SegPrefDefault();
  /* +++ At least get me my own RefSet */
  refset = RefSetComp(ARENA_DEFAULT_REFSET);
  SegPrefExpress(MV2segPref(mv2), SegPrefRefSet, (void *)&refset);

  mv2->reuseSize = SizeAlignUp(reuseSize, ArenaAlign(arena));
  mv2->abqOverflow = FALSE;
  mv2->contingency = FALSE;

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


static void EnsureABQ(mv2) 
{
  AVERT(MV2, mv2);

  if (mv2->abqOverflow && ABQEmpty(MV2ABQ(mv2))) {
    mv2->abqOverflow = FALSE;
    CBSSetMinSize(MV2CBS(mv2), reuseSize);
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

  /* Attempt to retrieve a free block from the ABQ */
  EnsureABQ(mv2);
  res = ABQPeek(MV2ABQ(mv2), &block);
  if (res == ResOK) {
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);
    seg = segOfAddr(base);
    /* Only use whole block if it would leave a splinter smaller than reuse size*/
    if (AddrOffset(base, limit) - idealSize > reuseSize) {
      limit = AddrAdd(base, idealSize);
    }
    res = CBSDelete(MV2CBS(mv2), base, limit);
    if (res == ResOK)
      goto done;
    NOTREACHED;
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
  
  res = CBSInsert(MV2CBS(mv2), base, limit);
  if (res == ResOK) {
    /* +++ put splinter > minSize at head of ABQ */
    return;
  }
  
  NOTREACHED;
}
