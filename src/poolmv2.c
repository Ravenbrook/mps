/* impl.c.poolmv2: NEW MANUAL VARIABLE POOL
 *
 * $HopeName:  $
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

SRCID(poolmv2, "$HopeName: $");

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
  SegPrefStruct segPrefStruct;  /* the preferences for segments */
  Size reuseSize;               /* Size at which blocks are recycled */
  Bool contingency;
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
Res ABQInit(Arena arena, ABQ abq, Count count)
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
void ABQFinish(Arena arena, ABQ abq)
{
  AVERT(Arena, arena);
  AVERT(ABQ, abq);
  /* must be empty */
  AVERT(abq->head == abq->tail);

  ArenaFree(arena, abq->queue, ABQqueueSize(abq->count));
  
  abq->count = 0;
  abq->queue = NULL;
}

/* ABQPop -- pop a block from the head of the ABQ */
Res ABQPop(ABQ abq, Addr *addrReturn)
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
Res ABQPop(ABQ abq, Addr *addrReturn)
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
Res ABQPush(ABQ abq, Addr addr)
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
Res ABQDelete(ABQ abq, Addr addr)
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

static void noteNew(void **pReturn, CBS cbs, Addr base, Addr limit) 
{
  Res res;
  
  UNUSED(limit);
  AVER(pReturn != NULL);
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVERT(base < limit);
  
  /* --- why not just give me the node? */
  res = ABQPush(MV2ABQ(CBSMV2(cbs)), base);
  if (res == ResOK) {
    /* why not just yes/no for interest? */
    *pReturn = (void *)base;
  }
  /* +++ return segment policy */
}

static void noteShrink(void **pIO, CBS cbs, Addr base, Addr limit) 
{
  UNUSED(pIO);
  UNUSED(cbs);
  UNUSED(base);
  UNUSED(limit);
  AVER(pIO != NULL);
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVER(*pIO == (void *)base);
  AVERT(base < limit);
  
}

static void noteGrow(void **pIO, CBS cbs, Addr base, Addr limit)
{
  UNUSED(pIO);
  UNUSED(cbs);
  UNUSED(base);
  UNUSED(limit);
  AVER(pIO != NULL);
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVER(*pIO == (void *)base);
  AVERT(base < limit);

}

  
static void noteDelete(void *p, CBS cbs)
{
  Res res;
  
  UNUSED(pIO);
  UNUSED(limit);
  AVER(p != NULL);
  AVERT(CBS, cbs);
  AVERT(MV2, CBSMV2(cbs));
  AVER(*pIO == (void *)base);
  AVERT(base < limit);

  res = ABQDelete(MV2ABQ(CBSMV2(cbs)), base);
  if (res == ResOK) {
    NOTREACHED;
  }
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

  res = CBSInit(arena, MV2CBS(mv2), noteNew, noteShrink, noteGrow, noteDelete, reuseSize);
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

  /* Finish segPref? */

  mv2->sig = SigInvalid;
}


/* MV2BufferFill -- refill an allocation buffer
 *
 * See design.mps.poolmv2..impl.c.poolmv2.ap.fill:
 * An AP fill request will be handled as follows:
 * o If the request is larger than max size, attempt to request a segment
 * from the arena sufficient to satisfy the request
 * o Attempt to retrieve a free block from the head of the ABQ (removing
 * it from ABQ and CBS if found).
 * o If contingency mode, attempt to find a block on the CBS, using oldest-fit search
 * o Attempt to request a block of max size from the arena
 * o Attempt to find a block on the CBS, using oldest-fit search
 * o Otherwise, fail
 */

static Res MV2BufferFill(Seg *segReturn,
                         Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  Seg seg;
  MV2 mv2;
  Res res;
  Addr base;
  Addr limit;
  Arena arena;
  Size alignedSize;

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
  alignedSize = SizeAlignUp(size, ArenaAlign(arena));

  /* Allocate oversized blocks directly from arena */
  if (alignedSize > mv2->reuseSize) {
    res = SegAlloc(&seg, MV2segPref(mv2), alignedSize, pool);
    if (res == ResOK) {
      base = SegBase(seg);
      limit = AddrAdd(base, alignedSize);
      goto done;
    }
    /* --- switch to contingency mode ??? */
    return res;
  }

  alignedSize = mv2->reuseSize;

  /* Attempt to retrieve a free block from the ABQ */
  res = ABQPeek(MV2ABQ(mv2), &base);
  if (res == ResOK) {
    seg = segOfAddr(base);
    /* --- use the whole block if it is a splinter? */
    limit = AddrAdd(base, alignedSize);
    res = CBSDelete(MV2CBS(mv2), base, limit);
    if (res == ResOK)
      goto done;
    NOTREACHED;
  }
  
  /* If contingency mode, search the CBS using oldest-fit */
  /* +++ */

  /* Attempt to request a block from the arena */
  res = SegAlloc(&seg, MV2segPref(mv2), alignedSize, pool);
  if (res == ResOK) {
    base = SegBase(seg);
    limit = AddrAdd(base, alignedSize);
    goto done;
  }

  /* Attempt to find a block on the CBS, using oldest-fit */
  /* +++ */

fail:
  return res;
  
done:
  /* Give the buffer the entire segment to allocate in. */
  *segReturn = seg;
  *baseReturn = base;
  *limitReturn = limit;
  EVENT_PPWAW(MV2BufferFill, mv2, buffer, size, base, alignedSize);
  return ResOK;
}

