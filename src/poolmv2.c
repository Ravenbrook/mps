/* impl.c.poolmv2: MANUAL VARIABLE POOL, II
 *
 * $HopeName: MMsrc!poolmv2.c(MMdevel_gavinm_splay.10) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .limitation.seg : MV2BufferFill may fill a buffer with a range that
 * spans more than one segment.  SegBuffer will only mark the first
 * such segment as being buffered.  Using multiple segments as a
 * substitute for pages is a short-term solution.
 */

#include "mpm.h"
#include "poolmv2.h"
#include "mpscmv2.h"
#include "abq.h"
#include "meter.h"

SRCID(poolmv2, "$HopeName: MMsrc!poolmv2.c(MMdevel_gavinm_splay.10) $");

/* Signatures */
#define MV2Sig ((Sig)0x5193F299) /* SIGnature MV2 */

/* Prototypes */
typedef struct MV2Struct *MV2;
static Bool MV2Check(MV2 mv2);


typedef struct MV2Struct 
{
  PoolStruct poolStruct;
  CBSStruct cbsStruct;          /* The coalescing block structure */
  ABQStruct abqStruct;          /* The available block queue */
  SegPrefStruct segPrefStruct;  /* The preferences for segments */
  Size reuseSize;               /* Size at which blocks are recycled */
  Size minSize;
  Size medianSize;
  Size maxSize;
  Bool abqOverflow;             /* ABQ dropped some candidates */
  Bool contingency;             /* High fragmentation mode */
  /* A splinter >= minSize returned from a buffer will be used ASAP */
  /* See design.mps.poolmv2.arch.ap.no-fit.* */
  Bool splinter;                /* Saved splinter */
  Seg splinterSeg;              /* Saved splinter seg */
  Addr splinterBase;            /* Saved splinter base */
  Addr splinterLimit;           /* Saved splinter size */

  /* pool accounting */
  Size size;                    /* size of segs in pool */
  Size available;               /* bytes available for allocation */
  
  /* pool meters*/
  METER_DECL(segAllocs);
  METER_DECL(segFrees);
  METER_DECL(bufferFills);
  METER_DECL(bufferEmpties);
  METER_DECL(poolFrees);
  METER_DECL(poolSize);
  METER_DECL(poolAvailable);
  /* abq meters */
  METER_DECL(overflows);
  METER_DECL(refills);
  METER_DECL(refillPushes);
  METER_DECL(refillOverflows);
  /* contingency meters */
  METER_DECL(contingencies);
  METER_DECL(contingencySearches);
  /* splinter meters */
  METER_DECL(splinters);
  METER_DECL(splintersUsed);
  METER_DECL(splintersDropped);
  METER_DECL(sawdust);
  /* exception meters */
  METER_DECL(exceptions);
  METER_DECL(exceptionSplinters);
  
  Sig sig;
} MV2Struct;


/* Macros */
#define PoolPoolMV2(pool) PARENT(MV2Struct, poolStruct, (pool))
#define MV2Pool(mv2) (&(mv2)->poolStruct)
#define MV2ABQ(mv2) (&(mv2)->abqStruct)
#define CBSMV2(cbs) PARENT(MV2Struct, cbsStruct, (cbs))
#define MV2CBS(mv2)	(&(mv2)->cbsStruct)
#define MV2segPref(mv2) (&(mv2)->segPrefStruct)
/* .trans.something the C language sucks */
#define unless(cond) if (!(cond))
#define when(cond) if (cond)


/* Methods */

/*
 * MV2SegAlloc -- encapsulates SegAlloc with associated accounting and
 * metering
 */
static Res MV2SegAlloc(Seg *segReturn, MV2 mv2, Size size, Pool pool)
{
  Res res = SegAlloc(segReturn, MV2segPref(mv2), size, pool);

  if (res == ResOK) {
    Size segSize = SegSize(*segReturn);
    
    mv2->size += segSize;
    METER_ACC(mv2->poolSize, mv2->size);
    mv2->available += segSize;
    METER_ACC(mv2->poolAvailable, mv2->available);
    METER_ACC(mv2->segAllocs, segSize);
  }
  return res;
}
  

/*
 * MV2SegFree -- encapsulates SegFree with associated accounting and
 * metering
 */
static void MV2SegFree(MV2 mv2, Seg seg) 
{
  Size size = SegSize(seg);

  mv2->available -= size;
  METER_ACC(mv2->poolAvailable, mv2->available);
  mv2->size -= size;
  METER_ACC(mv2->poolSize, mv2->size);
  SegFree(seg);
  METER_ACC(mv2->segFrees, size);
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
      Res r = ABQPeek(MV2ABQ(mv2), &oldBlock);
      AVER(r == ResOK);
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
        {
          Res r = CBSDelete(MV2CBS(mv2), segBase, segLimit);
          AVER(r == ResOK);
        }
        MV2SegFree(mv2, seg);
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
    AVER(res == ResOK || CBSMV2(cbs)->abqOverflow);
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
  mv2->medianSize = medianSize;
  mv2->maxSize = maxSize;
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
  METER_INIT(mv2->poolSize, "pool size");
  METER_INIT(mv2->poolAvailable, "pool available");
  METER_INIT(mv2->overflows, "ABQ overflows");
  METER_INIT(mv2->refills, "ABQ refills");
  METER_INIT(mv2->refillPushes, "ABQ refill pushes");
  METER_INIT(mv2->refillOverflows, "ABQ refill overflows");
  METER_INIT(mv2->contingencies, "contingencies");
  METER_INIT(mv2->contingencySearches, "contingency searches");
  METER_INIT(mv2->splinters, "splinters");
  METER_INIT(mv2->splintersUsed, "splinters used");
  METER_INIT(mv2->splintersDropped, "splinters dropped");
  METER_INIT(mv2->sawdust, "sawdust");
  METER_INIT(mv2->exceptions, "exceptions");
  METER_INIT(mv2->exceptionSplinters, "exception splinters");

  mv2->sig = MV2Sig;

  AVERT(MV2, mv2);
  return ResOK;

failABQ:
  CBSFinish(MV2CBS(mv2));
failCBS:
  AVER(res != ResOK);
  return res;
}


/*
 * MV2Finish
 */
static void MV2Finish(Pool pool)
{
  MV2 mv2;
  Arena arena;
  Ring ring;
  Ring node, nextNode;
  
  AVERT(Pool, pool);
  mv2 = PoolPoolMV2(pool);
  AVERT(MV2, mv2);
  arena = PoolArena(pool);
  AVERT(Arena, arena);

  EVENT_P(MV2Finish, mv2);

  /* Free the segments in the pool */
  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    MV2SegFree(mv2, SegOfPoolRing(node));
  }

  /* Finish the ABQ and CBS structures */
  CBSFinish(MV2CBS(mv2));
  ABQFinish(arena, MV2ABQ(mv2));

  mv2->sig = SigInvalid;
}


/*
 * ABQRefillCallback -- Called from CBSIterate at the behest of
 * ABQRefillIfNecessary
 */
static Bool ABQRefillCallback(CBS cbs, CBSBlock block, void *closureP,
                              unsigned long closureS)
{
  Res res;
  MV2 mv2;
  
  AVERT(CBS, cbs);
  mv2 = CBSMV2(cbs);
  AVERT(MV2, mv2);
  AVERT(ABQ, MV2ABQ(mv2));
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) >= mv2->reuseSize);
  UNUSED(closureP);
  UNUSED(closureS);

  METER_ACC(mv2->refillPushes, ABQDepth(MV2ABQ(mv2)));
  res = ABQPush(MV2ABQ(mv2), block);
  if (res != ResOK) {
    mv2->abqOverflow = TRUE;
    METER_ACC(mv2->refillOverflows, CBSBlockSize(block));
  }

  return res == ResOK;
}
  

/*
 * ABQRefillIfNecessary -- Refill the ABQ if it had overflown and is
 * now empty.
 */
static void ABQRefillIfNecessary(MV2 mv2, Size size) 
{
  AVERT(MV2, mv2);

  if (mv2->abqOverflow && ABQIsEmpty(MV2ABQ(mv2))) {
    mv2->abqOverflow = FALSE;
    METER_ACC(mv2->refills, size);
    CBSIterateLarge(MV2CBS(mv2), &ABQRefillCallback, NULL, 0);
  }
}


typedef struct MV2ClosureStruct *MV2Closure;

typedef struct MV2ClosureStruct 
{
  CBSBlock block;
  Size min;
  /* meters */
  Count steps;
} MV2ClosureStruct;


/*
 * MV2ContingencyCallback -- Called from CBSIterate at the behest of
 * MV2ContingencySearch
 */
static Bool MV2ContingencyCallback(CBS cbs, CBSBlock block, void *closureP,
                                   unsigned long closureS)
{
  MV2Closure cl;
  
  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);
  AVER(closureP != NULL);
  AVER(closureS == sizeof(MV2ClosureStruct));

  cl = (MV2Closure)closureP;
  
  cl->steps++;
  if (CBSBlockSize(block) > cl->min) {
     cl->block = block;
    return FALSE;
  }

  return TRUE;
}


/*
 * MV2ContingencySearch -- Search the CBS for a block of size min
 */
static Res MV2ContingencySearch(CBSBlock *blockReturn, CBS cbs, Size min)
{
  MV2ClosureStruct cls;

  AVERT(CBS, cbs);
  AVER(blockReturn != NULL);
  AVER(min > 0);

  cls.block = NULL;
  cls.min = min;
  cls.steps = 0;
  
  CBSIterate(cbs, &MV2ContingencyCallback, (void *)&cls, (unsigned long)sizeof(cls));
  if (cls.block != NULL) {
    AVER(CBSBlockSize(cls.block) >= min);
    METER_ACC(CBSMV2(cbs)->contingencySearches, cls.steps);
    *blockReturn = cls.block;
    return ResOK;
  }
    
  return ResFAIL;
}


/*
 * MV2BufferFill -- refill an allocation buffer
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
    res = MV2SegAlloc(&seg, mv2, alignedSize, pool);
    if (res == ResOK) {
      base = SegBase(seg);
      limit = AddrAdd(base, idealSize);
      METER_ACC(mv2->exceptions, minSize);
      METER_ACC(mv2->exceptionSplinters, alignedSize - minSize);
      goto done;
    }
    AVER(res != ResOK);
    return res;
  }

  /* design.mps.poolmv2.arch.ap.no-fit.return */
  /* Use any splinter, if available */
  if (mv2->splinter) {
    base = mv2->splinterBase;
    limit = mv2->splinterLimit;
    if(AddrOffset(base, limit) >= minSize) {
      seg = mv2->splinterSeg;
      mv2->splinter = FALSE;
      METER_ACC(mv2->splintersUsed, AddrOffset(base, limit));
      goto done;
    }
  }
  
retry:
  /* Attempt to retrieve a free block from the ABQ */
  ABQRefillIfNecessary(mv2, idealSize);
  res = ABQPeek(MV2ABQ(mv2), &block);
  if (res != ResOK) {
    /* If contingency mode, search the CBS */
    if (mv2->contingency) {
      res = MV2ContingencySearch(&block, MV2CBS(mv2), minSize);
    }
  }
  if (res == ResOK) {
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);
    {
      Bool b = SegOfAddr(&seg, arena, base);
      AVER(b);
    }
    /* Use the whole block unless the remnant would stay in the ABQ */
    if (AddrOffset(base, limit) > idealSize + reuseSize) {
      limit = AddrAdd(base, idealSize);
    }
    {
      Res r = CBSDelete(MV2CBS(mv2), base, limit);
      AVER(r == ResOK);
    }
    goto done;
  }
  
  /* Attempt to request a block from the arena */
  res = MV2SegAlloc(&seg, mv2, idealSize, pool);
  if (res == ResOK) {
    base = SegBase(seg);
    limit = SegLimit(seg);
    goto done;
  }

  /* Enter contingency mode if not already there */
  if (!mv2->contingency) {
    mv2->contingency = TRUE;
    METER_ACC(mv2->contingencies, idealSize);
  }

  /* Try minimum */
  if (idealSize > alignedSize) {
    idealSize = alignedSize;
    goto retry;
  }
  
  AVER(res != ResOK);
  return res;
  
done:
  /* --- base/limit may span more than one segment */
  *segReturn = seg;
  *baseReturn = base;
  *limitReturn = limit;
  mv2->available -= AddrOffset(base, limit);
  METER_ACC(mv2->poolAvailable, mv2->available);
  METER_ACC(mv2->bufferFills, AddrOffset(base, limit));
  EVENT_PPWAW(MV2BufferFill, mv2, buffer, minSize, base,
              AddrOffset(base, limit));
  AVER(AddrOffset(base, limit) >= minSize);
  return ResOK;
}


/*
 * MV2BufferEmpty -- detach a buffer from a segment
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
  mv2->available += size;
  METER_ACC(mv2->poolAvailable, mv2->available);
  METER_ACC(mv2->bufferEmpties, size);

  if (size == 0)
    return;

  /* discard sawdust */
  if (size < mv2->minSize) {
    Res res = CBSInsert(MV2CBS(mv2), base, limit);
    AVER(res == ResOK);
    METER_ACC(mv2->sawdust, size);
    return;
  }

  METER_ACC(mv2->splinters, size);
  /* put the splinter at the effective head of the ABQ, unless there is
     already one there that is larger */
  if (mv2->splinter) {
    Size oldSize = AddrOffset(mv2->splinterBase, mv2->splinterLimit);

    /* Old better, drop new */
    if (size < oldSize) {
      Res res = CBSInsert(MV2CBS(mv2), base, limit);
      AVER(res == ResOK);
      METER_ACC(mv2->splintersDropped, size);
      return;
    }
    else {
      /* New better, drop old */
      Res res = CBSInsert(MV2CBS(mv2), mv2->splinterBase,
			  mv2->splinterLimit);
      AVER(res == ResOK);
      METER_ACC(mv2->splintersDropped, oldSize);
    }
  }

  /* --- MV2 may put more than one segment in a buffer, so find the
     (base) segment of the splinter */
  {
    Bool b = SegOfAddr(&seg, PoolArena(pool), base);
    AVER(b);
  }
  mv2->splinter = TRUE;
  mv2->splinterSeg = seg;
  mv2->splinterBase = base;
  mv2->splinterLimit = limit;
}


/*
 * MV2Free -- free a block
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
  METER_ACC(mv2->poolAvailable, mv2->available);
}


/*
 * MV2Describe -- Describe an MV2 pool
 */
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
	       "  medianSize: $U \n", (WriteFU)mv2->medianSize,
	       "  maxSize: $U \n", (WriteFU)mv2->maxSize,
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
  METER_WRITE(mv2->segAllocs, stream);
  METER_WRITE(mv2->segFrees, stream);
  METER_WRITE(mv2->bufferFills, stream);
  METER_WRITE(mv2->bufferEmpties, stream);
  METER_WRITE(mv2->poolFrees, stream);
  METER_WRITE(mv2->poolSize, stream);
  METER_WRITE(mv2->poolAvailable, stream);
  METER_WRITE(mv2->overflows, stream);
  METER_WRITE(mv2->refills, stream);
  METER_WRITE(mv2->refillPushes, stream);
  METER_WRITE(mv2->refillOverflows, stream);
  METER_WRITE(mv2->contingencies, stream);
  METER_WRITE(mv2->contingencySearches, stream);
  METER_WRITE(mv2->splinters, stream);
  METER_WRITE(mv2->splintersUsed, stream);
  METER_WRITE(mv2->splintersDropped, stream);
  METER_WRITE(mv2->sawdust, stream);
  METER_WRITE(mv2->exceptions, stream);
  METER_WRITE(mv2->exceptionSplinters, stream);
  
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


/*
 * MV2Check -- validate an MV2 Pool
 */
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
    /* CHECKT(Seg, mv2->splinterSeg); */
    CHECKL(SegCheck(mv2->splinterSeg));
    CHECKL(mv2->splinterBase >= SegBase(mv2->splinterSeg));
    /* --- even a splinter may consist of more than one seg */
    /* CHECKL(mv2->splinterLimit <= SegLimit(mv2->splinterSeg)); */
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

/* --- should these be pool generics? */

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

/* Total bytes */

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

