/* impl.c.poolepdl: ELECTRONIC PUBLISHING DISPLAY LIST POOL
 * 
 * $HopeName: MMsrc!poolepdl.c(trunk.38) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 *
 * .purpose: This is a pool class for EPcore display lists.
 *
 * .design: design.mps.poolepdl
 *
 * .depend: We depend on SegNext working in address order.
 */

#include "dbgpool.h"
#include "poolepdl.h"
#include "poolmfs.h"
#include "mpscepdl.h"
#include "mpm.h"

SRCID(poolepdl, "$HopeName: MMsrc!poolepdl.c(trunk.38) $");


/* EPDLBlockStruct -- block structure
 *
 * The pool maintains a descriptor structure for each contiguous free
 * block of memory it manages. The descriptor is on a simple
 * linked-list of such descriptors (the free list), which is in
 * ascending order of address. See design.mps.poolepdl
 */

typedef struct EPDLBlockStruct *EPDLBlock;
typedef struct EPDLBlockStruct {
  EPDLBlock next;
  Addr base;
  Addr limit;
  Size size;
} EPDLBlockStruct;


/* Debug Instrumentation */

#define DEBUG_SIZE(x)   STATISTIC_DECL(Size debug_##x);
#define DEBUG_DOUBLE(x) STATISTIC_DECL(double debug_##x);
#define DEBUG_CLEAR(x)  STATISTIC(epdl->debug_##x = 0)
#define DEBUG_ADD(x, n) STATISTIC(epdl->debug_##x += (n))
#define DEBUG_INC(x)    DEBUG_ADD(x, 1)
#define DEBUG_MAX(x, n) \
  STATISTIC((epdl->debug_##x < (n)) && (epdl->debug_##x = (n), TRUE))
#define DEBUG_WRITE(x) \
  STATISTIC_WRITE("  debug: " #x "    $U\n", (WriteFU)epdl->debug_##x)
#define DEBUG_WRITE_DOUBLE(x) \
  STATISTIC_WRITE("  debug: " #x "    $UkB\n", \
                  (WriteFU)((Size)(epdl->debug_##x / 1024)))


/* EPDLStruct -- EPDL pool outer structure */

#define EPDLSig           ((Sig)0x519EBd79) /* SIGnature EPDL */

typedef struct EPDLStruct *EPDL;
typedef struct EPDLStruct {     /* EPDL pool outer structure */
  PoolStruct poolStruct;        /* generic structure */
  Pool blockPool;               /* for managing block descriptors */
  SegPref segPref;              /* the preferences for segments */
  Size extendBy;                /* segment size to extend pool by */
  Size avgSize;                 /* client estimate of allocation size */
  Size total;                   /* total bytes in pool */
  Size free;                    /* total free bytes in pool */
  Size lost;                    /* total lost bytes in pool */
  Count length;                 /* length of free list */
  EPDLBlock freeList;           /* head of free list */

  DEBUG_SIZE(maxLength)             /* max length of free list */
  DEBUG_SIZE(maxFree)               /* max bytes free */
  DEBUG_SIZE(maxTotal)              /* max bytes owned by pool */
  DEBUG_SIZE(allocs)                /* number of successful allocations */
  DEBUG_SIZE(frees)                 /* number of frees */
  DEBUG_SIZE(allocsFailed)          /* number of failed allocations */
  DEBUG_SIZE(bytesAlloc)            /* bytes successfully allocated */
  DEBUG_SIZE(bytesFree)             /* bytes freed */
  DEBUG_SIZE(segAdd)                /* segments added */
  DEBUG_SIZE(segAddFail)            /* first SegAlloc failures */
  DEBUG_SIZE(segAddFailTwo)         /* second SegAlloc failures */
  DEBUG_SIZE(segFree)               /* segments freed */
  DEBUG_SIZE(segsFree)              /* number of frees which free segs */
  DEBUG_DOUBLE(segFreeBlockSkip)    /* free blocks examined to free segs */
  DEBUG_SIZE(segFreeBlockPerfect)   /* free blocks removed to free segs */
  DEBUG_SIZE(segFreeBlockHead)      /* blocks cut at head to free segs */
  DEBUG_SIZE(segFreeBlockTail)      /* blocks cut at tail to free segs */
  DEBUG_SIZE(segFreeBlockSplit)     /* blocks split to free segs */
  DEBUG_SIZE(bytesAdded)            /* bytes obtained from arena */
  DEBUG_SIZE(bytesReturned)         /* bytes returned to arena */
  DEBUG_SIZE(blockAdd)              /* blocks added to free list */
  DEBUG_SIZE(blockAddFail)          /* failures to add to free list */
  DEBUG_SIZE(blockFree)             /* blocks removed from free list */
  DEBUG_SIZE(allocListExamine)      /* allocations examining free list */
  DEBUG_DOUBLE(allocBlockExamine)   /* blocks examined to allocate */
  DEBUG_SIZE(perfect)               /* perfect fit for allocation */
  DEBUG_DOUBLE(freeBlockExamine)    /* blocks examined to free */
  DEBUG_SIZE(mergeBelow)            /* blocks merged below on freeing */
  DEBUG_SIZE(mergeAbove)            /* blocks merged above on freeing */
  DEBUG_SIZE(mergeBoth)             /* blocks merged at both ends */

  Sig sig; /* design.mps.pool.outer-structure.sig */
} EPDLStruct;


#define PoolPoolEPDL(pool)   PARENT(EPDLStruct, poolStruct, pool)
#define EPDLPool(epdl)       (&((epdl)->poolStruct))


typedef struct EPDLDebugStruct {
  EPDLStruct epdlStruct;         /* EPDL structure */
  PoolDebugMixinStruct debug;    /* debug mixin */
} EPDLDebugStruct;

typedef struct EPDLDebugStruct *EPDLDebug;


#define EPDLPoolEPDLDebug(epdl)   PARENT(EPDLDebugStruct, epdlStruct, epdl)
#define EPDLDebugPoolEPDL(epdld)  (&((epdld)->epdlStruct))


static Bool EPDLCheck(EPDL epdl);


/* EPDLBlockCheck -- check the consistency of a block structure */

static Bool EPDLBlockCheck(EPDLBlock block)
{
  CHECKL(block != NULL);
  CHECKL(block->size > 0);
  CHECKL(block->limit == AddrAdd(block->base, block->size));
  /* There are other possible checks, e.g. checking that the block is   */
  /* in a block pool, but they are all hard.                            */
  UNUSED(block); /* impl.c.mpm.check.unused */
  return TRUE;
}


/* EPDLNewBlock(&b,epdl,base,limit,size) makes b a new free block       */
/* (with contents base, limit, size) and increments epdl->length. If    */
/* the block allocation fails, it adds size to epdl->lost.              */

static Res EPDLNewBlock(EPDLBlock *blockReturn, EPDL epdl,
                        Addr base, Addr limit, Size size)
{
  Res res;
  Addr new;
  EPDLBlock block;

  AVERT(EPDL, epdl);
  AVER(AddrAdd(base, size) == limit);

  res = PoolAlloc(&new, epdl->blockPool, sizeof(EPDLBlockStruct),
                  /* withReservoirPermit */ FALSE);
  if (res != ResOK) {
    DEBUG_INC(blockAddFail);
    epdl->lost += size;
    return res;
  }

  ++ epdl->length;
  DEBUG_MAX(maxLength, epdl->length);
  DEBUG_INC(blockAdd);
  block = (EPDLBlock)new;
  block->base = base;
  block->limit = limit;
  block->size = size;
 
  AVERT(EPDLBlock, block);

  *blockReturn = block;
  return ResOK;
}


/* EPDLRemoveAreaFromFreeList(epdl,block,base,limit) removes the area   */
/* [base, limit) from block, which must be on the free list for epdl    */

/* It's called when one or more segments in the block have been returned*/
/* to the arena; the free list must be fixed up so that the memory in   */
/* those segments will not then be allocated (i.e. no longer appears    */
/* to belong to this pool).                                             */

static void EPDLRemoveAreaFromFreeList(EPDL epdl, EPDLBlock block, Addr base,
                                      Addr limit)
{
  Size size;
  EPDLBlock *blockPtr;

  AVERT(EPDL, epdl);
  AVERT(EPDLBlock, block);
  AVER(limit > base);
  AVER(base >= block->base);
  AVER(limit <= block->limit);

  size = AddrOffset(base, limit);
  epdl->free -= size;
  epdl->total -= size;

  /* find the pointer to this block, in case we have to remove or split it */
  blockPtr = &epdl->freeList;
  while(*blockPtr != block) {
    blockPtr = &((*blockPtr)->next);
    DEBUG_INC(segFreeBlockSkip);
  }

  if (base == block->base) {
    if (limit == block->limit) { /* remove whole block */
      DEBUG_INC(segFreeBlockPerfect);
      *blockPtr = block->next;
      PoolFree(epdl->blockPool, (Addr)block, sizeof(EPDLBlockStruct));
      DEBUG_INC(blockFree);
      -- epdl->length;
    } else { /* truncate head of block */
      DEBUG_INC(segFreeBlockHead);
      block->base = limit;
      block->size = AddrOffset(limit, block->limit);
    }
  } else {
    if (limit != block->limit) { /* split the block */
      EPDLBlock new;
      Res res = EPDLNewBlock(&new, epdl, limit, block->limit,
                             AddrOffset(limit, block->limit));
      DEBUG_INC(segFreeBlockSplit);
      if (res == ResOK) {
        new->next = block->next;
        block->next = new;
      }
    } else {
      DEBUG_INC(segFreeBlockTail);
    }
    /* truncate tail of block */
    block->limit = base;
    block->size = AddrOffset(block->base, base);
  }
}


/* EPDLBlockFreeSegs(epdl, block) frees any segments which lie wholly   */
/* within the block, and corrects the block accordingly (by calling     */
/* EPDLRemoveAreaFromFreeList()). When freeing an object, this          */
/* function is applied to the resulting block. The purpose is to        */
/* return unused segments to the arena.                                 */

static void EPDLBlockFreeSegs(EPDL epdl, EPDLBlock block)
{
  Seg seg;
  Arena arena;
  Bool b;
  Pool pool;
  Addr segLimit;  /* limit of the current segment when iterating */
  Addr freeBase;  /* base of the area returned to the arena */
  Addr freeLimit; /* end of the area returned to the arena */

  AVERT(EPDL, epdl);
  AVERT(EPDLBlock, block);

  pool = EPDLPool(epdl);
  arena = PoolArena(pool);
  b = SegOfAddr(&seg, arena, block->base);
  AVER(b);

  segLimit = SegLimit(seg);
  if(segLimit > block->limit) /* then there are no segs to free */
    return;

  freeLimit = (Addr)0;  /* avoid compiler warning */

  freeBase = SegBase(seg);
  if(freeBase < block->base) { /* then we can't free this first seg */
    Bool bb;
    if(segLimit == block->limit) /* no segs to free */
      return;
    bb = SegNext(&seg, arena, freeBase); /* move on to the next seg */
    /* Can't have been the last segment, because the block */
    /* overlaps the end, so there must be another segment */
    AVER(bb);
    segLimit = SegLimit(seg);
    freeBase = SegBase(seg);
  }

  AVER(freeBase >= block->base);
  AVER(SegPool(seg) == pool);

  if (block->limit >= segLimit) { /* then we have some segs to free */
    DEBUG_INC(segsFree);
    while(block->limit > segLimit) { /* free a seg and move on */
      Addr base;
      Seg segNext;
      Bool bb;

      base = SegBase(seg);
      bb = SegNext(&segNext, arena, base);
      /* Can't have been the last segment, because the block */
      /* overlaps the end, so there must be another segment */
      AVER(bb);
      AVER(SegPool(segNext) == pool);
      DEBUG_INC(segFree);
      DEBUG_ADD(bytesReturned, AddrOffset(SegBase(seg), segLimit));
      SegFree(seg);
      freeLimit = segLimit;
      seg = segNext;
      segLimit = SegLimit(seg);
    }
    if (block->limit == segLimit) { /* then free the last seg */
      DEBUG_INC(segFree);
      DEBUG_ADD(bytesReturned, AddrOffset(SegBase(seg), segLimit));
      SegFree(seg);
      freeLimit = segLimit;
    }
    
    EPDLRemoveAreaFromFreeList(epdl, block, freeBase, freeLimit);
  }
}


/* EPDLAddToFreeList(&p,epdl,base,limit) adds the memory [base, limit)  */
/* to epdl's free list, and returns in p a pointer to the block of      */
/* which this memory now forms a part. It can fail if it is unable to   */
/* allocate a new block descriptor. This is done when freeing an        */
/* object or when adding a new segment to the pool.                     */

static Res EPDLAddToFreeList(EPDLBlock *blockReturn, EPDL epdl,
                             Addr base, Addr limit)
{
  Res res;
  EPDLBlock prev, next, block;
  Size size;

  AVERT(EPDL, epdl);
  AVER(base <= limit);
  AVER(blockReturn != NULL);

  size = AddrOffset(base, limit);

  next = epdl->freeList;
  
  if ((next != NULL) && (next->limit <= base)) {
    /* this is not the first block */
    do { /* find blocks immediately before and after */
      DEBUG_INC(freeBlockExamine);
      prev = next;
      next = prev->next;
    } while((next != NULL) && (next->limit <= base));
    
    AVER(base >= prev->limit);
    AVER((next == NULL) || (limit <= next->base));
    
    if ((next != NULL) && (limit == next->base)) { /* merge with next */
      next->base = base;
      next->size += size;
      block = next;
      if (next->base == prev->limit) { /* merge with both */
        DEBUG_INC(mergeBoth);
        prev->limit = next->limit;
        prev->size += next->size;
        prev->next = next->next;
        block = prev;
        PoolFree(epdl->blockPool, (Addr)next, sizeof(EPDLBlockStruct));
        DEBUG_INC(blockFree);
        -- epdl->length;
      } else {
        DEBUG_INC(mergeAbove);
      }
    } else if (base == prev->limit) { /* merge with previous */
      DEBUG_INC(mergeBelow);
      prev->size += size;
      prev->limit = limit;
      block = prev;
    } else { /* no merges; independent block */
      res = EPDLNewBlock(&block, epdl, base, limit, size);
      if (res != ResOK)
        return res;
      block->next = next;
      prev->next = block;
    }
  } else { /* this comes before the first free block */
    AVER((next == NULL) || (limit <= next->base));
    if ((next != NULL) && (limit == next->base)) {
      DEBUG_INC(mergeAbove);
      next->base = base;
      next->size += size;
      block = next;
    } else { /* no merge; independent block at head of list */
      res = EPDLNewBlock(&block, epdl, base, limit, size);
      if (res != ResOK)
        return res;
      epdl->freeList = block;
      block->next = next;
    }
  }
  epdl->free += size;
  DEBUG_MAX(maxFree,epdl->free);
  *blockReturn = block;
  return ResOK;
}


/* EPDLAddSeg(epdl, size, withReservoirPermit) gets a new segment from */
/* the arena, large enough for an object of size 'size', and adds it */
/* to the pool by calling EPDLAddToFreeList(). */

static Res EPDLAddSeg(EPDL epdl, Size size, Bool withReservoirPermit)
{
  Pool pool;
  Arena arena;
  Size segSize;
  Seg seg;
  Res res;
  EPDLBlock block;
  Align align;

  AVERT(EPDL, epdl);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  pool = EPDLPool(epdl);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);

  AVER(SizeIsAligned(size, pool->alignment));

  /* use extendBy unless it's too small.
   * see design.mps.poolepdl.design.segSize */
  if(size <= epdl->extendBy)
    segSize = epdl->extendBy;
  else
    segSize = size;

  segSize = SizeAlignUp(segSize, align);

  res = SegAlloc(&seg, SegClassGet(), epdl->segPref, segSize, pool, 
                 withReservoirPermit);
  if(res != ResOK) {
    DEBUG_INC(segAddFail);
    /* try again for a seg just large enough for object */
    /* see design.mps.poolepdl.design.segFail */
    segSize = SizeAlignUp(size, align);
    res = SegAlloc(&seg, SegClassGet(), epdl->segPref, segSize, pool, 
                   withReservoirPermit);
    if (res != ResOK) {
      DEBUG_INC(segAddFailTwo);
      return res;
    }
  }
  DEBUG_INC(segAdd);
  DEBUG_ADD(bytesAdded, segSize);
  epdl->total += segSize;
  DEBUG_MAX(maxTotal, epdl->total);

  res = EPDLAddToFreeList(&block,
                          epdl, SegBase(seg), SegLimit(seg));
  /* discard 'block' variable */
  return res;
}


/* Finds the first free block on the free list that is big enough */
/* to accommodate the request.  The free list is extended by */
/* adding a segment if necessary. */
/* Only fails if attempt to extend free list with new segment fails. */
/* A _pointer_ to the block is returned.  This pointer can be used */
/* to modify the free list if the block is removed (which it will be */
/* if it's a perfect fit). */

static Res FindFirstFree(EPDLBlock **blockPtrReturn,
                         EPDL epdl, Size size,
                         Bool withReservoirPermit)
{
  EPDLBlock *blockPtr;
  EPDLBlock block;
  Res res;

  AVER(blockPtrReturn != NULL);
  /* other parms checked by callers */

  do {
    if(size <= epdl->free) {
      DEBUG_INC(allocListExamine);
      block = epdl->freeList;
      blockPtr = &epdl->freeList;
      while(block) { /* look for a fit */
        DEBUG_INC(allocBlockExamine);
        if(size <= block->size) { /* fit */
          *blockPtrReturn = blockPtr;
          return ResOK;
        }
        blockPtr = &block->next;
        block = *blockPtr;
      }
    }
    /* no fit found; make a new segment and try again */
    res = EPDLAddSeg(epdl, size, withReservoirPermit);
  } while(res == ResOK); /* should only go round this loop at most */
                         /* twice, as seg should be large enough */
                         /* (but this isn't checked for). */
  return res;
}


/*  == Allocate ==  */

static Res EPDLAlloc(Addr *aReturn, Pool pool, Size size,
                     Bool withReservoirPermit)
{
  Res res;
  EPDL epdl;
  Addr new;
  EPDLBlock block;
  EPDLBlock *blockPtr;

  AVERT(Pool, pool);
  epdl = PoolPoolEPDL(pool);
  AVERT(EPDL, epdl);

  AVER(aReturn != NULL);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  size = SizeAlignUp(size, pool->alignment);

  res = FindFirstFree(&blockPtr, epdl, size, withReservoirPermit);
  if(res != ResOK) {
    DEBUG_INC(allocsFailed);
    return res;
  }
  /* fit */
  block = *blockPtr;
  AVER(block != NULL);
  AVER(size <= block->size);

  if(size == block->size) {
    /* perfect fit; remove the block */
    DEBUG_INC(perfect);
    *blockPtr = block->next;
    PoolFree(epdl->blockPool, (Addr)block,
             sizeof(EPDLBlockStruct));
    DEBUG_INC(blockFree);
    --epdl->length;
    new = block->base;
  } else {
    /* imperfect fit; splinter block */
    /* allocate from left hand end of block */
    new = block->base;
    block->base = AddrAdd(block->base, size);
    block->size -= size;
  }

  AVER(AddrIsAligned(new, pool->alignment));
  *aReturn = new;
  epdl->free -= size;
  DEBUG_INC(allocs);
  DEBUG_ADD(bytesAlloc,size);

  return ResOK;
}

static Res EPDRAlloc(Addr *aReturn, Pool pool, Size size,
                     Bool withReservoirPermit)
{
  Res res;
  EPDL epdl;
  Addr new;
  EPDLBlock block;
  EPDLBlock *blockPtr;

  AVERT(Pool, pool);
  epdl = PoolPoolEPDL(pool);
  AVERT(EPDL, epdl);

  AVER(aReturn != NULL);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  size = SizeAlignUp(size, pool->alignment);

  res = FindFirstFree(&blockPtr, epdl, size, withReservoirPermit);
  if(res != ResOK) {
    DEBUG_INC(allocsFailed);
    return res;
  }
  /* fit */
  block = *blockPtr;
  AVER(block != NULL);
  AVER(size <= block->size);

  if(size == block->size) {
    /* perfect fit; remove the block */
    DEBUG_INC(perfect);
    *blockPtr = block->next;
    PoolFree(epdl->blockPool, (Addr)block,
             sizeof(EPDLBlockStruct));
    DEBUG_INC(blockFree);
    --epdl->length;
    new = block->base;
  } else {
    /* imperfect fit; splinter block */
    /* allocate from right hand end of block */
    /* reduce block by size then point new at end of block */
    block->size -= size;
    block->limit = AddrAdd(block->base, block->size);
    new = block->limit;
  }

  AVER(AddrIsAligned(new, pool->alignment));
  *aReturn = new;
  epdl->free -= size;
  DEBUG_INC(allocs);
  DEBUG_ADD(bytesAlloc,size);

  return ResOK;
}


/* EPDLFree -- free the given block */

static void EPDLFree(Pool pool, Addr old, Size size)
{
  Addr base, limit;
  EPDL epdl;
  EPDLBlock block;
  Res res;

  AVERT(Pool, pool);
  epdl = PoolPoolEPDL(pool);
  AVERT(EPDL, epdl);

  AVER(old != (Addr)0);
  AVER(AddrIsAligned(old, pool->alignment));
  AVER(size > 0);

  size = SizeAlignUp(size, pool->alignment);
  base = old;
  limit = AddrAdd(base, size);

  DEBUG_INC(frees);
  DEBUG_ADD(bytesFree, size);

  res = EPDLAddToFreeList(&block, epdl, base, limit);
  if (res != ResOK)
    return;
  EPDLBlockFreeSegs(epdl, block);
 /* Free segments eagerly. See design.mps.poolepdl.issue.eager */
}


/* == Initialize == */

static Res EPDLInitComm(Pool pool, SegPrefKind segPrefKind, va_list arg)
{
  Size extendBy, avgSize, align, blockExtendBy;
  EPDL epdl;
  Arena arena;
  Res res;
  void *p;
  ZoneSet zones;

  AVERT(Pool, pool);

  /* .arg: class-specific additional arguments; see design.mps.poolepdl.arg */
  /* .arg.check: we do the same three checks here and in EPDLCheck */ 
  extendBy = va_arg(arg, Size);
  avgSize = va_arg(arg, Size);
  align = va_arg(arg, Size);
  AVER(extendBy > 0);           /* .arg.check */
  AVER(avgSize > 0);            /* .arg.check */
  AVER(avgSize <= extendBy);    /* .arg.check */

  epdl = PoolPoolEPDL(pool);
  arena = PoolArena(pool);

  epdl->extendBy = extendBy;
  epdl->avgSize = avgSize;
  pool->alignment = align;

  /* this number is a temporary hack; */
  /* see design.mps.poolepdl.design.blockExtend */
  blockExtendBy = sizeof(EPDLBlockStruct) * 64;

  res = PoolCreate(&epdl->blockPool, 
                   arena, PoolClassMFS(), 
                   blockExtendBy, sizeof(EPDLBlockStruct));
  if(res != ResOK)
    return res;

  res = ControlAlloc(&p, arena, sizeof(SegPrefStruct), 
                     /* withReservoirPermit */ FALSE);
  if (res != ResOK) {
    PoolDestroy(epdl->blockPool);
    return res;
  }
  
  epdl->segPref = (SegPref)p;
  *epdl->segPref = *SegPrefDefault();
  SegPrefExpress(epdl->segPref, segPrefKind, NULL);
  /* If using zone placement, just put it apart from the others. */
  zones = ZoneSetComp(ArenaDefaultZONESET);
  SegPrefExpress(epdl->segPref, SegPrefZoneSet, (void *)&zones);

  epdl->total = 0;
  epdl->free = 0;
  epdl->lost = 0;
  epdl->length = 0;
  epdl->freeList = NULL;

  DEBUG_CLEAR(maxLength);
  DEBUG_CLEAR(maxFree);
  DEBUG_CLEAR(maxTotal);
  DEBUG_CLEAR(allocs);
  DEBUG_CLEAR(frees);
  DEBUG_CLEAR(allocsFailed);
  DEBUG_CLEAR(bytesAlloc);
  DEBUG_CLEAR(bytesFree);
  DEBUG_CLEAR(segAdd);
  DEBUG_CLEAR(segAddFail);
  DEBUG_CLEAR(segAddFailTwo);
  DEBUG_CLEAR(segFree);
  DEBUG_CLEAR(segsFree);
  DEBUG_CLEAR(segFreeBlockSkip);
  DEBUG_CLEAR(segFreeBlockPerfect);
  DEBUG_CLEAR(segFreeBlockHead);
  DEBUG_CLEAR(segFreeBlockTail);
  DEBUG_CLEAR(segFreeBlockSplit);
  DEBUG_CLEAR(bytesAdded);
  DEBUG_CLEAR(bytesReturned);
  DEBUG_CLEAR(blockAdd);
  DEBUG_CLEAR(blockAddFail);
  DEBUG_CLEAR(blockFree);
  DEBUG_CLEAR(allocListExamine);
  DEBUG_CLEAR(allocBlockExamine);
  DEBUG_CLEAR(perfect);
  DEBUG_CLEAR(freeBlockExamine);
  DEBUG_CLEAR(mergeBelow);
  DEBUG_CLEAR(mergeAbove);
  DEBUG_CLEAR(mergeBoth);

  epdl->sig = EPDLSig;
  AVERT(EPDL, epdl);
  EVENT_PPUWWW(PoolInitEPDL, pool, arena, (segPrefKind == SegPrefLow),
               extendBy, avgSize, align);
  return ResOK;
}


static Res EPDLInit(Pool pool, va_list arg)
{
  /* EPDL is always low in memory */
  return EPDLInitComm(pool, SegPrefLow, arg);
}

static Res EPDRInit(Pool pool, va_list arg)
{
  /* EPDR is always high in memory */
  return EPDLInitComm(pool, SegPrefHigh, arg);
}


static void EPDLFinish(Pool pool)
{
  EPDL epdl;
  Arena arena;
  Ring ring, node, nextNode;
  Seg seg;

  AVERT(Pool, pool);
  epdl = PoolPoolEPDL(pool);
  AVERT(EPDL, epdl);

  arena = PoolArena(pool);

  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    seg = SegOfPoolRing(node);
    AVER(SegPool(seg) == pool);
    SegFree(seg);
  }

  ControlFree(arena, epdl->segPref, sizeof(SegPrefStruct));
  PoolDestroy(epdl->blockPool);
  epdl->sig = SigInvalid;
}


/* EPDLDebugMixin - find debug mixin in class EPDLDebug */

static PoolDebugMixin EPDLDebugMixin(Pool pool)
{
  EPDL epdl;

  AVERT(Pool, pool);
  epdl = PoolPoolEPDL(pool);
  AVERT(EPDL, epdl);
  /* Can't check EPDLDebug, because this is called during init */
  return &(EPDLPoolEPDLDebug(epdl)->debug);
}


static Res EPDLDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  EPDL epdl;

  AVERT(Pool, pool);
  epdl = PoolPoolEPDL(pool);
  AVERT(EPDL, epdl);
  AVER(stream != NULL);

  res = WriteF(stream,
               "  blockPool $P ($U)\n",
               (WriteFP)epdl->blockPool, (WriteFU)epdl->blockPool->serial,
               "  extendBy  $W\n",  (WriteFW)epdl->extendBy,
               "  avgSize   $W\n",  (WriteFW)epdl->avgSize,
               "  total     $U\n",  (WriteFU)epdl->total,
               "  free      $U\n",  (WriteFU)epdl->free,
               "  lost      $U\n",  (WriteFU)epdl->lost,
               "  length    $U\n",  (WriteFU)epdl->length,
               DEBUG_WRITE(maxLength)
               DEBUG_WRITE(maxFree)
               DEBUG_WRITE(maxTotal)
               DEBUG_WRITE(allocs)
               DEBUG_WRITE(frees)
               DEBUG_WRITE(allocsFailed)
               DEBUG_WRITE(bytesAlloc)
               DEBUG_WRITE(bytesFree)
               DEBUG_WRITE(segAdd)
               DEBUG_WRITE(segAddFail)
               DEBUG_WRITE(segAddFailTwo)
               DEBUG_WRITE(segFree)
               DEBUG_WRITE(segsFree)
               DEBUG_WRITE_DOUBLE(segFreeBlockSkip)
               DEBUG_WRITE(segFreeBlockPerfect)
               DEBUG_WRITE(segFreeBlockHead)
               DEBUG_WRITE(segFreeBlockTail)
               DEBUG_WRITE(segFreeBlockSplit)
               DEBUG_WRITE(bytesAdded)
               DEBUG_WRITE(bytesReturned)
               DEBUG_WRITE(blockAdd)
               DEBUG_WRITE(blockAddFail)
               DEBUG_WRITE(blockFree)
               DEBUG_WRITE(allocListExamine)
               DEBUG_WRITE_DOUBLE(allocBlockExamine)
               DEBUG_WRITE(perfect)
               DEBUG_WRITE_DOUBLE(freeBlockExamine)
               DEBUG_WRITE(mergeBelow)
               DEBUG_WRITE(mergeAbove)
               DEBUG_WRITE(mergeBoth)
               NULL);
  return res;               
}



DEFINE_POOL_CLASS(EPDLPoolClass, this)
{
  INHERIT_CLASS(this, AbstractAllocFreePoolClass);
  this->name = "EPDL";
  this->size = sizeof(EPDLStruct);
  this->offset = offsetof(EPDLStruct, poolStruct);
  this->init = EPDLInit;
  this->finish = EPDLFinish;
  this->alloc = EPDLAlloc;
  this->free = EPDLFree;
  this->describe = EPDLDescribe;
}

DEFINE_POOL_CLASS(EPDRPoolClass, this)
{
  INHERIT_CLASS(this, EPDLPoolClass);
  this->name = "EPDR";
  this->init = EPDRInit;
  this->alloc = EPDRAlloc;
}


PoolClass PoolClassEPDL(void)
{
  return EnsureEPDLPoolClass();
}

PoolClass PoolClassEPDR(void)
{
  return EnsureEPDRPoolClass();
}


DEFINE_POOL_CLASS(EPDLDebugPoolClass, this)
{
  INHERIT_CLASS(this, EPDLPoolClass);
  PoolClassMixInDebug(this);
  this->name = "EPDLDBG";
  this->size = sizeof(EPDLDebugStruct);
  this->debugMixin = EPDLDebugMixin;
}

DEFINE_POOL_CLASS(EPDRDebugPoolClass, this)
{
  INHERIT_CLASS(this, EPDRPoolClass);
  PoolClassMixInDebug(this);
  this->name = "EPDRDBG";
  this->size = sizeof(EPDLDebugStruct);
  this->debugMixin = EPDLDebugMixin;
}



/* MPS Interface Extensions. */

mps_class_t mps_class_epdl(void)
{
  return (mps_class_t)(EnsureEPDLPoolClass());
}

mps_class_t mps_class_epdr(void)
{
  return (mps_class_t)(EnsureEPDRPoolClass());
}

mps_class_t mps_class_epdl_debug(void)
{
  return (mps_class_t)(EnsureEPDLDebugPoolClass());
}

mps_class_t mps_class_epdr_debug(void)
{
  return (mps_class_t)(EnsureEPDRDebugPoolClass());
}


/* Total free bytes. See design.mps.poolepdl.issue.space-enter */

size_t mps_epdl_free_size(mps_pool_t mps_pool)
{
  Pool pool;
  EPDL epdl;

  pool = (Pool)mps_pool;
  AVERT(Pool, pool);
  epdl = PoolPoolEPDL(pool);
  AVERT(EPDL, epdl);
  
  return (size_t)epdl->free;
}


/* Total owned bytes. See design.mps.poolepdl.issue.space-enter */

size_t mps_epdl_size(mps_pool_t mps_pool)
{
  Pool pool;
  EPDL epdl;

  pool = (Pool)mps_pool;
  AVERT(Pool, pool);
  epdl = PoolPoolEPDL(pool);
  AVERT(EPDL, epdl);

  return (size_t)epdl->total;
} 


/* EPDLCheck -- check the consistency of an EPDL structure */

static Bool EPDLCheck(EPDL epdl)
{
  CHECKS(EPDL, epdl);
  CHECKD(Pool, EPDLPool(epdl));
  CHECKL(IsSubclassPoly(EPDLPool(epdl)->class, EnsureEPDLPoolClass()));
  CHECKD(Pool, epdl->blockPool);
  CHECKD(SegPref, epdl->segPref);
  CHECKL(epdl->extendBy > 0);                   /* see .arg.check */
  CHECKL(epdl->avgSize > 0);                    /* see .arg.check */
  CHECKL(epdl->avgSize <= epdl->extendBy);      /* see .arg.check */
  /* the free and lost bytes are disjoint subsets of the total */
  CHECKL(epdl->total >= epdl->free + epdl->lost);
  CHECKL(SizeIsAligned(epdl->free, EPDLPool(epdl)->alignment)); 
  CHECKL(SizeIsAligned(epdl->lost, EPDLPool(epdl)->alignment));
  CHECKL(SizeIsAligned(epdl->total, ArenaAlign(PoolArena(EPDLPool(epdl)))));
  /* free list is empty if and only if it has zero length */
  CHECKL((epdl->length == 0) == (epdl->freeList == NULL));
  /* free list is empty if and only if there is no free space */
  CHECKL((epdl->length == 0) == (epdl->free == 0));
  /* everything on the free list is at least 'alignment' in size */
  CHECKL(epdl->length * EPDLPool(epdl)->alignment <= epdl->free);
  /* check the first thing on the free list */
  CHECKL(epdl->freeList == NULL || EPDLBlockCheck(epdl->freeList));
  return TRUE;
}
