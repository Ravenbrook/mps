/* impl.c.reserv: ARENA RESERVOIR
 *
 * $HopeName: MMsrc!reserv.c(MMdevel_tony_segments.1) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 *
 * .readership: Any MPS developer
 *
 */


#include "mpm.h"

SRCID(reserv, "$HopeName: MMsrc!reserv.c(MMdevel_tony_segments.1) $");


/* The reservoir pool is defined here. See design.mps.reservoir */

#define PoolPoolReservoir(pool) PARENT(ReservoirStruct, poolStruct, pool)

/* ResPoolInit -- Reservoir pool init method */
static Res ResPoolInit(Pool pool, va_list arg)
{
  Reservoir reservoir;

  UNUSED(arg);
  AVER(pool != NULL);
  reservoir = PoolPoolReservoir(pool);
  reservoir->sig = ReservoirSig;
  AVERT(Reservoir, reservoir);

  return ResOK;
}

/* ResPoolFinish -- Reservoir pool finish method 
 *
 * .reservoir.finish: This might be called from ArenaFinish, so the 
 * arena cannot be checked at this time. In order to avoid the 
 * check, insist that the reservoir is empty, by AVERing that
 * the seg ring is empty.
 */
static void ResPoolFinish(Pool pool)
{
  Reservoir reservoir;

  AVERT(Pool, pool);
  reservoir = PoolPoolReservoir(pool);
  AVERT(Reservoir, reservoir);
  AVER(RingCheckSingle(PoolSegRing(pool)));  /* .reservoir.finish */

  reservoir->sig = SigInvalid;
}


DEFINE_POOL_CLASS(ReservoirPoolClass, this)
{
  INHERIT_CLASS(this, AbstractPoolClass);
  this->name = "Reservoir";
  this->size = sizeof(ReservoirStruct);
  this->offset = offsetof(ReservoirStruct, poolStruct);
  this->init = ResPoolInit;
  this->finish = ResPoolFinish;
}


static Arena ReservoirArena (Reservoir reservoir)
{
  return reservoir->poolStruct.arena;
}


Bool ReservoirCheck(Reservoir reservoir)
{
  ReservoirPoolClass reservoircl = EnsureReservoirPoolClass();
  Arena arena;
  CHECKS(Reservoir, reservoir);
  CHECKD(Pool, &reservoir->poolStruct);
  CHECKL(reservoir->poolStruct.class == reservoircl);
  arena = ReservoirArena(reservoir);
  CHECKS(Arena, arena);  /* Can't use CHECKD; circularly referenced */
  /* could call ReservoirIsConsistent, but it's costly. */
  CHECKL(SizeIsAligned(reservoir->reservoirLimit, ArenaAlign(arena)));
  CHECKL(SizeIsAligned(reservoir->reservoirSize, ArenaAlign(arena)));

  return TRUE;
}


/* ReservoirIsConsistent
 *
 * Returns FALSE if the reservoir is corrupt.
 */

static Bool ReservoirIsConsistent(Reservoir reservoir)
{
  Bool res;
  Size size = 0;
  Ring node, nextNode;
  Pool pool;
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = ReservoirArena(reservoir);
  AVERT(Arena, arena);
  pool = &reservoir->poolStruct;
  AVERT(Pool, pool);

  /* Check that the size of the segments matches reservoirSize */
  RING_FOR(node, PoolSegRing(pool), nextNode) {
    Seg seg = SegOfPoolRing(node);
    Size segSize = SegSize(seg);
    AVER(segSize == ArenaAlign(arena));
    size += segSize;
  }
  if (size != reservoir->reservoirSize)
    return FALSE;

  /* design.mps.reservoir.align */
  res = SizeIsAligned(reservoir->reservoirLimit, arena->alignment) &&
        SizeIsAligned(reservoir->reservoirSize, arena->alignment) &&
        (reservoir->reservoirLimit >= reservoir->reservoirSize);

  return res;
}


/* ReservoirEnsureFull  
 * 
 * Ensures that the reservoir is the right size, by topping it up 
 * if possible.
 */

Res ReservoirEnsureFull(Reservoir reservoir)
{
  Size limit, alignment;
  Pool pool;
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = ReservoirArena(reservoir);

  AVERT(Arena, arena);
  alignment = arena->alignment;
  limit = reservoir->reservoirLimit;

  /* optimize the common case of a full reservoir */
  if (reservoir->reservoirSize == limit)
    return ResOK; 

  pool = &reservoir->poolStruct;
  AVERT(Pool, pool);

  while (reservoir->reservoirSize < limit) {
    Res res;
    Seg seg;
    res = (*arena->class->segAlloc)(&seg, SegPrefDefault(),   /* @@@@@@@@@ */
                                    alignment, pool);
    if (res != ResOK) {
      AVER(ReservoirIsConsistent(reservoir));
      return res;
    }
    AVER(SegSize(seg) == alignment);
    reservoir->reservoirSize += alignment;
  }
  AVER(ReservoirIsConsistent(reservoir));
  return ResOK;
}


static Seg ReservoirFirstSeg(Reservoir reservoir)
{
  Ring ring, node;
  Pool pool;
  Seg seg;
  
  AVERT(Reservoir, reservoir);
  pool = &reservoir->poolStruct;
  AVERT(Pool, pool);

  ring = PoolSegRing(pool);
  node = RingNext(ring);
  AVER(node != ring);  /* check there is at least 1 segment */
  seg = SegOfPoolRing(node);
  return seg;
}


static void ReservoirShrink(Reservoir reservoir, Size want)
{
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = ReservoirArena(reservoir);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(want, arena->alignment));
  AVER(reservoir->reservoirSize >= want);

  if (reservoir->reservoirSize == want)
    return;

  /* Iterate over reservoir segs, freeing them while reservoir is too big */
  while (reservoir->reservoirSize > want) {
    Seg seg = ReservoirFirstSeg(reservoir);
    Size size = SegSize(seg);
    (*arena->class->segFree)(seg);
    reservoir->reservoirSize -= size;
  }
  AVER(reservoir->reservoirSize <= want);
  AVER(ReservoirIsConsistent(reservoir));
}


Res ReservoirWithdraw(Seg *segReturn, Reservoir reservoir,
                      Size size, Pool pool)
{
  Ring ring;
  Ring node, nextNode;
  Pool respool;
  Arena arena;
  
  AVER(segReturn != NULL);
  AVERT(Reservoir, reservoir);
  arena = ReservoirArena(reservoir);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(size, arena->alignment));
  AVERT(Pool, pool);
  respool = &reservoir->poolStruct;
  AVERT(Pool, respool);

  /* @@@ As a short-term measure, we only permit the reservoir to */
  /* hold or allocate single-page segments. */
  /* See change.dylan.jackdaw.160125 */
  if(size != ArenaAlign(arena))
    return ResMEMORY;

  /* Return the first segment which is big enough */
  ring = PoolSegRing(respool);
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    Size segSize = SegSize(seg);
    if (segSize == size) {
      reservoir->reservoirSize -= segSize;
      SegFinish(seg);
      SegInit(seg, pool);
      AVER(ReservoirIsConsistent(reservoir));
      *segReturn = seg;
      return ResOK;
    }
  }
  AVER(ReservoirIsConsistent(reservoir));  
  return ResMEMORY; /* no suitable segment in the reservoir */
}


void ReservoirDeposit(Reservoir reservoir, Seg seg)
{
  Pool respool;
  Size have, limit, new;
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = ReservoirArena(reservoir);
  AVERT(Arena, arena);
  respool = &reservoir->poolStruct;
  AVERT(Pool, respool);

  have = reservoir->reservoirSize;
  limit = reservoir->reservoirLimit;
  new = SegSize(seg);
  AVER(have < limit); /* The reservoir mustn't be full */

  /* @@@ Short-term fix that multi-page segments aren't put */
  /* directly into the reservoir.  See change.dylan.jackdaw.160125 */
  if(new != ArenaAlign(arena)) {
    (*arena->class->segFree)(seg);
    (void)ReservoirEnsureFull(reservoir);
  } else {
    /* Reassign the segment to the reservoir pool */
    SegFinish(seg);
    SegInit(seg, respool);
    reservoir->reservoirSize += new; 
  }
  AVER(ReservoirIsConsistent(reservoir));
}


static Count MutatorBufferCount(Arena arena)
{
  Ring nodep, nextp;
  Count count = 0;

  AVERT(Arena, arena);
  
  /* Iterate over all pools, and count the mutator buffers in each */
  RING_FOR(nodep, &arena->poolRing, nextp) {
    Pool pool = RING_ELT(Pool, arenaRing, nodep);
    Ring nodeb, nextb;
    RING_FOR(nodeb, &pool->bufferRing, nextb) {
      Buffer buff = RING_ELT(Buffer, poolRing, nodeb);
      if (buff->isMutator)
        count++;
    }
  }
  return count;
}


void ReservoirSetLimit(Reservoir reservoir, Size size)
{
  Size needed;
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = ReservoirArena(reservoir);
  AVERT(Arena, arena);

  if (size > 0) {
    Size wastage;
    /* design.mps.reservoir.wastage */
    wastage = ArenaAlign(arena) * MutatorBufferCount(arena);
    /* design.mps.reservoir.align */
    needed = SizeAlignUp(size, ArenaAlign(arena)) + wastage;
  } else {
    needed = 0; /* design.mps.reservoir.really-empty */
  }

  AVER(SizeIsAligned(needed, arena->alignment));

  if (needed > reservoir->reservoirSize) {
    /* Try to grow the reservoir */
    reservoir->reservoirLimit = needed;
    ReservoirEnsureFull(reservoir);
  } else {
    /* Shrink the reservoir */
    ReservoirShrink(reservoir, needed);
    reservoir->reservoirLimit = needed;
    AVER(ReservoirIsConsistent(reservoir));  
  }
}

Size ReservoirLimit(Reservoir reservoir)
{
  AVERT(Reservoir, reservoir);
  AVER(ReservoirIsConsistent(reservoir));  
  return reservoir->reservoirLimit;
}

Size ReservoirAvailable(Reservoir reservoir)
{
  AVERT(Reservoir, reservoir);
  ReservoirEnsureFull(reservoir);
  return reservoir->reservoirSize;
}


Res ReservoirInit(Reservoir reservoir, Arena arena)
{
  Res res;
  reservoir->reservoirLimit = (Size)0;
  reservoir->reservoirSize = (Size)0;
  /* initialize the reservoir pool, design.mps.reservoir */
  res = PoolInit(&reservoir->poolStruct, 
                 arena, EnsureReservoirPoolClass());
  if (res != ResOK) {
    PoolFinish(&reservoir->poolStruct);
  }
  return res;
}

void ReservoirFinish (Reservoir reservoir)
{
  PoolFinish(&reservoir->poolStruct);
}

