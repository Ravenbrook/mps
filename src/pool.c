/* impl.c.pool: POOL IMPLEMENTATION
 *
 * $HopeName: MMsrc!pool.c(MMdevel_assertid.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This is the implementation of the generic pool interface.  The
 * functions here dispatch to pool-specific methods.
 *
 * See impl.h.mpmst for definition of Pool.  
 * See design.mps.pool for design. 
 */

#include "mpm.h"

SRCID(pool, "$HopeName: MMsrc!pool.c(MMdevel_assertid.1) $");


Bool PoolClassCheck(PoolClass class)
{
  CHECKS(0xA55E62, PoolClass, class);
  CHECKL(0xA55E62, class->name != NULL); /* Should be <=6 char C identifier */
  CHECKL(0xA55E62, class->size >= sizeof(PoolStruct));
  /* Offset of generic Pool within class-specific instance cannot be */
  /* greater than the size of the class-specific portion of the instance */
  CHECKL(0xA55E62, class->offset <= (size_t)(class->size - sizeof(PoolStruct)));
  CHECKL(0xA55E62, AttrCheck(class->attr));
  CHECKL(0xA55E62, FUNCHECK(class->init));
  CHECKL(0xA55E62, FUNCHECK(class->finish));
  CHECKL(0xA55E62, FUNCHECK(class->alloc));
  CHECKL(0xA55E62, FUNCHECK(class->free));
  CHECKL(0xA55E62, FUNCHECK(class->bufferInit));
  CHECKL(0xA55E62, FUNCHECK(class->bufferFill));
  CHECKL(0xA55E62, FUNCHECK(class->bufferEmpty));
  CHECKL(0xA55E62, FUNCHECK(class->bufferFinish));
  CHECKL(0xA55E62, FUNCHECK(class->condemn));
  CHECKL(0xA55E62, FUNCHECK(class->grey));
  CHECKL(0xA55E62, FUNCHECK(class->scan));
  CHECKL(0xA55E62, FUNCHECK(class->fix));
  CHECKL(0xA55E62, FUNCHECK(class->reclaim));
  CHECKL(0xA55E62, FUNCHECK(class->describe));
  CHECKL(0xA55E62, class->endSig == PoolClassSig);
  return TRUE;
}

Bool PoolCheck(Pool pool)
{
  CHECKS(0xA55E62, Pool, pool);
  CHECKU(0xA55E62, Space, pool->space);
  /* Break modularity for checking efficiency */
  CHECKL(0xA55E62, pool->serial < pool->space->poolSerial);
  CHECKD(0xA55E62, PoolClass, pool->class);
  CHECKL(0xA55E62, RingCheck(&pool->spaceRing));
  CHECKL(0xA55E62, RingCheck(&pool->bufferRing));
  CHECKL(0xA55E62, RingCheck(&pool->segRing));
  CHECKL(0xA55E62, RingCheck(&pool->actionRing));
  /* Cannot check pool->bufferSerial */
  CHECKL(0xA55E62, AlignCheck(pool->alignment));
  return TRUE;
}

/* PoolInit, PoolInitV -- initialize a pool
 *
 * Initialize the generic fields of the pool and calls class-specific init. 
 * See design.mps.pool.align
 */

Res PoolInit(Pool pool, Space space, PoolClass class, ...)
{
  Res res;
  va_list args;
  va_start(args, class);
  res = PoolInitV(pool, space, class, args);
  va_end(args);
  return res;
}

Res PoolInitV(Pool pool, Space space,
              PoolClass class, va_list args)
{
  Res res;

  AVER(0xA55E62, pool != NULL);
  AVERT(0xA55E62, Space, space);
  AVERT(0xA55E62, PoolClass, class);

  pool->class = class;
  pool->space = space;
  /* .ring.init: See .ring.finish */
  RingInit(&pool->spaceRing);
  RingInit(&pool->bufferRing);
  RingInit(&pool->segRing);
  RingInit(&pool->actionRing);
  pool->bufferSerial = (Serial)0;
  pool->actionSerial = (Serial)0;
  pool->alignment = MPS_PF_ALIGN;

  /* Initialise signature last; see design.mps.sig */
  pool->sig = PoolSig;
  pool->serial = space->poolSerial;
  ++(space->poolSerial);

  AVERT(0xA55E62, Pool, pool);

  /* Do class-specific initialization. */
  res = (*class->init)(pool, args);
  if(res != ResOK)
    goto failInit;

  /* Add initialized pool to list of pools in space. */
  RingAppend(SpacePoolRing(space), &pool->spaceRing);

  EVENT3(PoolInit, pool, space, class);

  return ResOK;

failInit:
  pool->sig = SigInvalid;      /* Leave space->poolSerial incremented */
  RingFinish(&pool->actionRing);
  RingFinish(&pool->segRing);
  RingFinish(&pool->bufferRing);
  RingRemove(&pool->spaceRing);
  RingFinish(&pool->spaceRing);
  return res;
}

/* PoolCreate, PoolCreateV: Allocate and initialise pool */

Res PoolCreate(Pool *poolReturn, Space space, 
               PoolClass class, ...)
{
  Res res;
  va_list args;
  va_start(args, class);
  res = PoolCreateV(poolReturn, space, class, args);
  va_end(args);
  return res;
}

Res PoolCreateV(Pool *poolReturn, Space space,  
                PoolClass class, va_list args)
{
  Res res;
  Pool pool;
  void *base;

  AVER(0xA55E62, poolReturn != NULL);
  AVERT(0xA55E62, Space, space);
  AVERT(0xA55E62, PoolClass, class);

  /* .space.alloc: Allocate the pool instance structure with the size */
  /* requested  in the pool class.  See .space.free */
  res = SpaceAlloc(&base, space, class->size); 
  if(res != ResOK)
      goto failSpaceAlloc;

  /* base is the address of the class-specific pool structure. */
  /* We calculate the address of the generic pool structure within the */
  /* instance by using the offset information from the class. */
  pool = (Pool)PointerAdd(base, class->offset);

  /* Initialize the pool. */  
  res = PoolInitV(pool, space, class, args);
  if(res != ResOK) 
    goto failPoolInit;
  
  *poolReturn = pool;  
  return ResOK;

failPoolInit:
  SpaceFree(space, base, class->size);
failSpaceAlloc:
  return res;
}

/* PoolFinish -- Finish pool including class-specific and generic fields. */

void PoolFinish(Pool pool)
{
  AVERT(0xA55E62, Pool, pool);  
  
  /* Do any class-specific finishing. */
  (*pool->class->finish)(pool);

  /* Detach the pool from the space, and unsig it. */
  RingRemove(&pool->spaceRing);
  pool->sig = SigInvalid;
  
  /* .ring.finish: Finish the generic fields.  See .ring.init */
  RingFinish(&pool->actionRing);
  RingFinish(&pool->segRing);
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->spaceRing);
  
  EVENT1(PoolFinish, pool);
}

/* PoolDestroy -- Finish and free pool. */

void PoolDestroy(Pool pool)
{
  PoolClass class;
  Space space;
  Addr base;

  AVERT(0xA55E62, Pool, pool);  
  
  class = pool->class; /* } In case PoolFinish changes these */
  space = pool->space; /* } */

  /* Finish the pool instance structure. */
  PoolFinish(pool);

  /* .space.free: Free the pool instance structure.  See .space.alloc */
  base = AddrSub((Addr)pool, (Size)(class->offset));
  SpaceFree(space, base, (Size)(class->size));
}

Res PoolAlloc(Addr *pReturn, Pool pool, Size size)
{
  Res res;

  AVER(0xA55E62, pReturn != NULL);
  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, (pool->class->attr & AttrALLOC) != 0);
  AVER(0xA55E62, size > 0);

  res = (*pool->class->alloc)(pReturn, pool, size);
  if(res != ResOK) return res;

  /* Make sure that the allocated address was in the pool's memory. */
  AVER(0xA55E62, PoolHasAddr(pool, *pReturn));

  EVENT3(PoolAlloc, (Word)pool, (Word)*pReturn, (Word)size);

  return ResOK;
}

void PoolFree(Pool pool, Addr old, Size size)
{
  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, (pool->class->attr & AttrFREE) != 0);
  AVER(0xA55E62, old != NULL);
  AVER(0xA55E62, PoolHasAddr(pool, old));
  AVER(0xA55E62, size > 0);
  (*pool->class->free)(pool, old, size);
  
  EVENT3(PoolFree, (Word)pool, (Word)old, (Word)size);
}

Res PoolCondemn(Pool pool, Trace trace, Seg seg)
{  
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);
  AVER(0xA55E62, pool->space == trace->space);
  AVER(0xA55E62, seg->pool == pool);
  return (*pool->class->condemn)(pool, trace, seg);
}

void PoolGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);
  AVER(0xA55E62, pool->space == trace->space);
  AVER(0xA55E62, seg->pool == pool);
  (*pool->class->grey)(pool, trace, seg);
}

Res PoolScan(ScanState ss, Pool pool, Seg seg)
{
  AVERT(0xA55E62, ScanState, ss);
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Seg, seg);
  AVER(0xA55E62, ss->space == pool->space);

  /* The segment must belong to the pool. */
  AVER(0xA55E62, pool == seg->pool);

  /* Should only scan for a rank for which there are references */
  /* in the segment. */
  AVER(0xA55E62, RankSetIsMember(seg->rankSet, ss->rank));

  /* Should only scan segments which contain grey objects. */
  AVER(0xA55E62, TraceSetInter(seg->grey, ss->traces) != TraceSetEMPTY);

  return (*pool->class->scan)(ss, pool, seg);
}

/* See impl.h.mpm for macro version; see design.mps.pool.req.fix */
Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, ScanState, ss);
  AVERT(0xA55E62, Seg, seg);
  AVER(0xA55E62, pool == seg->pool);
  AVER(0xA55E62, refIO != NULL);

  /* Should only be fixing references to white segments. */
  AVER(0xA55E62, TraceSetInter(seg->white, ss->traces) != TraceSetEMPTY);

  return PoolFix(pool, ss, seg, refIO);
}

void PoolReclaim(Pool pool, Trace trace, Seg seg)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);
  AVER(0xA55E62, pool->space == trace->space);
  AVER(0xA55E62, seg->pool == pool);

  /* There shouldn't be any grey things left for this trace. */
  AVER(0xA55E62, !TraceSetIsMember(seg->grey, trace->ti));

  /* Should only be reclaiming segments which are still white. */
  AVER(0xA55E62, TraceSetIsMember(seg->white, trace->ti));

  (*pool->class->reclaim)(pool, trace, seg);
}


Res PoolDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  Ring node;

  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, stream != NULL);
  
  res = WriteF(stream,
               "Pool $P ($U) {\n", (WriteFP)pool, (WriteFU)pool->serial,
               "  class $P (\"$S\")\n", 
               (WriteFP)pool->class, pool->class->name,
               "  space $P ($U)\n", 
               (WriteFP)pool->space, (WriteFU)pool->space->serial,
               "  alignment $W\n", (WriteFW)pool->alignment,
               NULL);
  if(res != ResOK) return res;

  res = (*pool->class->describe)(pool, stream);
  if(res != ResOK) return res;

  node = RingNext(&pool->bufferRing);
  while(node != &pool->bufferRing) {
    Buffer buffer = RING_ELT(Buffer, poolRing, node);
    res = BufferDescribe(buffer, stream);
    if(res != ResOK) return res;
    node = RingNext(node);
  }

  res = WriteF(stream,
               "} Pool $P ($U)\n", (WriteFP)pool, (WriteFU)pool->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}


/* .pool.space: Thread safe; see design.mps.interface.c.thread-safety */
/* See impl.h.mpm for macro version */
Space (PoolSpace)(Pool pool)
{
  AVERT(0xA55E62, Pool, pool);
  return pool->space;
}


/* PoolSegAlloc -- allocate a segment in a pool
 *
 * @@@@ There's no need for this routine.  The segment could be
 * attached in SegInit.
 */

Res PoolSegAlloc(Seg *segReturn, SegPref pref, Pool pool, Size size)
{
  Res res;
  Seg seg;
  Space space;

  AVER(0xA55E62, segReturn != NULL);
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, SegPref, pref);
  space = PoolSpace(pool);
  AVER(0xA55E62, SizeIsAligned(size, ArenaAlign(space)));

  res = SegAlloc(&seg, pref, space, size, pool);
  if(res != ResOK) return res;

  RingAppend(&pool->segRing, &seg->poolRing);

  *segReturn = seg;
  return ResOK;
}


/* PoolSegFree -- free a segment from a pool
 *
 * @@@@ There's no need for this routine.  The segment could be
 * detached in SegFinish.
 */

void PoolSegFree(Pool pool, Seg seg)
{
  Space space;

  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Seg, seg);
  AVER(0xA55E62, seg->pool == pool);

  space = PoolSpace(pool);

  ShieldFlush(space); /* See impl.c.shield.shield.flush */

  RingRemove(&seg->poolRing);

  SegFree(space, seg);
}


Bool PoolOfAddr(Pool *poolReturn, Space space, Addr addr)
{
  Seg seg;

  AVER(0xA55E62, poolReturn != NULL);
  /* Cannot AVERT space here, because PoolOfAddr is called under SpaceCheck */

  if(SegOfAddr(&seg, space, addr)) {
    *poolReturn = seg->pool;
    return TRUE;
  }

  return FALSE;
}


Bool PoolHasAddr(Pool pool, Addr addr)
{
  Pool addrPool;
  Space space;
  Bool managed;

  AVERT(0xA55E62, Pool, pool);

  space = PoolSpace(pool);
  managed = PoolOfAddr(&addrPool, space, addr);
  if(managed && addrPool == pool)
    return TRUE;
  else
    return FALSE;
}


/* See impl.h.mpm for macro version */
Align (PoolAlignment)(Pool pool)
{
  AVERT(0xA55E62, Pool, pool);
  return pool->alignment;
}


/* PoolNo*, PoolTriv* -- Trivial and non-methods for Pool Classes 
 * See design.mps.pool.no and design.mps.pool.triv
 */

void PoolTrivFinish(Pool pool)
{
  AVERT(0xA55E62, Pool, pool);
  NOOP;
}

Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(0xA55E62, pReturn != NULL);
  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, size > 0);
  NOTREACHED(0xA55E62);
  return ResUNIMPL;
}

Res PoolTrivAlloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(0xA55E62, pReturn != NULL);
  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, size > 0);
  return ResLIMIT;
}

void PoolNoFree(Pool pool, Addr old, Size size)
{
  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, old != NULL);
  AVER(0xA55E62, size > 0);
  NOTREACHED(0xA55E62);
}

void PoolTrivFree(Pool pool, Addr old, Size size)
{
  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, old != NULL);
  AVER(0xA55E62, size > 0);
  NOOP;                         /* trivial free has no effect */
}

Res PoolNoBufferInit(Pool pool, Buffer buffer)
{
  AVERT(0xA55E62, Pool, pool);
  UNUSED(buffer);
  NOTREACHED(0xA55E62);
  return ResUNIMPL;
}

/* The generic method initialised all generic fields; */
/* This doesn't override any fields */
Res PoolTrivBufferInit(Pool pool, Buffer buffer)
{
  AVERT(0xA55E62, Pool, pool);
  UNUSED(buffer);
  return ResOK;
}

void PoolNoBufferFinish(Pool pool, Buffer buffer)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, BufferIsReset(buffer));
  NOTREACHED(0xA55E62);
}

void PoolTrivBufferFinish(Pool pool, Buffer buffer)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, BufferIsReset(buffer));
  NOOP;
}

Res PoolNoBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                     Pool pool, Buffer buffer, Size size)
{
  AVER(0xA55E62, baseReturn != NULL);
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, size > 0);
  NOTREACHED(0xA55E62);
  return ResUNIMPL;
}

Res PoolTrivBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                       Pool pool, Buffer buffer, Size size)
{
  Res res;
  Addr p;
  Seg seg;
  Bool b;

  AVER(0xA55E62, baseReturn != NULL);
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, size > 0);

  res = PoolAlloc(&p, pool, size);
  if(res != ResOK) return res;
  
  b = SegOfAddr(&seg, PoolSpace(pool), p);
  AVER(0xA55E62, b);
  
  *segReturn = seg;
  *baseReturn = p;
  *limitReturn = AddrAdd(p, size);
  return ResOK;
}

void PoolNoBufferEmpty(Pool pool, Buffer buffer)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, !BufferIsReset(buffer));
  AVER(0xA55E62, BufferIsReady(buffer));
  NOTREACHED(0xA55E62);
}

void PoolTrivBufferEmpty(Pool pool, Buffer buffer)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, !BufferIsReset(buffer));
  AVER(0xA55E62, BufferIsReady(buffer));
}

Res PoolNoDescribe(Pool pool, mps_lib_FILE *stream)
{
  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, stream != NULL);
  NOTREACHED(0xA55E62);
  return ResUNIMPL;
}

Res PoolTrivDescribe(Pool pool, mps_lib_FILE *stream)
{
  AVERT(0xA55E62, Pool, pool);
  AVER(0xA55E62, stream != NULL);
  return WriteF(stream, "  No class-specific description available.\n", NULL);
}

Res PoolNoCondemn(Pool pool, Trace trace, Seg seg)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);
  NOTREACHED(0xA55E62);
  return ResUNIMPL;
}

void PoolNoGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);
  NOTREACHED(0xA55E62);
}

void PoolTrivGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);

  /* @@@@ The trivial grey method probably shouldn't exclude */
  /* the white segments, since they might also contain grey objects. */
  /* It's probably also the Tracer's responsibility to raise the */
  /* shield. */
  /* @@@@ This should be calculated by comparing colour */
  /* with the mutator colour.  For the moment we assume */
  /* a read-barrier collector. */

  if(!TraceSetIsMember(seg->white, trace->ti)) {
    seg->grey = TraceSetAdd(seg->grey, trace->ti);
    ShieldRaise(trace->space, seg, AccessREAD);
  }
}

Res PoolNoScan(ScanState ss, Pool pool, Seg seg)
{
  AVERT(0xA55E62, ScanState, ss);
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Seg, seg);
  NOTREACHED(0xA55E62);
  return ResUNIMPL;
}

Res PoolNoFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, ScanState, ss);
  AVERT(0xA55E62, Seg, seg);
  AVER(0xA55E62, refIO != NULL);
  NOTREACHED(0xA55E62);
  return ResUNIMPL;
}

void PoolNoReclaim(Pool pool, Trace trace, Seg seg)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);
  NOTREACHED(0xA55E62);
}
