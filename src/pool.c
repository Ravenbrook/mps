/* impl.c.pool: POOL IMPLEMENTATION
 *
 * $HopeName: MMsrc!pool.c(MMdevel_restr2.3) $
 * Copyright (C) 1994,1995,1996 Harlequin Group, all rights reserved
 *
 * This is the implementation of the generic pool interface.  The
 * functions here dispatch to pool-specific methods.
 */

#include "mpm.h"

SRCID(pool, "$HopeName: MMsrc!pool.c(MMdevel_restr2.3) $");


Bool PoolClassCheck(PoolClass class)
{
  CHECKS(PoolClass, class);
  CHECKL(class->endSig == PoolClassSig);
  CHECKL(class->name != NULL);
  CHECKL(class->size >= sizeof(PoolStruct));
  CHECKL(class->offset <= (size_t)(class->size - sizeof(PoolStruct)));
  CHECKL(class->init != NULL);
  CHECKL(class->finish != NULL);
  /* if there's a free there must be an alloc */
  CHECKL(class->alloc != NULL || class->free == NULL);
  /* if theres a bufferCreate there must be other buffer methods */
  if((class->attr & AttrBUF) != 0) {
    CHECKL(class->bufferFill != NULL);
    CHECKL(class->bufferTrip != NULL);
  }
  /* can't say much about access */
  return TRUE;
}


Bool PoolCheck(Pool pool)
{
  CHECKS(Pool, pool);
  CHECKU(Space, pool->space);
  CHECKL(pool->serial < pool->space->poolSerial);
  CHECKD(PoolClass, pool->class);
  CHECKL(RingCheck(&pool->spaceRing));
  CHECKL(RingCheck(&pool->bufferRing));
  CHECKL(AlignCheck(pool->alignment));
  return TRUE;
}


/* PoolInitV -- initialize a pool
 *
 * Initialize the generic fields of the pool.  The pool gets the
 * default alignment initially.
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

Res PoolInitV(Pool pool, Space space, PoolClass class, va_list args)
{
  Res res;

  AVER(pool != NULL);
  AVERT(Space, space);

  pool->class = class;
  pool->space = space;
  RingInit(&pool->spaceRing);
  RingInit(&pool->bufferRing);
  pool->alignment = ARCH_ALIGN;

  pool->sig = PoolSig;
  pool->serial = space->poolSerial;
  ++space->poolSerial;

  AVERT(Pool, pool);

  /* Do class-specific initialization. */
  res = (*class->init)(pool, args);
  if(res)
    goto failInit;

  RingAppend(SpacePoolRing(space), &pool->spaceRing);
  return ResOK;

failInit:
  pool->sig = SigInvalid;
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->spaceRing);
  SpaceFree(space, (Addr)pool, class->size);
  return res;
}


Res PoolCreate(Pool *poolReturn, PoolClass class, Space space, ...)
{
  Res res;
  va_list arg;
  va_start(arg, space);
  res = PoolCreateV(poolReturn, class, space, arg);
  va_end(arg);
  return res;
}

Res PoolCreateV(Pool *poolReturn, PoolClass class,
                  Space space, va_list arg)
{
  Res res;
  Pool pool;
  Addr base;

  AVER(poolReturn != NULL);
  AVERT(Space, space);

  /* Allocate the pool instance structure with the size requested */
  /* in the pool class. */
  res = SpaceAlloc(&base, space, class->size);
  if(res) return res;

  /* Calculate the adress of the generic pool structure within the */
  /* instance by using the offset information from the class. */
  pool = (Pool)AddrAdd(base, class->offset);

  /* Initialize the pool. */  
  res = PoolInitV(pool, space, class, arg);
  if(res) {
    SpaceFree(space, base, class->size);
    return res;
  }
  
  *poolReturn = pool;  
  return ResOK;
}


void PoolFinish(Pool pool)
{
  PoolClass class;

  AVERT(Pool, pool);  
  
  class = pool->class;

  /* Do any class-specific finishing. */
  (*class->finish)(pool);
  
  /* Detach the pool from the space, and unsig it. */
  RingRemove(&pool->spaceRing);
  pool->sig = SigInvalid;
  
  /* Finish the generic fields. */
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->spaceRing);
}

void PoolDestroy(Pool pool)
{
  PoolClass class;
  Space space;
  Addr base;

  AVERT(Pool, pool);  
  
  class = pool->class;
  space = pool->space;

  /* Finish the pool instance structure. */
  PoolFinish(pool);

  /* Free the pool instance structure. */
  base = AddrAdd((Addr)pool, -class->offset);
  SpaceFree(space, base, class->size);
}


Res (PoolAlloc)(Addr *pReturn, Pool pool, Size size)
{
  Res res;

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);

  res = (*pool->class->alloc)(pReturn, pool, size);
  if(res != ResOK) return res;

  /* Make sure that the allocated address was in the pool's memory. */
  AVER(PoolHasAddr(pool, *pReturn));

  return ResOK;
}

void PoolFree(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(old != (Addr)0);
  AVER(PoolHasAddr(pool, old));

  if(pool->class->free != NULL)
    (*pool->class->free)(pool, old, size);
}

Res PoolCondemn(RefSet *condemnedReturn, Pool pool,
                  Space space, TraceId ti)
{
  if(pool->class->condemn != NULL)
    return (*pool->class->condemn)(condemnedReturn, pool, space, ti);

  *condemnedReturn = RefSetEmpty;
  return ResOK;
}

void PoolGrey(Pool pool, Space space, TraceId ti)
{
  if(pool->class->grey != NULL)
    (*pool->class->grey)(pool, space, ti);
}

Res PoolScan(ScanState ss, Pool pool, Bool *finishedReturn)
{
  if(pool->class->scan != NULL)
    return (*pool->class->scan)(ss, pool, finishedReturn);
  else {
    *finishedReturn = TRUE;
    return ResOK;
  }
}

Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  AVERT(Pool, pool);
  return PoolFix(pool, ss, seg, refIO);
}

void PoolReclaim(Pool pool, Space space, TraceId ti)
{
  if(pool->class->reclaim != NULL)
    (*pool->class->reclaim)(pool, space, ti);
}


void PoolAccess(Pool pool, Seg seg, AccessSet mode)
{
  if(pool->class->access != NULL)
    (*pool->class->access)(pool, seg, mode);
}


Res PoolDescribe(Pool pool, Lib_FILE *stream)
{
  AVERT(Pool, pool);
  AVER(stream != NULL);

  Lib_fprintf(stream,
              "Pool %p {\n"
              "  Class %s\n"
              "  alignment %lu\n",
              pool,
              pool->class->name,
              (unsigned long)pool->alignment);

  if(pool->class->describe == NULL)
    Lib_fprintf(stream, "  No class-specific description available.\n");
  else
    (void)(*pool->class->describe)(pool, stream);

  Lib_fprintf(stream, "} Pool %p\n", pool);

  return ResOK;
}


/* Thread safe */
Space (PoolSpace)(Pool pool)
{
  return pool->space;
}


Res PoolSegAlloc(Seg *segReturn, Pool pool, Size size)
{
  Res res;
  Seg seg;
  Space space;

  AVER(segReturn != NULL);
  AVERT(Pool, pool);
  space = PoolSpace(pool);
  AVER(SizeIsAligned(size, ArenaAlign(space)));

  res = SegAlloc(&seg, space, size, pool);
  if(res != ResOK)
    return res;

  seg->pool = pool;

  *segReturn = seg;
  return ResOK;
}


void PoolSegFree(Pool pool, Seg seg)
{
  Space space;

  AVERT(Pool, pool);

  space = PoolSpace(pool);

  ShieldFlush(space);

  SegFree(space, seg);
}


Bool PoolOfAddr(Pool *poolReturn, Space space, Addr addr)
{
  Seg seg;

  AVER(poolReturn != NULL);

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

  AVERT(Pool, pool);

  space = PoolSpace(pool);
  if(PoolOfAddr(&addrPool, space, addr) && addrPool == pool)
    return TRUE;
  else
    return FALSE;
}


Align (PoolAlignment)(Pool pool)
{
  AVERT(Pool, pool);
  return pool->alignment;
}
