/* impl.c.dbgpool: POOL DEBUG MIXIN
 *
 * $HopeName$
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 */

#include "dbgpool.h"
#include "mpslib.h"
#include "mpm.h"
#include <stdarg.h>


#define TagInitMethodCheck(f) \
  ((f) != NULL) /* that's the best we can do */


/* PoolDebugMixinCheck -- check a PoolDebugMixin */

Bool PoolDebugMixinCheck(PoolDebugMixin debug)
{
  /* Nothing to check about fenceTemplate */
  /* Nothing to check about fenceSize */
  CHECKL(TagInitMethodCheck(debug->tagInit));
  return TRUE;
}


/* PoolDebugOptionsCheck -- check a PoolDebugOptions */

static Bool PoolDebugOptionsCheck(PoolDebugOptions opt)
{
  CHECKL(opt != NULL);
  CHECKL(opt->fenceTemplate != NULL);
  /* Nothing to check about fenceSize */
  CHECKL(TagInitMethodCheck(opt->tagInit));
  return TRUE;
}


/* DebugPoolInit -- init wrapper for a debug pool */

Res DebugPoolInit(PoolDebugMixin debug, Pool pool, va_list args)
{
  Res res;
  PoolDebugOptions options;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  options = va_arg(args, PoolDebugOptions);
  AVERT(PoolDebugOptions, options);

  res = (pool->class->super->init)(pool, args);
  if(res != ResOK)
    return res;

  /* @@@@ This parses a user argument, options, so it should really */
  /* go through the MPS interface. */
  if (options->fenceSize != 0) {
    if (debug->fenceSize % PoolAlignment(pool) != 0) {
      res = ResPARAM;
      goto alignFail;
    }

    /* Fenceposting turns on tagging */
    if (options->tagInit == NULL) options->tagInit = TagTrivInit;

    debug->fenceSize = options->fenceSize;
    /* @@@@ copy */
    debug->fenceTemplate = options->fenceTemplate;
  }
  if (options->tagInit != NULL) {
    debug->tagInit = options->tagInit;
  }
  return ResOK;

alignFail:
  (pool->class->super->finish)(pool);
  return res;
}


/* FenceAlloc -- allocation wrapper for fenceposts
 *
 * Allocates an object, adding fenceposts on both sides.  Layout:
 *
 * |----------|-------------------------------------|------|----------|
 *   start fp              client object              slop    end fp
 *
 * slop is the extra allocation from rounding up the client request to
 * the pool's alignment.  The fenceposting code does this, so there's a
 * better chance of the end fencepost being flush with the next object
 * (can't be guaranteed, since the underlying pool could have allocated
 * an even larger block).  The alignment slop is filled from the
 * fencepost template as well (as much as fits, .fence.size guarantees
 * the template is larger).
 */

static Res FenceAlloc(Addr *aReturn, PoolDebugMixin debug, Pool pool,
                      Size size, Bool withReservoirPermit)
{
  Res res;
  Addr new, clientNew;
  Size alignedSize;

  AVER(aReturn != NULL);
  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(Bool, withReservoirPermit);

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  res = (pool->class->super->alloc)(&new, pool,
                                    alignedSize + 2*debug->fenceSize,
                                    withReservoirPermit);
  if(res != ResOK)
    return res;
  clientNew = AddrAdd(new, debug->fenceSize);
  /* @@@@ shields? */
  /* start fencepost, see .trans.memcpy to justify the casts */
  mps_lib_memcpy((void *)new, debug->fenceTemplate, debug->fenceSize);
  /* alignment slop, see .trans.memcpy to justify the casts */
  mps_lib_memcpy((void *)AddrAdd(clientNew, size),
                 debug->fenceTemplate, alignedSize - size);
  /* end fencepost, see .trans.memcpy to justify the casts */
  mps_lib_memcpy((void *)AddrAdd(clientNew, alignedSize),
                 debug->fenceTemplate, debug->fenceSize);

  *aReturn = clientNew;
  return res;
}


/* FenceFree -- freeing wrapper for debug mixin */

static void FenceFree(PoolDebugMixin debug,
                      Pool pool, Addr old, Size size)
{
  Size alignedSize;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  /* Can't check old */
  AVER(size > 0);

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  (pool->class->super->free)(pool, old,
                             alignedSize + 2*debug->fenceSize);
}


/* DebugPoolAlloc -- allocation wrapper for debug mixin */

Res DebugPoolAlloc(Addr *aReturn, PoolDebugMixin debug, Pool pool,
                   Size size, Bool withReservoirPermit)
{
  Res res;
  Addr new;

  AVER(aReturn != NULL);
  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(Bool, withReservoirPermit);

  res = FenceAlloc(&new, debug, pool, size, withReservoirPermit);
  if(res != ResOK)
    return res;
  res = TagAlloc(pool, new, size, NULL);
  if(res != ResOK)
    goto tagFail;

  *aReturn = new;
  return res;

tagFail:
  FenceFree(debug, pool, new, size);
  return res;
}


/* DebugPoolFree -- freeing wrapper for debug mixin */

void DebugPoolFree(PoolDebugMixin debug, Pool pool, Addr old, Size size)
{
  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  /* Can't check old */
  AVER(size > 0);

  TagFree(pool, old, size, NULL);
  FenceFree(debug, pool, old, size);
}
