/* impl.c.dbgpool: POOL DEBUG MIXIN
 *
 * $HopeName: MMsrc!dbgpool.c(MMdevel_fencepost.1) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 */

#include "dbgpool.h"
#include "mpslib.h"
#include "mpm.h"
#include <stdarg.h>


/* tagStruct -- tags for storing info baout allocated objects */

typedef struct tagStruct {
  Addr addr;
  Size size;
  SplayNode splayNode;
  char userdata[1 /* actually variable length */];
} tagStruct;

#define SplayNode2Tag(node) PARENT(tagStruct, splayNode, (node))

typedef tagStruct *Tag;


/* tag init methods: copying the user-supplied data into the tag */

#define TagInitMethodCheck(f) \
  ((f) != NULL) /* that's the best we can do */

static void TagTrivInit(void* tag, va_list args)
{
  UNUSED(tag); UNUSED(args);
}


/* AddrComp -- splay comparison function for address ordering of tags */

static Compare AddrComp(void *key, SplayNode node)
{
  Addr addr1, addr2;

  addr1 = *(Addr *)key;
  addr2 = SplayNode2Tag(node)->addr;
  if (addr1 < addr2)
    return CompareLESS;
  else if (addr1 > addr2) {
    /* Check key is not inside the object of this tag */
    AVER_CRITICAL(AddrAdd(addr2, SplayNode2Tag(node)->size) <= addr1);
    return CompareGREATER;
  } else
    return CompareEQUAL;
}


/* PoolDebugMixinCheck -- check a PoolDebugMixin */

Bool PoolDebugMixinCheck(PoolDebugMixin debug)
{
  /* Nothing to check about fenceTemplate */
  /* Nothing to check about fenceSize */
  CHECKL(TagInitMethodCheck(debug->tagInit));
  /* Nothing to check about tagSize */
  CHECKD(Pool, debug->tagPool);
  CHECKL(CHECKTYPE(Addr, void*)); /* tagPool relies on this */
  /* Nothing to check about missingTags */
  CHECKL(SplayTreeCheck(debug->index));
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

  /* fencepost init */
  /* @@@@ This parses a user argument, options, so it should really */
  /* go through the MPS interface.  Possibly the template needs to be */
  /* copied into Addr memory. */
  if (options->fenceSize != 0) {
    if (debug->fenceSize % PoolAlignment(pool) != 0) {
      res = ResPARAM;
      goto alignFail;
    }
    /* Fenceposting turns on tagging */
    if (options->tagInit == NULL) {
      options->tagSize = 0;
      options->tagInit = TagTrivInit;
    }
    debug->fenceSize = options->fenceSize;
    debug->fenceTemplate = options->fenceTemplate;
  }
  
  /* tag init */
  if (options->tagInit != NULL) {
    debug->tagInit = options->tagInit;
    debug->tagSize = options->tagSize + sizeof(tagStruct) - 1;
    /* Should we use a separate arena? */
    /* This pool has to be like the arena control pool: the blocks */
    /* allocated must be accessible using void*. */
    res = PoolCreate(&debug->tagPool, PoolArena(pool), PoolClassMFS(),
                     debug->tagSize, debug->tagSize);
    if(res != ResOK)
      goto tagFail;
    debug->missingTags = 0;
    SplayTreeInit(debug->index, AddrComp);
  }

  debug->sig = PoolDebugMixinSig;
  AVERT(PoolDebugMixin, debug);
  return ResOK;

tagFail:
alignFail:
  (pool->class->super->finish)(pool);
  return res;
}


/* DebugPoolFinish -- finish wrapper for a debug pool */

void DebugPoolFinish(PoolDebugMixin debug, Pool pool)
{
  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);

  SplayTreeFinish(debug->index);
  (pool->class->super->finish)(pool);
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
  /* @@@@ change the type of mps_lib_memcpy to Addr? */
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


/* FenceCheck -- check fences of an object */

static Bool FenceCheck(PoolDebugMixin debug, Pool pool,
                       Addr obj, Size size)
{
  Size alignedSize;

  AVERT_CRITICAL(PoolDebugMixin, debug);
  AVERT_CRITICAL(Pool, pool);
  /* Can't check obj */

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  /* @@@@ mps_lib_memcmp? */
  return (memcmp(AddrAdd(obj, -debug->fenceSize), debug->fenceTemplate,
                 debug->fenceSize)
          && memcmp(AddrAdd(obj, alignedSize), debug->fenceTemplate,
                    debug->fenceSize));
}


/* FenceFree -- freeing wrapper for fenceposts */

static void FenceFree(PoolDebugMixin debug,
                      Pool pool, Addr old, Size size)
{
  Size alignedSize;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  /* Can't check old */
  AVER(size > 0);

  AVER(FenceCheck(debug, pool, old, size));

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  (pool->class->super->free)(pool, AddrAdd(old, -debug->fenceSize),
                             alignedSize + 2*debug->fenceSize);
}


/* TagAlloc -- tag allocation */

static Res TagAlloc(PoolDebugMixin debug,
                    Pool pool, Addr new, Size size,
                    Bool withReservoirPermit, void *tagData)
{
  Tag tag;
  Res res;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  AVER(size > 0);

  res = PoolAlloc((Addr*)&tag, debug->tagPool, debug->tagSize, FALSE);
  if (res != ResOK)
    if (withReservoirPermit) { /* design.mps.debug.@@@@ */
      debug->missingTags++;
      return ResOK;
    } else {
      return res;
    }
  tag->addr = new; tag->size = size;
  (debug->tagInit)((void *)tag->userdata, tagData);
  res = SplayTreeInsert(debug->index, tag->splayNode, (void *)&new);
  AVER(res == ResOK);
  return ResOK;
}


static void TagFree(PoolDebugMixin debug, Pool pool, Addr old, Size size)
{
  SplayNode node;
  Tag tag;
  Res res;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  AVER(size > 0);

  res = SplayTreeSearch(&node, debug->index, (void *)&old);
  if (res != ResOK) {
    AVER(debug->missingTags > 0);
    debug->missingTags--;
    return;
  }
  tag = SplayNode2Tag(node);
  AVER(tag->size == size);
  res = SplayTreeDelete(debug->index, node, (void *)&old);
  AVER(res == ResOK);
  PoolFree(debug->tagPool, (Addr)tag, debug->tagSize);
}


/* DebugPoolAlloc -- allocation wrapper for a debug pool */

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
  if (res != ResOK)
    return res;
  res = TagAlloc(debug, pool, new, size, withReservoirPermit, NULL);
  if (res != ResOK)
    goto tagFail;

  *aReturn = new;
  return res;

tagFail:
  FenceFree(debug, pool, new, size);
  return res;
}


/* DebugPoolFree -- freeing wrapper for a debug pool */

void DebugPoolFree(PoolDebugMixin debug, Pool pool, Addr old, Size size)
{
  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  /* Can't check old */
  AVER(size > 0);

  TagFree(debug, pool, old, size);
  FenceFree(debug, pool, old, size);
}
