/*  impl.c.pooln
 *
 *                         NULL POOL
 *
 *  $HopeName: MMsrc!pooln.c(trunk.7) $
 *
 *  Copyright(C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the null pool class.  Begin null it
 *  all functions are implemented in a trivial manner.
 */

#include "mpm.h"
#include "pooln.h"

SRCID(pooln, "$HopeName: MMsrc!pooln.c(trunk.7) $");


typedef struct PoolNStruct {
  PoolStruct poolStruct;                /* generic pool structure */
  /* and that's it */
} PoolNStruct;

#define PoolPoolN(pool)	PARENT(PoolNStruct, poolStruct, pool)


/*  Class's methods  */

static Res NInit(Pool pool, va_list arg);
static void NFinish(Pool pool);
static Res NAlloc(Addr *pReturn, Pool pool, Size size);
static void NFree(Pool pool, Addr old, Size size);
static Res NBufferCreate(Buffer *bufReturn, Pool pool);
static void NBufferDestroy(Pool pool, Buffer buf);
static Res NBufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size);
static Bool NBufferTrip(Pool pool, Buffer buffer, Addr p, Size size);
static void NBufferExpose(Pool pool, Buffer buffer);
static void NBufferCover(Pool pool, Buffer buffer);
static Res NDescribe(Pool pool, Lib_FILE *stream);
static Res NCondemn(RefSet *condemnedReturn, Pool pool,
                     Space space, TraceId ti);
static void NMark(Pool pool, Space space, TraceId ti);
static Res NScan(ScanState ss, Pool pool, Bool *finishedReturn);
static Res NFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
static void NReclaim(Pool pool, Space space, TraceId ti);
static void NAccess(Pool pool, Seg seg, AccessSet mode);


/*  Class Structure  */
static PoolClassStruct PoolClassNStruct;


PoolClass PoolClassN(void)
{
  PoolClassInit(&PoolClassNStruct,
                "N",
                sizeof(PoolNStruct), offsetof(PoolNStruct, poolStruct),
                NInit, NFinish,
                NAlloc, NFree,
                NBufferCreate, NBufferDestroy,
                NBufferFill, NBufferTrip,
                NBufferExpose, NBufferCover,
                NCondemn, NMark, NScan,
                NFix, NReclaim,
                NAccess,
                NDescribe);
  return &PoolClassNStruct;
}

Bool PoolNCheck(PoolN poolN)
{
  CHECKL(poolN != NULL);
  CHECKD(Pool, &poolN->poolStruct);
  CHECKL(poolN->poolStruct.class == &PoolClassNStruct);
  return TRUE;
}

static Res NInit(Pool pool, va_list args)
{
  PoolN poolN = PoolPoolN(pool);

  UNUSED(args);
  
  /* Initialize pool-specific structures. */

  AVERT(PoolN, poolN);

  return ResOK;
}

static void NFinish(Pool pool)
{
  PoolN poolN = PoolPoolN(pool);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);

  /* Finish pool-specific structures. */
}

Pool (PoolNPool)(PoolN poolN)
{
  AVERT(PoolN, poolN);
  return &poolN->poolStruct;
}

static Res NAlloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVER(size > 0);

  return ResLIMIT;  /* limit of nil blocks exceeded */
}

static void NFree(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVER(old != (Addr)0);
  AVER(size > 0);

  NOTREACHED;  /* can't allocate, should never free */
}

static Res NBufferCreate(Buffer *bufReturn, Pool pool)
{
  AVER(bufReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);

  return ResLIMIT;  /* limit of nil buffers exceeded */
}

static void NBufferDestroy(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);

  NOTREACHED;  /* can't create, so shouldn't destroy */
}

static Res NBufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(pReturn != NULL);

  NOTREACHED;	/* can't create buffers, so shouldn't fill them */
  return ResUNIMPL;
}

static Bool NBufferTrip(Pool pool, Buffer buffer, Addr p, Size size)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(p != 0);
  AVER(size > 0);

  NOTREACHED;	/* can't create buffers, so they shouldn't trip */
  return FALSE;
}

static void NBufferExpose(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);

  NOTREACHED;	/* can't create buffers, so shouldn't expose them */
}

static void NBufferCover(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);

  NOTREACHED;	/* can't create buffers, so shouldn't cover them */
}

static Res NDescribe(Pool pool, Lib_FILE *stream)
{
  PoolN poolN;

  UNUSED(stream);

  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);

  poolN = PARENT(PoolNStruct, poolStruct, pool);
  AVERT(PoolN, poolN);

  return ResOK;
}

static Res NCondemn(RefSet *condemnedReturn, Pool pool,
                     Space space, TraceId ti)
{
  AVER(condemnedReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(Space, space);

  return ResOK;
}

static void NMark(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(Space, space);
}

static Res NScan(ScanState ss, Pool pool, Bool *finishedReturn)
{
  AVER(finishedReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(ScanState, ss);

  return ResOK;
}

static Res NFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  NOTREACHED;  /* since we don't allocate any objects, should never
                * be called upon to fix a reference */
  return ResFAIL;
}

static void NReclaim(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(Space, space);
  /* all unmarked and condemned objects reclaimed */
}

static void NAccess(Pool pool, Seg seg, AccessSet mode)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(Seg, seg);
  UNUSED(mode);
  /* deal with access to segment */
}
