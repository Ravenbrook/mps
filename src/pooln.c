/* impl.c.pooln: NULL POOL
 *
 * $HopeName: MMsrc!pooln.c(MMdevel_assertid.1) $
 * Copyright(C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This is the implementation of the null pool class.  Begin null it
 * all functions are implemented in a trivial manner.
 */

#include "mpm.h"
#include "pooln.h"

SRCID(pooln, "$HopeName: MMsrc!pooln.c(MMdevel_assertid.1) $");


typedef struct PoolNStruct {
  PoolStruct poolStruct;                /* generic pool structure */
  /* and that's it */
} PoolNStruct;

#define PoolPoolN(pool) PARENT(PoolNStruct, poolStruct, pool)


static Res NInit(Pool pool, va_list args)
{
  PoolN poolN = PoolPoolN(pool);

  UNUSED(args);
  
  /* Initialize pool-specific structures. */

  AVERT(0xA55E62, PoolN, poolN);

  return ResOK;
}

static void NFinish(Pool pool)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  /* Finish pool-specific structures. */
}

Pool (PoolNPool)(PoolN poolN)
{
  AVERT(0xA55E62, PoolN, poolN);
  return &poolN->poolStruct;
}

static Res NAlloc(Addr *pReturn, Pool pool, Size size)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  AVER(0xA55E62, pReturn != NULL);
  AVER(0xA55E62, size > 0);

  return ResLIMIT;  /* limit of nil blocks exceeded */
}

static void NFree(Pool pool, Addr old, Size size)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  AVER(0xA55E62, old != (Addr)0);
  AVER(0xA55E62, size > 0);

  NOTREACHED(0xA55E62);  /* can't allocate, should never free */
}

static Res NBufferInit(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  UNUSED(buffer);

  return ResLIMIT;  /* limit of nil buffers exceeded */
}

static void NBufferFinish(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, BufferIsReset(buffer));

  NOTREACHED(0xA55E62);  /* can't create, so shouldn't destroy */
}

static Res NBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                       Pool pool, Buffer buffer, Size size)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);
  AVER(0xA55E62, segReturn != NULL);
  AVER(0xA55E62, baseReturn != NULL);
  AVER(0xA55E62, limitReturn != NULL);
  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, BufferIsReset(buffer));
  AVER(0xA55E62, size > 0);

  NOTREACHED(0xA55E62);   /* can't create buffers, so shouldn't fill them */
  return ResUNIMPL;
}

static void NBufferEmpty(Pool pool, Buffer buffer)
{
  AVERT(0xA55E62, Pool, pool);
  AVERT(0xA55E62, Buffer, buffer);
  AVER(0xA55E62, !BufferIsReset(buffer));
  AVER(0xA55E62, BufferIsReady(buffer));

  NOTREACHED(0xA55E62);   /* can't create buffers, so they shouldn't trip */
}

static Res NDescribe(Pool pool, mps_lib_FILE *stream)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  UNUSED(stream);

  return ResOK;
}

static Res NCondemn(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);

  return ResOK;
}

static void NGrey(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);
}

static Res NScan(ScanState ss, Pool pool, Seg seg)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  AVERT(0xA55E62, ScanState, ss);
  AVERT(0xA55E62, Seg, seg);

  return ResOK;
}

static Res NFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  AVERT(0xA55E62, ScanState, ss);
  UNUSED(refIO);
  AVERT(0xA55E62, Seg, seg);

  /* since we don't allocate any objects, should never */
  /* be called upon to fix a reference */
  NOTREACHED(0xA55E62);

  return ResFAIL;
}

static void NReclaim(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(0xA55E62, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xA55E62, PoolN, poolN);

  AVERT(0xA55E62, Trace, trace);
  AVERT(0xA55E62, Seg, seg);
  /* all unmarked and white objects reclaimed */
}

static PoolClassStruct PoolClassNStruct = {
  PoolClassSig,                         /* sig */
  "N",                                  /* name */
  sizeof(PoolNStruct),                  /* size */
  offsetof(PoolNStruct, poolStruct),    /* offset */
  AttrSCAN | AttrALLOC | AttrFREE | AttrBUF | AttrBUF_RESERVE | AttrGC,
  NInit,                                /* init */
  NFinish,                              /* finish */
  NAlloc,                               /* alloc */
  NFree,                                /* free */
  NBufferInit,                          /* bufferInit */
  NBufferFill,                          /* bufferFill */
  NBufferEmpty,                         /* bufferEmpty */
  NBufferFinish,                        /* bufferFinish */
  NCondemn,                             /* condemn */
  NGrey,                                /* grey */
  NScan,                                /* scan */
  NFix,                                 /* fix */
  NReclaim,                             /* reclaim */
  NDescribe,                            /* describe */
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};

PoolClass PoolClassN(void)
{
  return &PoolClassNStruct;
}

Bool PoolNCheck(PoolN poolN)
{
  CHECKL(0xA55E62, poolN != NULL);
  CHECKD(0xA55E62, Pool, &poolN->poolStruct);
  CHECKL(0xA55E62, poolN->poolStruct.class == &PoolClassNStruct);
  return TRUE;
}

