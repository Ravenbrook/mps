/* impl.c.pooln: NULL POOL
 *
 * $HopeName: MMsrc!pooln.c(MMdevel_assertid.3) $
 * Copyright(C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This is the implementation of the null pool class.  Begin null it
 * all functions are implemented in a trivial manner.
 */

#include "mpm.h"
#include "pooln.h"

SRCID(pooln, "$HopeName: MMsrc!pooln.c(MMdevel_assertid.3) $");

#define PoolNSig        ((Sig)0x519B7499)

typedef struct PoolNStruct {
  PoolStruct poolStruct;                /* generic pool structure */
  /* class-specific fields go here */
  Sig sig;				/* design.mps.sig */
} PoolNStruct;

#define PoolPoolN(pool) PARENT(PoolNStruct, poolStruct, pool)


static Res NInit(Pool pool, va_list args)
{
  PoolN poolN = PoolPoolN(pool);

  UNUSED(args);
  
  /* Initialize pool-specific structures. */
  
  poolN->sig = PoolNSig;

  AVERT(0xB0040000, PoolN, poolN);

  return ResOK;
}

static void NFinish(Pool pool)
{
  PoolN poolN;

  AVERT(0xB0040001, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040002, PoolN, poolN);

  poolN->sig = SigInvalid;

  /* Finish pool-specific structures. */
}

Pool (PoolNPool)(PoolN poolN)
{
  AVERT(0xB0040003, PoolN, poolN);
  return &poolN->poolStruct;
}

static Res NAlloc(Addr *pReturn, Pool pool, Size size)
{
  PoolN poolN;

  AVERT(0xB0040004, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040005, PoolN, poolN);

  AVER(0xB0040006, pReturn != NULL);
  AVER(0xB0040007, size > 0);

  return ResLIMIT;  /* limit of nil blocks exceeded */
}

static void NFree(Pool pool, Addr old, Size size)
{
  PoolN poolN;

  AVERT(0xB0040008, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040009, PoolN, poolN);

  AVER(0xB004000A, old != (Addr)0);
  AVER(0xB004000B, size > 0);

  NOTREACHED(0xB004000C);  /* can't allocate, should never free */
}

static Res NBufferInit(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(0xB004000D, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB004000E, PoolN, poolN);

  UNUSED(buffer);

  return ResLIMIT;  /* limit of nil buffers exceeded */
}

static void NBufferFinish(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(0xB004000F, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040010, PoolN, poolN);

  AVERT(0xB0040011, Buffer, buffer);
  AVER(0xB0040012, BufferIsReset(buffer));

  NOTREACHED(0xB0040013);  /* can't create, so shouldn't destroy */
}

static Res NBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                       Pool pool, Buffer buffer, Size size)
{
  PoolN poolN;

  AVERT(0xB0040014, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040015, PoolN, poolN);
  AVER(0xB0040016, segReturn != NULL);
  AVER(0xB0040017, baseReturn != NULL);
  AVER(0xB0040018, limitReturn != NULL);
  AVERT(0xB0040019, Buffer, buffer);
  AVER(0xB004001A, BufferIsReset(buffer));
  AVER(0xB004001B, size > 0);

  NOTREACHED(0xB004001C);   /* can't create buffers, so shouldn't fill them */
  return ResUNIMPL;
}

static void NBufferEmpty(Pool pool, Buffer buffer)
{
  AVERT(0xB004001D, Pool, pool);
  AVERT(0xB004001E, Buffer, buffer);
  AVER(0xB004001F, !BufferIsReset(buffer));
  AVER(0xB0040020, BufferIsReady(buffer));

  NOTREACHED(0xB0040021);   /* can't create buffers, so they shouldn't trip */
}

static Res NDescribe(Pool pool, mps_lib_FILE *stream)
{
  PoolN poolN;

  AVERT(0xB0040022, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040023, PoolN, poolN);

  UNUSED(stream);

  return ResOK;
}

static Res NCondemn(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(0xB0040024, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040025, PoolN, poolN);

  AVERT(0xB0040026, Trace, trace);
  AVERT(0xB0040027, Seg, seg);

  return ResOK;
}

static void NGrey(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(0xB0040028, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040029, PoolN, poolN);

  AVERT(0xB004002A, Trace, trace);
  AVERT(0xB004002B, Seg, seg);
}

static Res NScan(ScanState ss, Pool pool, Seg seg)
{
  PoolN poolN;

  AVERT(0xB004002C, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB004002D, PoolN, poolN);

  AVERT(0xB004002E, ScanState, ss);
  AVERT(0xB004002F, Seg, seg);

  return ResOK;
}

static Res NFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  PoolN poolN;

  AVERT(0xB0040030, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040031, PoolN, poolN);

  AVERT(0xB0040032, ScanState, ss);
  UNUSED(refIO);
  AVERT(0xB0040033, Seg, seg);

  /* since we don't allocate any objects, should never */
  /* be called upon to fix a reference */
  NOTREACHED(0xB0040034);

  return ResFAIL;
}

static void NReclaim(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(0xB0040035, Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(0xB0040036, PoolN, poolN);

  AVERT(0xB0040037, Trace, trace);
  AVERT(0xB0040038, Seg, seg);
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
  CHECKL(0xB0040039, poolN != NULL);
  CHECKD(0xB004003A, Pool, &poolN->poolStruct);
  CHECKL(0xB004003B, poolN->poolStruct.class == &PoolClassNStruct);
  return TRUE;
}

