/* poollo.c: LEAF POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: <design/poollo>.  This is a leaf pool class.
 */

#include "mpsclo.h"
#include "mpm.h"
#include "mps.h"

SRCID(poollo, "$Id$");


/* LOStruct -- leaf object pool instance structure */

#define LOSig           ((Sig)0x51970B07) /* SIGnature LO POoL */

typedef struct LOStruct *LO;

typedef struct LOStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  PoolGenStruct pgenStruct;     /* generation representing the pool */
  PoolGen pgen;                 /* NULL or pointer to pgenStruct */
  Sig sig;                      /* <code/misc.h#sig> */
} LOStruct;

typedef LO LOPool;
#define LOPoolCheck LOCheck
DECLARE_CLASS(Pool, LOPool, AbstractCollectPool);


/* forward declaration */
static Bool LOCheck(LO lo);


/* LOVarargs -- decode obsolete varargs */

static void LOVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_FORMAT;
  args[0].val.format = va_arg(varargs, Format);
  args[1].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}


/* LOInit -- initialize an LO pool */

static Res LOInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  LO lo;
  Res res;
  ArgStruct arg;
  Chain chain;
  unsigned gen = LO_GEN_DEFAULT;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);
  UNUSED(klass); /* used for debug pools only */

  res = NextMethod(Pool, LOPool, init)(pool, arena, klass, args);
  if (res != ResOK)
    goto failNextInit;
  lo = CouldBeA(LOPool, pool);

  /* Ensure a format was supplied in the argument list. */
  AVER(pool->format != NULL);

  if (ArgPick(&arg, args, MPS_KEY_CHAIN))
    chain = arg.val.chain;
  else {
    chain = ArenaGlobals(arena)->defaultChain;
    gen = 1; /* avoid the nursery of the default chain by default */
  }
  if (ArgPick(&arg, args, MPS_KEY_GEN))
    gen = arg.val.u;

  AVERT(Format, pool->format);
  AVER(FormatArena(pool->format) == arena);
  AVERT(Chain, chain);
  AVER(gen <= ChainGens(chain));
  AVER(chain->arena == arena);

  pool->alignment = pool->format->alignment;
  pool->alignShift = SizeLog2(pool->alignment);

  lo->pgen = NULL;

  SetClassOfPoly(pool, CLASS(LOPool));
  lo->sig = LOSig;
  AVERC(LOPool, lo);

  res = PoolGenInit(&lo->pgenStruct, ChainGen(chain, gen), pool);
  if (res != ResOK)
    goto failGenInit;
  lo->pgen = &lo->pgenStruct;

  EVENT2(PoolInitLO, pool, pool->format);

  return ResOK;

failGenInit:
  NextMethod(Inst, LOPool, finish)(MustBeA(Inst, pool));
failNextInit:
  AVER(res != ResOK);
  return res;
}


/* LOFinish -- finish an LO pool */

static void LOFinish(Inst inst)
{
  Pool pool = MustBeA(AbstractPool, inst);
  LO lo = MustBeA(LOPool, pool);
  Ring node, nextNode;

  RING_FOR(node, &pool->segRing, nextNode) {
    Seg seg = SegOfPoolRing(node);
    SegFree(seg);
  }
  PoolGenFinish(lo->pgen);

  lo->sig = SigInvalid;

  NextMethod(Inst, LOPool, finish)(inst);
}


/* LOBufferFill -- fill buffer from an LO segment */

static Res LOBufferFill(Addr *baseReturn, Addr *limitReturn,
                        Pool pool, Buffer buffer,
                        Size size)
{
  LO lo = MustBeA(LOPool, pool);
  Res res;
  Ring node, nextNode;
  RankSet rankSet;
  Seg seg;
  Bool b;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERC(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(BufferRankSet(buffer) == RankSetEMPTY);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* Try to find a segment with enough space already. */
  rankSet = BufferRankSet(buffer);
  RING_FOR(node, PoolSegRing(pool), nextNode) {
    seg = SegOfPoolRing(node);
    if (SegBufferFill(baseReturn, limitReturn, seg, size, rankSet))
      return ResOK;
  }

  /* No segment had enough space, so make a new one. */
  res = PoolGenAlloc(&seg, lo->pgen, CLASS(MarkSweepSeg),
                     SizeArenaGrains(size, PoolArena(pool)),
                     argsNone);
  if (res != ResOK)
    return res;
  b = SegBufferFill(baseReturn, limitReturn, seg, size, rankSet);
  AVER(b);
  return ResOK;
}


/* loSegPoolGen -- get pool generation for an LO segment */

static PoolGen loSegPoolGen(Pool pool, Seg seg)
{
  LO lo = MustBeA(LOPool, pool);
  AVERT(Seg, seg);
  return lo->pgen;
}


/* LOTotalSize -- total memory allocated from the arena */
/* TODO: This code is repeated in AMS */

static Size LOTotalSize(Pool pool)
{
  LO lo = MustBeA(LOPool, pool);
  return lo->pgen->totalSize;
}


/* LOFreeSize -- free memory (unused by client program) */
/* TODO: This code is repeated in AMS */

static Size LOFreeSize(Pool pool)
{
  LO lo = MustBeA(LOPool, pool);
  return lo->pgen->freeSize;
}


/* LOPoolClass -- the class definition */

DEFINE_CLASS(Pool, LOPool, klass)
{
  INHERIT_CLASS(klass, LOPool, AbstractCollectPool);
  klass->instClassStruct.finish = LOFinish;
  klass->size = sizeof(LOStruct);
  klass->varargs = LOVarargs;
  klass->init = LOInit;
  klass->bufferFill = LOBufferFill;
  klass->segPoolGen = loSegPoolGen;
  klass->totalSize = LOTotalSize;
  klass->freeSize = LOFreeSize;
  AVERT(PoolClass, klass);
}


/* mps_class_lo -- the external interface to get the LO pool class */

mps_pool_class_t mps_class_lo(void)
{
  return (mps_pool_class_t)CLASS(LOPool);
}


/* LOCheck -- check an LO pool */

ATTRIBUTE_UNUSED
static Bool LOCheck(LO lo)
{
  CHECKS(LO, lo);
  CHECKC(LOPool, lo);
  CHECKD(Pool, &lo->poolStruct);
  CHECKC(LOPool, lo);
  if (lo->pgen != NULL) {
    CHECKL(lo->pgen == &lo->pgenStruct);
    CHECKD(PoolGen, lo->pgen);
  }
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
