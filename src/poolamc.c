/* impl.c.poolamc: AUTOMATIC MOSTLY COPYING POOL CLASS
 *
 * $HopeName$
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 */

#include "mpm.h"
#include "poolamc.h"
#include "mpscamc.h"

SRCID(poolamc, "$HopeName$");

#define GEN_MAX         ((Size)4)

/* GenStruct -- generation structure
 *
 * GenStruct represents a generation, which is a set of objects which
 * can be condemned together.  The intention is that each generation
 * represents a set of objects of similar age, and therefore similar
 * probability of death, allowing predictions to be made about the
 * benefit of collection.
 *
 * Each GenStruct contains an Option object which allows the strategy
 * module to reason about the possibility of collecting the generation.
 *
 * GEN_MAX GenStructs are located in an array in the PoolAMCStruct.
 */

typedef struct GenStruct *Gen;
typedef struct GenStruct {
  OptionStruct optionStruct;    /* the option of collecting this generation */
  BufferStruct bufferStruct;	/* forwarding buffer into this generation */
} GenStruct;


/* PoolAMCStruct -- the class-specific pool structure
 *
 */

#define AMCSig          ((Sig)0x519A3CB1)

typedef struct PoolAMCStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  Format format;                /* format of objects in pool */
  GenStruct gen[GEN_MAX];       /* generations */
  Sig sig;                      /* design.mps.sig */
} PoolAMCStruct;

#define PoolPoolAMC(pool) PARENT(PoolAMCStruct, poolStruct, pool)


/* GenIndex -- return the number of a generation
 *
 * This function returns the number of a generation within a pool.
 */

static Index GenIndex(PoolAMC amc, Gen gen)
{
  AVER(&amc->gen[0] <= gen);
  AVER(gen < &amc->gen[GEN_MAX]);
  return gen - amc->gen;
}


/* AMCInit -- initialize the class-specific pool structure
 *
 * AMCInit accepts the pool-specific arguments to PoolCreate
 * and sets up the fields of the PoolAMCStruct.
 */

static Res AMCInit(Pool pool, va_list args)
{
  PoolAMC amc = PoolPoolAMC(pool);
  Index i;
  Res res;

  /* design.mps.poolamc.init */
  amc->format = va_arg(args, Format);
  AVERT(Format, amc->format);

  amc->poolStruct.alignment = amc->format->alignment; /* @@@@? */

  /* This is really a loop of GenInits. */
  for(i = 0; i < GEN_MAX; ++i) {
    Gen gen = &amc->gen[i];
    OptionInit(&gen->optionStruct, pool);
    /* .init.buf: This couldn't be done here unless the checking */
    /* in AMCBufferInit was relaxed.  See .buf.init. */
    res = BufferInit(&gen->bufferStruct, pool, RankEXACT);
    AVER(res == ResOK);
    gen->bufferStruct.i = i;	/* allocate in generation i */
    gen->bufferStruct.mutator = FALSE;
  }
  
  amc->sig = AMCSig;
  AVERT(PoolAMC, amc);

  return ResOK;
}


/* AMCFinish -- finish the AMC pool structure */

static void AMCFinish(Pool pool)
{
  PoolAMC amc;
  Ring node;
  Index i;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);

  /* Destroy all segments in the pool. */
  node = RingNext(&pool->segRing);
  while(node != &pool->segRing) {
    Ring next = RingNext(node);
    Seg seg = RING_ELT(Seg, poolRing, node);

    PoolSegFree(pool, seg);

    node = next;
  }
  
  /* Finish the generations. */
  for(i = 0; i < GEN_MAX; ++i) {
    Gen gen = &amc->gen[i];
    BufferFinish(&gen->bufferStruct);
    OptionFinish(&gen->optionStruct);
  }

  amc->sig = SigInvalid;
}


Pool (PoolAMCPool)(PoolAMC amc)
{
  AVERT(PoolAMC, amc);
  return &amc->poolStruct;
}


#if 0 /* @@@@ Could make Alloc allocate a padding object. */
static Res AMCAlloc(Addr *pReturn, Pool pool, Size size)
{
  PoolAMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);

  AVER(pReturn != NULL);
  AVER(size > 0);

  return ResLIMIT;  /* limit of nil blocks exceeded */
}

static void AMCFree(Pool pool, Addr old, Size size)
{
  PoolAMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);

  AVER(old != (Addr)0);
  AVER(size > 0);

  NOTREACHED;  /* can't allocate, should never free */
}
#endif /* 0 */


static Res AMCBufferInit(Pool pool, Buffer buffer)
{
  PoolAMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);

  /* .buf.init: The checking is relaxed here so that buffers */
  /* can be initialized before the pool is completely ready. */
  /* This is unfortunate. */
  /* AVERT(PoolAMC, amc); */
  
  buffer->i = 0;                /* allocate in generation 0 */

  return ResOK;
}

static void AMCBufferFlush(PoolAMC amc, Buffer buffer)
{
  Size size;
  Space space;
  Seg seg;

  AVERT(PoolAMC, amc);
  AVERT(Buffer, buffer);
  AVER(!BufferIsReset(buffer));	/* @@@@ double-check these */
  AVER(BufferIsReady(buffer));	/* @@@@ double-check these */

  /* Pad out the segment with a dummy object. */
  /* See design.mps.poolamc.seg.content. */
  space = BufferSpace(buffer);
  seg = BufferSeg(buffer);
  size = AddrOffset(buffer->ap.init, SegLimit(space, seg));
  if(size > 0)
    (*amc->format->pad)(buffer->ap.init, size);

  BufferReset(buffer);          /* cause next reserve to refill */
}

static void AMCBufferFinish(Pool pool, Buffer buffer)
{
  PoolAMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);
  AVERT(Buffer, buffer);

  if(!BufferIsReset(buffer))
    AMCBufferFlush(amc, buffer);
}

static Res AMCBufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size)
{
  PoolAMC amc;
  Space space;
  Size alignedSize;
  Res res;
  Seg seg;
  Addr base;
  void *p;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);

  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(pReturn != NULL);

  if(!BufferIsReset(buffer))
    AMCBufferFlush(amc, buffer);
  else
    AVER(buffer->p == NULL);

  space = BufferSpace(buffer);
  alignedSize = SizeAlignUp(size, ArenaAlign(space));

  res = PoolSegAlloc(&seg, pool, alignedSize);
  if(res != ResOK) return res;

  SegSetP(seg, &amc->gen[buffer->i]);        /* design.mps.poolamc.seg.p */

  /* Give the buffer the entire segment to allocate in. */
  base = SegBase(space, seg);
  BufferSet(buffer, seg, base, base, AddrAdd(base, alignedSize));

  /* Actually do the reserve. */
  p = (void *)buffer->ap.init;
  buffer->ap.alloc = AddrAdd(buffer->ap.alloc, size);

  /* If the mutator owns this buffer it's about to put something */
  /* in there, so the segment must take the mutator colour.  Otherwise */
  /* we leave the colour setting up to the reserver. */
  if(buffer->mutator)
    ColMerge(SegCol(seg), SpaceMutCol(space));

  /* The rank of the segment is determined by the buffer's rank. */
  seg->rank = buffer->rank;

  *pReturn = p;
  return ResOK;
}

static Bool AMCBufferTrip(Pool pool, Buffer buffer, Addr p, Size size)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(p != 0);
  AVER(size > 0);

  NOTREACHED;   /* can't create buffers, so they shouldn't trip */
  return FALSE;
}

static void AMCBufferExpose(Pool pool, Buffer buffer)
{
  PoolAMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);

  AVERT(Buffer, buffer);

  NOTREACHED;   /* can't create buffers, so shouldn't expose them */
}

static void AMCBufferCover(Pool pool, Buffer buffer)
{
  PoolAMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);

  AVERT(Buffer, buffer);

  NOTREACHED;   /* can't create buffers, so shouldn't cover them */
}

static Res AMCDescribe(Pool pool, mps_lib_FILE *stream)
{
  PoolAMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);

  UNUSED(stream);

  return ResOK;
}

static void AMCCondemn(Pool pool, Option option, Seg seg, TraceId ti)
{
  PoolAMC amc;
  Gen gen;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);
  AVERT(Seg, seg);
  AVERT(Option, option);

  AVER(SegPool(seg) == pool);
  AVER(option->pool == pool);
  
  gen = PARENT(GenStruct, optionStruct, option);
  AVER(&amc->gen[0] <= gen && gen < &amc->gen[GEN_MAX]);

  if(gen == (Gen)SegP(seg) && SegBuffer(seg) == NULL)
    ColSetWhite(SegCol(seg), TraceSetSingle(ti));
}

static Res AMCScan(Pool pool, Fix fix, Seg seg)
{
  PoolAMC amc;
  Addr base, limit;
  Space space;
  Res res;
  FormatScanMethod scan;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);
  AVERT(Seg, seg);
  AVERT(Fix, fix);
  
  space = PoolSpace(pool);
  scan = amc->format->scan;

  /* The segment being scanned shouldn't be white for the traces */
  /* because AMC doesn't mix white and non-white in a segment. */
  AVER(!ColIsWhite(SegCol(seg), fix->ts));
  
  /* The segment being scanned should be grey for the traces, otherwise */
  /* the scan method shouldn't've been called by the Tracer. */
  AVER(ColIsGrey(SegCol(seg), fix->ts));
  
  /* While the segment remains buffered, scan to the limit of */
  /* initialized objects in the buffer.  Either it will be reached, */
  /* or more objects will appear until the segment fills up and the */
  /* buffer moves away. */
  base = SegBase(space, seg);
  while(SegBuffer(seg) != NULL) {
    limit = SegBuffer(seg)->ap.init;
    if(base >= limit) goto scanned;     /* end of buffer reached? */
    res = (*scan)(fix, base, limit);
    if(res != ResOK) return res;
    base = limit;
  }

  /* If the segment is unbuffered, or becomes so during scanning */
  /* (e.g. if the forwarding buffer gets flushed) then scan to */
  /* the limit of the segment. */
  limit = SegLimit(space, seg);
  if(base < limit) {
    res = (*scan)(fix, base, limit);
    if(res != ResOK) return res;
  }

scanned:
  /* Check the defensive exit conditions of the loops above. */
  AVER(base == limit);

  return ResOK;
}

static Res AMCFix(Ref *refIO, Pool pool, Fix fix, Seg seg)
{
  PoolAMC amc;
  Res res;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);
  AVERT(Fix, fix);
  AVERT(Seg, seg);

  AVER(ColIsWhite(SegCol(seg), fix->ts));

  switch(fix->rank) {
    case RankAMBIG:
    ColGreyen(SegCol(seg), fix->ts);
    break;
    
    case RankEXACT:
    case RankWEAK: {
      Ref ref = *refIO;
      Ref newRef = (*amc->format->isMoved)(ref);

      if(newRef == 0 && fix->rank != RankWEAK) {
        Format format = amc->format;
        Buffer buffer = &amc->gen[1].bufferStruct; /* @@@@ */
        Size length;

        length = AddrOffset(ref, (*format->skip)(ref));

        do {
          res = BufferReserve(&newRef, buffer, length);
          if(res != ResOK) return res;
          ColMergeNGreyen(SegCol(BufferSeg(buffer)), SegCol(seg), fix->ts);
          (*format->copy)(ref, newRef);
        } while(!BufferCommit(buffer, newRef, length));
      
        (*format->move)(ref, newRef);
      }

      *refIO = newRef;
    } break;

    case RankFINAL:
    default:
    NOTREACHED;
  }

  return ResOK;
}

static void AMCReclaim(Pool pool, Seg seg, TraceSet ts)
{
  PoolAMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(PoolAMC, amc);

  AVERT(Seg, seg);
  AVER(TraceSetCheck(ts));

  /* AMC doesn't mix white and non-white objects on segments, */
  /* so a white segment at this stage is completely unreachable */
  /* and can be freed without fuss. */
  PoolSegFree(pool, seg);
}


static PoolClassStruct PoolClassAMCStruct = {
  PoolClassSig,                         /* sig */
  "AMC",                                /* name */
  sizeof(PoolAMCStruct),                /* size */
  offsetof(PoolAMCStruct, poolStruct),  /* offset */
  AttrSCAN | AttrALLOC | AttrFREE | AttrBUF | AttrBUF_RESERVE | AttrGC,
  AMCInit,                              /* init */
  AMCFinish,                            /* finish */
  PoolNoAlloc,                          /* alloc */
  PoolNoFree,                           /* free */
  AMCBufferInit,                        /* bufferInit */
  AMCBufferFinish,                      /* bufferFinish */
  AMCBufferFill,                        /* bufferFill */
  AMCBufferTrip,                        /* bufferTrip */
  AMCBufferExpose,                      /* bufferExpose */
  AMCBufferCover,                       /* bufferCover */
  AMCCondemn,				/* condemn */
  AMCScan,                              /* scan */
  AMCFix,                               /* fix */
  AMCReclaim,                           /* reclaim */
  AMCDescribe,                          /* describe */
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};

Bool PoolAMCCheck(PoolAMC amc)
{
  Index i;
  CHECKL(amc != NULL);
  CHECKS(AMC, amc);
  CHECKD(Pool, &amc->poolStruct);
  CHECKL(amc->poolStruct.class == &PoolClassAMCStruct);
  CHECKD(Format, amc->format);
  for(i = 0; i < GEN_MAX; ++i) {
    /* @@@@ Check generation */
  }
  return TRUE;
}

PoolClass PoolClassAMC(void)
{
  return &PoolClassAMCStruct;
}

mps_class_t mps_class_amc(void)
{
  return (mps_class_t)(PoolClassAMC());
}
