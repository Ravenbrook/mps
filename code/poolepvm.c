/* impl.c.poolepvm: ELECTRONIC PUBLISHING "VIRTUAL MEMORY" CLASS
 *
 * $Id$
 * $HopeName: MMsrc!poolepvm.c(trunk.45) $
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .purpose: This is the implementation of the PostScript Virtual Memory
 * pool class for MM/EP-core.
 *
 * .design: see design.mps.poolams and design.mps.poolepvm.  */

#include "mpscepvm.h"
#include "mps.h"
#include "poolams.h"
#include "mpm.h"
#include "chain.h"
#include <stdarg.h>

SRCID(poolepvm, "$Id$");


/* structures */


typedef struct EPVMSegStruct *EPVMSeg;
typedef struct EPVMSaveStruct *EPVMSave;
typedef struct EPVMStruct *EPVM;


/* EPVMSegStruct -- EPVM segment instances */

#define EPVMSegSig      ((Sig)0x519EBF39) /* SIGnature EPVM seG */

typedef struct EPVMSegStruct {
  AMSSegStruct amsSegStruct; /* superclass fields must come first */
  EPVMSave save;         /* owning save structure */
  Sig sig;               /* design.mps.pool.outer-structure.sig */
} EPVMSegStruct;


/* EPVMSaveStruct -- Save level info
 * 
 * See design.mps.poolepvm.arch.save.  */

#define EPVMSaveSig     ((Sig)0x519EBF35) /* SIGnature EPVM Save */

typedef struct EPVMSaveStruct {
  Sig sig;
  Index level;                  /* save level */
  Size size;                    /* total size of segs at this level */
  Bool smallStringSeg;          /* small seg alloc'ed at this level? */
  Bool smallObjectSeg;          /* small seg alloc'ed at this level? */
  EPVM epvm;                    /* owning epvm */
  RingStruct segRing;           /* ring of segs at this level */
} EPVMSaveStruct;


/* EPVMStruct -- PostScript Virtual Memory pool instance structure */

#define EPVMSig         ((Sig)0x519EBF33) /* SIGnature EPVM */

typedef struct EPVMStruct {
  AMSStruct amsStruct;      /* generic AMS structure */
  Index saveLevel;          /* current save level */
  Index maxSaveLevel;       /* maximum save level */
  Size subsequentSegRound;  /* modulus of subsequent segs */
  EPVMSaveStruct *saves;    /* pointer to array of save structs */
  Sig sig;
} EPVMStruct;


/* Forward declarations */

static Bool EPVMCheck(EPVM epvm);
static BufferClass EPVMBufferClassGet(void);
static SegClass EPVMSegClassGet(void);


/* macros to get between child and parent structures */

#define Seg2EPVMSeg(seg)             ((EPVMSeg)(seg))
#define EPVMSeg2Seg(epvmSeg)         ((Seg)(epvmSeg))
#define AMS2EPVM(ams)   PARENT(EPVMStruct, amsStruct, ams)
#define EPVM2AMS(epvm)  (&(epvm)->amsStruct)
#define Pool2EPVM(pool) PARENT(EPVMStruct, amsStruct.poolStruct, pool)
#define EPVM2Pool(epvm) (&(epvm)->amsStruct.poolStruct)


/* macros for maneuvering about between structures */

#define EPVMSegEPVM(epvmSeg)       ((epvmSeg)->save->epvm)
#define EPVMLevelSave(epvm, level) (&(epvm)->saves[level])
#define EPVMCurrentSave(epvm)      (&(epvm)->saves[(epvm)->saveLevel])


/* EPVMSaveCheck -- check the save level structure */

static Bool EPVMSaveCheck(EPVMSave save)
{
  CHECKS(EPVMSave, save);
  CHECKU(EPVM, save->epvm);
  CHECKL(save->level <= save->epvm->maxSaveLevel);
  CHECKL(save->size <= EPVM2AMS(save->epvm)->size);
  if (save->level > save->epvm->saveLevel) /* nothing at this level */
    CHECKL(save->size == 0);
  CHECKL(SizeIsAligned(save->size,
                       ArenaAlign(PoolArena(EPVM2Pool(save->epvm)))));
  CHECKL(BoolCheck(save->smallStringSeg));
  CHECKL(BoolCheck(save->smallObjectSeg));
  CHECKL(RingCheck(&save->segRing));

  return TRUE;
}


/* EPVMSaveInit -- initialize a save structure */

static void EPVMSaveInit(EPVM epvm, Index level)
{
  EPVMSave save = EPVMLevelSave(epvm, level);

  RingInit(&save->segRing);
  save->epvm = epvm;
  save->level = level;
  save->size = 0;
  save->smallStringSeg = FALSE;
  save->smallObjectSeg = FALSE;
  save->sig = EPVMSaveSig;
  /* .save.nocheck: Can't be checked before the pool init is */
  /* completed, then it is checked by EPVMCheck. */
}


/* EPVMSegCheck -- check the EPVM segment */

static Bool EPVMSegCheck(EPVMSeg epvmSeg)
{
  Seg seg;

  CHECKS(EPVMSeg, epvmSeg);
  CHECKL(AMSSegCheck(&epvmSeg->amsSegStruct));
  seg = EPVMSeg2Seg(epvmSeg);
  CHECKU(EPVMSave, epvmSeg->save);
  CHECKL(epvmSeg->save->size >= SegSize(seg));
  /* buffers only on the current save level */
  if (SegBuffer(seg) != NULL)
    CHECKL(EPVMCurrentSave(EPVMSegEPVM(epvmSeg)) == epvmSeg->save);
  /* See design.mps.poolepvm.protection.format and */
  /* d.m.p.protection.hack. */
  AVER(SegSummary(seg) == RefSetUNIV || SegSummary(seg) == RefSetEMPTY);

  return TRUE;
}


/* EPVMSegInit -- initialise an epvm segment */

static Res EPVMSegInit(Seg seg, Pool pool, Addr base, Size size, 
                       Bool reservoirPermit, va_list args)
{
  SegClass super;
  EPVMSeg epvmSeg;
  EPVM epvm;
  EPVMSave save;
  Res res;

  AVERT(Seg, seg);
  epvmSeg = Seg2EPVMSeg(seg);
  AVERT(Pool, pool);
  epvm = Pool2EPVM(pool);
  AVERT(EPVM, epvm);
  /* no useful checks for base and size */
  AVER(BoolCheck(reservoirPermit));

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(EPVMSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if (res != ResOK)
    return res;

  save = EPVMCurrentSave(epvm);
  epvmSeg->save = save;
  save->size += SegSize(seg);

  if (RankSetEMPTY == SegRankSet(seg)) {
    save->smallStringSeg = TRUE;
  } else {
    save->smallObjectSeg = TRUE;
  }

  epvmSeg->sig = EPVMSegSig;
  AVERT(EPVMSeg, epvmSeg);

  return ResOK;
}


/* EPVMSegFinish -- Finish method for EPVM segments */

static void EPVMSegFinish(Seg seg)
{
  SegClass super;
  EPVMSeg epvmSeg;
  EPVMSave save;
  Size size;

  AVERT(Seg, seg);
  epvmSeg = Seg2EPVMSeg(seg);
  AVERT(EPVMSeg, epvmSeg);

  save = epvmSeg->save;
  AVERT(EPVMSave, save);
  size = SegSize(seg);
  AVER(save->size >= size);
  save->size -= size;
  epvmSeg->sig = SigInvalid;

  /* finish the superclass fields last */
  super = SEG_SUPERCLASS(EPVMSegClass);
  super->finish(seg);
}  


/* EPVMSegDescribe -- describe an EPVM segment */

static Res EPVMSegDescribe(Seg seg, mps_lib_FILE *stream)
{
  Res res;
  EPVMSeg epvmSeg;
  SegClass super;

  /* .describe.check: Debugging tools don't AVER things. */
  if (!CHECKT(Seg, seg)) 
    return ResFAIL;
  if (stream == NULL) 
    return ResFAIL;
  epvmSeg = Seg2EPVMSeg(seg);
  if (!CHECKT(EPVMSeg, epvmSeg)) 
    return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(EPVMSegClass);
  res = super->describe(seg, stream);
  if (res != ResOK)
    return res;

  res = WriteF(stream,
               "  save $P\n", (WriteFP)epvmSeg->save,
               NULL);
  return res;
}


/* EPVMSegClass -- Class definition for EPVM segments */

DEFINE_SEG_CLASS(EPVMSegClass, class)
{
  INHERIT_CLASS(class, AMSSegClass);
  SegClassMixInNoSplitMerge(class);
  class->name = "EPVMSEG";
  class->size = sizeof(EPVMSegStruct);
  class->init = EPVMSegInit;
  class->finish = EPVMSegFinish;
  class->describe = EPVMSegDescribe;
}


/* EPVMSegSizePolicy -- method for deciding the segment size */

static Res EPVMSegSizePolicy(Size *sizeReturn,
                             Pool pool, Size size, RankSet rankSet)
{
  Bool smallSeg;  /* has a small segment already been allocated? */
  EPVM epvm;
  EPVMSave save;

  AVER(sizeReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(RankSetCheck(rankSet));

  epvm = Pool2EPVM(pool);
  save = EPVMCurrentSave(epvm);

  if (RankSetEMPTY == rankSet) {
    /* string segment */
    smallSeg = save->smallStringSeg;
  } else {
    /* object segment */
    AVER(RankSetSingle(RankEXACT) == rankSet);
    smallSeg = save->smallObjectSeg;
  }

  if (smallSeg) {
    /* small segment already allocated, so this is a */
    /* subsequent segment */
    size = SizeRoundUp(size, epvm->subsequentSegRound);
  } else {
    size = SizeAlignUp(size, ArenaAlign(PoolArena(pool)));
  }
  if (size == 0) /* overflow */
    return ResMEMORY;

  *sizeReturn = size;
  return ResOK;
}


/* epvmSaveDiscard -- discards all the segs at one save level */

static void epvmSaveDiscard(EPVMSave save)
{
  Ring ring, node, next;           /* for iterating over the segs */

  /* destroy all the segs */
  ring = &save->segRing;
  RING_FOR(node, ring, next) {
    EPVMSeg epvmSeg = RING_ELT(EPVMSeg, amsSegStruct.segRing, node);
    Seg seg = EPVMSeg2Seg(epvmSeg);
    Buffer buffer;

    AVERT_CRITICAL(EPVMSeg, epvmSeg);
    buffer = SegBuffer(seg);
    if (buffer != NULL)
      BufferDetach(buffer, EPVM2Pool(save->epvm));
    SegFree(seg);
  }
  save->smallStringSeg = FALSE;
  save->smallObjectSeg = FALSE;

  AVER(save->size == 0);
}


/* EPVMCurrentRing -- the ring of segments for the current save level */

static Ring EPVMCurrentRing(AMS ams, RankSet rankSet, Size size)
{
  UNUSED(rankSet); UNUSED(size);
  return &EPVMCurrentSave(AMS2EPVM(ams))->segRing;
}


/* EPVMSegsDestroy -- destroy all the segments */

static void EPVMSegsDestroy(AMS ams)
{
  EPVM epvm;
  Index i;

  AVERT(AMS, ams);
  epvm = AMS2EPVM(ams);
  AVERT(EPVM, epvm);

  for(i = 0; i <= epvm->maxSaveLevel; ++i) {
    EPVMSave save = EPVMLevelSave(epvm, i);
    epvmSaveDiscard(save);
  }
}


static Res EPVMIterate(Seg seg, AMSObjectFunction f, void *closure);


/* EPVMInit -- the pool class initialization method
 * 
 * Takes three additional arguments: the format of the objects allocated
 * in the pool, the maximum save level, and the current save level.  */

static Res EPVMInit(Pool pool, va_list args)
{
  EPVM epvm; AMS ams;
  void *p;
  Res res;
  Index i;
  mps_epvm_save_level_t maxSaveLevel;
  Format format;
  Arena arena;
  Chain chain;
  static GenParamStruct epvmGenParam = { SizeMAX, 0.5 /* dummy */ };

  AVERT(Pool, pool);
  arena = PoolArena(pool);

  format = va_arg(args, Format);
  res = ChainCreate(&chain, arena, 1, &epvmGenParam);
  if (res != ResOK)
    return res;
  res = AMSInitInternal(Pool2AMS(pool), format, chain);
  if (res != ResOK)
    goto failAMSInit;

  epvm = Pool2EPVM(pool);
  maxSaveLevel = va_arg(args, mps_epvm_save_level_t);
  epvm->maxSaveLevel = (Index)maxSaveLevel;
  AVER(epvm->maxSaveLevel > 0);
  epvm->saveLevel = (Index)va_arg(args, mps_epvm_save_level_t);
  AVER(epvm->saveLevel <= epvm->maxSaveLevel);
  epvm->subsequentSegRound =
    SizeAlignUp(EPVMDefaultSubsequentSegSIZE, ArenaAlign(arena));

  res = ControlAlloc(&p, arena, sizeof(EPVMSaveStruct) * (maxSaveLevel+1),
                     FALSE);
  if (res != ResOK)
    goto failSaveAlloc;
  epvm->saves = p;
  for(i = 0; i <= epvm->maxSaveLevel; ++i)
    EPVMSaveInit(epvm, i);

  ams = Pool2AMS(pool);
  ams->iterate = EPVMIterate; /* should use a format variant */
  ams->segSize = EPVMSegSizePolicy;
  ams->allocRing = EPVMCurrentRing;
  ams->segsDestroy = EPVMSegsDestroy;
  ams->segClass = EPVMSegClassGet;

  epvm->sig = EPVMSig;
  AVERT(EPVM, epvm);
  EVENT_PPPUU(PoolInitEPVM, pool, arena, format, maxSaveLevel, epvm->saveLevel);
  return ResOK;

failSaveAlloc:
  AMSFinish(pool);
failAMSInit:
  ChainDestroy(chain);
  return res;
}


/* EPVMFinish -- the pool class finishing method
 * 
 * Destroys all the segs in the pool.  Can't invalidate the EPVM until
 * we've destroyed all the segs, as it may be checked.  */

static void EPVMFinish(Pool pool)
{
  EPVM epvm; AMS ams;
  Index i;
  Chain chain;

  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);
  epvm = Pool2EPVM(pool);
  AVERT(EPVM, epvm);

  chain = AMSChain(ams);
  AMSFinish(pool);
  /* Can't invalidate the EPVM until we've destroyed all the segs. */
  epvm->sig = SigInvalid;

  /* Can't invalidate the save levels until after the segs. */
  for(i = 0; i <= epvm->maxSaveLevel; ++i) {
    EPVMSave save = EPVMLevelSave(epvm, i);
    RingFinish(&save->segRing);
    save->sig = SigInvalid;
  }
  ControlFree(PoolArena(pool), epvm->saves,
              sizeof(EPVMSaveStruct) * (epvm->maxSaveLevel + 1));
  ChainDestroy(chain);
}


/* EPVMBufferInit -- initialize a buffer
 *
 * See design.mps.poolepvm.arch.obj-string.  */

static Res EPVMBufferInit (Buffer buffer, Pool pool, va_list args)
{
  Bool isObj = va_arg(args, Bool);
  BufferClass super;
  Res res;

  AVERT(Buffer, buffer);
  AVERT(Pool, pool);
  AVERT(Bool, isObj);

  /* Initialize the superclass fields first via next-method call */
  super = BUFFER_SUPERCLASS(EPVMBufferClass);
  res = super->init(buffer, pool, args);
  if (res != ResOK)
    return res;

  if (isObj)
    BufferSetRankSet(buffer, RankSetSingle(RankEXACT));

  EVENT_PPU(BufferInitEPVM, buffer, pool, isObj);
  return ResOK;
}


/* EPVMBufferClass -- EPVMBufferClass class definition 
 *
 * Like SegBufClass, but with special initialization.  */
 
DEFINE_BUFFER_CLASS(EPVMBufferClass, class)
{
  INHERIT_CLASS(class, SegBufClass);
  class->name = "EPVMBUF";
  class->init = EPVMBufferInit;
}



/* EPVMIterate -- applies a function to each allocated grain in a seg
 * 
 * EPVMIterate (seg, f, closure) applies f to all the allocated grains
 * in the seg.  See design.mps.poolepvm.low.iterate.  */

static Res EPVMIterate(Seg seg, AMSObjectFunction f, void *closure)
{
  EPVMSeg epvmSeg;
  Res res;
  Index i = 0;
  Size grainSize;
  Buffer buffer;
  Index scanLimit = 0, limit = 0; /* stop uninitialised warnings */

  AVERT(Seg, seg);
  AVERT(AMSObjectFunction, f);
  /* Can't check closure */
  epvmSeg = Seg2EPVMSeg(seg);
  AVERT(EPVMSeg, epvmSeg);

  grainSize = PoolAlignment(EPVM2Pool(EPVMSegEPVM(epvmSeg)));
  buffer = SegBuffer(seg);
  if (buffer != NULL) {
    scanLimit = AMS_ADDR_INDEX(seg, BufferScanLimit(buffer));
    limit = AMS_ADDR_INDEX(seg, BufferLimit(buffer));
  }
  while (i < epvmSeg->amsSegStruct.grains) {
    if ((buffer != NULL) && (i == scanLimit) && (i < limit)) {
      /* skip buffer */
      i = AMS_ADDR_INDEX(seg, BufferLimit(buffer));
    } else { /* not in the buffer */
      AVER((buffer == NULL) || (i < scanLimit) || (i >= limit));
      if (AMS_ALLOCED(seg, i)) { /* there is an object here */
        Addr p = AMS_INDEX_ADDR(seg, i);

        res = (*f)(seg, i, p, AddrAdd(p, grainSize), closure);
        if (res != ResOK)
          return res;
      }
      ++i;
    }
  }
  AVER(i == epvmSeg->amsSegStruct.grains);
  return ResOK;
}


/* EPVMScan -- the pool class scanning method */

static Res EPVMScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  Res res;
  EPVM epvm;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  epvm = Pool2EPVM(pool);
  AVERT(EPVM, epvm);
  AVER(SegCheck(seg));
  /* Summaries must be univ, because we always compute them so. */
  AVER(SegSummary(seg) == RefSetUNIV);

  res = AMSScan(totalReturn, ss, pool, seg);
  /* See design.mps.poolepvm.protection.format. */
  ScanStateSetSummary(ss, RefSetUNIV);
  return res;
}


/* EPVMFix -- the pool class fixing method */

static Res EPVMFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AMSSeg amsseg;
  Index i;                      /* the index of the fixed grain */
  Ref ref;
  Bool alloced;                 /* is this grain allocated? */
  Bool isStringSeg;             /* is it a string segment? */

  AVERT_CRITICAL(Pool, pool);
  AVER_CRITICAL(CHECKT(EPVM, Pool2EPVM(pool)));
  UNUSED(pool); /* not used in hot varieties */
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(refIO != NULL);

  amsseg = Seg2AMSSeg(seg);
  AVERT_CRITICAL(EPVMSeg, Seg2EPVMSeg(seg)); /* checks amsseg too */
  /* It's a white seg, so it must have colour tables. */
  AVER_CRITICAL(amsseg->colourTablesInUse);

  ref = *refIO;
  i = AMS_ADDR_INDEX(seg, ref);
  alloced = AMS_ALLOCED(seg, i);
  isStringSeg = (SegRankSet(seg) == RankSetEMPTY);

  ss->wasMarked = TRUE;

  switch (ss->rank) {
  case RankAMBIG:
    break; /* all real references to PS VM are unambiguous */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    /* A real reference must be both allocated and aligned (except in */
    /* string segments, see design.mps.poolepvm.low.align). */
    AVER_CRITICAL(alloced);
    AVER_CRITICAL(isStringSeg
                  || AddrIsAligned((Addr)ref, PoolAlignment(pool)));
    if (AMSIsWhite(seg, i)) {
      ss->wasMarked = FALSE;
      if (ss->rank == RankWEAK) { /* then splat the reference */
        *refIO = (Ref)0;
      } else {
        ++ss->preservedInPlaceCount;
        if (isStringSeg) {
          /* turn it black (design.mps.poolepvm.arch.obj-string) */
          AMSWhiteBlacken(seg, i);
        } else {
          /* turn it grey */
          AMSWhiteGreyen(seg, i);
          /* turn this segment grey */
          SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
          /* mark it for scanning - design.mps.poolams.marked.fix */
          amsseg->marksChanged = TRUE;
        }
      }
    }
    break;
  default:
    NOTREACHED;
  }

  return ResOK;
}


/* EPVMDescribe -- the pool class description method
 * 
 * Iterates over the segments, describing all of them.  */

static Res EPVMDescribe(Pool pool, mps_lib_FILE *stream)
{
  EPVM epvm; AMS ams;
  Ring node, nextNode;
  Res res;
  Index i;

  if (!CHECKT(Pool, pool)) return ResFAIL;
  epvm = Pool2EPVM(pool);
  if (!CHECKT(EPVM, epvm)) return ResFAIL;
  ams = EPVM2AMS(epvm);
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "EPVM $P {\n", (WriteFP)epvm,
               "  pool $P ($U)\n",
               (WriteFP)pool, (WriteFU)pool->serial,
               "  size $W\n", (WriteFW)ams->size,
               "  grain shift $U\n", (WriteFU)ams->grainShift,
               "  save level $U (max $U)\n",
               (WriteFU)epvm->saveLevel, (WriteFU)epvm->maxSaveLevel,
               NULL);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  saves and segments\n"
               "    * = black, + = grey, - = white, . = alloc, ! = bad\n"
               "    buffers: [ = base, < = scan limit, | = init,\n"
               "             > = alloc, ] = limit\n",
               NULL);
  if (res != ResOK) return res;

  for(i = 0; i <= epvm->maxSaveLevel; ++i) {
    EPVMSave save = EPVMLevelSave(epvm, i);
    res = WriteF(stream,
                 "  level $U:\n", (WriteFU)save->level,
                 "    size $U\n", (WriteFU)save->size,
                 "    segments:\n",
                 NULL);
    if (res != ResOK) return res;
    RING_FOR(node, &save->segRing, nextNode) {
      EPVMSeg epvmSeg = RING_ELT(EPVMSeg, amsSegStruct.segRing, node);
      res = SegDescribe(EPVMSeg2Seg(epvmSeg), stream);
      if (res != ResOK) return res;
    }
  }

  res = WriteF(stream, "} EPVM $P\n", (WriteFP)epvm, NULL);
  return res;
}


/* EPVMPoolClass -- the pool class definition */

DEFINE_POOL_CLASS(EPVMPoolClass, this)
{
  INHERIT_CLASS(this, AMSPoolClass);
  this->name = "EPVM";
  this->size = sizeof(EPVMStruct);
  this->offset = offsetof(EPVMStruct, amsStruct.poolStruct);
  this->init = EPVMInit;
  this->finish = EPVMFinish;
  this->bufferClass = EPVMBufferClassGet;
  this->scan = EPVMScan;
  this->fix = EPVMFix;
  this->fixEmergency = EPVMFix;
  this->describe = EPVMDescribe;
}


/* EPVMCheck -- the check method for an EPVM */

static Bool EPVMCheck(EPVM epvm)
{
  Index i;
  Size size;

  CHECKS(EPVM, epvm);
  CHECKL(AMSCheck(EPVM2AMS(epvm)));
  CHECKL(EPVM2Pool(epvm)->class == EPVMPoolClassGet());
  CHECKL(epvm->saveLevel <= epvm->maxSaveLevel);
  /* subsequentSegRound is ArenaAligned too, but we can't get arena */
  CHECKL(epvm->subsequentSegRound > 0);
  CHECKL(epvm->saves != NULL);
  /* design.mps.poolepvm.low.size */
  size = 0;
  for(i = 0; i <= epvm->saveLevel; ++i) {
    EPVMSave save = EPVMLevelSave(epvm, i);
    CHECKD(EPVMSave, save);
    CHECKL(save->level == i);
    size += save->size;
  }
  CHECKL(size == EPVM2AMS(epvm)->size);

  /* Check that the interfaces are type compatible */
  CHECKL(CHECKTYPE(mps_epvm_save_level_t, Index));
  /* We assume all the common types have been checked in mpsi.c. */

  return TRUE;
}


/* mps_class_epvm -- return the pool class descriptor to the client */

mps_class_t mps_class_epvm(void)
{
  return (mps_class_t)EPVMPoolClassGet();
}


/* mps_epvm_check -- check if a pointer points to an EPVM pool
 * 
 * See design.mps.poolepvm.low.check.  */

mps_bool_t mps_epvm_check(mps_pool_t *mps_pool_o,
                          mps_epvm_save_level_t *mps_level_o,
                          mps_arena_t mps_arena,
                          mps_addr_t mps_addr)
{
  Pool pool;
  EPVMSeg epvmSeg;
  Seg seg;
  Bool b;
  Addr addr;
  Arena arena = (Arena)mps_arena;

  ArenaEnter(arena);
  
  AVER(mps_pool_o != NULL);
  AVER(mps_level_o != NULL);
  AVER(mps_addr != NULL);

  addr = (Addr)mps_addr;
  b = SegOfAddr(&seg, arena, addr);
  if (!b)
    goto returnFalse;
  
  pool = SegPool(seg);
  if (pool->class != EPVMPoolClassGet())
    goto returnFalse;

  epvmSeg = Seg2EPVMSeg(seg);
  AVERT(EPVMSeg, epvmSeg);
  AVER(AMS_ALLOCED(seg, AMS_ADDR_INDEX(seg, addr)));

  *mps_pool_o = (mps_pool_t)pool;
  *mps_level_o = (mps_epvm_save_level_t)epvmSeg->save->level;
  ArenaLeave(arena);
  return TRUE;

returnFalse:
  ArenaLeave(arena);
  return FALSE;
}


/* mps_epvm_save -- increment the save level */

void mps_epvm_save(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  EPVM epvm;
  EPVMSave save;
  Ring node, nextNode;
  Arena arena;

  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);
  ArenaEnter(arena);

  AVERT(Pool, pool);
  AVER(pool->class == EPVMPoolClassGet());
  epvm = Pool2EPVM(pool);
  AVERT(EPVM, epvm);
  AVER(epvm->saveLevel < epvm->maxSaveLevel);

  /* detach any buffers */
  save = EPVMCurrentSave(epvm);
  RING_FOR(node, &save->segRing, nextNode) {
    EPVMSeg epvmSeg = RING_ELT(EPVMSeg, amsSegStruct.segRing, node);
    Buffer buffer;

    AVERT_CRITICAL(EPVMSeg, epvmSeg);
    buffer = SegBuffer(EPVMSeg2Seg(epvmSeg));
    /* .buffer.single-thread: This relies on single-threaded access, */
    /* otherwise the buffer could be half-way through commit. */
    if (buffer != NULL)
      BufferDetach(buffer, pool);
  }

  ++epvm->saveLevel;

  EVENT_P(PoolPush, pool);
  ArenaLeave(arena);
}


/* mps_epvm_restore -- return to an earlier save level
 *
 * Discard the more recent levels.  */

void mps_epvm_restore(mps_pool_t mps_pool,
                      mps_epvm_save_level_t mps_level)
{
  Pool pool = (Pool)mps_pool;
  EPVM epvm;
  Arena arena;
  Index level = (Index)mps_level;
  Index i;

  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);
  ArenaEnter(arena);

  AVERT(Pool, pool);
  AVER(pool->class == EPVMPoolClassGet());
  epvm = Pool2EPVM(pool);
  AVERT(EPVM, epvm);
  AVER(level < epvm->saveLevel);

  i = level+1;
  while (i <= epvm->saveLevel) {
    epvmSaveDiscard(EPVMLevelSave(epvm, i));
    ++i;
  }
  epvm->saveLevel = level;

  EVENT_PU(PoolPop, pool, level);
  ArenaLeave(arena);
}


/* mps_epvm_size -- return the total segment size of the pool */

size_t mps_epvm_size(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  EPVM epvm;
  Arena arena;
  size_t size;

  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);
  ArenaEnter(arena);

  AVERT(Pool, pool);
  AVER(pool->class == EPVMPoolClassGet());
  epvm = Pool2EPVM(pool);
  AVERT(EPVM, epvm);
  size = EPVM2AMS(epvm)->size;

  ArenaLeave(arena);

  return size;
}


/* mps_epvm_free_size -- return the free space available in the pool */

size_t mps_epvm_free_size(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  EPVM epvm;
  Arena arena;
  Count free = 0;
  size_t res;
  Index i;

  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);
  ArenaEnter(arena);

  AVERT(Pool, pool);
  AVER(pool->class == EPVMPoolClassGet());
  epvm = Pool2EPVM(pool);
  AVERT(EPVM, epvm);

  for(i = 0; i <= epvm->saveLevel; ++i) {
    EPVMSave save = EPVMLevelSave(epvm, i);
    Ring node, nextNode;

    RING_FOR(node, &save->segRing, nextNode) {
      EPVMSeg epvmSeg;
      Buffer buffer;

      epvmSeg = RING_ELT(EPVMSeg, amsSegStruct.segRing, node);
      AVERT(EPVMSeg, epvmSeg);
      buffer = SegBuffer(EPVMSeg2Seg(epvmSeg));
      free += epvmSeg->amsSegStruct.free;
      if (buffer != NULL) {
        Addr alloc, limit;

        AVERT(Buffer,buffer);
        AVER(!BufferIsReset(buffer));

        alloc = BufferAlloc(buffer);
        limit = BufferLimit(buffer);
        if (alloc == limit)
          continue;
        free += AddrOffset(alloc, limit) >> EPVM2AMS(epvm)->grainShift;
      }
    }
  }
  res = (size_t)(free << EPVM2AMS(epvm)->grainShift);

  ArenaLeave(arena);

  return res;
}


/* mps_epvm_collect -- collect a pool
 *
 * This will do a complete collection of the pool in one go.  */

mps_res_t mps_epvm_collect(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  EPVM epvm;
  Arena arena;
  Trace trace;
  Res res;

  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);
  ArenaEnter(arena);

  AVERT(Pool, pool);
  AVER(pool->class == EPVMPoolClassGet());
  epvm = Pool2EPVM(pool);
  AVERT(EPVM, epvm);

  res = TraceCreate(&trace, arena);
  AVER(res == ResOK); /* Should be available, since nothing else starts them. */

  res = ChainCondemnAll(AMSChain(EPVM2AMS(epvm)), trace);
  if (res != ResOK)
    goto failBegin;

  TraceStart(trace, 0.0, 0.0);
  ArenaPark(ArenaGlobals(arena));
  ArenaRelease(ArenaGlobals(arena)); /* ArenaPark leaves the arena clamped */

  ArenaLeave(arena);
  return MPS_RES_OK;

failBegin:
  TraceDestroy(trace);
  ArenaLeave(arena);
  return res;
}
