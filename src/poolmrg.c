/* impl.c.poolmrg
 * 
 * MANUAL RANK GUARDIAN POOL
 * 
 * $HopeName: MMsrc!poolmrg.c(MMdevel_drj_message.2) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * Any MPS developer.  It will help to read design.mps.poolmrg.
 * 
 * DESIGN
 * 
 * See design.mps.poolmrg.
 * 
 * .improve.rank: At the moment, the pool is a guardian for the final
 * rank.  It could be generalized to be a guardian for an arbitrary
 * rank (a guardian for RankEXACT would tell you if the object was
 * ambiguously referenced, for example).  The code that would need to be
 * modified bears this tag.
 */


#include "mpm.h"
#include "poolmrg.h"

SRCID(poolmrg, "$HopeName: MMsrc!poolmrg.c(MMdevel_drj_message.2) $");


#define MRGSig          ((Sig)0x519369B0) /* SIGnature MRG POol */


/* Types */

typedef struct MRGStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  RingStruct entry;             /* design.mps.poolmrg.poolstruct.entry */
  RingStruct free;              /* design.mps.poolmrg.poolstruct.free */
  RingStruct group;             /* design.mps.poolmrg.poolstruct.group */
  Size extendBy;                /* design.mps.poolmrg.extend */
  Sig sig;                      /* impl.h.mps.sig */
} MRGStruct;

#define PoolPoolMRG(pool) PARENT(MRGStruct, poolStruct, pool)

static Pool MRGPool(MRG mrg);

typedef union LinkPartUnion {
  MessageStruct messageStruct;
  RingStruct linkRing;
} LinkPartUnion;

/* design.mps.poolmrg.guardian.assoc */
static Index indexOfRefPart(Addr a, Space space)
{
  Seg seg;
  Bool b;
  Addr base;
  Addr *pbase, *pa;

  b = SegOfAddr(&seg, space, a);
  AVER(b);
  base = SegBase(space, seg);
  pbase = (Addr *)base;
  pa = (Addr *)a;
  return pa - pbase;
}

/* design.mps.poolmrg.guardian.assoc */
static Index indexOfLinkPart(Addr a, Space space)
{
  Seg seg;
  Bool b;
  Addr base ;
  LinkPartUnion *pbase, *pa;

  b = SegOfAddr(&seg, space, a);
  AVER(b);
  base = SegBase(space, seg);
  pbase = (LinkPartUnion *)base;
  pa = (LinkPartUnion *)a;
  return pa - pbase;
}

#define MRGGroupSig     ((Sig)0x5193699b) /* SIGnature MRG GrouP */

typedef struct MRGGroupStruct {
  Sig sig;                      /* impl.h.misc.sig */
  RingStruct group;		/* design.mps.poolmrg.group.group */
  Seg refseg;                   /* design.mps.poolmrg.group.segs */
  Seg linkseg;                  /* design.mps.poolmrg.group.segs */
} MRGGroupStruct;
typedef MRGGroupStruct *MRGGroup;


/* Forward declarations */

static MessageClassStruct MRGMessageClassStruct;
static PoolClassStruct PoolClassMRGStruct;
static Bool MRGGroupCheck(MRGGroup group);
static Bool MRGCheck(MRG mrg);


static void MRGGroupDestroy(MRGGroup group, MRG mrg)
{
  Pool pool;

  pool = MRGPool(mrg);
  RingRemove(&group->group);
  PoolSegFree(pool, group->refseg);
  PoolSegFree(pool, group->linkseg);
  SpaceFree(PoolSpace(pool), (Addr)group, (Size)sizeof(MRGGroupStruct));
}

static Res MRGGroupCreate(MRGGroup *groupReturn, MRG mrg)
{
  Addr base;
  MRGGroup group;
  Pool pool;
  Res res;
  LinkPartUnion *linkpart;
  Seg linkseg;
  Seg refseg;
  Size linksegsize;
  Space space;
  Addr *refpart;
  Word i, guardians;
  void *v;

  pool = MRGPool(mrg);
  space = PoolSpace(pool);
  res = SpaceAlloc(&v, space, (Size)sizeof(MRGGroupStruct));
  if(res != ResOK)
    goto failSpaceAlloc;
  group = v;
  res = PoolSegAlloc(&refseg, SegPrefDefault(), pool, mrg->extendBy);
  if(res != ResOK)
    goto failRefSegAlloc;

  guardians = mrg->extendBy / sizeof(Addr);     /* per seg */
  linksegsize = guardians * sizeof(LinkPartUnion);
  linksegsize = SizeAlignUp(linksegsize, ArenaAlign(space));
  res = PoolSegAlloc(&linkseg, SegPrefDefault(), pool, linksegsize);
  if(res != ResOK)
    goto failLinkSegAlloc;

  /* Link Segment is coerced to an array of LinkPartUnions, */
  /* each one is appended to the free Ring using the linkRing. */
  /* The ref part of each guardian is cleared. */

  AVER(guardians > 0);
  base = SegBase(space, linkseg);
  linkpart = (LinkPartUnion *)base;
  refpart = (Addr *)SegBase(space, refseg);

  for(i=0; i<guardians; ++i) {
    RingInit(&linkpart[i].linkRing);
    RingAppend(&mrg->free, &linkpart[i].linkRing);
    refpart[i] = 0;
  }
  AVER((Addr)(&linkpart[i]) <= SegLimit(space, linkseg));
  AVER((Addr)(&refpart[i]) <= SegLimit(space, refseg));
  SegSetRankSet(refseg, RankSetSingle(RankFINAL)); /* design.mps.seg.field.rankSet.start */
  SegSetSummary(refseg, RefSetUNIV);               /* design.mps.seg.field.summary.start */

  group->refseg = refseg;
  group->linkseg = linkseg;
  SegSetP(refseg, group);
  SegSetP(linkseg, group);
  RingInit(&group->group);
  RingAppend(&mrg->group, &group->group);
  group->sig = MRGGroupSig;

  return ResOK;

failLinkSegAlloc:
  PoolSegFree(pool, refseg);
failRefSegAlloc:
  SpaceFree(space, (Addr)group, (Size)sizeof(MRGGroupStruct)); 
failSpaceAlloc:
  return res;
}

/* finalize the indexth guardian in the group */
static void MRGFinalize(Space space, MRGGroup group, Word index)
{
  LinkPartUnion *linkpart;
  Message message;

  linkpart = (LinkPartUnion *)SegBase(space, group->linkseg);
  RingRemove(&linkpart[index].linkRing);
  message = &linkpart[index].messageStruct;
  MessageInit(space, message, &MRGMessageClassStruct);
  MessagePost(space, message);

  return;
}

static Res MRGGroupScan(ScanState ss, MRGGroup group, MRG mrg)
{
  Addr base;
  Res res;
  Space space;
  Addr *refpart;
  Word guardians, i;

  space = PoolSpace(MRGPool(mrg));

  guardians = mrg->extendBy / sizeof(Addr);	/* per seg */
  AVER(guardians > 0);
  base = SegBase(space, group->refseg);
  refpart = (Addr *)base;
  TRACE_SCAN_BEGIN(ss) {
    for(i=0; i<guardians; ++i) {
      if(!TRACE_FIX1(ss, refpart[i]))
	continue;
      res = TRACE_FIX2(ss, &refpart[i]);
      if(res != ResOK) {
        return res;
      }
      if(ss->rank == RankFINAL && !ss->wasMarked) {     /* .improve.rank */
	MRGFinalize(space, group, i);
      }
    }
  } TRACE_SCAN_END(ss);

  return ResOK;
}


static Res MRGInit(Pool pool, va_list args)
{
  MRG mrg = PoolPoolMRG(pool);

  UNUSED(args);
  
  RingInit(&mrg->entry);
  RingInit(&mrg->free);
  RingInit(&mrg->group);
  mrg->extendBy = ArenaAlign(PoolSpace(pool));
  mrg->sig = MRGSig;

  AVERT(MRG, mrg);

  return ResOK;
}

static void MRGFinish(Pool pool)
{
  MRG mrg;
  Ring node;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  node = RingNext(&mrg->group);
  while(node != &mrg->group) {
    Ring next = RingNext(node);
    MRGGroup group = RING_ELT(MRGGroup, group, node);
    MRGGroupDestroy(group, mrg);

    node = next;
  }

  mrg->sig = SigInvalid;
}

static Pool MRGPool(MRG mrg)
{
  AVERT(MRG, mrg);
  return &mrg->poolStruct;
}

static Res MRGAlloc(Addr *pReturn, Pool pool, Size size)
{
  Addr *refpart;
  Bool b;
  Index gi;
  MRG mrg;
  MRGGroup group;
  MRGGroup junk;                /* .group.useless */
  Res res;
  Ring f;
  Seg seg;
  Space space;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(pReturn != NULL);
  AVER(size == sizeof(Addr));   /* design.mps.poolmrg.alloc.one-size */

  space = PoolSpace(pool);

  f = RingNext(&mrg->free);

  /* design.mps.poolmrg.alloc.grow */

  if(f == &mrg->free) {                 /* (Ring has no elements) */
    res = MRGGroupCreate(&junk, mrg);   /* .group.useless: group isn't used */
    if(res != ResOK) {
      return res;
    }
    f = RingNext(&mrg->free);
  }
  AVER(f != &mrg->free);

  /* design.mps.poolmrg.alloc.pop */
  RingRemove(f);
  RingAppend(&mrg->entry, f);
  /* ISO C clause 6.5.2.1:  */
  /* "A pointer to a union object, suitably converted, points to */
  /* each of its members, and vice-versa" */
  gi = indexOfLinkPart((Addr)(LinkPartUnion *)f, space);
  b = SegOfAddr(&seg, space, (Addr)f);
  AVER(b);
  group = SegP(seg);
  refpart = (Addr *)SegBase(space, group->refseg);

  /* design.mps.poolmrg.guardian.ref.alloc */
  *pReturn = (Addr)(&refpart[gi]);
  return ResOK;
}

static void MRGFree(Pool pool, Addr old, Size size)
{
  MRG mrg;
  Index gi;
  Space space;
  Seg seg;
  MRGGroup group;
  Bool b;
  LinkPartUnion *linkpart;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(old != (Addr)0);
  AVER(size == sizeof(Addr));

  space = PoolSpace(pool);
  b = SegOfAddr(&seg, space, old);
  AVER(b);
  group = SegP(seg);
  linkpart = (LinkPartUnion *)SegBase(space, group->linkseg);

  /* design.mps.poolmrg.guardian.ref.free */
  gi = indexOfRefPart(old, space);

  RingInit(&linkpart[gi].linkRing);
  RingAppend(&mrg->free, &linkpart[gi].linkRing);
  {
    RefSet sum = RefSetEMPTY;
    sum = SegSummary(seg);
    sum = RefSetAdd(space, sum, (Addr)0);
    TraceSetSummary(space, seg, sum);
    ShieldExpose(space, seg);
    *(Addr *)old = (Addr)0;     /* design.mps.poolmrg.free.overwrite */
    ShieldCover(space, seg);
  }
}

static Res MRGDescribe(Pool pool, mps_lib_FILE *stream)
{
  MRG mrg;
  Ring r;
  Space space;
  Bool b;
  MRGGroup group;
  Index gi;
  Seg seg;
  Addr *refpart;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);
  /* Cannot check stream */

  space = PoolSpace(pool);

  WriteF(stream, "  extendBy $W\n", mrg->extendBy, NULL);
  WriteF(stream, "  Entry queue:\n", NULL);
  RING_FOR(r, &mrg->entry) {
    b = SegOfAddr(&seg, space, (Addr)r);
    AVER(b);
    group = SegP(seg);
    refpart = (Addr *)SegBase(space, group->refseg);
    gi = indexOfLinkPart((Addr)r, space);
    WriteF(stream,
           "    at $A ref $A\n",
           (WriteFA)&refpart[gi], (WriteFA)refpart[gi],
           NULL);
  }
#if 0
  WriteF(stream, "  Exit queue:\n", NULL);
  RING_FOR(r, &mrg->exit) {
    b = SegOfAddr(&seg, space, (Addr)r);
    AVER(b);
    group = SegP(seg);
    refpart = (Addr *)SegBase(space, group->refseg);
    gi = indexOfLinkPart((Addr)r, space);
    WriteF(stream,
           "    at $A ref $A\n",
           (WriteFA)&refpart[gi], (WriteFA)refpart[gi],
           NULL);
  }
#endif

  return ResOK;
}

static Res MRGScan(ScanState ss, Pool pool, Seg seg)
{
  MRG mrg;
  Res res;
  MRGGroup group;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);
  AVERT(Seg, seg);

  AVER(SegRankSet(seg) == RankSetSingle(RankFINAL));
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);
  group = (MRGGroup)SegP(seg);
  AVER(seg == group->refseg);

  res = MRGGroupScan(ss, group, mrg);
  if(res != ResOK) return res;

  return ResOK;
}


/* Implementation of MRG's MessageClass */


/* returns the type of a message (this is always MessageTypeFinalization) */
static MessageType MRGMessageType(Message message)
{
  Seg seg;
  Bool b;
  Space space;

  AVERT(Message, message);
  AVER(MessageGetClass(message) == &MRGMessageClassStruct);
  space = MessageSpace(message);
  b = SegOfAddr(&seg, space, (Addr)message);
  AVER(b);
  AVER(SegPool(seg)->class == &PoolClassMRGStruct);

  return MessageTypeFinalization;
}

/* copies the message */
static void MRGMessageDeliver(Message message, void *buffer, size_t length)
{
  Addr *refpart;
  Bool b;
  Index index;
  MRGGroup group;
  MessageFinalization finalizationMessage;
  Seg seg;
  Space space;

  AVERT(Message, message);
  AVER(MessageGetClass(message) == &MRGMessageClassStruct);
  AVER(buffer != NULL);
  AVER(length == sizeof(MessageFinalizationStruct));

  space = MessageSpace(message);
  b = SegOfAddr(&seg, space, (Addr)(LinkPartUnion *)message);
  AVER(b);
  AVER(SegPool(seg)->class == &PoolClassMRGStruct);
  group = (MRGGroup)SegP(seg);

  index = indexOfLinkPart((Addr)(LinkPartUnion *)message, space);
  refpart = (Addr *)SegBase(space, group->refseg);

  finalizationMessage = buffer;
  finalizationMessage->type = MessageTypeFinalization;
  finalizationMessage->ref = refpart[index];

  return;
}

/* deletes the message (frees up the memory) */
static void MRGMessageDelete(Message message)
{
  Addr *refpart;
  Seg seg;
  Bool b;
  Space space;
  Index index;
  MRGGroup group;

  AVERT(Message, message);

  space = MessageSpace(message);
  b = SegOfAddr(&seg, space, (Addr)message);
  AVER(b);
  AVER(SegPool(seg)->class == &PoolClassMRGStruct);
  group = (MRGGroup)SegP(seg);
  AVERT(MRGGroup, group);
  MessageFinish(message);

  index = indexOfLinkPart((Addr)(LinkPartUnion *)message, space);
  refpart = (Addr *)SegBase(space, group->refseg);

  PoolFree(SegPool(seg), (Addr)&refpart[index], sizeof(Addr));
}


static MessageClassStruct MRGMessageClassStruct = {
  MessageClassSig,			/* sig */
  "MRGFinal",				/* name */
  MRGMessageType,			/* Type */
  MRGMessageDeliver,			/* Deliver */
  MRGMessageDelete,			/* Delete */
  MessageClassSig			/* endSig */
};

static PoolClassStruct PoolClassMRGStruct = {
  PoolClassSig,                         /* sig */
  "MRG",                                /* name */
  sizeof(MRGStruct),                    /* size */
  offsetof(MRGStruct, poolStruct),      /* offset */
  AttrSCAN | AttrALLOC | AttrFREE | AttrINCR_RB,
  MRGInit,                              /* init */
  MRGFinish,                            /* finish */
  MRGAlloc,                             /* alloc */
  MRGFree,                              /* free */
  PoolNoBufferInit,                     /* bufferInit */
  PoolNoBufferFill,                     /* bufferFill */
  PoolNoBufferEmpty,                    /* bufferEmpty */
  PoolNoBufferFinish,                   /* bufferFinish */
  PoolNoTraceBegin,			/* traceBegin */
  PoolNoCondemn,                        /* condemn */
  PoolTrivGrey,                         /* grey */
  MRGScan,                              /* scan */
  PoolNoFix,                            /* fix */
  PoolNoReclaim,                        /* reclaim */
  PoolNoTraceEnd,			/* traceEnd */
  PoolNoBenefit,			/* benefit */
  MRGDescribe,                          /* describe */
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};

PoolClass PoolClassMRG(void)
{
  return &PoolClassMRGStruct;
}

/* .check.norecurse: the expression &mrg->poolStruct is used instead of
 * the more natural MRGPool(mrg).  The latter results in infinite
 * recursion because MRGPool calls MRGCheck.
 */
static Bool MRGCheck(MRG mrg)
{
  Space space;

  CHECKS(MRG, mrg);
  CHECKD(Pool, &mrg->poolStruct);
  CHECKL(mrg->poolStruct.class == &PoolClassMRGStruct);
  CHECKL(RingCheck(&mrg->entry));
  CHECKL(RingCheck(&mrg->free));
  CHECKL(RingCheck(&mrg->group));
  space = PoolSpace(&mrg->poolStruct);  /* .check.norecurse */
  CHECKL(mrg->extendBy == ArenaAlign(space));
  return TRUE;
}


static Bool MRGGroupCheck(MRGGroup group)
{
  CHECKS(MRGGroup, group);
  CHECKL(RingCheck(&group->group));
  CHECKL(SegCheck(group->refseg));
  CHECKL(SegCheck(group->linkseg));
  return TRUE;
}
