/* impl.c.poolmrg
 * 
 * MANUAL RANK GUARDIAN POOL
 * 
 * $HopeName: MMsrc!poolmrg.c(MMdevel_drj_message.6) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 * 
 * DESIGN
 * 
 * .design: See design.mps.poolmrg.
 *
 * NOTES
 * 
 * .improve.rank: At the moment, the pool is a guardian for the final
 * rank.  It could be generalized to be a guardian for an arbitrary
 * rank (a guardian for RankEXACT would tell you if the object was
 * ambiguously referenced, for example).  The code that would need to be
 * modified bears this tag.
 *
 * .check-index: The number of guardians per segment is derived from
 * the pool.  As most of the code doesn't have access to the pool,
 * and knowledge of this derivation should be localised, the indexes 
 * are often unvalidated.
 */


#include "mpm.h"
#include "poolmrg.h"

SRCID(poolmrg, "$HopeName: MMsrc!poolmrg.c(MMdevel_drj_message.6) $");


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

/* enumerate the states of a Guardian */
enum {
  MRGGuardianFree,
  MRGGuardianPrefinal,
  MRGGuardianFinal,
  MRGGuardianPostfinal
};

typedef struct LinkStruct {
  int state;			 /* Free, Prefinal, Final, Postfinal */
  union LinkUnion {
    MessageStruct messageStruct; /* state = Final */
    RingStruct linkRing;	 /* state e {Free, Prefinal} */
  } the;
} LinkStruct;
typedef union LinkUnion LinkUnion;
typedef LinkStruct *Link;

#define linkOfMessage(message) \
  PARENT(LinkStruct, the, ((LinkUnion *)(message)))
#define linkOfRing(ring) \
  PARENT(LinkStruct, the, ((LinkUnion *)(ring)))


#define MRGGroupSig     ((Sig)0x5193699b) /* SIGnature MRG GrouP */

typedef struct MRGGroupStruct {
  Sig sig;                      /* impl.h.misc.sig */
  RingStruct group;             /* design.mps.poolmrg.group.group */
  Seg refSeg;                   /* design.mps.poolmrg.group.segs */
  Seg linkSeg;                  /* design.mps.poolmrg.group.segs */
} MRGGroupStruct;
typedef MRGGroupStruct *MRGGroup;


/* Forward declarations */

static MessageClassStruct MRGMessageClassStruct;
static PoolClassStruct PoolClassMRGStruct;
static Bool MRGGroupCheck(MRGGroup group);
static Bool MRGCheck(MRG mrg);


/* design.mps.poolmrg.guardian.assoc */

static Ref *refOfLink(Link link, Space space)
{
  Seg seg;
  Bool b;
  Link linkBase;
  Index index;
  MRGGroup group;
  Ref *refBase;

  AVER(link != NULL); /* Better checks done by SegOfAddr */
  AVERT(Space, space);

  b = SegOfAddr(&seg, space, (Addr)link);
  AVER(b);
  linkBase = (Link)SegBase(space, seg);
  index = link - linkBase; /* .check-index */

  AVER(SegPool(seg)->class == &PoolClassMRGStruct);
  group = (MRGGroup)SegP(seg);
  AVERT(MRGGroup, group);
  AVER(group->linkSeg == seg);

  refBase = (Ref *)SegBase(space, group->refSeg);
  return &refBase[index];
}

static Link linkOfRef(Ref *ref, Space space)
{
  Seg seg;
  Bool b;
  Ref *refBase;
  Index index;
  MRGGroup group;
  Link linkBase;

  AVER(ref != NULL); /* Better checks done by SegOfAddr */
  AVERT(Space, space);

  b = SegOfAddr(&seg, space, (Addr)ref);
  AVER(b);
  refBase = (Ref *)SegBase(space, seg);
  index = ref - refBase; /* .check-index */

  AVER(SegPool(seg)->class == &PoolClassMRGStruct);
  group = SegP(seg);
  AVERT(MRGGroup, group);
  AVER(group->refSeg == seg);

  linkBase = (Link)SegBase(space, group->linkSeg);
  return &linkBase[index];
}


static void MRGGroupDestroy(MRGGroup group, MRG mrg)
{
  Pool pool;

  AVERT(MRGGroup, group);
  AVERT(MRG, mrg);

  pool = MRGPool(mrg);
  RingRemove(&group->group);
  PoolSegFree(pool, group->refSeg);
  PoolSegFree(pool, group->linkSeg);
  SpaceFree(PoolSpace(pool), (Addr)group, (Size)sizeof(MRGGroupStruct));
}

static Res MRGGroupCreate(MRGGroup *groupReturn, MRG mrg)
{
  Ref *refBase;
  Count guardians;	/* guardians per seg */
  Index i;
  Link linkBase;
  MRGGroup group;
  Pool pool;
  Res res;
  Seg linkSeg;
  Seg refSeg;
  Size linkSegSize;
  Space space;
  void *v;

  AVER(groupReturn != NULL);
  AVERT(MRG, mrg);

  pool = MRGPool(mrg);
  space = PoolSpace(pool);
  res = SpaceAlloc(&v, space, (Size)sizeof(MRGGroupStruct));
  if(res != ResOK)
    goto failSpaceAlloc;

  group = v;
  res = PoolSegAlloc(&refSeg, SegPrefDefault(), pool, mrg->extendBy);
  if(res != ResOK)
    goto failRefSegAlloc;

  guardians = mrg->extendBy / sizeof(Ref);     /* per seg */
  linkSegSize = guardians * sizeof(LinkStruct);
  linkSegSize = SizeAlignUp(linkSegSize, ArenaAlign(space));
  res = PoolSegAlloc(&linkSeg, SegPrefDefault(), pool, linkSegSize);
  if(res != ResOK)
    goto failLinkSegAlloc;

  /* Link Segment is coerced to an array of LinkStructs, */
  /* each one is appended to the free Ring using the linkRing. */
  /* The ref part of each guardian is cleared. */

  AVER(guardians > 0);
  linkBase = (Link)SegBase(space, linkSeg);
  refBase = (Ref *)SegBase(space, refSeg);

  for(i=0; i<guardians; ++i) {
    RingInit(&linkBase[i].the.linkRing);
    linkBase[i].state = MRGGuardianFree;
    RingAppend(&mrg->free, &linkBase[i].the.linkRing);
    refBase[i] = 0;
  }
  AVER((Addr)(&linkBase[i]) <= SegLimit(space, linkSeg));
  AVER((Addr)(&refBase[i]) <= SegLimit(space, refSeg));
  /* design.mps.seg.field.rankSet.start */
  SegSetRankSet(refSeg, RankSetSingle(RankFINAL));
  /* design.mps.seg.field.summary.start */
  SegSetSummary(refSeg, RefSetUNIV);

  group->refSeg = refSeg;
  group->linkSeg = linkSeg;
  SegSetP(refSeg, group);
  SegSetP(linkSeg, group);
  RingInit(&group->group);
  RingAppend(&mrg->group, &group->group);
  group->sig = MRGGroupSig;

  return ResOK;

failLinkSegAlloc:
  PoolSegFree(pool, refSeg);
failRefSegAlloc:
  SpaceFree(space, (Addr)group, (Size)sizeof(MRGGroupStruct)); 
failSpaceAlloc:
  AVER(res != ResOK);
  return res;
}

/* finalize the indexth guardian in the group */
static void MRGFinalize(Space space, MRGGroup group, Index index)
{
  Link link, linkBase;
  Message message;

  AVERT(Space, space);
  AVERT(MRGGroup, group);
  /* .check-index */

  linkBase = (Link)SegBase(space, group->linkSeg);
  link = &linkBase[index];

  /* only finalize it if it hasn't been finalized already */
  if(link->state != MRGGuardianFinal) {
    AVER(link->state == MRGGuardianPrefinal);
    RingRemove(&link->the.linkRing);
    link->state = MRGGuardianFinal;
    message = &link->the.messageStruct;
    MessageInit(space, message, &MRGMessageClassStruct);
    MessagePost(space, message);
  }
}

static Res MRGGroupScan(ScanState ss, MRGGroup group, MRG mrg)
{
  Res res;
  Space space;
  Ref *refBase;
  Count guardians;
  Index i;

  AVERT(ScanState, ss);
  AVERT(MRGGroup, group);
  AVERT(MRG, mrg);

  space = PoolSpace(MRGPool(mrg));

  guardians = mrg->extendBy / sizeof(Ref);     /* per seg */
  AVER(guardians > 0);
  refBase = (Ref *)SegBase(space, group->refSeg);
  TRACE_SCAN_BEGIN(ss) {
    for(i=0; i<guardians; ++i) {
      if(!TRACE_FIX1(ss, refBase[i]))
        continue;
      res = TRACE_FIX2(ss, &refBase[i]);
      if(res != ResOK) 
        return res;
      
      if(ss->rank == RankFINAL && !ss->wasMarked) {     /* .improve.rank */
        MRGFinalize(space, group, i);
      }
    }
  } TRACE_SCAN_END(ss);

  return ResOK;
}


static Res MRGInit(Pool pool, va_list args)
{
  MRG mrg;
  
  AVER(pool != NULL); /* Can't check more; see pool contract @@@@ */
  UNUSED(args);
  
  mrg = PoolPoolMRG(pool);

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
  MRG mrg;
  MRGGroup junk;                /* .group.useless */
  Res res;
  Ring f;
  Space space;
  Link link;
  Ref *ref;

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size == sizeof(Ref));   /* design.mps.poolmrg.alloc.one-size */

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  space = PoolSpace(pool);

  f = RingNext(&mrg->free);

  /* design.mps.poolmrg.alloc.grow */

  if(f == &mrg->free) {                 /* (Ring has no elements) */
    res = MRGGroupCreate(&junk, mrg);   /* .group.useless: group isn't used */
    if(res != ResOK) 
      return res;
    
    f = RingNext(&mrg->free);
  }
  AVER(f != &mrg->free);

  link = linkOfRing(f);
  AVER(link->state == MRGGuardianFree);
  /* design.mps.poolmrg.alloc.pop */
  RingRemove(f);
  link->state = MRGGuardianPrefinal;
  RingAppend(&mrg->entry, f);

  /* design.mps.poolmrg.guardian.ref.alloc */
  ref = refOfLink(link, space);
  AVER(*ref == 0);
  *pReturn = (Addr)ref;

  return ResOK;
}

static void MRGFree(Pool pool, Addr old, Size size)
{
  MRG mrg;
  Space space;
  Seg seg;
  Bool b;
  Link link;
  Ref *ref;

  AVERT(Pool, pool);
  AVER(old != (Addr)0);
  AVER(size == sizeof(Ref));

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  ref = (Ref *)old;

  space = PoolSpace(pool);
  b = SegOfAddr(&seg, space, old);
  AVER(b);

  /* design.mps.poolmrg.guardian.ref.free */
  link = linkOfRef(ref, space);

  AVER(link->state == MRGGuardianPostfinal);
  RingInit(&link->the.linkRing);
  link->state = MRGGuardianFree;
  RingAppend(&mrg->free, &(link->the.linkRing));
  {
    RefSet sum = RefSetEMPTY;
    sum = SegSummary(seg);
    sum = RefSetAdd(space, sum, (Addr)0);
    TraceSetSummary(space, seg, sum);
    ShieldExpose(space, seg);
    *ref = 0;     /* design.mps.poolmrg.free.overwrite */
    ShieldCover(space, seg);
  }
}

/* Describe
 *
 * This could be improved by implementing MRGSegDescribe
 * and having MRGDescribe iterate over all the pool's segments.
 */
static Res MRGDescribe(Pool pool, mps_lib_FILE *stream)
{
  MRG mrg;
  Ring r;
  Space space;
  Ref *ref;

  AVERT(Pool, pool);
  /* Cannot check stream */

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  space = PoolSpace(pool);

  WriteF(stream, "  extendBy $W\n", mrg->extendBy, NULL);
  WriteF(stream, "  Entry queue:\n", NULL);
  RING_FOR(r, &mrg->entry) {
    ref = refOfLink(linkOfRing(r), space);
    WriteF(stream,
           "    at $A ref $A\n",
           (WriteFA)ref, (WriteFA)*ref,
           NULL);
  }

  return ResOK;
}

static Res MRGScan(ScanState ss, Pool pool, Seg seg)
{
  MRG mrg;
  Res res;
  MRGGroup group;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(SegRankSet(seg) == RankSetSingle(RankFINAL));
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);
  group = (MRGGroup)SegP(seg);
  AVER(seg == group->refSeg);

  res = MRGGroupScan(ss, group, mrg);
  if(res != ResOK) 
    return res;

  return ResOK;
}


/* Implementation of MRG's MessageClass */

/* deletes the message (frees up the memory) */
static void MRGMessageDelete(Message message)
{
  Ref *ref;
  Bool b;
  Space space;
  Link link;
  Seg seg; 

  AVERT(Message, message);

  space = MessageSpace(message);

  b = SegOfAddr(&seg, space, (Addr)message);
  AVER(b);
  AVER(SegPool(seg)->class == &PoolClassMRGStruct);

  link = linkOfMessage(message);
  MessageFinish(message);
  AVER(link->state == MRGGuardianFinal);
  link->state = MRGGuardianPostfinal;
  ref = refOfLink(link, space);
  PoolFree(SegPool(seg), (Addr)ref, sizeof(Ref));
}

static void MRGMessageFinalizationRef(Ref *refReturn,
				      Space space, Message message)
{
  Link link;
  Ref *ref;

  AVER(refReturn != NULL);
  AVERT(Space, space);
  AVERT(Message, message);

  AVER(message->type == MessageTypeFinalization);

  link = linkOfMessage(message);
  AVER(link->state == MRGGuardianFinal);
  ref = refOfLink(link, space);
  AVER(*ref != 0);
  *refReturn = *ref;
}


static MessageClassStruct MRGMessageClassStruct = {
  MessageClassSig,              /* sig */
  "MRGFinal",                   /* name */
  MRGMessageDelete,             /* Delete */
  MRGMessageFinalizationRef,	/* FinalizationRef */
  MessageClassSig               /* design.mps.message.class.sig.double */
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
  PoolNoTraceBegin,                     /* traceBegin */
  PoolNoCondemn,                        /* condemn */
  PoolTrivGrey,                         /* grey */
  MRGScan,                              /* scan */
  PoolNoFix,                            /* fix */
  PoolNoReclaim,                        /* reclaim */
  PoolNoTraceEnd,                       /* traceEnd */
  PoolNoBenefit,                        /* benefit */
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
  CHECKL(SegCheck(group->refSeg));
  CHECKL(SegCheck(group->linkSeg));
  return TRUE;
}
