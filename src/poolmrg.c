/* impl.c.poolmrg
 * 
 * MANUAL RANK GUARDIAN POOL
 * 
 * $HopeName: MMsrc!poolmrg.c(MMdevel_drj_message.9) $
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
 */


#include "mpm.h"
#include "poolmrg.h"

SRCID(poolmrg, "$HopeName: MMsrc!poolmrg.c(MMdevel_drj_message.9) $");


/* Types */

/* enumerate the states of a Guardian */
enum {
  MRGGuardianFREE,
  MRGGuardianPREFINAL,
  MRGGuardianFINAL,
  MRGGuardianPOSTFINAL
};

/* structure of objects within pool */
typedef struct LinkStruct *Link;
typedef struct LinkStruct {
  int state;                     /* Free, Prefinal, Final, Postfinal */
  union {
    MessageStruct messageStruct; /* state = Final */
    RingStruct linkRing;         /* state e {Free, Prefinal} */
  } the;
} LinkStruct;

#define linkOfMessage(message) \
  PARENT(LinkStruct, the.messageStruct, (message))

#define linkOfRing(ring) \
  PARENT(LinkStruct, the.linkRing, (ring))


#define MRGSig          ((Sig)0x519369B0) /* SIGnature MRG POol */

typedef struct MRGStruct {
  PoolStruct poolStruct;       /* generic pool structure */
  RingStruct entryRing;        /* design.mps.poolmrg.poolstruct.entry */
  RingStruct freeRing;         /* design.mps.poolmrg.poolstruct.free */
  RingStruct groupRing;        /* design.mps.poolmrg.poolstruct.group */
  Size extendBy;               /* design.mps.poolmrg.extend */
  Sig sig;                     /* impl.h.mps.sig */
} MRGStruct;

#define PoolPoolMRG(pool) PARENT(MRGStruct, poolStruct, pool)


#define MRGGroupSig     ((Sig)0x5193699b) /* SIGnature MRG GrouP */

typedef struct MRGGroupStruct {
  Sig sig;                      /* impl.h.misc.sig */
  RingStruct mrgRing;           /* design.mps.poolmrg.group.group */
  Seg refSeg;                   /* design.mps.poolmrg.group.segs */
  Seg linkSeg;                  /* design.mps.poolmrg.group.segs */
} MRGGroupStruct;
typedef MRGGroupStruct *MRGGroup;

static Bool MRGGroupCheck(MRGGroup group)
{
  CHECKS(MRGGroup, group);
  CHECKL(RingCheck(&group->mrgRing));
  CHECKL(SegCheck(group->refSeg));
  CHECKL(SegCheck(group->linkSeg));
  return TRUE;
}

/* .check.norecurse: the expression &mrg->poolStruct is used instead */
/* of the more natural MRGPool(mrg).  The latter results in infinite */
/* recursion because MRGPool calls MRGCheck. */

static Bool MRGCheck(MRG mrg)
{
  Space space;

  CHECKS(MRG, mrg);
  CHECKD(Pool, &mrg->poolStruct);
  CHECKL(mrg->poolStruct.class == PoolClassMRG());
  CHECKL(RingCheck(&mrg->entryRing));
  CHECKL(RingCheck(&mrg->freeRing));
  CHECKL(RingCheck(&mrg->groupRing));
  space = PoolSpace(&mrg->poolStruct);  /* .check.norecurse */
  CHECKL(mrg->extendBy == ArenaAlign(space));
  return TRUE;
}


static Count MRGGuardiansPerSeg(MRG mrg)
{
  Count nGuardians;
  AVERT(MRG, mrg);

  nGuardians = mrg->extendBy / sizeof(Ref);
  AVER(nGuardians > 0);

  return(nGuardians);
}

/* design.mps.poolmrg.guardian.assoc */

#define refOfIndex(group, index) \
  ((Ref *)SegBase((group)->refSeg) + (index))

static Ref *MRGRefOfLink(Link link, Space space)
{
  Seg seg;
  Bool b;
  Link linkBase;
  Index index;
  MRGGroup group;

  AVER(link != NULL); /* Better checks done by SegOfAddr */
  AVERT(Space, space);

  b = SegOfAddr(&seg, space, (Addr)link);
  AVER(b);
  linkBase = (Link)SegBase(seg);
  AVER(link >= linkBase);
  index = link - linkBase; 
  AVER(index < MRGGuardiansPerSeg(PoolPoolMRG(SegPool(seg))));

  AVER(SegPool(seg)->class == PoolClassMRG());
  group = (MRGGroup)SegP(seg);
  AVERT(MRGGroup, group);
  AVER(group->linkSeg == seg);

  return refOfIndex(group, index);
}

#define linkOfIndex(group, index) \
  ((Link)SegBase((group)->linkSeg) + (index))

static Link MRGLinkOfRef(Ref *ref, Space space)
{
  Seg seg;
  Bool b;
  Ref *refBase;
  Index index;
  MRGGroup group;

  AVER(ref != NULL); /* Better checks done by SegOfAddr */
  AVERT(Space, space);

  b = SegOfAddr(&seg, space, (Addr)ref);
  AVER(b);
  refBase = (Ref *)SegBase(seg);
  AVER(ref >= refBase);
  index = ref - refBase; 
  AVER(index < MRGGuardiansPerSeg(PoolPoolMRG(SegPool(seg))));

  AVER(SegPool(seg)->class == PoolClassMRG());
  group = SegP(seg);
  AVERT(MRGGroup, group);
  AVER(group->refSeg == seg);

  return linkOfIndex(group, index);
}

static void MRGGuardianInit(MRG mrg, Link link, Ref *ref) 
{
  AVERT(MRG, mrg);
  AVER(link != NULL);
  AVER(ref != NULL);

  RingInit(&link->the.linkRing);
  link->state = MRGGuardianFREE;
  RingAppend(&mrg->freeRing, &link->the.linkRing);
  *ref = 0; /* design.mps.poolmrg.free.overwrite */
}


/* MRGMessage* -- Implementation of MRG's MessageClass 
 */

/* deletes the message (frees up the memory) */
static void MRGMessageDelete(Message message)
{
  Ref *ref;
  Pool pool;
  Space space;
  Link link;

  AVERT(Message, message);

  space = MessageSpace(message);

  /* Calculate pool */
  {
    Bool b;
    Seg seg; 
    b = SegOfAddr(&seg, space, (Addr)message);
    AVER(b);

    pool = SegPool(seg);
  }
  AVER(pool->class == PoolClassMRG());

  link = linkOfMessage(message);
  MessageFinish(message);
  AVER(link->state == MRGGuardianFINAL);
  link->state = MRGGuardianPOSTFINAL;
  ref = MRGRefOfLink(link, space);
  PoolFree(pool, (Addr)ref, sizeof(Ref));
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
  AVER(link->state == MRGGuardianFINAL);
  ref = MRGRefOfLink(link, space);
  AVER(*ref != 0);
  *refReturn = *ref;
}

static MessageClassStruct MRGMessageClassStruct = {
  MessageClassSig,             /* sig */
  "MRGFinal",                  /* name */
  MRGMessageDelete,            /* Delete */
  MRGMessageFinalizationRef,   /* FinalizationRef */
  MessageClassSig              /* design.mps.message.class.sig.double */
};


static Pool MRGPool(MRG mrg)
{
  AVERT(MRG, mrg);
  return &mrg->poolStruct;
}

static void MRGGroupDestroy(MRGGroup group, MRG mrg)
{
  Pool pool;

  AVERT(MRGGroup, group);
  AVERT(MRG, mrg);

  pool = MRGPool(mrg);
  RingRemove(&group->mrgRing);
  RingFinish(&group->mrgRing);
  group->sig = SigInvalid;
  PoolSegFree(pool, group->refSeg);
  PoolSegFree(pool, group->linkSeg);
  SpaceFree(PoolSpace(pool), (Addr)group, (Size)sizeof(MRGGroupStruct));
}

static Res MRGGroupCreate(MRGGroup *groupReturn, MRG mrg)
{
  Ref *refBase;
  Count nGuardians;       /* guardians per seg */
  Index i;
  Link linkBase;
  MRGGroup group;
  Pool pool;
  Res res;
  Seg linkSeg;
  Seg refSeg;
  Size linkSegSize;
  Space space;
  void *p;

  AVER(groupReturn != NULL);
  AVERT(MRG, mrg);

  pool = MRGPool(mrg);
  space = PoolSpace(pool);
  res = SpaceAlloc(&p, space, (Size)sizeof(MRGGroupStruct));
  if(res != ResOK)
    goto failSpaceAlloc;
  group = p;

  res = PoolSegAlloc(&refSeg, SegPrefDefault(), pool, mrg->extendBy);
  if(res != ResOK)
    goto failRefSegAlloc;

  nGuardians = MRGGuardiansPerSeg(mrg); 
  linkSegSize = nGuardians * sizeof(LinkStruct);
  linkSegSize = SizeAlignUp(linkSegSize, ArenaAlign(space));
  res = PoolSegAlloc(&linkSeg, SegPrefDefault(), pool, linkSegSize);
  if(res != ResOK)
    goto failLinkSegAlloc;

  /* Link Segment is coerced to an array of LinkStructs, */
  /* each one is appended to the free Ring using the linkRing. */
  /* The ref part of each guardian is cleared. */

  linkBase = (Link)SegBase(linkSeg);
  refBase = (Ref *)SegBase(refSeg);

  for(i = 0; i < nGuardians; ++i) {
    MRGGuardianInit(mrg, linkBase + i, refBase + i);
  }
  AVER((Addr)(&linkBase[i]) <= SegLimit(linkSeg));
  AVER((Addr)(&refBase[i]) <= SegLimit(refSeg));
  /* design.mps.seg.field.rankSet.start */
  SegSetRankSet(refSeg, RankSetSingle(RankFINAL)); /* .improve.rank */
  /* design.mps.seg.field.summary.start */
  SegSetSummary(refSeg, RefSetUNIV);

  group->refSeg = refSeg;
  group->linkSeg = linkSeg;
  SegSetP(refSeg, group);
  SegSetP(linkSeg, group);
  RingInit(&group->mrgRing);
  RingAppend(&mrg->groupRing, &group->mrgRing);
  group->sig = MRGGroupSig;
  AVERT(MRGGroup, group);
  *groupReturn = group;

  return ResOK;

failLinkSegAlloc:
  PoolSegFree(pool, refSeg);
failRefSegAlloc:
  SpaceFree(space, (Addr)group, (Size)sizeof(MRGGroupStruct)); 
failSpaceAlloc:
  AVER(res != ResOK);
  return res;
}


/* MRGFinalise -- finalize the indexth guardian in the group 
 */
static void MRGFinalize(Space space, MRGGroup group, Index index)
{
  Link link;
  Message message;

  AVERT(Space, space);
  AVERT(MRGGroup, group);
  AVER(index < MRGGuardiansPerSeg(PoolPoolMRG(SegPool(group->refSeg))));

  link = linkOfIndex(group, index);

  /* only finalize it if it hasn't been finalized already */
  if(link->state != MRGGuardianFINAL) {
    AVER(link->state == MRGGuardianPREFINAL);
    RingRemove(&link->the.linkRing);
    RingFinish(&link->the.linkRing);
    link->state = MRGGuardianFINAL;
    message = &link->the.messageStruct;
    MessageInit(space, message, &MRGMessageClassStruct);
    MessagePost(space, message);
  }
}

static Res MRGGroupScan(ScanState ss, MRGGroup group, MRG mrg)
{
  Res res;
  Space space;
  Ref *ref    ;
  Count nGuardians;
  Index i;

  AVERT(ScanState, ss);
  AVERT(MRGGroup, group);
  AVERT(MRG, mrg);

  space = PoolSpace(MRGPool(mrg));

  nGuardians = MRGGuardiansPerSeg(mrg); 
  AVER(nGuardians > 0);
  TRACE_SCAN_BEGIN(ss) {
    for(i=0; i < nGuardians; ++i) {
      ref = refOfIndex(group, i);
      if(TRACE_FIX1(ss, *ref)) {
        res = TRACE_FIX2(ss, ref);
        if(res != ResOK) 
          return res;
      
        if(ss->rank == RankFINAL && !ss->wasMarked) { /* .improve.rank */
          MRGFinalize(space, group, i);
	}
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

  RingInit(&mrg->entryRing);
  RingInit(&mrg->freeRing);
  RingInit(&mrg->groupRing);
  mrg->extendBy = ArenaAlign(PoolSpace(pool));
  mrg->sig = MRGSig;

  AVERT(MRG, mrg);

  return ResOK;
}

static void MRGFinish(Pool pool)
{
  MRG mrg;
  Ring node, nextNode;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  RING_FOR(node, &mrg->groupRing, nextNode) {
    MRGGroup group = RING_ELT(MRGGroup, mrgRing, node);
    MRGGroupDestroy(group, mrg);
  }
  mrg->sig = SigInvalid;
  RingFinish(&mrg->groupRing);
  /* @@@@ Don't finish entryRing or freeRing */
}

static Res MRGAlloc(Addr *pReturn, Pool pool, Size size)
{
  MRG mrg;
  MRGGroup junk;                /* .group.useless */
  Res res;
  Ring freeNode;
  Space space;
  Link link;
  Ref *ref;

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size == sizeof(Ref));   /* design.mps.poolmrg.alloc.one-size */

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  space = PoolSpace(pool);

  /* design.mps.poolmrg.alloc.grow */
  if(RingIsSingle(&mrg->freeRing)) {
    /* .group.useless: group isn't used */
    res = MRGGroupCreate(&junk, mrg);   
    if(res != ResOK) 
      return res;
  }
  AVER(!RingIsSingle(&mrg->freeRing));
  freeNode = RingNext(&mrg->freeRing);

  link = linkOfRing(freeNode);
  AVER(link->state == MRGGuardianFREE);
  /* design.mps.poolmrg.alloc.pop */
  RingRemove(freeNode);
  link->state = MRGGuardianPREFINAL;
  RingAppend(&mrg->entryRing, freeNode);

  /* design.mps.poolmrg.guardian.ref.alloc */
  ref = MRGRefOfLink(link, space);
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
  link = MRGLinkOfRef(ref, space);

  AVER(link->state == MRGGuardianPOSTFINAL);
  MRGGuardianInit(mrg, link, ref);
  {
    RefSet sum = SegSummary(seg);
    sum = RefSetAdd(space, sum, (Addr)0);
    TraceSetSummary(space, seg, sum);
    ShieldExpose(space, seg);
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
  Ring node, nextNode;
  Space space;
  Ref *ref;

  AVERT(Pool, pool);
  /* Cannot check stream */

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  space = PoolSpace(pool);

  WriteF(stream, "  extendBy $W\n", mrg->extendBy, NULL);
  WriteF(stream, "  Entry queue:\n", NULL);
  RING_FOR(node, &mrg->entryRing, nextNode) {
    ref = MRGRefOfLink(linkOfRing(node), space);
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

  AVER(SegRankSet(seg) == RankSetSingle(RankFINAL)); /* .improve.rank */
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);
  group = (MRGGroup)SegP(seg);
  AVER(seg == group->refSeg);

  res = MRGGroupScan(ss, group, mrg);
  if(res != ResOK) 
    return res;

  return ResOK;
}


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

