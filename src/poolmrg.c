/* impl.c.poolmrg
 * 
 * MANUAL RANK GUARDIAN POOL
 * 
 * $HopeName: MMsrc!poolmrg.c(MMdevel_drj_message.5) $
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

SRCID(poolmrg, "$HopeName: MMsrc!poolmrg.c(MMdevel_drj_message.5) $");


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

typedef struct LinkPartStruct {
  int state;				/* Free, Prefinal, Final */
  union LinkPartUnion {
    MessageStruct messageStruct;	/* state = Final */
    RingStruct linkRing;		/* state e {Free, Prefinal} */
  } the;
} LinkPartStruct;
typedef union LinkPartUnion LinkPartUnion;

#define LinkPartOfMessage(message) \
  PARENT(LinkPartStruct, the, ((LinkPartUnion *)(message)))
#define LinkPartOfRing(ring) \
  PARENT(LinkPartStruct, the, ((LinkPartUnion *)(ring)))

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
static Index indexOfLinkPart(LinkPartStruct *linkpart, Space space)
{
  Seg seg;
  Bool b;
  Addr base;
  LinkPartStruct *baselinkpart;

  b = SegOfAddr(&seg, space, (Addr)linkpart);
  AVER(b);
  base = SegBase(space, seg);
  baselinkpart = (LinkPartStruct *)base;
  return linkpart - baselinkpart;
}

#define MRGGroupSig     ((Sig)0x5193699b) /* SIGnature MRG GrouP */

typedef struct MRGGroupStruct {
  Sig sig;                      /* impl.h.misc.sig */
  RingStruct group;             /* design.mps.poolmrg.group.group */
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
  Addr *refpart;
  Addr base;
  Count guardians;	/* guardians per seg */
  Index i;
  LinkPartStruct *linkpart;
  MRGGroup group;
  Pool pool;
  Res res;
  Seg linkseg;
  Seg refseg;
  Size linksegsize;
  Space space;
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
  linksegsize = guardians * sizeof(LinkPartStruct);
  linksegsize = SizeAlignUp(linksegsize, ArenaAlign(space));
  res = PoolSegAlloc(&linkseg, SegPrefDefault(), pool, linksegsize);
  if(res != ResOK)
    goto failLinkSegAlloc;

  /* Link Segment is coerced to an array of LinkPartStructs, */
  /* each one is appended to the free Ring using the linkRing. */
  /* The ref part of each guardian is cleared. */

  AVER(guardians > 0);
  base = SegBase(space, linkseg);
  linkpart = (LinkPartStruct *)base;
  refpart = (Addr *)SegBase(space, refseg);

  for(i=0; i<guardians; ++i) {
    RingInit(&linkpart[i].the.linkRing);
    linkpart[i].state = MRGGuardianFree;
    RingAppend(&mrg->free, &linkpart[i].the.linkRing);
    refpart[i] = 0;
  }
  AVER((Addr)(&linkpart[i]) <= SegLimit(space, linkseg));
  AVER((Addr)(&refpart[i]) <= SegLimit(space, refseg));
  /* design.mps.seg.field.rankSet.start */
  SegSetRankSet(refseg, RankSetSingle(RankFINAL));
  /* design.mps.seg.field.summary.start */
  SegSetSummary(refseg, RefSetUNIV);

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
  LinkPartStruct *linkpart;
  Message message;

  linkpart = (LinkPartStruct *)SegBase(space, group->linkseg);
  /* only finalize it if it hasn't been finalized already */
  if(linkpart[index].state != MRGGuardianFinal) {
    AVER(linkpart[index].state == MRGGuardianPrefinal);
    RingRemove(&linkpart[index].the.linkRing);
    linkpart[index].state = MRGGuardianFinal;
    message = &linkpart[index].the.messageStruct;
    MessageInit(space, message, &MRGMessageClassStruct);
    MessagePost(space, message);
  }
}

static Res MRGGroupScan(ScanState ss, MRGGroup group, MRG mrg)
{
  Addr base;
  Res res;
  Space space;
  Addr *refpart;
  Word guardians, i;

  space = PoolSpace(MRGPool(mrg));

  guardians = mrg->extendBy / sizeof(Addr);     /* per seg */
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
  LinkPartStruct *linkPart;

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

  linkPart = LinkPartOfRing(f);
  AVER(linkPart->state == MRGGuardianFree);
  /* design.mps.poolmrg.alloc.pop */
  RingRemove(f);
  linkPart->state = MRGGuardianPrefinal;
  RingAppend(&mrg->entry, f);
  gi = indexOfLinkPart(linkPart, space);
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
  LinkPartStruct *linkpart;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(old != (Addr)0);
  AVER(size == sizeof(Addr));

  space = PoolSpace(pool);
  b = SegOfAddr(&seg, space, old);
  AVER(b);
  group = SegP(seg);
  linkpart = (LinkPartStruct *)SegBase(space, group->linkseg);

  /* design.mps.poolmrg.guardian.ref.free */
  gi = indexOfRefPart(old, space);

  AVER(linkpart[gi].state == MRGGuardianPostfinal);
  RingInit(&linkpart[gi].the.linkRing);
  linkpart[gi].state = MRGGuardianFree;
  RingAppend(&mrg->free, &linkpart[gi].the.linkRing);
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

/* Describe
 *
 * This could be improved by implementind MRGSegDescribe
 * and having MRGDescribe iterate over all the pool's segments.
 */
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
    gi = indexOfLinkPart(LinkPartOfRing(r), space);
    WriteF(stream,
           "    at $A ref $A\n",
           (WriteFA)&refpart[gi], (WriteFA)refpart[gi],
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

/* deletes the message (frees up the memory) */
static void MRGMessageDelete(Message message)
{
  Addr *refpart;
  Seg seg;
  Bool b;
  Space space;
  Index index;
  MRGGroup group;
  LinkPartStruct *linkpart;

  AVERT(Message, message);

  space = MessageSpace(message);
  b = SegOfAddr(&seg, space, (Addr)message);
  AVER(b);
  AVER(SegPool(seg)->class == &PoolClassMRGStruct);
  group = (MRGGroup)SegP(seg);
  AVERT(MRGGroup, group);
  linkpart = LinkPartOfMessage(message);
  MessageFinish(message);
  linkpart->state = MRGGuardianPostfinal;

  index = indexOfLinkPart(linkpart, space);
  refpart = (Addr *)SegBase(space, group->refseg);

  PoolFree(SegPool(seg), (Addr)&refpart[index], sizeof(Addr));
}

static void MRGMessageFinalizationRef(Ref *refReturn,
				      Space space, Message message)
{
  Seg seg;
  Bool b;
  MRGGroup group;
  LinkPartStruct *linkpart;
  Index i;
  Addr *refpart;

  AVER(refReturn != NULL);
  AVERT(Space, space);
  AVERT(Message, message);

  AVER(message->type == MessageTypeFinalization);

  b = SegOfAddr(&seg, space, (Addr)message);
  AVER(b);
  group = SegP(seg);
  AVERT(MRGGroup, group);
  linkpart = LinkPartOfMessage(message);
  AVER(linkpart->state == MRGGuardianFinal);
  i = indexOfLinkPart(linkpart, space);
  refpart = (Addr *)SegBase(space, group->refseg);
  *refReturn = refpart[i];
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
  CHECKL(SegCheck(group->refseg));
  CHECKL(SegCheck(group->linkseg));
  return TRUE;
}
