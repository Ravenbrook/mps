/* impl.h.mpm: MEMORY POOL MANAGER DEFINITIONS
 *
 * $HopeName: MMsrc!mpm.h(MMdevel_trace2.1) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 */

#ifndef mpm_h
#define mpm_h

#include "mpmconf.h"
#include "mpmtypes.h"
#include "mpmst.h"
#include "lock.h"
#include "th.h"
#include "poolmv.h"
#include "poolmfs.h"
#include "ss.h"
#include "mpslib.h"


/* AVER, AVERT -- MPM assertions
 *
 * AVER and AVERT are used to assert conditions within the MPM (as
 * opposed to in the MPS Interface layer, impl.c.mpsi).  This allows
 * control over internal and interface checking.
 */

#ifdef TARGET_MPM_ASSERT        /* impl.h.target */
#define AVER                    ASSERT
#define AVERT(type, val)        ASSERT(type ## Check(val))
#else
#define AVER(cond)              NOCHECK(cond)
#define AVERT(type, val)        NOCHECK(type ## Check(val))
#endif


/* MPMCheck -- check MPM assumptions */

extern Bool MPMCheck(void);


/* Miscellaneous Checks -- see impl.c.mpm */

extern Bool BoolCheck(Bool b);
extern Bool FunCheck(Fun f);
extern Bool AttrCheck(Attr attr);
#define FUNCHECK(f)	(FunCheck((Fun)f))


/* Address/Size Interface -- see impl.c.mpm */

extern Bool (WordIsAligned)(Word word, Align align);
#define WordIsAligned(w, a)     (((w) & ((a) - 1)) == 0)

extern Word (WordAlignUp)(Word word, Align align);
#define WordAlignUp(w, a)       (((w) + (a) - 1) & ~((a) - 1))

extern Bool AlignCheck(Align align);

extern Addr (AddrAdd)(Addr addr, Size size);
#define AddrAdd(p, s)           ((Addr)((Word)(p) + (s)))

extern Addr (AddrSub)(Addr addr, Size size);
#define AddrSub(p, s)           ((Addr)((Word)(p) - (s)))

extern Size (AddrOffset)(Addr base, Addr limit);
#define AddrOffset(p, l)        ((Size)((Word)(l) - (Word)(p)))


/* Logs and Powers
 * 
 * SizeIsP2 returns TRUE if and only if size is a non-negative integer
 * power of 2, and FALSE otherwise.
 * 
 * SizeLog2 returns the logarithm in base 2 of size.  size must be a
 * power of 2.
 * 
 * SizeFloorLog2 returns the floor of the logarithm in base 2 of size.
 * size can be any value.
 */

extern Bool SizeIsP2(Size size);
extern Shift SizeLog2(Size size);
extern Shift SizeFloorLog2(Size size);

#define AddrWord(a)             ((Word)a)
#define SizeWord(s)             ((Word)s)
#define AddrIsAligned(p, a)     WordIsAligned(AddrWord(p), a)
#define AddrAlignUp(p, a)       ((Addr)WordAlignUp(AddrWord(p), a))
#define SizeIsAligned(s, a)     WordIsAligned(SizeWord(s), a)
#define SizeAlignUp(s, a)       ((Size)WordAlignUp(SizeWord(s), a))

extern Res WriteF(mps_lib_FILE *stream, ...);


/* Ring Interface -- see impl.c.ring */

extern Bool RingCheck(Ring ring);
extern Bool RingCheckSingle(Ring ring);

extern void (RingInit)(Ring ring);
#define RingInit(ring) \
  BEGIN \
    Ring _ring = (ring); \
    AVER(NULL != _ring); \
    _ring->next = _ring; \
    _ring->prev = _ring; \
    AVER(RingCheck(_ring)); \
  END

extern void (RingFinish)(Ring ring);
#define RingFinish(ring) \
  BEGIN \
    Ring _ring = (ring); \
    AVER(RingCheckSingle(_ring)); \
    _ring->next = RingNONE; \
    _ring->prev = RingNONE; \
  END

extern void (RingAppend)(Ring ring, Ring new);
#define RingAppend(ring, new) \
  BEGIN \
    Ring _ring = (ring), _new = (new); \
    AVER(RingCheck(_ring)); \
    AVER(RingCheckSingle(_new)); \
    _new->prev = _ring->prev; \
    _new->next = _ring; \
    _ring->prev->next = _new; \
    _ring->prev = _new; \
  END

extern void (RingRemove)(Ring old);
#define RingRemove(old) \
  BEGIN \
    Ring _old = (old); \
    AVER(RingCheck(_old)); \
    _old->next->prev = _old->prev; \
    _old->prev->next = _old->next; \
    _old->next = _old; \
    _old->prev = _old; \
  END

extern Ring (RingNext)(Ring ring);
#define RingNext(ring)  ((ring)->next)

#define RING_ELT(type, field, node) \
   ((type)((char *)(node) - (size_t)(&((type)0)->field)))

#define RING_FOR(var, ring) \
  for(var = RingNext(ring); \
      var != (ring); \
      var = RingNext(var))


/* Pool Interface -- see impl.c.pool */

extern Res PoolInit(Pool pool, Space space, PoolClass class, ...);
extern Res PoolInitV(Pool pool, Space space, PoolClass class, va_list args);
extern void PoolFinish(Pool pool);
extern Bool PoolClassCheck(PoolClass class);
extern Bool PoolCheck(Pool pool);
extern Res PoolDescribe(Pool pool, mps_lib_FILE *stream);

#define PoolSpace(pool)         ((pool)->space)
#define PoolAlignment(pool)     ((pool)->alignment)
#define PoolOptionRing(pool)	(&(pool)->optionRing)

extern Space (PoolSpace)(Pool pool);
extern Align (PoolAlignment)(Pool pool);
extern Ring (PoolOptionRing)(Pool pool);

extern Res PoolSegAlloc(Seg *segReturn, Pool pool, Size size);
extern void PoolSegFree(Pool pool, Seg seg);
extern Bool PoolOfAddr(Pool *poolReturn, Space space, Addr addr);
extern Bool PoolHasAddr(Pool pool, Addr addr);
extern Res PoolCreate(Pool *poolReturn, PoolClass class,
                        Space space, ...);
extern Res PoolCreateV(Pool *poolReturn, PoolClass class,
                         Space space, va_list arg);
extern void PoolDestroy(Pool pool);
extern Res PoolAlloc(Addr *pReturn, Pool pool, Size size);
extern void PoolFree(Pool pool, Addr old, Size size);
extern void PoolCondemn(Pool pool, Option option, Seg seg, TraceId ti);
extern Res PoolScan(Pool pool, Fix fix, Seg seg);
extern void PoolReclaim(Pool pool, Seg seg, TraceSet ts);
extern Res (PoolFix)(Addr *refIO, Pool pool, Fix fix, Seg seg);
#define PoolFix(refIO, pool, fix, seg) \
  ((*(pool)->class->fix)(refIO, pool, fix, seg))

extern void PoolTrivFinish(Pool pool);
extern Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size);
extern Res PoolTrivAlloc(Addr *pReturn, Pool pool, Size size);
extern void PoolNoFree(Pool pool, Addr old, Size size);
extern void PoolTrivFree(Pool pool, Addr old, Size size);
extern Res PoolNoBufferInit(Pool pool, Buffer buf);
extern Res PoolTrivBufferInit(Pool pool, Buffer buf);
extern void PoolNoBufferFinish(Pool pool, Buffer buf);
extern void PoolTrivBufferFinish(Pool pool, Buffer buf);
extern Res PoolNoBufferFill(Addr *baseReturn, Pool pool, Buffer buffer, Size size);
extern Bool PoolNoBufferTrip(Pool pool, Buffer buffer, Addr base, Size size);
extern void PoolNoBufferExpose(Pool pool, Buffer buffer);
extern void PoolNoBufferCover(Pool pool, Buffer buffer);
extern Res PoolNoDescribe(Pool pool, mps_lib_FILE *stream);
extern Res PoolTrivDescribe(Pool pool, mps_lib_FILE *stream);
extern void PoolNoCondemn(Pool pool, Option option, Seg seg, TraceId ti);
extern Res PoolNoScan(Pool pool, Fix fix, Seg seg);
extern Res PoolNoFix(Ref *refIO, Pool pool, Fix fix, Seg seg);
extern void PoolNoReclaim(Pool pool, Seg seg, TraceSet ts);


/* Trace Interface -- see impl.c.trace */

extern Bool ColCheck(Col col);
extern void ColInit(Col col);
extern Bool ColIsWhite(Col col, TraceSet ts);
extern Bool ColIsGrey(Col col, TraceSet ts);
extern void ColBlacken(Col col, TraceSet ts);
extern void ColGreyen(Col col, TraceSet ts);
extern Bool ColSuper(Col col1, Col col2);
extern void ColSetBlack(Col col, TraceSet ts);
extern void ColSetGrey(Col col, TraceSet ts);
extern void ColAddWhite(Col col, TraceSet ts);
extern void ColSetWhite(Col col, TraceSet ts);
extern void ColMerge(Col col1, Col col2);
extern void ColMergeNGreyen(Col col1, Col col2, TraceSet ts);

#define TraceSetSingle(ti)	BS_SINGLE(TraceSet, ti)
#define TraceSetMember(ts, ti)	BS_MEMBER(ts, ti)
#define TraceSetComp(ts)	BS_COMP(ts)
#define TraceSetAdd(ts, ti)	BS_ADD(TraceSet, ts, ti)
#define TraceSetDel(ts, ti)	BS_DEL(TraceSet, ts, ti)
#define TraceSetUnion(ts1, ts2)	BS_UNION(ts1, ts2)
#define TraceSetInter(ts1, ts2)	BS_INTER(ts1, ts2)
#define TraceSetSuper(ts1, ts2)	BS_SUPER(ts1, ts2)
#define TraceSetSub(ts1, ts2)	BS_SUB(ts1, ts2)
#define TraceSetDiff(ts1, ts2)	BS_DIFF(ts1, ts2)

extern TraceSet (TraceSetComp)(TraceSet ts);
extern TraceSet (TraceSetAdd)(TraceSet ts, TraceId id);
extern TraceSet (TraceSetDel)(TraceSet ts, TraceId id);
extern TraceSet (TraceSetUnion)(TraceSet ts1, TraceSet ts2);
extern TraceSet (TraceSetInter)(TraceSet ts1, TraceSet ts2);
extern Bool (TraceSetMember)(TraceSet ts, TraceId id);
extern TraceSet (TraceSetSingle)(TraceId id);
extern TraceSet (TraceSetDiff)(TraceSet ts1, TraceSet ts2);
extern Bool (TraceSetSuper)(TraceSet ts1, TraceSet ts2);
extern Bool (TraceSetSub)(TraceSet ts1, TraceSet ts2);

extern Bool FixCheck(Fix fix);
extern Bool TraceIdCheck(TraceId id);
extern Bool TraceSetCheck(TraceSet ts);
extern Bool TraceCheck(Trace trace);

extern Res TraceCreate(Trace *traceReturn, Space space);
extern Res TraceStart(Trace trace, Option option);
extern Res TraceStep(Trace trace);
extern void TraceDestroy(Trace trace);

extern Res TraceComplete(Space space, Option option); /* @@@@ */

extern Res TraceFix(Ref *refIO, Fix fix);
extern RefSet TraceWhite(Trace trace);

/* Equivalent to impl.h.mps MPS_SCAN_BEGIN */

#define TRACE_SCAN_BEGIN(fix) \
  BEGIN \
    Shift SCANzoneShift = (fix)->zoneShift; \
    RefSet SCANwhite = (fix)->white; \
    RefSet SCANsummary = (fix)->summary; \
    Word SCANt; \
    {

/* Equivalent to impl.h.mps MPS_FIX1 */

#define TRACE_FIX1(fix, ref) \
  (SCANt = (Word)1<<((Word)(ref)>>SCANzoneShift&(WORD_WIDTH-1)), \
   SCANsummary |= SCANt, \
   SCANwhite & SCANt)

/* Equivalent to impl.h.mps MPS_FIX2 */

#define TRACE_FIX2(refIO, fix) \
  ((*(fix)->f)(refIO, fix))

/* Equivalent to impl.h.mps MPS_FIX */

#define TRACE_FIX(refIO, fix) \
  (TRACE_FIX1(fix, *(refIO)) ? \
   TRACE_FIX2(refIO, fix) : ResOK)

/* Equivalent to impl.h.mps MPS_SCAN_END */

#define TRACE_SCAN_END(fix) \
   } \
   (fix)->summary = SCANsummary; \
  END

extern Res TraceScanArea(Fix fix, Addr *base, Addr *limit);
extern Res TraceScanAreaTagged(Fix fix, Addr *base, Addr *limit);


/* Strategy Interface -- NOT DOCUMENTED */

extern void OptionInit(Option option, Pool pool);
extern void OptionFinish(Option option);
extern Bool OptionCheck(Option option);
extern void OptionCondemn(Option option, Seg seg, TraceId ti);
extern void Think(Space space);


/* Space Interface -- see impl.c.space */

extern Res SpaceCreate(Space *spaceReturn, Addr base, Size size);
extern void SpaceDestroy(Space space);
extern Bool SpaceCheck(Space space);
extern Res SpaceDescribe(Space space, mps_lib_FILE *stream);
extern Bool SpaceAccess(Addr addr, AccessSet mode);
extern void SpaceEnter(Space space);
extern void SpaceLeave(Space space);
extern void SpacePoll(Space space);
extern Res SpaceAlloc(Addr *baseReturn, Space space, Size size);
extern void SpaceFree(Space space, Addr base, Size size);

#define SpacePoolRing(space)    (&(space)->poolRing)
#define SpaceRootRing(space)    (&(space)->rootRing)
#define SpaceTraceRing(space)   (&(space)->traceRing)
#define SpaceThreadRing(space)  (&(space)->threadRing)
#define SpaceEpoch(space)       ((space)->epoch) /* .epoch.ts */
#define SpaceTrace(space, ti)	(&(space)->trace[ti])
#define SpaceZoneShift(space)	((space)->zoneShift)
#define SpaceMutCol(space)	(&(space)->mutColStruct)


/* Arena Interface -- see impl.c.arena* */

extern Res ArenaCreate(Space *spaceReturn, Size size, Addr base);
extern void ArenaDestroy(Space space);
extern Bool ArenaCheck(Arena arena);
extern Align ArenaAlign(Space space);
extern Size ArenaReserved(Space space);
extern Size ArenaCommitted(Space space);
extern Res SegAlloc(Seg *segReturn, Space space, Size size, Pool pool);
extern void SegFree(Space space, Seg seg);
extern Addr SegBase(Space space, Seg seg);
extern Addr SegLimit(Space space, Seg seg);
extern Size SegSize(Space space, Seg seg);
extern Bool SegOfAddr(Seg *segReturn, Space space, Addr addr);
extern Seg SegFirst(Space space);
extern Seg SegNext(Space space, Seg seg);
extern Bool SegCheck(Seg seg);
extern Col SegCol(Seg seg);

#define SegPool(seg)		((seg)->pool)
#define SegPoolRing(seg)	(&(seg)->poolRing)
#define SegBuffer(seg)		((seg)->buffer)
#define SegSetBuffer(seg, b)	((void)((seg)->buffer = (b)))
#define SegP(seg)		((seg)->p)
#define SegSetP(seg, pp)	((void)((seg)->p = (pp)))
#define SegCol(seg)		(&(seg)->colStruct)

extern Pool (SegPool)(Seg seg);
extern Ring (SegPoolRing)(Seg seg);
extern Buffer (SegBuffer)(Seg seg);
extern void (SegSetBuffer)(Seg seg, Buffer buffer);
extern void *(SegP)(Seg seg);
extern void (SegSetP)(Seg seg, void *p);
extern Col (SegCol)(Seg seg);


/* Buffer Interface -- see impl.c.buffer */

extern Res BufferCreate(Buffer *bufferReturn, Pool pool, Rank rank);
extern void BufferDestroy(Buffer buffer);
extern Bool BufferCheck(Buffer buffer);
extern Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream);
extern Res BufferReserve(Addr *pReturn, Buffer buffer, Size size);
extern Res BufferFill(Addr *pReturn, Buffer buffer, Size size);
extern Bool BufferCommit(Buffer buffer, Addr p, Size size);
extern Bool BufferTrip(Buffer buffer, Addr p, Size size);
extern void BufferExpose(Buffer buffer);
extern void BufferCover(Buffer buffer);
extern Res BufferInit(Buffer buffer, Pool pool, Rank rank);
extern void BufferFinish(Buffer buffer);
extern void BufferSet(Buffer buffer, Seg seg, Addr base, Addr init, Addr limit);
extern void BufferReset(Buffer buffer);
extern Bool BufferIsReset(Buffer buffer);
extern Bool BufferIsReady(Buffer buffer);
extern AP BufferAP(Buffer buffer);
extern Buffer BufferOfAP(AP ap);
extern Space BufferSpace(Buffer buffer);

#define BufferPool(buffer)	((buffer)->pool)
#define BufferSeg(buffer)	((buffer)->seg)
#define BufferCol(buffer)	((buffer)->col)

extern Pool (BufferPool)(Buffer buffer);
extern Seg (BufferSeg)(Buffer buffer);
extern Col (BufferCol)(Buffer buffer);


/* Format Interface -- see impl.c.format */

extern Bool FormatCheck(Format format);
extern Res FormatCreate(Format *formatReturn, Space space,
                        Align alignment,
                        FormatScanMethod scan,
                        FormatSkipMethod skip,
                        FormatMoveMethod move,
                        FormatIsMovedMethod isMoved,
                        FormatCopyMethod copy,
                        FormatPadMethod pad);
extern void FormatDestroy(Format format);
extern Space FormatSpace(Format format);
extern Res FormatDescribe(Format format, mps_lib_FILE *stream);


/* Reference Interface -- see impl.c.ref */

extern Bool RankCheck(Rank rank);

#define RankSetSingle(rank)	BS_SINGLE(RankSet, rank)
#define RankSetAdd(s, r)	BS_ADD(RankSet, s, r)
#define RankSetIsSingle(set)	BS_IS_SINGLE(set)
#define RankSetSuper(s1, s2)	BS_SUPER(s1, s2)
#define RankSetMember(s, r)	BS_MEMBER(s, r)
#define RankSetUnion(s1, s2)	BS_UNION(s1, s2)

extern RankSet (RankSetSingle)(Rank rank);
extern RankSet (RankSetAdd)(RankSet set, Rank rank);
extern Bool (RankSetIsSingle)(Rank rank);
extern Bool (RankSetSuper)(RankSet s1, RankSet s2);
extern Bool (RankSetMember)(RankSet set, Rank rank);
extern RankSet (RankSetUnion)(RankSet s1, RankSet s2);

#define RefSetZone(space, addr) \
  (((Word)(addr) >> space->zoneShift) & (WORD_WIDTH - 1))
#define RefSetUnion(rs1, rs2)	BS_UNION(rs1, rs2)
#define RefSetInter(rs1, rs2)	BS_INTER(rs1, rs2)
#define RefSetAdd(space, rs, addr) \
  BS_ADD(RefSet, rs, RefSetZone(space, addr))
#define RefSetMember(space, rs, addr) \
  BS_MEMBER(rs, RefSetZone(space, addr))
#define RefSetSuper(rs1, rs2)	BS_SUPER(rs1, rs2)

extern RefSet (RefSetUnion)(RefSet rs1, RefSet rs2);
extern RefSet (RefSetInter)(RefSet rs1, RefSet rs2);
extern RefSet (RefSetAdd)(Space space, RefSet rs, Addr addr);
extern Bool (RefSetMember)(Space space, RefSet rs, Addr addr);
extern Bool (RefSetSuper)(RefSet rs1, RefSet rs2);

extern RefSet RefSetOfSeg(Space space, Seg seg);


/* Shield Interface -- see impl.c.shield */

extern void ShieldRaise(Space space, Seg seg, AccessSet mode);
extern void ShieldLower(Space space, Seg seg, AccessSet mode);
extern void ShieldEnter(Space space);
extern void ShieldLeave(Space space);
extern void ShieldExpose(Space space, Seg seg);
extern void ShieldCover(Space space, Seg seg);
extern void ShieldSuspend(Space space);
extern void ShieldResume(Space space);
extern void ShieldFlush(Space space);


/* Protection Interface -- see impl.c.prot* */

extern void ProtSetup(void);

extern void ProtSet(Addr base, Addr limit, AccessSet mode);
extern void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
                      void *p, size_t s);
extern void ProtSync(Space space);


/* Location Dependency -- see impl.c.ld */

extern void LDReset(LD ld, Space space);
extern void LDAdd(LD ld, Space space, Addr addr);
extern Bool LDIsStale(LD ld, Space space, Addr addr);
extern void LDAge(Space space, RefSet moved);


/* Root Interface -- see impl.c.root */

extern Res RootCreateTable(Root *rootReturn, Space space,
                           Rank rank, Addr *refs, size_t size);
extern Res RootCreateReg(Root *rootReturn, Space space,
                         Rank rank, Thread thread,
                         RootScanRegMethod scan,
                         void *p);
extern Res RootCreateFmt(Root *rootReturn, Space space,
                         Rank rank, FormatScanMethod scan,
                         Addr base, Addr limit);
extern Res RootCreate(Root *rootReturn, Space space,
                      Rank rank, RootScanMethod scan,
                      void *p, size_t s);
extern void RootDestroy(Root root);
extern Bool RootCheck(Root root);
extern Res RootDescribe(Root root, mps_lib_FILE *stream);
extern Space RootSpace(Root root);
extern Rank RootRank(Root root);
extern Res RootScan(Root root, Fix fix);


/* VM Interface -- see impl.c.vm* */

extern Align VMAlign(void);
extern Bool VMCheck(VM vm);
extern Res VMCreate(Space *spaceReturn, Size size, Addr base);
extern void VMDestroy(Space space);
extern Addr VMBase(Space space);
extern Addr VMLimit(Space space);
extern Res VMMap(Space space, Addr base, Addr limit);
extern void VMUnmap(Space space, Addr base, Addr limit);
extern Size VMReserved(Space space);
extern Size VMMapped(Space space);

#endif /* mpm_h */
