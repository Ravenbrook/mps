/* impl.h.mpm: MEMORY POOL MANAGER DEFINITIONS
 *
 * $HopeName: MMsrc!mpm.h(MMdevel_restr.4) $
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


#ifdef TARGET_MPM_ASSERT	/* impl.h.target */
#define AVER(cond)		ASSERT(cond)
#define AVERT(type, val)	ASSERT(type ## Check(val))
#else
#define AVER(cond)		NOCHECK(cond)
#define AVERT(type, val)	NOCHECK(type ## Check(val))
#endif


extern Bool MPMCheck(void);


/* Ring Interface -- see impl.c.ring */

extern Bool RingCheck(Ring ring);
extern Bool RingCheckSingle(Ring ring);

extern void (RingInit)(Ring ring);
#define RingInit(_ring) \
  BEGIN \
    AVER(NULL != (_ring)); \
    (_ring)->next = (_ring); \
    (_ring)->prev = (_ring); \
    AVER(RingCheck(_ring)); \
  END

extern void (RingFinish)(Ring ring);
#define RingFinish(_ring) \
  BEGIN \
    AVER(RingCheckSingle(_ring)); \
    (_ring)->next = NULL; \
    (_ring)->prev = NULL; \
  END

extern void (RingAppend)(Ring ring, Ring new);
#define RingAppend(_ring, _new) \
  BEGIN \
    AVER(RingCheck(_ring)); \
    AVER(RingCheckSingle(_new)); \
    (_new)->prev = (_ring)->prev; \
    (_new)->next = (_ring); \
    (_ring)->prev->next = (_new); \
    (_ring)->prev = (_new); \
  END

extern void (RingRemove)(Ring old);
#define RingRemove(_old) \
  BEGIN \
    AVER(RingCheck(_old)); \
    (_old)->next->prev = (_old)->prev; \
    (_old)->prev->next = (_old)->next; \
    (_old)->next = (_old); \
    (_old)->prev = (_old); \
  END

extern Ring (RingNext)(Ring ring);
#define RingNext(_ring)	((_ring)->next)

#define RING_ELT(type, field, node) \
   ((type)((char *)(node) - (size_t)(&((type)0)->field)))

#define RING_FOR(_var, _ring) \
  for(_var = RingNext(_ring); \
      _var != (_ring); \
      _var = RingNext(_var))


/* Pool Interface -- see impl.c.pool */

extern void PoolInit(Pool pool, Space space, PoolClass class);
extern void PoolFinish(Pool pool);
extern Bool PoolCheck(Pool pool);
extern Res PoolDescribe(Pool pool, Lib_FILE *stream);

extern Space (PoolSpace)(Pool pool);
extern PoolClass (PoolGetClass)(Pool pool);
extern Ring (PoolSpaceRing)(Pool pool);
extern Ring (PoolBufferRing)(Pool pool);
extern Align (PoolAlignment)(Pool pool);

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
extern Res PoolCondemn(RefSet *condemnedReturn, Pool pool,
                         Space space, TraceId ti);
extern void PoolGrey(Pool pool, Space space, TraceId ti);
extern Res PoolScan(ScanState ss, Pool pool, Bool *finishedReturn);
extern Res PoolFix(Pool pool, ScanState ss, Seg seg, Addr *refIO);
extern void PoolReclaim(Pool pool, Space space, TraceId ti);
extern void PoolAccess(Pool pool, Seg seg, ProtMode mode);
extern Size PoolPoll(Pool pool);


/* Trace Interface -- see impl.c.trace */

#define TraceSetAdd(set, id)		((set) | ((TraceSet)1 << (id)))
#define TraceSetDelete(set, id)		((set) & ~((TraceSet)1 << (id)))
#define TraceSetIsMember(set, id)	(((set) >> (id)) & 1)
#define TraceSetUnion(set1, set2)	((set1) | (set2))

extern Res TraceCreate(TraceId *tiReturn, Space space);
extern void TraceDestroy(Space space, TraceId ti);

extern Bool ScanStateCheck(ScanState ss);
extern Bool TraceIdCheck(TraceId id);
extern Bool TraceSetCheck(TraceSet ts);

extern Res TraceFlip(Space space, TraceId ti, RefSet condemned);
extern Size TracePoll(Space space, TraceId ti);

extern Res TraceRunAtomic(Space space, TraceId ti);
extern Res TraceRun(Space space, TraceId ti, Bool *finishedReturn);

extern Res TraceFix(ScanState ss, Ref *refIO);

#define TRACE_SCAN_BEGIN(ss) \
  BEGIN \
    Shift SCANzoneShift = (ss)->zoneShift; \
    RefSet SCANcondemned = (ss)->condemned; \
    RefSet SCANsummary = (ss)->summary; \
    Word SCANt; \
    {

#define TRACE_FIX1(ss, ref) \
  (SCANt = (Word)1<<((Word)(ref)>>SCANzoneShift&(WORD_WIDTH-1)), \
   SCANsummary |= SCANt, \
   SCANcondemned & SCANt)

#define TRACE_FIX2(ss, refIO) \
  ((*(ss)->fix)((ss), (refIO)))

#define TRACE_FIX(ss, refIO) \
  (TRACE_FIX1((ss), *(refIO)) ? \
   TRACE_FIX2((ss), (refIO)) : ResOK)

#define TRACE_SCAN_END(ss) \
   } \
   (ss)->summary = SCANsummary; \
  END

extern Res TraceScanArea(ScanState ss, Addr *base, Addr *limit);
extern Res TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit);


/* Space Interface -- see impl.c.space */

extern Res SpaceCreate(Space *spaceReturn);
extern void SpaceDestroy(Space space);
extern Bool SpaceCheck(Space space);
extern Res SpaceDescribe(Space space, Lib_FILE *stream);
extern Bool SpaceAccess(Addr addr, ProtMode mode);
extern void SpaceLockClaim(Space space);
extern void SpaceLockRelease(Space space);
extern void SpacePoll(Space space);
extern Res SpaceAlloc(Addr *baseReturn, Space space, Size size);
extern void SpaceFree(Space space, Addr base, Size size);

#define SpacePoolRing(space)	(&(space)->poolRing)
#define SpaceRootRing(space)	(&(space)->rootRing)
#define SpaceTraceRing(space)	(&(space)->traceRing)
#define SpaceThreadRing(space)	(&(space)->threadRing)
#define SpaceEpoch(space)       ((space)->epoch) /* .epoch.ts */


/* Arena Interface -- see impl.c.arena* */

extern Res ArenaCreate(Space *spaceReturn, Size size);
extern void ArenaDestroy(Space space);
extern Bool ArenaCheck(Arena arena);
extern Align ArenaAlign(Space space);
extern Res SegAlloc(Seg *segReturn, Space space, Size size, Pool pool);
extern void SegFree(Space space, Seg seg);
extern Addr SegBase(Space space, Seg seg);
extern Addr SegLimit(Space space, Seg seg);
extern Size SegSize(Space space, Seg seg);
extern Bool SegOfAddr(Seg *segReturn, Space space, Addr addr);
extern Seg SegFirst(Space space);
extern Seg SegNext(Space space, Seg seg);
extern Bool SegCheck(Seg seg);


/* Buffer Interface -- see impl.c.buffer */

extern Res BufferCreate(Buffer *bufferReturn, Pool pool);
extern void BufferDestroy(Buffer buffer);
extern Bool BufferCheck(Buffer buffer);
extern Res BufferDescribe(Buffer buffer, Lib_FILE *stream);
extern Res BufferReserve(Addr *pReturn, Buffer buffer, Size size);
extern Res BufferFill(Addr *pReturn, Buffer buffer, Size size);
extern Bool BufferCommit(Buffer buffer, Addr p, Size size);
extern Bool BufferTrip(Buffer buffer, Addr p, Size size);
extern void BufferExpose(Buffer buffer);
extern void BufferCover(Buffer buffer);
extern void BufferInit(Buffer buffer, Pool pool);
extern void BufferFinish(Buffer buffer);
extern void BufferSet(Buffer buffer, Seg seg, Addr base, Addr init, Addr limit);
extern void BufferReset(Buffer buffer);
extern Bool BufferIsReset(Buffer buffer);
extern Ring BufferPoolRing(Buffer buffer);
extern Bool BufferIsReady(Buffer buffer);
extern Ap BufferAp(Buffer buffer);
extern Buffer BufferOfAp(Ap ap);
extern Space BufferSpace(Buffer buffer);
extern Pool (BufferPool)(Buffer buffer);
#define BufferPool(buffer) ((buffer)->pool)


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


/* Pool Class Interface -- see impl.c.poolclas */

extern void PoolClassInit(
  PoolClass class,
  const char *name,
  size_t size,
  size_t offset,
  PoolMethodCreate create,
  PoolMethodDestroy destroy,
  PoolMethodAlloc alloc,
  PoolMethodFree free_,
  PoolMethodBufferCreate bufferCreate,
  PoolMethodBufferDestroy bufferDestroy,
  PoolMethodBufferFill bufferFill,
  PoolMethodBufferTrip bufferTrip,
  PoolMethodBufferExpose bufferExpose,
  PoolMethodBufferCover bufferCover,
  PoolMethodCondemn condemn,
  PoolMethodGrey grey,
  PoolMethodScan scan,
  PoolMethodFix fix,
  PoolMethodReclaim reclaim,
  PoolMethodAccess access,
  PoolMethodDescribe describe
);

extern void PoolClassFinish(PoolClass class);

extern Bool PoolClassCheck(PoolClass class);


/* Reference Interface -- see impl.c.ref */

extern Bool RankCheck(Rank rank);

#define RefSetEmpty		((RefSet)0)
#define RefSetUniv		((RefSet)-1)
#define RefSetUnion(rs1, rs2)	((rs1) | (rs2))
#define RefSetInter(rs1, rs2)	((rs1) & (rs2))
#define RefSetZone(space, addr) \
  (((addr) >> space->zoneShift) & (WORD_WIDTH - 1))
#define RefSetAdd(space, rs, addr) \
  ((rs) | ((RefSet)1 << RefSetZone(space, addr)))
#define RefSetIsMember(space, rs, addr) \
  (((rs) >> RefSetZone(space, addr)) & 1)
extern RefSet RefSetOfSeg(Space space, Seg seg);


/* Shield Interface -- see impl.c.shield */

extern void ShieldRaise(Space space, Seg seg, ProtMode mode);
extern void ShieldLower(Space space, Seg seg, ProtMode mode);
extern void ShieldEnter(Space space);
extern void ShieldLeave(Space space);
extern void ShieldExpose(Space space, Seg seg);
extern void ShieldCover(Space space, Seg seg);
extern void ShieldSuspend(Space space);
extern void ShieldResume(Space space);


/* Protection Interface -- see impl.c.prot* */

extern void ProtSetup(void);

extern void ProtSet(Addr base, Addr limit, ProtMode mode);
extern void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
                      void *p, size_t s);


/* Location Dependency -- see impl.c.ld */

extern void LDReset(LD ld, Space space);
extern void LDAdd(LD ld, Space space, Addr addr);
extern Bool LDIsStale(LD ld, Space space, Addr addr);
extern void LDAge(Space space, RefSet moved);


/* Root Interface -- see impl.c.root */

extern Res RootCreateTable(Root *rootReturn, Space space,
                             Rank rank, Addr *base, Addr *limit);
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
extern Res RootDescribe(Root root, Lib_FILE *stream);
extern Bool RootIsAtomic(Root root);
extern Rank RootRank(Root root);
extern void RootGrey(Root root, Space space, TraceId ti);
extern Res RootScan(ScanState ss, Root root);
extern Space RootSpace(Root root);


/* VM Interface -- see impl.c.vm* */

extern Align VMAlign(void);
extern Bool VMCheck(VM vm);
extern Res VMCreate(Space *spaceReturn, size_t size);
extern void VMDestroy(Space space);
extern Addr VMBase(Space space);
extern Addr VMLimit(Space space);
extern Res VMMap(Space space, Addr base, Addr limit);
extern void VMUnmap(Space space, Addr base, Addr limit);


/* Address/Size Interface -- see impl.c.mpm */

extern Bool (WordIsAligned)(Word word, Align align);
#define WordIsAligned(w, a)	(((w) & ((a) - 1)) == 0)

extern Word (WordAlignUp)(Word word, Align align);
#define WordAlignUp(w, a)	(((w) + (a) - 1) & ~((a) - 1))

extern Bool AlignCheck(Align align);

extern Addr (AddrAdd)(Addr addr, Size size);
#define AddrAdd(p, s)		((Addr)((Word)(p) + (s)))

extern Size (AddrOffset)(Addr base, Addr limit);
#define AddrOffset(p, l)	((Size)((Word)(l) - (Word)(p)))

extern Bool SizeIsP2(Size size);
extern Shift SizeLog2(Size size);

#define AddrIsAligned(p, a)	WordIsAligned((Word)(p), a)
#define SizeIsAligned(s, a)	WordIsAligned((Word)(s), a)
#define SizeAlignUp(s, a)	((Size)WordAlignUp((Word)(s), a))
#define AddrAlignUp(p, a)	((Addr)WordAlignUp((Word)(p), a))


#endif /* mpm_h */
