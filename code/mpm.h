/* mpm.h: MEMORY POOL MANAGER DEFINITIONS
 *
 * $Id$
 * Copyright (c) 2001,2003 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .trans.bufferinit: The Buffer data structure has an Init field and
 * an Init method, there's a name clash.  We resolve this by calling the
 * accessor BufferGetInit. */

#ifndef mpm_h
#define mpm_h

#include "config.h"
#include "misc.h"
#include "check.h"

#include "event.h"
#include "lock.h"
#include "th.h"
#include "ss.h"
#include "mpslib.h"
#include "ring.h"
#include "tract.h" /* only for certain Seg macros */
#include "mpmtypes.h"
#include "mpmst.h"


/* MPMCheck -- check MPM assumptions */

EXTERN Bool MPMCheck(void);


/* Miscellaneous Checks -- see <code/mpm.c> */

/* <design/type/#bool.check> */
#define BoolCheck(b) ((unsigned)(b) <= 1)

EXTERN Bool FunCheck(Fun f);
#define FUNCHECK(f)     (FunCheck((Fun)f))

EXTERN Bool ShiftCheck(Shift shift);
EXTERN Bool AttrCheck(Attr attr);
EXTERN Bool RootVarCheck(RootVar rootVar);


/* Address/Size Interface -- see <code/mpm.c> */

EXTERN Bool AlignCheck(Align align);

EXTERN Bool (WordIsAligned)(Word word, Align align);
#define WordIsAligned(w, a)     (((w) & ((a) - 1)) == 0)

EXTERN Word (WordAlignUp)(Word word, Align align);
#define WordAlignUp(w, a)       (((w) + (a) - 1) & ~((Word)(a) - 1))

/* Rounds w up to a multiple of r, see <code/mpm.c> for exact behaviour */
EXTERN Word (WordRoundUp)(Word word, Size round);
#define WordRoundUp(w, r)       (((w)+(r)-1) - ((w)+(r)-1)%(r))

EXTERN Word (WordAlignDown)(Word word, Align align);
#define WordAlignDown(w, a)     ((w) & ~((Word)(a) - 1))

#define size_tAlignUp(s, a) ((size_t)WordAlignUp((Word)(s), a))

#define PointerAdd(p, s) ((void *)((char *)(p) + (s)))
#define PointerSub(p, s) ((void *)((char *)(p) - (s)))

#define PointerOffset(base, limit) \
  ((size_t)((char *)(limit) - (char *)(base)))

#define PointerAlignUp(p, s) \
  ((void *)WordAlignUp((Word)(p), (Align)(s)))

#define AddrAdd(p, s) ((Addr)PointerAdd((void *)(p), s))
#define AddrSub(p, s) ((Addr)PointerSub((void *)(p), s))

#define AddrOffset(b, l) \
  ((Size)(PointerOffset((void *)(b), (void *)(l))))

EXTERN Addr (AddrAlignDown)(Addr addr, Align align);
#define AddrAlignDown(p, a) ((Addr)WordAlignDown((Word)(p), a))

#define AlignWord(s)            ((Word)(s))

#define AddrIsAligned(p, a)     WordIsAligned((Word)(p), a)
#define AddrAlignUp(p, a)       ((Addr)WordAlignUp((Word)(p), a))

#define SizeIsAligned(s, a)     WordIsAligned((Word)(s), a)
#define SizeAlignUp(s, a)       ((Size)WordAlignUp((Word)(s), a))
#define SizeAlignDown(s, a)     ((Size)WordAlignDown((Word)(s), a))
/* r not required to be a power of 2 */
#define SizeRoundUp(s, r)       ((Size)WordRoundUp((Word)(s), (Size)(r)))

#define IndexIsAligned(s, a)    WordIsAligned((Word)(s), a)
#define IndexAlignUp(s, a)      ((Index)WordAlignUp((Word)(s), a))
#define IndexAlignDown(s, a)    ((Index)WordAlignDown((Word)(s), a))

#define AlignIsAligned(a1, a2)  WordIsAligned((Word)(a1), a2)


EXTERN Addr (AddrSet)(Addr target, Byte value, Size size);
/* This is one of the places that implements Addr, so it's allowed to */
/* convert to void *, see <design/type/#addr.ops.mem>. */
#define AddrSet(target, value, size) \
  mps_lib_memset(target, (int)(value), size)

EXTERN Addr (AddrCopy)(Addr target, Addr source, Size size);
#define AddrCopy(target, source, size) \
  mps_lib_memcpy(target, source, size)

EXTERN int (AddrComp)(Addr a, Addr b, Size size);
#define AddrComp(a, b, size) \
  mps_lib_memcmp(a, b, size)


/* ADDR_PTR -- turns an Addr into a pointer to the given type */

#define ADDR_PTR(type, addr) ((type *)(addr))


/* Clock */

#define ClockNow() ((Clock)mps_clock())
#define ClocksPerSec() ((Clock)mps_clocks_per_sec())


/* Result codes */

EXTERN Bool ResIsAllocFailure(Res res);


/* Logs and Powers
 *
 * SizeIsP2 returns TRUE if and only if size is a non-negative integer
 * power of 2, and FALSE otherwise.
 *
 * SizeLog2 returns the logarithm in base 2 of size.  size must be a
 * power of 2.
 *
 * SizeFloorLog2 returns the floor of the logarithm in base 2 of size.
 * size can be any positive non-zero value.  */

EXTERN Bool SizeIsP2(Size size);
EXTERN Shift SizeLog2(Size size);
EXTERN Shift SizeFloorLog2(Size size);

EXTERN Bool WordIsP2(Word word);

/* Formatted Output -- see <design/writef/>, <code/mpm.c> */

EXTERN Res WriteF(mps_lib_FILE *stream, ...);
EXTERN Res WriteF_v(mps_lib_FILE *stream, va_list args);
EXTERN Res WriteF_firstformat_v(mps_lib_FILE *stream,
                                const char *firstformat, va_list args);

#if defined(DIAG_WITH_STREAM_AND_WRITEF)
EXTERN int Stream_fputc(int c, mps_lib_FILE *stream);
EXTERN int Stream_fputs(const char *s, mps_lib_FILE *stream);
#else
#define Stream_fputc mps_lib_fputc
#define Stream_fputs mps_lib_fputs
#endif


/* Miscellaneous support -- see <code/mpm.c> */

EXTERN size_t StringLength(const char *s);
EXTERN Bool StringEqual(const char *s1, const char *s2);


/* Version Determination
 *
 * See <design/version-library/>.  */

EXTERN char *MPSVersion(void);


/* Pool Interface -- see impl.c.pool */

EXTERN Res PoolInit(Pool pool, Arena arena, PoolClass class, ...);
EXTERN Res PoolInitV(Pool pool, Arena arena, PoolClass class, va_list args);
EXTERN void PoolFinish(Pool pool);
EXTERN Bool PoolClassCheck(PoolClass class);
EXTERN Bool PoolCheck(Pool pool);
EXTERN Res PoolDescribe(Pool pool, mps_lib_FILE *stream);

#define PoolArena(pool)         ((pool)->arena)
#define PoolAlignment(pool)     ((pool)->alignment)
#define PoolSegRing(pool)       (&(pool)->segRing)

EXTERN Bool PoolFormat(Format *formatReturn, Pool pool);

EXTERN double PoolMutatorAllocSize(Pool pool);

EXTERN Bool PoolOfAddr(Pool *poolReturn, Arena arena, Addr addr);
EXTERN Bool PoolHasAddr(Pool pool, Addr addr);

EXTERN Res PoolCreate(Pool *poolReturn, Arena arena, PoolClass class, ...);
EXTERN Res PoolCreateV(Pool *poolReturn, Arena arena, PoolClass class,
                       va_list arg);
EXTERN void PoolDestroy(Pool pool);
EXTERN BufferClass PoolDefaultBufferClass(Pool pool);
EXTERN Res PoolAlloc(Addr *pReturn, Pool pool, Size size,
                     Bool withReservoirPermit);
EXTERN void PoolFree(Pool pool, Addr old, Size size);
EXTERN Res PoolTraceBegin(Pool pool, Trace trace);
EXTERN Res PoolAccess(Pool pool, Seg seg, Addr addr,
                      AccessSet mode, MutatorFaultContext context);
EXTERN Res PoolWhiten(Pool pool, Trace trace, Seg seg);
EXTERN void PoolGrey(Pool pool, Trace trace, Seg seg);
EXTERN void PoolBlacken(Pool pool, TraceSet traceSet, Seg seg);
EXTERN Res PoolScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg);
EXTERN Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO);
#define PoolFix(pool, ss, seg, refIO) \
  ((*(pool)->fix)(pool, ss, seg, refIO))
EXTERN Res PoolFixEmergency(Pool pool, ScanState ss, Seg seg, Addr *refIO);
EXTERN void PoolReclaim(Pool pool, Trace trace, Seg seg);
EXTERN void PoolTraceEnd(Pool pool, Trace trace);
EXTERN void PoolWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
                     void *v, size_t s);
EXTERN void PoolFreeWalk(Pool pool, FreeBlockStepMethod f, void *p);
EXTERN Res PoolTrivInit(Pool pool, va_list arg);
EXTERN void PoolTrivFinish(Pool pool);
EXTERN Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size,
                       Bool withReservoirPermit);
EXTERN Res PoolTrivAlloc(Addr *pReturn, Pool pool, Size size,
                         Bool withReservoirPermit);
EXTERN void PoolNoFree(Pool pool, Addr old, Size size);
EXTERN void PoolTrivFree(Pool pool, Addr old, Size size);
EXTERN Res PoolNoBufferFill(Addr *baseReturn, Addr *limitReturn,
                            Pool pool, Buffer buffer, Size size,
                            Bool withReservoirPermit);
EXTERN Res PoolTrivBufferFill(Addr *baseReturn, Addr *limitReturn,
                              Pool pool, Buffer buffer, Size size,
                              Bool withReservoirPermit);
EXTERN void PoolNoBufferEmpty(Pool pool, Buffer buffer,
                              Addr init, Addr limit);
EXTERN void PoolTrivBufferEmpty(Pool pool, Buffer buffer,
                                Addr init, Addr limit);
EXTERN Res PoolTrivDescribe(Pool pool, mps_lib_FILE *stream);
EXTERN Res PoolNoTraceBegin(Pool pool, Trace trace);
EXTERN Res PoolTrivTraceBegin(Pool pool, Trace trace);
EXTERN Res PoolNoAccess(Pool pool, Seg seg, Addr addr,
                        AccessSet mode, MutatorFaultContext context);
EXTERN Res PoolSegAccess(Pool pool, Seg seg, Addr addr,
                         AccessSet mode, MutatorFaultContext context);
EXTERN Res PoolSingleAccess(Pool pool, Seg seg, Addr addr,
                            AccessSet mode, MutatorFaultContext context);
EXTERN Res PoolNoWhiten(Pool pool, Trace trace, Seg seg);
EXTERN Res PoolTrivWhiten(Pool pool, Trace trace, Seg seg);
EXTERN void PoolNoGrey(Pool pool, Trace trace, Seg seg);
EXTERN void PoolTrivGrey(Pool pool, Trace trace, Seg seg);
EXTERN void PoolNoBlacken(Pool pool, TraceSet traceSet, Seg seg);
EXTERN void PoolTrivBlacken(Pool pool, TraceSet traceSet, Seg seg);
EXTERN Res PoolNoScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg);
EXTERN Res PoolNoFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
EXTERN void PoolNoReclaim(Pool pool, Trace trace, Seg seg);
EXTERN void PoolTrivTraceEnd(Pool pool, Trace trace);
EXTERN void PoolNoRampBegin(Pool pool, Buffer buf, Bool collectAll);
EXTERN void PoolTrivRampBegin(Pool pool, Buffer buf, Bool collectAll);
EXTERN void PoolNoRampEnd(Pool pool, Buffer buf);
EXTERN void PoolTrivRampEnd(Pool pool, Buffer buf);
EXTERN Res PoolNoFramePush(AllocFrame *frameReturn, Pool pool, Buffer buf);
EXTERN Res PoolTrivFramePush(AllocFrame *frameReturn, Pool pool, Buffer buf);
EXTERN Res PoolNoFramePop(Pool pool, Buffer buf, AllocFrame frame);
EXTERN Res PoolTrivFramePop(Pool pool, Buffer buf, AllocFrame frame);
EXTERN void PoolNoFramePopPending(Pool pool, Buffer buf, AllocFrame frame);
EXTERN void PoolNoWalk(Pool pool, Seg seg, FormattedObjectsStepMethod step,
                       void *p, size_t s);
EXTERN void PoolNoFreeWalk(Pool pool, FreeBlockStepMethod f, void *p);
EXTERN PoolDebugMixin PoolNoDebugMixin(Pool pool);
EXTERN BufferClass PoolNoBufferClass(void);

#define ClassOfPool(pool) ((pool)->class)
#define SuperclassOfPool(pool) \
  ((PoolClass)ProtocolClassSuperclassPoly((pool)->class))


/* Abstract Pool Classes Interface -- see <code/poolabs.c> */
EXTERN void PoolClassMixInAllocFree(PoolClass class);
EXTERN void PoolClassMixInBuffer(PoolClass class);
EXTERN void PoolClassMixInScan(PoolClass class);
EXTERN void PoolClassMixInFormat(PoolClass class);
EXTERN void PoolClassMixInCollect(PoolClass class);
EXTERN AbstractPoolClass AbstractPoolClassGet(void);
EXTERN AbstractAllocFreePoolClass AbstractAllocFreePoolClassGet(void);
EXTERN AbstractBufferPoolClass AbstractBufferPoolClassGet(void);
EXTERN AbstractBufferPoolClass AbstractSegBufPoolClassGet(void);
EXTERN AbstractScanPoolClass AbstractScanPoolClassGet(void);
EXTERN AbstractCollectPoolClass AbstractCollectPoolClassGet(void);

/* DEFINE_POOL_CLASS
 *
 * Convenience macro -- see <design/protocol/#int.define-special>. */

#define DEFINE_POOL_CLASS(className, var) \
  DEFINE_ALIAS_CLASS(className, PoolClass, var)

#define POOL_SUPERCLASS(className) \
  ((PoolClass)SUPERCLASS(className))


/* Message Interface -- see <design/message/> */
/* -- Internal (MPM) Interface -- functions for message originator */
EXTERN Bool MessageCheck(Message message);
EXTERN Bool MessageClassCheck(MessageClass class);
EXTERN Bool MessageTypeCheck(MessageType type);
EXTERN void MessageInit(Arena arena, Message message,
                        MessageClass class, MessageType type);
EXTERN void MessageFinish(Message message);
EXTERN Arena MessageArena(Message message);
EXTERN Bool MessageOnQueue(Message message);
EXTERN void MessagePost(Arena arena, Message message);
EXTERN void MessageEmpty(Arena arena);
/* -- Delivery (Client) Interface -- functions for recipient */
EXTERN void MessageTypeEnable(Arena arena, MessageType type);
EXTERN void MessageTypeDisable(Arena arena, MessageType type);
EXTERN Bool MessagePoll(Arena arena);
EXTERN Bool MessageQueueType(MessageType *typeReturn, Arena arena);
EXTERN Bool MessageGet(Message *messageReturn, Arena arena,
                       MessageType type);
EXTERN void MessageDiscard(Arena arena, Message message);
/* -- Message Methods, Generic */
EXTERN MessageType MessageGetType(Message message);
EXTERN MessageClass MessageGetClass(Message message);
EXTERN Clock MessageGetClock(Message message);
/* -- Message Method Dispatchers, Type-specific */
EXTERN void MessageFinalizationRef(Ref *refReturn,
                                   Arena arena, Message message);
EXTERN Size MessageGCLiveSize(Message message);
EXTERN Size MessageGCCondemnedSize(Message message);
EXTERN Size MessageGCNotCondemnedSize(Message message);
EXTERN const char *MessageGCStartWhy(Message message);
/* -- Message Method Stubs, Type-specific */
EXTERN void MessageNoFinalizationRef(Ref *refReturn,
                                     Arena arena, Message message);
EXTERN Size MessageNoGCLiveSize(Message message);
EXTERN Size MessageNoGCCondemnedSize(Message message);
EXTERN Size MessageNoGCNotCondemnedSize(Message message);
EXTERN const char *MessageNoGCStartWhy(Message message);


/* Trace Interface -- see <code/trace.c> */

#define TraceSetSingle(trace)       BS_SINGLE(TraceSet, (trace)->ti)
#define TraceSetIsSingle(ts)        BS_IS_SINGLE(ts)
#define TraceSetIsMember(ts, trace) BS_IS_MEMBER(ts, (trace)->ti)
#define TraceSetAdd(ts, trace)      BS_ADD(TraceSet, ts, (trace)->ti)
#define TraceSetDel(ts, trace)      BS_DEL(TraceSet, ts, (trace)->ti)
#define TraceSetUnion(ts1, ts2)     BS_UNION(ts1, ts2)
#define TraceSetInter(ts1, ts2)     BS_INTER(ts1, ts2)
#define TraceSetDiff(ts1, ts2)      BS_DIFF(ts1, ts2)
#define TraceSetSuper(ts1, ts2)     BS_SUPER(ts1, ts2)
#define TraceSetSub(ts1, ts2)       BS_SUB(ts1, ts2)
#define TraceSetComp(ts)            BS_COMP(ts)

#define TRACE_SET_ITER(ti, trace, ts, arena) \
  for(ti = 0, trace = ArenaTrace(arena, ti); ti < TraceLIMIT; \
      ++ti, trace = ArenaTrace(arena, ti)) BEGIN \
    if (TraceSetIsMember(ts, trace)) {

#define TRACE_SET_ITER_END(ti, trace, ts, arena) } END


EXTERN void ScanStateInit(ScanState ss, TraceSet ts, Arena arena,
                          Rank rank, ZoneSet white);
EXTERN void ScanStateFinish(ScanState ss);
EXTERN Bool ScanStateCheck(ScanState ss);
EXTERN void ScanStateSetSummary(ScanState ss, RefSet summary);
EXTERN RefSet ScanStateSummary(ScanState ss);

/* See impl.h.mpmst.ss */
#define ScanStateZoneShift(ss)             ((Shift)(ss)->ss_s._zs)
#define ScanStateWhite(ss)                 ((ZoneSet)(ss)->ss_s._w)
#define ScanStateUnfixedSummary(ss)        ((RefSet)(ss)->ss_s._ufs)
#define ScanStateSetZoneShift(ss, shift)   ((void)((ss)->ss_s._zs = (shift)))
#define ScanStateSetWhite(ss, zs)          ((void)((ss)->ss_s._w = (zs)))
#define ScanStateSetUnfixedSummary(ss, rs) ((void)((ss)->ss_s._ufs = (rs)))

EXTERN Bool TraceIdCheck(TraceId id);
EXTERN Bool TraceSetCheck(TraceSet ts);
EXTERN Bool TraceCheck(Trace trace);
EXTERN Res TraceCreate(Trace *traceReturn, Arena arena, int why);
EXTERN void TraceDestroy(Trace trace);

EXTERN Res TraceAddWhite(Trace trace, Seg seg);
EXTERN Res TraceCondemnZones(Trace trace, ZoneSet condemnedSet);
EXTERN Res TraceStart(Trace trace, double mortality, double finishingTime);
EXTERN Size TracePoll(Globals globals);

EXTERN Rank TraceRankForAccess(Arena arena, Seg seg);
EXTERN void TraceSegAccess(Arena arena, Seg seg, AccessSet mode);

EXTERN void TraceQuantum(Trace trace);
EXTERN Res TraceStartCollectAll(Trace *traceReturn, Arena arena, int why);

/* traceanc.c -- Trace Ancillary */

EXTERN Bool TraceStartMessageCheck(TraceStartMessage message);
EXTERN const char *TraceStartWhyToString(int why);
EXTERN void TracePostStartMessage(Trace trace);
EXTERN Bool TraceMessageCheck(TraceMessage message);  /* trace end */
EXTERN void TracePostMessage(Trace trace);  /* trace end */
EXTERN Bool TraceIdMessagesCheck(Arena arena, TraceId ti);
EXTERN Res TraceIdMessagesCreate(Arena arena, TraceId ti);
EXTERN void TraceIdMessagesDestroy(Arena arena, TraceId ti);

/* Collection control parameters */

EXTERN double TraceTopGenMortality;
EXTERN double TraceWorkFactor;


/* Equivalent to <code/mps.h> MPS_SCAN_BEGIN */

#define TRACE_SCAN_BEGIN(ss) \
  BEGIN \
    /* Check range on zoneShift before casting to Shift. */ \
    AVER(ScanStateZoneShift(ss) < MPS_WORD_WIDTH); \
    { \
      Shift SCANzoneShift = ScanStateZoneShift(ss); \
      ZoneSet SCANwhite = ScanStateWhite(ss); \
      RefSet SCANsummary = ScanStateUnfixedSummary(ss); \
      Word SCANt; \
      mps_addr_t SCANref; \
      Res SCANres; \
      {

/* Equivalent to <code/mps.h> MPS_FIX1 */

#define TRACE_FIX1(ss, ref) \
  (SCANt = (Word)1 << ((Word)(ref) >> SCANzoneShift & (MPS_WORD_WIDTH-1)), \
   SCANsummary |= SCANt, \
   (SCANwhite & SCANt) != 0)

/* Equivalent to <code/mps.h> MPS_FIX2 */

/* TODO: The ref is copied to avoid breaking strict aliasing rules that could
   well affect optimised scan loops.  This code could be improved by
   returning the fixed ref as a result and using longjmp to signal errors,
   and that might well improve all scan loops too.  The problem is whether
   some embedded client platforms support longjmp.  RB 2012-09-07 */

#define TRACE_FIX2(ss, refIO) \
  (SCANref = (mps_addr_t)*(refIO), \
   SCANres = _mps_fix2(&(ss)->ss_s, &SCANref), \
   *(refIO) = SCANref, \
   SCANres)

/* Equivalent to <code/mps.h> MPS_FIX */

#define TRACE_FIX(ss, refIO) \
  (TRACE_FIX1(ss, *(refIO)) ? TRACE_FIX2(ss, refIO) : ResOK)

/* Equivalent to <code/mps.h> MPS_SCAN_END */

#define TRACE_SCAN_END(ss) \
      } \
      ScanStateSetUnfixedSummary(ss, SCANsummary); \
    } \
  END

EXTERN Res TraceScanArea(ScanState ss, Addr *base, Addr *limit);
EXTERN Res TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit);
EXTERN Res TraceScanAreaMasked(ScanState ss,
                               Addr *base, Addr *limit, Word mask);
EXTERN void TraceScanSingleRef(TraceSet ts, Rank rank, Arena arena,
                               Seg seg, Ref *refIO);


/* Arena Interface -- see <code/arena.c> */

/* DEFINE_ARENA_CLASS
 *
 * Convenience macro -- see <design/protocol/#int.define-special>. */

#define DEFINE_ARENA_CLASS(className, var) \
  DEFINE_ALIAS_CLASS(className, ArenaClass, var)

#define ARENA_SUPERCLASS(className) \
  ((ArenaClass)SUPERCLASS(className))

EXTERN AbstractArenaClass AbstractArenaClassGet(void);
EXTERN Bool ArenaClassCheck(ArenaClass class);

EXTERN Bool ArenaCheck(Arena arena);
EXTERN Res ArenaCreateV(Arena *arenaReturn, ArenaClass class, va_list args);
EXTERN void ArenaDestroy(Arena arena);
EXTERN Res ArenaInit(Arena arena, ArenaClass class);
EXTERN void ArenaFinish(Arena arena);
EXTERN Res ArenaDescribe(Arena arena, mps_lib_FILE *stream);
EXTERN Res ArenaDescribeTracts(Arena arena, mps_lib_FILE *stream);
EXTERN Bool ArenaAccess(Addr addr, AccessSet mode, MutatorFaultContext context);

EXTERN Bool GlobalsCheck(Globals arena);
EXTERN Res GlobalsInit(Globals arena);
EXTERN void GlobalsFinish(Globals arena);
EXTERN Res GlobalsCompleteCreate(Globals arenaGlobals);
EXTERN void GlobalsPrepareToDestroy(Globals arenaGlobals);
EXTERN Res GlobalsDescribe(Globals arena, mps_lib_FILE *stream);
EXTERN Ring GlobalsRememberedSummaryRing(Globals);

#define ArenaGlobals(arena) (&(arena)->globals)
#define GlobalsArena(glob) PARENT(ArenaStruct, globals, glob)

#define ArenaRootRing(arena)    (&(arena)->rootRing)
#define ArenaTraceRing(arena)   (&(arena)->traceRing)
#define ArenaThreadRing(arena)  (&(arena)->threadRing)
#define ArenaEpoch(arena)       ((arena)->epoch) /* .epoch.ts */
#define ArenaTrace(arena, ti)   (&(arena)->trace[ti])
#define ArenaZoneShift(arena)   ((arena)->zoneShift)
#define ArenaAlign(arena)       ((arena)->alignment)
#define ArenaGreyRing(arena, rank) (&(arena)->greyRing[rank])


EXTERN void (ArenaEnter)(Arena arena);
EXTERN void (ArenaLeave)(Arena arena);

#if defined(THREAD_SINGLE) && defined(PROTECTION_NONE)
#define ArenaEnter(arena)  UNUSED(arena)
#define ArenaLeave(arena)  UNUSED(arena)
#endif

EXTERN void ArenaEnterRecursive(Arena arena);
EXTERN void ArenaLeaveRecursive(Arena arena);

EXTERN void (ArenaPoll)(Globals globals);
#ifdef MPS_PROD_EPCORE
#define ArenaPoll(globals)  UNUSED(globals)
#endif
/* .nogc.why: ScriptWorks doesn't use MM-provided incremental GC, so */
/* doesn't need to poll when allocating. */

EXTERN Bool (ArenaStep)(Globals globals, double interval, double multiplier);
EXTERN void ArenaClamp(Globals globals);
EXTERN void ArenaRelease(Globals globals);
EXTERN void ArenaPark(Globals globals);
EXTERN void ArenaExposeRemember(Globals globals, int remember);
EXTERN void ArenaRestoreProtection(Globals globals);
EXTERN Res ArenaStartCollect(Globals globals, int why);
EXTERN Res ArenaCollect(Globals globals, int why);
EXTERN Bool ArenaHasAddr(Arena arena, Addr addr);

EXTERN void ArenaSetEmergency(Arena arena, Bool emergency);
EXTERN Bool ArenaEmergency(Arena arean);

EXTERN Res ControlInit(Arena arena);
EXTERN void ControlFinish(Arena arena);
EXTERN Res ControlAlloc(void **baseReturn, Arena arena, size_t size,
                        Bool withReservoirPermit);
EXTERN void ControlFree(Arena arena, void *base, size_t size);
EXTERN Res ControlDescribe(Arena arena, mps_lib_FILE *stream);


/* Peek/Poke
 *
 * These are provided so that modules in the MPS can make occasional
 * access to client data.  They perform the appropriate shield and
 * summary manipulations that are necessary.
 *
 * Note that Peek and Poke can be called with address that may or
 * may not be in arena managed memory.  */

/* Peek reads a value */
EXTERN Ref ArenaPeek(Arena arena, Ref *p);
/* Same, but p must be in seg */
EXTERN Ref ArenaPeekSeg(Arena arena, Seg seg, Ref *p);
/* Poke stores a value */
EXTERN void ArenaPoke(Arena arena, Ref *p, Ref ref);
/* Same, but p must be in seg */
EXTERN void ArenaPokeSeg(Arena arena, Seg seg, Ref *p, Ref ref);


/* Read/Write
 *
 * These simulate mutator reads and writes to locations.
 * They are effectively a software barrier, and maintain the tricolor
 * invariant (hence performing any scanning or color manipulation
 * necessary).
 *
 * Only Read provided right now.  */

Ref ArenaRead(Arena arena, Ref *p);


EXTERN Size ArenaReserved(Arena arena);
EXTERN Size ArenaCommitted(Arena arena);
EXTERN Size ArenaSpareCommitted(Arena arena);

EXTERN Size ArenaCommitLimit(Arena arena);
EXTERN Res ArenaSetCommitLimit(Arena arena, Size limit);
EXTERN Size ArenaSpareCommitLimit(Arena arena);
EXTERN void ArenaSetSpareCommitLimit(Arena arena, Size limit);
EXTERN void ArenaNoSpareCommitExceeded(Arena arena);

EXTERN double ArenaMutatorAllocSize(Arena arena);
EXTERN Size ArenaAvail(Arena arena);

EXTERN Res ArenaExtend(Arena, Addr base, Size size);

EXTERN void ArenaCompact(Arena arena, Trace trace);

EXTERN Res ArenaFinalize(Arena arena, Ref obj);
EXTERN Res ArenaDefinalize(Arena arena, Ref obj);

EXTERN Bool ArenaIsReservedAddr(Arena arena, Addr addr);

#define ArenaReservoir(arena) (&(arena)->reservoirStruct)

EXTERN Bool ReservoirCheck(Reservoir reservoir);
EXTERN Res ReservoirInit(Reservoir reservoir, Arena arena);
EXTERN void ReservoirFinish (Reservoir reservoir);
EXTERN Size ReservoirLimit(Reservoir reservoir);
EXTERN void ReservoirSetLimit(Reservoir reservoir, Size size);
EXTERN Size ReservoirAvailable(Reservoir reservoir);
EXTERN Res ReservoirEnsureFull(Reservoir reservoir);
EXTERN void ReservoirDeposit(Reservoir reservoir, Addr base, Size size);
EXTERN Res ReservoirWithdraw(Addr *baseReturn, Tract *baseTractReturn,
                             Reservoir reservoir, Size size, Pool pool);

EXTERN Res ArenaAlloc(Addr *baseReturn, SegPref pref,
                      Size size, Pool pool, Bool withReservoirPermit);
EXTERN void ArenaFree(Addr base, Size size, Pool pool);

EXTERN Res ArenaNoExtend(Arena arena, Addr base, Size size);


/* Locus interface */

EXTERN Bool SegPrefCheck(SegPref pref);
EXTERN SegPref SegPrefDefault(void);
EXTERN void SegPrefExpress(SegPref pref, SegPrefKind kind, void *p);

EXTERN void LocusInit(Arena arena);
EXTERN void LocusFinish(Arena arena);
EXTERN Bool LocusCheck(Arena arena);


/* Segment interface */

EXTERN Res SegAlloc(Seg *segReturn, SegClass class, SegPref pref,
                    Size size, Pool pool, Bool withReservoirPermit, ...);
EXTERN void SegFree(Seg seg);
EXTERN Bool SegOfAddr(Seg *segReturn, Arena arena, Addr addr);
EXTERN Bool SegFirst(Seg *segReturn, Arena arena);
EXTERN Bool SegNext(Seg *segReturn, Arena arena, Addr addr);
EXTERN void SegSetWhite(Seg seg, TraceSet white);
EXTERN void SegSetGrey(Seg seg, TraceSet grey);
EXTERN void SegSetRankSet(Seg seg, RankSet rankSet);
EXTERN void SegSetRankAndSummary(Seg seg, RankSet rankSet, RefSet summary);
EXTERN Res SegMerge(Seg *mergedSegReturn, Seg segLo, Seg segHi,
                    Bool withReservoirPermit, ...);
EXTERN Res SegSplit(Seg *segLoReturn, Seg *segHiReturn, Seg seg, Addr at,
                    Bool withReservoirPermit, ...);
EXTERN Res SegDescribe(Seg seg, mps_lib_FILE *stream);
EXTERN void SegSetSummary(Seg seg, RefSet summary);
EXTERN Buffer SegBuffer(Seg seg);
EXTERN void SegSetBuffer(Seg seg, Buffer buffer);
EXTERN Bool SegCheck(Seg seg);
EXTERN Bool GCSegCheck(GCSeg gcseg);
EXTERN Bool SegClassCheck(SegClass class);
EXTERN SegClass SegClassGet(void);
EXTERN SegClass GCSegClassGet(void);
EXTERN void SegClassMixInNoSplitMerge(SegClass class);


/* DEFINE_SEG_CLASS -- define a segment class */

#define DEFINE_SEG_CLASS(className, var) \
  DEFINE_ALIAS_CLASS(className, SegClass, var)


#define SEG_SUPERCLASS(className) \
  ((SegClass)SUPERCLASS(className))

#define ClassOfSeg(seg) ((seg)->class)

EXTERN Size SegSize(Seg seg);
EXTERN Addr (SegBase)(Seg seg);
EXTERN Addr (SegLimit)(Seg seg);
#define SegBase(seg)            (TractBase((seg)->firstTract))
#define SegLimit(seg)           ((seg)->limit)
#define SegPool(seg)            (TractPool((seg)->firstTract))
/* .bitfield.promote: The bit field accesses need to be cast to the */
/* right type, otherwise they'll be promoted to signed int, see */
/* standard.ansic.6.2.1.1. */
#define SegRankSet(seg)         ((RankSet)(seg)->rankSet)
#define SegPM(seg)              ((AccessSet)(seg)->pm)
#define SegSM(seg)              ((AccessSet)(seg)->sm)
#define SegDepth(seg)           ((unsigned)(seg)->depth)
#define SegGrey(seg)            ((TraceSet)(seg)->grey)
#define SegWhite(seg)           ((TraceSet)(seg)->white)
#define SegNailed(seg)          ((TraceSet)(seg)->nailed)
#define SegOfPoolRing(node)     (RING_ELT(Seg, poolRing, (node)))
#define SegOfGreyRing(node)     (&(RING_ELT(GCSeg, greyRing, (node)) \
                                   ->segStruct))

#define SegSummary(seg)         (((GCSeg)(seg))->summary)

#define SegSetPM(seg, mode)     ((void)((seg)->pm = (mode)))
#define SegSetSM(seg, mode)     ((void)((seg)->sm = (mode)))
#define SegSetDepth(seg, d)     ((void)((seg)->depth = (d)))
#define SegSetNailed(seg, ts)   ((void)((seg)->nailed = (ts)))


/* Buffer Interface -- see <code/buffer.c> */

EXTERN Res BufferCreate(Buffer *bufferReturn, BufferClass class,
                        Pool pool, Bool isMutator, ...);
EXTERN Res BufferCreateV(Buffer *bufferReturn, BufferClass class,
                         Pool pool, Bool isMutator, va_list args);
EXTERN void BufferDestroy(Buffer buffer);
EXTERN Bool BufferCheck(Buffer buffer);
EXTERN Bool SegBufCheck(SegBuf segbuf);
EXTERN Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream);
EXTERN Res BufferReserve(Addr *pReturn, Buffer buffer, Size size,
                         Bool withReservoirPermit);
/* macro equivalent for BufferReserve, keep in sync with <code/buffer.c> */
/* TODO: Perhaps this isn't really necessary now that we build the MPS with
   more global optimisation and inlining. RB 2012-09-07 */
#define BUFFER_RESERVE(pReturn, buffer, size, withReservoirPermit) \
  (AddrAdd(BufferAlloc(buffer), size) > BufferAlloc(buffer) && \
   AddrAdd(BufferAlloc(buffer), size) <= (Addr)BufferAP(buffer)->limit ? \
     (*(pReturn) = BufferAlloc(buffer), \
      BufferAP(buffer)->alloc = AddrAdd(BufferAlloc(buffer), size), \
      ResOK) : \
   BufferFill(pReturn, buffer, size, withReservoirPermit))

EXTERN Res BufferFill(Addr *pReturn, Buffer buffer, Size size,
                      Bool withReservoirPermit);

EXTERN Bool BufferCommit(Buffer buffer, Addr p, Size size);
/* macro equivalent for BufferCommit, keep in sync with <code/buffer.c> */
/* TODO: Perhaps this isn't really necessary now that we build the MPS with
 more global optimisation and inlining. RB 2012-09-07 */
#define BUFFER_COMMIT(buffer, p, size) \
  (BufferAP(buffer)->init = BufferAlloc(buffer), \
   BufferAP(buffer)->limit != 0 || BufferTrip(buffer, p, size))

EXTERN Bool BufferTrip(Buffer buffer, Addr p, Size size);
EXTERN void BufferFinish(Buffer buffer);
EXTERN Bool BufferIsReset(Buffer buffer);
EXTERN Bool BufferIsReady(Buffer buffer);
EXTERN Bool BufferIsMutator(Buffer buffer);
EXTERN void BufferSetAllocAddr(Buffer buffer, Addr addr);
EXTERN void BufferAttach(Buffer buffer,
                         Addr base, Addr limit, Addr init, Size size);
EXTERN void BufferDetach(Buffer buffer, Pool pool);
EXTERN void BufferFlip(Buffer buffer);

EXTERN mps_ap_t (BufferAP)(Buffer buffer);
#define BufferAP(buffer)        (&(buffer)->ap_s)
EXTERN Buffer BufferOfAP(mps_ap_t ap);
#define BufferOfAP(ap)          PARENT(BufferStruct, ap_s, ap)

#define BufferArena(buffer) ((buffer)->arena)
#define BufferPool(buffer)  ((buffer)->pool)

EXTERN Seg BufferSeg(Buffer buffer);

EXTERN RankSet BufferRankSet(Buffer buffer);
EXTERN void BufferSetRankSet(Buffer buffer, RankSet rankset);

#define BufferBase(buffer)      ((buffer)->base)
#define BufferGetInit(buffer) /* see .trans.bufferinit */ \
  ((Addr)(BufferAP(buffer)->init))
#define BufferAlloc(buffer)     ((Addr)(BufferAP(buffer)->alloc))
#define BufferLimit(buffer)     ((buffer)->poolLimit)
EXTERN Addr BufferScanLimit(Buffer buffer);

EXTERN void BufferReassignSeg(Buffer buffer, Seg seg);

EXTERN Bool BufferIsTrapped(Buffer buffer);
EXTERN Bool BufferIsTrappedByMutator(Buffer buffer);

EXTERN void BufferRampBegin(Buffer buffer, AllocPattern pattern);
EXTERN Res BufferRampEnd(Buffer buffer);
EXTERN void BufferRampReset(Buffer buffer);

EXTERN Res BufferFramePush(AllocFrame *frameReturn, Buffer buffer);
EXTERN Res BufferFramePop(Buffer buffer, AllocFrame frame);
EXTERN FrameState BufferFrameState(Buffer buffer);
EXTERN void BufferFrameSetState(Buffer buffer, FrameState state);


/* DEFINE_BUFFER_CLASS -- define a buffer class */

#define DEFINE_BUFFER_CLASS(className, var) \
  DEFINE_ALIAS_CLASS(className, BufferClass, var)

#define BUFFER_SUPERCLASS(className) \
  ((BufferClass)SUPERCLASS(className))

EXTERN Bool BufferClassCheck(BufferClass class);
EXTERN BufferClass BufferClassGet(void);
EXTERN BufferClass SegBufClassGet(void);
EXTERN BufferClass RankBufClassGet(void);

EXTERN AllocPattern AllocPatternRamp(void);
EXTERN AllocPattern AllocPatternRampCollectAll(void);


/* Format Interface -- see <code/format.c> */

EXTERN Bool FormatCheck(Format format);
EXTERN Res FormatCreate(Format *formatReturn, Arena arena,
                        Align alignment,
                        FormatVariety variety,
                        mps_fmt_scan_t scan,
                        mps_fmt_skip_t skip,
                        mps_fmt_fwd_t move,
                        mps_fmt_isfwd_t isMoved,
                        mps_fmt_copy_t copy,
                        mps_fmt_pad_t pad,
                        mps_fmt_class_t class,
                        Size headerSize);
EXTERN void FormatDestroy(Format format);
EXTERN Arena FormatArena(Format format);
EXTERN Res FormatDescribe(Format format, mps_lib_FILE *stream);


/* Reference Interface -- see <code/ref.c> */

EXTERN Bool RankCheck(Rank rank);
EXTERN Bool RankSetCheck(RankSet rankSet);

#define RankSetIsMember(rs, r)  BS_IS_MEMBER((rs), (r))
#define RankSetSingle(r)        BS_SINGLE(RankSet, (r))
#define RankSetIsSingle(r)      BS_IS_SINGLE(r)
#define RankSetUnion(rs1, rs2)  BS_UNION((rs1), (rs2))
#define RankSetDel(rs, r)       BS_DEL(RankSet, (rs), (r))

#define AddrZone(arena, addr) \
  (((Word)(addr) >> (arena)->zoneShift) & (MPS_WORD_WIDTH - 1))

#define RefSetUnion(rs1, rs2)   BS_UNION((rs1), (rs2))
#define RefSetInter(rs1, rs2)   BS_INTER((rs1), (rs2))
#define RefSetDiff(rs1, rs2)    BS_DIFF((rs1), (rs2))
#define RefSetAdd(arena, rs, addr) \
  BS_ADD(RefSet, rs, AddrZone(arena, addr))
#define RefSetIsMember(arena, rs, addr) \
  BS_IS_MEMBER(rs, AddrZone(arena, addr))
#define RefSetSuper(rs1, rs2)   BS_SUPER((rs1), (rs2))
#define RefSetSub(rs1, rs2)     BS_SUB((rs1), (rs2))


/* Zone sets -- see design.mps.refset */

#define ZoneSetUnion(zs1, zs2) BS_UNION(zs1, zs2)
#define ZoneSetInter(zs1, zs2) BS_INTER(zs1, zs2)
#define ZoneSetDiff(zs1, zs2)  BS_DIFF(zs1, zs2)
#define ZoneSetAdd(arena, zs, addr) \
  BS_ADD(ZoneSet, zs, AddrZone(arena, addr))
#define ZoneSetIsMember(arena, zs, addr) \
  BS_IS_MEMBER(zs, AddrZone(arena, addr))
#define ZoneSetSub(zs1, zs2)   BS_SUB(zs1, zs2)
#define ZoneSetSuper(zs1, zs2) BS_SUPER(zs1, zs2)
#define ZoneSetComp(zs)        BS_COMP(zs)

EXTERN ZoneSet ZoneSetOfRange(Arena arena, Addr base, Addr limit);
EXTERN ZoneSet ZoneSetOfSeg(Arena arena, Seg seg);


/* Shield Interface -- see <code/shield.c> */

EXTERN void (ShieldRaise)(Arena arena, Seg seg, AccessSet mode);
EXTERN void (ShieldLower)(Arena arena, Seg seg, AccessSet mode);
EXTERN void (ShieldEnter)(Arena arena);
EXTERN void (ShieldLeave)(Arena arena);
EXTERN void (ShieldExpose)(Arena arena, Seg seg);
EXTERN void (ShieldCover)(Arena arena, Seg seg);
EXTERN void (ShieldSuspend)(Arena arena);
EXTERN void (ShieldResume)(Arena arena);
EXTERN void (ShieldFlush)(Arena arena);

#if defined(THREAD_SINGLE) && defined(PROTECTION_NONE)
#define ShieldRaise(arena, seg, mode) \
  BEGIN UNUSED(arena); UNUSED(seg); UNUSED(mode); END
#define ShieldLower(arena, seg, mode) \
  BEGIN UNUSED(arena); UNUSED(seg); UNUSED(mode); END
#define ShieldEnter(arena) BEGIN UNUSED(arena); END
#define ShieldLeave(arena) BEGIN UNUSED(arena); END
#define ShieldExpose(arena, seg)  \
  BEGIN UNUSED(arena); UNUSED(seg); END
#define ShieldCover(arena, seg) \
  BEGIN UNUSED(arena); UNUSED(seg); END
#define ShieldSuspend(arena) BEGIN UNUSED(arena); END
#define ShieldResume(arena) BEGIN UNUSED(arena); END
#define ShieldFlush(arena) BEGIN UNUSED(arena); END
#endif


/* Protection Interface
 *
 * See <design/prot/> for the design of the generic interface including
 * the contracts for these functions.
 *
 * This interface has several different implementations, typically one
 * per platform, see <code/prot.c>* for the various implementations, and
 * <design/prot/>* for the corresponding designs. */

EXTERN void ProtSetup(void);

EXTERN void ProtSet(Addr base, Addr limit, AccessSet mode);
EXTERN void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
                      void *p, size_t s);
EXTERN void ProtSync(Arena arena);
EXTERN Bool ProtCanStepInstruction(MutatorFaultContext context);
EXTERN Res ProtStepInstruction(MutatorFaultContext context);


/* Mutator Fault Context */

EXTERN Addr MutatorFaultContextSP(MutatorFaultContext mfc);
EXTERN Res MutatorFaultContextScan(ScanState ss, MutatorFaultContext mfc);


/* Location Dependency -- see <code/ld.c> */

EXTERN void LDReset(mps_ld_t ld, Arena arena);
EXTERN void LDAdd(mps_ld_t ld, Arena arena, Addr addr);
EXTERN Bool LDIsStale(mps_ld_t ld, Arena arena, Addr addr);
EXTERN void LDAge(Arena arena, RefSet moved);
EXTERN void LDMerge(mps_ld_t ld, Arena arena, mps_ld_t from);


/* Root Interface -- see <code/root.c> */

EXTERN Res RootCreateTable(Root *rootReturn, Arena arena,
                           Rank rank, RootMode mode,
                           Addr *base, Addr *limit);
EXTERN Res RootCreateTableMasked(Root *rootReturn, Arena arena,
                                 Rank rank, RootMode mode,
                                 Addr *base, Addr *limit,
                                 Word mask);
EXTERN Res RootCreateReg(Root *rootReturn, Arena arena,
                           Rank rank, Thread thread,
                           mps_reg_scan_t scan,
                           void *p, size_t s);
EXTERN Res RootCreateFmt(Root *rootReturn, Arena arena,
                           Rank rank, RootMode mode,
                           mps_fmt_scan_t scan,
                           Addr base, Addr limit);
EXTERN Res RootCreateFun(Root *rootReturn, Arena arena,
                        Rank rank, mps_root_scan_t scan,
                        void *p, size_t s);
EXTERN void RootDestroy(Root root);
EXTERN Bool RootModeCheck(RootMode mode);
EXTERN Bool RootCheck(Root root);
EXTERN Res RootDescribe(Root root, mps_lib_FILE *stream);
EXTERN Res RootsDescribe(Globals arenaGlobals, mps_lib_FILE *stream);
EXTERN Rank RootRank(Root root);
EXTERN AccessSet RootPM(Root root);
EXTERN RefSet RootSummary(Root root);
EXTERN void RootGrey(Root root, Trace trace);
EXTERN Res RootScan(ScanState ss, Root root);
EXTERN Arena RootArena(Root root);
EXTERN Bool RootOfAddr(Root *root, Arena arena, Addr addr);
EXTERN void RootAccess(Root root, AccessSet mode);
typedef Res (*RootIterateFn)(Root root, void *p);
EXTERN Res RootsIterate(Globals arena, RootIterateFn f, void *p);


/* VM Interface -- see <code/vm.c>* */

EXTERN Align VMAlign(VM vm);
EXTERN Bool VMCheck(VM vm);
EXTERN Res VMCreate(VM *VMReturn, Size size);
EXTERN void VMDestroy(VM vm);
EXTERN Addr VMBase(VM vm);
EXTERN Addr VMLimit(VM vm);
EXTERN Res VMMap(VM vm, Addr base, Addr limit);
EXTERN void VMUnmap(VM vm, Addr base, Addr limit);
EXTERN Size VMReserved(VM vm);
EXTERN Size VMMapped(VM vm);


/* Stack Probe */

EXTERN void StackProbe(Size depth);


/* STATISTIC -- gather statistics (in some varieties)
 *
 * The argument of STATISTIC is an expression; the expansion followed by
 * a semicolon is syntactically a statement.
 *
 * The argument of STATISTIC_STAT is a statement; the expansion followed by
 * a semicolon is syntactically a statement.
 *
 * STATISTIC_WRITE is inserted in WriteF arguments to output the values
 * of statistic fields.
 *
 * .statistic.whitehot: The implementation of STATISTIC for
 * non-statistical varieties passes the parameter to DISCARD to ensure
 * the parameter is syntactically an expression.  The parameter is
 * passed as part of a comma-expression so that its type is not
 * important.  This permits an expression of type void.  */

#if defined(STATISTICS)

#define STATISTIC(gather) BEGIN (gather); END
#define STATISTIC_STAT(gather) BEGIN gather; END
#define STATISTIC_WRITE(format, arg) (format), (arg),

#elif defined(STATISTICS_NONE)

#define STATISTIC(gather) DISCARD(((gather), 0))
#define STATISTIC_STAT DISCARD_STAT
#define STATISTIC_WRITE(format, arg)

#else

#error "No statistics configured."

#endif


/* ------------ DIAG_WITH_STREAM_AND_WRITEF --------------- */

Bool DiagIsOn(void);
mps_lib_FILE *DiagStream(void);


/* Diag*F functions -- formatted diagnostic output
 *
 * Note: do not call these directly; use the DIAG_*F macros below.
 */

EXTERN void DiagSingleF(const char *tag, ...);
EXTERN void DiagFirstF(const char *tag, ...);
EXTERN void DiagMoreF(const char *format, ...);
EXTERN void DiagEnd(const char *tag);


#if defined(DIAG_WITH_STREAM_AND_WRITEF)

/* Diagnostic Calculation and Output */
#define DIAG_DECL(decl) decl
#define DIAG_STREAM (DiagStream())
#define DIAG(s) BEGIN \
    s \
  END


/* DIAG_*F macros -- formatted diagnostic output
 *
 * Note: when invoking these macros, the value passed as macro 
 * argument "args" might contain commas; it must therefore be 
 * enclosed in parentheses.  That makes these macros unclean in 
 * all sorts of ways.
 */

#define DIAG_WRITEF(args) DIAG( \
  if(DiagIsOn()) { \
    WriteF args; \
  } \
)
#define DIAG_SINGLEF(args) DIAG( \
  DiagSingleF args; \
)
#define DIAG_FIRSTF(args) DIAG( \
  DiagFirstF args; \
)
#define DIAG_MOREF(args) DIAG( \
  DiagMoreF args; \
)

/* Note: extra parens *not* required when invoking DIAG_END */
#define DIAG_END(tag) DIAG( \
  DiagEnd(tag); \
)


#else

/* Diagnostic Calculation and Output */
#define DIAG_DECL(decl)
#define DIAG(s) BEGIN END
#define DIAG_WRITEF(args) BEGIN END

/* DIAG_*F macros */
#define DIAG_SINGLEF(args) BEGIN END
#define DIAG_FIRSTF(args) BEGIN END
#define DIAG_MOREF(args) BEGIN END
#define DIAG_END(tag) BEGIN END

#endif

/* ------------ DIAG_WITH_PRINTF --------------- */

#if defined(DIAG_WITH_PRINTF)

#include <stdio.h>

#define DIAG_PRINTF(args) BEGIN\
  printf args ; \
  END

#else

#define DIAG_PRINTF(args) BEGIN\
  END

#endif


#endif /* mpm_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2003, 2008 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
