/* impl.h.eventdef -- Event Logging Definitions
 *
 * $HopeName: MMsrc!eventdef.h(MMdevel_gavinm_160033.3) $
 * Copyright (C) 1997, 1998 Harlequin Group plc, all rights reserved.
 *
 * .readership: MPS developers.
 * .source: design.mps.telemetry
 *
 * .desc: This file declares relationships that define the various 
 * event types.  It is intended to be used with clever definitions 
 * of the RELATION macro. 
 *
 * TRANSGRESSIONS
 *
 * .trans.nocppguard: This file has no #ifdef guard around the entire file.
 * This is so that the file can be included multiple times.  This is
 * useful because each inclusion can use a different definition of
 * RELATION.  However this may be slightly shot by having the version
 * defined here.
 */

/* No #ifndef eventdef_h, see .trans.nocppguard. */


/* EVENT_VERSION_* -- three part version number
 *
 * Increment the minor version when adding new events,
 * the median version when changing an existing event,
 * and the major version when changing the format of the event file.
 */

#define EVENT_VERSION_MAJOR  ((unsigned)0)
#define EVENT_VERSION_MEDIAN ((unsigned)0)
#define EVENT_VERSION_MINOR  ((unsigned)1)


/* Relations -- Generic definitions of events
 * 
 * These specify:
 *   - Type: The name of the event type, without the leading "Event";
 *   - Code: The unique 16-bit code associated with this event type,
 *     not currently used (see impl.h.eventcom);
 *   - Always: Whether this event type should appear in optimised 
 *     varieties, not currently used;
 *   - Kind: Category into which this event falls, without the
 *     leading "EventKind";
 *   - Format: Character sequence indicating the format of the event
 *     parameters, similar to writef (Pointer, Addr, Word, Unsigned, 
 *     String, Double).
 */

RELATION(AMCGenCreate                    , 0x0001, FALSE, Pool, PP)
RELATION(AMCGenDestroy                   , 0x0002, FALSE, Pool, P)
RELATION(AMCInit                         , 0x0003, FALSE, Pool, PP)
RELATION(AMCFinish                       , 0x0004, FALSE, Pool, P)
/* AMCBufferInit's Kind (below) should be Gen or Cohort or something */
RELATION(AMCBufferInit                   , 0x0005, FALSE, Pool, PP)
RELATION(AMCBufferFill                   , 0x0006, FALSE, Seg, PPWAW)
RELATION(AMCBufferEmpty                  , 0x0007, FALSE, Seg, PPW)
#if 0 /* Not in use */
RELATION(AMCTraceBegin                   , 0x0008, FALSE, Trace, PPPP)
RELATION(AMCCondemn                      , 0x0009, FALSE, Seg, PPPP)
#endif
RELATION(AMCScanBegin                    , 0x000a, FALSE, Trace, PPP)
RELATION(AMCScanEnd                      , 0x000b, FALSE, Trace, PPP)
RELATION(AMCFix                          , 0x000c, FALSE, Ref, 0)
RELATION(AMCFixInPlace                   , 0x000d, FALSE, Ref, 0)
RELATION(AMCFixForward                   , 0x000e, FALSE, Ref, A)
RELATION(AMCReclaim                      , 0x000f, FALSE, Seg, PPP)
RELATION(AMCTraceEnd                     , 0x0010, FALSE, Trace, PPP)
RELATION(ArenaCreate                     , 0x0011, FALSE, Arena, PP)
RELATION(ArenaDestroy                    , 0x0012, FALSE, Arena, P)
RELATION(SegAlloc                        , 0x0013, FALSE, Seg, PPAWP)
RELATION(SegFree                         , 0x0014, FALSE, Seg, PP)
RELATION(PoolInit                        , 0x0015, FALSE, Pool, PPP)
RELATION(PoolFinish                      , 0x0016, FALSE, Pool, P)
RELATION(PoolAlloc                       , 0x0017, FALSE, Object, PAW)
RELATION(PoolFree                        , 0x0018, FALSE, Object, PAW)
RELATION(TraceStart                      , 0x001c, FALSE, Trace, PPP)
#if 0 /* Not in use */
RELATION(TraceCreate                     , 0x001d, FALSE, Trace, PPPU)
#endif
RELATION(TraceDestroy                    , 0x001e, FALSE, Trace, P)
RELATION(SegSetGrey                      , 0x001f, FALSE, Seg, PPU)
RELATION(TraceFlipBegin                  , 0x0020, FALSE, Trace, PP)
RELATION(TraceFlipEnd                    , 0x0021, FALSE, Trace, PP)
RELATION(TraceReclaim                    , 0x0022, FALSE, Seg, P)
RELATION(TraceScan                       , 0x0023, FALSE, Seg, UUPPP)
RELATION(TraceAccess                     , 0x0024, FALSE, Seg, PPU)
/* TracePoll's kind isn't really Trace, but then it isn't Seg either */
RELATION(TracePoll                       , 0x0025, FALSE, Trace, PP)
RELATION(TraceFix                        , 0x0026, FALSE, Ref, PPAU)
RELATION(TraceFixSeg                     , 0x0027, FALSE, Ref, P)
RELATION(TraceFixWhite                   , 0x0028, FALSE, Ref, 0)
/* TraceScanArea{Tagged,}'s kind isn't really Trace, but not Seg either */
RELATION(TraceScanArea                   , 0x0029, FALSE, Trace, PPP)
RELATION(TraceScanAreaTagged             , 0x002a, FALSE, Trace, PPP)
RELATION(VMCreate                        , 0x002b, FALSE, Arena, PAA)
RELATION(VMDestroy                       , 0x002c, FALSE, Arena, P)
RELATION(VMMap                           , 0x002d, FALSE, Seg, PAA)
RELATION(VMUnmap                         , 0x002e, FALSE, Seg, PAA)
RELATION(ArenaExtend                     , 0x002f, FALSE, Arena, PAW)
RELATION(ArenaRetract                    , 0x0030, FALSE, Arena, PAW)
RELATION(TraceSegGreyen                  , 0x0031, FALSE, Seg, PPU)
/* RootScanned isn't a seg event, but so what */
RELATION(RootScan                        , 0x0032, FALSE, Seg, PWW)
RELATION(Intern                          , 0x0033, FALSE, User, WS)
RELATION(Label                           , 0x0034, FALSE, User, AW)
/* TraceStep is not really seg level event */
RELATION(TraceStep                       , 0x0033, FALSE, Seg, PP)
RELATION(BufferReserve                   , 0x0034, FALSE, Object, PAW)
RELATION(BufferCommit                    , 0x0035, FALSE, Object, PAWW)
/* BufferInit/Finish is not really pool level event */
RELATION(BufferInit                      , 0x0036, FALSE, Pool, PPU)
RELATION(BufferFinish                    , 0x0037, FALSE, Pool, P)
RELATION(MV2Finish                       , 0x0038, FALSE, Pool, P)
RELATION(MV2BufferFill                   , 0x0039, FALSE, Seg, PPWAW)
RELATION(MV2BufferEmpty                  , 0x003A, FALSE, Seg, PPW)
RELATION(ArenaEnter                      , 0x003B,  TRUE, Call, PU)
RELATION(ArenaLeave                      , 0x003C,  TRUE, Call, PU)

