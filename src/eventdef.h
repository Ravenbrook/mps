/* impl.h.eventdef -- Event Logging Definitions
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName$
 *
 * .readership: MPS developers.
 * .source: design.mps.telemetry
 *
 * .desc: This file declares relationships that define the various event
 * types.  It is intended to be used with clever definitions of the 
 * RELATION macro.  The macro's parameters are:
 *   - Type: The (baseless) name of the event type;
 *   - Code: The unique 16-bit code associated with this event type;
 *   - Always: Whether this event type should appear in optimised 
 *     varieties;
 *   - Format: Character sequence indicating the format of the event
 *     parameters, as for writef.
 */

/* No protection because file is re-entrant. */


/* EVENT_VERSION_* -- three part version number
 *
 * Increment the minor version when adding new events,
 * the median version when changing an existing event,
 * and the major version when changing the format of the event file.
 */

#define EVENT_VERSION_MAJOR  ((unsigned)0)
#define EVENT_VERSION_MEDIAN ((unsigned)0)
#define EVENT_VERSION_MINOR  ((unsigned)0)


RELATION(AMCGenCreate                    , 0x0001, TRUE, PP)
RELATION(AMCGenDestroy                   , 0x0002, TRUE, P)
RELATION(AMCInit                         , 0x0003, TRUE, PP)
RELATION(AMCFinish                       , 0x0004, TRUE, P)
RELATION(AMCBufferInit                   , 0x0005, TRUE, PP)
RELATION(AMCBufferFill                   , 0x0006, TRUE, PPWAW)
RELATION(AMCBufferEmpty                  , 0x0007, TRUE, PPW)
RELATION(AMCTraceBegin                   , 0x0008, TRUE, PPPP)
RELATION(AMCCondemn                      , 0x0009, TRUE, PPPP)
RELATION(AMCScanBegin                    , 0x000a, TRUE, PPP)
RELATION(AMCScanEnd                      , 0x000b, TRUE, PPP)
RELATION(AMCFix                          , 0x000c, TRUE, 0)
RELATION(AMCFixAmbig                     , 0x000d, TRUE, 0)
RELATION(AMCFixForward                   , 0x000e, TRUE, A)
RELATION(AMCReclaim                      , 0x000f, TRUE, PPP)
RELATION(AMCTraceEnd                     , 0x0010, TRUE, PPP)
RELATION(ArenaCreate                     , 0x0011, TRUE, PP)
RELATION(ArenaDestroy                    , 0x0012, TRUE, P)
RELATION(SegAlloc                        , 0x0013, TRUE, PPAWP)
RELATION(SegFree                         , 0x0014, TRUE, PP)
RELATION(PoolInit                        , 0x0015, TRUE, PPP)
RELATION(PoolFinish                      , 0x0016, TRUE, P)
RELATION(PoolAlloc                       , 0x0017, TRUE, PAW)
RELATION(PoolFree                        , 0x0018, TRUE, PAW)
RELATION(SpaceCreate                     , 0x001a, TRUE, P)
RELATION(SpaceDestroy                    , 0x001b, TRUE, P)
RELATION(TraceStart                      , 0x001c, TRUE, PPP)
RELATION(TraceCreate                     , 0x001d, TRUE, PPPU)
RELATION(TraceDestroy                    , 0x001e, TRUE, P)
RELATION(TraceSegGreyen                  , 0x001f, TRUE, PPU)
RELATION(TraceFlipBegin                  , 0x0020, TRUE, PP)
RELATION(TraceFlipEnd                    , 0x0021, TRUE, PP)
RELATION(TraceReclaim                    , 0x0022, TRUE, P)
RELATION(TraceScan                       , 0x0023, TRUE, UUPPP)
RELATION(TraceAccess                     , 0x0024, TRUE, PPU)
RELATION(TracePoll                       , 0x0025, TRUE, PP)
RELATION(TraceFix                        , 0x0026, TRUE, PPAU)
RELATION(TraceFixSeg                     , 0x0027, TRUE, P)
RELATION(TraceFixWhite                   , 0x0028, TRUE, 0)
RELATION(TraceScanArea                   , 0x0029, TRUE, PPP)
RELATION(TraceScanAreaTagged             , 0x002a, TRUE, PPP)
RELATION(VMCreate                        , 0x002b, TRUE, PPAA)
RELATION(VMDestroy                       , 0x002c, TRUE, P)
RELATION(VMMap                           , 0x002d, TRUE, PAA)
RELATION(VMUnmap                         , 0x002e, TRUE, PAA)
