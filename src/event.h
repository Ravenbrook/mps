/* impl.h.event -- Event Logging Interface
 *
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!event.h(MMdevel_event_format.2) $
 *
 * .readership: MPS developers.
 * .sources: mps.design.event
 */

#ifndef event_h
#define event_h

#include "mpm.h"

extern Res EventEnter(EventType type, Size length, ...);
extern Res EventFlush(void);
extern Res EventInit(void);
extern void EventFinish(void);

#ifdef EVENT

#define HEADER Word code; Word length; Word clock; 

typedef struct { 
  HEADER
} EventStruct;

typedef struct {
  HEADER
  Addr a;
} EventAStruct;

typedef struct {
  HEADER
  void *p;
} EventPStruct;

typedef struct {
  HEADER
  void *p1, *p2;
} EventPPStruct;

typedef struct {
  HEADER
  void *p;
  Addr a1, a2;
} EventPAAStruct;

typedef struct {
  HEADER
  void *p;
  Addr a;
  unsigned u;
} EventPAUStruct;

typedef struct {
  HEADER
  void *p1, *p2, *p3;
} EventPPPStruct;

typedef struct {
  HEADER
  void *p1, *p2;
  unsigned u;
} EventPPUStruct;

typedef struct {
  HEADER
  void *p;
  Addr a1, a2;
  unsigned u;
} EventPAAUStruct;

typedef struct {
  HEADER
  void *p1, *p2;
  Addr a1, a2;
} EventPPAAStruct;

typedef struct {
  HEADER
  void *p1, *p2;
  Addr a;
  unsigned u;
} EventPPAUStruct;

typedef struct {
  HEADER
  void *p1, *p2, *p3, *p4;
} EventPPPPStruct;

typedef struct {
  HEADER
  void *p1, *p2, *p3;
  unsigned u;
} EventPPPUStruct;

typedef struct {
  HEADER
  void *p1, *p2;
  Addr a;
  unsigned u;
  void *p3;
} EventPPAUPStruct;

typedef struct {
  HEADER
  void *p1, *p2;
  unsigned u1, u2, u3;
} EventPPUUUStruct;

typedef struct {
  HEADER
  void *p1;
  unsigned u;
  void *p2, *p3, *p4;
} EventPUPPPStruct;

#undef HEADER

typedef union {
  EventStruct any;
  EventAStruct a;
  EventPStruct p;
  EventPPStruct pp;
  EventPAAStruct paa;
  EventPAUStruct pau;
  EventPPPStruct ppp;
  EventPPUStruct ppu;
  EventPAAUStruct paau;
  EventPPAAStruct ppaa;
  EventPPAUStruct ppau;
  EventPPPPStruct pppp;
  EventPPPUStruct pppu;
  EventPPAUPStruct ppaup;
  EventPPUUUStruct ppuuu;
  EventPUPPPStruct puppp;
} EventUnion;  

#define EVENT_BEGIN(type, _length) \
  BEGIN \
    EventUnion *_event; \
    Word _aligned_length = WordAlignUp((_length), MPS_PF_ALIGN); \
    if((_aligned_length) > EventLimit - EventNext) { \
      EventFlush(); /* @@@ should pass length */ \
    } \
    AVER((_aligned_length) <= EventLimit - EventNext); \
    _event = (EventUnion *)EventNext; \
    _event->any.code = Event ## type; \
    _event->any.length = (_aligned_length); \
    _event->any.clock = mps_clock(); \
    EventNext += _aligned_length;

#define EVENT_END(type, length) \
  END

extern Word *EventNext, *EventLimit;

#define EVENT_0(type) \
  EVENT_BEGIN(type, sizeof(EventStruct)) \
  EVENT_END(type, sizeof(EventStruct))

#define EVENT_A(type, _a) \
  EVENT_BEGIN(type, sizeof(EventAStruct)) \
  _event->a.a = (_a); \
  EVENT_END(type, sizeof(EventAStruct))

#define EVENT_P(type, _p) \
  EVENT_BEGIN(type, sizeof(EventPStruct)) \
  _event->p.p = (_p); \
  EVENT_END(type, sizeof(EventPStruct))

#define EVENT_PP(type, _p1, _p2) \
  EVENT_BEGIN(type, sizeof(EventPPStruct)) \
  _event->pp.p1 = (_p1); \
  _event->pp.p2 = (_p2); \
  EVENT_END(type, sizeof(EventPPStruct))

#define EVENT_PAA(type, _p, _a1, _a2) \
  EVENT_BEGIN(type, sizeof(EventPAAStruct)) \
  _event->paa.p = (_p); \
  _event->paa.a1 = (_a1); \
  _event->paa.a2 = (_a2); \
  EVENT_END(type, sizeof(EventPAAStruct))

#define EVENT_PAU(type, _p, _a, _u) \
  EVENT_BEGIN(type, sizeof(EventPAUStruct)) \
  _event->pau.p = (_p); \
  _event->pau.a = (_a); \
  _event->pau.u = (_u); \
  EVENT_END(type, sizeof(EventPAUStruct))

#define EVENT_PPP(type, _p1, _p2, _p3) \
  EVENT_BEGIN(type, sizeof(EventPPPStruct)) \
  _event->ppp.p1 = (_p1); \
  _event->ppp.p2 = (_p2); \
  _event->ppp.p3 = (_p3); \
  EVENT_END(type, sizeof(EventPPPStruct))

#define EVENT_PPU(type, _p1, _p2, _u) \
  EVENT_BEGIN(type, sizeof(EventPPUStruct)) \
  _event->ppu.p1 = (_p1); \
  _event->ppu.p2 = (_p2); \
  _event->ppu.u = (_u); \
  EVENT_END(type, sizeof(EventPPUStruct))

#define EVENT_PAAU(type, _p, _a1, _a2, _u) \
  EVENT_BEGIN(type, sizeof(EventPAAUStruct)) \
  _event->paau.p = (_p); \
  _event->paau.a1 = (_a1); \
  _event->paau.a2 = (_a2); \
  _event->paau.u = (_u); \
  EVENT_END(type, sizeof(EventPAAUStruct))

#define EVENT_PPAA(type, _p1, _p2, _a1, _a2) \
  EVENT_BEGIN(type, sizeof(EventPPAAStruct)) \
  _event->ppaa.p1 = (_p1); \
  _event->ppaa.p2 = (_p2); \
  _event->ppaa.a1 = (_a1); \
  _event->ppaa.a2 = (_a2); \
  EVENT_END(type, sizeof(EventPPAAStruct))

#define EVENT_PPAU(type, _p1, _p2, _a, _u) \
  EVENT_BEGIN(type, sizeof(EventPPAUStruct)) \
  _event->ppau.p1 = (_p1); \
  _event->ppau.p2 = (_p2); \
  _event->ppau.a = (_a); \
  _event->ppau.u = (_u); \
  EVENT_END(type, sizeof(EventPPAUStruct))

#define EVENT_PPPP(type, _p1, _p2, _p3, _p4) \
  EVENT_BEGIN(type, sizeof(EventPPPPStruct)) \
  _event->pppp.p1 = (_p1); \
  _event->pppp.p2 = (_p2); \
  _event->pppp.p3 = (_p3); \
  _event->pppp.p4 = (_p4); \
  EVENT_END(type, sizeof(EventPPPPStruct))

#define EVENT_PPPU(type, _p1, _p2, _p3, _u) \
  EVENT_BEGIN(type, sizeof(EventPPPUStruct)) \
  _event->pppu.p1 = (_p1); \
  _event->pppu.p2 = (_p2); \
  _event->pppu.p3 = (_p3); \
  _event->pppu.u = (_u); \
  EVENT_END(type, sizeof(EventPPPUStruct))

#define EVENT_PPAUP(type, _p1, _p2, _a, _u, _p3) \
  EVENT_BEGIN(type, sizeof(EventPPAUPStruct)) \
  _event->ppaup.p1 = (_p1); \
  _event->ppaup.p2 = (_p2); \
  _event->ppaup.a = (_a); \
  _event->ppaup.u = (_u); \
  _event->ppaup.p3 = (_p3); \
  EVENT_END(type, sizeof(EventPPAUPStruct))

#define EVENT_PPUUU(type, _p1, _p2, _u1, _u2, _u3) \
  EVENT_BEGIN(type, sizeof(EventPPUUUStruct)) \
  _event->ppuuu.p1 = (_p1); \
  _event->ppuuu.p2 = (_p2); \
  _event->ppuuu.u1 = (_u1); \
  _event->ppuuu.u2 = (_u2); \
  _event->ppuuu.u3 = (_u3); \
  EVENT_END(type, sizeof(EventPPUUUStruct))


#define EVENT_PUPPP(type, p0, p1, p2, p3, p4) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2, (Word)p3, \
             (Word)p4)

#else /* EVENT not */

#define EVENT_0(type)                          NOOP
#define EVENT_A(type, p0)                      NOOP
#define EVENT_P(type, p0)                      NOOP
#define EVENT_PP(type, p0, p1)                 NOOP
#define EVENT_PAA(type, p0, p1, p2)            NOOP
#define EVENT_PAU(type, p0, p1, p2)            NOOP
#define EVENT_PPP(type, p0, p1, p2)            NOOP
#define EVENT_PPU(type, p0, p1, p2)            NOOP
#define EVENT_PAAU(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPAA(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPAU(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPPP(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPPU(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPAUP(type, p0, p1, p2, p3, p4)  NOOP
#define EVENT_PPUUU(type, p0, p1, p2, p3, p4)  NOOP
#define EVENT_PUPPP(type, p0, p1, p2, p3, p4)  NOOP

#define EventInit()                        NOOP
#define EventFinish()                      NOOP

#endif /* EVENT */

#endif /* event_h */
