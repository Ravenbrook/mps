/* impl.h.event -- Event Logging Interface
 *
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!event.h(MMdevel_event_format.1) $
 *
 * .readership: MPS developers.
 * .sources: mps.design.event
 */

#ifndef event_h
#define event_h

#include "mpm.h"

extern Res EventEnter(EventType type, Size length, ...);
extern Res EventInit(void);
extern void EventFinish(void);

#ifdef EVENT

extern Word *EventNext, *EventLimit;


#define EVENT_0(type) \
  EventEnter(Event ## type, 0)

#define EVENT_A(type, p0) \
  EventEnter(Event ## type, 1, (Word)p0)

#define EVENT_P(type, p0) \
  EventEnter(Event ## type, 1, (Word)p0)

#define EVENT_PP(type, p0, p1) \
  EventEnter(Event ## type, 2, (Word)p0, (Word)p1)

#define EVENT_PAA(type, p0, p1, p2) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2)

#define EVENT_PPP(type, p0, p1, p2) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2)

#define EVENT_PPU(type, p0, p1, p2) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2)

#define EVENT_PAAU(type, p0, p1, p2, p3) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2, (Word)p3)

#define EVENT_PPAA(type, p0, p1, p2, p3) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2, (Word)p3)

#define EVENT_PPPP(type, p0, p1, p2, p3) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2, (Word)p3)

#define EVENT_PPPU(type, p0, p1, p2, p3) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2, (Word)p3)

#define EVENT_PPAUP(type, p0, p1, p2, p3, p4) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2, (Word)p3, \
             (Word)p4)

#define EVENT_PPUUU(type, p0, p1, p2, p3, p4) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2, (Word)p3, \
             (Word)p4)

#define EVENT_PUPPP(type, p0, p1, p2, p3, p4) \
  EventEnter(Event ## type, 3, (Word)p0, (Word)p1, (Word)p2, (Word)p3, \
             (Word)p4)

#else /* EVENT not */

#define EVENT_0(type)                          NOOP
#define EVENT_A(type, p0)                      NOOP
#define EVENT_P(type, p0)                      NOOP
#define EVENT_PP(type, p0, p1)                 NOOP
#define EVENT_PAA(type, p0, p1, p2)            NOOP
#define EVENT_PPP(type, p0, p1, p2)            NOOP
#define EVENT_PPU(type, p0, p1, p2)            NOOP
#define EVENT_PAAU(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPAA(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPPP(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPPU(type, p0, p1, p2, p3)       NOOP
#define EVENT_PPAUP(type, p0, p1, p2, p3, p4)  NOOP
#define EVENT_PPUUU(type, p0, p1, p2, p3, p4)  NOOP
#define EVENT_PUPPP(type, p0, p1, p2, p3, p4)  NOOP

#define EventInit()                        NOOP
#define EventFinish()                      NOOP

#endif /* EVENT */

#endif /* event_h */
