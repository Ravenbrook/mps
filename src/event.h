/* impl.h.event -- Event Logging Interface
 *
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!event.h(MMdevel_event.1) $
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

#ifdef TARGET_EVENT

extern Word *EventNext, *EventLimit;

#ifdef TARGET_EVENT_INLINE

#define EVENT_BEGIN(type, length)                                         \
  BEGIN                                                                   \
    Count _length = (length);                                             \
    Count _i = 0;                                                         \
    Word *_alloc = EventNext + _length + EVENT_HEADER_SIZE;               \
    if(_alloc <= EventLimit) {                                            \
      EventNext[i++] = (type);                                            \
      EventNext[i++] = _length;                                           \
      EventNext[i++] = (Word)mps_clock();                                 \
      AVER(i == EVENT_HEADER_SIZE);

#define EVENT_END(params)                                                 \
      AVER(i == _length + EVENT_HEADER_SIZE);                             \
      EventNext = _alloc;                                                 \
    } else                                                                \
      EventEnter params;                                                  \
  END

#define EVENT0(type)                                                      \
  EVENT_BEGIN(Event ## type, 0)                                           \
  EVENT_END((Event ## type, 0))

#define EVENT1(type, p0)                                                  \
  EVENT_BEGIN(Event ## type, 1)                                           \
    EventNext[i++] = (Word)(p0);                                          \
  EVENT_END((Event ## type, 1,                                            \
            (Word)p0))

#define EVENT2(type, p0, p1)                                              \
  EVENT_BEGIN(Event ## type, 2)                                           \
    EventNext[i++] = (Word)(p0);                                          \
    EventNext[i++] = (Word)(p1);                                          \
  EVENT_END((Event ## type, 2,                                            \
            (Word)p0, (Word)p1))

#define EVENT3(type, p0, p1, p2)                                          \
  EVENT_BEGIN(Event ## type, 3)                                           \
    EventNext[i++] = (Word)(p0);                                          \
    EventNext[i++] = (Word)(p1);                                          \
    EventNext[i++] = (Word)(p2);                                          \
  EVENT_END((Event ## type, 3,                                            \
            (Word)p0, (Word)p1, (Word)p2))

#define EVENT4(type, p0, p1, p2, p3)                                      \
  EVENT_BEGIN(Event ## type, 4)                                           \
    EventNext[i++] = (Word)(p0);                                          \
    EventNext[i++] = (Word)(p1);                                          \
    EventNext[i++] = (Word)(p2);                                          \
    EventNext[i++] = (Word)(p3);                                          \
  EVENT_END((Event ## type, 4,                                            \
            (Word)p0, (Word)p1, (Word)p2, (Word)p3))

#define EVENT5(type, p0, p1, p2, p3, p4)                                  \
  EVENT_BEGIN(Event ## type, 5)                                           \
    EventNext[i++] = (Word)(p0);                                          \
    EventNext[i++] = (Word)(p1);                                          \
    EventNext[i++] = (Word)(p2);                                          \
    EventNext[i++] = (Word)(p3);                                          \
    EventNext[i++] = (Word)(p4);                                          \
  EVENT_END((Event ## type, 5,                                            \
            (Word)p0, (Word)p1, (Word)p2, (Word)p3, (Word)p4))

#else /* TARGET_EVENT_INLINE not */

#define EVENT0(type)                                                      \
  EventEnter(Event ## type, 0)

#define EVENT1(type, p0)                                                  \
  EventEnter(Event ## type, 1, (Word)p0)

#define EVENT2(type, p0, p1)                                              \
  EventEnter(Event ## type, 2, (Word)p0, (Word)p1)

#define EVENT3(type, p0, p1, p2)                                          \
  EventEnter(Event ## type, 3,                                            \
             (Word)p0, (Word)p1, (Word)p2)

#define EVENT4(type, p0, p1, p2, p3)                                      \
  EventEnter(Event ## type, 4,                                            \
             (Word)p0, (Word)p1, (Word)p2, (Word)p3)

#define EVENT5(type, p0, p1, p2, p3, p4)                                  \
  EventEnter(Event ## type, 5,                                            \
             (Word)p0, (Word)p1, (Word)p2, (Word)p3, (Word)p4)

#endif /* TARGET_EVENT_INLINE */

#else /* TARGET_EVENT not */

#define EVENT0(type)                       NOOP
#define EVENT1(type, p0)                   NOOP
#define EVENT2(type, p0, p1)               NOOP
#define EVENT3(type, p0, p1, p2)           NOOP
#define EVENT4(type, p0, p1, p2, p3)       NOOP
#define EVENT5(type, p0, p1, p2, p3, p4)   NOOP

#endif /* TARGET_EVENT */


#endif /* event_h */
