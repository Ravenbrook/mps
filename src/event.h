/* impl.h.event -- Event Logging Interface
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!event.h(MMdevel_event_string.3) $
 *
 * .readership: MPS developers.
 * .sources: mps.design.event
 */

#ifndef event_h
#define event_h

#include "mpm.h"

extern Res EventFlush(void);
extern Res EventInit(void);
extern void EventFinish(void);

#include "eventcom.h"
#include "eventgen.h"

#ifdef EVENT

/* Note that enum values can be up to fifteen bits long portably. */

#define RELATION(type, code, always, format) \
  enum { \
    Event ## type ## High = (code >> 8), \
    Event ## type ## Low = (code & 0xFF), \
    Event ## type ## Always = always,\
    Event ## type ## Format = EventFormat ## format \
  }; 
  
#include "eventdef.h"

#undef RELATION


/* @@@@ We can't use memcpy, because it's a dependence on the ANSI C
 * library, despite the fact that many compilers will inline it.
 * Also, because we're always dealing with aligned words, we could 
 * copy more efficiently.
 */

#define _memcpy(to, from, length) \
  BEGIN \
    Index _i; \
    char *_to = (char *)(to); \
    char *_from = (char *)(from); \
    Count _length2 = (length); \
    for(_i = 0; _i < _length2; _i++) \
      _to[_i] = _from[_i]; \
  END

extern EventUnion Event;

#define EVENT_BEGIN(type, format, _length) \
  BEGIN \
    AVER(EventFormat ## format == Event ## type ## Format); \
    /* @@@@ As an interim measure, send the old event codes */ \
    Event.any.code = Event ## type; \
    /* @@@@ Length is in words, excluding header; this will change */ \
    /* We know that _length is aligned to word size */ \
    Event.any.length = ((_length / sizeof(Word)) - 3); \
    Event.any.clock = mps_clock(); 

#define EVENT_END(type, length) \
  if((length) > EventLimit - EventNext) \
    EventFlush(); /* @@@ should pass length */ \
  AVER((length) <= EventLimit - EventNext); \
  _memcpy(EventNext, &Event, (length)); \
  EventNext += (length); \
  END

extern char *EventNext, *EventLimit;

#else /* EVENT not */

#define EventInit()                        NOOP
#define EventFinish()                      NOOP

#endif /* EVENT */

#endif /* event_h */
