/* impl.c.event: EVENT LOGGING
 *
 * $HopeName: MMsrc!event.c(MMdevel_assertid.1) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MPS developers.
 * .sources: mps.design.event
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * .trans.ref: The reference counting used to destroy the mps_io object 
 * isn't right.
 *
 * .trans.log: The log file will be re-created if the lifetimes of 
 * spaces don't overlap, but shared if they do.  mps_io_create cannot
 * be called twice, but EventInit avoids this anyway.
 *
 * .trans.ifdef: This file should logically be split into two, event.c
 * (which contains NOOP definitions, for general use) and eventdl.c, which
 * is specific to the logging variety and actually does logging (maybe).
 * Unfortunately, the build system doesn't really cope, and so this file
 * consists of two versions which are conditional on the EVENT symbol.
 */

#include "mpm.h"
#include "event.h"
#include "mpsio.h"

SRCID(event, "$HopeName: MMsrc!event.c(MMdevel_assertid.1) $");

#ifdef EVENT /* .trans.ifdef */

static Bool eventInited = FALSE;
static mps_io_t eventIO;
static Word eventBuffer[EVENT_BUFFER_SIZE];
static Count eventUserCount;

Word *EventNext, *EventLimit; /* Used by macros in impl.h.event */

static Res EventFlush(void)
{
  Res res;
  
  AVER(0xA55E62, eventInited);

  res = (Res)mps_io_write(eventIO,
                          (void *)eventBuffer,
                          (char *)EventNext - (char *)eventBuffer);
  if(res != ResOK) return res;

  EventNext = eventBuffer;

  return ResOK;
}

Res (EventInit)(void)
{
  Res res;

  /* Initialize the event system if this is the first call. */
  if(!eventInited) { /* See .trans.log */
    AVER(0xA55E62, EventNext == 0);
    AVER(0xA55E62, EventLimit == 0);
    res = (Res)mps_io_create(&eventIO);
    if(res != ResOK) return res;
    EventNext = eventBuffer;
    EventLimit = &eventBuffer[EVENT_BUFFER_SIZE];
    eventUserCount = 0;
    eventInited = TRUE;
  }

  ++eventUserCount;

  return ResOK;
}

void (EventFinish)(void)
{
  AVER(0xA55E62, eventInited);
  AVER(0xA55E62, eventUserCount > 0);
  
  (void)EventFlush();
  (void)mps_io_flush(eventIO);

  --eventUserCount;
}

Res EventEnter(EventType type, Count length, ...)
{
  Res res;
  va_list args;
  Word *alloc;
  Count i=0, size;

  AVER(0xA55E62, eventInited);

  size = length + EVENT_HEADER_SIZE;  /* Include header. */

  AVER(0xA55E62, size < EVENT_BUFFER_SIZE);     /* Events must fit in buffer. */

  alloc = EventNext + size;

  if(alloc > EventLimit) {
    res = EventFlush();
    if(res != ResOK) return res;
    alloc = EventNext + size;
  }

  AVER(0xA55E62, alloc <= EventLimit);

  EventNext[i++] = type;
  EventNext[i++] = length;
  EventNext[i++] = (Word)mps_clock();

  AVER(0xA55E62, i == EVENT_HEADER_SIZE); 

  va_start(args, length);
  for(; i < size; ++i)
    EventNext[i] = va_arg(args, Word);

  va_end(args);
  EventNext = alloc;
  
  return ResOK;
}

#else /* EVENT, not */

Res (EventInit)(void)
{
  return(ResOK);  
}

void (EventFinish)(void)
{
  NOOP;
}

#endif /* EVENT */
