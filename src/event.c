/* impl.c.event: EVENT LOGGING
 *
 * $HopeName: MMsrc!event.c(MMdevel_event.5) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MPS developers.
 * .sources: mps.design.event
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * The reference counting used to destroy the mps_io object isn't right.
 * The log file will be re-created if the lifetimes of spaces don't
 * overlap, but shared if they do.
 */

#include "mpm.h"
#include "event.h"
#include "mpsio.h"

SRCID(event, "$HopeName: MMsrc!event.c(MMdevel_event.5) $");

static Bool eventInited = FALSE;
static mps_io_t eventIO;
static Word eventBuffer[EVENT_BUFFER_SIZE];
static Count eventUserCount;

Word *EventNext, *EventLimit; /* Used by macros in impl.h.event */

static Res EventFlush(void)
{
  Res res;
  
  AVER(eventInited);

  res = (Res)mps_io_write(eventIO,
                          (void *)eventBuffer,
                          (char *)EventNext - (char *)eventBuffer);
  if(res != ResOK) return res;

  EventNext = eventBuffer;

  return ResOK;
}

Res EventInit(void)
{
  Res res;

  /* Initialize the event system if this is the first call. */
  if(!eventInited) {
    AVER(EventNext == 0);
    AVER(EventLimit == 0);
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

void EventFinish(void)
{
  AVER(eventInited);
  AVER(eventUserCount > 0);
  
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

  AVER(eventInited);

  size = length + EVENT_HEADER_SIZE;  /* Include header. */

  AVER(size < EVENT_BUFFER_SIZE);     /* Events must fit in buffer. */

  alloc = EventNext + size;

  if(alloc > EventLimit) {
    res = EventFlush();
    if(res != ResOK) return res;
    alloc = EventNext + size;
  }

  AVER(alloc <= EventLimit);

  EventNext[i++] = type;
  EventNext[i++] = length;
  EventNext[i++] = (Word)mps_clock();

  AVER(i == EVENT_HEADER_SIZE); 

  va_start(args, length);
  for(; i < size; ++i)
    EventNext[i] = va_arg(args, Word);

  va_end(args);
  EventNext = alloc;
  
  return ResOK;
}
