/* impl.c.event: EVENT LOGGING
 *
 * $HopeName: MMsrc!event.c(MMdevel_event.2) $
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

SRCID(event, "$HopeName: MMsrc!event.c(MMdevel_event.2) $");

static Bool eventInited = FALSE;
static mps_io_t eventIO;
static Word eventBuffer[EVENT_BUFFER_SIZE];
static unsigned eventUserCount;

Word *EventNext, *EventLimit;

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

Res EventEnter(EventType type, Size length, ...)
{
  Res res;
  va_list args;
  Word *alloc;
  Size i, size;

  AVER(eventInited);

  size = length + 2;                  /* Include header and timestamp. */

  AVER(size < EVENT_BUFFER_SIZE);     /* Events must fit in buffer. */

  alloc = EventNext + size;

  if(alloc > EventLimit) {
    res = EventFlush();
    if(res != ResOK) return res;
    alloc = EventNext + size;
  }

  AVER(alloc <= EventLimit);

  EventNext[0] = type | (length << 16);
  EventNext[1] = (Word)mps_clock();
  va_start(args, length);
  for(i=0; i<length; ++i)
    EventNext[i+2] = va_arg(args, Word);
  va_end(args);
  EventNext = alloc;
  
  return ResOK;
}
