/* impl.c.event: EVENT LOGGING
 *
 * $HopeName: MMsrc!event.c(MMdevel_event.1) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MPS developers.
 * .sources: mps.design.event
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * There's no way this meets requirments yet.
 */

#include "mpm.h"
#include "mpsio.h"

SRCID(event, "$HopeName: MMsrc!event.c(MMdevel_event.1) $");

#define EVENT_BUFFER		(size_t)4096

static Bool eventInited = FALSE;
static mps_io_t eventIO;
static Word eventBuffer[EVENT_BUFFER];
static Word *eventBase, *eventInit, *eventLimit;

static void EventReset(void)
{
  AVER(eventInited);
  eventBase = &eventBuffer[0];
  eventInit = eventBase;
  eventLimit = &eventBuffer[EVENT_BUFFER];
  eventInit[0] = EventEVENT_TIME;
  eventInit[1] = (Word)mps_clock();
  eventInit = eventInit + 2;
}

Res EventFlush(void)
{
  Res res;
  
  AVER(eventInited);

  res = (Res)mps_io_flush(eventIO,
                          (void *)eventBase,
                          (char *)eventInit - (char *)eventBase);
  if(res != ResOK) return res;

  EventReset();

  return ResOK;
}

Res EventEnter(EventType type, Size length, ...)
{
  Res res;
  va_list args;
  Word *alloc;
  Size i, size;

  size = length + 2;                  /* include header and timestamp */

  AVER(size < EVENT_BUFFER);          /* @@@@ assumes event will fit in buffer */

  if(!eventInited) {
    res = (Res)mps_io_create(&eventIO);
    if(res != ResOK) return res;
    eventInited = TRUE;
    EventReset();
  }

  alloc = eventInit + size;

  if(alloc > eventLimit) {
    res = EventFlush();
    if(res != ResOK) return res;
    alloc = eventInit + size;
  }

  AVER(alloc <= eventLimit);

  eventInit[0] = type | (length << 16);
  eventInit[1] = (Word)mps_clock();
  va_start(args, length);
  for(i=0; i<length; ++i)
    eventInit[i+2] = va_arg(args, Word);
  va_end(args);
  eventInit = alloc;
  
  return ResOK;
}
