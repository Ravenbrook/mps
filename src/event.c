/* impl.c.event: EVENT LOGGING
 *
 * $HopeName$
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

SRCID(event, "$HopeName$");

#define EVENT_BUFFER		(size_t)4096

static Bool eventInit = FALSE;
static unsigned eventPointCount = 0;
static mps_io_t eventIO;
static Word eventBuffer[EVENT_BUFFER];
static EventPointStruct eventPointStruct;

Res EventPointCreate(EventPoint *pointReturn)
{
  Res res;

  AVER(pointReturn != NULL);

  if(!eventInit) {
    AVER(eventPointCount == 0);
    res = (Res)mps_io_create(&eventIO);
    if(res != ResOK) return res;
    eventPointStruct.base = &eventBuffer[0];
    eventPointStruct.init = eventPointStruct.base;
    eventPointStruct.limit = &eventBuffer[EVENT_BUFFER];
    eventInit = TRUE;
  }
  
  ++eventPointCount;

  *pointReturn = &eventPointStruct;
  return ResOK;
}

static Res EventPointFlush(EventPoint point)
{
  Res res;

  res = (Res)mps_io_flush(eventIO,
                          (void *)point->base,
                          (char *)point->init - (char *)point->base);
  if(res != ResOK) return res;

  point->init = point->base;
  return ResOK;
}

void EventPointDestroy(EventPoint point)
{
  AVER(point == &eventPointStruct);
  AVER(eventPointCount > 0);

  /* @@@@ Nasty hack to flush last few events.  This will turn into a */
  /* proper hack later. */  
  --eventPointCount;
  if(eventPointCount == 0)
    EventPointFlush(&eventPointStruct);
}

Res EventEnter(EventPoint point, EventType type, Size length, ...)
{
  Res res;
  va_list args;
  Word *alloc;
  Size i;

  AVER(eventInit);
  AVER(point == &eventPointStruct);
  AVER(length + 1 < EVENT_BUFFER);  /* @@@@ assumes event will fit in buffer */

  alloc = point->init + length + 1;   /* one word for event header */

  if(alloc > point->limit) {
    res = EventPointFlush(point);
    if(res != ResOK) return res;
    alloc = point->init + length + 1;
  }

  AVER(alloc <= point->limit);

  point->init[0] = type | (length << 16);
  va_start(args, length);
  for(i=0; i<length; ++i)
    point->init[i+1] = va_arg(args, Word);
  va_end(args);
  point->init = alloc;
  
  return ResOK;
}
