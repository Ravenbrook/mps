/*  impl.c.than
 *
 *                  ANSI THREADS MANAGER
 *
 *  $HopeName: MMsrc!than.c(MMdevel_assertid.1) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a single-threaded implementation of the threads manager.
 *  Has stubs for thread suspension.
 *  See design.mps.thread-manager.
 *
 *  .single: We only expect at most one thread on the ring.
 *
 *  This supports the impl.h.th
 */

#include "mpm.h"

SRCID(than, "$HopeName: MMsrc!than.c(MMdevel_assertid.1) $");


Bool ThreadCheck(Thread thread)
{
  CHECKS(0xA55E62, Thread, thread);
  CHECKU(0xA55E62, Space, thread->space);
  CHECKL(0xA55E62, thread->serial < thread->space->threadSerial);
  CHECKL(0xA55E62, RingCheck(&thread->spaceRing));
  return TRUE;
}


Res ThreadRegister(Thread *threadReturn, Space space)
{
  Res res;
  Thread thread;
  Ring ring;
  void *p;

  AVER(0xA55E62, threadReturn != NULL);

  res = SpaceAlloc(&p, space, sizeof(ThreadStruct));
  if(res != ResOK) return res;
  thread = (Thread)p;

  thread->space = space;
  RingInit(&thread->spaceRing);

  thread->sig = ThreadSig;
  thread->serial = space->threadSerial;
  ++space->threadSerial;

  AVERT(0xA55E62, Thread, thread);

  ring = SpaceThreadRing(space);
  AVER(0xA55E62, RingCheckSingle(ring));  /* .single */

  RingAppend(ring, &thread->spaceRing);

  *threadReturn = thread;
  return ResOK;
}

void ThreadDeregister(Thread thread, Space space)
{
  AVERT(0xA55E62, Thread, thread);
  AVERT(0xA55E62, Space, space);

  RingRemove(&thread->spaceRing);

  thread->sig = SigInvalid;

  RingFinish(&thread->spaceRing);

  SpaceFree(space, (Addr)thread, sizeof(ThreadStruct));
}

void ThreadRingSuspend(Ring threadRing)
{
  AVERT(0xA55E62, Ring, threadRing);
  return;
}

void ThreadRingResume(Ring threadRing)
{
  AVERT(0xA55E62, Ring, threadRing);
  return;
}

/* Must be thread-safe.  See design.mps.interface.c.thread-safety. */
Space ThreadSpace(Thread thread)
{
  return thread->space;
}

Res ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  return StackScan(ss, stackBot);
}

Res ThreadDescribe(Thread thread, mps_lib_FILE *stream)
{
  Res res;
  
  res = WriteF(stream,
               "Thread $P ($U) {\n", (WriteFP)thread, (WriteFU)thread->serial,
               "  space $P ($U)\n",  
               (WriteFP)thread->space, (WriteFU)thread->space->serial,
               "} Thread $P ($U)\n", (WriteFP)thread, (WriteFU)thread->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}
