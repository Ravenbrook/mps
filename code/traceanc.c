/* traceanc.c: ANCILLARY SUPPORT FOR TRACER
 *
 * $Id$
 * Copyright (c) 2001-2003, 2006-2008 Ravenbrook Limited.
 * See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * All this ancillary stuff was making trace.c very cluttered.  
 * Put it here instead.  RHSK 2008-12-09.
 *
 * CONTENTS
 *
 *   - TraceStartMessage.  Posted when a trace starts.
 *
 *   - TraceMessage.  Posted when a trace ends.
 *
 *   - TraceIdMessages.  Pre-allocated messages for traceid.
 *
 *   - ArenaRelease, ArenaClamp, ArenaPark.
 *
 *   - ArenaStartCollect, ArenaCollect.
 *
 *   - Transforms.
 *
 *   - ArenaExposeRemember and ArenaRestoreProtection.
 */

#include "mpm.h"



/* --------  TraceStartMessage  -------- */


/* TraceStartMessage -- posted when a trace starts
 *
 * Internal names:
 *   trace start
 *   TraceStartMessage, tsMessage (struct *)
 *   MessageTypeGCSTART (enum)
 *
 * External names:
 *   mps_message_type_gc_start (enum macro)
 *   MPS_MESSAGE_TYPE_GC_START (enum)
 *
 * (Note: this should properly be called "trace begin", but it's much 
 * too late to change it now!)
 *
 * See <design/message-gc/>.
 */

#define TraceStartMessageSig ((Sig)0x51926535) /* SIG TRaceStartMeSsage */

/* .whybuf:
 * .whybuf.len: Length (in chars) of a char buffer used to store the 
 * reason why a collection started in the TraceStartMessageStruct 
 * (used by mps_message_type_gc_start).  If the reason is too long to 
 * fit, it must be truncated.
 * .whybuf.nul: Check insists that the last char in the array is NUL 
 * (even if there is another NUL earlier in the buffer); this makes 
 * the NUL-termination check fast.
 */
#define TRACE_START_MESSAGE_WHYBUF_LEN 128

typedef struct TraceStartMessageStruct {
  Sig sig;
  char why[TRACE_START_MESSAGE_WHYBUF_LEN];  /* .whybuf */
  MessageStruct messageStruct;
} TraceStartMessageStruct;

#define TraceStartMessageMessage(traceStartMessage) \
  (&((traceStartMessage)->messageStruct))
#define MessageTraceStartMessage(message) \
  (PARENT(TraceStartMessageStruct, messageStruct, message))

Bool TraceStartMessageCheck(TraceStartMessage tsMessage)
{
  CHECKS(TraceStartMessage, tsMessage);
  CHECKD(Message, TraceStartMessageMessage(tsMessage));
  CHECKL(MessageGetType(TraceStartMessageMessage(tsMessage)) ==
         MessageTypeGCSTART);

  /* Check that why is NUL terminated.  See .whybuf.nul */
  CHECKL(tsMessage->why[NELEMS(tsMessage->why)-1] == '\0');

  return TRUE;
}

static void TraceStartMessageDelete(Message message)
{
  TraceStartMessage tsMessage;
  Arena arena;

  AVERT(Message, message);
  tsMessage = MessageTraceStartMessage(message);
  AVERT(TraceStartMessage, tsMessage);

  arena = MessageArena(message);
  tsMessage->sig = SigInvalid;
  MessageFinish(message);

  ControlFree(arena, (void *)tsMessage, sizeof(TraceStartMessageStruct));
}

static const char *TraceStartMessageWhy(Message message)
{
  TraceStartMessage tsMessage;

  AVERT(Message, message);
  tsMessage = MessageTraceStartMessage(message);
  AVERT(TraceStartMessage, tsMessage);

  return tsMessage->why;
}

static MessageClassStruct TraceStartMessageClassStruct = {
  MessageClassSig,               /* sig */
  "TraceGCStart",                /* name */
  MessageTypeGCSTART,            /* Message Type */
  TraceStartMessageDelete,       /* Delete */
  MessageNoFinalizationRef,      /* FinalizationRef */
  MessageNoGCLiveSize,           /* GCLiveSize */
  MessageNoGCCondemnedSize,      /* GCCondemnedSize */
  MessageNoGCNotCondemnedSize,   /* GCNotCondemnedSize */
  TraceStartMessageWhy,          /* GCStartWhy */
  MessageClassSig                /* <design/message/#class.sig.double> */
};

static void traceStartMessageInit(Arena arena, TraceStartMessage tsMessage)
{
  AVERT(Arena, arena);

  MessageInit(arena, TraceStartMessageMessage(tsMessage),
              &TraceStartMessageClassStruct, MessageTypeGCSTART);
  tsMessage->why[0] = '\0';
  tsMessage->why[NELEMS(tsMessage->why)-1] = '\0';  /* .whybuf.nul */

  tsMessage->sig = TraceStartMessageSig;
  AVERT(TraceStartMessage, tsMessage);
}

/* TraceStartWhyToString -- why-code to text
 *
 * Converts a TraceStartWhy* code into a constant string describing 
 * why a trace started.
 */
 
const char *TraceStartWhyToString(int why)
{
  const char *r;

  switch(why) {
  case TraceStartWhyCHAIN_GEN0CAP:
    r = "Generation 0 of a chain has reached capacity:"
        " start a minor collection.";
    break;
  case TraceStartWhyDYNAMICCRITERION:
    r = "Need to start full collection now, or there won't be enough"
        " memory (ArenaAvail) to complete it.";
    break;
  case TraceStartWhyOPPORTUNISM:
    r = "Opportunism: client predicts plenty of idle time,"
        " so start full collection.";
    break;
  case TraceStartWhyCLIENTFULL_INCREMENTAL:
    r = "Client requests: start incremental full collection now.";
    break;
  case TraceStartWhyCLIENTFULL_BLOCK:
    r = "Client requests: immediate full collection.";
    break;
  case TraceStartWhyWALK:
    r = "Walking all live objects.";
    break;
  case TraceStartWhyTRANSFORM:
    r = "Transform objects from old to new.";
    break;
  default:
    NOTREACHED;
    r = "Unknown reason (internal error).";
    break;
  }

  /* Should fit in buffer without truncation; see .whybuf.len */
  AVER(StringLength(r) < TRACE_START_MESSAGE_WHYBUF_LEN);

  return r;
}


/* traceStartWhyToTextBuffer
 *
 * Converts a TraceStartWhy* code into a string describing why a trace
 * started, and copies that into the text buffer the caller provides.
 * s specifies the beginning of the buffer to write the string
 * into, len specifies the length of the buffer.
 * The string written will be NUL terminated, and truncated if
 * necessary.
 */

static void traceStartWhyToTextBuffer(char *s, size_t len, int why)
{
  const char *r;
  size_t i;

  AVER(s);
  /* len can be anything, including 0. */
  AVER(TraceStartWhyBASE <= why);
  AVER(why < TraceStartWhyLIMIT);

  r = TraceStartWhyToString(why);

  for(i=0; i<len; ++i) {
    s[i] = r[i];
    if(r[i] == '\0')
      break;
  }
  s[len-1] = '\0';  /* .whybuf.nul */
}

/* TracePostStartMessage -- complete and post trace start message
 *
 */

void TracePostStartMessage(Trace trace)
{
  Arena arena;
  TraceId ti;
  TraceStartMessage tsMessage;

  AVERT(Trace, trace);
  AVER(trace->state == TraceUNFLIPPED);

  arena = trace->arena;
  AVERT(Arena, arena);

  ti = trace->ti;
  AVERT(TraceId, ti);

  tsMessage = arena->tsMessage[ti];
  if(tsMessage) {
    AVERT(TraceStartMessage, tsMessage);

    traceStartWhyToTextBuffer(tsMessage->why,
                              sizeof tsMessage->why, trace->why);

    MessagePost(arena, TraceStartMessageMessage(tsMessage));
    arena->tsMessage[ti] = NULL;
  } else {
    arena->droppedMessages += 1;
  }

  /* We have consumed the pre-allocated message */
  AVER(!arena->tsMessage[ti]);

  if(arena->alertCollection) {
    (*arena->alertCollection)(MPS_ALERT_COLLECTION_BEGIN, trace->why);
  }
}



/* --------  TraceMessage (trace end)  -------- */


/* TraceMessage -- posted when a trace ends
 *
 * Internal names:
 *   trace end
 *   TraceMessage, tMessage (struct *)
 *   MessageTypeGC (enum)
 *
 * External names:
 *   mps_message_type_gc (enum macro)
 *   MPS_MESSAGE_TYPE_GC (enum)
 *
 * (Note: this should properly be called "trace end", but it's much 
 * too late to change it now!)
 *
 * See <design/message-gc/>.
 */


/* TraceMessage -- type of trace end messages */

#define TraceMessageSig ((Sig)0x51926359)

typedef struct TraceMessageStruct  {
  Sig sig;
  Size liveSize;
  Size condemnedSize;
  Size notCondemnedSize;
  MessageStruct messageStruct;
} TraceMessageStruct;

#define TraceMessageMessage(traceMessage) (&((traceMessage)->messageStruct))
#define MessageTraceMessage(message) \
  (PARENT(TraceMessageStruct, messageStruct, message))

Bool TraceMessageCheck(TraceMessage tMessage)
{
  CHECKS(TraceMessage, tMessage);
  CHECKD(Message, TraceMessageMessage(tMessage));
  CHECKL(MessageGetType(TraceMessageMessage(tMessage)) ==
         MessageTypeGC);
  /* We can't check anything about the statistics.  In particular, */
  /* liveSize may exceed condemnedSize because they are only estimates. */

  return TRUE;
}

static void TraceMessageDelete(Message message)
{
  TraceMessage tMessage;
  Arena arena;

  AVERT(Message, message);
  tMessage = MessageTraceMessage(message);
  AVERT(TraceMessage, tMessage);

  arena = MessageArena(message);
  tMessage->sig = SigInvalid;
  MessageFinish(message);

  ControlFree(arena, (void *)tMessage, sizeof(TraceMessageStruct));
}

static Size TraceMessageLiveSize(Message message)
{
  TraceMessage tMessage;

  AVERT(Message, message);
  tMessage = MessageTraceMessage(message);
  AVERT(TraceMessage, tMessage);

  return tMessage->liveSize;
}

static Size TraceMessageCondemnedSize(Message message)
{
  TraceMessage tMessage;

  AVERT(Message, message);
  tMessage = MessageTraceMessage(message);
  AVERT(TraceMessage, tMessage);

  return tMessage->condemnedSize;
}

static Size TraceMessageNotCondemnedSize(Message message)
{
  TraceMessage tMessage;

  AVERT(Message, message);
  tMessage = MessageTraceMessage(message);
  AVERT(TraceMessage, tMessage);

  return tMessage->notCondemnedSize;
}

static MessageClassStruct TraceMessageClassStruct = {
  MessageClassSig,               /* sig */
  "TraceGC",                     /* name */
  MessageTypeGC,                 /* Message Type */
  TraceMessageDelete,            /* Delete */
  MessageNoFinalizationRef,      /* FinalizationRef */
  TraceMessageLiveSize,          /* GCLiveSize */
  TraceMessageCondemnedSize,     /* GCCondemnedSize */
  TraceMessageNotCondemnedSize,  /* GCNotCondemnedSize */
  MessageNoGCStartWhy,           /* GCStartWhy */
  MessageClassSig                /* <design/message/#class.sig.double> */
};

static void traceMessageInit(Arena arena, TraceMessage tMessage)
{
  AVERT(Arena, arena);

  MessageInit(arena, TraceMessageMessage(tMessage),
              &TraceMessageClassStruct, MessageTypeGC);
  tMessage->liveSize = (Size)0;
  tMessage->condemnedSize = (Size)0;
  tMessage->notCondemnedSize = (Size)0;

  tMessage->sig = TraceMessageSig;
  AVERT(TraceMessage, tMessage);
}

/* TracePostMessage -- complete and post trace end message
 *
 * .message.data: The trace end message contains the live size
 * (forwardedSize + preservedInPlaceSize), the condemned size
 * (condemned), and the not-condemned size (notCondemned).
 */

void TracePostMessage(Trace trace)
{
  Arena arena;
  TraceId ti;
  TraceMessage tMessage;

  AVERT(Trace, trace);
  AVER(trace->state == TraceFINISHED);
  
  arena = trace->arena;
  AVERT(Arena, arena);

  ti = trace->ti;
  AVERT(TraceId, ti);

  tMessage = arena->tMessage[ti];
  if(tMessage) {
    AVERT(TraceMessage, tMessage);

    tMessage->liveSize = trace->forwardedSize + trace->preservedInPlaceSize;
    tMessage->condemnedSize = trace->condemned;
    tMessage->notCondemnedSize = trace->notCondemned;

    MessagePost(arena, TraceMessageMessage(tMessage));
    arena->tMessage[ti] = NULL;
  } else {
    arena->droppedMessages += 1;
  }
  
  /* We have consumed the pre-allocated message */
  AVER(!arena->tMessage[ti]);

  if(arena->alertCollection) {
    (*arena->alertCollection)(MPS_ALERT_COLLECTION_END, trace->why);
  }
}



/* --------  TraceIdMessages  -------- */


/* TraceIdMessagesCheck - pre-allocated messages for this traceid.
 *
 * Messages are absent when already sent, or when (exceptionally) 
 * ControlAlloc failed at the end of the previous trace.  If present, 
 * they must be valid.
 *
 * Messages are pre-allocated all-or-nothing.  So if we've got a 
 * start but no end, that's wrong.
 *
 * Note: this function does not take a pointer to a struct, so it is 
 * not a 'proper' _Check function.  It can be used in CHECKL, but 
 * not CHECKD etc.
 */

Bool TraceIdMessagesCheck(Arena arena, TraceId ti)
{
  CHECKL(!arena->tsMessage[ti]
         || TraceStartMessageCheck(arena->tsMessage[ti]));
  CHECKL(!arena->tMessage[ti]
         || TraceMessageCheck(arena->tMessage[ti]));
  CHECKL(! (arena->tsMessage[ti] && !arena->tMessage[ti]) );

  return TRUE;
}

/* TraceIdMessagesCreate -- pre-allocate all messages for this traceid
 *
 * See <design/message-gc/#lifecycle>.
 *
 * For remote control of ControlAlloc, to simulate low memory:
 *  #define ControlAlloc !TIMCA_remote() ? ResFAIL : ControlAlloc
 *  extern Bool TIMCA_remote(void);
 * See TIMCA_remote() in zmess.c
 */

Res TraceIdMessagesCreate(Arena arena, TraceId ti)
{
  void *p;
  TraceStartMessage tsMessage;
  TraceMessage tMessage;
  Res res;
  
  /* Ensure we don't leak memory */
  AVER(!arena->tsMessage[ti]);
  AVER(!arena->tMessage[ti]);
  
  res = ControlAlloc(&p, arena, sizeof(TraceStartMessageStruct), FALSE);
  if(res != ResOK)
    goto failTraceStartMessage;
  tsMessage = p;

  res = ControlAlloc(&p, arena, sizeof(TraceMessageStruct), FALSE);
  if(res != ResOK)
    goto failTraceMessage;
  tMessage = p;

  traceStartMessageInit(arena, tsMessage);
  AVERT(TraceStartMessage, tsMessage);
  arena->tsMessage[ti] = tsMessage;

  traceMessageInit(arena, tMessage);
  AVERT(TraceMessage, tMessage);
  arena->tMessage[ti] = tMessage;
  
  AVER(TraceIdMessagesCheck(arena, ti));
  
  return ResOK;

failTraceMessage:
  ControlFree(arena, tsMessage, sizeof(TraceStartMessageStruct));
failTraceStartMessage:
  AVER(TraceIdMessagesCheck(arena, ti));
  return res;
}

/* TraceIdMessagesDestroy -- destroy any pre-allocated messages
 *
 * Only used during ArenaDestroy.
 *
 * See <design/message-gc/#lifecycle>.
 */

void TraceIdMessagesDestroy(Arena arena, TraceId ti)
{
  TraceStartMessage tsMessage;
  TraceMessage tMessage;
  
  AVER(TraceIdMessagesCheck(arena, ti));

  tsMessage = arena->tsMessage[ti];
  if(tsMessage) {
    arena->tsMessage[ti] = NULL;
    TraceStartMessageDelete(TraceStartMessageMessage(tsMessage));
  }

  tMessage = arena->tMessage[ti];
  if(tMessage) {
    arena->tMessage[ti] = NULL;
    TraceMessageDelete(TraceMessageMessage(tMessage));
  }

  AVER(!arena->tsMessage[ti]);
  AVER(!arena->tMessage[ti]);
  AVER(TraceIdMessagesCheck(arena, ti));
}



/* --------  ArenaRelease, ArenaClamp, ArenaPark  -------- */


/* ArenaRelease, ArenaClamp, ArenaPark -- allow/prevent collection work.
 *
 * These functions allow or prevent collection work.
 */


/* Forward Declarations */
static void arenaForgetProtection(Globals globals);


/* ArenaClamp -- clamp the arena (no optional collection increments) */

void ArenaClamp(Globals globals)
{
  AVERT(Globals, globals);
  globals->clamped = TRUE;
}


/* ArenaRelease -- release the arena (allow optional collection
 * increments) */

void ArenaRelease(Globals globals)
{
  AVERT(Globals, globals);
  arenaForgetProtection(globals);
  globals->clamped = FALSE;
  (void)TracePoll(globals);
}


/* ArenaPark -- finish all current collections and clamp the arena */

void ArenaPark(Globals globals)
{
  TraceId ti;
  Trace trace;
  Arena arena;

  AVERT(Globals, globals);
  arena = GlobalsArena(globals);

  globals->clamped = TRUE;

  while(arena->busyTraces != TraceSetEMPTY) {
    /* Poll active traces to make progress. */
    TRACE_SET_ITER(ti, trace, arena->busyTraces, arena)
      TraceQuantum(trace);
      if(trace->state == TraceFINISHED) {
        TraceDestroy(trace);
      }
    TRACE_SET_ITER_END(ti, trace, arena->busyTraces, arena);
  }
}



/* --------  ArenaStartCollect, ArenaCollect  -------- */


/* ArenaStartCollect -- start a collection of everything in the
 * arena; leave unclamped. */

Res ArenaStartCollect(Globals globals, int why)
{
  Arena arena;
  Res res;
  Trace trace;

  AVERT(Globals, globals);
  arena = GlobalsArena(globals);

  ArenaPark(globals);
  res = TraceStartCollectAll(&trace, arena, why);
  if(res != ResOK)
    goto failStart;
  ArenaRelease(globals);
  return ResOK;

failStart:
  ArenaRelease(globals);
  return res;
}

/* ArenaCollect -- collect everything in arena; leave clamped */

Res ArenaCollect(Globals globals, int why)
{
  Res res;

  AVERT(Globals, globals);
  res = ArenaStartCollect(globals, why);
  if(res != ResOK)
    return res;

  ArenaPark(globals);
  return ResOK;
}



/* --------  Transforms  -------- */


static Bool transformCheckEpoch(Transform transform);
static Count transformSlotsRequired(Count capacity);
static OldNew transformSlot(Transform transform, Ref ref);
static unsigned long addrHash(Addr addr);
static Res transformSetCapacity(Transform transform, Count capacity);

/* Transforms: client interface */

Res TransformCreate(Transform *transformReturn, Arena arena)
{
  Transform transform;
  Res res;
  void *p;
  Globals globals;

  AVER(transformReturn != NULL);
  AVERT(Arena, arena);
  globals = ArenaGlobals(arena);

  res = ControlAlloc(&p, arena, sizeof(TransformStruct), FALSE);
  if (res != ResOK)
    return res;
  transform = (Transform)p;

  transform->arena = arena;

#if 0
  /* See <design/arena/#root-ring> */
  RingInit(&transform->arenaRing);
  transform->serial = globals->transformSerial;
  ++globals->transformSerial;
#endif

  transform->cSlots = 0;
  transform->cOldNews = 0;
  transform->aSlots = NULL;
  transform->iSlot = (Index)-1;
  STATISTIC(transform->slotCall = 0);
  STATISTIC(transform->slotMiss = 0);
  transform->epoch = ArenaEpoch(arena);

  transform->sig = TransformSig;

  AVERT(Transform, transform);
  transformCheckEpoch(transform);

  /*RingAppend(&globals->transformRing, &transform->arenaRing);*/

  *transformReturn = transform;
  return ResOK;
}

Res TransformAddOldNew(Transform transform, mps_addr_t *old_list, mps_addr_t *new_list, size_t count)
{
  Res res;
  Arena arena;
  Index i;
  Count added = 0;

  AVERT(Transform, transform);
  transformCheckEpoch(transform);
  arena = TransformArena(transform);
  AVER(old_list);
  AVER(new_list);
  /* count: cannot check */
  
  res = transformSetCapacity(transform, transform->cOldNews + count);
  if (res != ResOK)
    return res;

  /* Store lists, preserving order. */
  for(i = 0; i < count; i++) {
    OldNew oldnew;

    /* @@@@ To access client-provided lists, consider using ArenaPeek / TraceScanSeg */
    if(old_list[i] == NULL)
      continue;  /* permitted, but no transform to do */
    if(old_list[i] == new_list[i])
      continue;  /* ignore identity-transforms */

    oldnew = transformSlot(transform, old_list[i]);
    /* Is this old recorded already?  Not permitted. */
    AVER(oldnew->oldObj == NULL);
    oldnew->oldObj = old_list[i];
    oldnew->newObj = new_list[i];
    oldnew->next = NULL;
    added += 1;
    transform->cOldNews += 1;
  }

  AVERT(Transform, transform);
  transformCheckEpoch(transform);
  DIAG_SINGLEF(( "TransformAddOldNew_storelists",
    "Stored list of $U non-trivial old->new pairs, out of list of $U old->new pairs.", added, count, NULL ));
  
  return ResOK;
}

Res TransformApply(Bool *appliedReturn, Transform transform)
{
  Res res;
  Arena arena;
  Globals globals;
  Trace trace;
  
  AVER(appliedReturn != NULL);
  AVERT(Transform, transform);
  transformCheckEpoch(transform);
  arena = TransformArena(transform);

  globals = ArenaGlobals(arena);
  AVERT(Globals, globals);
  
  *appliedReturn = FALSE;

  ArenaPark(globals);
  AVER(arena->transform == NULL);
  arena->transform = transform;
  arena->transform_Abort = FALSE;
  arena->transform_Begun = FALSE;
  arena->transform_Found = 0;
  res = TraceStartTransform(&trace, arena);
  ArenaRelease(globals);
  ArenaPark(globals);
  /* at conclusion we expect either Abort, or Begun, or none found */
  AVER(!(arena->transform_Abort && arena->transform_Begun));
  /* done = TRUE iff either Begun or none found, ie. not Abort */
  *appliedReturn = !arena->transform_Abort;
  DIAG_SINGLEF(( "TransformApply_finished",
    "Reporting *appliedReturn = $U, $U non-trivial old->new pairs, edited $U refs.",
    *appliedReturn, transform->cOldNews, arena->transform_Found,
    NULL ));
  arena->transform = NULL;
  AVER(arena->transform == NULL);

  if(*appliedReturn)
    TransformDestroy(transform);

  return res;
}

void TransformDestroy(Transform transform)
{
  Arena arena;

  AVERT(Transform, transform);
  /* Don't transformCheckEpoch(transform); -- destroying a stale */
  /* transform is permitted (and necessary). */
  arena = TransformArena(transform);

  DIAG_SINGLEF(( "TransformDestroy",
    "transformSlot performance: calls $D, misses $D, misses/call $D",
    transform->slotCall, transform->slotMiss,
    transform->slotCall == 0.0
      ? 0.0
      : transform->slotMiss / transform->slotCall,
    NULL ));

#if 0
  RingRemove(&transform->arenaRing);
  RingFinish(&transform->arenaRing);
#endif

  if(transform->aSlots != NULL) {
    AVER(transform->cSlots > 0);
    ControlFree(arena, transform->aSlots, transform->cSlots * sizeof(OldNewStruct));
    transform->aSlots = NULL;
  }

  transform->sig = SigInvalid;

  ControlFree(arena, transform, sizeof(TransformStruct));
}

/* Transforms: internal interface */

Bool TransformCheck(Transform transform)
{
  CHECKS(Transform, transform);
  CHECKU(Arena, transform->arena);
#if 0
  CHECKL(transform->serial < ArenaGlobals(transform->arena)->transformSerial);
  CHECKL(RingCheck(&transform->arenaRing));
#endif
  if(transform->aSlots == NULL) {
    CHECKL(transform->cSlots == 0);
    CHECKL(transform->cOldNews == 0);
    CHECKL(transform->iSlot == (Index)-1);
  } else {
    CHECKL(transform->cSlots >= 1);
    CHECKL(transform->cOldNews < transform->cSlots);
    CHECKL(transform->iSlot == (Index)-1
           || transform->iSlot <= transform->cSlots);
    /* check table is not cramped */
    CHECKL(transformSlotsRequired(transform->cOldNews) <= transform->cSlots);
    /* check first and last elements */
    CHECKS(OldNew, transform->aSlots);
    CHECKS(OldNew, &transform->aSlots[transform->cSlots - 1]);
  }
  return TRUE;
}

Arena TransformArena(Transform transform)
{
  Arena arena;
  AVERT(Transform, transform);
  arena = transform->arena;
  AVERT(Arena, arena);
  return arena;
}

Bool TransformOldFirst(Ref *refReturn, Transform transform)
{
  if(transform->cOldNews == 0)
    return FALSE;

  /* Begin iteration. */
  AVER(transform->iSlot == (Index)-1);
  transform->iSlot = 0;

  return TransformOldNext(refReturn, transform);
}

Bool TransformOldNext(Ref *refReturn, Transform transform)
{
  Ref ref;

  AVER(transform->iSlot != (Index)-1);
  AVER(transform->iSlot <= transform->cSlots);

  do {
    if(transform->iSlot >= transform->cSlots) {
      /* End iteration. */
      transform->iSlot = (Index)-1;
      return FALSE;
    }
  
    ref = transform->aSlots[transform->iSlot].oldObj;
    transform->iSlot += 1;
  
    if(ref != NULL) {
      *refReturn = ref;
      return TRUE;
    }
  } while(TRUE);
}

Bool TransformGetNew(Ref *refNewReturn, Transform transform, Ref refOld)
{
  OldNew oldnew;

  AVERT_CRITICAL(Transform, transform);

  /* Find old->new pair. */
  oldnew = transformSlot(transform, refOld);
  if(oldnew->oldObj == NULL) {
    return FALSE;
  } else {
    AVER_CRITICAL(oldnew->oldObj == refOld);
    *refNewReturn = oldnew->newObj;
    return TRUE;
  }
}

/* Transforms: local functions */

static Bool transformCheckEpoch(Transform transform)
{
  if(TransformCheck(transform)) {
    /* [Temporary, while OldNews not scanned.  RHSK 2010-12-16] */
    CHECKL(transform->epoch == ArenaEpoch(transform->arena));
  }
  return TRUE;
}

static Count transformSlotsRequired(Count capacity)
{
  /* Choose an appropriate proportion of slots to remain free.  
   * Necessary for closed hashing, to avoid large-sized contiguous 
   * clumps of full cells, because each clump has a linear search cost. 
   */
  return capacity + (capacity / 3) + 1;
}

/* transformSetCapacity -- set the capacity, preserving contents
 *
 * Ensure the transform's hashtable can accommodate N entries (filled 
 * slots), without becoming cramped.  If necessary, resize the 
 * hashtable by allocating a new one and rehashing all old entries.
 * If insufficient memory, return error without modifying transform.
 */

static Res transformSetCapacity(Transform transform, Count capacity)
{
  Arena arena;
  Res res;
  Count cSlotsRequired;
  void *p;
  Count cSlotsPrev;
  Count cOldNewsPrev;
  OldNew aSlotsPrev;
  Index i;

  AVERT(Transform, transform);
  AVER(capacity >= transform->cOldNews);
  arena = TransformArena(transform);

  cSlotsRequired = transformSlotsRequired(capacity);
  AVER(cSlotsRequired > 0);
  if(transform->cSlots >= cSlotsRequired)
    return ResOK;

  /* Alloc new table and rehash. */
  DIAG_SINGLEF(( "transformSetCapacity_rehash",
    "capacity $U, ", capacity,
    "cSlots $U, ", transform->cSlots,
    "cSlotsRequired $U", cSlotsRequired,
    NULL ));

  /* alloc next */
  res = ControlAlloc(&p, arena, cSlotsRequired * sizeof(OldNewStruct),
                     /* withReservoirPermit */ FALSE);
  if (res != ResOK) {
    DIAG_SINGLEF(( "transformSetCapacity_fail_controlalloc",
      "cSlotsRequired $U", cSlotsRequired, NULL ));
    return res;
  }
  /* save prev */
  cSlotsPrev = transform->cSlots;
  cOldNewsPrev = transform->cOldNews;
  aSlotsPrev = transform->aSlots;
  /* init next */
  transform->cSlots = cSlotsRequired;
  transform->cOldNews = 0;
  transform->aSlots = (OldNew)p;
  for(i = 0; i < transform->cSlots; i++) {
    OldNew oldnew = &transform->aSlots[i];
    oldnew->oldObj = NULL;
    oldnew->newObj = NULL;
    oldnew->next = NULL;
    oldnew->sig = OldNewSig;
  }
  AVERT(Transform, transform);
  /* rehash prev -> next */
  for(i = 0; i < cSlotsPrev; i++) {
    OldNew prev;
    OldNew oldnew;
    prev = &aSlotsPrev[i];
    if(prev->oldObj != NULL) {
      oldnew = transformSlot(transform, prev->oldObj);
      /* Is this old recorded already?  Not permitted. */
      AVER(oldnew->oldObj == NULL);
      oldnew->oldObj = prev->oldObj;
      oldnew->newObj = prev->newObj;
      transform->cOldNews += 1;
    }
  }
  AVER(transform->cOldNews == cOldNewsPrev);
  AVERT(Transform, transform);
  /* free prev */
  if(aSlotsPrev != NULL) {
    AVER(cSlotsPrev > 0);
    ControlFree(arena, aSlotsPrev, cSlotsPrev * sizeof(OldNewStruct));
  }
  return ResOK;
}

/* addrHash -- return a hash value from an address
 *
 * This uses a single cycle of an MLCG, more commonly seen as a 
 * pseudorandom number generator.  It works extremely well as a 
 * hash function.
 *
 * (In particular, it is substantially better than simply doing this:
 *   seed = (unsigned long)addr * 48271;
 * Tested by RHSK 2010-12-28.)
 *
 * This MLCG is a full period generator: it cycles through every 
 * number from 1 to m-1 before repeating.  Therefore, no two numbers 
 * in that range hash to the same value.  Furthermore, it has prime 
 * modulus, which tends to avoid recurring patterns in the low-order 
 * bits, which is good because the hash will be used modulus the 
 * number of slots in the table.
 *
 * Of course it's only a 31-bit cycle, so we start by losing the top 
 * bit of the address, but that's hardly a great problem.
 *
 * The implementation is quite subtle.  See rnd() in testlib.c, where 
 * it has been exhaustively (ie: totally) tested.  RHSK 2010-12-28.
 */
#define R_m 2147483647UL
#define R_a 48271UL
static unsigned long addrHash(Addr addr)
{
  unsigned long seed = ((unsigned long)addr) & 0x7FFFFFFF;
  /* requires m == 2^31-1, a < 2^16 */
  unsigned long bot = R_a * (seed & 0x7FFF);
  unsigned long top = R_a * (seed >> 15);
  seed = bot + ((top & 0xFFFF) << 15) + (top >> 16);
  if(seed > R_m)
    seed -= R_m;
  return seed;
}

static OldNew transformSlot(Transform transform, Ref ref)
{
  Index j;
  Index i;

  STATISTIC(transform->slotCall += 1.0);

  j = addrHash(ref) % transform->cSlots;
  i = j;
  do {
    if(transform->aSlots[i].oldObj == ref
       || transform->aSlots[i].oldObj == NULL) {
     return &transform->aSlots[i];
    }
    STATISTIC(transform->slotMiss += 1.0);
    i++;
    if(i >= transform->cSlots)
      i = 0;
    AVER_CRITICAL(i == i % transform->cSlots);
    AVER_CRITICAL(i != j);
  } while(i != j);

  NOTREACHED;
  return NULL;
}


/* --------  ExposeRemember and RestoreProtection  -------- */


/* Low level stuff for Expose / Remember / Restore */

struct RememberedSummaryBlockStruct {
  RingStruct globalRing;        /* link on globals->rememberedSummaryRing */
  struct SummaryPair {
    Addr base;
    RefSet summary;
  } the[RememberedSummaryBLOCK];
};

typedef struct RememberedSummaryBlockStruct *RememberedSummaryBlock;

static void rememberedSummaryBlockInit(struct RememberedSummaryBlockStruct *block)
{
  size_t i;

  RingInit(&block->globalRing);
  for(i = 0; i < RememberedSummaryBLOCK; ++ i) {
    block->the[i].base = (Addr)0;
    block->the[i].summary = RefSetUNIV;
  }
}

static Res arenaRememberSummaryOne(Globals global, Addr base, RefSet summary)
{
  Arena arena;
  RememberedSummaryBlock block;

  AVER(summary != RefSetUNIV);

  arena = GlobalsArena(global);

  if(global->rememberedSummaryIndex == 0) {
    void *p;
    RememberedSummaryBlock newBlock;
    int res;

    res = ControlAlloc(&p, arena, sizeof *newBlock, 0);
    if(res != ResOK) {
      return res;
    }
    newBlock = p;
    rememberedSummaryBlockInit(newBlock);
    RingAppend(GlobalsRememberedSummaryRing(global),
      &newBlock->globalRing);
  }
  block = RING_ELT(RememberedSummaryBlock, globalRing,
    RingPrev(GlobalsRememberedSummaryRing(global)));
  AVER(global->rememberedSummaryIndex < RememberedSummaryBLOCK);
  AVER(block->the[global->rememberedSummaryIndex].base == (Addr)0);
  AVER(block->the[global->rememberedSummaryIndex].summary == RefSetUNIV);
  block->the[global->rememberedSummaryIndex].base = base;
  block->the[global->rememberedSummaryIndex].summary = summary;
  ++ global->rememberedSummaryIndex;
  if(global->rememberedSummaryIndex >= RememberedSummaryBLOCK) {
    AVER(global->rememberedSummaryIndex == RememberedSummaryBLOCK);
    global->rememberedSummaryIndex = 0;
  }

  return ResOK;
}

/* ArenaExposeRemember -- park arena and then lift all protection
   barriers.  Parameter 'remember' specifies whether to remember the
   protection state or not (for later restoration with
   ArenaRestoreProtection).
   */
void ArenaExposeRemember(Globals globals, int remember)
{
  Seg seg;
  Arena arena;

  AVERT(Globals, globals);

  ArenaPark(globals);

  arena = GlobalsArena(globals);
  if(SegFirst(&seg, arena)) {
    Addr base;

    do {
      base = SegBase(seg);
      if(IsSubclassPoly(ClassOfSeg(seg), GCSegClassGet())) {
        if(remember) {
          RefSet summary;

          summary = SegSummary(seg);
          if(summary != RefSetUNIV) {
            Res res = arenaRememberSummaryOne(globals, base, summary);
            if(res != ResOK) {
              /* If we got an error then stop trying to remember any
              protections. */
              remember = 0;
            }
          }
        }
        SegSetSummary(seg, RefSetUNIV);
        AVER(SegSM(seg) == AccessSetEMPTY);
      }
    } while(SegNext(&seg, arena, base));
  }
}

void ArenaRestoreProtection(Globals globals)
{
  Ring node, next;
  Arena arena;

  arena = GlobalsArena(globals);

  RING_FOR(node, GlobalsRememberedSummaryRing(globals), next) {
    RememberedSummaryBlock block =
      RING_ELT(RememberedSummaryBlock, globalRing, node);
    size_t i;

    for(i = 0; i < RememberedSummaryBLOCK; ++ i) {
      Seg seg;
      Bool b;

      if(block->the[i].base == (Addr)0) {
        AVER(block->the[i].summary == RefSetUNIV);
        continue;
      }
      b = SegOfAddr(&seg, arena, block->the[i].base);
      if(b && SegBase(seg) == block->the[i].base) {
        AVER(IsSubclassPoly(ClassOfSeg(seg), GCSegClassGet()));
        SegSetSummary(seg, block->the[i].summary);
      } else {
        /* Either seg has gone or moved, both of which are */
        /* client errors. */
        NOTREACHED;
      }
    }
  }

  arenaForgetProtection(globals);
}

static void arenaForgetProtection(Globals globals)
{
  Ring node, next;
  Arena arena;

  arena = GlobalsArena(globals);
  /* Setting this early means that we preserve the invariant
     <code/global.c#remembered.summary> */
  globals->rememberedSummaryIndex = 0;
  RING_FOR(node, GlobalsRememberedSummaryRing(globals), next) {
    RememberedSummaryBlock block =
      RING_ELT(RememberedSummaryBlock, globalRing, node);

    RingRemove(node);
    ControlFree(arena, block, sizeof *block);
  }
}

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2003, 2006-2008 Ravenbrook Limited
 * <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
