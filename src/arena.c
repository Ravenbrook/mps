/* impl.c.arena: GENERIC ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arena.c(MMdevel_arenaclass.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * The arena is the alpha and the omega.
 */

#include "mpm.h"

SRCID(arena, "$HopeName: MMsrc!arena.c(MMdevel_arenaclass.1) $");

#if 0
extern Bool SegPrefCheck(SegPref pref);
extern SegPref SegPrefDefault (void);
extern Res SegPrefExpress (SegPref, SegPrefKind, void *);
#endif


static Bool arenaRingInit = FALSE;
static RingStruct arenaRing;
static Serial arenaSerial;
static LockStruct arenaRingLock;


Bool ArenaClassCheck(ArenaClass class)
{
  CHECKS(ArenaClass, class);
  CHECKL(class->name != NULL); /* Should be <=6 char C identifier */
  CHECKL(class->size >= sizeof(ArenaStruct));
  /* Offset of generic Pool within class-specific instance cannot be */
  /* greater than the size of the class-specific portion of the instance */
  CHECKL(class->offset <= (size_t)(class->size - sizeof(ArenaStruct)));
  CHECKL(FUNCHECK(class->alloc));
  CHECKL(FUNCHECK(class->free));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->reserved));
  CHECKL(FUNCHECK(class->committed));
  CHECKL(FUNCHECK(class->extend));
  CHECKL(FUNCHECK(class->retract));
  CHECKL(FUNCHECK(class->segAlloc));
  CHECKL(FUNCHECK(class->segFree));
  CHECKL(FUNCHECK(class->segBase));
  CHECKL(FUNCHECK(class->segLimit));
  CHECKL(FUNCHECK(class->segOfAddr));
  CHECKL(FUNCHECK(class->segFirst));
  CHECKL(FUNCHECK(class->segNext));
  CHECKL(class->endSig == ArenaClassSig);
  return TRUE;
}


Bool ArenaCheck(Arena arena)
{
  CHECKS(Arena, arena);
  CHECKL(arena->serial < arenaSerial);
  CHECKD(ArenaClass, arena->class);
  CHECKL(RingCheck(&arena->globalRing));
  CHECKL(AlignCheck(arena->alignment));
  CHECKD(Lock, &arena->lockStruct);
  CHECKL(RingCheck(&arena->rootRing));
  CHECKL(RingCheck(&arena->poolRing));
  CHECKL(RingCheck(&arena->threadRing));
  CHECKL(RingCheck(&arena->formatRing));
  if(arena->poolReady)
    CHECKD(MV, &arena->controlPoolStruct);
  CHECKL(TraceSetCheck(arena->busyTraces));
  CHECKL(TraceSetCheck(arena->flippedTraces));
  CHECKL(TraceSetSuper(arena->busyTraces, arena->flippedTraces));
  /* @@@@ What about the other fields? */
  return TRUE;
}


/* ArenaCreate -- create and bootstrap an arena */

Res ArenaCreate(Arena *arenaReturn, ArenaClass class, Addr base, Size size)
{
  Res res;
  Arena arena;
  Index i;

  AVER(MPMCheck());
  EventInit();

  AVER(arenaReturn != NULL);
  AVERT(ArenaClass, class);
  /* check size, base */

  if(!arenaRingInit) {       /* @@@@ Race condition */
    LockInit(&arenaRingLock);
    LockClaim(&arenaRingLock);
    arenaRingInit = TRUE;
    RingInit(&arenaRing);
    arenaSerial = (Serial)0;
    ProtSetup();
  } else
    LockClaim(&arenaRingLock);

  /* Allocate space for the arena descriptor from somewhere. */
  res = (*class->alloc)(&arena);
  if(res != ResOK) goto failAlloc;

  /* Initialize the generic part of the arena */
  arena->class = class;
  RingInit(&arena->globalRing);
  arena->alignment = MPS_PF_ALIGN;	/* usually overridden by init */
  arena->zoneShift = ARENA_ZONESHIFT;	/* usually overridden by init */
  LockInit(&arena->lockStruct);
  RingInit(&arena->poolRing);
  arena->poolSerial = (Serial)0;
  RingInit(&arena->rootRing);
  arena->rootSerial = (Serial)0;
  RingInit(&arena->threadRing);
  arena->threadSerial = (Serial)0;
  RingInit(&arena->formatRing);
  arena->formatSerial = (Serial)0;
  arena->busyTraces = TraceSetEMPTY;    /* impl.c.trace */
  arena->flippedTraces = TraceSetEMPTY; /* impl.c.trace */
  arena->insideShield = FALSE;          /* impl.c.shield */
  arena->shCacheI = 0;
  arena->shDepth = 0;
  arena->suspended = 0;
  for(i = 0; i <SHIELD_CACHE_SIZE; i++)
    arena->shCache[i] = (Seg)0;
  arena->pollThreshold = (Size)0;
  arena->insidePoll = FALSE;
  arena->actionInterval = ARENA_POLL_MAX;	/* @@@@ */
  arena->epoch = (Epoch)0;              /* impl.c.ld */
  arena->prehistory = RefSetEMPTY;
  for(i = 0; i < ARENA_LD_LENGTH; ++i)
    arena->history[i] = RefSetEMPTY;
  arena->poolReady = FALSE;
  
  arena->sig = ArenaSig;
  arena->serial = arenaSerial;
  ++arenaSerial;
  
  AVERT(Arena, arena);

  /* Do class-specific initialization. */
  res = (*class->init)(arena, size, base);
  if(res != ResOK) goto failInit;

  /* Create the control pool */
  res = PoolInit(&arena->controlPoolStruct.poolStruct, 
                 arena, PoolClassMV(),
                 ARENA_CONTROL_EXTENDBY, ARENA_CONTROL_AVGSIZE,
                 ARENA_CONTROL_MAXSIZE);
  if(res != ResOK) goto failControlInit;
  arena->poolReady = TRUE;
  
  /* Add initialized arena to the global list of arenas. */
  RingAppend(&arenaRing, &arena->globalRing);
  LockReleaseMPM(&arenaRingLock);

  EVENT1(ArenaCreate, arena);

  *arenaReturn = arena;
  return ResOK;

failControlInit:
  (*class->finish)(arena);
failInit:
  arena->sig = SigInvalid;
  RingFinish(&arena->globalRing);
  (*class->free)(arena);
failAlloc:
  return res;
}


/* ArenaDestroy -- tear down and destroy an arena */

void ArenaDestroy(Arena arena)
{
  ArenaClass class;

  AVERT(Arena, arena);
  AVER(!arena->insideShield);
  AVER(!arena->insidePoll);
  /* @@@@ must be more we can check here */
  
  class = arena->class;

  /* Detach the arena from the global list. */
  LockClaim(&arenaRingLock);
  RingRemove(&arena->globalRing);
  LockReleaseMPM(&arenaRingLock);

  /* Destroy the control pool. */
  arena->poolReady = FALSE;
  PoolFinish(&arena->controlPoolStruct.poolStruct);

  /* Do any class-specific finishing. */
  (*class->finish)(arena);

  /* Finish the generic fields. */
  arena->sig = SigInvalid;
  LockFinish(&arena->lockStruct);
  RingFinish(&arena->poolRing);
  RingFinish(&arena->formatRing);
  RingFinish(&arena->rootRing);
  RingFinish(&arena->threadRing);
  RingFinish(&arena->globalRing);

  /* Send the arena descriptor back from whence it came. */
  (*class->free)(arena);  

  EVENT1(ArenaDestroy, arena);

  EventFinish();
}


void ArenaEnter(Arena arena)
{
  AVER(arena->sig == ArenaSig);
  LockClaim(&arena->lockStruct);
  ShieldEnter(arena);
}

void ArenaLeave(Arena arena)
{
  AVERT(Arena, arena);
  ShieldLeave(arena);
  ProtSync(arena);
  LockReleaseMPM(&arena->lockStruct);
}


/* ArenaAccess -- deal with an access fault
 *
 * This is called when a protected address is accessed.  The mode
 * corresponds to which mode bits need to be cleared in order
 * for the access to continue.
 */

Bool ArenaAccess(Addr addr, AccessSet mode)
{
  Seg seg;
  Ring node;

  LockClaim(&arenaRingLock);
  node = RingNext(&arenaRing);
  while(node != &arenaRing) {
    Arena arena = RING_ELT(Arena, globalRing, node);

    ArenaEnter(arena);
    AVERT(Arena, arena);
    if(ArenaSegOfAddr(&seg, arena, addr)) {
      LockReleaseMPM(&arenaRingLock);
      /* An access in a different thread may have already caused
       * the protection to be cleared.  This avoids calling
       * TraceAccess on protection that has already been cleared on
       * a separate thread
       */
      mode &= seg->pm;
      if(mode != 0)
        TraceAccess(arena, seg, mode);
      ArenaLeave(arena);
      return TRUE;
    }
    ArenaLeave(arena);

    node = RingNext(node);
  }

  LockReleaseMPM(&arenaRingLock);

  /* Currently the convention is that events are emitted on successful */
  /* completion of a function.  This is probably not very useful in */
  /* this case and the event should be moved to the beginning.  This */
  /* is waiting for Gavin's proposal and change. */
  EVENT2(ArenaAccess, addr, mode);

  return FALSE;
}


Res ArenaSegAlloc(Seg *segReturn, SegPref pref, Arena arena, Size size, Pool pool)
{
  Res res;
  Seg seg;

  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  AVERT(Arena, arena);
  AVER(size > 0);
  AVERT(Pool, pool);
  AVER(SizeIsAligned(size, arena->alignment));

  res = (*arena->class->segAlloc)(&seg, pref, arena, size, pool);
  if(res != ResOK) return res;

  EVENT4(ArenaSegAlloc, arena, seg, size, pool);

  *segReturn = seg;
  return ResOK;
}


void ArenaSegFree(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);
  
  (*arena->class->segFree)(arena, seg);

  EVENT2(ArenaSegFree, arena, seg);
}


Size ArenaReserved(Arena arena)
{
  AVERT(Arena, arena);
  return (*arena->class->reserved)(arena);
}


Size ArenaCommitted(Arena arena)
{
  AVERT(Arena, arena);
  return (*arena->class->committed)(arena);
}


Res ArenaExtend(Arena arena, Addr base, Size size)
{
  Res res;

  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > 0);

  res = (*arena->class->extend)(arena, base, size);
  if(res != ResOK) return res;
  
  EVENT3(ArenaExtend, arena, base, size);

  return ResOK;
}


Res ArenaRetract(Arena arena, Addr base, Size size)
{
  Res res;

  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > 0);

  res = (*arena->class->retract)(arena, base, size);
  if(res != ResOK) return res;
  
  EVENT3(ArenaRetract, arena, base, size);

  return ResOK;
}


Addr ArenaSegBase(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);
  
  return (*arena->class->segBase)(arena, seg);
}


Addr ArenaSegLimit(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);

  return (*arena->class->segLimit)(arena, seg);
}


/* ArenaVMSegSize -- return the size (limit - base) of a segment
 *
 * .improve.redundant-calc: There is scope for optimizing this,
 * because both base and limit calls may do roughly the same thing twice.
 */

Size ArenaSegSize(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);
  return AddrOffset(ArenaSegBase(arena, seg), ArenaSegLimit(arena, seg));
}


Bool ArenaSegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  AVER(segReturn != NULL);
  AVERT(Arena, arena);

  return (*arena->class->segOfAddr)(segReturn, arena, addr);
}


Seg ArenaSegFirst(Arena arena)
{
  AVERT(Arena, arena);
  return (*arena->class->segFirst)(arena);
}


Seg ArenaSegNext(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);
  return (*arena->class->segNext)(arena, seg);
}


Bool SegPrefCheck(SegPref pref)
{
  CHECKS(SegPref, pref);
  CHECKL(BoolCheck(pref->high));
  /* nothing else to check */
  return TRUE;
}

static SegPrefStruct segPrefDefault = {SegPrefSig, FALSE};

SegPref SegPrefDefault(void)
{
  return &segPrefDefault;
}

Res SegPrefExpress(SegPref sp, SegPrefKind kind, void *p)
{
  AVERT(SegPref,sp);
  AVER(sp != &segPrefDefault);

  switch(kind) {
  case SegPrefHigh:
    AVER(p == NULL);
    sp->high = TRUE;
    return ResOK;

  case SegPrefLow:
    AVER(p == NULL);
    sp->high = FALSE;
    return ResOK;

  default:
    /* see design.mps.pref.default */
    return ResOK;
  }
}


Res ArenaAlloc(void **baseReturn, Arena arena, Size size)
{
  Addr base;
  Res res;
  Pool pool;

  AVERT(Arena, arena);
  AVER(baseReturn != NULL);
  AVER(size > 0);

  pool = MVPool(&arena->controlPoolStruct);
  res = PoolAlloc(&base, pool, size);
  if(res != ResOK) return res;

  /* .arenaalloc.addr-conv: This is the place where we go from the managed */
  /* addresses of PoolAlloc to the unmanaged addresses of ArenaAlloc */
  *baseReturn = (void *)base;
  return ResOK;
}


void ArenaFree(Arena arena, Addr base, Size size)
{
  Pool pool;
  AVERT(Arena, arena);
  AVER(base != NULL);
  AVER(size > 0);

  pool = MVPool(&arena->controlPoolStruct);
  PoolFree(pool, base, size);
}


/* ArenaPoll -- trigger periodic actions
 *
 * Poll all background activities to see if they need to do anything.
 * ArenaPoll does nothing if the amount of committed memory is less
 * than the arena poll threshold.  This means that actions are taken
 * as the memory demands increase.
 * 
 * @@@@ This is where time is "stolen" from the mutator in addition
 * to doing what it asks and servicing accesses.  This is where the
 * amount of time should be controlled, perhaps by passing time
 * limits to the various other activities.
 *
 * @@@@ Perhaps this should be based on a process table rather than
 * a series of manual steps for looking around.  This might be
 * worthwhile if we introduce background activities other than
 * tracing.
 */

void ArenaPoll(Arena arena)
{
  TraceId ti;
  Size size;
  Size interval;
  Res res;

  AVERT(Arena, arena);

  size = ArenaCommitted(arena);
  if(arena->insidePoll || size < arena->pollThreshold)
    return;

  arena->insidePoll = TRUE;

  /* Poll actions to see if any new action is to be taken. */
  ActionPoll(arena);

  /* Poll active traces to make progress. */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetIsMember(arena->busyTraces, ti)) {
      Trace trace = ArenaTrace(arena, ti);

      res = TracePoll(trace);
      AVER(res == ResOK); /* @@@@ */

      /* @@@@ Pick up results and use for prediction. */
      if(trace->state == TraceFINISHED)
        TraceDestroy(trace);
    }

  /* Work out when to come back and poll again. */
  /* This takes the min of the action interval and all the */
  /* trace intervals and puts that in the poll threshold. */
  interval = ARENA_POLL_MAX;
  if(arena->actionInterval < interval)
    interval = arena->actionInterval;
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetIsMember(arena->busyTraces, ti)) {
      Trace trace = ArenaTrace(arena, ti);
      if(trace->interval < interval)
        interval = trace->interval;
    }
  size = ArenaCommitted(arena);
  arena->pollThreshold = size + interval;

  arena->insidePoll = FALSE;

  /* Currently the convention is that events are emitted on successful */
  /* completion of a function.  This is probably not very useful in */
  /* this case and the event should be moved to the beginning.  This */
  /* is waiting for Gavin's proposal and change. */
  EVENT1(ArenaPoll, arena);
}
