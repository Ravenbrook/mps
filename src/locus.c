/* impl.c.locus: LOCI
 *
 * $HopeName: MMsrc!locus.c(MMdevel_ptw_pseudoloci.1) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 */

#include "mpm.h"
#include "locus.h"

/* Signatures */

/* Private prototypes */

static void LocusManagerEnsureReady(LocusManager manager);
static void LocusInit(Locus locus, LocusManager manager);
static void LocusFinish(Locus locus);
static void LocusEnsureReady(Locus locus);
static void LocusZoneRangeBest(Addr *baseReturn, Addr *limitReturn,
                               Locus locus);
static void LocusZoneRangeNextBest(Addr *baseReturn, Addr
                                   *limitReturn, Locus locus);
static void LocusRefSetBest(Locus locus);
static void LocusRefSetNextBest(locus);
static Count LogCount(Count val);
static Count LocusCohortDistance(Locus locus, RefSet preferred,
                                 RefSet disdained, Index lifetime);


/* Private types */

typedef struct LocusStruct 
{
  Bool inUse;                   /* active */
  Bool ready;                   /* client summary valid */
  /* summary of client's cohort descriptions */
  Index lifetime;               /* lifetime estimate */
  RefSet preferred;             /* preferred zones */
  RefSet disdained;             /* disdained zones */
  RefSet used;                  /* zones in use by this locus */
  /* Support for refset search policy */
  RefSet search;
  Index searchIndex;
  LocusManager manager;         /* backpointer */
  RingStruct clientRingStruct;  /* clients */
  Serial clientSerial;
  /* @@@ placement description */
} LocusStruct;


/* Accessors */

static LocusManager ArenaLocusManager(Arena arena) 
{
  return &arena->locusManagerStruct;
}

static Arena LocusManagerArena(LocusManager locusManager)
{
  return PARENT(ArenaStruct, locusManagerStruct, locusManager);
}

static Ring LocusClientRing(Locus locus)
{
  return &locus->clientRingStruct;
}

static Ring LocusClientLocusRing(LocusClient locusClient)
{
  return &locusClient->locusRingStruct;
}

static Locus LocusClientLocus(LocusClient locusClient)
{
  return locusClient->locus;
}

static LocusLocusManager(Locus locus)
{
  return locus->manager;
}

static LocusManager LocusClientLocusManager(LocusClient locusClient)
{
  return LocusLocusManager(LocusClientLocus(locusClient));
}

/* External Methods */

/* LocusManagerInit -- Initialize the locus manager */
void LocusManagerInit(LocusManager manager) 
{
  locusManager->ready = FALSE;
  
  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++)
    LocusInit(locus, manager);
}

/* LocusManagerFinish -- Finish the locus manager */
void LocusManagerFinish(LocusManager manager)
{
  manager->ready = FALSE;
  /* @@@ Finish the loci? */
}

/* LocusClientInit -- Initialize a locus client and assign it to a
   locus manager */
void LocusClientInit(LocusClient client, LocusManager manager)
{
  client->manager = manager;
  client->assigned = FALSE;
  client->locus = NULL;
  /* default: everything is good, nothing is bad */
  client->preferred = RefSetUNIV;
  client->disdained = RefSetEmpty;
  /* default: no lifetime */
  client->lifetime = 0;
  client->used = RefSetEmpty;
  RingInit(LocusClientLocusRing(client));
  client->locusSerial = 0;
}


/* LocusClientFinish -- Called when a locus client is done allocating,
   typically because the pool or generation is being destroyed */
void LocusClientFinish(LocusClient client)
{
  if (client->assigned) {
    LocusManager manager = client->manager;
    Locus locus = LocusClientLocus(client);
    AVER(locus->inUse);

    client->assigned = FALSE;
    client->locus = (Locus)NULL;
    /* AVER client on LocusClientRing(locus) */
    RingRemove(LocusClientLocusRing(client));
    /* Leave client->serial */

    /* note change */
    locus->ready = FALSE;
    
    /* Decommission locus if it has no clients */
    if (RingIsSingle(LocusClientRing(locus)))
      LocusFinish(locus);
  }
}


/* LocusClientSetCohortParameters -- Set the cohort parameters for
   this client.  The client passes in any a priori zone preferences it
   knows.  @@@ eventually the client will pass in cohort parameters
   such as lifetime, allocation pattern frequency, phase, etc. */
LocusClientSetCohortParameters(LocusClient client
                               RefSet preferred,
                               RefSet disdained,
                               /* @@@ cohort parameters */
                               Index lifetime)
{
  AVER(! client->assigned);
  if (client->assigned)
    ClientFinish(client);
  
  client->preferred = preferred;
  client->disdained = disdained;
  client->lifetime = lifetime;
  client->used = RefSetEMPTY;
}

static Count LogCount(Count val) 
{
  /* HACKMEM #169 from MIT AI Memo 239, Feb. 29, 1972.  In order of one-ups-manship: Gosper, Mann, Lenard, [Root and Mann])
   * 
   * To count the ones in a PDP-6/10 word: 
   * 
   *         LDB B,[014300,,A]      ;or MOVE B,A then LSH B,-1
   *         AND B,[333333,,333333]
   *         SUB A,B
   *         LSH B,-1
   *         AND B,[333333,,333333]
   *         SUBB A,B               ;each octal digit is replaced by number of 1's in it
   *         LSH B,-3
   *         ADD A,B
   *         AND A,[070707,,070707]
   *         IDIVI A,77             ;casting out 63.'s
   * 
   * These ten instructions, with constants extended, would work on word lengths
   * up to 62.; eleven suffice up to 254..
   */
  Count temp;
  AVER(MPS_WORD_WIDTH == 32);
  
  temp = (val >> 1) & 033333333333;
  temp = val - temp - ((temp >> 1) & 033333333333);
  return ((Count)(((temp + (temp >> 3)) & 030707070707) % 077));
}

static Count LocusCohortDistance(Locus locus, RefSet preferred,
                                 RefSet disdained, Index lifetime)
{
  Count lifrdiff = locus->lifetime < lifetime ?
    (Count)(lifetime - locus->lifetime):
    (Count)(locus->lifetime - lifetime);

  return lifediff +
    LogCount((Count)BS_SYM_DIFF(locus->preferred, preferred)) +
    LogCount((Count)BS_SYM_DIFF(locus->disdained, disdained));
}

  
/* LocusClientEnsureLocus -- Called to assign a client to a locus,
   based on the previously set parameters */
static void LocusClientEnsureLocus(LocusClient client)
{
  if (! client->assigned) {
    LocusManager manager = client->manager;
    Locus free = NULL;
    Locus best = NULL;
    Count bestDistance (Count)-1;
      
    /* Search for free, matching, or near locus */
    for (locus = &manager->locus[0];
         locus < &manager->locus[NUMLOCI] &&
           bestDistance > 0;
         locus++)
    {
      LocusEnsureReady(locus);
      if (! locus->inUse) {
        if (free == NULL)
          free = locus;
      } else {
        Count distance = LocusCohortDistance(locus, client->preferred,
                                             client->disdained,
                                             client->lifetime);
        if (best == NULL || distance < bestDistance) {
          best = locus;
          bestDistance = locusDistance;
        }
      }
    }

    /* If no perfect match, use a free locus if you've got it */
    if (bestDistance != 0 && free != NULL) {
      best = free;
      best->lifetime = client->lifetime;
    }

    AVER(best != NULL);
    client->locus = best;
    best->inUse = TRUE;
    client->locusSerial = best->clientSerial;
    best->clientSerial++;
    RingAppend(LocusClientRing(best),
               LocusClientLocusRing(client));
    client->assigned = TRUE;

    best->lifetime = (best->lifetime + client->lifetime) / 2;
    best->preferred = RefSetUnion(locus->preferred, client->preferred);
    best->disdained = RefSetUnion(locus->disdained, client->disdained);
      
    /* note change */
    manager->ready = FALSE;
  }
}



/* LocusClientZoneRangeBest -- Call LocusZoneRangeBest on this
   client's locus */
void LocusClientZoneRangeBest(Addr *baseReturn, Addr *limitReturn,
                              LocusClient client)
{
  Locus locus = LocusClientLocus(client);

  LocusClientEnsureAssinged(client);
  
  return LocusZoneRangeBest(baseReturn, limitReturn, locus);
}

/* LocusClientZoneRangeNextBest -- Call LocusZoneRangeNextBest on this
   client's locus */
void LocusClientZoneRangeNextBest(Addr *baseReturn,
                                  Addr *limitReturn,
                                  LocusClient client)
{
  Locus locus = LocusClientLocus(client);

  return LocusZoneRangeNextBest(baseReturn, limitReturn, locus);
}
    
/* LocusClientSegAdd -- Must be called by the locus client any time it
   acquires a new segment */
void LocusClientSegAdd(LocusClient client, Seg seg)
{
  Locus locus = LocusClientLocus(client);
  LocusManager manager = LocusLocusManager(locus);
  RefSet segRefSet = RefSetOfSeg(Seg);
  RefSet previous = client->used;
  RefSet next = RefSetUnion(previous, segRefSet);
  
  /* If this is a new zone for this client, check the locus is up to
     date */
  if (! RefSetSuper(previous, next)) {
    RefSet locusPrevious = locus->used;
    RefSet locusNext = RefSetUnion(locusPrevious, segRefSet);
    
    client->used = next;
    
    /* If this is a new zone for this locus, check the manager is up
       to date  */
    if (! RefSetSuper(locusPrevious, locusNext)) {
      locus->used = locusNext;
      if (manager->ready) {
        manager->free = RefSetDiff(manager->free, segRefSet);
      }
    }
  }
}

    
/* Internal methods */

/* LocusManagerEnsureReady -- Called to ensure the locus managers
   cached zone information is up to date before any zone calculations
   are made */
static void LocusManagerEnsureReady(LocusManager manager)
{
  if (! manager->ready) {
    manager->free = RefSetUNIV;
    
    for (locus = &manager->locus[0];
         locus < &manager->locus[NUMLOCI];
         locus++)
    if (locus->inUse)
    {
      LocusEnsureReady(locus);
      
      manager->free = RefSetDiff(manager->free,
                                      locus->used);
    }
    manager->ready = TRUE;
  }
}

static void LocusInit(Locus locus, LocusManager manager)
{
  locus->inUse = FALSE;
  locus->ready = FALSE;
  locus->lifetime = 0;
  locus->preferred = RefSetEMPTY;
  locus->disdained = RefSetEMPTY;
  locus->used = RefSetEMPTY;
  RingInit(LocusClientRing(locus));
  locus->clientSerial = (Serial)1;
  locus->manager = manager;
}

static void LocusFinish(Locus locus)
{
  AVER(RingIsSingle(LocusClientRing(locus)));

  locus->inUse = FALSE;
  locus->ready = FALSE;
  locus->lifetime = 0;
  locus->prefered = RefSetEMPTY;
  locus->disdained = RefSetEMPTY;
  locus->used = RefSetEMPTY;
  /* leave clientSerial */

  /* note change */
  LocusLocusManager(locus)->ready = FALSE;
}
  

/* LocusEnsureReady -- Validate the cached cohort attributes of the
   locus's clients.  If they have changed, inform the locus manager */
static void LocusEnsureReady(Locus locus)
{
  Ring client, next;
  RefSet preferred, disdained, used;
  Index lifetime;
  Bool changed = FALSE;
  
  if (locus->inUse && (! locus->ready)) {
    lifetime = 0;
    prefered = RefSetEMPTY;
    disdained = RefSetEMPTY;
    used = RefSetEMPTY;

    RING_FOR(client, LocusClientRing(locus), next) 
      {
        LocusClient client = RING_ELT(LocusClient, locusRingStruct,
                                      client);
        /* @@@ max or min? */
        lifetime = client->lifetime;
        preferred = RefSetUnion(preferred, client->preferred);
        disdained = RefSetUnion(disdained, client->disdained);
        used = RefSetUnion(used, client->used);
      }

    if (locus->lifetime != lifetime) {
      changed = TRUE;
      locus->lifetime = lifetime;
    }
    if (locus->preferred ! = preferred) {
      changed = TRUE;
      locus->preferred = preferred;    
    }
    if (locus->disdained ! = disdained) {
      changed = TRUE;
      locus->disdained = disdained;
    }
    if (locus->used ! = used) {
      changed = TRUE;
      locus->used = used;    
    }
    locus->ready = TRUE;
    if (changed)
      locus->manager->ready = FALSE;
  }
}


/* LocusZoneRangeBest -- Calculate the optimal zone range clients of
   this locus can allocate in.  Must be called before each allocation
   to determine where to allocate. */
static void LocusZoneRangeBest(Addr *baseReturn, Addr *limitReturn, Locus locus)
{
  LocusManager manager = LocusLocusManager(locus);
  Arena arena = LocusManagerArena(manager);
  Word zoneShift = ArenaZoneShift(arena);
  
  LocusRefSetBest(locus);
  locus->searchIndex = 0;
  
  return LocusZoneRangeNextBest(baseReturn, limitReturn, locus);
}


/* LocusZoneRangeNextBest -- Calculate the next best zone range
   clients of this locus can allocate in.  May be called any number of
   times, will eventually return [0, 0), meaning "anywhere".  Should
   be called when allocation fails in the zone range returned by
   LocusZoneRangeBest */
static void LocusZoneRangeNextBest(Addr *baseReturn, Addr *limitReturn, Locus locus)
{
  LocusManager manager = LocusLocusManager(locus);
  Arena arena = LocusManagerArena(manager);
  Word zoneShift = ArenaZoneShift(arena);
  
  AVER(manager->ready);

  for (;;) {
    RefSet ref = locus->search;
    Index i = locus->searchIndex;

    if (! (i < MPS_WORD_WIDTH)) {
      LocusRefSetNextBest(locus);
      locus->searchIndex = 0;
    }
    for (; i < MPS_WORD_WIDTH; i++) {
      if (BS_IS_MEMBER(ref, i)) {
        *baseReturn = i << zoneShift;
        *limitReturn =
          ((i + 1) & (MPS_WORD_WIDTH - 1)) << arena->zoneShift;
        for (; i < MPS_WORD_WIDTH; j++) {
          if (! BS_IS_MEMBER(ref, i)) {    
            *limitReturn = i << zoneShift;
          }
        }
        locus->searchIndex = i;
        return;
      }
    }
  }
}


/* LocusRefSetBest -- Calculate the optimal RefSet for the locus to
   allocate in.  Must be called for each allocation.  */
static void LocusRefSetBest(Locus locus)
{
  locus->search = RefSetEMPTY;

  return LocusRefSetNextBest(locus);
}


/* LocusRefSetNextBest -- Calculate the next most optimal RefSet for
   the locus to allocate in.  May be called any number of times and
   each time it will yield a larger, but less optimal RefSet.  It is
   fruitless to call it when the search RefSet is already RefSetUNIV
   */
static void LocusRefSetNextBest(locus)
{
  RefSet previous = locus->search;
  RefSet next = RefSetEMPTY;
  AVER(previous != RefSetUNIV);
  
  LocusManagerEnsureReady(manager);

  next = RefSetUnion(previous, RefSetInter(locus->used, locus->preferred));
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, RefSetInter(locus->free, locus->preferred));
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, RefSetDiff(locus->used, locus->disdained));
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, RefSetDiff(locus->free, locus->disdained));
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, locus->used);
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, locus->free);
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  locus->search = RefSetUNIV;
  return;
}
  

