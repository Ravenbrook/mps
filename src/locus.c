/* impl.c.locus: LOCI
 *
 * $HopeName: MMsrc!locus.c(MMdevel_ptw_pseudoloci.3) $
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
static void LocusClientEnsureLocus(LocusClient client);
static void LocusZoneRangeBest(Addr *baseReturn, Addr *limitReturn,
                               Locus locus);
static void LocusZoneRangeNextBest(Addr *baseReturn, Addr
                                   *limitReturn, Locus locus);
static void LocusRefSetBest(Locus locus);
static void LocusRefSetNextBest(Locus locus);
static Count LocusLocusClientDistance(Locus locus,
                                      LocusClient client);
static Count LogCount(Count val);
static Bool LocusManagerCheck(LocusManager manager);
static Bool LocusClientCheck(LocusClient client);
static Bool LocusCheck(Locus locus);


/* Private types */


/* Accessors */

/* @@@ belongs in impl.c.arena */
LocusManager ArenaLocusManager(Arena arena) 
{
  return &arena->locusManagerStruct;
}

static Arena LocusManagerArena(LocusManager manager)
{
  return PARENT(ArenaStruct, locusManagerStruct, manager);
}

/* @@@ belongs in impl.c.pool */
LocusClient PoolLocusClient(Pool pool)
{
  return &pool->locusClientStruct;
}

static Ring LocusClientRing(Locus locus)
{
  return &locus->clientRingStruct;
}

static Ring LocusClientLocusRing(LocusClient client)
{
  return &client->locusRingStruct;
}

static Locus LocusClientLocus(LocusClient client)
{
  return client->locus;
}

static LocusManager LocusLocusManager(Locus locus)
{
  return locus->manager;
}


/* External Methods */


/* LocusManagerInit -- Initialize the locus manager */
void LocusManagerInit(LocusManager manager) 
{
  Locus locus;
  
  manager->ready = FALSE;
  
  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++)
    LocusInit(locus, manager);
  AVERT(LocusManager, manager);
}


/* LocusManagerFinish -- Finish the locus manager */
void LocusManagerFinish(LocusManager manager)
{
  AVERT(LocusManager, manager);
  
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
  client->disdained = RefSetEMPTY;
  /* default: no lifetime */
  client->lifetime = 0;
  client->used = RefSetEMPTY;
  RingInit(LocusClientLocusRing(client));
  client->locusSerial = 0;
  AVERT(LocusClient, client);
}


/* LocusClientFinish -- Called when a locus client is done allocating,
   typically because the pool or generation is being destroyed */
void LocusClientFinish(LocusClient client)
{
  AVERT(LocusClient, client);
  
  if (client->assigned) {
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
void LocusClientSetCohortParameters(LocusClient client,
                                    RefSet preferred,
                                    RefSet disdained,
                                    /* @@@ cohort parameters */
                                    Index lifetime)
{
  AVERT(LocusClient, client);
  AVER(RefSetInter(preferred, disdained) == RefSetEMPTY);
  /* @@@ Lifetime */
  AVER(! client->assigned);
  if (client->assigned)
    LocusClientFinish(client);
  
  client->preferred = preferred;
  client->disdained = disdained;
  client->lifetime = lifetime;
  client->used = RefSetEMPTY;
}


/* LocusClientZoneRangeBest -- Call LocusZoneRangeBest on this
   client's locus */
void LocusClientZoneRangeBest(Addr *baseReturn, Addr *limitReturn,
                              LocusClient client)
{
  Locus locus;
  
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(LocusClient, client);
  
  locus = LocusClientLocus(client);

  LocusClientEnsureLocus(client);
  
  LocusZoneRangeBest(baseReturn, limitReturn, locus);
}


/* LocusClientZoneRangeNextBest -- Call LocusZoneRangeNextBest on this
   client's locus */
void LocusClientZoneRangeNextBest(Addr *baseReturn,
                                  Addr *limitReturn,
                                  LocusClient client)
{
  Locus locus;
  
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(LocusClient, client);
  
  locus = LocusClientLocus(client);

  AVER(client->assigned);
  
  LocusZoneRangeNextBest(baseReturn, limitReturn, locus);
}
    

/* LocusClientSegAdd -- Must be called by the locus client any time it
   acquires a new segment */
void LocusClientSegAdd(LocusClient client, Arena arena, Seg seg)
{
  RefSet segRefSet;
  RefSet previous;
  RefSet next;

  AVERT(LocusClient, client);
  AVERT(Seg, seg);
  AVER(client->assigned);
  
  segRefSet = RefSetOfSeg(arena, seg);
  previous = client->used;
  next = RefSetUnion(previous, segRefSet);

  /* If this is a new zone for this client, check the locus is up to
     date */
  if (! RefSetSuper(previous, next)) {
    Locus locus = LocusClientLocus(client);
    RefSet locusPrevious = locus->used;
    RefSet locusNext = RefSetUnion(locusPrevious, segRefSet);
    
    client->used = next;
    
    /* If this is a new zone for this locus, check the manager is up
       to date  */
    if (! RefSetSuper(locusPrevious, locusNext)) {
      LocusManager manager = LocusLocusManager(locus);

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
    Locus locus;
    
    manager->free = RefSetUNIV;
    
    for (locus = &manager->locus[0];
         locus < &manager->locus[NUMLOCI];
         locus++)
    if (locus->inUse)
    {
      LocusEnsureReady(locus);
      
      manager->free = RefSetDiff(manager->free, locus->used);
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
  locus->preferred = RefSetEMPTY;
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
  if (locus->inUse && (! locus->ready)) {
    Ring this, next;
    RefSet preferred, disdained, used;
    Index lifetime;
  
    lifetime = (Index)-1;
    preferred = RefSetEMPTY;
    disdained = RefSetEMPTY;
    used = RefSetEMPTY;

    RING_FOR(this, LocusClientRing(locus), next) 
      {
        LocusClient client = RING_ELT(LocusClient, locusRingStruct,
                                      this);
        if (lifetime == -1)
          lifetime = client->lifetime;
        else
          lifetime = (lifetime + client->lifetime) / 2;
        preferred = RefSetUnion(preferred, client->preferred);
        disdained = RefSetUnion(disdained, client->disdained);
        used = RefSetUnion(used, client->used);
      }

    /* @@@ we know the locus manager cache only depends on used for
       the current policy */
    locus->lifetime = lifetime;
    locus->preferred = preferred;    
    locus->disdained = disdained;
    if (locus->used != used) {
      locus->used = used;    
      LocusLocusManager(locus)->ready = FALSE;
    }
    locus->ready = TRUE;
  }
}


/* LocusClientEnsureLocus -- Called to assign a client to a locus,
   based on the previously set parameters */
static void LocusClientEnsureLocus(LocusClient client)
{
  if (! client->assigned) {
    LocusManager manager = client->manager;
    Locus locus;
    Locus free = NULL;
    Locus best = NULL;
    Count bestDistance = (Count)-1;
      
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
        Count distance = LocusLocusClientDistance(locus, client);
        if (best == NULL || distance < bestDistance) {
          best = locus;
          bestDistance = distance;
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


/* LocusZoneRangeBest -- Calculate the optimal zone range clients of
   this locus can allocate in.  Must be called before each allocation
   to determine where to allocate. */
static void LocusZoneRangeBest(Addr *baseReturn, Addr *limitReturn, Locus locus)
{
  LocusRefSetBest(locus);
  locus->searchIndex = 0;

  LocusZoneRangeNextBest(baseReturn, limitReturn, locus);
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
        *baseReturn = (Addr)(i << zoneShift);
        *limitReturn =
          (Addr)(((i + 1) & (MPS_WORD_WIDTH - 1)) << arena->zoneShift);
        for (; i < MPS_WORD_WIDTH; i++) {
          if (! BS_IS_MEMBER(ref, i)) {    
            *limitReturn = (Addr)(i << zoneShift);
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

  LocusRefSetNextBest(locus);
}


/* LocusRefSetNextBest -- Calculate the next most optimal RefSet for
   the locus to allocate in.  May be called any number of times and
   each time it will yield a larger, but less optimal RefSet.  It is
   fruitless to call it when the search RefSet is already RefSetUNIV
   */
static void LocusRefSetNextBest(Locus locus)
{
  LocusManager manager = LocusLocusManager(locus);
  RefSet previous = locus->search;
  RefSet next = RefSetEMPTY;
  AVER(previous != RefSetUNIV);
  
  LocusManagerEnsureReady(manager);

  next = RefSetUnion(previous, RefSetInter(locus->used, locus->preferred));
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, RefSetInter(manager->free, locus->preferred));
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, RefSetDiff(locus->used, locus->disdained));
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, RefSetDiff(manager->free, locus->disdained));
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, locus->used);
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  next = RefSetUnion(previous, manager->free);
  if (! RefSetSuper(previous, next)) {
    locus->search = next;
    return;
  }
  locus->search = RefSetUNIV;
  return;
}
  

/* LocusLocusClientDistance -- measure the distance between the cohort
   specification of a locus and a locus client*/
static Count LocusLocusClientDistance(Locus locus,
                                      LocusClient client)
{
  Count lifediff = locus->lifetime < client->lifetime ?
    (Count)(client->lifetime - locus->lifetime):
    (Count)(locus->lifetime - client->lifetime);

  return lifediff +
    LogCount((Count)BS_SYM_DIFF(locus->preferred, client->preferred)) +
    LogCount((Count)BS_SYM_DIFF(locus->disdained, client->disdained));
}

  
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
static Count LogCount(Count val) 
{
  Count temp;
  AVER(MPS_WORD_WIDTH == 32);
  
  temp = (val >> 1) & 033333333333;
  temp = val - temp - ((temp >> 1) & 033333333333);
  return ((Count)(((temp + (temp >> 3)) & 030707070707) % 077));
}


static Bool LocusManagerCheck(LocusManager manager) 
{
  Locus locus;

  CHECKL(BoolCheck(manager->ready));
  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++) {
    CHECKL(LocusCheck(locus));
    if (locus->inUse && locus->ready)
      CHECKL(RefSetInter(manager->free, locus->used) == RefSetEMPTY);
  }
  return TRUE;
}


static Bool LocusClientCheck(LocusClient client)
{
  CHECKL(BoolCheck(client->assigned));
  if (client->assigned) {
    CHECKL(LocusCheck(client->locus));
  } else {
    CHECKL(RingCheckSingle(LocusClientLocusRing(client)));
  }
  CHECKL(RingCheck(LocusClientLocusRing(client)));
  CHECKL(RefSetCheck(client->preferred));
  CHECKL(RefSetCheck(client->disdained));
  CHECKL(RefSetCheck(client->used));
  return TRUE;
}
 
  
static Bool LocusCheck(Locus locus)
{
  CHECKL(BoolCheck(locus->inUse));
  CHECKL(BoolCheck(locus->ready));
  if (locus->ready) {
    Ring this, next;
    CHECKL(locus->inUse);
    CHECKL(RingCheck(LocusClientRing(locus)));
    RING_FOR(this, LocusClientRing(locus), next) 
    {
      LocusClient client = RING_ELT(LocusClient, locusRingStruct,
                                    this);
      CHECKL(RefSetSuper(locus->preferred, client->preferred));
      CHECKL(RefSetSuper(locus->disdained, client->disdained));
      CHECKL(RefSetSuper(locus->used, client->used));
    }
  }
  return TRUE;
}
