/* impl.c.locus: LOCI
 *
 * $HopeName: MMsrc!locus.c(MMdevel_ptw_pseudoloci.6) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .readership: any MPS developer
 *
 * .purpose: The Locus mechanism consists of a Locus Manager, which
 * manages a number of Loci, each of which represent a group of
 * LocusClients.  The Locus mechanism attempts to group clients into a
 * Locus according to their cohort attributes.  The Loci track their
 * clients zone usage and can offer advice to the Arena on what zones
 * to allocate segments in for each client in order to minimize mixing
 * cohorts in zones.  Two benefits accrue to Arena implementations
 * that heed this advice: 1. the segments of a zone will have more
 * uniform lifetime behavior (which should reduce segment-level
 * fragmentation), and 2. the accuracy of inter-cohort refsets will
 * improve (which should optimize garbage-collection scanning).
 *
 * As side-benefits: 1. the ZoneUsage summaries maintained by the
 * LocusManager, Locus, and LocusClient objects can be used to assess
 * the effectiveness of segment placement, and 2. the higher level
 * summaries may be of use to the collection strategy manager.
 *
 * .design: See design.mps.loci
 *
 * .limitation:  Presently only Pools are LocusClients.  Generational
 * pools may wish to treat each generation as a separate cohort (and
 * hence client).  This is not yet implemented.
 *
 */

#include "mpm.h"
#include "locus.h"


/* Signatures */


/* Private prototypes */

static void LocusManagerNoteZoneAlloc(LocusManager manager,
                                      RefSet ref);
static void LocusManagerNoteZoneFree(LocusManager manager,
                                     RefSet ref);
static void LocusInit(Locus locus, LocusManager manager);
static void LocusFinish(Locus locus);
static void LocusEnsureReady(Locus locus);
static void LocusClientEnsureLocus(LocusClient client);
static void LocusLocusClientDelete(Locus locus, LocusClient client);
static void LocusZoneRangeInitialize(Locus locus);
static Bool LocusZoneRangeFinished(Locus locus);
static void LocusZoneRangeNext(Addr *baseReturn, Addr
                               *limitReturn, Locus locus);
static void LocusNoteZoneAlloc(Locus locus, RefSet ref);
static void LocusNoteZoneFree(Locus locus, RefSet ref);
static void LocusRefSetNext(Locus locus);
static Count LocusLocusClientDistance(Locus locus,
                                      LocusClient client);
static void ZoneUsageInit(ZoneUsage desc);
static RefSet ZoneUsageFinish(ZoneUsage desc);
static RefSet ZoneUsageIncrement(ZoneUsage desc, RefSet ref);
static RefSet ZoneUsageDecrement(ZoneUsage desc, RefSet ref);
static Count LogCount(Count val);
static Bool LocusManagerCheck(LocusManager manager);
static Bool LocusClientCheck(LocusClient client);
static Bool LocusCheck(Locus locus);
static Bool ZoneUsageCheck(ZoneUsage desc);

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

static Pool LocusClientPool(LocusClient client)
{
  return PARENT(PoolStruct, locusClientStruct, client);
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

static ZoneUsage LocusClientZoneUsage(LocusClient client)
{
  return &client->zoneUsageStruct;
}

static ZoneUsage LocusZoneUsage(Locus locus)
{
  return &locus->zoneUsageStruct;
}

static ZoneUsage LocusManagerZoneUsage(LocusManager manager)
{
  return &manager->zoneUsageStruct;
}


/* External Methods */


/* Locus Manager Methods */


/* LocusManagerInit -- Initialize the locus manager */
void LocusManagerInit(LocusManager manager) 
{
  Locus locus;
  
  ZoneUsageInit(LocusManagerZoneUsage(manager));
  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++)
    LocusInit(locus, manager);
  AVERT(LocusManager, manager);
}


/* LocusManagerFinish -- Finish the locus manager */
void LocusManagerFinish(LocusManager manager)
{
  Locus locus;
  RefSet deleted;
  
  AVERT(LocusManager, manager);
  
  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++)
    LocusFinish(locus);
  deleted = ZoneUsageFinish(LocusManagerZoneUsage(manager));
  AVER(deleted == RefSetEMPTY);
}


/* Locus Client Methods */


/* LocusClientInit -- Initialize a locus client and assign it to a
   locus manager */
void LocusClientInit(LocusClient client, LocusManager manager)
{
  /* @@@ record client->parent if other than pools can be clients */
  client->manager = manager;
  client->assigned = FALSE;
  client->locus = NULL;
  /* default: everything is good, nothing is bad */
  client->preferred = RefSetUNIV;
  client->disdained = RefSetEMPTY;
  /* default: no lifetime */
  client->lifetime = 0;
  ZoneUsageInit(LocusClientZoneUsage(client));
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
    RefSet deleted = ZoneUsageFinish(LocusClientZoneUsage(client));

    if (deleted != RefSetEMPTY)
      LocusNoteZoneFree(locus, deleted);
    
    LocusLocusClientDelete(locus, client);
    
    client->assigned = FALSE;
    client->locus = (Locus)NULL;
  }
}

/* LocusClientSetCohortParameters -- Set the cohort parameters for
   this client.  The client passes in any a priori zone preferences it
   knows and a representation of its mean object lifetime.  @@@
   eventually the client will pass in more cohort parameters such as
   allocation pattern, frequency, phase, etc. */
void LocusClientSetCohortParameters(LocusClient client,
                                    RefSet preferred,
                                    RefSet disdained,
                                    /* @@@ cohort parameters */
                                    Index lifetime)
{
  AVERT(LocusClient, client);
  AVER(RefSetInter(preferred, disdained) == RefSetEMPTY);
  /* @@@ AVER lifetime */
  AVER(! client->assigned);

  /* failsafe: ignore an attempt to reset cohort (not supported) */
  if (! client->assigned) {
    client->preferred = preferred;
    client->disdained = disdained;
    client->lifetime = lifetime;
    ZoneUsageInit(LocusClientZoneUsage(client));
  }
}


/* ZoneRange iteration methods -- support iterating over the set of
   zone ranges from most to least desirable.  Used by the LocusManager
   Arena at SegAlloc time to choose the zone to allocate in
 */

/* LocusClientZoneRangeInitialize -- Initialize the zone range
   iteration */
void LocusClientZoneRangeInitialize(LocusClient client)
{
  Locus locus;

  AVERT(LocusClient, client);
  LocusClientEnsureLocus(client);
  locus = LocusClientLocus(client);
  
  LocusZoneRangeInitialize(locus);
}


/* LocusClientZoneRangeFinished -- Is the zone range iteration complete? */
Bool LocusClientZoneRangeFinished(LocusClient client)
{
  Locus locus;

  AVERT(LocusClient, client);
  AVER(client->assigned);
  locus = LocusClientLocus(client);
  
  return LocusZoneRangeFinished(locus);
}


/* LocusClientZoneRangeNext -- Return the next zone range in the
   iteration sequence */
void LocusClientZoneRangeNext(Addr *baseReturn,
                              Addr *limitReturn,
                              LocusClient client)
{
  Locus locus;
  
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(LocusClient, client);
  AVER(client->assigned);
  locus = LocusClientLocus(client);
  
  LocusZoneRangeNext(baseReturn, limitReturn, locus);
}


/* NoteSeg methods -- used by the LocusManager Arena to update the
   Locus ZoneUsage summaries on each segment allocation or free
   */

/* LocusClientNoteSegAlloc -- Inform the LocusManager that a segment
   has been allocated to a LocusClient  */
void LocusClientNoteSegAlloc(LocusClient client, Arena arena, Seg seg)
{
  RefSet segRefSet;
  RefSet new;

  AVERT(LocusClient, client);
  AVERT(Seg, seg);
  AVER(client->assigned);
  
  segRefSet = RefSetOfSeg(arena, seg);
  new = ZoneUsageIncrement(LocusClientZoneUsage(client), segRefSet);

  /* If there are new zones, update the locus */
  if (new != RefSetEMPTY) {
    LocusNoteZoneAlloc(LocusClientLocus(client), new);
  }
}


/* LocusClientNoteSegFree -- Inform the LocusManager that a client is
   no longer using a segment */
void LocusClientNoteSegFree(LocusClient client, Arena arena, Seg seg)
{
  RefSet segRefSet;
  RefSet deleted;

  AVERT(LocusClient, client);
  AVERT(Seg, seg);
  AVER(client->assigned);
  
  segRefSet = RefSetOfSeg(arena, seg);
  deleted = ZoneUsageDecrement(LocusClientZoneUsage(client), segRefSet);

  /* If there are deleted zones, update the locus */
  if (deleted != RefSetEMPTY) {
    LocusNoteZoneFree(LocusClientLocus(client), deleted);
  }
}


/* Internal Methods */


/* Locus Manager Methods */


/* LocusManagerNoteZoneAlloc -- Note that a locus is newly using a
   zone */
static void LocusManagerNoteZoneAlloc(LocusManager manager,
                                      RefSet ref)
{
  (void)ZoneUsageIncrement(LocusManagerZoneUsage(manager), ref);
}


/* LocusManagerNoteZoneFree -- Note that a locus is no longer using a
   zone */
static void LocusManagerNoteZoneFree(LocusManager manager,
                                      RefSet ref)
{
  (void)ZoneUsageDecrement(LocusManagerZoneUsage(manager), ref);
}


/* Locus Methods */


static void LocusInit(Locus locus, LocusManager manager)
{
  locus->inUse = FALSE;
  locus->ready = FALSE;
  locus->lifetime = 0;
  locus->preferred = RefSetEMPTY;
  locus->disdained = RefSetEMPTY;
  RingInit(LocusClientRing(locus));
  locus->clientSerial = (Serial)1;
  ZoneUsageInit(LocusZoneUsage(locus));
  locus->manager = manager;
}


static void LocusFinish(Locus locus)
{
  RefSet deleted;
  
  AVER(RingIsSingle(LocusClientRing(locus)));

  locus->inUse = FALSE;
  locus->ready = FALSE;
  locus->lifetime = 0;
  locus->preferred = RefSetEMPTY;
  locus->disdained = RefSetEMPTY;
  /* leave clientSerial */

  deleted = ZoneUsageFinish(LocusZoneUsage(locus));
  /* note change */
  LocusManagerNoteZoneFree(LocusLocusManager(locus), deleted);
}
  

/* LocusEnsureReady -- Validate the cached cohort attributes of the
   locus's clients */
static void LocusEnsureReady(Locus locus)
{
  if (locus->inUse && (! locus->ready)) {
    Ring this, next;
    RefSet preferred, disdained, used;
    Index lifetime;
  
    lifetime = (Index)-1;
    preferred = RefSetUNIV;
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
        preferred = RefSetInter(preferred, client->preferred);
        disdained = RefSetUnion(disdained, client->disdained);
        /* @@@ only for AVER */
        used = RefSetUnion(used, RefSetComp(LocusClientZoneUsage(client)->free));
      }

    locus->lifetime = lifetime;
    locus->preferred = preferred;    
    locus->disdained = disdained;
    AVER(used == RefSetComp(LocusZoneUsage(locus)->free));
    locus->ready = TRUE;
  }
}


/* LocusClientEnsureLocus -- Called to assign a client to a locus,
   based on the previously set parameters.  @@@ This is not strictly a
   locus method, but it is the logical inverse of
   LocusLocusClientDelete (v. i.) */
/* @@@ Clients are assigned first come, first served to the "best fit"
   locus.  This may not yield an optimal distribution of clients among
   loci, especially if clients are added and removed as time passes.
   Someday write a method that computes the cohort-distance among all
   clients and (re-)assigns them to loci to minimize the intra-locus
   cohort distance?  Sounds NP to me... and presumably has to be
   weighed against the cost of the resulting zone pollution from
   moving in-use clients. */
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
  }
}


/* LocusLocusClientDelete -- Delete a client from a locus, typically
   because the client is finished */
static void LocusLocusClientDelete(Locus locus, LocusClient client)
{
  AVER(locus->inUse);
  
  /* AVER client on LocusClientRing(locus) */
  RingRemove(LocusClientLocusRing(client));
  /* Leave client->serial */

  /* note change in cohort constituents */
  locus->ready = FALSE;
    
  /* Decommission locus if it has no clients */
  if (RingIsSingle(LocusClientRing(locus)))
    LocusFinish(locus);
}


/* LocusZoneRangeInitialize -- Initialize the locus zone range
   iteration */
static void LocusZoneRangeInitialize(Locus locus)
{
  locus->search = RefSetEMPTY;
  locus->searchIndex = RefSetSize;
  locus->i = 0;
  locus->j = 0;
  locus->k = 0;
  
  /* Do this once, for the iteration */
  LocusEnsureReady(locus);
  /* These are expensive checks, only do them now, when we are about
     to rely on the information */
  AVERT(ZoneUsage, LocusZoneUsage(locus));
  AVERT(ZoneUsage, LocusManagerZoneUsage(LocusLocusManager(locus)));
}


/* LocusZoneRangeFinished -- Is the zone range iteration done? */
static Bool LocusZoneRangeFinished(Locus locus)
{
  return locus->search == RefSetUNIV && locus->searchIndex == RefSetSize;
}


/* LocusZoneRangeNext --  Return the next zone range in the iteration */
static void LocusZoneRangeNext(Addr *baseReturn, Addr *limitReturn, Locus locus)
{
  LocusManager manager = LocusLocusManager(locus);
  Arena arena = LocusManagerArena(manager);
  Word zoneShift = ArenaZoneShift(arena);
  RefSet ref = locus->search;
  Index i = locus->searchIndex;
  
  AVER(locus->ready);
  /* @@@ rewrite using RefSet_FOR */
  for (;;) {
    if (! (i < RefSetSize)) {
      LocusRefSetNext(locus);
      ref = locus->search;
      i = 0;
    }
    for (; i < RefSetSize; i++) {
      /* @@@ faster to shift ref and quit on 0 */
      if (BS_IS_MEMBER(ref, i)) {
        *baseReturn = (Addr)(i << zoneShift);
        *limitReturn =
          (Addr)(((i + 1) & RefSetMASK) << arena->zoneShift);
        for (; i < RefSetSize; i++) {
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


/* LocusNoteZoneAlloc -- Note that a client is newly using a zone */
static void LocusNoteZoneAlloc(Locus locus, RefSet ref)
{
  RefSet new = ZoneUsageIncrement(LocusZoneUsage(locus), ref);

  /* If there are new zones, update the manager */
  if (new != RefSetEMPTY) {
    LocusManagerNoteZoneAlloc(LocusLocusManager(locus), new);
  }
}


/* LocusNoteZoneFree -- Note that a client is no longer using a zone */
static void LocusNoteZoneFree(Locus locus, RefSet ref)
{
  RefSet deleted = ZoneUsageDecrement(LocusZoneUsage(locus), ref);

  /* If there are deleted zones, update the manager */
  if (deleted != RefSetEMPTY) {
    LocusManagerNoteZoneFree(LocusLocusManager(locus), deleted);
  }
}


/* LocusRefSetNext -- Calculate the next most optimal RefSet for the
   locus to allocate in.  May be called any number of times and each
   time it will yield a larger, but less optimal RefSet.  It is
   fruitless to call it when the search RefSet is already RefSetUNIV
   */
/* The search order attempts to minimize "zone spread" and "zone
   pollution" by preferring zones "owned" by the locus over free ones
   and free ones over "shared".  Within each of those groups,
   preferred zones are tried first, then "neutral" (not disdained)
   zones.  Disdained zones are _always_ the very last resort, which
   should tend to migrate a locus out of its disdained zones as
   quickly as possible.
   */
/* @@@ We could try first using the client's values and then the
   locus's for (presumably) better performance when disparate clients
   share a locus, but I deemed that _too_ hairy for now.
   */
static void LocusRefSetNext(Locus locus)
{
  ZoneUsage locusDesc = LocusZoneUsage(locus);
  ZoneUsage managerDesc = LocusManagerZoneUsage(LocusLocusManager(locus));
  RefSet used = RefSetComp(locusDesc->free);
  RefSet previous = locus->search;
  RefSet bad, base, good;
  RefSet next = RefSetEMPTY;
  Index i = locus->i;
  
  AVER(previous != RefSetUNIV);
  AVER(locus->ready);
  /* failsafe: we must have up-to-date cohort info */
  LocusEnsureReady(locus);
  
  for (; i <= 1; i++) {
    Index j = locus->j;
    
    switch (i) {
      case 0: bad = locus->disdained; break;
      case 1: bad = RefSetEMPTY; break;
      default: NOTREACHED;
    }
    for (; j <= 3; j++) {
      Index k = locus->k;
      
      switch (j) {
        /* zones held exclusively by this locus */
        case 0:
          base = RefSetInter(used, managerDesc->exclusive);
          break;
        /* free zones */
        case 1: base = managerDesc->free; break;
        /* shared zones */
        case 2: base = used; break;
        /* anywhere */
        case 3: base = RefSetUNIV; break;
        default: NOTREACHED;
      }
      for (; k <= 1; k++) {
        switch (k) {
          case 0: good = locus->preferred; break;
          case 1: good = RefSetUNIV; break;
          default: NOTREACHED;
        }
        next = RefSetUnion(previous,
                           RefSetInter(RefSetDiff(base, bad),
                                       good));
        if (RefSetDiff(next, previous) != RefSetEMPTY) {
          locus->search = next;
          locus->i = i;
          locus->j = j;
          locus->k = k;
          return;
        }
      }
    }
  }
  NOTREACHED;
}
  

/* LocusLocusClientDistance -- measure the distance between the cohort
   specification of a locus and a locus client*/
/* @@@ This is only a first cut, weighting all factors equally */
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

  
/* Zone Usage Methods -- Tally zone usage by zone and maintain
   summaries of unused, exclusively used, and multiply used zones.  */


/* ZoneUsageInit -- Initialize a zone usage descriptor */
static void ZoneUsageInit(ZoneUsage desc)
{
  Index zone, next;
  
  desc->free = RefSetUNIV;
  desc->exclusive = RefSetEMPTY;
  desc->shared = RefSetEMPTY;
  RefSet_FOR(RefSetUNIV, zone, next)
    {
      desc->usage[zone] = 0;
    }
  AVERT(ZoneUsage, desc);
}


/* ZoneUsageFinish -- Finish a zone usage descriptor.  Returns the
   descriptor summary (i.e., the zones that are no longer in use as a
   result of the finish). */
static RefSet ZoneUsageFinish(ZoneUsage desc)
{
  AVERT(ZoneUsage, desc);

  return RefSetComp(desc->free);
}
      

/* ZoneUsageIncrement -- Increment the usage counts for all the zones
   in a RefSet.  Returns a summary of any new zones as a RefSet, which
   can be used to propagate the changes to a higher-level summary. */
static RefSet ZoneUsageIncrement(ZoneUsage desc, RefSet ref)
{
  Count *usage = desc->usage;
  RefSet shared = RefSetEMPTY;
  RefSet new = RefSetEMPTY;
  Index zone, next;

  RefSet_FOR(ref, zone, next) 
    {
      Count u = usage[zone];

      AVER(u != (Count)-1);
      switch(u) {
        case 0:
          new = RefSetAddZone(new, zone);
          break;
        case 1:
          shared = RefSetAddZone(shared, zone);
          break;
      }
      usage[zone] = u + 1;
    }
  desc->shared = RefSetUnion(desc->shared, shared);
  desc->exclusive = RefSetUnion(RefSetDiff(desc->exclusive, shared), new);
  desc->free = RefSetDiff(desc->free, new);
  AVERT(ZoneUsage, desc);

  return new;
}


/* ZoneUsageDecrement -- Decrement the usage counts for all the zones
   in a RefSet.  Returns a summary of any deleted zones as a RefSet,
   which can be used to propagate the changes to a higher-level
   summary. */
static RefSet ZoneUsageDecrement(ZoneUsage desc, RefSet ref)
{
  Count *usage = desc->usage;
  RefSet single = RefSetEMPTY;
  RefSet deleted = RefSetEMPTY;
  Index zone, next;

  RefSet_FOR(ref, zone, next) 
    {
      Count u = usage[zone] - 1;

      AVER(u != (Count)-1);
      switch(u) {
        case 0:
          deleted = RefSetAddZone(deleted, zone);
          break;
        case 1:
          single = RefSetAddZone(single, zone);
          break;
      }
      usage[zone] = u;
    }
  desc->shared = RefSetDiff(desc->shared, single);
  desc->exclusive = RefSetUnion(RefSetDiff(desc->exclusive, deleted), single);
  desc->free = RefSetUnion(desc->free, deleted);
  AVERT(ZoneUsage, desc);

  return deleted;
}


/* Utility Routines */

/* HACKMEM #169 from MIT AI Memo 239, Feb. 29, 1972.  In order of
 * one-ups-manship: Gosper, Mann, Lenard, [Root and Mann])
 *
 * To count the ones in a PDP-6/10 word: 
 * 
 * LDB B,[014300,,A]  ;or MOVE B,A then LSH B,-1
 * AND B,[333333,,333333]
 * SUB A,B
 * LSH B,-1
 * AND B,[333333,,333333]
 * SUBB A,B           ;each octal digit replaced by number of 1's in it
 * LSH B,-3
 * ADD A,B
 * AND A,[070707,,070707]
 * IDIVI A,77         ;casting out 63.'s
 * 
 * These ten instructions, with constants extended, would work on word
 * lengths up to 62.; eleven suffice up to 254..
 */
static Count LogCount(Count val) 
{
  Count temp;
  AVER(BS_SIZE(Count) == 32);
  
  temp = (val >> 1) & 033333333333;
  temp = val - temp - ((temp >> 1) & 033333333333);
  return ((Count)(((temp + (temp >> 3)) & 030707070707) % 077));
}


/* Check Methods */


static Bool LocusManagerCheck(LocusManager manager) 
{
  Locus locus;

  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++) {
    CHECKL(LocusCheck(locus));
  }
  /* ZoneUsage check is expensive */
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
    }
  }
  /* ZoneUsage check is expensive */
  return TRUE;
}


static Bool ZoneUsageCheck(ZoneUsage desc)
{
  Index zone, next;
  RefSet free = RefSetEMPTY;
  RefSet exclusive = RefSetEMPTY;
  RefSet shared = RefSetEMPTY;
  Count *usage = desc->usage;
  
  RefSet_FOR(RefSetUNIV, zone, next)
    {
      switch (usage[zone]) {
        case 0:
          free = RefSetAddZone(free, zone);
          break;
        case 1:
          exclusive = RefSetAddZone(exclusive, zone);
          break;
        default:
          shared = RefSetAddZone(shared, zone);
          break;
      }
    }
  CHECKL(RefSetComp(free) == RefSetUnion(exclusive, shared));
  CHECKL(desc->free == free);
  CHECKL(desc->exclusive == exclusive);
  CHECKL(desc->shared == shared);

  return TRUE;
}

  
/* Describe Methods */


Res LocusManagerDescribe(LocusManager manager, mps_lib_FILE *stream)
{
  Res res;
  Locus locus;
  Count numLoci = 0;

  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++) {
    if (locus->inUse) {
      numLoci++;
    }
  }

  res = WriteF(stream,
               "LocusManager $P\n{\n", (WriteFP)manager,
               "  arena: ",
               NULL);
  if (res != ResOK)
    return res;

  res = ArenaName(LocusManagerArena(manager), stream);
  if (res != ResOK)
    return res;
  
  res = WriteF(stream,
               "\n",
               "  active loci: $U\n", (WriteFU)numLoci,
               NULL);
  if (res != ResOK)
    return res;
  
  if (numLoci > 0) {
    /* @@@ indenting-stream(stream, "  ") */
    res = WriteF(stream,
                 "Locus",
                 NULL);
    if (res != ResOK)
      return res;
    
    res = ZoneUsageDescribe(LocusManagerZoneUsage(manager), stream);
    if (res != ResOK)
      return res;

    for (locus = &manager->locus[0];
         locus < &manager->locus[NUMLOCI];
         locus++) {
      if (locus->inUse) {
        /* @@@ indenting-stream(stream, "  ") */
        res = LocusDescribe(locus, stream);
        if (res != ResOK)
          return res;
      }
    }
  }
  
  res = WriteF(stream, "}\n", NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}     


Res LocusDescribe(Locus locus, mps_lib_FILE *stream)
{
  Res res;
  Ring this, next;
  Count numClients = 0;

  RING_FOR(this, LocusClientRing(locus), next)
    {
      numClients++;
    }

  res = WriteF(stream,
               "Locus $P\n{\n", (WriteFP)locus,
               "  manager: $P\n", (WriteFP)locus->manager,
               "  clients: $U\n", (WriteFU)numClients,
               NULL);
  if (res != ResOK)
    return res;
  
  if (locus->inUse) {
    res = WriteF(stream,
                 "  ready: $S\n", (WriteFS)locus->ready?"yes":"no",
                 "  preferred: $B\n", (WriteFB)locus->preferred,
                 "  disdained: $B\n", (WriteFB)locus->disdained,
                 "  lifetime: $U\n", (WriteFU)locus->lifetime,
                 "Client",
                 NULL);
    if (res != ResOK)
      return res;
    /* @@@ indenting-stream(stream, "  ") */
    res = ZoneUsageDescribe(LocusZoneUsage(locus), stream);
    if (res != ResOK)
      return res;
    res = WriteF(stream,
                 "  search: $B\n", (WriteFB)locus->search,
                 "  searchIndex: $U\n", (WriteFU)locus->searchIndex,
                 "  searchState: [$U, $U, $U]\n",
                 locus->i, locus->j, locus->k, 
                 "  clientSerial: $U\n", (WriteFU)locus->clientSerial,
                 NULL);
    if (res != ResOK)
      return res;

    RING_FOR(this, LocusClientRing(locus), next)
      {
        /* @@@ indenting-stream(stream, "  ") */
        LocusClientDescribe(RING_ELT(LocusClient, locusRingStruct,
                                     this),
                            stream);
      }
  }

  res = WriteF(stream, "}\n", NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


Res LocusClientDescribe(LocusClient client, mps_lib_FILE *stream)
{
  Res res;

  res = WriteF(stream,
               "LocusClient $P\n{\n", (WriteFP)client,
               "  client: ",
               NULL);
  if (res != ResOK)
    return res;

  /* @@@ Needs adjusting if non-pool parents */
  res = PoolName(LocusClientPool(client), stream);
  if (res != ResOK)
    return res;

  res = WriteF(stream,
               "\n",  
               "  manager: $P\n", (WriteFP)client->manager,
               "  preferred: $B\n", (WriteFB)client->preferred,
               "  disdained: $B\n", (WriteFB)client->disdained,
               "  lifetime: $U\n", (WriteFU)client->lifetime,
               NULL);
  if (res != ResOK)
    return res;
  
  if (client->assigned) {
    res = WriteF(stream,
                 "  locus: $P\n", (WriteFP)client->locus,
                 "  locusSerial: $U\n", (WriteFU)client->locusSerial,
                 "Segment",
                 NULL);
    if (res != ResOK)
      return res;

    /* @@@ indenting-stream(stream, "  ") */
    res = ZoneUsageDescribe(LocusClientZoneUsage(client), stream);
    if (res != ResOK)
      return res;
  }

  res = WriteF(stream, "}\n", NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


Res ZoneUsageDescribe(ZoneUsage desc, mps_lib_FILE *stream)
{
  Res res;
  Count *usage = desc->usage;
  Index zone, next;

  AVERT(ZoneUsage, desc);
  AVER(stream != NULL);

  res = WriteF(stream,
               "ZoneUsage $P\n{\n", (WriteFP)desc,
               "  free     : $B\n", (WriteFB)desc->free,
               "  shared   : $B\n", (WriteFB)desc->shared,
               "  exclusive: $B\n", (WriteFB)desc->exclusive,
               NULL);
  if (res != ResOK)
    return res;

  RefSet_FOR(RefSetComp(desc->free), zone, next)
    {
      res = WriteF(stream,
                   "  usage[$U]: $U",
                   (WriteFU)zone,
                   (WriteFU)usage[zone],
                   NULL);
      if (res != ResOK)
        return res;
    }

  res = WriteF(stream, "\n}\n", NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


