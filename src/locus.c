/* impl.c.locus: LOCI
 *
 * $HopeName: MMsrc!locus.c(MMdevel_ptw_pseudoloci.16) $
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
 * clients and cohorts in zones.  Three benefits accrue to Arena
 * implementations that heed this advice: 1. the segments of a zone
 * will have more uniform lifetime behavior (which should reduce
 * segment-level fragmentation), 2. the accuracy of inter-cohort
 * refsets will improve (which should optimize garbage-collection
 * scanning), 3. individual clients will tend to acquire adjacent
 * segments
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


/* Private prototypes */

static void LocusManagerZoneRangeInitialize(LocusManager manager,
                                            Locus locus,
                                            LocusClient client,
                                            Index startZone,
                                            Bool searchUp);
static Bool LocusManagerZoneRangeFinished(LocusManager manager,
                                          Locus locus,
                                          LocusClient client);
static void LocusManagerZoneRangeNext(Addr *baseReturn,
                                      Addr *limitReturn,
                                      LocusManager manager,
                                      Locus locus,
                                      LocusClient client);
static void  LocusManagerRefSetCalculate(LocusManager manager,
                                         Bool describePolicy,
                                         mps_lib_FILE *stream);
static void LocusManagerNoteZoneAlloc(LocusManager manager,
                                      RefSet ref);
static void LocusManagerNoteZoneFree(LocusManager manager,
                                     RefSet ref);
static void LocusInit(Locus locus, LocusManager manager);
static void LocusFinish(Locus locus);
static void LocusActivate(Locus locus);
static void LocusDeactivate(Locus locus);
static void LocusEnsureReady(Locus locus);
static void LocusClientEnsureLocus(LocusClient client);
static void LocusLocusClientDelete(Locus locus, LocusClient client);
static void LocusNoteZoneAlloc(Locus locus, RefSet ref);
static void LocusNoteZoneFree(Locus locus, RefSet ref);
static Count LocusLocusClientDistance(Locus locus,
                                      LocusClient client);
static void ZoneUsageInit(ZoneUsage desc);
static RefSet ZoneUsageFinish(ZoneUsage desc);
static RefSet ZoneUsageIncrement(ZoneUsage desc, RefSet ref);
static RefSet ZoneUsageDecrement(ZoneUsage desc, RefSet ref);
static Count LogCount(Count val);
static Res LocusClientName(LocusClient client, mps_lib_FILE *stream);
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

static LocusManager LocusClientLocusManager(LocusClient client)
{
  return client->manager;
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

static RefSet ZoneUsageUsed(ZoneUsage desc)
{
  return RefSetComp(desc->free);
}

static RefSet ZoneUsageFree(ZoneUsage desc)
{
  return desc->free;
}

static RefSet ZoneUsageExclusive(ZoneUsage desc)
{
  return desc->exclusive;
}

#if 0
static RefSet ZoneUsageShared(ZoneUsage desc)
{
  return desc->shared;
}

static Count* ZoneUsageUsage(ZoneUsage desc)
{
  return desc->usage;
}
#endif

/* External Methods */


/* Locus Manager Methods */


/* LocusManagerInit -- Initialize the locus manager */
void LocusManagerInit(LocusManager manager) 
{
  Locus locus;
  
  manager->searchCacheValid = FALSE;
  manager->searchCacheIndex = 0;
  manager->searchCacheLimit = 0;
  manager->searchUp = TRUE;
  manager->searchStartZone = 0;
  manager->searchCurrentZone = 0;
  manager->searchCount = 0;
  manager->searchRefSet = RefSetEMPTY;
  manager->searchUseFree = TRUE;
  manager->searchExpand = TRUE;
  ZoneUsageInit(LocusManagerZoneUsage(manager));
  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++)
    LocusInit(locus, manager);
  manager->activeLoci = 0;
  manager->sig = LocusManagerSig;
  
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
  AVER(manager->activeLoci == 0);
  manager->sig = SigInvalid;
}


/* Locus Client Methods */


/* Accessors */

Arena (LocusClientArena)(LocusClient client)
{
  return client->arena;
}

Pool (LocusClientPool)(LocusClient client) 
{
  return client->pool;
}

Cohort (LocusClientCohort)(LocusClient client)
{
  return client->cohort;
}

Serial (LocusClientSerial)(LocusClient client)
{
  return client->locusSerial;
}

Ring (LocusClientSegRing)(LocusClient client)
{
  return &client->segRingStruct;
}


/* LocusClientInit -- Initialize a locus client and assign it to a
   locus manager */
void LocusClientInit(LocusClient client, Arena arena,
                     Pool pool, Cohort cohort,
                     LocusClientNameMethod clientNameMethod)
{
  client->manager = ArenaLocusManager(arena);
  client->assigned = FALSE;
  client->locus = NULL;
  client->arena = arena;
  client->pool = pool;
  client->cohort = cohort;
  /* default: everything is good, nothing is bad */
  client->preferred = RefSetUNIV;
  client->disdained = RefSetEMPTY;
  /* default: no lifetime */
  client->lifetime = LifetimeNONE;
  RingInit(LocusClientSegRing(client));
  /* diagnostic -- clients don't need to know their parent */
  client->clientNameMethod = clientNameMethod;
  ZoneUsageInit(LocusClientZoneUsage(client));
  RingInit(LocusClientLocusRing(client));
  client->locusSerial = 0;
  client->sig = LocusClientSig;
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
  client->sig = SigInvalid;
}

/* LocusClientSetCohortParameters -- Set the cohort parameters for
   this client.  The client passes in any a priori zone preferences it
   knows and a representation of its mean object lifetime.  @@@
   eventually the client will pass in more cohort parameters such as
   allocation pattern, frequency, phase, etc. (perhaps collectable,
   scannable, rank, etc.?) */
void LocusClientSetCohortParameters(LocusClient client,
                                    RefSet preferred,
                                    RefSet disdained,
                                    /* @@@ cohort parameters */
                                    Lifetime lifetime)
{
  AVERT(LocusClient, client);
  AVERT(RefSet, preferred);
  AVERT(RefSet, disdained);
  AVERT(Lifetime, lifetime);
  AVER(RefSetInter(preferred, disdained) == RefSetEMPTY);
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
void LocusClientZoneRangeInitialize(LocusClient client,
                                    Index startZone, Bool searchUp)
{
  AVERT(LocusClient, client);
  AVER(startZone == (startZone & RefSetMASK));
  startZone &= RefSetMASK;

  LocusClientEnsureLocus(client);

  LocusManagerZoneRangeInitialize(client->manager, client->locus,
                                  client, startZone, searchUp);
}


/* LocusClientZoneRangeFinished -- Is the zone range iteration complete? */
Bool LocusClientZoneRangeFinished(LocusClient client)
{
  AVERT(LocusClient, client);
  AVER(client->assigned);
  
  return LocusManagerZoneRangeFinished(client->manager, client->locus,
                                       client);

}


/* LocusClientZoneRangeNext -- Return the next zone range in the
   iteration sequence */
void LocusClientZoneRangeNext(Addr *baseReturn,
                              Addr *limitReturn,
                              LocusClient client)
{
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(LocusClient, client);
  AVER(client->assigned);
  
  LocusManagerZoneRangeNext(baseReturn, limitReturn, client->manager,
                            client->locus, client);
}


/* LocusClientSeg methods -- used by the LocusManager Arena to apprise
   the LocusManager of segment allocations and frees.
   */

/* LocusClientSegInit -- Inform the LocusManager that a segment
   has been allocated to a LocusClient  */
void LocusClientSegInit(LocusClient client, Seg seg)
{
  AVERT(LocusClient, client);
  AVERT(Seg, seg);
  AVER(client->assigned);
  AVER(client == SegClient(seg));
  
  /* add the segment to the client ring */
  AVER(RingIsSingle(SegClientRing(seg)));
  RingAppend(LocusClientSegRing(client), SegClientRing(seg));
}


/* LocusClientSegValid -- Inform the LocusManager that a segment is
   valid, in particular, SegBase and SegLimit are valid, so the
   segment's RefSet can be accumulated */
void LocusClientSegValid(LocusClient client, Seg seg)
{
  RefSet segRefSet;
  RefSet new;

  AVERT(LocusClient, client);
  AVERT(Seg, seg);
  AVER(client->assigned);
  AVER(client == SegClient(seg));
  
  segRefSet = RefSetOfSeg(LocusClientArena(client), seg);
  /* Should have chosen a seg in the currently being searched RefSet
     */
  AVER(RefSetSuper(LocusClientLocusManager(client)->searchRefSet, segRefSet));
  
  new = ZoneUsageIncrement(LocusClientZoneUsage(client), segRefSet);

  /* If there are new zones, update the locus */
  if (new != RefSetEMPTY) {
    /* Zone change invalidates search cache */
    LocusClientLocusManager(client)->searchCacheValid = FALSE;
    LocusNoteZoneAlloc(LocusClientLocus(client), new);
  }
}


/* LocusClientSegFinish -- Inform the LocusManager that a LocusClient
   is no longer using a segment */
void LocusClientSegFinish(LocusClient client, Seg seg)
{
  RefSet segRefSet;
  RefSet deleted;

  AVERT(LocusClient, client);
  AVERT(Seg, seg);
  AVER(client->assigned);
  AVER(client == SegClient(seg));
  
  segRefSet = RefSetOfSeg(LocusClientArena(client), seg);
  deleted = ZoneUsageDecrement(LocusClientZoneUsage(client), segRefSet);

  /* If there are deleted zones, update the locus */
  if (deleted != RefSetEMPTY) {
    /* Zone change invalidates search cache */
    LocusClientLocusManager(client)->searchCacheValid = FALSE;
    LocusNoteZoneFree(LocusClientLocus(client), deleted);
  }
  /* remove the segment from the client ring */
  RingRemove(SegClientRing(seg));
}


/* Internal Methods */


/* Locus Manager Methods */


/* LocusManagerZoneRangeInitialize -- Initialize the locus zone range
   iteration */
static void LocusManagerZoneRangeInitialize(LocusManager manager,
                                            Locus locus,
                                            LocusClient client,
                                            Index startZone,
                                            Bool searchUp) 
{
  if (manager->searchCacheValid &&
      manager->searchClient == client &&
      manager->searchLocus == locus) {
    /* cached search */
  } else {
    /* new client, or cache invalid: fill the cache */
    manager->searchClient = client;
    manager->searchLocus = locus;

    LocusEnsureReady(locus);
    /* These are expensive checks, only do them now, when we are about
     to rely on the information */
    AVERT(ZoneUsage, LocusClientZoneUsage(client));
    AVERT(ZoneUsage, LocusZoneUsage(locus));
    AVERT(ZoneUsage, LocusManagerZoneUsage(LocusLocusManager(locus)));

    /* @@@ policy knobs, not currently turned */
    manager->searchUseFree = TRUE;
    manager->searchExpand = TRUE;
    LocusManagerRefSetCalculate(manager, FALSE, NULL);
    manager->searchCacheValid = TRUE;
  }
  
  manager->searchUp = searchUp;
  manager->searchStartZone = startZone;
  manager->searchCount = 0;
  manager->searchCurrentZone = startZone;
  manager->searchCacheIndex = 0;
  manager->searchRefSet = RefSetEMPTY;
  return;
}


/* LocusManagerZoneRangeFinished -- Is the zone range iteration done?
   */
static Bool LocusManagerZoneRangeFinished(LocusManager manager,
                                          Locus locus,
                                          LocusClient client)
{
  AVER(manager->searchClient == client);

  return (manager->searchCount == 0 &&
          manager->searchCacheIndex == manager->searchCacheLimit);
}


extern RefSet RefSetOfRange(Space space, Addr rangeBase, Addr
                            rangeLimit);

RefSet RefSetOfRange(Space space, Addr rangeBase, Addr rangeLimit)
{
  Word base, limit;

  AVERT(Space, space);

  /* .rsos.zones */
  base = (Word)rangeBase >> space->zoneShift;
  limit = (((Word)rangeLimit-1) >> space->zoneShift) + 1;

  if(limit - base >= MPS_WORD_WIDTH)        /* .rsos.univ */
    return RefSetUNIV;

  base  &= MPS_WORD_WIDTH - 1;
  limit &= MPS_WORD_WIDTH - 1;

  if(base < limit)                      /* .rsos.swap */
    return ((RefSet)1<<limit) - ((RefSet)1<<base);
  else
    return ~(((RefSet)1<<base) - ((RefSet)1<<limit));
}


#if 0
static Word Rotate(Word word, int rotation);
#endif

/* A decent compiler should be able to compile this to a single
   instruction */
#define ROTATE(word, rotation)                                  \
  (((word) << ((rotation) & (MPS_WORD_WIDTH - 1))) |            \
    ((word) >> (( - (rotation)) & (MPS_WORD_WIDTH - 1))))


#if 0
static Word(Rotate)(Word word, int rotation) 
{
  return ROTATE(word, rotation);
}

static void (RefSetFind)(Index *zoneReturn,
                         RefSet refSet, Index next,
                         Bool up, Bool set, Bool first);
#endif


/* RefSetFind -- Find a zone in refSet, searching up or down from next
   according to up, searching for set or reset according to set,
   searching for first or last (consecutive) according to first. */
static Count (RefSetFind)(Index *zoneReturn,
                          RefSet refSet, Index next, Count count,
                          Bool up, Bool set, Bool first)
{
  Count rsfCount = count + (first ? 0 : 1);

  refSet = ROTATE(refSet, (- next));                                                                     
                                                                                                                
#define RefSetFindLoop(up, set, first)                                          \
  {                                                                             \
    /* The intent of this macro is that a decent compiler can optimize          \
       out all the (presumably constant) ?:'s and reduce this loop to a         \
       very small number of instructions. */                                    \
    for (; rsfCount != 0; refSet = ROTATE(refSet, (up)?-1:1), rsfCount--) {   \
      if (((!(set)) == (!(first)))?(refSet & 01):(! (refSet & 01))) {         \
        break;                                                                  \
      }                                                                         \
    }                                                                           \
  }
  
  /* Branch to the optimized loop */
  if ((!(set)) == (!(first))) {
      if (up) {
        RefSetFindLoop(TRUE, TRUE, TRUE);
      } else {
        RefSetFindLoop(FALSE, TRUE, TRUE);
      }
  } else {
      if (up) {
        RefSetFindLoop(TRUE, TRUE, FALSE);
      } else {
        RefSetFindLoop(FALSE, TRUE, FALSE);
      }
  }

#undef RefSetFindLoop
  
    *(zoneReturn) = (next + ((up)?
                             (count - rsfCount) :
                             (rsfCount - count))) & RefSetMASK;
  return rsfCount;
}

/* LocusManagerZoneRangeNext -- Return the next zone range in the
   iteration */
static void LocusManagerZoneRangeNext(Addr *baseReturn,
                                      Addr *limitReturn, 
                                      LocusManager manager,
                                      Locus locus,
                                      LocusClient client)
{
  RefSet current = manager->searchRefSet;
  Index zone = manager->searchCurrentZone;
  Count count = manager->searchCount;
  
  AVER(locus->ready);
  AVER(manager->searchCacheValid);
  AVER(manager->searchClient == client);
  AVER(manager->searchLocus == locus);

  for (;;) {
    Index start = zone;
    Index end = zone;

    if (count == 0) {
      count = RefSetSize;
      zone = manager->searchStartZone;
      current = manager->searchCache[manager->searchCacheIndex];
      manager->searchCacheIndex++;
    }

    AVER(current != RefSetEMPTY);

    if (RefSetIsMemberZone(current, zone)) {
      AVER(count == RefSetSize);
      /* We are in the middle of a range, find the ends */
      /* Start is from zone the down, set, last */
      (RefSetFind)(&start, current, zone, count, FALSE, TRUE, FALSE);
      /* End is from zone the up, reset, first */
      (RefSetFind)(&end, current, zone, count, TRUE, FALSE, TRUE);
      /* normalize */
      if (end == start) end += RefSetSize;
      count -= (end - start);
      AVER(start <= zone);
      AVER(zone < end);
    } else if (manager->searchUp) {
      /* Find the next range above */
      /* Start is from zone the up, set, first */
      count = (RefSetFind)(&start, current, zone, count, TRUE, TRUE, TRUE);
      /* End is from start the up, reset, first */
      count = (RefSetFind)(&end, current, start, count, TRUE, FALSE, TRUE);
      /* normalize */
      if (end < start) end += RefSetSize;
      count -= (end - zone);
    } else {
      /* Find the next range below */
      /* End is from zone the down, reset, last */
      count = (RefSetFind)(&end, current, zone, count, FALSE, FALSE, FALSE);
      /* Start is from end-1 the down, set, last */
      count = (RefSetFind)(&start, current, end - 1, count - 1, FALSE, TRUE, FALSE);
      /* normalize */
      if (end < start) start -= RefSetSize;
    }
    
    if (start != end) {
      Arena arena = LocusManagerArena(manager);
      Word zoneShift = ArenaZoneShift(arena);

      /* save your place */
      if (manager->searchUp) {
        manager->searchCurrentZone = end;
        manager->searchCount = count;
      } else {
        manager->searchCurrentZone = start - 1;
        manager->searchCount = count - 1;
      }
      manager->searchRefSet = current;

      /* ZoneBaseAddr */
      *baseReturn = (Addr)(start << zoneShift);
      *limitReturn = (Addr)(end << zoneShift);

      return;
    }
  }
}


/* LocusManagerRefSetCalculate -- Calculate the optimal RefSet for a
   client to allocate in.  Calculates up to RefSetSize RefSet's, each
   one broader than the previous, from the most to least desirable.

   This function embodies the central policy of the locus mechanism:
   
   The search order attempts to minimize "zone spread" and "zone
   pollution" by preferring zones owned by the client, shared by the
   client within the locus, zones owned by the locus, free (if
   permitted, by useFree), shared by the client, shared by the locus,
   and used by other loci (if permitted, by expand).  Within each of
   those groups, preferred zones are tried first, then "neutral" (not
   disdained) zones.  Disdained zones are _always_ the very last
   resort, which should tend to migrate a locus out of its disdained
   zones as quickly as possible.
   */
static void LocusManagerRefSetCalculate(LocusManager manager,
                                        Bool describePolicy,
                                        mps_lib_FILE *stream) 
{
  LocusClient client = manager->searchClient;
  Locus locus = manager->searchLocus;
  ZoneUsage clientDesc = LocusClientZoneUsage(client);
  ZoneUsage locusDesc = LocusZoneUsage(locus);
  ZoneUsage managerDesc = LocusManagerZoneUsage(LocusLocusManager(locus));
  RefSet previous = RefSetEMPTY;
  char *badDesc = "", *baseDesc = "", *goodDesc = "";
  RefSet bad = RefSetEMPTY, base = RefSetEMPTY, good = RefSetEMPTY;
  RefSet next = RefSetEMPTY;
  Index r = 0;
  Index i = 0;
  
  AVER(locus->ready);
  /* failsafe: we must have up-to-date cohort info */
  LocusEnsureReady(locus);
  
  for (; i <= 2; i++) {
    Index j = 0;
    
    switch (i) {
      case 0:
        /* bad for the locus */
        badDesc = ", not disdained by the locus";
        bad = locus->disdained;
        break;
      case 1:
        /* bad for the client */
        badDesc = ", not disdained by the client";
        bad = client->disdained;
        break;
      case 2:
        /* last resort */
        badDesc = "";
        bad = RefSetEMPTY;
        break;
      default:
        NOTREACHED;
    }
    for (; j <= 7; j++) {
      Index k = 0;
      
      switch (j) {
        case 0:
          /* zones used by this client, exclusive to this client */
          baseDesc = "used by the client, exclusive to the client";
          base = RefSetInter(ZoneUsageUsed(clientDesc), ZoneUsageExclusive(locusDesc));
          break;
        case 1:
          /* zones used by this client, exclusive to this locus */
          baseDesc = "used by the client, exclusive to the locus";
          base = RefSetInter(ZoneUsageUsed(clientDesc), ZoneUsageExclusive(managerDesc));
          break;
        case 2:
          /* zones used by this locus, exclusive to this locus */
          baseDesc = "used by the locus, exclusive to the locus";
          base = RefSetInter(ZoneUsageUsed(locusDesc), ZoneUsageExclusive(managerDesc));
          break;
        case 3:
          /* free zones */
          if (manager->searchUseFree) {
            baseDesc = "unused by any client";
            base = ZoneUsageFree(managerDesc);
            break;
          }
        case 4:
          /* zones used by this client, shared with other loci */
          baseDesc = "used by the client, shared with other loci";
          base = ZoneUsageUsed(clientDesc);
          break;
        case 5:
          /* zones used by this locus, shared with other loci */
          baseDesc = "used by the locus, shared with other loci";
          base = ZoneUsageUsed(locusDesc);
          break;
        case 6:
          /* zones used by other loci, exclusive to those loci (@@@
             spread the pain?) */
          if (manager->searchExpand) {
            baseDesc = "used by other loci, exclusive to those loci";
            base = ZoneUsageExclusive(managerDesc);
            break;
          }
        case 7:
          /* last resort */
          if (manager->searchExpand) {
            baseDesc = "anywhere";
            base = RefSetUNIV;
            break;
          }
        default:
          /* can be reached if ! manager->searchExpand */
          base = RefSetEMPTY;
          break;
      }
      for (; k <= 2; k++) {
        switch (k) {
          case 0:
            /* good for the client */
            goodDesc = ", preferred by the client";
            good = client->preferred;
            break;
          case 1:
            /* good for the locus */
            goodDesc = ", preferred by the locus";
            good = locus->preferred;
            break;
          case 2:
            /* last resort */
            goodDesc = "";
            good = RefSetUNIV;
            break;
          default:
            NOTREACHED;
        }
        if (describePolicy) {
          WriteF(stream, baseDesc, badDesc, goodDesc, "\n", NULL);
        }
        next = RefSetUnion(previous,
                           RefSetInter(RefSetDiff(base, bad),
                                       good));
        if (RefSetDiff(next, previous) != RefSetEMPTY) {
          AVER(r < RefSetSize);
          manager->searchCache[r] = next;
          r++;
          previous = next;
        }
      }
    }
  }
  manager->searchCacheLimit = r;
}
  

/* LocusManagerNoteZoneAlloc -- Note that a locus is newly using a
   zone */
static void LocusManagerNoteZoneAlloc(LocusManager manager,
                                      RefSet ref)
{
  (void) ZoneUsageIncrement(LocusManagerZoneUsage(manager), ref);
}


/* LocusManagerNoteZoneFree -- Note that a locus is no longer using a
   zone */
static void LocusManagerNoteZoneFree(LocusManager manager,
                                     RefSet ref)
{
  (void) ZoneUsageDecrement(LocusManagerZoneUsage(manager), ref);
}


/* Locus Methods */


static void LocusInit(Locus locus, LocusManager manager)
{
  locus->inUse = FALSE;
  locus->ready = FALSE;
  locus->lifetime = LifetimeNONE;
  locus->preferred = RefSetEMPTY;
  locus->disdained = RefSetEMPTY;
  RingInit(LocusClientRing(locus));
  locus->clientSerial = (Serial)1;
  ZoneUsageInit(LocusZoneUsage(locus));
  locus->manager = manager;
  locus->clients = 0;
  locus->sig = LocusSig;
}

static void LocusFinish(Locus locus)
{
  LocusDeactivate(locus);
  locus->sig = SigInvalid;
}

static void LocusActivate(Locus locus)
{
  locus->inUse = TRUE;
  locus->ready = FALSE;
  locus->lifetime = LifetimeNONE;
  locus->preferred = RefSetEMPTY;
  locus->disdained = RefSetEMPTY;
  ZoneUsageInit(LocusZoneUsage(locus));
  locus->clients = 0;
  LocusLocusManager(locus)->activeLoci++;
}

static void LocusDeactivate(Locus locus)
{
  RefSet deleted;
  
  AVER(RingIsSingle(LocusClientRing(locus)));

  locus->inUse = FALSE;
  locus->ready = FALSE;
  locus->lifetime = LifetimeNONE;
  locus->preferred = RefSetEMPTY;
  locus->disdained = RefSetEMPTY;

  deleted = ZoneUsageFinish(LocusZoneUsage(locus));
  /* note change */
  LocusManagerNoteZoneFree(LocusLocusManager(locus), deleted);
  LocusLocusManager(locus)->activeLoci--;
  AVER(locus->clients == 0);
  locus->clients = 0;
}


  

/* LocusEnsureReady -- Validate the cached cohort attributes of the
   locus's clients */
static void LocusEnsureReady(Locus locus)
{
  if (locus->inUse && (! locus->ready)) {
    Ring this, next;
    RefSet preferred, disdained, used;
    Lifetime lifetime = LifetimeNONE;
    Count clients = 0;
  
    preferred = RefSetUNIV;
    disdained = RefSetEMPTY;
    used = RefSetEMPTY;

    RING_FOR(this, LocusClientRing(locus), next) 
      {
        LocusClient client = RING_ELT(LocusClient, locusRingStruct,
                                      this);
        preferred = RefSetUnion(preferred, client->preferred);
        disdained = RefSetUnion(disdained, client->disdained);
        clients++;
        lifetime = lifetime / clients * (clients - 1) +
          client->lifetime / clients;
        used = RefSetUnion(used, RefSetComp(LocusClientZoneUsage(client)->free));
      }

    AVER(used == RefSetComp(LocusZoneUsage(locus)->free));
    AVER(locus->clients == clients);
    locus->lifetime = lifetime;
    locus->preferred = preferred;    
    locus->disdained = disdained;
    locus->ready = TRUE;
  }
}


/* LocusClientEnsureLocus -- Called to assign a client to a locus,
   based on the previously set parameters.  @@@ This is not strictly a
   locus method, but it is the logical inverse of
   LocusLocusClientDelete (v. i.)
   
   @@@ Clients are assigned first come, first served to the "best fit"
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
    }

    AVER(best != NULL);
    if (!best->inUse) {
      LocusActivate(best);
    }
    /* update locus summaries */
    best->clients++;
    best->lifetime = best->lifetime / best->clients *
      (best->clients - 1) +
      client->lifetime / best->clients;
    best->preferred = RefSetUnion(best->preferred, client->preferred);
    best->disdained = RefSetUnion(best->disdained, client->disdained);
    /* add to locus */
    client->locus = best;
    client->locusSerial = best->clientSerial;
    best->clientSerial++;
    RingAppend(LocusClientRing(best),
               LocusClientLocusRing(client));
    client->assigned = TRUE;
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
  locus->clients--;
  
  /* note change in cohort constituents */
  locus->ready = FALSE;
    
  /* Decommission locus if it has no clients */
  if (RingIsSingle(LocusClientRing(locus)))
    LocusFinish(locus);
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


/* LocusLocusClientDistance -- measure the distance between the cohort
   specification of a locus and a locus client

   @@@ This is only a first cut, weighting all factors equally */
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
  desc->sig = ZoneUsageSig;
  AVERT(ZoneUsage, desc);
}


/* ZoneUsageFinish -- Finish a zone usage descriptor.  Returns the
   descriptor summary (i.e., the zones that are no longer in use as a
   result of the finish). */
static RefSet ZoneUsageFinish(ZoneUsage desc)
{
  AVERT(ZoneUsage, desc);

  desc->sig = SigInvalid;
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


Bool LocusManagerCheck(LocusManager manager) 
{
  Locus locus;
  Count activeLoci = 0;

  CHECKS(LocusManager, manager);
  CHECKL(BoolCheck(manager->searchCacheValid));
  if (manager->searchCacheValid) {
    CHECKD(LocusClient, manager->searchClient);
    CHECKD(Locus, manager->searchLocus);
    CHECKL(manager->searchCacheIndex <= manager->searchCacheLimit);
  }
  CHECKL(BoolCheck(manager->searchUseFree));
  CHECKL(BoolCheck(manager->searchExpand));
  for (locus = &manager->locus[0];
       locus < &manager->locus[NUMLOCI];
       locus++) {
    CHECKD(Locus, locus);
    if (locus->inUse)
      activeLoci++;
  }
  CHECKD(ZoneUsage, LocusManagerZoneUsage(manager));
  CHECKL(manager->activeLoci == activeLoci);
  return TRUE;
}


Bool LocusClientCheck(LocusClient client)
{
  CHECKS(LocusClient, client);
  CHECKU(LocusManager, client->manager);
  CHECKL(BoolCheck(client->assigned));
  if (client->assigned) {
    CHECKU(Locus, client->locus);
  } else {
    CHECKL(RingCheckSingle(LocusClientLocusRing(client)));
  }
  CHECKL(RefSetCheck(client->preferred));
  CHECKL(RefSetCheck(client->disdained));
  CHECKL(RefSetCheck(client->used));
  CHECKD(ZoneUsage, LocusClientZoneUsage(client));
  CHECKL(RingCheck(LocusClientLocusRing(client)));
  return TRUE;
}
 
  
Bool LocusCheck(Locus locus)
{
  CHECKS(Locus, locus);
  CHECKU(LocusManager, locus->manager);
  CHECKL(BoolCheck(locus->inUse));
  CHECKL(BoolCheck(locus->ready));
  if (locus->ready) {
    Ring this, next;
    Lifetime lifetime = LifetimeNONE;
    Count clients = 0;
    
    CHECKL(locus->inUse);
    CHECKL(RingCheck(LocusClientRing(locus)));
    RING_FOR(this, LocusClientRing(locus), next) 
    {
      LocusClient client = RING_ELT(LocusClient, locusRingStruct,
                                    this);
      CHECKD(LocusClient, client);
      CHECKL(RefSetSuper(locus->preferred, client->preferred));
      CHECKL(RefSetSuper(locus->disdained, client->disdained));
      clients++;
      lifetime = lifetime / clients * (clients - 1) +
        client->lifetime / clients;
    }
    CHECKL(locus->lifetime == lifetime);
    CHECKL(locus->clients == clients);
  }
  if (! locus->inUse) {
    CHECKL(RingCheckSingle(LocusClientRing(locus)));
  }
  CHECKD(ZoneUsage, LocusZoneUsage(locus));
  return TRUE;
}


static Bool ZoneUsageCheck(ZoneUsage desc)
{
  Index zone, next;
  RefSet free = RefSetEMPTY;
  RefSet exclusive = RefSetEMPTY;
  RefSet shared = RefSetEMPTY;
  Count *usage = desc->usage;
  
  CHECKS(ZoneUsage, desc);
  CHECKL(RefSetCheck(desc->free));
  CHECKL(RefSetCheck(desc->exclusive));
  CHECKL(RefSetCheck(desc->shared));
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
  CHECKL(RefSetInter(exclusive, shared) == RefSetEMPTY);
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
               "  active loci: $U\n", (WriteFU)manager->activeLoci,
               NULL);
  if (res != ResOK)
    return res;
  
  if (manager->activeLoci > 0) {
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
  
  if (manager->searchCacheValid) {
    Index i;

#if 0
    /* Describe the policy */
    LocusManagerRefSetCalculate(manager, TRUE, stream);
#endif

    res = WriteF(stream,
                 "  searchClient: ",
                 NULL);
    if (res != ResOK)
      return res;
    res = LocusClientName(manager->searchClient, stream);
    if (res != ResOK)
      return res;
    res = WriteF(stream,
                 "\n",
                 "  searchCacheIndex: $U\n", (WriteFU)manager->searchCacheIndex,
                 "  searchCacheLimit: $U\n", (WriteFU)manager->searchCacheLimit,
                 "  searchCache: \n",
                 NULL);
    if (res != ResOK)
      return res;

    for (i = 0; i < manager->searchCacheLimit; i++) {
      res = WriteF(stream,
                   "  search[$U]: $B\n", (WriteFU)i,
                   (WriteFU)manager->searchCache[i],
                   NULL);
      if (res != ResOK)
        return res;
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

  res = WriteF(stream,
               "Locus $P\n{\n", (WriteFP)locus,
               "  manager: $P\n", (WriteFP)locus->manager,
               "  clients: $U\n", (WriteFU)locus->clients,
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

static Res LocusClientName(LocusClient client, mps_lib_FILE *stream)
{
  return WriteF(stream,
                "<LocusClient $P ($U)>",
               (WriteFP)client, (WriteFU)LocusClientSerial(client),
               NULL);
}
  

Res LocusClientDescribe(LocusClient client, mps_lib_FILE *stream)
{
  Res res;

  res = WriteF(stream,
               "LocusClient $P ($U)\n{\n",
               (WriteFP)client, (WriteFU)client->locusSerial,
               "  client: ",
               NULL);
  if (res != ResOK)
    return res;

  /* describe parent */
  res = (*client->clientNameMethod)(client, stream);
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


