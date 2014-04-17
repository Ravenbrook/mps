/* zei.c -- Zone And Era Interval approximations
 *
 * $Id$
 * Copyright 2014 Ravenbrook Limited.  See end of file for license.
 * 
 * Approximations are covers of sets of references.  There is no
 * way of adding a single reference to an approximation, instead a cover (a
 * superset) * which contains the ref is added.  Approximations are used for
 * optimizations which conservatively eliminate some references or sets of
 * references by testing for intersection.
 */

SRCID(zei, "$Id$");

#include "mpm.h"

void ZoneApproxInitEmpty(ZoneApprox za)
{
  za->zones = ZoneSetEMPTY;
}

void ZoneApproxInitFull(ZoneApprox za)
{
  za->zones = ZoneSetUNIV;
}

void ZoneApproxCopy(ZoneApprox dest, ZoneApprox src)
{
  dest->zones = src->zones;
}

void ZoneApproxGrow(ZoneApprox za, ZoneApprox extra)
{
  za->zones = ZoneSetUnion(za->zones, extra->zones);
}

void ZoneApproxBound(ZoneApprox za, ZoneApprox bound)
{
  za->zones = ZoneSetInter(za->zones, bound->zones);
}

Bool ZoneApproxMeets(ZoneApprox a, ZoneApprox b)
{
  return ZoneSetInter(a->zones, b->zones) != ZoneSetEMPTY;
}

Bool ZoneApproxContains(ZoneApprox a, ZoneApprox b)
{
  return ZoneSetSuper(a->zones, b->zones);
}

Bool ZoneApproxIsEmpty(ZoneApprox za)
{
  return za == ZoneSetEMPTY;
}

Bool ZoneApproxIsFull(ZoneApprox za)
{
  return za->zones == ZoneSetUNIV;
}

void EraFirstInitEmpty(EraFirst ef)
{
  ef->era = EraINFINITY;
}

void EraFirstInitFull(EraFirst ef)
{
  ef->era = EraZERO;
}

void EraFirstCopy(EraFirst dest, EraFirst src)
{
  dest->era = src->era;
}

void EraFirstGrow(EraFirst ef, EraFirst extra)
{
  if (extra->era < ef->era)
    ef->era = extra->era;
}

void EraFirstBound(EraFirst ef, EraFirst bound)
{
  if (bound->era > ef->era)
    ef->era = bound->era;
}

Bool EraFirstMeets(EraFirst a, EraFirst b)
{
  return a->era < EraINFINITY && b->era < EraINFINITY;
}

Bool EraFirstContains(EraFirst a, EraFirst b)
{
  return a->era <= b->era;
}

Bool EraFirstIsEmpty(EraFirst ef)
{
  return ef->era == EraINFINITY;
}

Bool EraFirstIsFull(EraFirst ef)
{
  return ef->era == EraZERO;
}

void EraLastInitEmpty(EraLast el)
{
  el->era = EraZERO;
}

void EraLastInitFull(EraLast el)
{
  el->era = EraINFINITY;
}

void EraLastCopy(EraLast dest, EraLast src)
{
  dest->era = src->era;
}

void EraLastGrow(EraLast el, EraLast extra)
{
  if (extra->era > el->era)
    el->era = extra->era;
}

void EraLastBound(EraLast el, EraLast bound)
{
  if (bound->era < el->era)
    el->era = bound->era;
}

Bool EraLastMeets(EraLast a, EraLast b)
{
  /* EraZERO must not actually be used for an era, hence EraMIN==1 */
  return a->era > EraZERO && b->era > EraZERO;
}

Bool EraLastContains(EraLast a, EraLast b)
{
  return a->era >= b->era;
}

Bool EraLastIsEmpty(EraLast el)
{
  return el->era == EraZERO;
}

Bool EraLastIsFull(EraLast el)
{
  return el->era == EraINFINITY;
}

static void eraIntervalInitEra2(EraInterval ei, Era min, Era max) {
  ei->min.era = min;
  ei->max.era = max;
}

void EraIntervalInitEmpty(EraInterval ei)
{
  EraFirstInitEmpty(&ei->min);
  EraLastInitEmpty(&ei->max);
}

void EraIntervalInitFull(EraInterval ei)
{
  EraFirstInitFull(&ei->min);
  EraLastInitFull(&ei->max);
}

void EraIntervalCopy(EraInterval dest, EraInterval src)
{
  EraFirstCopy(&dest->min, &src->min);
  EraLastCopy(&dest->max, &src->max);
}

void EraIntervalGrow(EraInterval ei, EraInterval extra)
{
  EraFirstGrow(&ei->min, &extra->min);
  EraLastGrow(&ei->max, &extra->max);
}

void EraIntervalBound(EraInterval ei, EraInterval bound)
{
  EraFirstBound(&ei->min, &bound->min);
  EraLastBound(&ei->max, &bound->max);
  /* FIXME is this valid
  if (ei->min.era > ei->max.era)
    EraIntervalInitEmpty(ei);
    */
}

Bool EraIntervalMeets(EraInterval a, EraInterval b)
{
  return a->min.era <= b->max.era && b->min.era <= a->max.era;
}

Bool EraIntervalContains(EraInterval a, EraInterval b)
{
  return a->min.era <= b->min.era && b->max.era <= a->max.era;
}

Bool EraIntervalIsEmpty(EraInterval ei)
{
  return EraFirstIsEmpty(&ei->min) || EraLastIsEmpty(&ei->max);
}

Bool EraIntervalIsFull(EraInterval ei)
{
  return EraFirstIsFull(&ei->min) && EraLastIsFull(&ei->max);
}

void ZEIInitEmpty(ZEI zei)
{
  ZoneApproxInitEmpty(&zei->za);
  EraIntervalInitEmpty(&zei->eras);
}

void ZEIInitFull(ZEI zei)
{
  ZoneApproxInitFull(&zei->za);
  EraIntervalInitFull(&zei->eras);
}

void ZEICopy(ZEI dest, ZEI src)
{
  ZoneApproxCopy(&dest->za, &src->za);
  EraIntervalCopy(&dest->eras, &src->eras);
}

void ZEIGrow(ZEI zei, ZEI extra)
{
  ZoneApproxGrow(&zei->za, &extra->za);
  EraIntervalGrow(&zei->eras, &extra->eras);
}

void ZEIBound(ZEI zei, ZEI bound)
{
  ZoneApproxBound(&zei->za, &bound->za);
  EraIntervalBound(&zei->eras, &bound->eras);
}

Bool ZEIMeets(ZEI a, ZEI b)
{
  return ZoneApproxMeets(&a->za, &b->za)
    && EraIntervalMeets(&a->eras, &b->eras);
}

Bool ZEIContains(ZEI a, ZEI b)
{
  return ZoneApproxContains(&a->za, &b->za)
    && EraIntervalContains(&a->eras, &b->eras);
}

Bool ZEIIsEmpty(ZEI zei)
{
  return ZoneApproxIsEmpty(&zei->za) || EraIntervalIsEmpty(&zei->eras);
}

Bool ZEIIsFull(ZEI zei)
{
  return ZoneApproxIsFull(&zei->za) && EraIntervalIsFull(&zei->eras);
}


void EraAge(Arena arena)
{
  AVER(arena->era >= EraMIN);
  ++arena->era;
  AVER(arena->era > EraMIN && arena->era < EraINFINITY);
}

void EraIntervalBoundFuture(EraInterval ei, Arena arena)
{
  EraIntervalStruct future;
  eraIntervalInitEra2(&future, arena->era, EraINFINITY);
  EraIntervalBound(ei, &future);
}

void EraIntervalBoundPast(EraInterval ei, Arena arena)
{
  EraIntervalStruct past;
  eraIntervalInitEra2(&past, EraZERO, arena->era);
  EraIntervalBound(ei, &past);
}

void ZEIBoundPast(ZEI zei, Arena arena)
{
  ZEIStruct past;
  ZEIInitFull(&past);
  eraIntervalInitEra2(&past.eras, EraZERO, arena->era);
  ZEIBound(zei, &past);
}

void ZEIGrowFuture(ZEI zei, Arena arena)
{
  ZEIStruct future;
  ZEIInitEmpty(&future);
  eraIntervalInitEra2(&future.eras, arena->era, EraINFINITY);
  ZEIGrow(zei, &future);
}

void ZEIGrowZoneSet(ZEI zei, ZoneSet zones)
{
  ZEIStruct extra;
  ZEIInitEmpty(&extra);
  ZEISetZoneSet(&extra, zones);
  ZEIGrow(zei, &extra);
}

ZoneSet ZEIZoneSet(ZEI zei)
{
  return zei->za.zones;
}

void ZEISetZoneSet(ZEI zei, ZoneSet zones)
{
  zei->za.zones = zones;
}

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
