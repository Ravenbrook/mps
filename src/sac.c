/* impl.h.sac: SEGREGATED ALLOCATION CACHES
 *
 * $HopeName: MMsrc!sac.c(MM_epcore_brisling.1) $
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 */

#include "mpm.h"
#include "sac.h"

SRCID(sac, "$HopeName: MMsrc!sac.c(MM_epcore_brisling.1) $");


/* SACCheck -- check function for SACs */

static Bool SACFreeListBlockCheck(SACFreeListBlock fb)
{
  Count j;
  Addr cb;

  /* nothing to check about size */
  CHECKL(fb->count <= fb->countMax);
  /* check the freelist has the right number of blocks */
  for (j = 0, cb = fb->blocks; j < fb->count; ++j) {
    CHECKL(cb != NULL);
    /* @@@@ ignoring shields for now */
    cb = *ADDR_PTR(Addr, cb);
  }
  CHECKL(cb == NULL);
  return TRUE;
}

static Bool SACCheck(SAC sac)
{
  Index i, j;
  Bool b;
  Size prevSize;

  CHECKS(SAC, sac);
  CHECKU(Pool, sac->pool);
  CHECKL(sac->classesCount > 0);
  CHECKL(sac->classesCount > sac->middleIndex);
  CHECKL(BoolCheck(sac->esacStruct.trapped));
  CHECKL(sac->esacStruct.middle > 0);
  /* check classes above middle */
  prevSize = sac->esacStruct.middle;
  for (j = sac->middleIndex + 1, i = 0;
       j <= sac->classesCount; ++j, i += 2) {
    CHECKL(prevSize < sac->esacStruct.freelists[i].size);
    b = SACFreeListBlockCheck(&(sac->esacStruct.freelists[i]));
    if (!b) return b;
  }
  /* check overlarge class */
  CHECKL(sac->esacStruct.freelists[i-2].size == SizeMAX);
  CHECKL(sac->esacStruct.freelists[i-2].count == 0);
  CHECKL(sac->esacStruct.freelists[i-2].countMax == 0);
  CHECKL(sac->esacStruct.freelists[i-2].blocks == NULL);
  /* check classes above middle */
  prevSize = sac->esacStruct.middle;
  for (j = sac->middleIndex, i = 1; j > 0; --j, i += 2) {
    CHECKL(prevSize > sac->esacStruct.freelists[i].size);
    b = SACFreeListBlockCheck(&(sac->esacStruct.freelists[i]));
    if (!b) return b;
  }
  /* check smallest class */
  CHECKL(sac->esacStruct.freelists[i].size == 0);
  b = SACFreeListBlockCheck(&(sac->esacStruct.freelists[i]));
  return b;
}


/* SACSize -- calculate size of a SAC structure */

static Size SACSize(Index middle, Count classesCount)
{
  Index listLen; /* length of the freelist, folded 'round middle */

  if (middle + 1 < classesCount - middle)
    listLen = 2 * (classesCount - middle) - 1;
  else
    listLen = 2 * (middle + 1);

  /* Allocate SAC */
  return sizeof(SAC) + listLen * sizeof(SACFreeListBlock);
}


/* SACCreate -- create an SAC object */

Res SACCreate(SAC *sacReturn, Pool pool, Count classesCount,
              SACClasses classes)
{
  void *p;
  SAC sac;
  Res res;
  Index i, j;
  Index middle;  /* index of the size in the middle */
  unsigned totalfreq = 0;

  AVER(sacReturn != NULL);
  AVERT(Pool, pool);
  AVER(classesCount > 0);
  for (i = 0; i < classesCount; ++i) {
    AVER(classes[i].blockSize > 0);
    AVER(SizeIsAligned(classes[i].blockSize, pool->alignment));
    AVER(i == 0 || classes[i-1].blockSize < classes[i].blockSize);
    /* no restrictions on count */
    /* no restrictions on frequency */
  }

  /* Calculate frequency scale */
  for (i = 0; i < classesCount; ++i) {
    totalfreq += classes[i].frequency; /* @@@@ check? */
  }

  /* Find middle one */
  totalfreq /= 2;
  for (i = 0; i < classesCount; ++i) {
    if (totalfreq < classes[i].frequency) break;
    totalfreq -= classes[i].frequency;
  }
  if (totalfreq <= classes[i].frequency / 2)
    middle = i;
  else
    middle = i + 1; /* there must exist another class at i+1 */

  /* Allocate SAC */
  res = ArenaAlloc(&p, PoolArena(pool), SACSize(middle, classesCount));
  if(res != ResOK)
    goto failSACAlloc;
  sac = p;

  /* Move classes in place */
  /* It's important this matches SACFind. */
  for (j = middle + 1, i = 0; j < classesCount; ++j, i += 2) {
    sac->esacStruct.freelists[i].size = classes[j].blockSize;
    sac->esacStruct.freelists[i].count = 0;
    sac->esacStruct.freelists[i].countMax = classes[j].cachedCount;
    sac->esacStruct.freelists[i].blocks = NULL;
  }
  sac->esacStruct.freelists[i].size = SizeMAX;
  sac->esacStruct.freelists[i].count = 0;
  sac->esacStruct.freelists[i].countMax = 0;
  sac->esacStruct.freelists[i].blocks = NULL;
  for (j = middle, i = 1; j > 0; --j, i += 2) {
    sac->esacStruct.freelists[i].size = classes[j-1].blockSize;
    sac->esacStruct.freelists[i].count = 0;
    sac->esacStruct.freelists[i].countMax = classes[j].cachedCount;
    sac->esacStruct.freelists[i].blocks = NULL;
  }
  sac->esacStruct.freelists[i].size = 0;
  sac->esacStruct.freelists[i].count = 0;
  sac->esacStruct.freelists[i].countMax = classes[j].cachedCount;
  sac->esacStruct.freelists[i].blocks = NULL;

  /* finish init */
  sac->esacStruct.trapped = FALSE;
  sac->esacStruct.middle = classes[middle].blockSize;
  sac->pool = pool;
  sac->classesCount = classesCount;
  sac->middleIndex = middle;
  sac->sig = SACSig;
  *sacReturn = sac;
  return ResOK;

failSACAlloc:
  return res;
}


/* SACDestroy -- destroy an SAC object */

void SACDestroy(SAC sac)
{
  AVERT(SAC, sac);
  SACFlush(sac);
  sac->sig = SigInvalid;
  ArenaFree(PoolArena(sac->pool), sac,
            SACSize(sac->middleIndex, sac->classesCount));
}


/* SACFind -- find the index corresponding to size
 *
 * This function replicates the loop in MPS_SAC_ALLOC, only with
 * added checks.
 */

static void SACFind(Index *iReturn, Size *blockSizeReturn,
                    SAC sac, Size size)
{
  Index i, j;

  if (size > sac->esacStruct.middle) {
    i = 0; j = sac->middleIndex + 1;
    AVER(j <= sac->classesCount);
    while (size > sac->esacStruct.freelists[i].size) {
      AVER(j < sac->classesCount);
      i += 2; ++j;
    }
    *blockSizeReturn = sac->esacStruct.freelists[i].size;
  } else {
    Size prevSize = sac->esacStruct.middle;

    i = 1; j = sac->middleIndex;
    while (size <= sac->esacStruct.freelists[i].size) {
      AVER(j > 0);
      prevSize = sac->esacStruct.freelists[i].size;
      i += 2; --j;
    }
    *blockSizeReturn = prevSize;
  }
  *iReturn = i;
}


/* SACFill -- alloc an object, and perhaps fill the cache */

Res SACFill(Addr *p_o, SAC sac, Size size, Bool hasReservoirPermit)
{
  Index i;
  Count blockCount, j;
  Size blockSize;
  Addr p, fl;
  Res res = ResOK; /* stop compiler complaining */

  AVER(p_o != NULL);
  AVERT(SAC, sac);
  AVER(size != 0);

  SACFind(&i, &blockSize, sac, size);
  /* Check it's empty (in the future, there will be other cases). */
  AVER(sac->esacStruct.freelists[i].count == 0);

  /* Fill the cache for this class, and alloc one more to return. */
  blockCount = sac->esacStruct.freelists[i].countMax;
  /* Adjust size for the overlarge class. */
  if (blockSize == SizeMAX) blockSize = size;
  AVER(SizeIsAligned(blockSize, sac->pool->alignment));
  for (j = 0, fl = sac->esacStruct.freelists[i].blocks;
       j <= blockCount; ++j) {
    res = PoolAlloc(&p, sac->pool, blockSize, hasReservoirPermit);
    if (res != ResOK)
      break;
    /* @@@@ ignoring shields for now */
    *ADDR_PTR(Addr, p) = fl; fl = p;
  }
  /* If didn't get any, just return. */
  if (j == 0)
    return res;

  /* Take the last one off, and return it. */
  sac->esacStruct.freelists[i].count = j - 1;
  *p_o = fl;
  /* @@@@ ignoring shields for now */
  sac->esacStruct.freelists[i].blocks = *ADDR_PTR(Addr, fl);
  return ResOK;
}


/* SACClassFlush -- empty the cache for this class */

static void SACClassFlush(SAC sac, Index i, Size blockSize)
{
  Addr cb, fl;
  Count blockCount, j;

  blockCount = sac->esacStruct.freelists[i].count;
  for (j = 0, fl = sac->esacStruct.freelists[i].blocks;
       j < blockCount; ++j) {
    /* @@@@ ignoring shields for now */
    cb = fl; fl = *ADDR_PTR(Addr, cb);
    PoolFree(sac->pool, cb, blockSize);
  }
  AVER(fl == NULL);
  sac->esacStruct.freelists[i].count = 0;
  sac->esacStruct.freelists[i].blocks = fl;
}


/* SACEmpty -- free an object, and perhaps empty the cache */

void SACEmpty(SAC sac, Addr p, Size size)
{
  Index i;
  Size blockSize;

  AVERT(SAC, sac);
  AVER(p != NULL);
  AVER(PoolHasAddr(sac->pool, p));
  AVER(size > 0);

  SACFind(&i, &blockSize, sac, size);
  /* Check it's full (in the future, there will be other cases). */
  AVER(sac->esacStruct.freelists[i].count
       == sac->esacStruct.freelists[i].countMax);

  /* Adjust size for the overlarge class. */
  if (blockSize == SizeMAX) blockSize = size;
  AVER(SizeIsAligned(blockSize, sac->pool->alignment));
  SACClassFlush(sac, i, blockSize);
  if (sac->esacStruct.freelists[i].countMax > 0) {
    /* Leave the latest one in the cache. */
    sac->esacStruct.freelists[i].count = 1;
    /* @@@@ ignoring shields for now */
    *ADDR_PTR(Addr, p) = NULL;
    sac->esacStruct.freelists[i].blocks = p;
  } else {
    /* Free even the latest one. */
    PoolFree(sac->pool, p, blockSize);
  }
}


/* SACFlush -- flush the cache, releasing all memory held in it */

void SACFlush(SAC sac)
{
  Index i, j;
  Size prevSize;

  AVERT(SAC, sac);

  for (j = sac->middleIndex + 1, i = 0;
       j < sac->classesCount; ++j, i += 2) {
    SACClassFlush(sac, i, sac->esacStruct.freelists[i].size);
  }
  /* no need to flush overlarge, there's nothing there */
  prevSize = sac->esacStruct.middle;
  for (j = sac->middleIndex, i = 1; j > 0; --j, i += 2) {
    SACClassFlush(sac, i, prevSize);
    prevSize = sac->esacStruct.freelists[i].size;
  }
  /* flush smallest class */
  SACClassFlush(sac, i, prevSize);
}
