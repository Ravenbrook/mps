/* impl.c.bt: BIT TABLES
 *
 * $HopeName: MMsrc!bt.c(MMdevel_gavinm_bt.2) $
 * Copyright (C) 1997 Harlequin Group, all rights reserved
 *
 * READERSHIP
 *
 * .readership: Any MPS developer
 *
 * DESIGN
 *
 * .design: see design.mps.bt
 *
 * PURPOSE
 *
 * .purpose: see design.mps.bt
 */


#include "mpm.h"

SRCID(bt, "$HopeName: MMsrc!bt.c(MMdevel_gavinm_bt.2) $");

/* is the whole word of bits at this index set? */

#define BTIsWordSet(bt,i) ((bt)[(i)>>MPS_WORD_SHIFT] == ~(Word)0)

/* align bit-table indices up and down */

#define BTIndexAlignUp(index) ((Index)SizeAlignUp((index), MPS_WORD_WIDTH))
#define BTIndexAlignDown(index) ((Index)SizeAlignDown((index), MPS_WORD_WIDTH))

/* return a word mask of bits set only in requested range */

#define BTMask(base,limit) \
  ((~(Word)0 >> (MPS_WORD_WIDTH - ((limit) - (base)))) << (base))

/* return word and bit indexes from index */

#define BTWordIndex(index) ((index) >> MPS_WORD_SHIFT)
#define BTBitIndex(index) ((index) & (MPS_WORD_WIDTH - 1))

/* macro to act on a base-limit range/
 * Two actions should be provided:
 *   - BIT_ACTION(wordIndex, mask) -- operates on part-words
 *   - WORD_ACTION(wordIndex) -- Operates on full words in range
 * WORD_ACTIONs should not use break or continue.
 */

#define ACT_ON_RANGE(base,limit) \
  BEGIN \
    Index actInnerBase = BTIndexAlignUp((base)); \
    Index actInnerLimit = BTIndexAlignDown((limit)); \
\
    if(actInnerBase > actInnerLimit) { /* no inner range */ \
      if(base < limit) \
        BIT_ACTION(BTWordIndex((base)), \
                   BTMask(BTBitIndex((base)), \
                          BTBitIndex((limit)))); \
    } else { \
      Index actWordIndex, actWordBase, actWordLimit; \
\
      actWordBase = BTWordIndex(actInnerBase); \
      actWordLimit = BTWordIndex(actInnerLimit); \
\
      if(base < actInnerBase) { \
        BIT_ACTION(actWordBase-1, \
                   BTMask(BTBitIndex((base)), MPS_WORD_WIDTH)); \
      } \
\
      for(actWordIndex = actWordBase; actWordIndex < actWordLimit; \
        ++actWordIndex) \
        WORD_ACTION(actWordIndex); \
\
      if(limit > actInnerLimit) \
        BIT_ACTION(actWordLimit, BTMask(0, BTBitIndex((limit)))); \
    } \
  END

#define ACT_ON_RANGE_HIGH(base,limit) \
  BEGIN \
    Index actInnerBase = BTIndexAlignUp((base)); \
    Index actInnerLimit = BTIndexAlignDown((limit)); \
\
    if(actInnerBase > actInnerLimit) { /* no inner range */ \
      if(base < limit) \
        BIT_ACTION(BTWordIndex((base)), \
                   BTMask(BTBitIndex((base)), \
                   BTBitIndex((limit)))); \
    } else { \
      Index actWordIndex, actWordBase, actWordLimit; \
\
      actWordBase = BTWordIndex(actInnerBase); \
      actWordLimit = BTWordIndex(actInnerLimit); \
\
      if(limit > actInnerLimit) \
        BIT_ACTION(actWordLimit, BTMask(0, BTBitIndex((limit)))); \
\
      for(actWordIndex = actWordLimit; actWordIndex > actWordBase; \
        --actWordIndex) \
        WORD_ACTION(actWordIndex-1); \
      if(base < actInnerBase) { \
        BIT_ACTION(actWordBase-1, \
		   BTMask(BTBitIndex((base)), MPS_WORD_WIDTH)); \
      } \
\
    } \
  END


/* BTCreate -- allocate a BT from the control pool
 * 
 * See design.mps.bt.if.create
 */

Res BTCreate(BT *btReturn, Arena arena, Count length)
{
  Res res;
  BT bt;
  void *p;

  AVER(btReturn != NULL);
  AVERT(Arena, arena);
  AVER(length > 0);

  res = ArenaAlloc(&p, arena, BTSize(length));
  if(res != ResOK)
    return res;
  bt = (BT)p;

  *btReturn = bt;
  return ResOK;
}

/* BTDestroy -- free a BT to the control pool.
 * 
 * See design.mps.bt.if.destroy
 */

void BTDestroy(BT bt, Arena arena, Count length)
{
  AVER(bt != NULL);
  AVERT(Arena, arena);
  AVER(length > 0);
  
  ArenaFree(arena, bt, BTSize(length));
}

/* BTCheck -- check the validity of a bit table
 *
 * There's not much that can be checked at present.  This is
 * discussed in review.impl.c.bt.4.
 */

static Bool BTCheck(BT bt)
{
  AVER(bt != NULL);
  AVER(AddrIsAligned((Addr)bt, sizeof(Word)));
  return TRUE;
}


/* design.mps.bt.fun.size */
Size (BTSize)(unsigned long n)
{
  /* check that the expression used in rounding up doesn't overflow */
  AVER(n+MPS_WORD_WIDTH-1 > n);

  return BTSize(n);
}
  

/* design.mps.bt.fun.get */
int (BTGet)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  return BTGet(t, i);
}
  

/* design.mps.bt.fun.set */
void (BTSet)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  BTSet(t, i);
}


/* design.mps.bt.fun.res */
void (BTRes)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  BTRes(t, i);
}


/* design.mps.bt.fun.set-range */
void BTSetRange(BT t, Index i, Index j)
{
  AVER(BTCheck(t));
  AVER(i < j);

#define BIT_ACTION(i,mask) t[(i)] |= (mask)
#define WORD_ACTION(i) t[(i)] = ~(Word)(0)
 
  ACT_ON_RANGE(i, j);
    
#undef BIT_ACTION
#undef WORD_ACTION
}


/* BTIsResRange -- test whether a range of bits is all reset
 *
 * See design.mps.bt.fun.is-reset-range.
 */

Bool BTIsResRange(BT bt, Index base, Index limit)
{
  AVER(BTCheck(bt));
  AVER(base < limit);
  /* Can't check range of base or limit */

#define BIT_ACTION(i,mask) if((bt[(i)] & (mask)) != (Word)0) return FALSE
#define WORD_ACTION(i) if(bt[(i)] != (Word)0) return FALSE
 
  ACT_ON_RANGE(base, limit);
    
#undef BIT_ACTION
#undef WORD_ACTION

  return TRUE;
}


/* BTIsSetRange -- test whether a range of bits is all set
 *
 * See design.mps.bt.fun.is-set-range.
 */

Bool BTIsSetRange(BT bt, Index base, Index limit)
{
  AVER(BTCheck(bt));
  AVER(base < limit);
  /* Can't check range of base or limit */

#define BIT_ACTION(i,mask) if((bt[(i)] & (mask)) != (mask)) return FALSE
#define WORD_ACTION(i) if(bt[(i)] != ~(Word)0) return FALSE

  ACT_ON_RANGE(base, limit);

#undef BIT_ACTION
#undef WORD_ACTION

  return TRUE;
}


/* design.mps.bt.fun.res-range */
void BTResRange(BT t, Index base, Index limit)
{
  AVER(BTCheck(t));
  AVER(base < limit);

#define BIT_ACTION(i,mask) t[(i)] &= ~(mask)
#define WORD_ACTION(i) t[(i)] = (Word)(0)

  ACT_ON_RANGE(base, limit);

#undef BIT_ACTION
#undef WORD_ACTION

}


/* BTFindSetInWord -- finds the lowest set bit in a word
 * Must be called with a non-zero word.
 * Essentially this takes the log base two, but in such 
 * a way that it ignore higher bits.
 */
 
static Index BTFindSetInWord(Word word)
{
  Index index;
  Word mask;
  Count maskWidth;
  
  AVER(word != (Word)0);
  
  maskWidth = MPS_WORD_WIDTH >> 1;
  mask = ~(Word)0 >> maskWidth;
  index = 0;
  
  while(maskWidth != (Count)0) {
    if(word & mask != (Word)0) {
      index += maskWidth;
      word >>= maskWidth;
    }
    
    maskWidth >>= 1;
    mask >>= maskWidth;
  }
  
  return index;
}
  

/* BTFindSetInWordHigh -- finds the highest set bit in a word
 * Must be called with a non-zero word.
 * Essentially this takes the floor of the log base two.
 */
 
static Index BTFindSetInWordHigh(Word word)
{
  Index index;
  Word mask;
  Count maskWidth;
  
  AVER(word != (Word)0);
  
  maskWidth = MPS_WORD_WIDTH >> 1;
  mask = ~(Word)0 << maskWidth;
  index = 0;
  
  while(maskWidth != (Count)0) {
    if(word & mask != (Word)0) 
      index += maskWidth;
    else
      word <<= maskWidth;
    
    maskWidth >>= 1;
    mask <<= maskWidth;
  }
  
  return index;
}
  

/* BTFindSet -- find the lowest set bit in a range in a bit table.
 * Returns false if the range is entirely reset;
 * in this case indexReturn is unset.
 */

static Bool BTFindSet(Index *indexReturn,
                      BT bt, Index base, Index limit)
{
#define ACTION(wi,word) \
  BEGIN \
  if((word) != (Word)0) { \
    *indexReturn = ((wi) << MPS_WORD_SHIFT) | \
                    BTFindSetInWord((word)); \
    return TRUE; \
  } \
  END

#define BIT_ACTION(wi,mask)  ACTION((wi), (bt[(wi)] & (mask)))
#define WORD_ACTION(wi)      ACTION((wi), (bt[(wi)]))
  
  ACT_ON_RANGE(base, limit);

#undef ACTION  
#undef BIT_ACTION
#undef WORD_ACTION
  
  return FALSE;
}


/* BTFindSetHigh -- find the highest set bit in a range in a bit table.
 * Returns false if the range is entirely reset;
 * in this case indexReturn is unset.
 */

static Bool BTFindSetHigh(Index *indexReturn,
                      BT bt, Index base, Index limit)
{
#define ACTION(wi,word) \
  BEGIN \
  if((word) != (Word)0) { \
    *indexReturn = ((wi) << MPS_WORD_SHIFT) | \
                    BTFindSetInWordHigh((word)); \
    return TRUE; \
  } \
  END

#define BIT_ACTION(wi,mask)  ACTION((wi), (bt[(wi)] & (mask)))
#define WORD_ACTION(wi)      ACTION((wi), (bt[(wi)]))
  
  ACT_ON_RANGE_HIGH(base, limit);

#undef ACTION  
#undef BIT_ACTION
#undef WORD_ACTION
  
  return FALSE;
}


/* BTFindResRange -- find a reset range of bits in a bit table,
 * starting at the low end of the search range.
 *
 * See design.mps.bt.fun.find-res-range.
 */

static Bool BTFindResRange(Index *baseReturn, Index *limitReturn,
                           BT bt,
                           Index searchBase, Index searchLimit,
                           unsigned long minLength, unsigned long maxLength)
{
  Index base;      /* base of each candidate range */
  Index limit;     /* limit of each candidate range */
  Index setIndex;  /* index of first set bit found */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(BT, bt);
  AVER(searchBase < searchLimit);
  AVER(minLength > 0);
  AVER(minLength <= maxLength);
  AVER(maxLength <= searchLimit - searchBase);

  base = searchBase;
  while (base <= searchLimit - minLength) {
    limit = base + minLength;
    if (BTIsWordSet(bt, limit - 1)) {   /* skip to the next word */
      base = BTIndexAlignUp(limit);
      if (base < limit) /* overflow case */
        return FALSE;
    } else {                           /* check the candidate range */
      if(!BTFindSetHigh(&setIndex, bt, base, limit)) { 
        /* found reset range of minLength [base,limit) */
        if(searchLimit - base > maxLength)
          searchLimit = base + maxLength; /* limit search */
        if(!BTFindSet(&setIndex, bt, limit, searchLimit))
          setIndex = searchLimit;
        *baseReturn = base;
        *limitReturn = setIndex;
        AVER(setIndex - base >= minLength);
        AVER(setIndex - base <= maxLength);
        return TRUE;
      } else {
        base = setIndex + 1;            /* Skip to reset or unknown bit */
      }
    }
  }
  /* failure */
  return FALSE;
}


/* BTFindResRangeHigh -- find a reset range of bits in a bit table,
 * starting at the high end of the search range.
 *
 * See design.mps.bt.fun.find-res-range.
 */

static Bool BTFindResRangeHigh(Index *baseReturn, Index *limitReturn,
                               BT bt,
                               Index searchBase, Index searchLimit,
                               unsigned long minLength,
                               unsigned long maxLength)
{
  Index base;     /* base of each candidate range */
  Index limit;    /* limit of each candidate range */
  Index setIndex; /* index of first set bit */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(BT, bt);
  AVER(searchBase < searchLimit);
  AVER(minLength > 0);
  AVER(minLength <= maxLength);
  AVER(maxLength <= searchLimit - searchBase);

  limit = searchLimit;
  while (limit >= searchBase + minLength) {
    base = limit - minLength;
    if (BTIsWordSet(bt,base)) {            /* skip to the next word */
      limit = BTIndexAlignDown(base);
    } else {                            /* check the candidate range */
      if(!BTFindSet(&setIndex, bt, base, limit)) { 
        /* found reset range of minLength [base,limit) */
        if(limit - searchBase > maxLength)
          searchBase = limit - maxLength; /* limit search */
        if(!BTFindSetHigh(&setIndex, bt, searchBase, base))
          setIndex = searchBase;
        *baseReturn = setIndex;
        *limitReturn = limit;
        AVER(limit - setIndex >= minLength);
        AVER(limit - setIndex <= maxLength);
        return TRUE;
      } else {
        limit = setIndex;            /* Skip to reset or unknown bit */
      }
    }
  }
  /* failure */
  return FALSE;
}


/* BTFindLongResRange -- find long range of reset bits in a bit table
 *
 * See design.mps.bt.fun.find-long-res-range.
 */

Bool BTFindLongResRange(Index *baseReturn, Index *limitReturn,
                        BT bt,
                        Index searchBase, Index searchLimit,
                        unsigned long length)
{
  /* All parameters are checked by BTFindResRange. */
  return BTFindResRange(baseReturn, limitReturn,
                        bt,
                        searchBase, searchLimit,
                        length, searchLimit - searchBase);
}


/* BTFindShortResRange -- find short range of reset bits in a bit table
 *
 * See design.mps.bt.fun.find-short-res-range.
 */

Bool BTFindShortResRange(Index *baseReturn, Index *limitReturn,
                         BT bt,
                         Index searchBase, Index searchLimit,
                         unsigned long length)
{
  /* All parameters are checked by BTFindResRange. */
  return BTFindResRange(baseReturn, limitReturn,
                        bt,
                        searchBase, searchLimit,
                        length, length);
}

/* BTFindShortResRangeHigh -- find short range of reset bits in a bit table,
 * starting to look from the top of the search range.
 *
 * See design.mps.bt.fun.find-short-res-range-high.
 */

Bool BTFindShortResRangeHigh(Index *baseReturn, Index *limitReturn,
			     BT bt,
			     Index searchBase, Index searchLimit,
			     unsigned long length)
{
  /* All parameters are checked by BTFindResRangeHigh. */
  return BTFindResRangeHigh(baseReturn, limitReturn,
			    bt,
			    searchBase, searchLimit,
			    length, length);
}

/* BTRangesSame -- check that a range of bits in two BTs are the same.
 * 
 * See design.mps.bt.if.ranges-same
 */
 
Bool BTRangesSame(BT comparand, BT comparator, Index base, Index limit)
{
  AVER(BTCheck(comparand));
  AVER(BTCheck(comparator));
  AVER(base < limit);

#define BIT_ACTION(i,mask) \
  if((comparand[(i)] & (mask)) != (comparator[(i)] & (mask))) \
    return FALSE
#define WORD_ACTION(i) \
  if(comparand[(i)] != comparator[(i)]) return FALSE
 
  ACT_ON_RANGE(base, limit);
    
#undef BIT_ACTION
#undef WORD_ACTION

  return TRUE;
}

/* BTCopyInvertRange -- copy a range of bits from one BT to another,
 * inverting them as you go.
 * 
 * See design.mps.bt.if.copy-invert-range
 */

void BTCopyInvertRange(BT fromBT, BT toBT, Index base, Index limit)
{
  AVER(BTCheck(fromBT));
  AVER(BTCheck(toBT));
  AVER(fromBT != toBT);
  AVER(base < limit);

#define BIT_ACTION(i,mask) \
  toBT[(i)] = (toBT[(i)] & ~(mask)) | (~fromBT[(i)] & (mask))
#define WORD_ACTION(i) toBT[(i)] = ~fromBT[(i)]
 
  ACT_ON_RANGE(base, limit);
    
#undef BIT_ACTION
#undef WORD_ACTION
}

