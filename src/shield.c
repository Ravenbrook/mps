/* impl.c.shield: SHIELD IMPLEMENTATION
 *
 * $HopeName: MMsrc!shield.c(MMdevel_shieldclass.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * See: idea.shield, design.mps.shield.
 *
 * This implementation of the shield avoids suspending threads for
 * as long as possible.  When threads are suspended, it maintains a
 * cache of covered segments where the desired and actual protection
 * do not match.  This cache is flushed on leaving the shield.
 *
 * Definitions
 *
 * .def.synced: a seg is synced if the prot and shield modes are the
 * same, and unsynced otherwise.
 * .def.depth: the depth of a segment is defined as
 *   depth == #exposes - #covers + I(in cache),  where
 *     #exposes = the total number of times the seg has been exposed
 *     #covers  = the total number of times the seg has been covered
 *     I(in cache) = 1 if the segment is in the cache
 *                   0 otherwise
 *   The cache is initially empty and cover should not be called
 *   without a matching expose, so this figure should always be
 *   non-negative.
 * .def.total.depth: The total depth is the sum of the depth over
 * all segments
 * .def.outside: being outside the shield is being between calls
 * to leave and enter, and similarly .def.inside: being inside the
 * shield is being between calls to enter and leave.
 * .def.suspended: suspended is true iff the threads are suspended
 * .def.exposed: a segment is exposed if #exposes > #covers and
 * covered otherwise.
 *
 * Properties
 *
 * .prop.outside.running: The mutator may not be suspended while
 * outside the shield.
 * .prop.mutator.access: An attempt by the mutator to access
 * shielded memory must cause a SpaceAccess.
 * .prop.inside.access: Inside the shield it must be possible to access
 * all unshielded segments and all exposed segments.
 *
 * Invariants
 *
 * These invariants are maintained by the code.
 *
 * .inv.outside.running: The mutator is running while outside the
 * shield.
 * .inv.unsynced.suspended: If any segment is not synced,
 *  the mutator is suspended.
 * .inv.unsynced.depth: All unsynced segments have positive depth.
 * .inv.outside.depth: The total depth is zero while outside the shield.
 * .inv.prot.shield: The prot mode is never more than the shield mode.
 * .inv.expose.prot: An exposed seg is not protected.
 *
 * Hints at proofs of properties from invariants
 *
 * inv.outside.running directly ensures prop.outside running.
 *
 * As the depth of a segment cannot be negative
 *   total depth == 0 => for all segments, depth == 0
 *                    => all segs are synced (by .inv.unsynced.depth)
 * 
 * If the mutator is running then all segs must be synced
 * (.inv.unsynced.suspend).  Which means that the hardware protection
 * (prot mode) must reflect the software protection (shield mode).
 * Hence all shielded memory will be hardware protected while the
 * mutator is running.  This ensures .prop.mutator.access.
 *
 * inv.prot.shield and inv.expose.prot ensure prop.inside.access.
 */

#include "mpm.h"

SRCID(shield, "$HopeName: MMsrc!shield.c(MMdevel_shieldclass.1) $");


Bool ShieldCheck(Shield shield)
{
  CHECKL(shield != NULL);
  /* @@@@ what else? */
  return TRUE;
}


Bool ShieldClassCheck(ShieldClass class)
{
  CHECKS(ShieldClass, class);
  CHECKL(FUNCHECK(class->protect));
  CHECKL(class->endSig == ShieldClassSig);
  return TRUE;
}


void ShieldInit(Shield shield)
{
  AVER(shield != NULL);

  shield->_protectionMode = AccessSetEMPTY;
  shield->_shieldMode = AccessSetEMPTY;
  shield->_depth = 0;

  AVERT(Shield, shield);
}


void ShieldFinish(Shield shield)
{
  AVERT(Shield, shield);

  /* Check that the thing is not exposed, or in the shield */
  /* cache (see .def.depth). */
  AVER(shield->_depth == 0);

  AVER(shield->_protectionMode == AccessSetEMPTY);
  AVER(shield->_shieldMode == AccessSetEMPTY);
}


void ShieldSuspend(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->insideShield);

  if(!arena->suspended) {
    ThreadRingSuspend(ArenaThreadRing(arena));
    arena->suspended = TRUE;
  }
}

void ShieldResume(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->insideShield);
  AVER(arena->suspended);
  /* It is only correct to actually resume the mutator here if 
   * shDepth is 0
   */
}

/* This ensures actual prot mode does not include mode */
static void protLower(Shield shield, ShieldClass class,
                      Arena arena, AccessSet mode)
{
  AVERT(Arena, arena);
  AVERT(Shield, shield);
  AVERT(ShieldClass, class);

  if(shield->_protectionMode & mode) {
    shield->_protectionMode &= ~mode;
    (*class->protect)(shield, shield->_protectionMode);
  }
}

static void sync(Arena arena, Shield shield, ShieldClass class)
{
  AVERT(Arena, arena);
  AVERT(Shield, shield);
  AVERT(ShieldClass, class);

  if(shield->_protectionMode != shield->_shieldMode) {
    (*class->protect)(shield, shield->_shieldMode);
    shield->_protectionMode = shield->_shieldMode;
    /* .inv.prot.shield */
  }
}

static void flush(Arena arena, Size i)
{
  Shield shield;
  ShieldClass class;

  AVERT(Arena, arena);
  AVER(i < SHIELD_CACHE_SIZE);

  shield = arena->shCache[i].shield;
  class = arena->shCache[i].class;
  if(shield != NULL) {
    AVERT(Shield, shield);
    AVERT(ShieldClass, class);

    AVER(arena->shDepth > 0);
    AVER(shield->_depth > 0);
    --arena->shDepth;
    --shield->_depth;
    
    if(shield->_depth == 0)
      sync(arena, shield, class);

    arena->shCache[i].shield = NULL;
    arena->shCache[i].class = NULL;
  } else
    AVER(class == NULL);
}

/* If the segment is out of sync, either sync it, or ensure
 * depth > 0, and the arena is suspended.
 */
static void cache(Arena arena, Shield shield, ShieldClass class)
{
  AVERT(Arena, arena);
  AVERT(Shield, shield);
  AVERT(ShieldClass, class);

  if(shield->_shieldMode != shield->_protectionMode) {
    if(shield->_depth > 0)
      ShieldSuspend(arena);
    else if(SHIELD_CACHE_SIZE == 0 || !arena->suspended)
      sync(arena, shield, class);
    else {
      ++shield->_depth;
      ++arena->shDepth;

      AVER(arena->shDepth > 0);
      AVER(shield->_depth > 0); /* check for overflow */
      AVER(arena->shCacheI < SHIELD_CACHE_SIZE);

      flush(arena, arena->shCacheI);
      arena->shCache[arena->shCacheI].shield = shield;
      arena->shCache[arena->shCacheI].class = class;
      ++arena->shCacheI;
      if(arena->shCacheI == SHIELD_CACHE_SIZE)
	arena->shCacheI = 0;
    }
  }
}

void ShieldRaise(Shield shield, ShieldClass class,
                 Arena arena, AccessSet mode)
{
  AVERT(Arena, arena);
  AVERT(Shield, shield);
  AVERT(ShieldClass, class);

  AVER((shield->_shieldMode & mode) == AccessSetEMPTY);
  shield->_shieldMode |= mode; /* .inv.prot.shield preserved */

  /* ensure .inv.unsynced.suspended and .inv.unsynced.depth */
  cache(arena, shield, class);
}

void ShieldLower(Shield shield, ShieldClass class,
                 Arena arena, AccessSet mode)
{
  AVERT(Arena, arena);
  AVERT(Shield, shield);
  AVERT(ShieldClass, class);
  UNUSED(arena);

  AVER((shield->_shieldMode & mode) == mode);

  /* synced(seg) is not changed by the following */
  /* preserving .inv.unsynced.suspended */
  /* Also .inv.prot.shield preserved */
  shield->_shieldMode &= ~mode;
  protLower(shield, class, arena, mode);
}

void ShieldEnter(Arena arena)
{
  Size i;

  AVERT(Arena, arena);
  AVER(!arena->insideShield);
  AVER(arena->shDepth == 0);
  AVER(!arena->suspended);
  AVER(arena->shCacheI < SHIELD_CACHE_SIZE);
  for(i = 0; i < SHIELD_CACHE_SIZE; i++) {
    AVER(arena->shCache[i].shield == NULL);
    AVER(arena->shCache[i].class == NULL);
  }

  arena->insideShield = TRUE;
}

/* .shield.flush: Flush empties the shield cache.
 * This needs to be called before segments are destroyed as there
 * may be references to them in the cache.
 */
void ShieldFlush(Arena arena)
{
  Size i;

  for(i = 0; i < SHIELD_CACHE_SIZE; ++i) {
    if(arena->shDepth == 0)
      break;
    flush(arena, i);
  }
}

void ShieldLeave(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->insideShield);

  ShieldFlush(arena);
  /* Cache is empty so .inv.outside.depth holds */
  AVER(arena->shDepth == 0);

  /* Ensuring the mutator is running at this point
   * guarantees inv.outside.running */
  if(arena->suspended) {
    ThreadRingResume(ArenaThreadRing(arena));
    arena->suspended = FALSE;
  }
  arena->insideShield = FALSE;
}


void ShieldExpose(Shield shield, ShieldClass class, Arena arena)
{
  AccessSet mode = AccessREAD | AccessWRITE;

  AVERT(Arena, arena);
  AVERT(Shield, shield);
  AVERT(ShieldClass, class);
  AVER(arena->insideShield);

  ++shield->_depth;
  ++arena->shDepth;
  AVER(arena->shDepth > 0);
  AVER(shield->_depth > 0); /* check for overflow */
  if(shield->_protectionMode & mode)
    ShieldSuspend(arena);

  /* This ensures inv.expose.prot */
  protLower(shield, class, arena, mode);
}

void ShieldCover(Shield shield, ShieldClass class, Arena arena)
{
  AVERT(Arena, arena);
  AVERT(Shield, shield);
  AVERT(ShieldClass, class);
  AVER(shield->_protectionMode == AccessSetEMPTY);

  AVER(arena->shDepth > 0);
  AVER(shield->_depth > 0);
  --shield->_depth;
  --arena->shDepth;

  /* ensure inv.unsynced.depth */
  cache(arena, shield, class);
}
