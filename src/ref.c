/* impl.c.ref: REFERENCES
 *
 * $HopeName: MMsrc!ref.c(trunk.3) $
 * Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 * Ref is an alias for Addr which can be used to document where
 * references are being passed.
 *
 * RefSet is a conservative approximation to a set of references.
 * It is conservative in the sense that if isMember returns FALSE
 * for a reference then it is guaranteed that the reference was
 * not added into the RefSet (ie, accumulating a pointer guarantees
 * that isMember will return TRUE for that reference), but the
 * converse is not true (ie isMember can return TRUE for references
 * that have not been accumulated).
 *
 * RefSets are designed to provide a very fast method for Add and
 * for IsMember.  Add is used to implement reference summaries,
 * which provide a remembered set.  IsMember is used to inline part
 * of the Fix function, and provide good discrimination of the
 * condemned set.  It is expected that the discrimination provided
 * will be useful for distinguishing segments and groups of segments.
 */

#include "mpm.h"

SRCID(ref, "$HopeName: MMsrc!ref.c(trunk.3) $");

Bool RankCheck(Rank rank)
{
  CHECKL(rank >= 0);
  CHECKL(rank < RankMAX);
  return TRUE;
}
