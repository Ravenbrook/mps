/* impl.c.ref: REFERENCES
 *
 * $HopeName: MMsrc!ref.c(MMdevel_drj_amcz.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved
 *
 * .def.ref: Ref is an alias for Addr which can be used to document
 * where references are being passed.
 *
 * .def.refset: RefSet is a conservative approximation to a set of
 * references.  It is conservative in the sense that if isMember returns
 * FALSE for a reference then it is guaranteed that the reference was
 * not added into the RefSet (i.e., accumulating a pointer guarantees
 * that isMember will return TRUE for that reference), but the converse
 * is not true (i.e., isMember can return TRUE for references that have
 * not been accumulated).
 *
 * RefSets are designed to provide a very fast method for Add and
 * for IsMember.  Add is used to implement reference summaries,
 * which provide a remembered set.  IsMember is used to inline part
 * of the Fix function, and provide good discrimination of the
 * white set.  It is expected that the discrimination provided
 * will be useful for distinguishing segments and groups of segments.
 *
 * .def.objset: ObjSets are a parallel concept to RefSets and give
 * a conservative approximation to a set of objects.  It is conservative
 * in the sense that if isMember returns FALSE for an object, then it is
 * guarantees that the object is not in the set.
 *
 * For a set of objects, another object may have a reference to the set
 * if its reference set intersects with the object set.
 */

#include "mpm.h"

SRCID(ref, "$HopeName: MMsrc!ref.c(MMdevel_drj_amcz.1) $");

Bool RankCheck(Rank rank)
{
  CHECKL(rank < RankMAX);
  UNUSED(rank); /* impl.c.mpm.check.unused */
  return TRUE;
}


Bool RankSetCheck(RankSet rankSet)
{
  CHECKL(rankSet < (1uL << RankMAX));
  UNUSED(rankSet); /* impl.c.mpm.check.unused */
  return TRUE;
}


/* ObjSetOfSeg -- calculate the object set of segment addresses
 *
 * .osos.def: The object set of a segment is the union of the
 * set of potential references _to_ that segment, i.e. of all the
 * addresses the segment occupies.
 *
 * .osos.zones: The base and limit zones of the segment
 * are calculated.  The limit zone is one plus the zone of the last
 * address in the segment, not the zone of the limit address.
 *
 * .osos.univ: If the segment is large enough to span all zones,
 * its reference set is universal.
 *
 * .osos.swap: If the base zone is less than the limit zone,
 * then the reference set looks like 000111100, otherwise it looks like
 * 111000011.
 */

ObjSet ObjSetOfSeg(Space space, Seg seg)
{
  Word base, limit;

  AVERT(Space, space);
  AVERT(Seg, seg);

  /* .rsos.zones */
  base = (Word)SegBase(seg) >> space->zoneShift;
  limit = (((Word)SegLimit(seg)-1) >> space->zoneShift) + 1;

  if(limit - base >= MPS_WORD_WIDTH)        /* .osos.univ */
    return ObjSetUNIV;

  base  &= MPS_WORD_WIDTH - 1;
  limit &= MPS_WORD_WIDTH - 1;

  if(base < limit)                      /* .osos.swap */
    return ((ObjSet)1<<limit) - ((ObjSet)1<<base);
  else
    return ~(((ObjSet)1<<base) - ((ObjSet)1<<limit));
}
