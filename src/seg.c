/* impl.c.seg: SEGMENTS
 *
 * $HopeName$
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 */

#include "mpm.h"

SRCID(seg, "$HopeName$");


/* SegCheck -- check the integrity of a segment */

Bool SegCheck(Seg seg)
{
  CHECKU(Pool, seg->pool);
  CHECKL(TraceSetCheck(seg->white));
  CHECKL(TraceSetCheck(seg->grey));
  if(seg->buffer != NULL)
    CHECKU(Buffer, seg->buffer);
  CHECKL(RingCheck(&seg->poolRing));
  /* @@@@ Check rankSet? */
  /* @@@@ Check pm and sm? */
  /* @@@@ Check depth? */
  /* This next line might be the way to set things up. */
  /* CHECKL((seg->rankSet == RankSetEMPTY) == (seg->summary == RefSetEMPTY)); */
  CHECKL(BoolCheck(seg->single));
  return TRUE;
}


/* SegInit -- initialize the generic part of a segment */

void SegInit(Seg seg, Pool pool)
{
  seg->pool = pool;
  seg->p = NULL;
  seg->rankSet = RankSetEMPTY;
  seg->white = TraceSetEMPTY;
  seg->grey = TraceSetEMPTY;
  seg->summary = RefSetUNIV;		/* @@@@ Safe, but should start empty. */
  seg->buffer = NULL;
  RingInit(&seg->poolRing);
  seg->pm = AccessSetEMPTY;		/* see impl.c.shield */
  seg->sm = AccessSetEMPTY;
  seg->depth = 0;
  seg->single = FALSE;

  AVERT(Seg, seg);
}


/* SegFinish -- finish the generic part of a segment */

void SegFinish(Seg seg)
{
  AVERT(Seg, seg);

  AVER(seg->depth == 0);

  /* @@@@ What else could we check? */

  RingFinish(&seg->poolRing);
}
