/* impl.c.seg: SEGMENTS
 *
 * $HopeName: MMsrc!seg.c(MMdevel_segabs.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .design: The design for this module is design.mps.seg.
 *
 * TRANSGRESSIONS
 *
 * .check.shield: The "pm", "sm", and "depth" fields are not checked by
 * SegCheck, because I haven't spent time working out the invariants.
 * We should certainly work them out, by studying impl.c.shield, and
 * assert things about shielding, protection, shield cache consistency,
 * etc. richard 1997-04-03
 */

#include "mpm.h"

SRCID(seg, "$HopeName: MMsrc!seg.c(MMdevel_segabs.1) $");


/* SegCheck -- check the integrity of a segment */

Bool SegCheck(Seg seg)
{
  CHECKU(Pool, seg->_pool);
  CHECKL(TraceSetCheck(seg->_white));
  CHECKL(TraceSetCheck(seg->_grey));
/*  CHECKL(TraceSetCheck(seg->_black)); */
  if(seg->_buffer != NULL) {
    CHECKU(Buffer, seg->_buffer);
    /* design.mps.seg.field.buffer.owner */
    CHECKL(BufferPool(seg->_buffer) == seg->_pool);
  }
  CHECKL(RingCheck(&seg->_poolRing));
  CHECKL(RankSetCheck(seg->_rankSet));
  if(seg->_rankSet == RankSetEMPTY) {
    /* design.mps.seg.field.rankSet.empty: If there are no refs */
    /* in the segment then it cannot contain black or grey refs. */
    CHECKL(seg->_grey == TraceSetEMPTY);
/*    CHECKL(seg->_black == TraceSetEMPTY); */
    CHECKL(seg->_summary == RefSetEMPTY);
    CHECKL(seg->_sm == AccessSetEMPTY);
    CHECKL(seg->_pm == AccessSetEMPTY);
  } else {
    /* design.mps.seg.field.rankSet.single: The Tracer only permits */
    /* one rank per segment [ref?] so this field is either empty or a */
    /* singleton. */
    CHECKL(RankSetIsSingle(seg->_rankSet));
    /* .check.wb: If summary isn't universal then it must be Write shielded */
    CHECKL(seg->_summary == RefSetUNIV || (seg->_sm & AccessWRITE));
  }
  /* "pm", "sm", and "depth" not checked.  See .check.shield. */
  CHECKL(BoolCheck(seg->_single));
  return TRUE;
}


/* SegInit -- initialize the generic part of a segment */

void SegInit(Seg seg, Pool pool)
{
  seg->_pool = pool;
  seg->_p = NULL;
  seg->_rankSet = RankSetEMPTY;
/*  seg->_black = TraceSetEMPTY; */
  seg->_white = TraceSetEMPTY;
  seg->_grey = TraceSetEMPTY;
  seg->_summary = RefSetEMPTY;
  seg->_buffer = NULL;
  RingInit(&seg->_poolRing);
  seg->_pm = AccessSetEMPTY;
  seg->_sm = AccessSetEMPTY;
  seg->_depth = 0;
  seg->_single = FALSE;

  AVERT(Seg, seg);
}


/* SegFinish -- finish the generic part of a segment */

void SegFinish(Seg seg)
{
  AVERT(Seg, seg);

  /* Check that the segment is not exposed, or in the shield */
  /* cache (see impl.c.shield.def.depth). */
  AVER(seg->_depth == 0);
  
  /* Don't leave a dangling buffer allocating into hyperspace. */
  AVER(seg->_buffer == NULL);

  RingFinish(&seg->_poolRing);
}
