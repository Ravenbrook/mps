/* impl.c.seg: SEGMENTS
 *
 * $HopeName: MMsrc!seg.c(MMdevel_assertid.1) $
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

SRCID(seg, "$HopeName: MMsrc!seg.c(MMdevel_assertid.1) $");


/* SegCheck -- check the integrity of a segment */

Bool SegCheck(Seg seg)
{
  CHECKU(0xA55E62, Pool, seg->pool);
  CHECKL(0xA55E62, TraceSetCheck(seg->white));
  CHECKL(0xA55E62, TraceSetCheck(seg->grey));
  CHECKL(0xA55E62, TraceSetCheck(seg->black));
  if(seg->buffer != NULL) {
    CHECKU(0xA55E62, Buffer, seg->buffer);
    /* design.mps.seg.field.buffer.owner */
    CHECKL(0xA55E62, BufferPool(seg->buffer) == seg->pool);
  }
  CHECKL(0xA55E62, RingCheck(&seg->poolRing));
  CHECKL(0xA55E62, RankSetCheck(seg->rankSet));
  if(seg->rankSet == RankSetEMPTY) {
    /* design.mps.seg.field.rankSet.empty: If there are no refs */
    /* in the segment then it cannot contain black or grey refs. */
    CHECKL(0xA55E62, seg->grey == TraceSetEMPTY);
    CHECKL(0xA55E62, seg->black == TraceSetEMPTY);
    CHECKL(0xA55E62, seg->summary == RefSetEMPTY);
    CHECKL(0xA55E62, seg->sm == AccessSetEMPTY);
    CHECKL(0xA55E62, seg->pm == AccessSetEMPTY);
  } else {
    /* design.mps.seg.field.rankSet.single: The Tracer only permits */
    /* one rank per segment [ref?] so this field is either empty or a */
    /* singleton. */
    CHECKL(0xA55E62, RankSetIsSingle(seg->rankSet));
    /* .check.wb: If summary isn't universal then it must be Write shielded */
    CHECKL(0xA55E62, seg->summary == RefSetUNIV || (seg->sm & AccessWRITE));
  }
  /* "pm", "sm", and "depth" not checked.  See .check.shield. */
  CHECKL(0xA55E62, BoolCheck(seg->single));
  return TRUE;
}


/* SegInit -- initialize the generic part of a segment */

void SegInit(Seg seg, Pool pool)
{
  seg->pool = pool;
  seg->p = NULL;
  seg->rankSet = RankSetEMPTY;
  seg->black = TraceSetEMPTY;
  seg->white = TraceSetEMPTY;
  seg->grey = TraceSetEMPTY;
  seg->summary = RefSetEMPTY;
  seg->buffer = NULL;
  RingInit(&seg->poolRing);
  seg->pm = AccessSetEMPTY;
  seg->sm = AccessSetEMPTY;
  seg->depth = 0;
  seg->single = FALSE;

  AVERT(0xA55E62, Seg, seg);
}


/* SegFinish -- finish the generic part of a segment */

void SegFinish(Seg seg)
{
  AVERT(0xA55E62, Seg, seg);

  /* Check that the segment is not exposed, or in the shield */
  /* cache (see impl.c.shield.def.depth). */
  AVER(0xA55E62, seg->depth == 0);
  
  /* Don't leave a dangling buffer allocating into hyperspace. */
  AVER(0xA55E62, seg->buffer == NULL);

  RingFinish(&seg->poolRing);
}
