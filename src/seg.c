/* impl.c.seg: SEGMENTS
 *
 * $HopeName: MMsrc!seg.c(MMdevel_assertid.3) $
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

SRCID(seg, "$HopeName: MMsrc!seg.c(MMdevel_assertid.3) $");


/* SegCheck -- check the integrity of a segment */

Bool SegCheck(Seg seg)
{
  CHECKS(0x5E990014, Seg, seg);
  CHECKU(0x5E990000, Pool, seg->pool);
  CHECKL(0x5E990001, TraceSetCheck(seg->white));
  CHECKL(0x5E990002, TraceSetCheck(seg->grey));
  CHECKL(0x5E990003, TraceSetCheck(seg->black));
  if(seg->buffer != NULL) {
    CHECKU(0x5E990004, Buffer, seg->buffer);
    /* design.mps.seg.field.buffer.owner */
    CHECKL(0x5E990005, BufferPool(seg->buffer) == seg->pool);
  }
  CHECKL(0x5E990006, RingCheck(&seg->poolRing));
  CHECKL(0x5E990007, RankSetCheck(seg->rankSet));
  if(seg->rankSet == RankSetEMPTY) {
    /* design.mps.seg.field.rankSet.empty: If there are no refs */
    /* in the segment then it cannot contain black or grey refs. */
    CHECKL(0x5E990008, seg->grey == TraceSetEMPTY);
    CHECKL(0x5E990009, seg->black == TraceSetEMPTY);
    CHECKL(0x5E99000A, seg->summary == RefSetEMPTY);
    CHECKL(0x5E99000B, seg->sm == AccessSetEMPTY);
    CHECKL(0x5E99000C, seg->pm == AccessSetEMPTY);
  } else {
    /* design.mps.seg.field.rankSet.single: The Tracer only permits */
    /* one rank per segment [ref?] so this field is either empty or a */
    /* singleton. */
    CHECKL(0x5E99000D, RankSetIsSingle(seg->rankSet));
    /* .check.wb: If summary isn't universal then it must be Write shielded */
    CHECKL(0x5E99000E, seg->summary == RefSetUNIV || (seg->sm & AccessWRITE));
  }
  /* "pm", "sm", and "depth" not checked.  See .check.shield. */
  CHECKL(0x5E99000F, BoolCheck(seg->single));
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
  
  seg->sig = SegSig;

  AVERT(0x5E990010, Seg, seg);
}


/* SegFinish -- finish the generic part of a segment */

void SegFinish(Seg seg)
{
  AVERT(0x5E990011, Seg, seg);

  /* Check that the segment is not exposed, or in the shield */
  /* cache (see impl.c.shield.def.depth). */
  AVER(0x5E990012, seg->depth == 0);
  
  /* Don't leave a dangling buffer allocating into hyperspace. */
  AVER(0x5E990013, seg->buffer == NULL);
  
  seg->sig = SigInvalid;

  RingFinish(&seg->poolRing);
}
