/* impl.c.shield: SHIELD IMPLEMENTATION
 *
 * $HopeName: MMsrc!shield.c(trunk.4) $
 *
 * See: idea.shield, design.mps.shield.
 *
 * Invariant: The protected memory is a subset of the shielded memory when
 *            inside the shield, and the same set when outside.
 */

#include "mpm.h"

SRCID(shield, "$HopeName: MMsrc!shield.c(trunk.4) $");

static void protect(Space space, Seg seg, ProtMode mode)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  if(seg->pm != mode) {
    ProtSet(SegBase(space, seg), SegLimit(space, seg), mode);
    seg->pm = mode;
  }
}

void ShieldRaise(Space space, Seg seg, ProtMode mode)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  AVER((seg->sm & mode) == ProtNONE);
  seg->sm |= mode;

  if(seg->sm >> 2 == 0)
    protect(space, seg, seg->sm);
}

void ShieldLower(Space space, Seg seg, ProtMode mode)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  AVER((seg->sm & mode) == mode);
  seg->sm &= ~mode;

  if(seg->sm >> 2 == 0)
    protect(space, seg, seg->sm); /* will only remove protection */
}


void ShieldEnter(Space space)
{
  AVERT(Space, space);
  AVER(!space->insideShield);

  ThreadRingSuspend(SpaceThreadRing(space));
  space->insideShield = TRUE;
}

void ShieldLeave(Space space)
{
  AVERT(Space, space);
  AVER(space->insideShield);

  ThreadRingResume(SpaceThreadRing(space));
  space->insideShield = FALSE;
}


void ShieldExpose(Space space, Seg seg)
{
  AVERT(Space, space);
  AVER(space->insideShield);

  seg->sm += 4;

  protect(space, seg, ProtNONE);
}

void ShieldCover(Space space, Seg seg)
{
  AVERT(Space, space);
  AVERT(Seg, seg);
  AVER(seg->pm == ProtNONE);

  AVER(seg->sm >= 4);
  seg->sm -= 4;

  if(seg->sm >> 2 == 0)
    protect(space, seg, seg->sm);
}
