/* impl.c.protan: ANSI MEMORY PROTECTION
 *
 * $HopeName: MMsrc!protan.c(MMdevel_assertid.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer
 *
 * DESIGN
 *
 * design.mps.protan
 *
 */

#include "mpm.h"

SRCID(protan, "$HopeName: MMsrc!protan.c(MMdevel_assertid.1) $");

void ProtSetup(void)
{
  NOOP;
}

void ProtSet(Addr base, Addr limit, AccessSet pm)
{
  AVER(0xA55E62, base < limit);
  /* .improve.protset.check: There is nor AccessSetCheck, so we */
  /* don't check it. */
  UNUSED(pm);
  NOOP;
}

/* design.mps.protan.fun.sync */
void ProtSync(Space space)
{
  Bool synced;

  AVERT(0xA55E62, Space, space);

  do {
    Seg seg;

    synced = TRUE;
    seg = SegFirst(space);
    while(seg != NULL) {
      if(seg->pm != AccessSetEMPTY) {   /* design.mps.protan.fun.sync.seg */
        ShieldEnter(space);
        TraceAccess(space, seg, seg->pm);
        ShieldLeave(space);
        synced = FALSE;
      }
      seg = SegNext(space, seg);
    }
  } while(!synced);
}

void ProtTramp(void **rReturn,
                void *(*f)(void *, size_t),
                void *p, size_t s)
{
  AVER(0xA55E62, rReturn != NULL);
  AVER(0xA55E62, FUNCHECK(f));
  /* Can't check p and s as they are interpreted by the client */

  *(rReturn) = (*(f))(p, s);
}
