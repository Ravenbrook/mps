/* impl.c.protan: ANSI MEMORY PROTECTION
 *
 * $HopeName: MMsrc!protan.c(trunk.4) $
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

SRCID(protan, "$HopeName: MMsrc!protan.c(trunk.4) $");

void ProtSetup(void)
{
  NOOP;
}

void ProtSet(Addr base, Addr limit, AccessSet pm)
{
  AVER(base < limit);
  /* .improve.protset.check: There is nor AccessSetCheck, so we */
  /* don't check it. */
  UNUSED(pm);
  NOOP;
}

/* design.mps.protan.fun.sync */
void ProtSync(Arena arena)
{
  Bool synced;

  AVERT(Arena, arena);

  do {
    Seg seg;

    synced = TRUE;
    seg = ArenaSegFirst(arena);
    while(seg != NULL) {
      Seg next = ArenaSegNext(arena, seg);
      if(seg->pm != AccessSetEMPTY) {   /* design.mps.protan.fun.sync.seg */
        ShieldEnter(arena);
        TraceAccess(arena, seg, seg->pm);
        ShieldLeave(arena);
        synced = FALSE;
      }
      seg = next;
    }
  } while(!synced);
}

void ProtTramp(void **rReturn,
                void *(*f)(void *, size_t),
                void *p, size_t s)
{
  AVER(rReturn != NULL);
  AVER(FUNCHECK(f));
  /* Can't check p and s as they are interpreted by the client */

  *(rReturn) = (*(f))(p, s);
}
