/* impl.c.root: ROOT IMPLEMENTATION
 *
 * $HopeName: MMsrc!root.c(MMdevel_remem_root.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .scope: This is the implementation of the root datatype.
 * .design: For design, see design.mps.root and 
 * design.mps.root-interface
 */

#include "mpm.h"

SRCID(root, "$HopeName: MMsrc!root.c(MMdevel_remem_root.1) $");


/* RootVarCheck -- check a Root union discriminator
 *
 * .rootvarcheck: Synchronize with impl.h.mpmtypes.rootvar
 */

Bool RootVarCheck(RootVar rootVar)
{
  AVER(rootVar == RootTABLE ||
       rootVar == RootTABLE_MASKED ||
       rootVar == RootFUN ||
       rootVar == RootFMT ||
       rootVar == RootREG);
  return(TRUE);
}


/* RootCheck -- check the consistency of a root structure
 *
 * .rootcheck: Keep synchonized with impl.h.mpmst.root
 */

Bool RootCheck(Root root)
{
  CHECKS(Root, root);
  CHECKU(Space, root->space); 
  CHECKL(root->serial < root->space->rootSerial);
  CHECKL(RingCheck(&root->spaceRing));
  CHECKL(RankCheck(root->rank));
  CHECKL(TraceSetCheck(root->grey));
  CHECKL(BoolCheck(root->immutable));
  CHECKL(BoolCheck(root->protectable));
  /* .check.wb: If summary isn't universal then the root must either */
  /* be immutable, or write protected. */
  CHECKL(root->summary == RefSetUNIV ||
         (root->protection & AccessWRITE) ||
         root->immutable);
  /* Only protectable roots may be protected. */
  CHECKL(!root->protectable || root->protection != AccessSetEMPTY);
  if(root->protectable) {
    CHECKL(root->protBase != (Addr)0);
    CHECKL(root->protBase < root->protLimit);
  } else {
    CHECKL(root->protBase == (Addr)0);
    CHECKL(root->protLimit == (Addr)0);
  }
  /* Don't need to check var here, because of the switch below */
  switch(root->var)
  {
    case RootTABLE:
    CHECKL(root->the.table.base != 0);
    CHECKL(root->the.table.base < root->the.table.limit);
    break;

    case RootTABLE_MASKED:
    CHECKL(root->the.tableMasked.base != 0);
    CHECKL(root->the.tableMasked.base < root->the.tableMasked.limit);
    /* Can't check anything about the mask. */
    break;

    case RootFUN:
    CHECKL(root->the.fun.scan != NULL);
    break;

    case RootREG:
    CHECKL(root->the.reg.scan != NULL);
    CHECKD(Thread, root->the.reg.thread);
    break;

    case RootFMT:
    CHECKL(root->the.fmt.scan != NULL);
    CHECKL(root->the.fmt.base != 0);
    CHECKL(root->the.fmt.base < root->the.fmt.limit);
    break;

    default:
    NOTREACHED;
  }
  return TRUE;
}


/* rootCreate -- common root creation
 *
 * .create: create, RootCreateTable, RootCreateReg, RootCreateFmt, 
 *   RootCreateFun:
 * RootCreate* set up the appropriate union member, and call the generic
 * create function to do the actual creation 
 * 
 * See design.mps.root.init for initial value
 */

static Res rootCreate(Root *rootReturn, Space space,
                      Rank rank, RootVar type,
                      union RootUnion *theUnionP)
{
  Root root;
  Res res;
  void *p;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVERT(Rank, rank);
  AVERT(RootVar, type);

  res = SpaceAlloc(&p, space, sizeof(RootStruct));
  if(res != ResOK)
    return res;
  root = (Root)p; /* Avoid pun */

  root->space = space;
  root->rank = rank;
  root->var = type;
  root->the  = *theUnionP;
  root->grey = TraceSetEMPTY;
  root->summary = RefSetUNIV;
  root->immutable = FALSE;
  root->protectable = FALSE;
  root->protection = AccessSetEMPTY;
  root->protBase = (Addr)0;
  root->protLimit = (Addr)0;

  /* See design.mps.space.root-ring */
  RingInit(&root->spaceRing);

  root->serial = space->rootSerial;
  ++space->rootSerial;
  root->sig = RootSig;

  AVERT(Root, root);

  RingAppend(SpaceRootRing(space), &root->spaceRing);

  *rootReturn = root;
  return ResOK;
}

Res RootCreateTable(Root *rootReturn, Space space,
                      Rank rank, Addr *base, Addr *limit)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVER(base != 0);
  AVER(base < limit);  

  theUnion.table.base = base;
  theUnion.table.limit = limit;

  return rootCreate(rootReturn, space, rank, RootTABLE, &theUnion);
}

Res RootCreateTableMasked(Root *rootReturn, Space space,
                          Rank rank, Addr *base, Addr *limit,
                          Word mask)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVER(base != 0);
  AVER(base < limit);
  /* Can't check anything about mask. */

  theUnion.tableMasked.base = base;
  theUnion.tableMasked.limit = limit;
  theUnion.tableMasked.mask = mask;

  return rootCreate(rootReturn, space, rank, RootTABLE_MASKED,
                    &theUnion);
}

Res RootCreateReg(Root *rootReturn, Space space,
                    Rank rank, Thread thread,
                    RootScanRegMethod scan, void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVERT(Thread, thread);
  AVER(scan != NULL);

  theUnion.reg.scan = scan;
  theUnion.reg.thread = thread;
  theUnion.reg.p = p;
  theUnion.reg.s = s;

  return rootCreate(rootReturn, space, rank, RootREG, &theUnion);
}

Res RootCreateFmt(Root *rootReturn, Space space,
                  Rank rank, FormatScanMethod scan,
                  Addr base, Addr limit)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVER(FUNCHECK(scan));
  AVER(base != 0);
  AVER(base < limit);

  theUnion.fmt.scan = scan;
  theUnion.fmt.base = base;
  theUnion.fmt.limit = limit;

  return rootCreate(rootReturn, space, rank, RootFMT, &theUnion);
}

Res RootCreateFun(Root *rootReturn, Space space,
                 Rank rank,
                 RootScanMethod scan,
                 void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVER(FUNCHECK(scan));

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return rootCreate(rootReturn, space, rank, RootFUN, &theUnion);
}

void RootDestroy(Root root)
{
  Space space;

  AVERT(Root, root);

  space = RootSpace(root);

  AVERT(Space, space);

  RingRemove(&root->spaceRing);
  RingFinish(&root->spaceRing);

  root->sig = SigInvalid;

  SpaceFree(space, (Addr)root, sizeof(RootStruct));
}

Rank RootRank(Root root)
{
  AVERT(Root, root);
  return root->rank;
}

void RootGrey(Root root, Trace trace)
{
  AVERT(Root, root);
  AVERT(Trace, trace);
  
  root->grey = TraceSetAdd(root->grey, trace->ti);
}


/* RootSummary -- return the summary of a root */

RefSet RootSummary(Root root)
{
  AVERT(Root, root);
  return root->summary;
}


/* rootSetSummary -- set the summary of a root
 *
 * .set-summary: The summary of a root can only be set less than
 * RefSetUNIV if it is immutable or can be write protected.  Otherwise
 * we can't assume that the mutator won't write into it and invalidate
 * the summary.
 */

static void rootSetSummary(Root root, RefSet summary)
{
  AVERT(Root, root);
  /* Can't check summary. */

  if(root->immutable)
    root->summary = summary;
  else if(root->protectable) {
    if(summary == RefSetUNIV) {
      root->summary = summary;
      root->protection &= ~AccessWRITE;
    } else {
      root->protection |= AccessWRITE;
      root->summary = summary;
    }
  } else
    AVER(root->summary == RefSetUNIV);
}


Bool RootOfAddr(Root *rootReturn, Space space, Addr addr)
{
  Ring node;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  /* Can't check addr */

  RING_FOR(node, SpaceRootRing(space)) {
    Root root = RING_ELT(Root, spaceRing, node);
    if(root->protectable &&
       root->protBase <= addr &&
       addr < root->protLimit) {
      *rootReturn = root;
      return TRUE;
    }
  }

  return FALSE;
}


void RootAccess(Root root, AccessSet mode)
{
  AVERT(Root, root);
  /* Can't check mode. */
  AVER((root->protection & mode) != AccessSetEMPTY);
  AVER(mode == AccessWRITE); /* only write protection supported */

  rootSetSummary(root, RefSetUNIV);

  /* Access must now be allowed. */
  AVER((root->protection & mode) == AccessSetEMPTY);
}


/* RootScan -- scan a root */

Res RootScan(ScanState ss, Root root)
{
  Res res;
  Space space;          /* space which owns the root */
  RefSet summary;       /* caller's summary */

  AVERT(Root, root);
  AVERT(ScanState, ss);
  AVER(root->rank == ss->rank);
  AVER(root->space == ss->space);

  if(TraceSetInter(root->grey, ss->traces) == TraceSetEMPTY)
    return ResOK;

  space = root->space;
  summary = ss->summary;
  ss->summary = RefSetEMPTY;

  /* If the root is protected, lift the protection while scanning. */
  /* It's only safe to do this if the mutator's other threads are */
  /* suspended.  (This is a sort of mini-shield.) */
  if(root->protection != AccessSetEMPTY) {
    AVER(space->suspended);
    ProtSet(root->protBase, root->protLimit, AccessSetEMPTY);
  }

  switch(root->var) {
  case RootTABLE:
    res = TraceScanArea(ss,
                        root->the.table.base,
                        root->the.table.limit);
    if(res != ResOK)
      goto failScan;
    break;

  case RootTABLE_MASKED:
    res = TraceScanAreaMasked(ss,
                              root->the.tableMasked.base,
                              root->the.tableMasked.limit,
                              root->the.tableMasked.mask);
    if(res != ResOK)
      goto failScan;
    break;

  case RootFUN:
    res = (*root->the.fun.scan)(ss, root->the.fun.p, root->the.fun.s);
    if(res != ResOK)
      goto failScan;
    break;

  case RootREG:
    res = (*root->the.reg.scan)(ss, root->the.reg.thread,
                                root->the.reg.p, root->the.reg.s);
    if(res != ResOK)
      goto failScan;
    break;

  case RootFMT:
    res = (*root->the.fmt.scan)(ss, root->the.fmt.base,
				root->the.fmt.limit);
    if(res != ResOK)
      goto failScan;
    break;

  default:
    NOTREACHED;
    res = ResUNIMPL;
    goto failScan;
  }

  root->grey = TraceSetDiff(root->grey, ss->traces);
  rootSetSummary(root, ss->summary);
  ss->summary = RefSetUnion(summary, RootSummary(root));

  if(root->protection != AccessSetEMPTY)
    ProtSet(root->protBase, root->protLimit, root->protection);

  return ResOK;

failScan:
  ss->summary = summary;
  if(root->protection != AccessSetEMPTY)
    ProtSet(root->protBase, root->protLimit, root->protection);
  return res;
}

/* Must be thread-safe.  See design.mps.interface.c.thread-safety. */
Space RootSpace(Root root)
{
  /* Can't AVER root as that would not be thread-safe */
  /* AVERT(Root, root); */
  return root->space;
}

Res RootDescribe(Root root, mps_lib_FILE *stream)
{
  Res res;

  AVERT(Root, root);
  AVER(stream != NULL);

  res = WriteF(stream,
               "Root $P ($U) {\n", (WriteFP)root, (WriteFU)root->serial,
               "  space $P ($U)\n", (WriteFP)root->space, 
               (WriteFU)root->space->serial,
               "  rank $U\n", (WriteFU)root->rank,
               "  grey $B\n", (WriteFB)root->grey,
               "  summary $B\n", (WriteFB)root->summary,
               "  immutable $S\n", root->immutable ? "TRUE" : "FALSE",
               "  protectable $S\n", root->protectable ? "TRUE" : "FALSE",
               "  protection $B\n", (WriteFB)root->protection,
               "  protBase $A\n", (WriteFA)root->protBase,
               "  protLimit $A\n", (WriteFA)root->protLimit,
               NULL);
  if(res != ResOK) return res;

  switch(root->var) {
    case RootTABLE:
    res = WriteF(stream,
                 "  table base $A limit $A\n",
                 root->the.table.base,
                 root->the.table.limit,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootTABLE_MASKED:
    res = WriteF(stream,
                 "  table base $A limit $A mask $B\n",
                 root->the.tableMasked.base,
                 root->the.tableMasked.limit,
                 root->the.tableMasked.mask,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootFUN:
    res = WriteF(stream,
                 "  scan function $F\n", (WriteFF)root->the.fun.scan,
                 "  environment p $P s $W\n",
                 root->the.fun.p, (WriteFW)root->the.fun.s,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootREG:
    res = WriteF(stream,
                 "  thread $P\n", (WriteFP)root->the.reg.thread,
                 "  environment p $P", root->the.reg.p,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootFMT:
    res = WriteF(stream,
                 "  scan function $F\n", (WriteFF)root->the.fmt.scan,
                 "  format base $A limit $A\n",
                 root->the.fmt.base,
                 root->the.fmt.limit,
                 NULL);
    if(res != ResOK) return res;
    break;
           
    default:
    NOTREACHED;
  }

  res = WriteF(stream,
               "} Root $P ($U)\n", (WriteFP)root, (WriteFU)root->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}
