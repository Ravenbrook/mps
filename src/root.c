/* impl.c.root: ROOT IMPLEMENTATION
 *
 * $HopeName: MMsrc!root.c(MMdevel_assertid.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .scope: This is the implementation of the root datatype.
 * .design: For design, see design.mps.root and 
 * design.mps.root-interface
 */

#include "mpm.h"

SRCID(root, "$HopeName: MMsrc!root.c(MMdevel_assertid.1) $");


/* RootCheck -- check the consistency of a root structure
 *
 * .rootcheck: Keep synchonized with impl.h.mpmst.root
 */

Bool RootCheck(Root root)
{
  CHECKS(0xA55E62, Root, root);
  CHECKU(0xA55E62, Space, root->space); 
  CHECKL(0xA55E62, root->serial < root->space->rootSerial);
  CHECKL(0xA55E62, RingCheck(&root->spaceRing));
  CHECKL(0xA55E62, RankCheck(root->rank));
  CHECKL(0xA55E62, TraceSetCheck(root->grey));
  /* Don't need to check var here, because of the switch below */
  switch(root->var)
  {
    case RootTABLE:
    CHECKL(0xA55E62, root->the.table.base != 0);
    CHECKL(0xA55E62, root->the.table.base < root->the.table.limit);
    break;

    case RootFUN:
    CHECKL(0xA55E62, root->the.fun.scan != NULL);
    break;

    case RootREG:
    CHECKL(0xA55E62, root->the.reg.scan != NULL);
    CHECKD(0xA55E62, Thread, root->the.reg.thread);
    break;

    case RootFMT:
    CHECKL(0xA55E62, root->the.fmt.scan != NULL);
    CHECKL(0xA55E62, root->the.fmt.base != 0);
    CHECKL(0xA55E62, root->the.fmt.base < root->the.fmt.limit);
    break;

    default:
    NOTREACHED(0xA55E62);
  }
  return TRUE;
}


/* .create: create, RootCreateTable, RootCreateReg, RootCreateFmt, 
 *   RootCreateFun:
 * RootCreate* set up the appropriate union member, and call the generic
 * create function to do the actual creation 
 * 
 * See design.mps.root.init for initial value
 */

static Res create(Root *rootReturn, Space space,
                  Rank rank, RootVar type,
                  union RootUnion *theUnionP)
{
  Root root;
  Res res;
  void *p;

  AVER(0xA55E62, rootReturn != NULL);
  AVERT(0xA55E62, Space, space);
  AVERT(0xA55E62, Rank, rank);
  AVERT(0xA55E62, RootVar, type);

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

  /* See design.mps.space.root-ring */
  RingInit(&root->spaceRing);

  root->serial = space->rootSerial;
  ++space->rootSerial;
  root->sig = RootSig;

  AVERT(0xA55E62, Root, root);

  RingAppend(SpaceRootRing(space), &root->spaceRing);

  *rootReturn = root;
  return ResOK;
}

Res RootCreateTable(Root *rootReturn, Space space,
                      Rank rank, Addr *base, Addr *limit)
{
  union RootUnion theUnion;

  AVER(0xA55E62, rootReturn != NULL);
  AVERT(0xA55E62, Space, space);
  AVER(0xA55E62, RankCheck(rank));
  AVER(0xA55E62, base != 0);
  AVER(0xA55E62, base < limit);  

  theUnion.table.base = base;
  theUnion.table.limit = limit;

  return create(rootReturn, space, rank, RootTABLE, &theUnion);
}

Res RootCreateReg(Root *rootReturn, Space space,
                    Rank rank, Thread thread,
                    RootScanRegMethod scan, void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(0xA55E62, rootReturn != NULL);
  AVERT(0xA55E62, Space, space);
  AVER(0xA55E62, RankCheck(rank));
  AVERT(0xA55E62, Thread, thread);
  AVER(0xA55E62, scan != NULL);

  theUnion.reg.scan = scan;
  theUnion.reg.thread = thread;
  theUnion.reg.p = p;
  theUnion.reg.s = s;

  return create(rootReturn, space, rank, RootREG, &theUnion);
}

Res RootCreateFmt(Root *rootReturn, Space space,
                  Rank rank, FormatScanMethod scan,
                  Addr base, Addr limit)
{
  union RootUnion theUnion;

  AVER(0xA55E62, rootReturn != NULL);
  AVERT(0xA55E62, Space, space);
  AVER(0xA55E62, RankCheck(rank));
  AVER(0xA55E62, FUNCHECK(scan));
  AVER(0xA55E62, base != 0);
  AVER(0xA55E62, base < limit);

  theUnion.fmt.scan = scan;
  theUnion.fmt.base = base;
  theUnion.fmt.limit = limit;

  return create(rootReturn, space, rank, RootFMT, &theUnion);
}

Res RootCreateFun(Root *rootReturn, Space space,
                 Rank rank,
                 RootScanMethod scan,
                 void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(0xA55E62, rootReturn != NULL);
  AVERT(0xA55E62, Space, space);
  AVER(0xA55E62, RankCheck(rank));
  AVER(0xA55E62, FUNCHECK(scan));

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return create(rootReturn, space, rank, RootFUN, &theUnion);
}

void RootDestroy(Root root)
{
  Space space;

  AVERT(0xA55E62, Root, root);

  space = RootSpace(root);

  AVERT(0xA55E62, Space, space);

  RingRemove(&root->spaceRing);
  RingFinish(&root->spaceRing);

  root->sig = SigInvalid;

  SpaceFree(space, (Addr)root, sizeof(RootStruct));
}

Rank RootRank(Root root)
{
  AVERT(0xA55E62, Root, root);
  return root->rank;
}

void RootGrey(Root root, Trace trace)
{
  AVERT(0xA55E62, Root, root);
  AVERT(0xA55E62, Trace, trace);
  
  root->grey = TraceSetAdd(root->grey, trace->ti);
}

Res RootScan(ScanState ss, Root root)
{
  Res res;

  AVERT(0xA55E62, Root, root);
  AVERT(0xA55E62, ScanState, ss);
  AVER(0xA55E62, root->rank == ss->rank);

  if(TraceSetInter(root->grey, ss->traces) == TraceSetEMPTY)
    return ResOK;

  switch(root->var) {
    case RootTABLE:
    TraceScanArea(ss,
      root->the.table.base,
      root->the.table.limit);
    break;

    case RootFUN:
    res = (*root->the.fun.scan)(ss, root->the.fun.p, root->the.fun.s);
    if(res != ResOK)
      return res;
    break;

    case RootREG:
    res = (*root->the.reg.scan)(ss, root->the.reg.thread,
                              root->the.reg.p, root->the.reg.s);
    if(res != ResOK)
      return res;
    break;

    case RootFMT:
    res = (*root->the.fmt.scan)(ss, root->the.fmt.base,
                              root->the.fmt.limit);
    if(res != ResOK)
      return res;
    break;

    default:
    NOTREACHED(0xA55E62);
  }

  root->grey = TraceSetDiff(root->grey, ss->traces);

  return ResOK;
}

/* Must be thread-safe.  See design.mps.interface.c.thread-safety. */
Space RootSpace(Root root)
{
  return root->space;
}

Res RootDescribe(Root root, mps_lib_FILE *stream)
{
  Res res;

  AVERT(0xA55E62, Root, root);
  AVER(0xA55E62, stream != NULL);

  res = WriteF(stream,
               "Root $P ($U) {\n", (WriteFP)root, (WriteFU)root->serial,
               "  space $P ($U)\n", (WriteFP)root->space, 
               (WriteFU)root->space->serial,
               "  rank $U\n", (WriteFU)root->rank,
               "  grey $B\n", (WriteFB)root->grey,
               "  summary $B\n", (WriteFB)root->summary,
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
    NOTREACHED(0xA55E62);
  }

  res = WriteF(stream,
               "} Root $P ($U)\n", (WriteFP)root, (WriteFU)root->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}
