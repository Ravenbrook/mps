/* impl.c.root: ROOT IMPLEMENTATION
 *
 * $HopeName: MMsrc!root.c(MMdevel_assertid.2) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .scope: This is the implementation of the root datatype.
 * .design: For design, see design.mps.root and 
 * design.mps.root-interface
 */

#include "mpm.h"

SRCID(root, "$HopeName: MMsrc!root.c(MMdevel_assertid.2) $");


/* RootCheck -- check the consistency of a root structure
 *
 * .rootcheck: Keep synchonized with impl.h.mpmst.root
 */

Bool RootCheck(Root root)
{
  CHECKS(0x60020000, Root, root);
  CHECKU(0x60020001, Space, root->space); 
  CHECKL(0x60020002, root->serial < root->space->rootSerial);
  CHECKL(0x60020003, RingCheck(&root->spaceRing));
  CHECKL(0x60020004, RankCheck(root->rank));
  CHECKL(0x60020005, TraceSetCheck(root->grey));
  /* Don't need to check var here, because of the switch below */
  switch(root->var)
  {
    case RootTABLE:
    CHECKL(0x60020006, root->the.table.base != 0);
    CHECKL(0x60020007, root->the.table.base < root->the.table.limit);
    break;

    case RootFUN:
    CHECKL(0x60020008, root->the.fun.scan != NULL);
    break;

    case RootREG:
    CHECKL(0x60020009, root->the.reg.scan != NULL);
    CHECKD(0x6002000A, Thread, root->the.reg.thread);
    break;

    case RootFMT:
    CHECKL(0x6002000B, root->the.fmt.scan != NULL);
    CHECKL(0x6002000C, root->the.fmt.base != 0);
    CHECKL(0x6002000D, root->the.fmt.base < root->the.fmt.limit);
    break;

    default:
    NOTREACHED(0x6002000E);
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

  AVER(0x6002000F, rootReturn != NULL);
  AVERT(0x60020010, Space, space);
  AVERT(0x60020011, Rank, rank);
  AVERT(0x60020012, RootVar, type);

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

  AVERT(0x60020013, Root, root);

  RingAppend(SpaceRootRing(space), &root->spaceRing);

  *rootReturn = root;
  return ResOK;
}

Res RootCreateTable(Root *rootReturn, Space space,
                      Rank rank, Addr *base, Addr *limit)
{
  union RootUnion theUnion;

  AVER(0x60020014, rootReturn != NULL);
  AVERT(0x60020015, Space, space);
  AVER(0x60020016, RankCheck(rank));
  AVER(0x60020017, base != 0);
  AVER(0x60020018, base < limit);  

  theUnion.table.base = base;
  theUnion.table.limit = limit;

  return create(rootReturn, space, rank, RootTABLE, &theUnion);
}

Res RootCreateReg(Root *rootReturn, Space space,
                    Rank rank, Thread thread,
                    RootScanRegMethod scan, void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(0x60020019, rootReturn != NULL);
  AVERT(0x6002001A, Space, space);
  AVER(0x6002001B, RankCheck(rank));
  AVERT(0x6002001C, Thread, thread);
  AVER(0x6002001D, scan != NULL);

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

  AVER(0x6002001E, rootReturn != NULL);
  AVERT(0x6002001F, Space, space);
  AVER(0x60020020, RankCheck(rank));
  AVER(0x60020021, FUNCHECK(scan));
  AVER(0x60020022, base != 0);
  AVER(0x60020023, base < limit);

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

  AVER(0x60020024, rootReturn != NULL);
  AVERT(0x60020025, Space, space);
  AVER(0x60020026, RankCheck(rank));
  AVER(0x60020027, FUNCHECK(scan));

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return create(rootReturn, space, rank, RootFUN, &theUnion);
}

void RootDestroy(Root root)
{
  Space space;

  AVERT(0x60020028, Root, root);

  space = RootSpace(root);

  AVERT(0x60020029, Space, space);

  RingRemove(&root->spaceRing);
  RingFinish(&root->spaceRing);

  root->sig = SigInvalid;

  SpaceFree(space, (Addr)root, sizeof(RootStruct));
}

Rank RootRank(Root root)
{
  AVERT(0x6002002A, Root, root);
  return root->rank;
}

void RootGrey(Root root, Trace trace)
{
  AVERT(0x6002002B, Root, root);
  AVERT(0x6002002C, Trace, trace);
  
  root->grey = TraceSetAdd(root->grey, trace->ti);
}

Res RootScan(ScanState ss, Root root)
{
  Res res;

  AVERT(0x6002002D, Root, root);
  AVERT(0x6002002E, ScanState, ss);
  AVER(0x6002002F, root->rank == ss->rank);

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
    NOTREACHED(0x60020030);
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

  AVERT(0x60020031, Root, root);
  AVER(0x60020032, stream != NULL);

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
    NOTREACHED(0x60020033);
  }

  res = WriteF(stream,
               "} Root $P ($U)\n", (WriteFP)root, (WriteFU)root->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}
