/* impl.c.root: ROOT IMPLEMENTATION
 *
 * $HopeName: MMsrc!root.c(MMdevel_trace2.2) $
 * Copyright (C) 1995 Harlequin Group, all rights reserved.
 */

#include "mpm.h"

SRCID(root, "$HopeName: MMsrc!root.c(MMdevel_trace2.2) $");


/* RootCheck -- check consistency of root structure */

Bool RootCheck(Root root)
{
  CHECKS(Root, root);
  CHECKU(Space, root->space);
  CHECKL(root->serial < root->space->rootSerial);
  CHECKL(RingCheck(&root->spaceRing));
  CHECKL(RankCheck(root->rank));

  switch(root->var) {
    case RootTABLE:
    CHECKL(root->the.table.refs != NULL);
    CHECKL(root->the.table.size > 0);
    break;

    case RootFUN:
    CHECKL(FUNCHECK(root->the.fun.scan));
    break;

    case RootREG:
    CHECKL(FUNCHECK(root->the.reg.scan));
    CHECKD(Thread, root->the.reg.thread);
    break;

    case RootFMT:
    CHECKL(FUNCHECK(root->the.fmt.scan));
    CHECKL(root->the.fmt.base != 0);
    CHECKL(root->the.fmt.base < root->the.fmt.limit);
    break;

    default:
    NOTREACHED;
  }

  return TRUE;
}


/* create -- create a root of any variant
 *
 * This code is shared by the various RootCreate* methods below.
 */

static Res create(Root *rootReturn, Space space,
                  Rank rank, RootVar type,
                  union RootUnion theUnion)
{
  Root root;
  Res res;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVERT(Rank, rank);

  res = SpaceAlloc((Addr *)&root, space, sizeof(RootStruct));
  if(res != ResOK)
    return res;

  root->space = space;
  root->var = type;
  root->the  = theUnion;

  RingInit(&root->spaceRing);

  root->sig = RootSig;
  root->serial = space->rootSerial;
  ++space->rootSerial;

  AVERT(Root, root);

  RingAppend(SpaceRootRing(space), &root->spaceRing);

  *rootReturn = root;
  return ResOK;
}


/* RootCreateTable -- create a root consisting of a table of refs */

Res RootCreateTable(Root *rootReturn, Space space,
                    Rank rank, Addr *refs, size_t size)
{
  union RootUnion theUnion;

  AVER(refs != NULL);
  AVER(size > 0);

  theUnion.table.refs = refs;
  theUnion.table.size = size;

  return create(rootReturn, space, rank, RootTABLE, theUnion);
}


/* RootCreateReg -- create a root of a thread's registers */

Res RootCreateReg(Root *rootReturn, Space space,
                  Rank rank, Thread thread,
                  RootScanRegMethod scan, void *p)
{
  union RootUnion theUnion;

  AVER(FUNCHECK(scan));
  AVERT(Thread, thread);

  theUnion.reg.scan = scan;
  theUnion.reg.thread = thread;
  theUnion.reg.p = p;

  return create(rootReturn, space, rank, RootREG, theUnion);
}


/* RootCreateFmt -- create a root of formatted objects
 *
 * The root refers to an area of memory between base and limit
 * consisting of contiguous objects formatted according to the
 * scan method.
 */

Res RootCreateFmt(Root *rootReturn, Space space,
                  Rank rank, FormatScanMethod scan,
                  Addr base, Addr limit)
{
  union RootUnion theUnion;

  AVER(FUNCHECK(scan));
  AVER(base != 0);
  AVER(base < limit);

  theUnion.fmt.scan = scan;
  theUnion.fmt.base = base;
  theUnion.fmt.limit = limit;

  return create(rootReturn, space, rank, RootFMT, theUnion);
}


/* RootCreate -- create a root from a scanning function */

Res RootCreate(Root *rootReturn, Space space,
               Rank rank, RootScanMethod scan,
               void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(FUNCHECK(scan));

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return create(rootReturn, space, rank, RootFUN, theUnion);
}


/* RootDestroy -- destroy a root */

void RootDestroy(Root root)
{
  Space space;

  AVERT(Root, root);

  space = RootSpace(root);

  RingRemove(&root->spaceRing);
  RingFinish(&root->spaceRing);

  root->sig = SigInvalid;

  SpaceFree(space, (Addr)root, sizeof(RootStruct));
}


/* RootRank -- get the rank of a root */

Rank RootRank(Root root)
{
  AVERT(Root, root);
  return root->rank;
}


static Res ScanArea(Fix fix, Addr *base, Addr *limit)
{
  Res res;
  Addr *p;
  Ref ref;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  TRACE_SCAN_BEGIN(fix) {
          p = base;
  loop:   if(p >= limit) goto out;
          ref = *p++;
          if(!TRACE_FIX1(fix, ref)) goto loop;
          res = TRACE_FIX2(p-1, fix);
          if(res == ResOK) goto loop;
          return res;
  out:    AVER(p == limit);
  } TRACE_SCAN_END(fix);

  return ResOK;
}


/* RootScan -- apply fix to references in a root */

Res RootScan(Root root, Fix fix)
{
  Res res;

  AVERT(Root, root);
  AVERT(Fix, fix);
  
  AVER(fix->rank == root->rank);

  switch(root->var) {
    case RootFUN:
    res = (*root->the.fun.scan)(fix, root->the.fun.p, root->the.fun.s);
    if(res != ResOK) return res;
    break;

    case RootTABLE:
    res = ScanArea(fix, root->the.table.refs,
                   &root->the.table.refs[root->the.table.size]);
    if(res != ResOK) return res;
    break;

    case RootREG:
    res = (*root->the.reg.scan)(fix, root->the.reg.thread,
                                root->the.reg.p);
    if(res != ResOK) return res;
    break;

    case RootFMT:
    res = (*root->the.fmt.scan)(fix, root->the.fmt.base,
                                root->the.fmt.limit);
    if(res != ResOK) return res;
    break;

    default:
    NOTREACHED;
  }

  return ResOK;
}


/* Must be thread-safe.  See impl.c.mpsi.thread-safety. */
Space RootSpace(Root root)
{
  return root->space;
}


Res RootDescribe(Root root, mps_lib_FILE *stream)
{
  Res res;

  AVERT(Root, root);
  AVER(stream != NULL);

  res = WriteF(stream,
               "Root $P ($U) {\n", (void *)root, (unsigned long)root->serial,
               "  space $P ($U)\n", (void *)root->space, (unsigned long)root->space->serial,
               /* @@@@ Add fields back here. */
               NULL);
  if(res != ResOK) return res;

  switch(root->var) {
    case RootTABLE:
    res = WriteF(stream,
                 "  table refs $P size $U\n",
                 (void *)root->the.table.refs,
                 (unsigned long)root->the.table.size,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootFUN:
    res = WriteF(stream,
                 "  scan function $P\n", (void *)root->the.fun.scan,
                 "  environment p $P s $W\n",
                 root->the.fun.p, (Word)root->the.fun.s,
                 NULL);
    if(res != ResOK) return res;
    break;

    default:
    NOTREACHED;
  }

  res = WriteF(stream,
               "} Root $P ($U)\n", (void*)root, (unsigned long)root->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}
