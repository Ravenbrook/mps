/* poolnode.c -- binary tree maps of address ranges to pools
 *
 * $Id$
 * Copyright (C) 2016 Ravenbrook Limited.  See end of file for license.
 */

#include "node.h"
#include "tree.h"
#include "range.h"
#include "mpm.h"


void PoolNodeInit(PoolNode poolNode, Addr base, Addr limit, Pool pool)
{
  NodeInit(PoolNodeNode(poolNode), base, limit);
  poolNode->pool = pool;
}


void PoolNodeFinish(PoolNode poolNode)
{
  NodeFinish(PoolNodeNode(poolNode));
  /* FIXME: this crashes segsmss, indicating reliance on dead data. */
  /* poolNode->pool = (Pool)0xF191583D; */
}


Bool PoolNodeCheck(PoolNode poolNode)
{
  CHECKL(poolNode != NULL);
  CHECKD_NOSIG(Node, PoolNodeNode(poolNode));
  CHECKU(Pool, PoolNodePool(poolNode));
  return TRUE;
}


/* PoolNodeInsert -- coalescing insert range into pool node tree
 *
 * FIXME: Too much code in common with CBSInsert, though the tests are
 * different, and so may be justified by performance of CBSInsert.
 * Need to test.  *This* code could be more general, though, calling a
 * coalescing tree insert that takes functions for tests and merges.
 */

static Res PoolNodeInsert(PoolNode *nodeReturn,
                          SplayTree splay,
                          Addr base, Addr limit, Pool pool,
                          Pool poolNodePool, Size poolNodeSize)
{
  PoolNode poolNode;
  Bool b;
  Tree left, right;

  AVERT(SplayTree, splay);
  AVER(base < limit);
  AVERT(Pool, pool);
  AVERT(Pool, poolNodePool);

  splay = ArenaPoolNodeSplay(PoolArena(pool));
  b = SplayTreeNeighbours(&left, &right, splay, base);
  AVER(b); /* should not already be in the tree */

  /* Can coalesce left? */
  if (left != TreeEMPTY &&
      PoolNodePool(PoolNodeOfTree(left)) == pool &&
      PoolNodeLimit(PoolNodeOfTree(left)) == base) {

    /* Can coalesce right? */
    if (right != TreeEMPTY &&
        PoolNodePool(PoolNodeOfTree(right)) == pool &&
        limit == PoolNodeBase(PoolNodeOfTree(right))) {

      /* coalesce right side, freeing right node */
      limit = PoolNodeLimit(PoolNodeOfTree(right));
      b = SplayTreeDelete(splay, NodeKey(right));
      AVER(b); /* should be in the tree, we just found it! */
      PoolFree(poolNodePool, (Addr)PoolNodeOfTree(right), poolNodeSize);
    }

    /* coalesce left side, returning left node */
    PoolNodeSetLimit(PoolNodeOfTree(left), limit);
    poolNode = PoolNodeOfTree(left);

  } else {

    /* Can coalesce right? */
    if (right != TreeEMPTY &&
        PoolNodePool(PoolNodeOfTree(right)) == pool &&
        limit == PoolNodeBase(PoolNodeOfTree(right))) {

      /* coalesce right side, returning right node */
      PoolNodeSetBase(PoolNodeOfTree(right), base);
      poolNode = PoolNodeOfTree(right);

    } else {

      /* can't coalesce, so make new node */
      Addr p;
      Res res = PoolAlloc(&p, poolNodePool, poolNodeSize);
      if (res != ResOK)
        return res;
      poolNode = (PoolNode)p;
      PoolNodeInit(poolNode, base, limit, pool);
      SplayTreeInsert(splay, PoolNodeTree(poolNode));
    }
  }

  *nodeReturn = poolNode;
  return ResOK;
}


/* PoolNodeAlloc -- coalescing insert pool node into tree */

Res PoolNodeAlloc(PoolNode *nodeReturn, LocusPref pref,
                  Pool pool, Size size,
                  Pool poolNodePool, Size poolNodeSize)
{
  Res res;
  Addr base;
  PoolNode poolNode;

  AVER(nodeReturn != NULL);
  AVERT(LocusPref, pref);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(Pool, poolNodePool);

  /* TOOD: Convert ArenaAlloc to return a range */
  res = ArenaAlloc(&base, pref, size, pool);
  if (res != ResOK)
    goto failArenaAlloc;

  res = PoolNodeInsert(&poolNode,
                       ArenaPoolNodeSplay(PoolArena(pool)),
                       base, AddrAdd(base, size), pool,
                       poolNodePool, poolNodeSize);
  if (res != ResOK)
    goto failNodeInsert;

  *nodeReturn = poolNode;
  return ResOK;

failNodeInsert:
  ArenaFree(base, size, pool);
failArenaAlloc:
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
