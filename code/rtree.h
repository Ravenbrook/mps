/* rtree.h: RANGE TREE HEADER
 *
 * $Id$
 * Copyright 2014 Ravenbrook Limited.  See end of file for license.
 *
 * A range tree is a binary tree of address ranges.
 */

#ifndef rtree_h
#define rtree_h

#include "mpmtypes.h"


typedef void (*RTreeUpdateMethod)(RNode node);

#define RTreeSig ((Sig)0x519626EE) /* SIGnature RTREE */

typedef struct RTreeStruct {
  Sig sig;
  RNode root;
  RTreeUpdateMethod update;
} RTreeStruct;


#define RNodeSig ((Sig)0x519690DE) /* SIGnature RNODE */

typedef struct RNodeStruct {
  Sig sig;
  RNode left, right;
  Addr base, limit;
} RNodeStruct;

extern RNodeStruct RTreeLeaf;
/* #define RTREE_LEAF (&RTreeLeaf) */
#define RTREE_LEAF NULL


extern Bool RTreeCheck(RTree tree);
extern Bool RNodeCheck(RNode node);
extern void RTreeInit(RTree tree, RTreeUpdateMethod update);
extern void RNodeInit(RNode node, Addr base, Addr limit);
extern void RNodeFinish(RNode node);
extern void RTreeFinish(RTree tree);
extern void RTreeReset(RTree tree);

extern void RTreeTrivUpdate(RNode node);

extern void RTreeInsert(RTree tree, RNode node);
extern void RTreeDelete(RTree tree, RNode node);
extern Bool RTreeFind(RNode *nodeReturn, RTree tree, Addr addr);

/* TODO: Call different function to avoid checking root again? */
#define RTREE_FIND(nodeReturn, tree, addr) \
  (/* (tree)->root != NULL && */ \
   (tree)->root->base <= (addr) && \
   (addr) < (tree)->root->limit ? \
     (*(nodeReturn) = (tree)->root, TRUE) : \
     RTreeFind(nodeReturn, tree, addr))

extern RNode RTreeFirst(RTree tree);
extern RNode RTreeNext(RTree tree, Addr prev);

#define RTREE_FOR(node, tree, next) \
  for (node = RTreeFirst(tree), next = (node != NULL ? node->base : NULL); \
       node != NULL; \
       node = RTreeNext(tree, next), next = (node != NULL ? node->base : NULL))

typedef Bool (*RTreeIterator)(RTree, RNode, void *, Size);
extern void RTreeIterate(RTree tree, RTreeIterator iterator,
                         void *closureP, Size size);

typedef Bool (*RTreeTestNodeMethod)(RTree tree, RNode node,
                                    void *closureP, Size closureS);
typedef Bool (*RTreeTestTreeMethod)(RTree tree, RNode node,
                                    void *closureP, Size closureS);
typedef void (*RTreeUpdateNodeMethod)(RTree tree, RNode node);

extern Bool RTreeFindFirst(RNode *nodeReturn, RTree rtree,
                           RTreeTestNodeMethod testNode,
                           RTreeTestTreeMethod testTree,
                           void *closureP, Size closureS);

extern Bool RTreeNeighbours(RNode *leftReturn, RNode *rightReturn,
                            RTree tree, Addr addr);

extern Count RTreeDebugSize(RTree tree);


#endif /* rtree_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
