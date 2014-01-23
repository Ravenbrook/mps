/* rtree.h: RANGE TREE IMPLEMENTATION
 *
 * $Id$
 * Copyright 2014 Ravenbrook Limited.  See end of file for license.
 */

#include "rtree.h"
#include "mpm.h"

SRCID(rtree, "$Id$");


/* TODO: Use this throughout the MPS */
/* Could use (Word)(addr - base) < (Word)(limit - base) */
#define ADDR_IN_AREA(base, limit, addr) ((base) <= (addr) && (addr) < (limit))
#define RNODE_HAS_ADDR(node, addr) ADDR_IN_AREA((node)->base, (node)->limit, addr)


/* TODO: Consider using a sentinel node rather than NULL */
static RNodeStruct RTreeLeaf = {SigInvalid, NULL, NULL, NULL, NULL};
#define RTREE_LEAF (&RTreeLeaf)

Bool RTreeCheck(RTree tree)
{
  CHECKS(RTree, tree);
  CHECKL(tree != NULL);
  return TRUE;
}

Bool RNodeCheck(RNode node)
{
  CHECKS(RNode, node);
  CHECKL(node != NULL);
  if (node->left != RTREE_LEAF) {
    CHECKD(RNode, node->left);
    AVER(node->left->limit <= node->base);
  }
  if (node->right != RTREE_LEAF) {
    CHECKD(RNode, node->right);
    AVER(node->limit <= node->right->base);
  }
  CHECKL(node->base != NULL);
  CHECKL(node->limit != NULL);
  CHECKL(node->base < node->limit);
  return TRUE;
}


void RTreeInit(RTree tree)
{
  AVER(tree != NULL);
  tree->root = RTREE_LEAF;
  tree->sig = RTreeSig;
  AVERT(RTree, tree);
}

void RTreeFinish(RTree tree)
{
  AVERT(RTree, tree);
  AVER(tree->root == RTREE_LEAF); /* tree should've been emptied */
  tree->sig = SigInvalid;
}

void RTreeReset(RTree tree)
{
  AVERT(RTree, tree);
  RTreeInit(tree);
}

void RNodeInit(RNode node, Addr base, Addr limit)
{
  AVER(node != NULL);
  AVER(node != RTREE_LEAF);
  node->left = RTREE_LEAF;
  node->right = RTREE_LEAF;
  node->base = base;
  node->limit = limit;
  node->sig = RNodeSig;
  AVERT(RNode, node);
}

void RNodeFinish(RNode node)
{
  AVER(node != RTREE_LEAF);
  AVERT(RNode, node);
  /* The node should've been deleted from any tree. */
  AVER(node->left == RTREE_LEAF);
  AVER(node->right == RTREE_LEAF);
  node->sig = SigInvalid;
}


/* descend -- descend tree searching for range, reversing pointers
 *
 * Descend the tree, reversing pointers as we go, until we find
 * a node containing addr or run out of nodes.  If a match was found,
 * return the node with the match, and tree->root points at that node's
 * parent (and reversed tree).  If there was no match, return RTREE_LEAF, and
 * tree->root points at one of key's nearest neighbours (and a reversed
 * tree).
 *
 * We use pointer reversal rather than recursion because the MPS must have
 * a bounded stack size and so may not use O(n) stack frames.
 */

static RNode descend(RTree tree, Addr addr)
{
  RNode node, parent;
  
  AVERT(RTree, tree);
  /* addr is arbitrary */
  
  node = tree->root;
  parent = RTREE_LEAF;
  while (node != RTREE_LEAF && !RNODE_HAS_ADDR(node, addr)) {
    RNode child;
    if (addr < node->base) {
      child = node->left;
      node->left = parent;
    } else {
      AVER(addr >= node->limit);
      child = node->right;
      node->right = parent;
    }
    parent = node;
    node = child;
  }
  tree->root = parent;
  return node;
}


/* splay -- ascend and balance tree, restoring reversed pointers
 *
 * Ascends a tree with pointers reversed by descend(), rebalancing it using
 * splay tree rotations, and bringing node to the root of the tree.
 * So splay(tree, addr, descend(tree, addr)) puts the node containing addr
 * at the root.
 *
 * A splay should always follow a descend, partly because it's necessary
 * to fix the pointers, but also because it reduces the length of the
 * descended path by half, meaning that repeating whatever caused the
 * descent can't have worse than O(n log n) complexity.
 */

static void splay(RTree tree, Addr addr, RNode node) {
  RNode parent;

  AVERT(RTree, tree);
  AVERT(RNode, node);

  parent = tree->root;
  while (parent != RTREE_LEAF) {
    RNode grandparent, ggp;
    if (addr < parent->base) {
      grandparent = parent->left;
      if (grandparent == RTREE_LEAF) { /* zig */
        parent->left = node->right;
        node->right = parent;
        break;
      }
      if (addr < grandparent->base) { /* zig-zig */
        ggp = grandparent->left;
        grandparent->left = parent->right;
        parent->right = grandparent;
        parent->left = node->right;
        node->right = parent;
      } else { /* zig-zag */
        AVER(addr >= grandparent->limit);
        ggp = grandparent->right;
        parent->left = node->right;
        grandparent->right = node->left;
        node->left = grandparent;
        node->right = parent;
      }
    } else { /* same as above with "right" and "left" swapped! */
      AVER(addr >= parent->limit);
      grandparent = parent->right;
      if (grandparent == RTREE_LEAF) { /* zag */
        parent->right = node->left;
        node->left = parent;
        break;
      }
      if (addr < grandparent->base) { /* zag-zig */
        ggp = grandparent->left;
        parent->right = node->left;
        grandparent->left = node->right;
        node->right = grandparent;
        node->left = parent;
      } else { /* zag-zag */
        AVER(addr >= grandparent->limit);
        ggp = grandparent->right;
        grandparent->right = parent->left;
        parent->left = grandparent;
        parent->right = node->left;
        node->left = parent;
      }
    }
    
    AVER(RNodeCheck(node));
    AVER(RNodeCheck(parent));
    AVER(RNodeCheck(grandparent));

    parent = ggp;
  }
  tree->root = node;
}


/* pop -- pop one node from the bottom of an inverted tree
 *
 * This can be used after descend() to get the parent of the found node,
 * or the nearest neighbour of the not-found node.  Returns NULL if there
 * is no such node (because the inverted tree is empty).  Essentially,
 * this undoes one level of tree inversion.
 */

static RNode pop(RTree tree, Addr addr)
{
  RNode node, parent;

  AVERT(RTree, tree);
  /* addr is abitrary */

  node = tree->root;
  if (node == RTREE_LEAF)
    return NULL;
  if (addr < node->base) {
    parent = node->left;
    node->left = RTREE_LEAF;
  } else {
    AVER(addr >= node->limit);
    parent = node->right;
    node->right = RTREE_LEAF;
  }
  tree->root = parent;
  return node;
}


/* ascend -- ascend the tree even if node is RTREE_LEAF
 *
 * Like splay() except that ascend(tree, addr, descend(tree, addr))
 * is valid even when addr is not found in the tree.
 *
 * "Ascend" is both transitive and intransitive.  It ascends the tree, and
 * the node ascends to the top.
 */

static void ascend(RTree tree, Addr addr, RNode node)
{
  AVERT(RTree, tree);
  /* addr is arbitrary */
  
  if (node != RTREE_LEAF) {
    AVERT(RNode, node);
    splay(tree, addr, node);
  } else {
    node = pop(tree, addr);
    if (node != NULL)
      splay(tree, addr, node);
  }
}


Bool RTreeFind(RNode *nodeReturn, RTree tree, Addr addr)
{
  RNode node;

  AVER(nodeReturn != NULL);
  AVERT(RTree, tree);
  /* addr is arbitrary */

  node = descend(tree, addr);
  ascend(tree, addr, node);
  
  if (node == RTREE_LEAF)
    return FALSE;

  *nodeReturn = node;
  return TRUE;
}


void RTreeInsert(RTree tree, RNode node)
{
  RNode exists;

  AVERT(RTree, tree);
  AVERT(RNode, node);
  AVER(node->left == RTREE_LEAF);
  AVER(node->right == RTREE_LEAF);

  exists = descend(tree, node->base);
  AVER(exists == RTREE_LEAF); /* TODO: what defensive action can we take? */
  splay(tree, node->base, node);
}


void RTreeDelete(RTree tree, RNode node)
{
  RNode exists;
  
  AVERT(RTree, tree);
  AVERT(RNode, node);

  exists = descend(tree, node->base);
  AVER(exists == node);

  if (exists == RTREE_LEAF || /* defensive */
      (node->left == RTREE_LEAF && node->right == RTREE_LEAF)) {
    /* Ascend without the node, removing it from the tree. */
    ascend(tree, node->base, RTREE_LEAF);
  } else {
    /* Find a nearest neighbour node in one of node's subtrees. */
    RTreeStruct subtree;
    RNode replacement;
    RTreeInit(&subtree);

    if (node->left != RTREE_LEAF) {
      subtree.root = node->left;
      exists = descend(&subtree, node->base);
      AVER(exists == RTREE_LEAF); /* overlapping node in subtree */
      replacement = pop(&subtree, node->base);
      AVER(replacement->right == RTREE_LEAF);
      
      /* Bypass the replacement node in the subtree. */
      ascend(&subtree, node->base, replacement->left);
      
      /* Replace the node to be deleted with the replacement. */
      replacement->left = subtree.root;
      replacement->right = node->right;
      
      /* Splay the replacement into the inverted main tree, bypassing
         the node to be deleted. */
      splay(tree, node->base, replacement);
    } else {
      /* Same as above, with right and left swapped! */
      subtree.root = node->right;
      exists = descend(&subtree, node->base);
      AVER(exists == RTREE_LEAF); /* overlapping node in subtree */
      replacement = pop(&subtree, node->base);
      AVER(replacement->left == RTREE_LEAF);
      
      /* Bypass the replacement node in the subtree. */
      ascend(&subtree, node->base, replacement->right);
      
      /* Replace the node to be deleted with the replacement. */
      replacement->right = subtree.root;
      replacement->left = node->left;
      
      /* Splay the replacement into the inverted main tree, bypassing
         the node to be deleted. */
      splay(tree, node->base, replacement);
    }
    
    /* Possible improvement: Detach the replacement node and push it
       into the main inverted tree, replacing node, then attach the
       subtree root to the main inverted tree, then splay replacement's
       child right to the top of the whole tree.  This avoids a possible
       missed rotation at the junction of the subtree and the main tree.
       Be careful: the key on the way up will need to be replacement->base. */

    /* Other possible improvement: The replacement is always the left- or
       right-most descendent of the node's child, so there's no need to
       do pointer comparisons to find it, though you should still splay
       on the way back up. */
  }

  /* Remove references to tree from node, for safety, and for RNodeFinish. */
  node->left = RTREE_LEAF;
  node->right = RTREE_LEAF;
}


/* RTreeFirst, RTreeNext -- tree traversal
 *
 * It may at first look seem horribly inefficient to traverse a tree by
 * splaying each node to the root, but it's known to be O(n log log n)
 * and conjectured to be O(n) <http://www.maths.tcd.ie/report_series/tcdmath/tcdm0207.pdf>.
 * However, it may be worth having an alternative implementation when the
 * tree is small to reduce overheads.
 */

RNode RTreeFirst(RTree tree)
{
  AVERT(RTree, tree);
  
  /* Splay the least node in the tree to the root. */
  /* TODO: This doesn't require a descend that uses comparisons, since
     it always goes left. */
  ascend(tree, (Addr)0, descend(tree, (Addr)0));
  if (tree->root == RTREE_LEAF)
    return NULL;
  return tree->root;
}

RNode RTreeNext(RTree tree, Addr prev)
{
  RTreeStruct subtree;

  /* The loop is allowed to change the tree, so try to splay
     the current node back to the root.  If the node was deleted
     or replaced this could bring a neighbour or a new node with the same
     base address to the top.  If the tree wasn't changed then
     it's a trivial operation. */
  ascend(tree, prev, descend(tree, prev));
  
  if (tree->root == RTREE_LEAF) /* nothing left in the tree */
    return NULL;

  if (tree->root->base > prev) /* ensure monotonic progress */
    return tree->root;
    
  /* If the node is still at the root, or its nearest left neighbour is
     there, or a similar replacement node is there, then we must search
     for the nearest right neighbour in the right subtree. */
  RTreeInit(&subtree);
  subtree.root = tree->root->right;
  /* TODO: This doesn't require a descend that uses comparisons, since
     it always goes left. */
  ascend(&subtree, prev, descend(&subtree, prev));
  tree->root->right = subtree.root;
  /* Either there was no next node, or the next node had better not have
     any left children, or it wasn't the next node. */
  if (tree->root->right == RTREE_LEAF)
    return NULL;
  AVER(tree->root->right->left == RTREE_LEAF);
  /* Could do one extra rotation to bring this to the top. */
  return tree->root->right;
}


void RTreeIterate(RTree tree, RTreeIterator iterator,
                  void *closureP, Size size)
{
  RNode node;
  Addr next;

  AVERT(RTree, tree);
  AVER(FUNCHECK(iterator));
  /* closureP and size arbitrary */
  
  RTREE_FOR(node, tree, next) {
    if (!(*iterator)(tree, node, closureP, size))
      break;
  }
}


Bool RTreeFindFirst(RNode *nodeReturn, RTree tree,
                    RTreeTestNodeMethod testNode,
                    RTreeTestTreeMethod testTree,
                    void *closureP, Size closureS)
{
  RNode node;
  Addr next;

  AVER(nodeReturn != NULL);
  AVERT(RTree, tree);
  AVER(FUNCHECK(testNode));
  AVER(FUNCHECK(testTree));
  /* closureP and closureS arbitrary */

  /* FIXME: Temporary O(n) implementation! */
  RTREE_FOR(node, tree, next) {
    if ((*testNode)(tree, node, closureP, closureS)) {
      *nodeReturn = node;
      return TRUE;
    }
  }
  return FALSE;
}


Bool RTreeNeighbours(RNode *leftReturn, RNode *rightReturn,
                     RTree tree, Addr addr)
{
  RNode node;
  RTreeStruct subtree;
  
  AVERT(RTree, tree);
  AVER(leftReturn != NULL);
  AVER(rightReturn != NULL);
  /* addr arbitrary */
  
  node = descend(tree, addr);
  if (node != RTREE_LEAF) {
    ascend(tree, addr, node);
    return FALSE;
  }

  node = pop(tree, addr);
  if (node == NULL) {
    AVER(tree->root == RTREE_LEAF);
    /* No need to ascend an empty tree. */
    *leftReturn = *rightReturn = NULL;
    return TRUE;
  }
  
  splay(tree, addr, node);

  RTreeInit(&subtree);
  if (addr < node->base) { /* node is the right neighbour */
    RNode left;
    *rightReturn = node;
    subtree.root = node->left;
    left = descend(&subtree, addr); /* TODO: Use faster descend_right */
    AVER(left == RTREE_LEAF);
    left = pop(&subtree, addr);
    *leftReturn = left;
    if (left != NULL) {
      splay(&subtree, addr, left);
      node->left = left;
    }
  } else {
    RNode right;
    AVER(addr >= node->limit);
    *leftReturn = node;
    subtree.root = node->right;
    right = descend(&subtree, addr); /* TODO: Use faster descend_left */
    AVER(right == RTREE_LEAF);
    right = pop(&subtree, addr);
    *rightReturn = right;
    if (right != NULL) {
      splay(&subtree, addr, right);
      node->right = right;
    }
  }

  return TRUE;
}


static Count RNodeDebugSize(RNode node)
{
  if (node == RTREE_LEAF)
    return 0;
  AVERT(RNode, node);
  return 1 + RNodeDebugSize(node->left) + RNodeDebugSize(node->right);
}

Count RTreeDebugSize(RTree tree)
{
  AVERT(RTree, tree);
  return RNodeDebugSize(tree->root);
}


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
