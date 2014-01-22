/* rtree.h: RANGE TREE IMPLEMENTATION
 *
 * $Id$
 * Copyright 2014 Ravenbrook Limited.  See end of file for license.
 */

#include "rtree.h"
#include "mpm.h"

SRCID(rtree, "$Id$");

Bool RTreeCheck(RTree tree)
{
  UNUSED(tree);
  CHECKL(tree != NULL);
  return TRUE;
}

Bool RNodeCheck(RNode node)
{
  UNUSED(node);
  CHECKL(node != NULL);
  if (node->left != NULL)
    CHECKD(RNode, node->left);
  if (node->right != NULL)
    CHECKD(RNode, node->right);
  CHECKL(node->base != NULL);
  CHECKL(node->limit != NULL);
  CHECKL(node->base < node->limit);
  return TRUE;
}


void RTreeInit(RTree tree)
{
  AVER(tree != NULL);
  tree->root = NULL;
  tree->sig = RTreeSig;
  AVERT(RTree, tree);
}

void RTreeFinish(RTree tree)
{
  AVERT(RTree, tree);
  AVER(tree->root == NULL); /* tree should've been emptied */
  tree->sig = SigInvalid;
}


void RNodeInit(RNode node, Addr base, Addr limit)
{
  AVER(node != NULL);
  node->left = NULL;
  node->right = NULL;
  node->base = base;
  node->limit = limit;
  node->sig = RNodeSig;
  AVERT(RNode, node);
}


void RNodeFinish(RNode node)
{
  AVERT(RNode, node);
  /* The node should've been deleted from any tree. */
  AVER(node->left == NULL);
  AVER(node->right == NULL);
  node->sig = SigInvalid;
}


/* descend -- descend tree searching for range, reversing pointers
 *
 * Descend the tree, reversing pointers as we go, until we find
 * a node containing addr or run out of nodes.  If a match was found,
 * return the node with the match, and tree->root points at that node's
 * parent (and reversed tree).  If there was no match, return NULL, and
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
  parent = NULL;
  while (node != NULL && !(node->base <= addr && addr < node->limit)) {
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
  while (parent != NULL) {
    RNode grandparent, ggp;
    if (addr < parent->base) {
      grandparent = parent->left;
      if (grandparent == NULL) { /* zig */
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
      if (grandparent == NULL) { /* zag */
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
    parent = ggp;
  }
  tree->root = node;
}


/* pop -- pop one node from the bottom of an inverted tree
 *
 * This can be used after descend() to get the parent of the found node,
 * or the nearest neighbour of the not-found node.  Returns NULL if there
 * is no such node (because the inverted tree is empty).
 */

static RNode pop(RTree tree, Addr addr)
{
  RNode node, parent;

  AVERT(RTree, tree);
  /* addr is abitrary */

  node = tree->root;
  if (node == NULL)
    return NULL;
  if (addr < node->base) {
    parent = node->left;
    node->left = NULL;
  } else {
    AVER(addr >= node->limit);
    parent = node->right;
    node->right = NULL;
  }
  tree->root = parent;
  return node;
}


/* ascend -- ascend the tree even if node is NULL
 *
 * Like splay() except that ascend(tree, addr, descend(tree, addr))
 * is valid even when addr is not found in the tree.
 */

static void ascend(RTree tree, Addr addr, RNode node)
{
  AVERT(RTree, tree);
  /* addr is arbitrary */
  
  if (node != NULL) {
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
  if (node == NULL)
    return FALSE;
  *nodeReturn = node;
  return TRUE;
}


void RTreeInsert(RTree tree, RNode node)
{
  RNode exists;

  AVERT(RTree, tree);
  AVERT(RNode, node);
  AVER(node->left == NULL);
  AVER(node->right == NULL);
  
  exists = descend(tree, node->base);
  AVER(exists == NULL);
  splay(tree, node->base, node);
}


void RTreeDelete(RTree tree, RNode node)
{
  RNode exists;
  
  AVERT(RTree, tree);
  AVERT(RNode, node);

  exists = descend(tree, node->base);
  AVER(exists == node);

  if (exists == NULL || /* defensive */
      (node->left == NULL && node->right == NULL))
    ascend(tree, node->base, NULL);
  else {
    /* Find a nearest neighbour node in one of node's subtrees. */
    RTreeStruct subtree;
    RNode replacement;
    RTreeInit(&subtree);
    if (node->left != NULL)
      subtree.root = node->left;
    else
      subtree.root = node->right;
    exists = descend(&subtree, node->base);
    AVER(exists == NULL); /* overlapping node in subtree */
    replacement = pop(&subtree, node->base);
    /* Splay the replacement to the top of the subtree. */
    splay(&subtree, node->base, replacement);
    AVER(subtree.root == replacement);
    /* Splay the subtree into the main tree, bypassing node. */
    splay(tree, node->base, subtree.root);
  }

  /* Remove references to tree from node, for safety, and for RNodeFinish. */
  node->left = NULL;
  node->right = NULL;
}


RNode RTreeFirst(RTree tree)
{
  RNode node;

  AVERT(RTree, tree);
  
  /* Splay the least node in the tree to the root. */
  node = descend(tree, (Addr)0);
  ascend(tree, (Addr)0, node);
  AVER(node == NULL || tree->root == node);

  return tree->root;
}

RNode RTreeNext(RTree tree, Addr prev)
{
  RTreeStruct subtree;

  /* The loop is allowed to change the tree, so try to splay
     the current node back to the root.  This might bring either
     the nearest right- or left- neighbour to the top if node
     was deleted.  On the other hand, if the tree wasn't changed then
     it's a trivial operation. */
  ascend(tree, prev, descend(tree, prev));
  
  if (tree->root == NULL) /* nothing left in the tree */
    return NULL;

  if (tree->root->base > prev) /* progress */
    return tree->root;
    
  /* If the node is still at the root, or its nearest left neighbour is
     there, or a similar replacement node is there, then we must search
     for the nearest right neighbour in the right subtree. */
  subtree.root = tree->root->right;
  ascend(&subtree, prev, descend(&subtree, prev));
  AVER(subtree.root == NULL || subtree.root->left == NULL);
  tree->root->right = subtree.root;
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
