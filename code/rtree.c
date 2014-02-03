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
RNodeStruct RTreeLeaf = {SigInvalid, NULL, NULL, NULL, NULL};

#ifdef TREE_DEBUG

static Count debugCountBetween(RNode node, Addr base, Addr limit)
{
  if (node == RTREE_LEAF)
    return 0;
  AVERT(RNode, node);
  AVER(base <= node->base);
  AVER(node->limit <= limit);
  return debugCountBetween(node->left, base, node->base) +
         1 +
         debugCountBetween(node->right, node->limit, limit);
}

static Count debugCount(RNode node)
{
  return debugCountBetween(node, (Addr)0, (Addr)-1);
}

#endif /* TREE_DEBUG */

Bool RTreeCheck(RTree tree)
{
  CHECKS(RTree, tree);
  CHECKL(tree != NULL);
  CHECKL(FUNCHECK(tree->update));
  return TRUE;
}

Bool RNodeCheck(RNode node)
{
  CHECKS(RNode, node);
  CHECKL(node != NULL);
#if RTREE_DEBUG
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
#endif /* RTREE_DEBUG */
  return TRUE;
}


void RTreeInit(RTree tree, RTreeUpdateMethod update)
{
  AVER(tree != NULL);
  AVER(FUNCHECK(update));
  tree->root = RTREE_LEAF;
  tree->update = update;
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
  RTreeInit(tree, tree->update);
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

void RTreeTrivUpdate(RNode node)
{
  UNUSED(node);
  NOOP;
}


/* stepUp/Down/Left/Right etc. -- pointer reversing traversal
 *
 * stepDownLeft and stepDownRight move one step down the tree, storing
 * the path back up in the pointer they traverse.  stepUpLeft and
 * stepUpRight are their opposites, restoring the pointer.
 *
 * stepDown decides which way to step using an address key, and stepUp
 * is its opposite.  You must use the same key, otherwise the wrong
 * pointer may be overwritten.
 *
 * runDownLeft and runDownRight apply stepDownLeft and stepDownRight until
 * there are no more nodes.  They reach the leftmost and rightmost ends of
 * the tree spines.
 *
 * runDown steps down the tree using an address key to decide which way
 * to go, and is the basis of finding a node using an address.
 */


static RNode stepDownLeft(RNode node, RNode *parentIO)
{
  RNode parent = *parentIO;
  RNode child = node->left;
  node->left = parent;
  *parentIO = node;
  return child;
}

static RNode stepDownRight(RNode node, RNode *parentIO)
{
  RNode parent = *parentIO;
  RNode child = node->right;
  node->right = parent;
  *parentIO = node;
  return child;
}

static RNode stepDown(RNode node, RNode *parentIO, Addr addr)
{
  if (addr < node->base)
    return stepDownLeft(node, parentIO);
  else {
    AVER(addr >= node->limit);
    return stepDownRight(node, parentIO);
  }
}

static RNode runDown(RNode node, RNode *parentIO, Addr addr)
{
  while (node != RTREE_LEAF && !RNODE_HAS_ADDR(node, addr))
    node = stepDown(node, parentIO, addr);
  return node;
}

static void runDownLeft(RNode node, RNode *parentIO)
{
  while (node != RTREE_LEAF)
    node = stepDownLeft(node, parentIO);
}

static void runDownRight(RNode node, RNode *parentIO)
{
  while (node != RTREE_LEAF)
    node = stepDownRight(node, parentIO);
}

static RNode stepUpRight(RNode node, RNode *parentIO)
{
  RNode parent = *parentIO, grandparent;
  grandparent = parent->left;
  parent->left = node;
  *parentIO = grandparent;
  return parent;
}

static RNode stepUpLeft(RNode node, RNode *parentIO)
{
  RNode parent = *parentIO, grandparent;
  grandparent = parent->right;
  parent->right = node;
  *parentIO = grandparent;
  return parent;
}
  
static RNode stepUp(RNode node, RNode *parentIO, Addr addr)
{
  RNode parent = *parentIO;
  if (addr < parent->base)
    return stepUpRight(node, parentIO);
  else {
    AVER(addr >= parent->limit);
    return stepUpLeft(node, parentIO);
  }
}

#if 0
static RNode runUpRight(RNode node, RNode parent)
{
  while (parent != RTREE_LEAF)
    node = stepUpRight(node, &parent);
  return node;
}
#endif

static RNode runUpRightUpdate(RNode node, RNode parent,
                              RTreeUpdateMethod update)
{
  while (parent != RTREE_LEAF) {
    node = stepUpRight(node, &parent);
    update(node);
  }
  return node;
}

#if 0
static RNode runUpLeft(RNode node, RNode parent)
{
  while (parent != RTREE_LEAF)
    node = stepUpLeft(node, &parent);
  return node;
}
#endif

static RNode runUpLeftUpdate(RNode node, RNode parent,
                             RTreeUpdateMethod update)
{
  while (parent != RTREE_LEAF) {
    node = stepUpLeft(node, &parent);
    update(node);
  }
  return node;
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
 *
 * descendLeft and descendRight are faster versions that run down
 * the left or right spines of the tree without comparing keys.
 */

static RNode descend(RTree tree, Addr addr)
{
  RNode parent = RTREE_LEAF;
  RNode node;
  AVERT(RTree, tree);
  node = runDown(tree->root, &parent, addr);
  tree->root = parent;
  return node;
}

static void descendLeft(RTree tree)
{
  RNode parent = RTREE_LEAF;
  AVERT(RTree, tree);
  runDownLeft(tree->root, &parent);
  tree->root = parent;
}

static void descendRight(RTree tree)
{
  RNode parent = RTREE_LEAF;
  AVERT(RTree, tree);
  runDownRight(tree->root, &parent);
  tree->root = parent;
}


/* splay -- ascend and balance tree, restoring reversed pointers
 *
 * Ascends a tree with pointers reversed by descend(), rebalancing it using
 * splay tree rotations, and bringing node to the root of the tree.
 * So splayUp(tree, addr, descend(tree, addr)) puts the node containing addr
 * at the root.
 *
 * A splay should always follow a descend, partly because it's necessary
 * to fix the pointers, but also because it reduces the length of the
 * descended path by half, meaning that repeating whatever caused the
 * descent can't have worse than O(n log n) complexity.
 */

static void zig(RNode node, RNode parent)
{
  parent->left = node->right;
  node->right = parent;
}

static void zigUpdate(RNode node, RNode parent, RTreeUpdateMethod update)
{
  parent->left = node->right;
  node->right = parent;
  update(parent);
}

static void zag(RNode node, RNode parent)
{
  parent->right = node->left;
  node->left = parent;
}

static void zagUpdate(RNode node, RNode parent, RTreeUpdateMethod update)
{
  parent->right = node->left;
  node->left = parent;
  update(parent);
}

static RNode zigzig(RNode node, RNode parent, RNode grandparent)
{
  RNode ggp = grandparent->left;
  grandparent->left = parent->right;
  parent->right = grandparent;
  parent->left = node->right;
  node->right = parent;
  return ggp;
}

static RNode zagzag(RNode node, RNode parent, RNode grandparent)
{
  RNode ggp = grandparent->right;
  grandparent->right = parent->left;
  parent->left = grandparent;
  parent->right = node->left;
  node->left = parent;
  return ggp;
}

static RNode zigzag(RNode node, RNode parent, RNode grandparent)
{
  RNode ggp = grandparent->right;
  parent->left = node->right;
  grandparent->right = node->left;
  node->left = grandparent;
  node->right = parent;
  return ggp;
}

static RNode zagzig(RNode node, RNode parent, RNode grandparent)
{
  RNode ggp = grandparent->left;
  parent->right = node->left;
  grandparent->left = node->right;
  node->right = grandparent;
  node->left = parent;
  return ggp;
}

static void update(RTree tree, RNode node)
{
  if (tree->update != RTreeTrivUpdate)
    (*tree->update)(node);
}

static void splayUp(RTree tree, Addr addr, RNode node) {
  RNode parent;

  AVERT(RTree, tree);
  AVERT(RNode, node);
  
  parent = tree->root;
  
  while (parent != RTREE_LEAF) {
    RNode grandparent, ggp;
    if (addr < parent->base) {
      grandparent = parent->left;
      if (grandparent == RTREE_LEAF) {
        zig(node, parent);
        update(tree, parent);
        break;
      }
      if (addr < grandparent->base) {
        ggp = zigzig(node, parent, grandparent);
      } else {
        AVER(addr >= grandparent->limit);
        ggp = zigzag(node, parent, grandparent);
      }
    } else {
      AVER(addr >= parent->limit);
      grandparent = parent->right;
      if (grandparent == RTREE_LEAF) {
        zag(node, parent);
        update(tree, parent);
        break;
      }
      if (addr < grandparent->base) {
        ggp = zagzig(node, parent, grandparent);
      } else {
        AVER(addr >= grandparent->limit);
        ggp = zagzag(node, parent, grandparent);
      }
    }
    
    /* Grandparent may be below parent, but never above, so this ordering
       will propagate the update from grandparent to parent correctly. */
    update(tree, grandparent);
    update(tree, parent);
    
#if RTREE_DEBUG
    AVER(RNodeCheck(node));
    AVER(RNodeCheck(parent));
    AVER(RNodeCheck(grandparent));
#endif

    parent = ggp;
  }

  update(tree, node);
  tree->root = node;

#ifdef RTREE_DEBUG
  (void)debugCount(node);
#endif
}


/* splayDownUpdate -- search and splay with update
 *
 * Search for a node containing addr.  Returns TRUE iff the a containing
 * node was found, and moves that node to the root.  Otherwise moves a
 * neighbouring node to root.  Nodes whose children have been changed are
 * updated towards the root.
 *
 * Splays and dividing the tree into two pointer-reversed subtrees on
 * the way down, then updates the spines of the two subtrees while
 * correcting their pointers.
 */

static Bool splayDownUpdate(RTree tree, Addr addr, RTreeUpdateMethod update)
{
  RNode node;
  RNode right = RTREE_LEAF; /* reversed subtree all preceeding addr */
  RNode left = RTREE_LEAF;  /* reversed subtree all succeeding addr */
  Bool found = FALSE;
#ifdef RTREE_DEBUG
  Count count = debugCount(tree->root);
#endif

  AVERT(RTree, tree);
  AVER(FUNCHECK(update));
  
  if (tree->root == RTREE_LEAF)
    return FALSE;
  
  node = tree->root;
  for (;;) {
    if (addr < node->base) {
      RNode child = node->left;
      if (child == RTREE_LEAF)
        break;
      else if (addr < child->base) {
        RNode grandchild = child->left;
        if (grandchild == RTREE_LEAF) {
          node = stepDownLeft(node, &right);
          break;
        }
        zigUpdate(child, node, update);
        node = stepDownLeft(child, &right);
      } else if (addr < child->limit) {
        node = stepDownLeft(node, &right);
        found = TRUE;
        break;
      } else { /* addr >= child->limit */
        node = stepDownLeft(node, &right);
        if (node->right == RTREE_LEAF)
          break;
         node = stepDownRight(node, &left);
      }
    } else if (addr < node->limit) {
      found = TRUE;
      break;
    } else { /* addr >= node->limit */
      RNode child = node->right;
      if (child == RTREE_LEAF)
        break;
      else if (addr < child->base) {
        node = stepDownRight(node, &left);
        if (node->left == RTREE_LEAF)
          break;
        node = stepDownLeft(node, &right);
      } else if (addr < child->limit) {
        node = stepDownRight(node, &left);
        found = TRUE;
        break;
      } else { /* addr >= child->limit */
        RNode grandchild = child->right;
        if (grandchild == RTREE_LEAF) {
          node = stepDownRight(node, &left);
          break;
        }
        zagUpdate(child, node, update);
        node = stepDownRight(child, &left);
      }
    }
  }
  
  node->right = runUpRightUpdate(node->right, right, update);
  node->left = runUpLeftUpdate(node->left, left, update);
  update(node);
  tree->root = node;

#ifdef RTREE_DEBUG
  AVER(debugCount(node) == count);
#endif

  return found;
}


/* splayDownFast -- search and splay without update
 *
 * Search for a node containing addr.  Returns TRUE iff the a containing
 * node was found, and moves that node to the root.  Otherwise moves a
 * neighbouring node to root.
 *
 * Splays and dividing the tree into two subtrees on the way down.
 */

static Bool splayDownFast(RTree tree, Addr addr)
{
  RNode node = tree->root;
  RNode left = RTREE_LEAF, *leftLink = &left;
  RNode right = RTREE_LEAF, *rightLink = &right;
  Bool found = FALSE;
#ifdef RTREE_DEBUG
  Count count = debugCount(node);
#endif
  
  if (node == RTREE_LEAF)
    return FALSE;
  
  for (;;) {
    if (addr < node->base) {
      RNode child = node->left;
      if (child == RTREE_LEAF)
        break;
      else if (addr < child->base) {
        if (child->left == RTREE_LEAF) {
          *rightLink = node;
          rightLink = &node->left;
          node = child;
          break;
        }
        node->left = child->right;
        child->right = node;
        *rightLink = child;
        rightLink = &child->left;
        node = child->left;
      } else if (addr < child->limit) {
        *rightLink = node;
        rightLink = &node->left;
        node = child;
        found = TRUE;
        break;
      } else { /* addr >= child->limit */
        *rightLink = node;
        rightLink = &node->left;
        node = child;
        if (node->right == RTREE_LEAF)
          break;
        *leftLink = node;
        leftLink = &node->right;
        node = node->right;
      }
    } else if (addr < node->limit) {
      found = TRUE;
      break;
    } else { /* addr >= node->limit */
      RNode child = node->right;
      if (child == RTREE_LEAF)
        break;
      else if (addr < child->base) {
        *leftLink = node;
        leftLink = &node->right;
        node = child;
        if (node->left == RTREE_LEAF)
          break;
        *rightLink = node;
        rightLink = &node->left;
        node = node->left;
      } else if (addr < child->limit) {
        *leftLink = node;
        leftLink = &node->right;
        node = child;
        found = TRUE;
        break;
      } else { /* addr >= child->limit */
        if (child->right == RTREE_LEAF) {
          *leftLink = node;
          leftLink = &node->right;
          node = child;
          break;
        }
        node->right = child->left;
        child->left = node;
        *leftLink = child;
        leftLink = &child->right;
        node = child->right;
      }
    }
  }
  
  *leftLink = node->left;
  *rightLink = node->right;
  node->left = left;
  node->right = right;
  tree->root = node;

#ifdef RTREE_DEBUG
  AVER(debugCount(node) == count);
#endif

  return found;
}

static Bool splayDown(RTree tree, Addr addr)
{
  AVERT(RTree, tree);
  if (tree->update == RTreeTrivUpdate)
    return splayDownFast(tree, addr);
  else
    return splayDownUpdate(tree, addr, tree->update);
}


/* splayLeft, splayRight -- ascend and balance after descendRight, descendLeft
 *
 * After applying descendRight you need not (and may not be able to) use a
 * key to decide how to ascend, but instead can ascend straight up the right
 * spine with splayLeft.  This is efficient because you are ascending in a
 * straight line and can know that you only need zags.
 *
 * Mutatis mutandis splayRight.
 */

static void splayLeft(RTree tree, RNode node) {
  RNode parent;

  AVERT(RTree, tree);
  AVERT(RNode, node);

  parent = tree->root;
  while (parent != RTREE_LEAF) {
    RNode grandparent = parent->right, ggp;

    if (grandparent == RTREE_LEAF) {
      zag(node, parent);
      update(tree, parent);
      break;
    }
    ggp = zagzag(node, parent, grandparent);

    update(tree, grandparent);
    update(tree, parent);

#if 0    
    AVER(RNodeCheck(node));
    AVER(RNodeCheck(parent));
    AVER(RNodeCheck(grandparent));
#endif

    parent = ggp;
  }
  update(tree, node);
  tree->root = node;
}

static void splayRight(RTree tree, RNode node) {
  RNode parent;

  AVERT(RTree, tree);
  AVERT(RNode, node);

  parent = tree->root;
  while (parent != RTREE_LEAF) {
    RNode grandparent = parent->left, ggp;

    if (grandparent == RTREE_LEAF) {
      zig(node, parent);
      update(tree, parent);
      break;
    }
    ggp = zigzig(node, parent, grandparent);

    update(tree, grandparent);
    update(tree, parent);
    
#if 0
    AVER(RNodeCheck(node));
    AVER(RNodeCheck(parent));
    AVER(RNodeCheck(grandparent));
#endif

    parent = ggp;
  }
  update(tree, node);
  tree->root = node;
}


/* ascend -- ascend the tree even if node is RTREE_LEAF
 *
 * Like splayUp() except that ascend(tree, addr, descend(tree, addr))
 * is valid even when addr is not found in the tree.
 *
 * "Ascend" is both transitive and intransitive.  It ascends the tree, and
 * the node ascends to the top.
 */

static void ascend(RTree tree, Addr addr, RNode node)
{
  AVERT(RTree, tree);
  
  /* addr is arbitrary */

  /* If we're given a node, splay it to the root. */
  if (node != RTREE_LEAF) {
    AVERT(RNode, node);
    splayUp(tree, addr, node);
    return;
  }

  /* Otherwise, if there's another node nearby, splay that to the top,
     just to help balance the tree. */
  if (tree->root != RTREE_LEAF) {
    node = stepUp(node, &tree->root, addr);
    splayUp(tree, addr, node);
  }
}


/* ascendLeft, ascendRight -- ascend the tree after a descendRight, descendLeft
 *
 * ascendLeft is to splayLeft as ascend is to splay.  And so on.
 */

static void ascendLeft(RTree tree, RNode node)
{
  AVERT(RTree, tree);
  /* addr is arbitrary */

  /* If we're given a node, splay it to the root. */
  if (node != RTREE_LEAF) {
    AVERT(RNode, node);
    splayLeft(tree, node);
    return;
  }

  /* Otherwise, if there's another node nearby, splay that to the top,
     just to help balance the tree. */
  if (tree->root != RTREE_LEAF) {
    node = stepUpLeft(node, &tree->root);
    splayLeft(tree, node);
  }
}

static void ascendRight(RTree tree, RNode node)
{
  AVERT(RTree, tree);
  /* addr is arbitrary */

  /* If we're given a node, splay it to the root. */
  if (node != RTREE_LEAF) {
    AVERT(RNode, node);
    splayRight(tree, node);
    return;
  }

  /* Otherwise, if there's another node nearby, splay that to the top,
     just to help balance the tree. */
  if (tree->root != RTREE_LEAF) {
    node = stepUpRight(node, &tree->root);
    splayRight(tree, node);
  }
}


Bool RTreeFind(RNode *nodeReturn, RTree tree, Addr addr)
{
  AVER(nodeReturn != NULL);
  AVERT(RTree, tree);
  /* addr is arbitrary */

  if (RNODE_HAS_ADDR(tree->root, addr)) {
    *nodeReturn = tree->root;
    return TRUE;
  }
  
  if (!splayDown(tree, addr))
    return FALSE;

  *nodeReturn = tree->root;
  return TRUE;
}
/*
  node = descend(tree, addr);
  ascend(tree, addr, node);
  
  if (node == RTREE_LEAF)
    return FALSE;

  *nodeReturn = node;
  return TRUE;
}
*/


void RTreeInsert(RTree tree, RNode node)
{
  RNode exists;

  AVERT(RTree, tree);
  AVERT(RNode, node);
  AVER(node->left == RTREE_LEAF);
  AVER(node->right == RTREE_LEAF);

  exists = descend(tree, node->base);
  AVER(exists == RTREE_LEAF); /* TODO: what defensive action can we take? */
  splayUp(tree, node->base, node);
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
    RTreeInit(&subtree, tree->update);

    if (node->left != RTREE_LEAF) {
      subtree.root = node->left;
      descendRight(&subtree);
      replacement = stepUpLeft(RTREE_LEAF, &subtree.root);
      AVER(replacement->right == RTREE_LEAF);
      
      /* Bypass the replacement node in the subtree. */
      ascendLeft(&subtree, replacement->left);
      
      /* Replace the node to be deleted with the replacement. */
      replacement->left = subtree.root;
      replacement->right = node->right;
      
      /* Splay the replacement into the inverted main tree, bypassing
         the node to be deleted. */
      splayUp(tree, node->base, replacement);
    } else {
      /* Same as above, with right and left swapped! */
      subtree.root = node->right;
      descendLeft(&subtree);
      replacement = stepUpRight(RTREE_LEAF, &subtree.root);
      AVER(replacement->left == RTREE_LEAF);
      
      /* Bypass the replacement node in the subtree. */
      ascendRight(&subtree, replacement->right);
      
      /* Replace the node to be deleted with the replacement. */
      replacement->right = subtree.root;
      replacement->left = node->left;
      
      /* Splay the replacement into the inverted main tree, bypassing
         the node to be deleted. */
      splayUp(tree, node->base, replacement);
    }
    
    /* Possible improvement: Detach the replacement node and push it
       into the main inverted tree, replacing node, then attach the
       subtree root to the main inverted tree, then splay replacement's
       child right to the top of the whole tree.  This avoids a possible
       missed rotation at the junction of the subtree and the main tree.
       Be careful: the key on the way up will need to be replacement->base. */
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
  descendLeft(tree);
  ascendRight(tree, RTREE_LEAF);
  if (tree->root == RTREE_LEAF)
    return NULL;
  return tree->root;
}

RNode RTreeNext(RTree tree, Addr prev)
{
  RTreeStruct subtree;
  
  AVERT_CRITICAL(RTree, tree);

  if (tree->root == RTREE_LEAF) /* nothing left in the tree */
    return NULL;

  /* The loop is allowed to change the tree, so try to splay
     the current node back to the root.  If the node was deleted
     or replaced this could bring a neighbour or a new node with the same
     base address to the top.  If the tree wasn't changed then
     it's a trivial operation. */
  if (!RNODE_HAS_ADDR(tree->root, prev))
    ascend(tree, prev, descend(tree, prev));
  
  if (tree->root->base > prev) /* ensure monotonic progress */
    return tree->root;

  if (tree->root->right == RTREE_LEAF) /* no more nodes */
    return NULL;
  
  /* The previous node is still at the root, or its nearest left neighbour is
     there, or a similar replacement node is there, so we must search
     for the nearest right neighbour in the right subtree. */
  RTreeInit(&subtree, tree->update);
  subtree.root = tree->root->right;
  descendLeft(&subtree);
  ascendRight(&subtree, RTREE_LEAF);
  /* Rotate successor to top. */
  AVER(subtree.root->left == RTREE_LEAF);
  tree->root->right = RTREE_LEAF;
  subtree.root->left = tree->root;
  tree->root = subtree.root;
  return tree->root;
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
  if (node != RTREE_LEAF) { /* found addr in tree */
    splayUp(tree, addr, node);
    return FALSE;
  }

  if (tree->root == RTREE_LEAF) {
    /* No need to ascend an empty tree. */
    *leftReturn = *rightReturn = NULL;
    return TRUE;
  }

  ascend(tree, addr, node);
  node = tree->root; /* a neighbour of addr */

  RTreeInit(&subtree, tree->update);
  if (addr < node->base) { /* node is the right neighbour */
    *rightReturn = node;
    if (node->left == RTREE_LEAF)
      *leftReturn = NULL;
    else {
      RNode left;
      subtree.root = node->left;
      descendRight(&subtree);
      left = stepUpLeft(RTREE_LEAF, &subtree.root);
      splayLeft(&subtree, left);
      node->left = left;
      *leftReturn = left;
    }
  } else {
    /* Same as above with left and right swapped! */
    AVER(addr >= node->limit);
    *leftReturn = node;
    if (node->right == RTREE_LEAF)
      *rightReturn = NULL;
    else {
      RNode right;
      subtree.root = node->right;
      descendLeft(&subtree);
      right = stepUpRight(RTREE_LEAF, &subtree.root);
      splayRight(&subtree, right);
      node->right = right;
      *rightReturn = right;
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
