/* impl.c.splay: SPLAY TREE IMPLEMENTATION
 *
 * $HopeName: MMsrc!splay.c(MMdevel_gavinm_splay.1) $
 * Copyright (C) 1998 Harlequin Group plc, all rights reserved.
 *
 * .readership: Any MPS developer.
 *
 * .intro: This is a portable implementation of splay trees.
 *
 * .purpose: Splay trees are used to manage potentially unbounded 
 * collections of ordered things.
 *
 * .sources: design.mps.splay,
 */


#include "mpm.h"


SRCID(splay, "$HopeName: MMsrc!splay.c(MMdevel_gavinm_splay.1) $");

/* Basic getter and setter methods */
#define SplayNodeLeft(node) ((node)->left)
#define SplayNodeSetLeft(node, child) ((void)((node)->left = (child)))
#define SplayNodeRight(node) ((node)->right)
#define SplayNodeSetRight(node, child) ((void)((node)->right = (child)))

#define SplayCompare(root, key, node) \
  (((root)->compare)((key), (node)))

Bool SplayRootCheck(SplayRoot root) {
  CHECKL(root != NULL);
  CHECKL(FUNCHECK(root->compare));
  return TRUE;
}

Bool SplayNodeCheck(SplayNode node) {
  CHECKL(node != NULL);
  return TRUE;
}

void SplayRootInit(SplayRoot root, SplayCompareMethod compare) {
  AVER(root != NULL);
  AVER(FUNCHECK(compare));

  root->compare = compare;
  root->root = NULL;

  AVERT(SplayRoot, root);
}

void SplayNodeInit(SplayNode node) {
  AVER(node != NULL);

  SplayNodeSetLeft(node, NULL);
  SplayNodeSetRight(node, NULL);

  AVERT(SplayNode, node);
}

void SplayNodeFinish(SplayNode node) {
  AVERT(SplayNode, node);

  if(SplayNodeLeft(node) != NULL) {
    SplayNodeFinish(SplayNodeLeft(node));
    SplayNodeSetLeft(node, NULL);
  }

  if(SplayNodeRight(node) != NULL) {
    SplayNodeFinish(SplayNodeRight(node));
    SplayNodeSetRight(node, NULL);
  }
}

void SplayRootFinish(SplayRoot root) {
  AVERT(SplayRoot, root);

  if(root->root != NULL) {
    SplayNodeFinish(root->root);
    root->root = NULL;
  }

  root->compare = NULL;
}

/* SplayLinkRight -- Move top to left child of top
 *
 * Link the current top node into the left child of the right tree,
 * leaving the top node as the left child of the old top node.
 */

static void SplayLinkRight(SplayNode *topIO, SplayNode *rightIO) {
  AVERT(SplayNode, *topIO);
  AVERT(SplayNode, *rightIO);

  SplayNodeSetLeft(*rightIO, *topIO);
  *rightIO = *topIO;
  /* Could say "SplayNodeSetLeft(*topIO, NULL)" here, */
  /* but we're guaranteed that it will never be read. */
  *topIO = SplayNodeLeft(*topIO);
}

/* SplayLinkLeft -- Move top to right child of top
 *
 * Link the current top node into the right child of the left tree,
 * leaving the top node as the right child of the old top node.
 */

static void SplayLinkLeft(SplayNode *topIO, SplayNode *leftIO) {
  AVERT(SplayNode, *topIO);
  AVERT(SplayNode, *leftIO);

  SplayNodeSetRight(*leftIO, *topIO);
  *leftIO = *topIO;
  /* Could say "SplayNodeSetRight(*topIO, NULL)" here, */
  /* but we're guaranteed that it will never be read. */
  *topIO = SplayNodeRight(*topIO);
}

/* SplayRotateLeft -- Rotate right child edge of node
 *
 * Rotates node, right child of node, and left child of right
 * child of node, leftwards in the order stated.
 */

static void SplayRotateLeft(SplayNode *nodeIO) {
  SplayNode nodeRight;

  AVERT(SplayNode, *nodeIO);
  AVERT(SplayNode, SplayNodeRight(*nodeIO));

  nodeRight = SplayNodeRight(*nodeIO);
  SplayNodeSetRight(*nodeIO, SplayNodeLeft(nodeRight));
  SplayNodeSetLeft(nodeRight, *nodeIO);
  *nodeIO = nodeRight;
}

/* SplayRotateRight -- Rotate left child edge of node
 *
 * Rotates node, left child of node, and right child of left
 * child of node, leftwards in the order stated.
 */

static void SplayRotateRight(SplayNode *nodeIO) {
  SplayNode nodeLeft;

  AVERT(SplayNode, *nodeIO);
  AVERT(SplayNode, SplayNodeLeft(*nodeIO));

  nodeLeft = SplayNodeLeft(*nodeIO);
  SplayNodeSetLeft(*nodeIO, SplayNodeRight(nodeLeft));
  SplayNodeSetRight(nodeLeft, *nodeIO);
  *nodeIO = nodeLeft;
}

/* SplayAssemble -- Assemble left right and top trees into one
 *
 * We do this by moving the children of the top tree to the last and 
 * first nodes in the left and right trees, and then moving the tops
 * of the left and right trees to the children of the top tree.
 */

static void SplayAssemble(SplayNode top, 
		          SplayNode leftTop, SplayNode leftLast,
			  SplayNode rightTop, SplayNode rightFirst) {
  AVERT(SplayNode, top);
  AVER(leftTop == NULL || 
       (SplayNodeCheck(leftTop) && SplayNodeCheck(leftLast)));
  AVER(rightTop == NULL || 
       (SplayNodeCheck(rightTop) && SplayNodeCheck(rightFirst)));
 
  if(leftTop != NULL) {
    SplayNodeSetRight(leftLast, SplayNodeLeft(top));
    SplayNodeSetLeft(top, leftTop);
  }
  /* otherwise leave top->left alone */

  if(rightTop != NULL) {
    SplayNodeSetLeft(rightFirst, SplayNodeRight(top));
    SplayNodeSetRight(top, rightTop);
  }
  /* otherwise leave top->right alone */
}

/* SplaySplay -- Splay the tree around the node with a given key
 *
 * If the key is not found, splays around an arbitrary neighbour.
 * Returns whether key was found.  This is the real logic behind
 * splay trees.
 */

static Bool SplaySplay(SplayNode *nodeReturn, SplayRoot root, void *key) {
  /* The sides structure avoids a boundary case in SplayLink* */
  SplayNodeStruct sides; /* rightTop and leftTop */
  SplayNode *topP, leftLast, rightFirst;  
  Bool found;
  Compare compareTop;

  AVERT(SplayRoot, root);
  AVER(nodeReturn != NULL);

  if(root->root == NULL) {
    *nodeReturn = NULL;
    return FALSE;
  }

  SplayNodeInit(&sides); /* left and right trees now NULL */
  topP = &(root->root);
  leftLast = &sides;
  rightFirst = &sides;

  while(TRUE) {
    compareTop = SplayCompare(root, key, *topP);
    switch(compareTop) {

    case CompareLESS: {
      SplayNode topLeft = SplayNodeLeft(*topP);
      if(topLeft == NULL) {
	found = FALSE;
	goto assemble;
      } else {
	Compare compareTopLeft = SplayCompare(root, key, topLeft);

	switch(compareTopLeft) {

	case CompareEQUAL: {                 /* zig */
	  SplayLinkRight(topP, &rightFirst);
	  found = TRUE;
	  goto assemble;
        } break;

	case CompareLESS: {                  /* zig-zig */
	  if(SplayNodeLeft(topLeft) == NULL)
	    goto terminalZig;
          SplayRotateRight(topP);
	  SplayLinkRight(topP, &rightFirst);
        } break;

	case CompareGREATER: {               /* zig-zag */
	  if(SplayNodeRight(topLeft) == NULL)
	    goto terminalZig;
	  SplayLinkRight(topP, &rightFirst);
	  SplayLinkLeft(topP, &leftLast);
        } break;

	default: {
	  NOTREACHED;
        } break;
        }
      }
    } break;

    case CompareGREATER: {
      SplayNode topRight = SplayNodeRight(*topP);
      if(topRight == NULL) {
	found = FALSE;
	goto assemble;
      } else {
	Compare compareTopRight = SplayCompare(root, key, topRight);

	switch(compareTopRight) {

	case CompareEQUAL: {                 /* zag */
	  SplayLinkLeft(topP, &leftLast);
	  found = TRUE;
	  goto assemble;
        } break;

	case CompareGREATER: {               /* zag-zag */
	  if(SplayNodeRight(topRight) == NULL)
	    goto terminalZag;
          SplayRotateLeft(topP);
	  SplayLinkLeft(topP, &leftLast);
        } break;

	case CompareLESS: {                  /* zag-zig */
	  if(SplayNodeLeft(topRight) == NULL)
	    goto terminalZag;
	  SplayLinkLeft(topP, &leftLast);
	  SplayLinkRight(topP, &rightFirst);
        } break;

	default: {
	  NOTREACHED;
        } break;
        }
      }
    } break;

    case CompareEQUAL: {
      found = TRUE;
      goto assemble;
    } break;

    default: {
      NOTREACHED;
    } break;
    }
  }; /* end while(TRUE) */
      
terminalZig:
  SplayLinkRight(topP, &rightFirst);
  found = FALSE;
  goto assemble;

terminalZag:
  SplayLinkLeft(topP, &leftLast);
  found = FALSE;
  goto assemble;

assemble:
  SplayAssemble(*topP, 
		SplayNodeRight(&sides), leftLast,
		SplayNodeLeft(&sides), rightFirst);

  *nodeReturn = *topP;

  return found;
}

Res SplayTreeInsert(SplayRoot root, SplayNode node, void *key) {
  SplayNode neighbour;

  AVERT(SplayRoot, root);
  AVERT(SplayNode, node);
  AVER(SplayNodeLeft(node) == NULL);
  AVER(SplayNodeRight(node) == NULL);

  if(root->root == NULL) {
    root->root = node;
  } else if(SplaySplay(&neighbour, root, key)) {
    return ResFAIL;
  } else {
    AVER(root->root == neighbour);
    switch(SplayCompare(root, key, neighbour)) {

    case CompareGREATER: { /* left neighbour */
      root->root = node;
      SplayNodeSetRight(node, SplayNodeRight(neighbour));
      SplayNodeSetLeft(node, neighbour);
      SplayNodeSetRight(neighbour, NULL);
    } break;

    case CompareLESS: { /* right neighbour */
      root->root = node;
      SplayNodeSetLeft(node, SplayNodeLeft(neighbour));
      SplayNodeSetRight(node, neighbour);
      SplayNodeSetLeft(neighbour, NULL);
    } break;

    case CompareEQUAL:
    default: {
      NOTREACHED;
    } break;
    }
  }

  return ResOK;
}

Res SplayTreeDelete(SplayRoot root, SplayNode node, void *key) {
  SplayNode rightHalf, del, leftLast;

  AVERT(SplayRoot, root);
  AVERT(SplayNode, node);

  if(!SplaySplay(&del, root, key) || del != node) {
    return ResFAIL;
  } else if(SplayNodeLeft(node) == NULL) {
    root->root = SplayNodeRight(node);
  } else if(SplayNodeRight(node) == NULL) {
    root->root = SplayNodeLeft(node);
  } else {
    rightHalf = SplayNodeRight(node);
    if(SplaySplay(&leftLast, root, key)) {
      return ResFAIL;
    } else {
      AVER(SplayNodeRight(leftLast) == NULL);
      SplayNodeSetRight(leftLast, rightHalf);
    }
  }

  SplayNodeFinish(node);

  return ResOK;
}

Res SplayTreeSearch(SplayNode *nodeReturn, SplayRoot root, void *key) {
  SplayNode node;

  AVERT(SplayRoot, root);
  AVER(nodeReturn != NULL);

  if(SplaySplay(&node, root, key)) {
    *nodeReturn = node;
  } else {
    return ResFAIL;
  }

  return ResOK;
}

/* Splay*Neighbour -- Give the root node, find its neighbour in order
 */

static SplayNode SplayLeftNeighbour(SplayNode node) {
  SplayNode neighbour;

  AVERT(SplayNode, node);

  neighbour = SplayNodeLeft(node);
  if(neighbour != NULL) {
    while(SplayNodeRight(neighbour) != NULL) 
      neighbour = SplayNodeRight(neighbour);
  }

  return(neighbour);
}

static SplayNode SplayRightNeighbour(SplayNode node) {
  SplayNode neighbour;

  AVERT(SplayNode, node);

  neighbour = SplayNodeRight(node);
  if(neighbour != NULL) {
    while(SplayNodeLeft(neighbour) != NULL) 
      neighbour = SplayNodeLeft(neighbour);
  }

  return(neighbour);
}


Res SplayTreeNeighbours(SplayNode *leftReturn, SplayNode *rightReturn,
                        SplayRoot root, void *key) {
  SplayNode node;

  AVERT(SplayRoot, root);
  AVER(leftReturn != NULL);
  AVER(rightReturn != NULL);

  if(SplaySplay(&node, root, key)) {
    return ResFAIL;
  } else if(node == NULL) {
    *leftReturn = *rightReturn = NULL;
  } else {
    switch(SplayCompare(root, key, node)) {

    case CompareLESS: {
      *rightReturn = node;
      *leftReturn = SplayLeftNeighbour(node);
    } break;

    case CompareGREATER: {
      *leftReturn = node;
      *rightReturn = SplayRightNeighbour(node);
    } break;

    case CompareEQUAL:
    default: {
      NOTREACHED;
    } break;
    }
  }
  return ResOK;
}

static Res SplayNodeDescribe(SplayNode node, mps_lib_FILE *stream,
                             SplayNodeDescribeMethod nodeDescribe) {
  Res res;

  AVERT(SplayNode, node);
  /* stream and nodeDescribe checked by SplayTreeDescribe */

  res = WriteF(stream, "( ", NULL);
  if(res != ResOK)
    return res;

  if(SplayNodeLeft(node) != NULL) {
    res = SplayNodeDescribe(SplayNodeLeft(node), stream, nodeDescribe);
    if(res != ResOK)
      return res;

    res = WriteF(stream, " / ", NULL);
    if(res != ResOK)
      return res;
  }

  res = (*nodeDescribe)(node, stream);
  if(res != ResOK)
    return res;

  if(SplayNodeRight(node) != NULL) {
    res = WriteF(stream, " \\ ", NULL);
    if(res != ResOK)
      return res;

    res = SplayNodeDescribe(SplayNodeRight(node), stream, nodeDescribe);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream, " )", NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}

Res SplayTreeDescribe(SplayRoot root, mps_lib_FILE *stream, 
		      SplayNodeDescribeMethod nodeDescribe) {
  Res res;

  AVERT(SplayRoot, root);
  AVER(stream != NULL);
  AVER(FUNCHECK(nodeDescribe));

  res = WriteF(stream,
	       "Splay $P {\n", (WriteFP)root,
	       "  compare $F\n", (WriteFF)root->compare,
	       NULL);
  if(res != ResOK)
    return res;

  if(root->root != NULL) {
    res = SplayNodeDescribe(root->root, stream, nodeDescribe);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream, 
	      "\n}\n", 
	      NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}
