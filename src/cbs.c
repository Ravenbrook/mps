/* impl.c.cbs: COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $HopeName: MMsrc!cbs.c(MMdevel_gavinm_splay.4) $
 * Copyright (C) 1998 Harlequin Group plc, all rights reserved.
 *
 * .readership: Any MPS developer.
 *
 * .intro: This is a portable implementation of coalescing block
 * structures.
 *
 * .purpose: CBSs are used to manage potentially unbounded 
 * collections of memory blocks.
 *
 * .sources: design.mps.cbs.
 */


#include "mpm.h"


SRCID(cbs, "$HopeName: MMsrc!cbs.c(MMdevel_gavinm_splay.4) $");

#define CBSRootOfSplayRoot(root) PARENT(CBSRootStruct, splayRoot, (root))
#define CBSNodeOfSplayNode(node) PARENT(CBSNodeStruct, splayNode, (node))
#define SplayRootOfCBSRoot(root) (&((root)->splayRoot))
#define SplayNodeOfCBSNode(node) (&((node)->splayNode))

static Bool CBSRootCheck(CBSRoot root) {
  UNUSED(root);
  CHECKL(root != NULL);
  /* don't check root->splayRoot? */
  CHECKD(Pool, root->nodePool);
  return TRUE;
}

static Bool CBSNodeCheck(CBSNode node) {
  UNUSED(node);
  CHECKL(node != NULL);
  /* Don't check root->spayNode? */
  /* Can't check base, limit, or p */
  return TRUE;
}

/* CBSSplayCompare -- Compare base to [base,limit) */

static Compare CBSSplayCompare(void *key, SplayNode node) {
  Addr base1, base2, limit2;
  CBSNode cbsNode;

  AVER(key != NULL);
  AVER(node != NULL);

  base1 = *(Addr *)key;
  cbsNode = CBSNodeOfSplayNode(node);
  base2 = cbsNode->base;
  limit2 = cbsNode->limit;

  if(base1 < base2) 
    return CompareLESS;
  else if(base1 >= limit2)
    return CompareGREATER;
  else
    return CompareEQUAL;
}

Res CBSRootInit(Arena arena, CBSRoot root, 
		CBSNewMethod new, CBSShrinkMethod shrink,
		CBSGrowMethod grow, CBSDeleteMethod delete,
		Size minSize) {
  Res res;

  AVERT(Arena, arena);

  SplayRootInit(SplayRootOfCBSRoot(root), &CBSSplayCompare);
  res = PoolCreate(&(root->nodePool), arena, PoolClassMFS(), 
		  sizeof(CBSNodeStruct) * 64, sizeof(CBSNodeStruct));
  if(res != ResOK)
    return res;

  root->new = new;
  root->shrink = shrink;
  root->grow = grow;
  root->delete = delete;
  root->minSize = minSize;

  AVERT(CBSRoot, root);

  return ResOK;
}

void CBSRootFinish(CBSRoot root) {
  AVERT(CBSRoot, root);

  SplayRootFinish(SplayRootOfCBSRoot(root));
  PoolDestroy(root->nodePool);
}

static Res CBSNodeCreate(CBSNode *nodeReturn,
			 CBSRoot root, Addr base, Addr limit) {
  Res res;
  Addr p;
  CBSNode node;

  AVERT(CBSRoot, root);

  res = PoolAlloc(&p, root->nodePool, sizeof(CBSNodeStruct));
  if(res != ResOK)
    goto failPoolAlloc;

  node = (CBSNode)p;

  SplayNodeInit(SplayNodeOfCBSNode(node));
  node->base = base;
  node->limit = limit;
  node->p = NULL;

  AVERT(CBSNode, node);

  res = SplayTreeInsert(SplayRootOfCBSRoot(root), SplayNodeOfCBSNode(node),
			(void *)&(node->base));
  if(res != ResOK)
    goto failSplayTreeInsert;

  *nodeReturn = node;
  return ResOK;

failSplayTreeInsert:
  PoolFree(root->nodePool, (Addr)node, sizeof(CBSNodeStruct));
failPoolAlloc:
  return res;
}

static Res CBSNodeDestroy(CBSRoot root, CBSNode node) {
  Res res;

  AVERT(CBSRoot, root);
  AVERT(CBSNode, node);

  res = SplayTreeDelete(SplayRootOfCBSRoot(root), SplayNodeOfCBSNode(node), 
                        (void *)&(node->base));
  if(res != ResOK)
    return res;

  node->base = (Addr)0;
  node->limit = (Addr)0;
  node->p = NULL;

  PoolFree(root->nodePool, (Addr)node, sizeof(CBSNodeStruct));

  return ResOK;
}

/* Node change operators
 *
 * These four functions are called whenever nodes are created,
 * destroyed, grow, or shrink.  They report to the client, and
 * perform the necessary memory management.  They are responsible
 * for the client interaction logic.
 *
 * This logic could be extended to use both the old and new sizes.
 * This would be best done by passing in the new range and making
 * the assignment inside these operators.
 */

static Res CBSNodeDelete(CBSRoot root, CBSNode node) {
  Res res;

  AVERT(CBSRoot, root);
  AVERT(CBSNode, node);

  if(root->delete != NULL && node->p != NULL)
    (*(root->delete))(node->p);

  res = CBSNodeDestroy(root, node);
  if(res != ResOK)
    return res;

  return ResOK;
}

static void CBSNodeShrink(CBSRoot root, CBSNode node) {
  AVERT(CBSRoot, root);
  AVERT(CBSNode, node);

  if(root->shrink != NULL && node->p != NULL)
    (*(root->shrink))(&(node->p), node->base, node->limit);
}

static void CBSNodeGrow(CBSRoot root, CBSNode node) {
  AVERT(CBSRoot, root);
  AVERT(CBSNode, node);

  if(root->grow != NULL && (node->p != NULL ||
     AddrOffset(node->base, node->limit) >= root->minSize))
    (*(root->grow))(&(node->p), node->base, node->limit);
}

static Res CBSNodeNew(CBSRoot root, Addr base, Addr limit) {
  CBSNode node;
  Res res;

  AVERT(CBSRoot, root);

  res = CBSNodeCreate(&node, root, base, limit);
  if(res != ResOK)
    return res;
  
  if(root->new != NULL && 
     AddrOffset(base, limit) >= root->minSize)
    (*(root->new))(&(node->p), base, limit);
  
  return ResOK;
}

Res CBSInsert(CBSRoot root, Addr base, Addr limit) {
  Res res;
  SplayNode leftSplay, rightSplay;
  CBSNode leftCBS, rightCBS;
  Bool leftMerge, rightMerge;

  AVERT(CBSRoot, root);
  AVER(base != (Addr)0);
  AVER(base < limit);

  res = SplayTreeNeighbours(&leftSplay, &rightSplay,
                            SplayRootOfCBSRoot(root), (void *)&base);
  if(res != ResOK)
    return res;

  leftCBS = (leftSplay == NULL) ? NULL : CBSNodeOfSplayNode(leftSplay);
  rightCBS = (rightSplay == NULL) ? NULL : CBSNodeOfSplayNode(rightSplay);

  /* We know that base falls outside leftCBS by the contract of */
  /* CBSSplayCompare. Now we see if limit falls within rightCBS. */
  if(rightCBS != NULL && limit > rightCBS->base)
    return ResFAIL;

  leftMerge = (leftCBS != NULL) && (leftCBS->limit == base);
  rightMerge = (rightCBS != NULL) && (limit == rightCBS->base);

  if(leftMerge) {
    if(rightMerge) {
      leftCBS->limit = rightCBS->limit;
      CBSNodeGrow(root, leftCBS);
      res = CBSNodeDelete(root, rightCBS);
      if(res != ResOK)
	return res;
    } else { /* leftMerge, !rightMerge */
      leftCBS->limit = limit;
      CBSNodeGrow(root, leftCBS);
    }
  } else { /* !leftMerge */
    if(rightMerge) {
      rightCBS->base = base;
      CBSNodeGrow(root, rightCBS);
    } else { /* !leftMerge, !rightMerge */
      res = CBSNodeNew(root, base, limit);
      if(res != ResOK)
	return res;
    }
  }

  return ResOK;
}

Res CBSDelete(CBSRoot root, Addr base, Addr limit) {
  Res res;
  CBSNode cbsNode;
  SplayNode splayNode;

  AVERT(CBSRoot, root);
  AVER(base != NULL);
  AVER(limit > base);

  res = SplayTreeSearch(&splayNode, SplayRootOfCBSRoot(root), (void *)&base);
  if(res != ResOK)
    goto failSplayTreeSearch;
  cbsNode = CBSNodeOfSplayNode(splayNode);

  if(limit > cbsNode->limit) {
    res = ResFAIL;
    goto failLimitCheck;
  }

  if(base == cbsNode->base) {
    if(limit == cbsNode->limit) { /* entire block */
      res = CBSNodeDelete(root, cbsNode);
      if(res != ResOK) 
	goto failDelete;
    } else { /* remaining fragment at right */
      AVER(limit < cbsNode->limit);
      cbsNode->base = limit;
      CBSNodeShrink(root, cbsNode);
    }
  } else {
    AVER(base > cbsNode->base);
    if(limit == cbsNode->limit) { /* remaining fragment at left */
      cbsNode->limit = base;
      CBSNodeShrink(root, cbsNode);
    } else { /* two remaining fragments */
      Addr oldLimit;
      AVER(limit < cbsNode->limit);
      oldLimit = cbsNode->limit;
      cbsNode->limit = base;
      CBSNodeShrink(root, cbsNode);
      res = CBSNodeNew(root, limit, oldLimit);
      if(res != ResOK)
	goto failNew;
    }
  }

  return ResOK;

failNew:
failDelete: 
failLimitCheck:
failSplayTreeSearch:
  return res;
}

static Res CBSSplayNodeDescribe(SplayNode splayNode, 
				mps_lib_FILE *stream) {
  Res res;
  CBSNode cbsNode;

  AVER(splayNode != NULL);
  AVER(stream != NULL);

  cbsNode = CBSNodeOfSplayNode(splayNode);

  res = WriteF(stream,
	       "[$P,$P)", (WriteFP)cbsNode->base, (WriteFP)cbsNode->limit,
	       NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}

Res CBSDescribe(CBSRoot root, mps_lib_FILE *stream) {
  Res res;

  AVERT(CBSRoot, root);
  AVER(stream != NULL);

  res = WriteF(stream,
	       "CBSRoot $P {\n", (WriteFP)root,
	       "  nodePool: $P\n", (WriteFP)root->nodePool,
	       "  new: $F ", (WriteFF)root->new,
	       "  grow $F ", (WriteFF)root->grow,
	       "  shrink: $F ", (WriteFF)root->shrink,
	       "  delete: $F \n", (WriteFF)root->delete,
	       NULL);
  if(res != ResOK)
    return res;

  res = SplayTreeDescribe(SplayRootOfCBSRoot(root), stream,
			  &CBSSplayNodeDescribe);
  if(res != ResOK)
    return res;

  res = WriteF(stream, "}\n", NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}
