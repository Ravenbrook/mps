/* impl.c.cbs: COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $HopeName: MMsrc!cbs.c(MMdevel_gavinm_splay.6) $
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


SRCID(cbs, "$HopeName: MMsrc!cbs.c(MMdevel_gavinm_splay.6) $");

typedef struct CBSNodeStruct {
  SplayNodeStruct splayNode;
  Addr base;
  Addr limit;
  void *p;
} CBSNodeStruct;
typedef struct CBSNodeStruct *CBSNode;

#define CBSOfSplayTree(tree) PARENT(CBSStruct, splayTree, (tree))
#define CBSNodeOfSplayNode(node) PARENT(CBSNodeStruct, splayNode, (node))
#define SplayTreeOfCBS(tree) (&((cbs)->splayTree))
#define SplayNodeOfCBSNode(node) (&((node)->splayNode))

static Bool CBSCheck(CBS cbs) {
  UNUSED(cbs);
  CHECKL(cbs != NULL);
  /* don't check cbs->splayTree? */
  CHECKD(Pool, cbs->nodePool);

  return TRUE;
}

static Bool CBSNodeCheck(CBSNode node) {
  UNUSED(node);
  CHECKL(node != NULL);
  /* Don't check node->splayNode? */
  /* Can't check base, limit, or p */
  return TRUE;
}

/* CBSSplayCompare -- Compare base to [base,limit) 
 *
 * See design.mps.splay.type.splay.compare.method
 */

static Compare CBSSplayCompare(void *key, SplayNode node) {
  Addr base1, base2, limit2;
  CBSNode cbsNode;

  /* NULL key compares less than everything. */
  if(key == NULL)
    return CompareLESS;

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


/* CBSInit -- Initialise a CBS structure
 *
 * See design.mps.cbs.function.cbs.init.
 */

Res CBSInit(Arena arena, CBS cbs, 
		CBSNewMethod new, CBSShrinkMethod shrink,
		CBSGrowMethod grow, CBSDeleteMethod delete,
		Size minSize) {
  Res res;

  AVERT(Arena, arena);

  SplayTreeInit(SplayTreeOfCBS(cbs), &CBSSplayCompare);
  res = PoolCreate(&(cbs->nodePool), arena, PoolClassMFS(), 
		  sizeof(CBSNodeStruct) * 64, sizeof(CBSNodeStruct));
  if(res != ResOK)
    return res;

  cbs->new = new;
  cbs->shrink = shrink;
  cbs->grow = grow;
  cbs->delete = delete;
  cbs->minSize = minSize;

  AVERT(CBS, cbs);

  return ResOK;
}


/* CBSFinish -- Finish a CBS structure
 *
 * See design.mps.cbs.function.cbs.finish.
 */

void CBSFinish(CBS cbs) {
  AVERT(CBS, cbs);

  SplayTreeFinish(SplayTreeOfCBS(cbs));
  PoolDestroy(cbs->nodePool);
}

static Res CBSNodeCreate(CBSNode *nodeReturn,
			 CBS cbs, Addr base, Addr limit) {
  Res res;
  Addr p;
  CBSNode node;

  AVERT(CBS, cbs);

  res = PoolAlloc(&p, cbs->nodePool, sizeof(CBSNodeStruct));
  if(res != ResOK)
    goto failPoolAlloc;

  node = (CBSNode)p;

  SplayNodeInit(SplayNodeOfCBSNode(node));
  node->base = base;
  node->limit = limit;
  node->p = NULL;

  AVERT(CBSNode, node);

  res = SplayTreeInsert(SplayTreeOfCBS(cbs), SplayNodeOfCBSNode(node),
			(void *)&(node->base));
  if(res != ResOK)
    goto failSplayTreeInsert;

  *nodeReturn = node;
  return ResOK;

failSplayTreeInsert:
  PoolFree(cbs->nodePool, (Addr)node, sizeof(CBSNodeStruct));
failPoolAlloc:
  return res;
}

static Res CBSNodeDestroy(CBS cbs, CBSNode node) {
  Res res;

  AVERT(CBS, cbs);
  AVERT(CBSNode, node);

  res = SplayTreeDelete(SplayTreeOfCBS(cbs), SplayNodeOfCBSNode(node), 
                        (void *)&(node->base));
  if(res != ResOK)
    return res;

  node->base = (Addr)0;
  node->limit = (Addr)0;
  node->p = NULL;

  PoolFree(cbs->nodePool, (Addr)node, sizeof(CBSNodeStruct));

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

static Res CBSNodeDelete(CBS cbs, CBSNode node) {
  Res res;

  AVERT(CBS, cbs);
  AVERT(CBSNode, node);

  if(cbs->delete != NULL && node->p != NULL)
    (*(cbs->delete))(node->p, cbs);

  res = CBSNodeDestroy(cbs, node);
  if(res != ResOK)
    return res;

  return ResOK;
}

static void CBSNodeShrink(CBS cbs, CBSNode node) {
  AVERT(CBS, cbs);
  AVERT(CBSNode, node);

  if(cbs->shrink != NULL && node->p != NULL)
    (*(cbs->shrink))(&(node->p), cbs, node->base, node->limit);
}

static void CBSNodeGrow(CBS cbs, CBSNode node) {
  AVERT(CBS, cbs);
  AVERT(CBSNode, node);

  if(cbs->grow != NULL && (node->p != NULL ||
     AddrOffset(node->base, node->limit) >= cbs->minSize))
    (*(cbs->grow))(&(node->p), cbs, node->base, node->limit);
}

static Res CBSNodeNew(CBS cbs, Addr base, Addr limit) {
  CBSNode node;
  Res res;

  AVERT(CBS, cbs);

  res = CBSNodeCreate(&node, cbs, base, limit);
  if(res != ResOK)
    return res;
  
  if(cbs->new != NULL && 
     AddrOffset(base, limit) >= cbs->minSize)
    (*(cbs->new))(&(node->p), cbs, base, limit);
  
  return ResOK;
}


/* CBSInsert -- Insert a range into the CBS
 *
 * See design.mps.cbs.functions.cbs.insert.
 */

Res CBSInsert(CBS cbs, Addr base, Addr limit) {
  Res res;
  SplayNode leftSplay, rightSplay;
  CBSNode leftCBS, rightCBS;
  Bool leftMerge, rightMerge;

  AVERT(CBS, cbs);
  AVER(base != (Addr)0);
  AVER(base < limit);

  res = SplayTreeNeighbours(&leftSplay, &rightSplay,
                            SplayTreeOfCBS(cbs), (void *)&base);
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
      CBSNodeGrow(cbs, leftCBS);
      res = CBSNodeDelete(cbs, rightCBS);
      if(res != ResOK)
	return res;
    } else { /* leftMerge, !rightMerge */
      leftCBS->limit = limit;
      CBSNodeGrow(cbs, leftCBS);
    }
  } else { /* !leftMerge */
    if(rightMerge) {
      rightCBS->base = base;
      CBSNodeGrow(cbs, rightCBS);
    } else { /* !leftMerge, !rightMerge */
      res = CBSNodeNew(cbs, base, limit);
      if(res != ResOK)
	return res;
    }
  }

  return ResOK;
}


/* CBSDelete -- Remove a range from a CBS
 *
 * See design.mps.cbs.function.cbs.delete.
 */

Res CBSDelete(CBS cbs, Addr base, Addr limit) {
  Res res;
  CBSNode cbsNode;
  SplayNode splayNode;

  AVERT(CBS, cbs);
  AVER(base != NULL);
  AVER(limit > base);

  res = SplayTreeSearch(&splayNode, SplayTreeOfCBS(cbs), (void *)&base);
  if(res != ResOK)
    goto failSplayTreeSearch;
  cbsNode = CBSNodeOfSplayNode(splayNode);

  if(limit > cbsNode->limit) {
    res = ResFAIL;
    goto failLimitCheck;
  }

  if(base == cbsNode->base) {
    if(limit == cbsNode->limit) { /* entire block */
      res = CBSNodeDelete(cbs, cbsNode);
      if(res != ResOK) 
	goto failDelete;
    } else { /* remaining fragment at right */
      AVER(limit < cbsNode->limit);
      cbsNode->base = limit;
      CBSNodeShrink(cbs, cbsNode);
    }
  } else {
    AVER(base > cbsNode->base);
    if(limit == cbsNode->limit) { /* remaining fragment at left */
      cbsNode->limit = base;
      CBSNodeShrink(cbs, cbsNode);
    } else { /* two remaining fragments */
      Addr oldLimit;
      AVER(limit < cbsNode->limit);
      oldLimit = cbsNode->limit;
      cbsNode->limit = base;
      CBSNodeShrink(cbs, cbsNode);
      res = CBSNodeNew(cbs, limit, oldLimit);
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


/* CBSIterate -- Iterate all nodes in CBS
 *
 * This is not necessarily efficient.
 *
 * See design.mps.cbs.function.cbs.iterate.
 */

void CBSIterate(CBS cbs, CBSIterateMethod iterate,
		void *closureP, unsigned long closureS) {
  SplayNode splayNode;
  SplayTree splayTree;
  CBSNode cbsNode;

  AVERT(CBS, cbs);
  AVER(FUNCHECK(iterate));

  splayTree = SplayTreeOfCBS(cbs);
  splayNode = SplayTreeFirst(splayTree, NULL);
  while(splayNode != NULL) {
    cbsNode = CBSNodeOfSplayNode(splayNode);
    if(!(*iterate)(&(cbsNode->p), cbs, cbsNode->base, cbsNode->limit,
		   closureP, closureS)) {
      break;
    }
    splayNode = SplayTreeNext(splayTree, NULL);
  }
}


/* CBSSetMinSize -- Set minimum interesting size for cbs
 *
 * This function may invoke the shrink and grow methods as
 * appropriate.  See design.mps.cbs.function.cbs.set.min-size.
 */

typedef struct {
  Size old;
  Size new;
} CBSSetMinSizeClosureStruct, *CBSSetMinSizeClosure;

static Bool CBSSetMinSizeGrow(void **clientPIO, CBS cbs, 
		              Addr base, Addr limit, 
			      void *closureP, unsigned long closureS) {
  CBSSetMinSizeClosure closure;
  Size size;
  
  if(*clientPIO ==  NULL) {
    closure = (CBSSetMinSizeClosure)closureP;
    AVER(closure->old > closure->new);
    size = AddrOffset(base, limit);
    if(size < closure->old && size >= closure->new)
    (*cbs->grow)(clientPIO, cbs, base, limit);
  }

  return TRUE;
}

static Bool CBSSetMinSizeShrink(void **clientPIO, CBS cbs, 
				Addr base, Addr limit, 
				void *closureP, unsigned long closureS) {
  CBSSetMinSizeClosure closure;
  Size size;
  
  if(*clientPIO !=  NULL) {
    closure = (CBSSetMinSizeClosure)closureP;
    AVER(closure->old < closure->new);
    size = AddrOffset(base, limit);
    if(size >= closure->old && size < closure->new)
    (*cbs->shrink)(clientPIO, cbs, base, limit);
  }

  return TRUE;
}
void CBSSetMinSize(CBS cbs, Size minSize) {
  CBSSetMinSizeClosureStruct closure;

  AVERT(CBS, cbs);

  closure.old = cbs->minSize;
  closure.new = minSize;

  if(minSize < cbs->minSize)
    CBSIterate(cbs, &CBSSetMinSizeGrow, (void *)&closure, 0);
  else if(minSize > cbs->minSize)
    CBSIterate(cbs, &CBSSetMinSizeShrink, (void *)&closure, 0);

  cbs->minSize = minSize;
}

/* CBSDescribe -- Describe a CBS
 *
 * See design.mps.cbs.function.cbs.describe.
 */

Res CBSDescribe(CBS cbs, mps_lib_FILE *stream) {
  Res res;

  AVERT(CBS, cbs);
  AVER(stream != NULL);

  res = WriteF(stream,
	       "CBS $P {\n", (WriteFP)cbs,
	       "  nodePool: $P\n", (WriteFP)cbs->nodePool,
	       "  new: $F ", (WriteFF)cbs->new,
	       "  grow $F ", (WriteFF)cbs->grow,
	       "  shrink: $F ", (WriteFF)cbs->shrink,
	       "  delete: $F \n", (WriteFF)cbs->delete,
	       NULL);
  if(res != ResOK)
    return res;

  res = SplayTreeDescribe(SplayTreeOfCBS(cbs), stream,
			  &CBSSplayNodeDescribe);
  if(res != ResOK)
    return res;

  res = WriteF(stream, "}\n", NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}
