/* impl.c.cbs: COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $HopeName: MMsrc!cbs.c(MMdevel_gavinm_mvff.1) $
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


SRCID(cbs, "$HopeName: MMsrc!cbs.c(MMdevel_gavinm_mvff.1) $");

typedef struct CBSEmergencyBlockStruct *CBSEmergencyBlock;
typedef struct CBSEmergencyBlockStruct {
  CBSEmergencyBlock next;
  Addr limit;
} CBSEmergencyBlockStruct;


typedef struct CBSEmergencyGrainStruct *CBSEmergencyGrain;
typedef struct CBSEmergencyGrainStruct {
  CBSEmergencyGrain next;
} CBSEmergencyGrainStruct;


#define CBSOfSplayTree(tree) PARENT(CBSStruct, splayTree, (tree))
#define CBSBlockOfSplayNode(node) PARENT(CBSBlockStruct, splayNode, (node))
#define SplayTreeOfCBS(tree) (&((cbs)->splayTree))
#define SplayNodeOfCBSBlock(block) (&((block)->splayNode))
#define KeyOfCBSBlock(block) ((void *)&((block)->base))


/* CBSEnter, CBSLeave -- Avoid re-entrance 
 *
 * .enter-leave: The callbacks are restricted in what they may call.
 * These functions enforce this.
 *
 * .enter-leave.simple: Simple queries may be called from callbacks.
 */

static void CBSEnter(CBS cbs) {
  /* Don't need to check as always called from interface function. */
  AVER(!cbs->inCBS);
  cbs->inCBS = TRUE;
  return;
}

static void CBSLeave(CBS cbs) {
  /* Don't need to check as always called from interface function. */
  AVER(cbs->inCBS);
  cbs->inCBS = FALSE;
  return;
}

Bool CBSCheck(CBS cbs) {
  /* See .enter-leave.simple. */
  CHECKS(CBS, cbs);
  CHECKL(cbs != NULL);
  CHECKL(SplayTreeCheck(SplayTreeOfCBS(cbs)));
  /* can't check emergencyBlockList or emergencyGrainList */
  CHECKD(Pool, cbs->blockPool);
  CHECKL(BoolCheck(cbs->mayUseInline));
  CHECKL(BoolCheck(cbs->inCBS));
  CHECKL(cbs->new == NULL || FUNCHECK(cbs->new));
  CHECKL(cbs->delete == NULL || FUNCHECK(cbs->delete));
  CHECKL(cbs->grow == NULL || FUNCHECK(cbs->grow));
  CHECKL(cbs->shrink == NULL || FUNCHECK(cbs->shrink));
  CHECKL(cbs->mayUseInline || cbs->emergencyBlockList == NULL);
  CHECKL(cbs->mayUseInline || cbs->emergencyGrainList == NULL);

  return TRUE;
}

/* CBSBlockCheck -- See design.mps.cbs.function.cbs.block.check */
 
Bool CBSBlockCheck(CBSBlock block) {
  /* See .enter-leave.simple. */
  UNUSED(block); /* Required because there is no signature */
  CHECKL(block != NULL);
  CHECKL(SplayNodeCheck(SplayNodeOfCBSBlock(block)));
 
  /* If the block is in the middle of being deleted, */
  /* the pointers will be equal. */
  CHECKL(CBSBlockBase(block) <= CBSBlockLimit(block));
  /* Can't check maxSize because it may be invalid at the time */
  return TRUE;
}
 
 
/* CBSBlockBase -- See design.mps.cbs.function.cbs.block.base */
 
Addr (CBSBlockBase)(CBSBlock block) {
  /* See .enter-leave.simple. */
  return CBSBlockBase(block);
}
 
 
/* CBSBlockLimit -- See design.mps.cbs.function.cbs.block.limit */
 
Addr (CBSBlockLimit)(CBSBlock block) {
  /* See .enter-leave.simple. */
  return CBSBlockLimit(block);
}
 
 
/* CBSBlockSize -- see design.mps.cbs.function.cbs.block.size */
 
Size (CBSBlockSize)(CBSBlock block) {
  /* See .enter-leave.simple. */
  return CBSBlockSize(block);
}


/* CBSSplayCompare -- Compare base to [base,limit) 
 *
 * See design.mps.splay.type.splay.compare.method
 */

static Compare CBSSplayCompare(void *key, SplayNode node) {
  Addr base1, base2, limit2;
  CBSBlock cbsBlock;

  /* NULL key compares less than everything. */
  if(key == NULL)
    return CompareLESS;

  AVER(node != NULL);

  base1 = *(Addr *)key;
  cbsBlock = CBSBlockOfSplayNode(node);
  base2 = cbsBlock->base;
  limit2 = cbsBlock->limit;

  if(base1 < base2) 
    return CompareLESS;
  else if(base1 >= limit2)
    return CompareGREATER;
  else
    return CompareEQUAL;
}

static Bool CBSTestNode(SplayTree tree, SplayNode node, 
                        void *closureP, unsigned long closureS)
{
  Size size;
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  AVER(closureP == NULL);
  AVER(closureS > 0);

  size = (Size)closureS;
  block = CBSBlockOfSplayNode(node);

  return CBSBlockSize(block) >= size;
}

static Bool CBSTestTree(SplayTree tree, SplayNode node,
                        void *closureP, unsigned long closureS) 
{
  Size size;
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  AVER(closureP == NULL);
  AVER(closureS > 0);

  size = (Size)closureS;
  block = CBSBlockOfSplayNode(node);

  return block->maxSize >= size;
}

static void CBSUpdateNode(SplayTree tree, SplayNode node,
                          SplayNode leftChild, SplayNode rightChild)
{
  Size maxSize;
  CBSBlock block;

  AVERT(SplayTree, tree);
  AVERT(SplayNode, node);
  if(leftChild != NULL)
    AVERT(SplayNode, leftChild);
  if(rightChild != NULL)
    AVERT(SplayNode, rightChild);

  block = CBSBlockOfSplayNode(node);
  maxSize = CBSBlockSize(block);

  if(leftChild != NULL) {
    Size size = CBSBlockOfSplayNode(leftChild)->maxSize;
    if(size > maxSize)
      maxSize = size;
  }

  if(rightChild != NULL) {
    Size size = CBSBlockOfSplayNode(rightChild)->maxSize;
    if(size > maxSize)
      maxSize = size;
  }

  block->maxSize = maxSize;
}

/* CBSInit -- Initialise a CBS structure
 *
 * See design.mps.cbs.function.cbs.init.
 */

Res CBSInit(Arena arena, CBS cbs, 
            CBSChangeSizeMethod new, 
            CBSChangeSizeMethod delete,
            CBSChangeSizeMethod grow,
            CBSChangeSizeMethod shrink,
            Size minSize,
            Bool mayUseInline) {
  Res res;

  AVERT(Arena, arena);
  AVER(new == NULL || FUNCHECK(new));
  AVER(delete == NULL || FUNCHECK(delete));
  AVER(BoolCheck(mayUseInline));

  SplayTreeInit(SplayTreeOfCBS(cbs), &CBSSplayCompare, &CBSUpdateNode);
  res = PoolCreate(&(cbs->blockPool), arena, PoolClassMFS(),
                   sizeof(CBSBlockStruct) * 64, sizeof(CBSBlockStruct));
  if(res != ResOK)
    return res;

  cbs->new = new;
  cbs->delete = delete;
  cbs->grow = grow;
  cbs->shrink = shrink;
  cbs->minSize = minSize;
  cbs->mayUseInline = mayUseInline;
  cbs->inCBS = TRUE;
  cbs->emergencyBlockList = NULL;
  cbs->emergencyGrainList = NULL;

  cbs->sig = CBSSig;

  AVERT(CBS, cbs);

  CBSLeave(cbs);
  return ResOK;
}


/* CBSFinish -- Finish a CBS structure
 *
 * See design.mps.cbs.function.cbs.finish.
 */

void CBSFinish(CBS cbs) {
  CBSEnter(cbs);

  AVERT(CBS, cbs);

  cbs->sig = SigInvalid;

  SplayTreeFinish(SplayTreeOfCBS(cbs));
  PoolDestroy(cbs->blockPool);
  cbs->emergencyBlockList = NULL;
  cbs->emergencyGrainList = NULL;
}


/* Node change operators
 *
 * These four functions are called whenever blocks are created,
 * destroyed, grow, or shrink.  They report to the client, and
 * perform the necessary memory management.  They are responsible
 * for the client interaction logic.
 */

static Res CBSBlockDelete(CBS cbs, CBSBlock block) {
  Res res;
  Size oldSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  oldSize = CBSBlockSize(block);

  res = SplayTreeDelete(SplayTreeOfCBS(cbs), SplayNodeOfCBSBlock(block), 
                        KeyOfCBSBlock(block));
  if(res != ResOK)
    return res;

  /* make invalid */
  block->limit = block->base;

  if(cbs->delete != NULL && oldSize >= cbs->minSize)
    (*(cbs->delete))(cbs, block, oldSize, (Size)0);

  PoolFree(cbs->blockPool, (Addr)block, sizeof(CBSBlockStruct));

  return ResOK;
}

static void CBSBlockShrink(CBS cbs, CBSBlock block, Size oldSize) {
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize > newSize);

  SplayNodeRefresh(SplayTreeOfCBS(cbs), SplayNodeOfCBSBlock(block),
                   KeyOfCBSBlock(block));
  AVER(CBSBlockSize(block) <= block->maxSize);

  if(cbs->delete != NULL && oldSize >= cbs->minSize && 
     newSize < cbs->minSize)
    (*(cbs->delete))(cbs, block, oldSize, newSize);
  else if(cbs->shrink != NULL && newSize >= cbs->minSize)
    (*(cbs->shrink))(cbs, block, oldSize, newSize);
}

static void CBSBlockGrow(CBS cbs, CBSBlock block, Size oldSize) {
  Size newSize;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);

  newSize = CBSBlockSize(block);
  AVER(oldSize < newSize);

  SplayNodeRefresh(SplayTreeOfCBS(cbs), SplayNodeOfCBSBlock(block),
                   KeyOfCBSBlock(block));
  AVER(CBSBlockSize(block) <= block->maxSize);

  if(cbs->new != NULL && oldSize < cbs->minSize &&
     newSize >= cbs->minSize)
    (*(cbs->new))(cbs, block, oldSize, newSize);
  else if(cbs->grow != NULL && oldSize >= cbs->minSize)
    (*(cbs->grow))(cbs, block, oldSize, newSize);
}

static Res CBSBlockNew(CBS cbs, Addr base, Addr limit) {
  CBSBlock block;
  Res res;
  Addr p;
  Size newSize;

  AVERT(CBS, cbs);

  res = PoolAlloc(&p, cbs->blockPool, sizeof(CBSBlockStruct),
                  /* withReservoirPermit */ FALSE);
  if(res != ResOK)
    goto failPoolAlloc;
  block = (CBSBlock)p;

  SplayNodeInit(SplayNodeOfCBSBlock(block));
  block->base = base;
  block->limit = limit;
  newSize = CBSBlockSize(block);
  block->maxSize = newSize;

  AVERT(CBSBlock, block);

  res = SplayTreeInsert(SplayTreeOfCBS(cbs), SplayNodeOfCBSBlock(block),
                        KeyOfCBSBlock(block));
  if(res != ResOK)
    goto failSplayTreeInsert;

  if(cbs->new != NULL && newSize >= cbs->minSize)
    (*(cbs->new))(cbs, block, (Size)0, newSize);

  return ResOK;

failSplayTreeInsert:
  PoolFree(cbs->blockPool, (Addr)block, sizeof(CBSBlockStruct));
failPoolAlloc:
  AVER(res != ResOK);
  return res;
}


/* CBSInsert -- Insert a range into the CBS
 *
 * See design.mps.cbs.functions.cbs.insert.
 */

Res CBSInsert(CBS cbs, Addr base, Addr limit) {
  Res res;
  SplayNode leftSplay, rightSplay;
  CBSBlock leftCBS, rightCBS;
  Bool leftMerge, rightMerge;
  Size oldSize;

  CBSEnter(cbs);

  AVERT(CBS, cbs);
  AVER(base != (Addr)0);
  AVER(base < limit);

  res = SplayTreeNeighbours(&leftSplay, &rightSplay,
                            SplayTreeOfCBS(cbs), (void *)&base);
  if(res != ResOK)
    goto fail;

  leftCBS = (leftSplay == NULL) ? NULL :
    CBSBlockOfSplayNode(leftSplay);

  rightCBS = (rightSplay == NULL) ? NULL :
    CBSBlockOfSplayNode(rightSplay);

  /* We know that base falls outside leftCBS by the contract of */
  /* CBSSplayCompare. Now we see if limit falls within rightCBS. */
  if(rightCBS != NULL && limit > rightCBS->base) {
    res = ResFAIL;
    goto fail;
  }

  leftMerge = (leftCBS != NULL) && (leftCBS->limit == base);
  rightMerge = (rightCBS != NULL) && (limit == rightCBS->base);

  if(leftMerge) {
    if(rightMerge) {
      Size oldLeftSize = CBSBlockSize(leftCBS);
      Size oldRightSize = CBSBlockSize(rightCBS);

      /* must block larger neighbour and destroy smaller neighbour; */
      /* see design.mps.cbs.function.cbs.insert.callback */
      if(oldLeftSize >= oldRightSize) {
        Addr rightLimit = rightCBS->limit;
        res = CBSBlockDelete(cbs, rightCBS);
        if(res != ResOK) 
          goto fail;
        leftCBS->limit = rightLimit;
        CBSBlockGrow(cbs, leftCBS, oldLeftSize);
      } else { /* left block is smaller */
        Addr leftBase = leftCBS->base;
        res = CBSBlockDelete(cbs, leftCBS);
        if(res != ResOK) 
          goto fail;
        rightCBS->base = leftBase;
        CBSBlockGrow(cbs, rightCBS, oldRightSize);
      }
    } else { /* leftMerge, !rightMerge */
      oldSize = CBSBlockSize(leftCBS);
      leftCBS->limit = limit;
      CBSBlockGrow(cbs, leftCBS, oldSize);
    }
  } else { /* !leftMerge */
    if(rightMerge) {
      oldSize = CBSBlockSize(rightCBS);
      rightCBS->base = base;
      CBSBlockGrow(cbs, rightCBS, oldSize);
    } else { /* !leftMerge, !rightMerge */
      res = CBSBlockNew(cbs, base, limit);
      if(res != ResOK) 
        goto fail;
    }
  }

  CBSLeave(cbs);
  return ResOK;

fail:
  CBSLeave(cbs);
  AVER(res != ResOK);
  return res;
}


/* CBSDelete -- Remove a range from a CBS
 *
 * See design.mps.cbs.function.cbs.delete.
 */

Res CBSDelete(CBS cbs, Addr base, Addr limit) {
  Res res;
  CBSBlock cbsBlock;
  SplayNode splayNode;
  Size oldSize;

  CBSEnter(cbs);

  AVERT(CBS, cbs);
  AVER(base != NULL);
  AVER(limit > base);

  res = SplayTreeSearch(&splayNode, SplayTreeOfCBS(cbs), (void *)&base);
  if(res != ResOK)
    goto failSplayTreeSearch;
  cbsBlock = CBSBlockOfSplayNode(splayNode);

  if(limit > cbsBlock->limit) {
    res = ResFAIL;
    goto failLimitCheck;
  }

  if(base == cbsBlock->base) {
    if(limit == cbsBlock->limit) { /* entire block */
      res = CBSBlockDelete(cbs, cbsBlock);
      if(res != ResOK) 
        goto failDelete;
    } else { /* remaining fragment at right */
      AVER(limit < cbsBlock->limit);
      oldSize = CBSBlockSize(cbsBlock);
      cbsBlock->base = limit;
      CBSBlockShrink(cbs, cbsBlock, oldSize);
    }
  } else {
    AVER(base > cbsBlock->base);
    if(limit == cbsBlock->limit) { /* remaining fragment at left */
      oldSize = CBSBlockSize(cbsBlock);
      cbsBlock->limit = base;
      CBSBlockShrink(cbs, cbsBlock, oldSize);
    } else { /* two remaining fragments */
      Size leftNewSize = AddrOffset(cbsBlock->base, base);
      Size rightNewSize = AddrOffset(limit, cbsBlock->limit);
      /* must shrink larger fragment and create smaller; */
      /* see design.mps.cbs.function.cbs.delete.callback */
      if(leftNewSize >= rightNewSize) {
        Addr oldLimit = cbsBlock->limit;
        AVER(limit < cbsBlock->limit);
        oldSize = CBSBlockSize(cbsBlock);
        cbsBlock->limit = base;
        CBSBlockShrink(cbs, cbsBlock, oldSize);
        res = CBSBlockNew(cbs, limit, oldLimit);
        if(res != ResOK)
          goto failNew;
      } else { /* right fragment is larger */
        Addr oldBase = cbsBlock->base;
        AVER(base > cbsBlock->base);
        oldSize = CBSBlockSize(cbsBlock);
        cbsBlock->base = limit;
        CBSBlockShrink(cbs, cbsBlock, oldSize);
        res = CBSBlockNew(cbs, oldBase, base);
        if(res != ResOK)
          goto failNew;
      }
    }
  }

  CBSLeave(cbs);
  return ResOK;

failNew:
failDelete: 
failLimitCheck:
failSplayTreeSearch:
  AVER(res != ResOK);
  CBSLeave(cbs);
  return res;
}

Res CBSBlockDescribe(CBSBlock block, mps_lib_FILE *stream) {
  Res res;

  /* See .enter-leave.simple. */

  AVER(stream != NULL);

  res = WriteF(stream,
               "[$P,$P)", (WriteFP)block->base, (WriteFP)block->limit,
               NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}

static Res CBSSplayNodeDescribe(SplayNode splayNode, 
                                mps_lib_FILE *stream) {
  Res res;
  CBSBlock cbsBlock;

  AVER(splayNode != NULL);
  AVER(stream != NULL);

  cbsBlock = CBSBlockOfSplayNode(splayNode);

  res = CBSBlockDescribe(cbsBlock, stream);

  if(res != ResOK)
    return res;

  return ResOK;
}


/* CBSIterate -- Iterate all blocks in CBS
 *
 * This is not necessarily efficient.
 *
 * See design.mps.cbs.function.cbs.iterate.
 */

/* Internal version for enter/leave checking. */
static void CBSIterateInternal(CBS cbs, CBSIterateMethod iterate,
                               void *closureP, unsigned long closureS) {
  SplayNode splayNode;
  SplayTree splayTree;
  CBSBlock cbsBlock;

  AVERT(CBS, cbs);
  AVER(FUNCHECK(iterate));

  splayTree = SplayTreeOfCBS(cbs);
  splayNode = SplayTreeFirst(splayTree, NULL);
  while(splayNode != NULL) {
    cbsBlock = CBSBlockOfSplayNode(splayNode);
    if(!(*iterate)(cbs, cbsBlock, closureP, closureS)) {
      break;
    }
    splayNode = SplayTreeNext(splayTree, splayNode, 
                              KeyOfCBSBlock(cbsBlock));
  }
  return;
}

void CBSIterate(CBS cbs, CBSIterateMethod iterate,
                void *closureP, unsigned long closureS) {
  AVERT(CBS, cbs);
  AVER(FUNCHECK(iterate));

  CBSEnter(cbs);

  CBSIterateInternal(cbs, iterate, closureP, closureS);

  CBSLeave(cbs);

  return;
}


/* CBSIterateLarge -- Iterate only large blocks
 *
 * This function iterates only blocks that are larger than or equal
 * to the minimum size.
 */

typedef struct CBSIterateLargeClosureStruct {
  void *p;
  unsigned long s;
  CBSIterateMethod f;
} CBSIterateLargeClosureStruct, *CBSIterateLargeClosure;

static Bool CBSIterateLargeAction(CBS cbs, CBSBlock block, 
                                  void *p, unsigned long s) {
  Bool b = TRUE;
  CBSIterateLargeClosure closure;

  closure = (CBSIterateLargeClosure)p;
  AVER(closure != NULL);
  AVER(s == (unsigned long)0);

  if (CBSBlockSize(block) >= cbs->minSize)
    b = (closure->f)(cbs, block, closure->p, closure->s);

  return b;
}


void CBSIterateLarge(CBS cbs, CBSIterateMethod iterate,
                     void *closureP, unsigned long closureS) {
  CBSIterateLargeClosureStruct closure;

  CBSEnter(cbs);

  AVERT(CBS, cbs);
  AVER(FUNCHECK(iterate));

  closure.p = closureP;
  closure.s = closureS;
  closure.f = iterate;

  CBSIterateInternal(cbs, &CBSIterateLargeAction, 
                     (void *)&closure, (unsigned long)0);

  CBSLeave(cbs);
  return;
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

static Bool CBSSetMinSizeGrow(CBS cbs, CBSBlock block,
                              void *p, unsigned long s) {
  CBSSetMinSizeClosure closure;
  Size size;
  
  UNUSED(s);
  closure = (CBSSetMinSizeClosure)p;
  AVER(closure->old > closure->new);
  size = CBSBlockSize(block);
  if(size < closure->old && size >= closure->new)
    (*cbs->new)(cbs, block, size, size);

  return TRUE;
}

static Bool CBSSetMinSizeShrink(CBS cbs, CBSBlock block,
                                void *p, unsigned long s) {
  CBSSetMinSizeClosure closure;
  Size size;
  
  UNUSED(s);
  closure = (CBSSetMinSizeClosure)p;
  AVER(closure->old < closure->new);
  size = CBSBlockSize(block);
  if(size >= closure->old && size < closure->new)
    (*cbs->delete)(cbs, block, size, size);

  return TRUE;
}

void CBSSetMinSize(CBS cbs, Size minSize) {
  CBSSetMinSizeClosureStruct closure;

  CBSEnter(cbs);

  AVERT(CBS, cbs);

  closure.old = cbs->minSize;
  closure.new = minSize;

  if(minSize < cbs->minSize)
    CBSIterateInternal(cbs, &CBSSetMinSizeGrow, 
                       (void *)&closure, (unsigned long)0);
  else if(minSize > cbs->minSize)
    CBSIterateInternal(cbs, &CBSSetMinSizeShrink, 
                       (void *)&closure, (unsigned long)0);

  cbs->minSize = minSize;

  CBSLeave(cbs);
}


Bool CBSFindFirst(Addr *baseReturn, Addr *limitReturn,
                  CBS cbs, Size size) 
{
  Bool found;
  SplayNode node;
  CBSBlock block;

  CBSEnter(cbs);

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(CBS, cbs);
  AVER(size > 0);
  AVER(sizeof(unsigned long) >= sizeof(Size));

  found = SplayFindFirst(&node, SplayTreeOfCBS(cbs), &CBSTestNode,
                         &CBSTestTree, NULL, (unsigned long)size);
  if(found) {
    block = CBSBlockOfSplayNode(node);
    AVER(CBSBlockSize(block) >= size);
    *baseReturn = CBSBlockBase(block);
    *limitReturn = CBSBlockLimit(block);
  }

  CBSLeave(cbs);
  return found;
}


Bool CBSFindLast(Addr *baseReturn, Addr *limitReturn,
                 CBS cbs, Size size) 
{
  Bool found;
  SplayNode node;
  CBSBlock block;

  CBSEnter(cbs);

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(CBS, cbs);
  AVER(size > 0);
  AVER(sizeof(unsigned long) >= sizeof(Size));

  found = SplayFindLast(&node, SplayTreeOfCBS(cbs), &CBSTestNode,
                        &CBSTestTree, NULL, (unsigned long)size);
  if(found) {
    block = CBSBlockOfSplayNode(node);
    AVER(CBSBlockSize(block) >= size);
    *baseReturn = CBSBlockBase(block);
    *limitReturn = CBSBlockLimit(block);
  }

  CBSLeave(cbs);
  return found;
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
               "  blockPool: $P\n", (WriteFP)cbs->blockPool,
               "  new: $F ", (WriteFF)cbs->new,
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
