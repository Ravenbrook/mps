/* impl.c.abq: AVAILABLE BLOCK QUEUE
 *
 * $HopeName: $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * See design.mps.poolmv2.impl.c.abq
 */

#include "mpm.h"
#include "meter.h"

SRCID(abq, "$HopeName: $");


/* Signatures */
#define ABQSig ((Sig)0x519AB099) /* SIGnature ABQ */


/* Prototypes */
typedef struct ABQStruct *ABQ;
extern Bool ABQCheck(ABQ abq);
extern Res ABQDescribe(ABQ abq, mps_lib_FILE *stream);
extern Res ABQInit(Arena arena, ABQ abq, Count count);
extern Bool ABQIsEmpty(ABQ abq);
extern void ABQFinish(Arena arena, ABQ abq);
extern Res ABQPop(ABQ abq, CBSBlock *blockReturn);
extern Res ABQPeek(ABQ abq, CBSBlock *blockReturn);
extern Res ABQPush(ABQ abq, CBSBlock block);
extern Res ABQDelete(ABQ abq, CBSBlock block);


/* Structures */
typedef struct ABQStruct
{
  Count count;
  Index head, tail;
  CBSBlock *queue;
  Sig sig;
}ABQStruct;


/* Methods */
Bool ABQCheck(ABQ abq)
{
  Index index;
  
  CHECKS(ABQ, abq);
  CHECKL(abq->count > 0);
  CHECKL(abq->head >= 0);
  CHECKL(abq->head < abq->count);
  CHECKL(abq->tail >= 0);
  CHECKL(abq->tail < abq->count);
  CHECKL(abq->queue != NULL);
  /* Is this really a local check? */
  for (index = abq->head; index != abq->tail; ) {
    CHECKL(CBSBlockCheck(abq->queue[index]));
    if (++index == abq->count)
      index = 0;
  }

  return TRUE;
}


Res ABQDescribe(ABQ abq, mps_lib_FILE *stream)
{
  Res res;
  Index index;

  AVERT(ABQ, abq);

  AVER(stream != NULL);

  res = WriteF(stream,
	       "ABQ $P\n{\n", (WriteFP)abq,
	       "  count: $U \n", (WriteFU)abq->count,
	       "  head: $U \n", (WriteFU)abq->head,
	       "  tail: $U \n", (WriteFU)abq->tail,
               "  queue: \n",
	       NULL);
  if(res != ResOK)
    return res;

  for (index = abq->head; index != abq->tail; ) {
    res = CBSBlockDescribe(abq->queue[index], stream);
    if(res != ResOK)
      return res;
    if (++index == abq->count)
      index = 0;
  }

  res = WriteF(stream, "}\n", NULL);
  if(res != ResOK)
    return res;
  
  return ResOK;
}


static Size ABQqueueSize(Count count)
{
  /* strange but true: the sizeof expression calculates the size of a
     single queue element */
  return (Size)(sizeof(((ABQ)NULL)->queue[0]) * count);
}


/* ABQInit -- Initialize an ABQ */
Res ABQInit(Arena arena, ABQ abq, Count count)
{
  void *p;
  Res res;

  AVERT(Arena, arena);
  AVER(abq != NULL);
  AVER(count > 0);

  res = ArenaAlloc(&p, arena, ABQqueueSize(count));
  if (res != ResOK)
    return res;

  abq->count = count;
  abq->head = 0;
  abq->tail = 0;
  abq->queue = (CBSBlock *)p;
  
  abq->sig = ABQSig;

  AVERT(ABQ, abq);
  return ResOK;
}


Bool ABQIsEmpty(ABQ abq) 
{
  AVERT(ABQ, abq);

  return abq->head == abq->tail;
}


/* ABQFinish -- finish an ABQ */
void ABQFinish(Arena arena, ABQ abq)
{
  AVERT(Arena, arena);
  AVERT(ABQ, abq);
  /* must be empty */
  AVER(ABQIsEmpty(abq));

  ArenaFree(arena, abq->queue, ABQqueueSize(abq->count));
  
  abq->count = 0;
  abq->queue = NULL;
}


/* ABQPop -- pop a block from the head of the ABQ */
Res ABQPop(ABQ abq, CBSBlock *blockReturn)
{
  Index index;
  
  AVER(blockReturn != NULL);
  AVERT(ABQ, abq);

  index = abq->head;
  if (index == abq->tail)
    return ResFAIL;

  *blockReturn = abq->queue[index];
  AVERT(CBSBlock, *blockReturn);

  if (++index == abq->count)
    index = 0;
  abq->head = index;
  
  AVERT(ABQ, abq);
  return ResOK;
}


/* ABQPeek -- peek at the head of the ABQ */
Res ABQPeek(ABQ abq, CBSBlock *blockReturn)
{
  Index index;
  
  AVER(blockReturn != NULL);
  AVERT(ABQ, abq);

  index = abq->head;
  if (index == abq->tail)
    return ResFAIL;

  *blockReturn = abq->queue[index];
  AVERT(CBSBlock, *blockReturn);

  /* Identical to pop, but don't write index back into head */

  AVERT(ABQ, abq);
  return ResOK;
}


/* ABQPush -- push a block onto the tail of the ABQ */
Res ABQPush(ABQ abq, CBSBlock block)
{
  Index index;

  AVERT(ABQ, abq);
  AVERT(CBSBlock, block);

  index = abq->tail;
  if (++index == abq->count)
    index = 0;
  
  if (index == abq->head)
    return ResFAIL;

  abq->queue[abq->tail] = block;
  abq->tail = index;

  AVERT(ABQ, abq);
  return ResOK;
}


/* ABQDelete -- delete a block from the ABQ */
Res ABQDelete(ABQ abq, CBSBlock block)
{
  Index index, next, count, tail;
  CBSBlock *queue;
  Bool found = FALSE;

  AVERT(ABQ, abq);
  AVERT(CBSBlock, block);

  index = abq->head;
  tail = abq->tail;
  count = abq->count;
  queue = abq->queue;
  
  while (index != tail) {
    if (queue[index] == block) {
      goto found;
    }
    if (++index == count)
      index = 0;
  }

  return ResFAIL;

found:
  /* index points to the node to be removed */
  next = index;
  if (++next == count)
    next = 0;
  while (next != tail) {
    queue[index] = queue[next];
    index = next;
    if (++next == count)
      next = 0;
  }
  abq->tail = index;
  AVERT(ABQ, abq);
  return ResOK;
}

