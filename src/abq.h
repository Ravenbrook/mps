/* impl.h.abq: ABQ INTERFACE
 *
 * $HopeName: MMsrc!abq.h(MMdevel_gavinm_splay.2) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 */

/* temporary -- goes in mpm.h */


#ifndef abq_h
#define abq_h

#include "mpm.h"
#include "meter.h"

/* mpmtypes.h */
typedef struct ABQStruct *ABQ;
extern Bool ABQCheck(ABQ abq);
extern Res ABQDescribe(ABQ abq, mps_lib_FILE *stream);
extern Res ABQInit(Arena arena, ABQ abq, Count count);
extern Bool ABQIsEmpty(ABQ abq);
extern Count ABQDepth(ABQ abq);
extern void ABQFinish(Arena arena, ABQ abq);
extern Res ABQPop(ABQ abq, CBSBlock *blockReturn);
extern Res ABQPeek(ABQ abq, CBSBlock *blockReturn);
extern Res ABQPush(ABQ abq, CBSBlock block);
extern Res ABQDelete(ABQ abq, CBSBlock block);

/* mpmst.h */
typedef struct ABQStruct
{
  Count count;
  Index head, tail;
  CBSBlock *queue;

  /* Meter queue depth at each operation */
  METER_DECL(push);
  METER_DECL(pop);
  METER_DECL(peek);
  METER_DECL(delete);
  
  Sig sig;
}ABQStruct;

#endif /* abq_h */

