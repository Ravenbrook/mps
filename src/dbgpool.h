/* impl.h.dbgpool: POOL DEBUG MIXIN
 *
 * $HopeName$
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 */

#include "mpm.h"
#include <stdarg.h>


typedef void (*TagInitMethod)(void* tag, va_list args);


typedef struct PoolDebugOptionsStruct {
  void* fenceTemplate;
  Size  fenceSize;
  TagInitMethod tagInit;
} PoolDebugOptionsStruct;

typedef PoolDebugOptionsStruct *PoolDebugOptions;


#define PoolDebugMixinSig ((Sig)0x519B0DB9)  /* SIGnature POol DeBuG */

typedef struct PoolDebugMixinStruct {
  Sig   sig;
  Addr  fenceTemplate;
  Size  fenceSize;
  TagInitMethod tagInit;
} PoolDebugMixinStruct;

typedef struct PoolDebugMixinStruct *PoolDebugMixin;


extern Bool PoolDebugMixinCheck(PoolDebugMixin dbg);

extern Res DebugPoolInit(PoolDebugMixin dbg, Pool pool, va_list args);
extern void DebugPoolFinish(PoolDebugMixin dbg, Pool pool);
extern Res DebugPoolAlloc(Addr *aReturn, PoolDebugMixin dbg, Pool pool,
                          Size size, Bool withReservoirPermit);
extern void DebugPoolFree(PoolDebugMixin dbg,
                          Pool pool, Addr old, Size size);
