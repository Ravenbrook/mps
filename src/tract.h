/* impl.h.tract: TRACT INTERFACE
 *
 * $HopeName$
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 */


#ifndef tract_h
#define tract_h

#include "mpmtypes.h"


/* TractStruct -- tract structure
 *
 * .tract: Tracts represent the grains of memory allocation from
 * the arena.  See design.mps.arena.
 * 
 * .bool: The hasSeg field is a boolean, but can't be represented
 * as type Bool. See design.mps.arena.tract.field.hasSeg.
 */

typedef struct TractStruct { /* Tract structure */
  Pool pool;      /* MUST BE FIRST (design.mps.arena.tract.field pool) */
  void *p;                    /* pointer for use of owning pool */
  Addr base;                  /* Base address of the tract */
  TraceSet white : TRACE_MAX; /* traces for which tract is white */
  unsigned int hasSeg : 1;    /* does tract have a seg in p? See .bool */
} TractStruct;


extern Addr (TractBase)(Tract tract);
#define TractBase(tract)        ((tract)->base)
extern Addr TractLimit(Tract tract);

extern Tract TractOfBaseAddr(Arena arena, Addr addr);
extern Bool TractOfAddr(Tract *tractReturn, Arena arena, Addr addr);
/* TractOfAddr macro, see design.mps.trace.fix.tractofaddr */
#define TRACT_OF_ADDR(tractReturn, arena, addr) \
  ((*(arena)->class->tractOfAddr)(tractReturn, arena, addr))

extern Bool TractFirst(Tract *tractReturn, Arena arena);
extern Bool TractNext(Tract *tractReturn, Arena arena, Addr addr);
extern Tract TractNextContig(Arena arena, Tract tract);
/* Macro version of TractNextContig for use in iteration macros */
#define TRACT_NEXT_CONTIG(arena, tract) \
  (*(arena)->class->tractNextContig)(arena, tract)

extern Bool TractCheck(Tract tract);
extern void TractInit(Tract tract, Pool pool, Addr base);
extern void TractFinish(Tract tract);

#define TractPool(tract)        ((tract)->pool)
#define TractP(tract)           ((tract)->p)
#define TractSetP(tract, pp)    ((void)((tract)->p = (pp)))
#define TractHasSeg(tract)      ((Bool)(tract)->hasSeg)
#define TractSetHasSeg(tract, b) ((void)((tract)->hasSeg = (b)))
#define TractWhite(tract)       ((tract)->white)
#define TractSetWhite(tract, w) ((void)((tract)->white = w))


/* TRACT_*SEG -- Test / set / unset seg->tract associations
 *
 * These macros all multiply evaluate the tract parameter 
 */

#define TRACT_SEG(segReturn, tract) \
  (TractHasSeg(tract) && ((*(segReturn) = (Seg)TractP(tract)), TRUE))

#define TRACT_SET_SEG(tract, seg) \
  (TractSetHasSeg(tract, TRUE), TractSetP(tract, seg))

#define TRACT_UNSET_SEG(tract) \
  (TractSetHasSeg(tract, FALSE), TractSetP(tract, NULL))


/* .tract.tract.for: See design.mps.arena.tract.tract.for */
/* paremeters arena & limit are evaluated multiple times */
#define TRACT_TRACT_FOR(tract, addr, arena, firstTract, limit)     \
  for((tract = (firstTract)), (addr = TractBase(tract)); \
      tract != NULL;  \
      (addr = AddrAdd((addr),(arena)->alignment)), \
      ((addr < (limit)) ? \
         (tract = TRACT_NEXT_CONTIG((arena), tract)) : \
         (tract = NULL) /* terminate loop */))

/* .tract.for: See design.mps.arena.tract.for */
/* paremeters arena & limit are evaluated multiple times */
#define TRACT_FOR(tract, addr, arena, base, limit)     \
  for((tract = TractOfBaseAddr((arena), (base))), (addr = (base)); \
      tract != NULL;  \
      (addr = AddrAdd((addr),(arena)->alignment)), \
      ((addr < (limit)) ? \
         (tract = TRACT_NEXT_CONTIG((arena), tract)) : \
         (tract = NULL) /* terminate loop */))


#endif /* tract_h */
