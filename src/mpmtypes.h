/* impl.h.mpmtypes: MEMORY POOL MANAGER TYPES
 *
 * $HopeName$
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .rationale: Types and type constants are almost all defined
 * in this header, in advance of any declarations of prototypes
 * or structures.  This avoids difficulties in defining recursive
 * data structures, and also provides a nice browsable list of
 * types.
 */

#ifndef mpmtypes_h
#define mpmtypes_h

#include "std.h"
#include "lib.h"	/* ANSI library support */

typedef MPS_T_WORD Word;		/* machine word */
typedef unsigned char Byte;		/* byte */
typedef Word Addr;
typedef Word Size;
typedef Word Align;
typedef unsigned Shift;
typedef struct RingStruct *Ring;
typedef struct BufferStruct *Buffer;
typedef struct ApStruct *Ap;
typedef struct FormatStruct *Format;
typedef struct LDStruct *LD;
typedef Size Epoch;
typedef struct LockStruct *Lock;
typedef struct PoolStruct *Pool;
typedef struct SpaceStruct *Space;
typedef struct PoolClassStruct *PoolClass;
typedef Addr Ref;
typedef Addr RefSet;
typedef int Rank;
typedef struct TraceStruct *Trace;
typedef unsigned int TraceId;
typedef unsigned int TraceSet;
typedef struct ScanStateStruct *ScanState;
typedef struct SegStruct *Seg;
typedef struct ArenaStruct *Arena;
typedef unsigned ProtMode;
typedef struct VMStruct *VM;
typedef struct RootStruct *Root;
typedef struct ThreadStruct *Thread;
typedef int RootVar;
typedef unsigned Serial;

typedef Res (*RootScanMethod)   (ScanState ss, void *p, size_t s);
typedef Res (*RootScanRegMethod)(ScanState ss, Thread thread, void *p);

typedef Res  (*BufferFillMethod)(Addr *pReturn, Buffer buffer, Size size);
typedef Bool (*BufferTripMethod)(Buffer buffer, Addr p, Size size);

typedef Res  (*PoolMethodCreate)       (Pool *poolReturn, Space space, va_list arg);
typedef void (*PoolMethodDestroy)      (Pool pool);
typedef Res  (*PoolMethodAlloc)        (Addr *pReturn, Pool pool, Size size);
typedef void (*PoolMethodFree)         (Pool pool, Addr old, Size size);
typedef Res  (*PoolMethodBufferCreate) (Buffer *bufReturn, Pool pool);
typedef void (*PoolMethodBufferDestroy)(Pool pool, Buffer buf);
typedef Res  (*PoolMethodBufferFill)   (Addr *baseReturn, Pool pool, Buffer buffer, Size size);
typedef Bool (*PoolMethodBufferTrip)   (Pool pool, Buffer buffer, Addr base, Size size);
typedef void (*PoolMethodBufferExpose) (Pool pool, Buffer buffer);
typedef void (*PoolMethodBufferCover)  (Pool pool, Buffer buffer);
typedef Res  (*PoolMethodDescribe)     (Pool pool, Lib_FILE *stream);
typedef Res  (*PoolMethodCondemn)      (RefSet *condemnedReturn, Pool pool, Space space, TraceId ti);
typedef void (*PoolMethodGrey)         (Pool pool, Space space, TraceId ti);
typedef Res  (*PoolMethodScan)         (ScanState ss, Pool pool, Bool *finishedReturn);
typedef Res  (*PoolMethodFix)          (Pool pool, ScanState ss, Seg seg, Ref *refIO);
typedef void (*PoolMethodReclaim)      (Pool pool, Space space, TraceId ti);
typedef void (*PoolMethodAccess)       (Pool pool, Seg seg, ProtMode mode);

typedef Res  (*FormatScanMethod)   (ScanState ss, Addr base, Addr limit);
typedef Addr (*FormatSkipMethod)   (Addr object);
typedef void (*FormatMoveMethod)   (Addr object, Addr to);
typedef Addr (*FormatIsMovedMethod)(Addr object);
typedef void (*FormatCopyMethod)   (Addr object, Addr to);
typedef void (*FormatPadMethod)    (Addr base, Size size);

#define ProtNONE        ((ProtMode)0)
#define ProtREAD        ((ProtMode)(1<<0))
#define ProtWRITE       ((ProtMode)(1<<1))
#define RingNone	((Ring)0)
#define TraceIdNone	((TraceId)-1)
#define TraceSetEmpty	((TraceSet)0)

enum {				/* rank constants */
  RankAMBIG,			/* ambiguous reference */
  RankEXACT,			/* exact strong reference */
  RankWEAK,			/* exact weak reference */
  RankFINAL,			/* exact strong notifying reference */
  RankMAX			/* the number of ranks, not a rank */
};

enum {				/* root variants */
  RootFUN,			/* function closure */
  RootTABLE,			/* table */
  RootREG,			/* register */
  RootFMT			/* formatted */
};

#endif /* mpmtypes_h */
