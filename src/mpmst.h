/* impl.h.mpmst: MEMORY POOL MANAGER DATA STRUCTURES
 *
 * $HopeName: MMsrc!mpmst.h(MMdevel_restr.4) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .rationale: Almost all MPM data structures are defined in this
 * header, or in headers selected from here.  Most structures have
 * already been partially declared in impl.h.mpmtypes.  This
 * organization means that there is an easily browsable view of the
 * data structures, and that it is easy to experiment.
 */

#ifndef mpmst_h
#define mpmst_h

#include "mpmtypes.h"

#if defined(MPS_OS_W3)
#include <windows.h>
#endif /* MPS_OS_w3 */


/* RingStruct -- double-ended queue structure
 *
 * See impl.c.ring.
 */

typedef struct RingStruct {     /* double-ended queue structure */
  Ring next, prev;              /* links to next and prev element */
} RingStruct;


/* PoolClassStruct -- pool class structure */

#define PoolClassSig    ((Sig)0x519C1A55)

typedef struct PoolClassStruct {
  Sig sig;                      /* impl.h.misc.sig */
  const char *name;             /* class name string */
  size_t size;                  /* size of instance structure */
  size_t offset;                /* offset of PoolStruct in instance */
  PoolMethodCreate create;
  PoolMethodDestroy destroy;
  PoolMethodAlloc alloc;
  PoolMethodFree free;
  PoolMethodBufferCreate bufferCreate;
  PoolMethodBufferDestroy bufferDestroy;
  PoolMethodBufferFill bufferFill;
  PoolMethodBufferTrip bufferTrip;
  PoolMethodBufferExpose bufferExpose;
  PoolMethodBufferCover bufferCover;
  PoolMethodCondemn condemn;
  PoolMethodGrey grey;
  PoolMethodScan scan;
  PoolMethodFix fix;
  PoolMethodReclaim reclaim;
  PoolMethodAccess access;
  PoolMethodDescribe describe;
} PoolClassStruct;


/* PoolStruct -- pool instance structure
 *
 * Each pool is owned by a space, and owns buffers and segments.
 */

#define PoolSig         ((Sig)0x519B0011)

typedef struct PoolStruct {     /* Pool instance structure */
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->poolSerial */
  PoolClass class;              /* pool class structure */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* link in list of pools in space */
  RingStruct segRing;           /* segs can be attached to the pool */
  RingStruct bufferRing;        /* allocation buffers are attached to pool */
  Serial bufferSerial;          /* serial of next buffer */
  Align alignment;              /* alignment modulus for units */
} PoolStruct;


/* MFSStruct -- MFS pool instance structure
 *
 * The MFS pool instance structure is declared here because it is in-lined
 * in the control pool structure which is in-lined in the space.  Normally,
 * pool instance structures are declared with the pools.
 */

#define MFSSig          ((Sig)0x5193F5B1)

typedef struct MFSStruct {      /* MFS instance structure */
  PoolStruct poolStruct;        /* generic pool structure */
  Size unroundedUnitSize;       /* the unit size requested */
  Size extendBy;                /* segment size rounded using unitSize */
  Size unitSize;                /* rounded for management purposes */
  unsigned unitsPerSeg;         /* number of units per segment */
  struct MFSHeaderStruct *freeList; /* head of the free list */
  Seg segList;                  /* the first segment */
  Sig sig;                      /* impl.h.misc.sig */
} MFSStruct;


/* MVStruct -- MV pool instance structure
 *
 * The MV pool instance structure is declared here because it is the
 * control pool structure which is in-lined in the space.  Normally,
 * pool instance structures are declared with the pools.
 */

#define MVSig           ((Sig)0x519E3FEE)

typedef struct MVStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  MFSStruct blockPoolStruct;    /* for managing block descriptors */
  MFSStruct spanPoolStruct;     /* for managing span descriptors */
  Size extendBy;                /* segment size to extend pool by */
  Size avgSize;                 /* client estimate of allocation size */
  Size maxSize;                 /* client estimate of maximum size */
  Size space;                   /* total free space in pool */
  Size lost;                    /* lost because free couldn't allocate(!) */
  struct MVSpanStruct *spans;   /* span chain */
  Sig sig;                      /* impl.h.misc.sig */
} MVStruct;


/* VMStruct -- virtual memory structure */

#define VMSig   ((Sig)0x519FEE33)

#if defined(MPS_OS_W3)

typedef struct VMStruct {       /* Win32 VM structure */
  Sig sig;                      /* impl.h.misc.sig */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#elif defined(MPS_OS_O1) || defined(MPS_OS_S7)

typedef struct VMStruct {       /* ANSI fake VM structure */
  Sig sig;                      /* impl.h.misc.sig */
  Addr base, limit;             /* boundaries of malloc'd memory */
  void *block;                  /* pointer to malloc'd block, for free() */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#elif defined(MPS_OS_SU)

typedef struct VMStruct {       /* SunOS 4 VM structure */
  Sig sig;                      /* impl.h.misc.sig */
  int zero_fd;                  /* file descriptor for /dev/zero */
  int none_fd;                  /* fildes used for PROT_NONE (/etc/passwd) */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
} VMStruct;

#else
#error "No definition of VMStruct for this OS."
#endif


/* SegStruct -- segment structure
 *
 * .seg.def: Segments are the basic units of memory allocation from
 * the arena, and also the units of scanning, shielding, and colour
 * for the MPM (pool classes may subdivide segments and have a finer
 * grained idea of colour, for example).
 */

typedef struct SegStruct {      /* segment structure */
  Pool pool;                    /* owner, MUST BE FIRST, impl.c.arenvm.page */
  Bool single;                  /* single page segment */
  ProtMode pm, sm;              /* protection and shield modes */
  Size depth;                   /* see impl.c.shield.def.depth */
  void *p;                      /* pointer for use of pool */
  TraceId condemned;            /* seg condemned? for which trace? */
} SegStruct;


/* ArenaStruct -- arena structure
 *
 * .def: The arena structure is in-lined in the space structure
 * (impl.h.mpmst.space).  This is the arena structure used by
 * the virtual memory based arena implementation, impl.c.arenavm.
 */

#define ArenaSig        ((Sig)0x519A7E9A)

typedef struct PageStruct *Page;/* page type */
typedef Word *BT;               /* bool table type */

typedef struct ArenaStruct {    /* arena structure */
  Sig sig;                      /* impl.h.misc.sig */
  VMStruct vmStruct;            /* virtual memory structure */
  Addr base;                    /* base address of arena area */
  Addr limit;                   /* limit address of arena area */
  Size pageSize;                /* size of block managed by page_s */
  Shift pageShift;              /* log2 of page size, for shifts */
  Size pages;                   /* number of pages in table */
  Page pageTable;               /* the page table */
  BT freeTable;                 /* page free table */
  Size tablesSize;              /* size of area occupied by tables */
  Size tablePages;              /* number of pages occupied by tables */
} ArenaStruct;


/* ApStruct -- allocation point structure
 *
 * The allocation point is exported to the client code so that it can
 * in-line buffered allocation.
 *
 * .ap: This structure must match impl.h.mps.ap.
 * See also impl.c.mpsi.check.ap.
 */

typedef struct ApStruct {
  Addr init;                    /* limit of initialized area */
  Addr alloc;                   /* limit of allocated area */
  Addr limit;                   /* limit of allocation buffer */
} ApStruct;


/* BufferStruct -- allocation buffer structure
 *
 * The buffer contains an AP which is exported to the client.
 */

#define BufferSig       ((Sig)0x519B0FFA)

typedef struct BufferStruct {
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from pool->bufferSerial */
  Space space;                  /* owning space */
  Pool pool;                    /* owning pool */
  Seg seg;                      /* segment being buffered */
  Addr base;                    /* base address of allocation buffer */
  ApStruct ap;                  /* the allocation point */
  Align alignment;              /* allocation alignment */
  Bool exposed;                 /* is buffer memory exposed? */
  RingStruct poolRing;          /* buffers are attached to pools */
  ProtMode shieldMode;          /* shielding for allocated memory */
  TraceSet grey;                /* colour for allocated memory */
  void *p; int i;               /* closure variables */
} BufferStruct;


/* FormatStruct -- object format structure */

#define FormatSig       ((Sig)0x519F43A2)

typedef struct FormatStruct {
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->formatSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* link in list of formats in space */
  Align alignment;              /* alignment of formatted objects */
  FormatScanMethod scan;
  FormatSkipMethod skip;
  FormatMoveMethod move;
  FormatIsMovedMethod isMoved;
  FormatCopyMethod copy;
  FormatPadMethod pad;
} FormatStruct;


/* LDStruct -- location dependency structure
 *
 * A version of this structure is exported to the client.
 * See impl.h.mps.ld and impl.c.mpsi.check.ld.
 */

typedef struct LDStruct {
  Epoch epoch;
  RefSet rs;
} LDStruct;


/* LockStruct and ThreadStruct -- locking and thread structures */

#define LockSig         ((Sig)0x519110CC)
#define ThreadSig       ((Sig)0x51924EAD)

#if defined(MPS_OS_W3)

typedef struct LockStruct {     /* Win32 lock structure */
  Sig sig;                      /* impl.h.misc.sig */
  unsigned long claims;         /* # claims held by the owning thread */
  CRITICAL_SECTION cs;          /* Win32's recursive lock thing */
} LockStruct;

typedef struct ThreadStruct {   /* Win32 thread structure */
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->threadSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* threads attached to space */
  HANDLE handle;                /* Handle of thread .thread.handle */
  DWORD id;                     /* Thread id of thread */
} ThreadStruct;

#elif defined(MPS_OS_SU) || defined(MPS_OS_O1) || defined(MPS_OS_S7)

typedef struct LockStruct {     /* ANSI fake lock structure */
  Sig sig;                      /* impl.h.misc.sig */
  unsigned long claims;         /* # claims held by owner */
} LockStruct;

typedef struct ThreadStruct {   /* ANSI fake thread structure */
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->threadSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* attaches to space */
} ThreadStruct;

#else
#error "No definition of LockStruct or ThreadStruct for this OS."
#endif


/* RootStruct -- tracing root structure */

#define RootSig         ((Sig)0x51940022)

typedef struct RootStruct {
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from space->rootSerial */
  Space space;                  /* owning space */
  RingStruct spaceRing;         /* attachment to space */
  Rank rank;                    /* rank of references in this root */
  TraceSet marked;              /* marked but not scanned for per trace */
  RootVar var;                  /* union discriminator */
  union RootUnion {
    struct {
      RootScanMethod scan;      /* the function which does the scanning */
      void *p;                  /* environment for f */
      size_t s;                 /* environment for f */
    } fun;
    struct {
      Addr *base;               /* first reference in table */
      Addr *limit;              /* last reference, plus one */
    } table;
    struct {
      RootScanRegMethod scan;
      Thread thread;
      void *p;
    } reg;
    struct {
      FormatScanMethod scan;
      Addr base, limit;
    } fmt;
  } the;
} RootStruct;


/* Scan State
 *
 * The first four fields of the trace structure must match the
 * external scan state structure (mps_ss_s) thus:
 *   ss->fix            mps_ss->fix
 *   ss->zoneShift      mpm_ss->w0
 *   ss->condemned      mpm_ss->w1
 *   ss->summary        mpm_ss->w2
 * See impl.h.mps.ss and impl.c.mpsi.check.ss.
 * .ss.zone: The zoneShift field is therefore declared as Word
 * rather than Shift.
 */

#define ScanStateSig    ((Sig)0x5195CA95)

typedef struct ScanStateStruct {
  Res (*fix)(ScanState mps_ss, Addr *ref_io);
  Word zoneShift;
  RefSet condemned;             /* condemned set, for inline fix test */
  RefSet summary;               /* accumulated summary of scanned references */
  Sig sig;                      /* impl.h.misc.sig */
  Space space;                  /* owning space */
  TraceId traceId;              /* trace ID of scan */
  Rank rank;                    /* reference rank of scanning */
} ScanStateStruct;

typedef struct TraceStruct {
  RefSet condemned;
} TraceStruct;


/* SpaceStruct -- the space structure
 *
 * .space: The space structure is the top-level state of the
 * MPS, and as such contains a lot of fields which are considered
 * "global".  These fields belong to different modules.  The module
 * which owns each group of fields is commented.
 */

#define SpaceSig        ((Sig)0x5195BACE)

typedef struct SpaceStruct {
  /* space fields (impl.c.space) */
  Sig sig;                      /* impl.h.misc.sig */
  Serial serial;                /* from static spaceSerial */
  RingStruct globalRing;        /* node in global ring of spaces */
  Bool poolReady;               /* has pool been initialized? */
  MVStruct controlPoolStruct;   /* pool for miscellaneous items */
  LockStruct lockStruct;        /* space's lock */
  Size pollThreshold;           /* see SpacePoll() */
  Bool insidePoll;              /* prevent recursive polling */

  /* arena fields (impl.c.arena*) */
  ArenaStruct arenaStruct;      /* the arena */

  /* pool fields (impl.c.pool) */
  RingStruct poolRing;          /* list of pools in space */
  Serial poolSerial;            /* serial of next pool */

  /* root fields (impl.c.root) */
  RingStruct rootRing;          /* ring of roots attached to space */
  Serial rootSerial;            /* serial of next root */

  /* format fields (impl.c.format) */
  RingStruct formatRing;        /* ring of formats attached to space */
  Serial formatSerial;          /* serial of next format */

  /* thread fields (impl.c.thread) */
  RingStruct threadRing;        /* ring of attached threads */
  Serial threadSerial;          /* serial of next thread */
  
  /* sheild fields (impl.c.shield) */
  Bool insideShield;             /* TRUE if inside shield */
  Seg shCache[SHIELD_CACHE_SIZE];/* Cache of unsynced segs */
  Size shCacheI;                 /* index into cache */
  Size shDepth;                  /* sum of depths of all segs */
  Bool suspended;                /* TRUE if mutator suspended */

  /* trace fields (impl.c.trace) */
  TraceSet busyTraces;          /* set of running traces */
  TraceStruct trace[TRACE_MAX]; /* trace structures */
  Shift zoneShift;              /* see ref.h */

  /* location dependeny fields (impl.c.ld) */
  Epoch epoch;                  /* current epoch */
  RefSet prehistory;            /* all-time history of movements */
  RefSet history[SPACE_LD_LENGTH]; /* history of object movements */
} SpaceStruct;


#endif /* mpmst_h */
