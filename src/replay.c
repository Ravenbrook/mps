/* impl.c.replay: Allocation replayer
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName$
 */

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "eventcom.h"
#include "eventpro.h"
#include "mpmtypes.h"

#include "mps.h"
#include "mpsavm.h"
#include "mpsacl.h"
#include "mpscmv.h"
#include "mpscmvff.h"
#include "mpscepvm.h"
#include "fmtpstst.h"
#include "mpscepdl.h"

#include "table.h"

#include <stddef.h> /* for size_t */
#include <stdio.h> /* for printf */
#include <stdarg.h> /* for va_list */
#include <stdlib.h> /* for EXIT_FAILURE */
#include <assert.h> /* for assert */
#include <string.h> /* for strcmp */
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


typedef unsigned int uint;
typedef unsigned long ulong;


/* command-line arguments */

static Bool partialLog = FALSE;

static char *prog; /* program name */


/* Globals */

static ulong totalEvents; /* count of events */
static ulong discardedEvents; /* count of ignored events */
static ulong unknownEvents; /* count of unknown events */
static Word eventTime = 0; /* current event time */

/* Dictionaries for translating from log to replay values */
static Table arenaTable; /* dictionary of arenas */
static Table poolTable; /* dictionary of poolReps */
static Table apTable; /* dictionary of apReps */


/* poolSupport -- describes pool support for explicit deallocation */

enum {supportTruncate, supportFree, supportNothing};
typedef int poolSupport;


/* objectTable -- object address mapping structure
 *
 * .obj-mapping.truncate: Pools that support truncate need to keep track
 * of object end points as well.  .obj-mapping.partial-free: Arbitrary
 * partial free is not supported.
 */

typedef struct objectTableStruct {
  Table startTable;
  Table endTable;
} objectTableStruct;
typedef struct objectTableStruct *objectTable;


/* poolRep -- pool tracking structure
 *
 * .pool.object-addr: Pools that support explicit free (or truncate)
 * need to maintain a mapping from the addresses in the log to those in
 * the replay.
 */

typedef struct poolRepStruct {
  mps_pool_t pool; /* the replay pool */
  objectTable objects;
} poolRepStruct;
typedef struct poolRepStruct *poolRep;


/* apRep -- ap tracking structure */

typedef struct apRepStruct {
  mps_ap_t ap; /* the replay ap */
  objectTable objects; /* object mapping for the pool of this ap */
} apRepStruct;
typedef struct apRepStruct *apRep;


/* PointerAdd -- add offset to pointer */

#define PointerAdd(p, s) ((void *)((char *)(p) + (s)))
#define PointerSub(p, s) ((void *)((char *)(p) - (s)))


/* error -- error signalling */

static void error(const char *format, ...)
{
  va_list args;

  fflush(stdout); /* sync */
  fprintf(stderr, "%s: @%lu ", prog, (ulong)eventTime);
  va_start(args, format);
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
  va_end(args);
  exit(EXIT_FAILURE);
}


/* usage -- usage message */

static void usage(void)
{
  fprintf(stderr,
          "Usage: %s [-f logfile] [-p] [-?]\n"
          "See guide.mps.telemetry for instructions.\n",
          prog);
}


/* usageError -- explain usage and error */

static void usageError(void)
{
  usage();
  error("Bad usage");
}


/* parseArgs -- parse command line arguments, return log file name */

static char *parseArgs(int argc, char *argv[])
{
  char *name = "mpsio.log";
  int i = 1;
  
  if (argc >= 1)
    prog = argv[0];
  else
    prog = "unknown";
  
  while (i < argc) { /* consider argument i */
    if (argv[i][0] == '-') { /* it's an option argument */
      switch (argv[i][1]) {
      case 'f': /* file name */
        ++ i;
        if (i == argc)
          usageError();
        else   
          name = argv[i];
        break;   
      case 'p': /* partial log */
        partialLog = TRUE;
        break;
      case '?': case 'h': /* help */
        usage();
        break;
      default:
        usageError();
      }
    } /* if option */
    ++ i;
  }
  return name;
}


#ifdef MPS_PROD_EPCORE


/* ensurePSFormat -- return the PS format, creating it, if necessary */

static mps_fmt_t psFormat = NULL;

static void ensurePSFormat(mps_fmt_t *fmtOut, mps_arena_t arena)
{
  mps_res_t eres;

  if (psFormat == NULL) {
    eres = mps_fmt_create_A(&psFormat, arena, ps_fmt_A());
    assert(eres == MPS_RES_OK);
  }
  *fmtOut = psFormat;
}


/* finishPSFormat -- finish the PS format, if necessary */

static void finishPSFormat(void)
{
  if (psFormat != NULL)
    mps_fmt_destroy(psFormat);
}


#endif


/* objectTableCreate -- create an objectTable */

static objectTable objectTableCreate(poolSupport support)
{
  if (support != supportNothing) {
    Res ires;
    objectTable table;

    table = malloc(sizeof(objectTableStruct));
    assert(table != NULL);
    ires = TableCreate(&table->startTable, (size_t)1<<12);
    assert(ires == ResOK);
    if (support == supportTruncate) {
      ires = TableCreate(&table->endTable, (size_t)1<<12);
      assert(ires == ResOK);
    } else {
      table->endTable = NULL;
    }
    return table;
  } else {
    return NULL;
  }
}


/* objectTableDestroy -- destroy an objectTable */

static void objectTableDestroy(objectTable table)
{
  if (table != NULL) {
    TableDestroy(table->startTable);
    if (table->endTable != NULL)
      TableDestroy(table->endTable);
    free(table);
  }
}


/* objDefine -- add a new mapping to an objectTable */

static void objDefine(objectTable table,
                      void *logObj, void *obj, size_t size)
{
  if (table != NULL) {
    Res ires;

    ires = TableDefine(table->startTable, (Word)logObj, obj);
    assert(ires == ResOK);
    if (table->endTable != NULL) {
      ires = TableDefine(table->endTable,
                         (Word)PointerAdd(logObj, size),
                         PointerAdd(obj, size));
      assert(ires == ResOK);
    }
  }
}


/* objRemove -- look up and remove a mapping in an objectTable */

static void objRemove(void **objReturn, objectTable table,
                      void *logObj, size_t size)
{
  Bool found;
  Res ires;
  void *obj;
  void *end;
  void *logEnd;

  found = TableLookup(&obj, table->startTable, (Word)logObj);
  if (found) {
    ires = TableRemove(table->startTable, (Word)logObj);
    assert(ires == ResOK);
    if (table->endTable != NULL) {
      ires = TableRemove(table->endTable,
                         (Word)PointerAdd(logObj, size));
      assert(ires == ResOK);
    }
    *objReturn = obj;
    return;
  }
  /* Must be a truncation. */
  assert(table->endTable != NULL);
  logEnd = PointerAdd(logObj, size);
  found = TableLookup(&end, table->endTable, (Word)logEnd);
  assert(found);
  obj = PointerSub(end, size);
  /* Remove the old end and insert the new one. */
  ires = TableRemove(table->endTable, (Word)logEnd);
  assert(ires == ResOK);
  ires = TableDefine(table->endTable, (Word)logObj, obj);
  assert(ires == ResOK);
  *objReturn = obj;
  return;
}


/* poolRecreate -- create and record a pool */

static void poolRecreate(void *logPool, void *logArena,
                         mps_class_t class, poolSupport support, ...)
{
  va_list args;
  mps_pool_t pool;
  mps_res_t eres;
  poolRep rep;
  Res ires;
  void *entry;
  Bool found;

  found = TableLookup(&entry, arenaTable, (Word)logArena);
  assert(found);
  va_start(args, support);
  eres = mps_pool_create_v(&pool, (mps_arena_t)entry, class, args);
  assert(eres == MPS_RES_OK);
  va_end(args);
  rep = malloc(sizeof(poolRepStruct));
  assert(rep != NULL);
  rep->pool = pool;
  rep->objects = objectTableCreate(support);
  ires = TableDefine(poolTable, (Word)logPool, (void *)rep);
  assert(ires == ResOK);
}


/* poolRedestroy -- destroy and derecord a pool */

static void poolRedestroy(void *logPool)
{
  Res ires;
  void *entry;
  Bool found;
  poolRep rep;

  found = TableLookup(&entry, poolTable, (Word)logPool);
  assert(found);
  rep = (poolRep)entry;
  mps_pool_destroy(rep->pool);
  ires = TableRemove(poolTable, (Word)logPool);
  assert(ires == ResOK);
  objectTableDestroy(rep->objects);
  free(rep);
}


/* apRecreate -- create and record an ap */

static void apRecreate(void *logAp, void *logPool, ...)
{
  va_list args;
  mps_ap_t ap;
  poolRep pRep;
  apRep aRep;
  mps_res_t eres;
  Res ires;
  void *entry;
  Bool found;

  found = TableLookup(&entry, poolTable, (Word)logPool);
  assert(found);
  pRep = (poolRep)entry;
  va_start(args, logPool);
  eres = mps_ap_create_v(&ap, pRep->pool, args);
  assert(eres == MPS_RES_OK);
  va_end(args);
  aRep = malloc(sizeof(apRepStruct));
  assert(aRep != NULL);
  aRep->ap = ap;
  aRep->objects = pRep->objects;
  ires = TableDefine(apTable, (Word)logAp, (void *)aRep);
  assert(ires == ResOK);
}


/* apRedestroy -- destroy and derecord an ap */

static void apRedestroy(void *logAp)
{
  Res ires;
  void *entry;
  Bool found;
  apRep rep;

  found = TableLookup(&entry, apTable, (Word)logAp);
  assert(found);
  rep = (apRep)entry;
  mps_ap_destroy(rep->ap);
  ires = TableRemove(apTable, (Word)logAp);
  assert(ires == ResOK);
  free(rep);
}


/* processEvent -- process event */

static void processEvent(EventProc proc, Event event, Word etime)
{
  mps_res_t eres;
  Res ires;
  Bool found;
  void *entry;
  static arenaJustCreated = FALSE;

  EventRecord(proc, event, etime);
  switch(event->any.code) {
  case EventArenaCreateVM: { /* arena, userSize, chunkSize */
    mps_arena_t arena;

    eres = mps_arena_create(&arena, mps_arena_class_vm(),
                            event->pww.w1);
    assert(eres == MPS_RES_OK);
    ires = TableDefine(arenaTable, (Word)event->pww.p0, (void *)arena);
    assert(ires == ResOK);
    arenaJustCreated = TRUE;
  } break;
  case EventArenaCreateVMNZ: { /* arena, userSize, chunkSize */
    mps_arena_t arena;

    eres = mps_arena_create(&arena, mps_arena_class_vmnz(),
                            event->pww.w1);
    assert(eres == MPS_RES_OK);
    ires = TableDefine(arenaTable, (Word)event->pww.p0, (void *)arena);
    assert(ires == ResOK);
    arenaJustCreated = TRUE;
  } break;
  case EventArenaCreateCL: { /* arena, size, base */
    mps_arena_t arena;
    void *base;

    base = malloc((size_t)event->pwa.w1);
    assert(base != NULL);
    eres = mps_arena_create(&arena, mps_arena_class_cl(),
                            (Size)event->pwa.w1, base);
    assert(eres == MPS_RES_OK);
    ires = TableDefine(arenaTable, (Word)event->pw.p0, (void *)arena);
    assert(ires == ResOK);
    arenaJustCreated = TRUE;
  } break;
  case EventArenaDestroy: { /* arena */
    found = TableLookup(&entry, arenaTable, (Word)event->p.p0);
    assert(found);
#ifdef MPS_PROD_EPCORE
    /* @@@@ assuming there's only one arena at a time */
    finishPSFormat();
#endif
    mps_arena_destroy((mps_arena_t)entry);
    ires = TableRemove(arenaTable, (Word)event->pw.p0);
    assert(ires == ResOK);
  } break;
  case EventPoolInitMVFF: {
    /* pool, arena, extendBy, avgSize, align, slotHigh, arenaHigh, firstFit */
    poolRecreate(event->ppwwwuuu.p0, event->ppwwwuuu.p1,
                 mps_class_mvff(), supportFree,
                 (size_t)event->ppwwwuuu.w2,
                 (size_t)event->ppwwwuuu.w3,
                 (size_t)event->ppwwwuuu.w4,
                 (mps_bool_t)event->ppwwwuuu.u5,
                 (mps_bool_t)event->ppwwwuuu.u6,
                 (mps_bool_t)event->ppwwwuuu.u7);
  } break;
  case EventPoolInitMV: { /* pool, arena, extendBy, avgSize, maxSize */
    /* .pool.control: The control pool will get created just after */
    /* its arena; ignore it. */
    if (!arenaJustCreated) {
      poolRecreate(event->ppwww.p0, event->ppwww.p1,
                   mps_class_mv(), supportFree, (size_t)event->ppwww.w2,
                   (size_t)event->ppwww.w3, (size_t)event->ppwww.w4);
    } else {
      arenaJustCreated = FALSE;
    }
  } break;
  case EventPoolInitMFS: { /* pool, arena, extendBy, unitSize */
    /* internal only */
    ++discardedEvents;
  } break;
  case EventPoolInit: { /* pool, arena, class */
    /* all internal only */
    ++discardedEvents;
  } break;
#ifdef MPS_PROD_EPCORE
  case EventPoolInitEPVM: {
    /* pool, arena, format, maxSaveLevel, saveLevel */
    mps_arena_t arena;
    mps_fmt_t format;

    found = TableLookup(&entry, arenaTable, (Word)event->pppuu.p1);
    assert(found);
    arena = (mps_arena_t)entry;
    ensurePSFormat(&format, arena); /* We know what the format is. */
    poolRecreate(event->pppuu.p0, event->pppuu.p1,
                 mps_class_epvm(), supportNothing, format,
                 (mps_epvm_save_level_t)event->pppuu.u3,
                 (mps_epvm_save_level_t)event->pppuu.u4);
  } break;
  case EventPoolInitEPDL: {
    /* pool, arena, isEPDL, extendBy, avgSize, align */
    poolRecreate(event->ppuwww.p0, event->ppuwww.p1,
                 event->ppuwww.u2 ? mps_class_epdl() : mps_class_epdr(),
                 event->ppuwww.u2 ? supportTruncate : supportFree,
                 (size_t)event->ppuwww.w3, (size_t)event->ppuwww.w4,
                 (size_t)event->ppuwww.w5);
  } break;
#endif
  case EventPoolFinish: { /* pool */
    found = TableLookup(&entry, poolTable, (Word)event->p.p0);
    if (found) {
      poolRedestroy(event->p.p0);
    } else {
      ++discardedEvents;
    }
  } break;
  case EventBufferInit: { /* buffer, pool, isMutator */
    if ((Bool)event->ppu.u2) {
      found = TableLookup(&entry, poolTable, (Word)event->ppu.p1);
      if (found) {
        apRecreate(event->ppu.p0, event->ppu.p1);
      } else {
        ++discardedEvents;
      }
    } else {
      ++discardedEvents;
    }
  } break;
#ifdef MPS_PROD_EPCORE
  case EventBufferInitEPVM: { /* buffer, pool, isObj */
    found = TableLookup(&entry, poolTable, (Word)event->ppu.p1);
    if (found) {
      apRecreate(event->ppu.p0, event->ppu.p1,
                 (mps_bool_t)event->ppu.u2);
    } else {
      ++discardedEvents;
    }
  } break;
#endif
  case EventBufferFinish: { /* buffer */
    found = TableLookup(&entry, apTable, (Word)event->p.p0);
    if (found) {
      apRedestroy(event->p.p0);
    } else {
      ++discardedEvents;
    }
  } break;
  case EventBufferReserve: { /* buffer, init, size */
    found = TableLookup(&entry, apTable, (Word)event->paw.p0);
    if (found) {
      apRep rep = (apRep)entry;
      mps_addr_t p;

      eres = mps_reserve(&p, rep->ap, (size_t)event->paw.w2);
      assert(eres == MPS_RES_OK);
    } else {
      ++discardedEvents;
    }
  } break;
  case EventBufferCommit: { /* buffer, p, size, clientClass */
    found = TableLookup(&entry, apTable, (Word)event->pawa.p0);
    if (found) {
      apRep rep = (apRep)entry;
      mps_addr_t obj = rep->ap->init;
      mps_bool_t committed;
      size_t size = (size_t)event->pawa.w2;

      committed = mps_commit(rep->ap, obj, size);
      assert(committed);
      objDefine(rep->objects, event->pawa.a1, obj, size);
    } else {
      ++discardedEvents;
    }
  } break;
  case EventPoolAlloc: { /* pool, obj, size */
    found = TableLookup(&entry, poolTable, (Word)event->paw.p0);
    if (found) {
      poolRep rep = (poolRep)entry;
      void *obj;
      size_t size = (size_t)event->paw.w2;

      eres = mps_alloc(&obj, rep->pool, size);
      assert(eres == MPS_RES_OK);
      objDefine(rep->objects, event->paw.a1, obj, size);
    } else {
      ++discardedEvents;
    }
  } break;
  case EventPoolFree: { /* pool, obj, size */
    found = TableLookup(&entry, poolTable, (Word)event->paw.p0);
    if (found) {
      poolRep rep = (poolRep)entry;
      void *obj;
      size_t size = (size_t)event->paw.w2;

      objRemove(&obj, rep->objects, event->paw.a1, size);
      mps_free(rep->pool, obj, size);
    } else {
      ++discardedEvents;
    }
  } break;
  case EventPoolPush: { /* pool */
    found = TableLookup(&entry, poolTable, (Word)event->p.p0);
    if (found) {
      poolRep rep = (poolRep)entry;

      /* It must be EPVM. */
      mps_epvm_save(rep->pool);
    }
  } break;
  case EventPoolPop: { /* pool, level */
    found = TableLookup(&entry, poolTable, (Word)event->pu.p0);
    if (found) {
      poolRep rep = (poolRep)entry;

      /* It must be EPVM. */
      mps_epvm_restore(rep->pool, (mps_epvm_save_level_t)event->pu.u1);
    }
  } break;
  case EventCommitLimitSet: { /* arena, limit, succeeded */
    found = TableLookup(&entry, arenaTable, (Word)event->pwu.p0);
    assert(found);
    eres = mps_arena_commit_limit_set((mps_arena_t)entry,
                                      (size_t)event->pwu.w1);
    assert((Bool)event->pwu.u2 == (eres == MPS_RES_OK));
  } break;
  case EventSpareCommitLimitSet: { /* arena, limit */
    found = TableLookup(&entry, arenaTable, (Word)event->pw.p0);
    assert(found);
    (void)mps_arena_spare_commit_limit_set((mps_arena_t)entry,
                                           (size_t)event->pw.w1);
  } break;
  case EventReservoirLimitSet: { /* arena, limit */
    found = TableLookup(&entry, arenaTable, (Word)event->pw.p0);
    assert(found);
    mps_reservoir_limit_set((mps_arena_t)entry, (size_t)event->pw.w1);
  } break;
  case EventVMMap: case EventVMUnmap:
  case EventVMCreate: case EventVMDestroy:
  case EventArenaWriteFaults:
  case EventSegAlloc: case EventSegAllocFail: case EventSegFree:
  case EventBufferFill: case EventBufferEmpty:
  case EventCBSInit: case EventMeterInit: case EventMeterValues:
  case EventIntern: case EventLabel: {
    ++discardedEvents;
  } break;
  default: {
    ++unknownEvents;
    if (unknownEvents < 12) /* don't output too much */
      printf("Unknown event @%ld: %s.\n", eventTime,
             EventCode2Name(EventGetCode(event)));
  } break;
  }
}


/* readLog -- read and parse log */


static void readLog(EventProc proc)
{
  while (TRUE) {
    Event event;
    EventCode code;
    Res res;

    res = EventRead(&event, proc);
    if (res == ResFAIL) break; /* eof */
    if (res != ResOK) error("Truncated log");
    eventTime = event->any.clock;
    code = EventGetCode(event);
    ++totalEvents;
    processEvent(proc, event, eventTime);
    EventDestroy(proc, event);
  }
}


/* logReader -- reader function for a file log */

static FILE *input;

static Res logReader(void *file, void *p, size_t len)
{
  size_t n;

  n = fread(p, 1, len, (FILE *)file);
  return (n < len) ? (feof((FILE *)file) ? ResFAIL : ResIO) : ResOK;
}


/* Checking macros, copied from check.h */

#define CHECKLVALUE(lv1, lv2) \
  ((void)sizeof((lv1) = (lv2)), (void)sizeof((lv2) = (lv1)), TRUE)

#define CHECKTYPE(t1, t2) \
  (sizeof(t1) == sizeof(t2) && \
   CHECKLVALUE(*((t1 *)0), *((t2 *)0)))


/* CHECKCONV -- check t2 can be cast to t1 without loss */

#define CHECKCONV(t1, t2) \
  (sizeof(t1) >= sizeof(t2))


/* main */

int main(int argc, char *argv[])
{
  char *filename;
  EventProc proc;
  Res res;

  /* Check using pointers as keys in the tables. */
  assert(CHECKCONV(Word, void *));
  /* Check storage of MPS opaque handles in the tables. */
  assert(CHECKTYPE(mps_arena_t, void *));
  assert(CHECKTYPE(mps_ap_t, void *));
  /* .event-conv: Conversion of event fields into the types required */
  /* by the MPS functions is justified by the reverse conversion */
  /* being acceptable (which is upto the event log generator). */

  filename = parseArgs(argc,argv);

  if (strcmp(filename, "-") == 0)
    input = stdin;
  else {
    input = fopen(filename, "rb");
    if (input == NULL)
      error("unable to open \"%s\"\n", filename);
  }

  res = EventProcInit(&proc, partialLog, logReader, (void *)input);
  if (res != ResOK)
    error("Can't init EventProc module: error %d.", res);

  totalEvents = 0; discardedEvents = 0; unknownEvents = 0;

  res = TableCreate(&arenaTable, (size_t)1);
  if (res != ResOK)
    error("unable to create arenaTable");
  res = TableCreate(&poolTable, (size_t)1<<4);
  if (res != ResOK)
    error("unable to create poolTable");
  res = TableCreate(&apTable, (size_t)1<<6);
  if (res != ResOK)
    error("unable to create apTable");

  readLog(proc);

  printf("Replayed %lu and discarded %lu events (%lu unknown).\n",
         totalEvents - discardedEvents - unknownEvents,
         discardedEvents + unknownEvents, unknownEvents);

  /* @@@@ add listing of remaining objects? */
  /* No point in cleaning up the tables, since we're quitting. */
  EventProcFinish(proc);
  return EXIT_SUCCESS;
}
