/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(MMdevel_arenaclass.2) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This is the implementation of the Segment abstraction from the VM
 * abstraction.  Use of this arena implies use of a VM.
 *
 * DESIGN
 * design.mps.arena.vm
 */


#include "mpm.h"


SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(MMdevel_arenaclass.2) $");


/* Types
 *
 * PI is the type of a value used to index into the page table.
 * 
 * BI is the type of a value used to index into a bool table (See ABTGet
 * and ABTSet in this module).
 */

typedef struct ArenaVMStruct *ArenaVM;
typedef struct PageStruct *Page;
typedef Word *ABT;
typedef Size PI;
typedef Size BI;


/* ArenaVMStruct -- VM Arena Structure */

#define ArenaVMSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */

typedef struct ArenaVMStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;	/* generic arena structure */
  VMStruct vmStruct;            /* virtual memory structure */
  Addr base;                    /* base address of arena area */
  Addr limit;                   /* limit address of arena area */
  Size pageSize;                /* size of block managed by PageStruct */
  Shift pageShift;              /* log2 of page size, for shifts */
  Index pages;                  /* number of pages in table */
  Page pageTable;               /* the page table */
  ABT freeTable;                /* page free table */
  Size tablesSize;              /* size of area occupied by tables */
  Index tablePages;             /* number of pages occupied by tables */
  Sig sig;                      /* design.mps.sig */
} ArenaVMStruct;


/* PageStruct -- page structure
 *
 * The page table (defined as a PageStruct array) is central to the
 * design of the arena.  See design.mps.arena.vm.table.*
 */

typedef struct PageStruct {     /* page structure */
  union {
    SegStruct head;             /* segment */
    struct {
      Pool pool;                /* .page: NULL, must be first field
                                 * see impl.h.mpmst.seg.pool */
      Seg seg;                  /* segment at base page of run */
      Addr limit;               /* limit of segment */
    } tail;                     /* tail page */
  } the;
} PageStruct;


/* Page Index to Base address mapping
 *
 * See design.mps.arena.vm.table.linear
 */

#define PageBase(arena, pi) \
  AddrAdd((arena)->base, ((pi) << arena->pageShift))


/* ABTGet -- get a bool from a bool table
 *
 * Note: The function version of ABTGet isn't used anywhere in
 * this source, but is left here in case we want to revert from
 * the macro.
 */

#if 0
static Bool (ABTGet)(ABT bt, BI i)
{
  Size wi = i >> MPS_WORD_SHIFT;            /* word index */
  Size bi = i & (MPS_WORD_WIDTH - 1);       /* bit index */
  return (bt[wi] >> bi) & 1;
}
#endif /* 0 */

#define ABTGet(bt, i) \
  (((bt)[(i)>>MPS_WORD_SHIFT] >> ((i)&(MPS_WORD_WIDTH-1))) & 1)


/* ABTSet -- set a bool in a bool table */

static void ABTSet(ABT bt, BI i, Bool b)
{
  Size bi = i & (MPS_WORD_WIDTH - 1);       /* bit index */
  Word mask = ~((Word)1 << bi);
  Size wi = i >> MPS_WORD_SHIFT;            /* word index */
  bt[wi] = (bt[wi] & mask) | ((Word)b << bi);
}


#define ArenaArenaVM(arena) PARENT(ArenaVMStruct, arenaStruct, arena)
#define ArenaVMVM(arena)	(&(arena)->vmStruct)


/* ArenaCheck -- check of the consistency of the arena structure */

static Bool ArenaVMCheck(ArenaVM arenaVM)
{
  CHECKS(ArenaVM, arenaVM);
  CHECKD(VM, ArenaVMVM(arenaVM));
  CHECKL(arenaVM->base != (Addr)0);
  CHECKL(arenaVM->base < arenaVM->limit);
  CHECKL(arenaVM->pageShift <= MPS_WORD_WIDTH);
  CHECKL(arenaVM->pageSize == 1uL << arenaVM->pageShift);
  CHECKL(VMAlign() == arenaVM->pageSize);
  CHECKL(arenaVM->pages == 
         AddrOffset(arenaVM->base, arenaVM->limit) >> arenaVM->pageShift);
  CHECKL(arenaVM->tablePages <= arenaVM->pages);
  CHECKL(arenaVM->tablesSize == arenaVM->tablePages << arenaVM->pageShift);
  CHECKL(arenaVM->pageTable != NULL);
  CHECKL((Addr)arenaVM->pageTable >= arenaVM->base);
  CHECKL((Addr)&arenaVM->pageTable[arenaVM->pages] <=
           AddrAdd(arenaVM->base, arenaVM->tablesSize));
  CHECKL(arenaVM->freeTable != NULL);
  CHECKL((Addr)arenaVM->freeTable >= arenaVM->base);
  CHECKL((Addr)&arenaVM->freeTable[(arenaVM->pages + MPS_WORD_WIDTH-1)>>MPS_WORD_SHIFT] <=
           arenaVM->limit);
  /* .improve.check-table: Could check the consistency of the tables. */
  return TRUE;
}


/* ArenaVMAlloc -- allocate space for the arena */

static Res ArenaVMAlloc(Arena *arenaReturn)
{
  /* @@@@ temporary */
  extern void *malloc(size_t);
  void *p = malloc(sizeof(ArenaVMStruct));
  AVER(p != NULL);
  *arenaReturn = p;
  return ResOK;
}


/* ArenaVMFree -- free the arena */

static void ArenaVMFree(Arena arena)
{
  /* @@@@ temporary */
  extern void free(void *);
  free(arena);
}


/* ArenaVMInit -- initialize the VM arena */

static Res ArenaVMInit(Arena arena, Size size, Addr base)
{
  Res res;
  Size f_words, f_size, p_size; /* see .init-tables */
  ArenaVM arenaVM;
  PI i;

  AVERT(Arena, arena);  
  AVER(size > 0);
  /* no restrictions on base, it's simply passed through to VMCreate */

  arenaVM = ArenaArenaVM(arena);

  size = SizeAlignUp(size, VMAlign());
  res = VMInit(ArenaVMVM(arenaVM), size, base);
  if(res != ResOK) goto failVMInit;

  /* see design.mps.space.arena */
  arenaVM->base = VMBase(ArenaVMVM(arenaVM));
  arenaVM->limit = VMLimit(ArenaVMVM(arenaVM));
  AVER(AddrOffset(arenaVM->base, arenaVM->limit) == size);
  arenaVM->pageSize = VMAlign();
  arenaVM->pageShift = SizeLog2(arenaVM->pageSize);
  arenaVM->pages = size >> arenaVM->pageShift;

  /* .init-tables: Allocate the page tables at the base of the arena.
   *
   * .improve.table.zone-zero: It would be better to make sure that the
   * page tables are in zone zero, since that zone is least useful for
   * GC. (but it would change how SegAlloc avoids allocating over the
   * tables, see .alloc.skip)
   *
   * There are two tables, the free table which is a bool array, and the
   * page table which is a PageStruct array.  Both tables are allocated
   * contiguously in one chunk.
   *
   * f_words is the number of words required for the free table.
   * 
   * f_size is the page-aligned size of the free table.
   * 
   * p_size is the page-aligned size of the page table.
   */
  f_words = SizeAlignUp(arenaVM->pages, MPS_WORD_WIDTH) >> MPS_WORD_SHIFT;
  f_size = SizeAlignUp(f_words * sizeof(Word), arenaVM->pageSize);
  p_size = SizeAlignUp(arenaVM->pages * sizeof(PageStruct), arenaVM->pageSize);
  arenaVM->tablesSize = f_size + p_size;
  res = VMMap(ArenaVMVM(arenaVM),
              arenaVM->base,
              AddrAdd(arenaVM->base, arenaVM->tablesSize));
  if(res != ResOK) goto failVMMap;
  arenaVM->freeTable = (ABT)arenaVM->base;
  arenaVM->pageTable = (Page)AddrAdd(arenaVM->base, f_size);

  /* .tablepages: pages whose page index is < tablePages are recorded as
   * free but never allocated as alloc starts searching after the tables
   * (see .alloc.skip)
   */
  arenaVM->tablePages = arenaVM->tablesSize >> arenaVM->pageShift;
  for(i = 0; i < arenaVM->pages; ++i)
    ABTSet(arenaVM->freeTable, i, TRUE);

  /* Set the zone shift to divide the arena into the same number of
   * zones as will fit into a reference set (the number of bits in a
   * word).  Note that some zones are discontiguous in the arena if the
   * size is not a power of 2. See design.mps.space.arena.
   */
  arena->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);
  arena->alignment = arenaVM->pageSize;

  /* Sign the arena. */
  arenaVM->sig = ArenaVMSig;
  
  AVERT(ArenaVM, arenaVM);
  
  return ResOK;

failVMMap:
  VMFinish(ArenaVMVM(arenaVM));
failVMInit:
  return res;
}


static Res ArenaVMExtend(Arena space, Addr base, Size size)
{
  return ResUNIMPL;
}

static Res ArenaVMRetract(Arena space, Addr base, Size size)
{
  return ResUNIMPL;
}


/* ArenaDestroy -- finish the arena and destroy the space structure */

static void ArenaVMFinish(Arena arena)
{
  ArenaVM arenaVM;

  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  
  arena->sig = SigInvalid;
  VMUnmap(ArenaVMVM(arenaVM), arenaVM->base, AddrAdd(arenaVM->base, arenaVM->tablesSize));
  VMFinish(ArenaVMVM(arenaVM));     /* .vm.create */
}


/* ArenaReserved -- return the amount of reserved address space
 * ArenaCommitted -- return the amount of committed virtual memory
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

static Size ArenaVMReserved(Arena arena)
{
  ArenaVM arenaVM;

  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  
  return VMReserved(ArenaVMVM(arenaVM));
}

static Size ArenaVMCommitted(Arena arena)
{
  ArenaVM arenaVM;

  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  
  return VMMapped(ArenaVMVM(arenaVM));
}


/* ArenaVMSegAlloc -- allocate a segment from the arena */

static Res ArenaVMSegAlloc(Seg *segReturn, SegPref pref, Arena arena, Size size, Pool pool)
{
  ArenaVM arenaVM;
  PI pi, count, pages, base = 0;        /* whinge stopper */
  Addr addr;
  Seg seg;
  Res res;

  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  
  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > 0);
  AVERT(Pool, pool);
  AVER(SizeIsAligned(size, arenaVM->pageSize));
  
  /* NULL is used as a discriminator (see
   * design.mps.arena.vm.table.disc), therefore the real pool must be
   * non-NULL.
   */
  AVER(pool != NULL);

  /* Search for a free run of pages in the free table.
   * .alloc.skip: Start from arenaVM->tablePages (.tablepages).
   * .improve.bit-twiddle:  This code can probably be seriously
   * optimised by twiddling the bit table.
   */
  pages = size >> arenaVM->pageShift;
  count = 0;
  for(pi = arenaVM->tablePages; pi < arenaVM->pages; ++pi) {
    if(ABTGet(arenaVM->freeTable, pi)) {
      if(count == 0)
        base = pi;
      ++count;
      if(count == pages)
        goto found;
    } else
      count = 0;
  }
  
  /* No space was found.
   * .improve.alloc-fail: This could be because the request was
   * too large, or perhaps the arena is fragmented.  We could return a
   * more meaningful code.
   */
  return ResRESOURCE;

found:
  /* .alloc.early-map: Map in the segment memory before actually
   * allocating the pages, because the unwind (in case of failure)
   * is simpler. */
  addr = PageBase(arenaVM, base);
  res = VMMap(ArenaVMVM(arenaVM), addr, AddrAdd(addr, size));
  if(res) return res;

  /* Initialize the generic segment structure. */
  seg = &arenaVM->pageTable[base].the.head;
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page,
   * allocate the rest of the pages and store the multi-page information
   * in the page table.
   */
  AVER(ABTGet(arenaVM->freeTable, base));
  ABTSet(arenaVM->freeTable, base, FALSE);
  if(pages > 1) {
    Addr limit = PageBase(arenaVM, base + pages);
    seg->single = FALSE;
    for(pi = base + 1; pi < base + pages; ++pi) {
      AVER(ABTGet(arenaVM->freeTable, pi));
      ABTSet(arenaVM->freeTable, pi, FALSE);
      arenaVM->pageTable[pi].the.tail.pool = NULL;
      arenaVM->pageTable[pi].the.tail.seg = seg;
      arenaVM->pageTable[pi].the.tail.limit = limit;
    }
  } else {
    seg->single = TRUE;
  }
  
  AVERT(Seg, seg);
  
  *segReturn = seg;
  return ResOK;
}


/* ArenaVMSegBase -- return the base address of a segment
 *
 * The segment base is calculated by working out the index of the
 * segment structure in the page table and then multiplying that
 * by the page size and adding it to the arena base address.
 */

static Addr ArenaVMSegBase(Arena arena, Seg seg)
{
  ArenaVM arenaVM;
  Page page;
  PI pi;
  
  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  AVERT(Seg, seg);

  page = PARENT(PageStruct, the.head, seg);
  pi = page - arenaVM->pageTable;

  return PageBase(arenaVM, pi);
}


/* ArenaVMSegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

static Addr ArenaVMSegLimit(Arena arena, Seg seg)
{
  ArenaVM arenaVM;
  Page page;
  Addr limit;

  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  AVERT(Seg, seg);

  if(seg->single)
    limit = AddrAdd(ArenaVMSegBase(arena, seg), arenaVM->pageSize);
  else {
    page = PARENT(PageStruct, the.head, seg);
    limit = page[1].the.tail.limit;
  }

  return limit;
}


/* ArenaVMSegFree - free a segment in the arena */

static void ArenaVMSegFree(Arena arena, Seg seg)
{
  ArenaVM arenaVM;
  Page page;
  PI pi, pl, pn;
  Addr base, limit; 

  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  AVERT(Seg, seg);

  page = PARENT(PageStruct, the.head, seg);
  limit = ArenaVMSegLimit(arena, seg);
  pi = page - arenaVM->pageTable;
  AVER(pi <= arenaVM->pages);

  SegFinish(seg);

  /* Remember the base address of the segment so it can be */
  /* unmapped .free.unmap */
  base = PageBase(arenaVM, pi);

  /* Calculate the number of pages in the segment, and hence the
   * limit for .free.loop */
  pn = AddrOffset(base, limit) >> arenaVM->pageShift;
  pl = pi + pn;
  /* .free.loop: */
  for( ; pi < pl; ++pi) {
    AVER(ABTGet(arenaVM->freeTable, pi) == FALSE);
    ABTSet(arenaVM->freeTable, pi, TRUE);
  }

  /* .free.unmap: Unmap the segment memory. */
  VMUnmap(ArenaVMVM(arenaVM), base, PageBase(arenaVM, pi));

  /* Double check that .free.loop takes us to the limit page of the
   * segment.
   */
  AVER(PageBase(arenaVM, pi) == limit);
}


/* ArenaVMSegOfAddr -- return the segment which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If it is a head page, return the segment, otherwise follow the
 * tail's pointer back to the segment in the head page.
 */

static Bool ArenaVMSegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  ArenaVM arenaVM;
  
  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  AVER(segReturn != NULL);
  
  if(arenaVM->base <= addr && addr < arenaVM->limit) {
    PI pi = AddrOffset(arenaVM->base, addr) >> arenaVM->pageShift;
    if(!ABTGet(arenaVM->freeTable, pi)) {
      Page page = &arenaVM->pageTable[pi];
      if(page->the.head.pool != NULL)
        *segReturn = &page->the.head;
      else
        *segReturn = page->the.tail.seg;
      return TRUE;
    }
  }
  
  return FALSE;
}


/* ArenaVMSegSearch -- search for a segment
 *
 * Searches for a segment in the arena starting at page index pi,
 * return NULL if there is none.  A segment is present if it is
 * not free, and its pool is not NULL.
 *
 * This function is private to this module and is used in the segment
 * iteration protocol (SegFirst and SegNext).
 */
static Seg ArenaVMSegSearch(ArenaVM arenaVM, PI pi)
{
  AVERT(ArenaVM, arenaVM);

  while(pi < arenaVM->pages &&
        (ABTGet(arenaVM->freeTable, pi) ||
         arenaVM->pageTable[pi].the.head.pool == NULL))
    ++pi;
  
  if(pi < arenaVM->pages)
    return &arenaVM->pageTable[pi].the.head;
  
  AVER(pi == arenaVM->pages);

  return NULL;
}


/* ArenaVMSegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 * See SEG_FOR (impl.h.mpm).
 */

static Seg ArenaVMSegFirst(Arena arena)
{
  ArenaVM arenaVM;
  
  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);

  /* We start from tablePages, as the tables can't be a segment.
   * See .tablepages */
  return ArenaVMSegSearch(arenaVM, (PI)arenaVM->tablePages);
}


/* ArenaVMSegNext -- return the next segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.  See SEG_FOR (impl.h.mpm).
 */

static Seg ArenaVMSegNext(Arena arena, Seg seg)
{
  ArenaVM arenaVM;
  Page page;
  PI pi;
  
  AVERT(Arena, arena);
  arenaVM = ArenaArenaVM(arena);
  AVERT(ArenaVM, arenaVM);
  AVERT(Seg, seg);

  page = PARENT(PageStruct, the.head, seg);
  pi = page - arenaVM->pageTable;
  return ArenaVMSegSearch(arenaVM, pi + 1);
}


static ArenaClassStruct ArenaClassVMStruct = {
  ArenaClassSig,
  "VM",					/* name */
  sizeof(ArenaVMStruct),		/* size */
  offsetof(ArenaVMStruct, arenaStruct),	/* offset */
  ArenaVMAlloc,				/* alloc */
  ArenaVMFree,				/* free */
  ArenaVMInit,				/* init */
  ArenaVMFinish,			/* finish */
  ArenaVMReserved,			/* reserved */
  ArenaVMCommitted,			/* committed */
  ArenaVMExtend,			/* extend */
  ArenaVMRetract,			/* retract */
  ArenaVMSegAlloc,			/* segAlloc */
  ArenaVMSegFree,			/* segFree */
  ArenaVMSegBase,			/* segBase */
  ArenaVMSegLimit,			/* segLimit */
  ArenaVMSegOfAddr,			/* segOfAddr */
  ArenaVMSegFirst,			/* segFirst */
  ArenaVMSegNext,			/* segNext */
  ArenaClassSig			/* @@@@ */
};

ArenaClass ArenaClassVM(void)
{
  return &ArenaClassVMStruct;
}
