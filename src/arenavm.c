/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(MMdevel_assertid.2) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This is the implementation of the Segment abstraction from the VM
 * abstraction.  Use of this arena implies use of a VM.
 *
 * DESIGN
 * design.mps.arena.vm
 */


#include "mpm.h"


SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(MMdevel_assertid.2) $");


/* Space Arena Projection
 * 
 * Only the arena module needs to discuss the arena object, hence, this
 * method is private to this module.
 */

#define SpaceArena(space)       (&(space)->arenaStruct)


/* Page Index to Base address mapping
 *
 * See design.mps.arena.vm.table.linear
 */

#define PageBase(arena, i) \
  AddrAdd((arena)->base, ((i) << arena->pageShift))


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


/* ArenaCreate -- create the arena
 *
 * In fact, this creates the space structure and initializes the
 * arena part.
 *
 * In fact, the space structure is created by calling VMCreate.
 */

Res ArenaCreate(Space *spaceReturn, Size size, Addr base)
{
  Res res;
  Space space;
  Size f_words, f_size, p_size; /* see .init-tables */
  Arena arena;
  
  AVER(0xA6F30000, spaceReturn != NULL);
  AVER(0xA6F30001, size > 0);
  /* no restrictions on base, it's simply passed through to VMCreate */

  /* VMCreate requires aligned size */
  size = SizeAlignUp(size, VMAlign());

  /* .vm.create: Create the space structure, initialize the VM part */
  res = VMCreate(&space, size, base);
  if(res) return res;

  arena = SpaceArena(space);
  /* see design.mps.space.arena */
  arena->base = VMBase(space);
  arena->limit = VMLimit(space);
  AVER(0xA6F30002, AddrOffset(arena->base, arena->limit) == size);
  arena->pageSize = VMAlign();
  arena->pageShift = SizeLog2(arena->pageSize);
  arena->pages = size >> arena->pageShift;

  /* .init-tables: Allocate the page tables at the base of the arena.
   *
   * .improve.table.zone-zero: It would be better to make sure that the
   * page tables are in zone zero, since that zone is least useful for
   * GC. (but it would change how SegAllocWithRefSet avoids allocating
   * over the tables, see .alloc.skip)
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
  f_words = SizeAlignUp(arena->pages, MPS_WORD_WIDTH) >> MPS_WORD_SHIFT;
  f_size = SizeAlignUp(f_words * sizeof(Word), arena->pageSize);
  p_size = SizeAlignUp(arena->pages * sizeof(PageStruct), arena->pageSize);
  arena->tablesSize = f_size + p_size;
  res = VMMap(space, arena->base, AddrAdd(arena->base, arena->tablesSize));
  if(res) {
    VMDestroy(space);
    return res;
  }
  arena->freeTable = (BT)arena->base;
  arena->pageTable = (Page)AddrAdd(arena->base, f_size);

  /* .tablepages: pages whose page index is < tablePages are recorded as
   * free but never allocated as alloc starts searching after the tables
   * (see .alloc.skip).  SegOfAddr uses the fact that these pages are
   * marked as free in order to detect "references" to these pages as
   * being bogus see .addr.free.
   */
  arena->tablePages = arena->tablesSize >> arena->pageShift;
  BTSetRange(arena->freeTable, 0, arena->pages);

  /* Set the zone shift to divide the arena into the same number of
   * zones as will fit into a reference set (the number of bits in a
   * word).  Note that some zones are discontiguous in the arena if the
   * size is not a power of 2. See design.mps.space.arena.
   */
  space->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);

  /* Sign the arena. */
  arena->sig = ArenaSig;
  
  AVERT(0xA6F30003, Arena, arena);
  
  EVENT2(ArenaCreate, arena, space);

  *spaceReturn = space;
  return ResOK;
}

Res ArenaExtend(Space space, Addr base, Size size)
{
  return ResUNIMPL;
}

Res ArenaRetract(Space space, Addr base, Size size)
{
  return ResUNIMPL;
}

/* ArenaDestroy -- finish the arena and destroy the space structure */

void ArenaDestroy(Space space)
{
  Arena arena;

  AVERT(0xA6F30004, Arena, SpaceArena(space));
  
  arena = SpaceArena(space);
  arena->sig = SigInvalid;
  VMUnmap(space, arena->base, AddrAdd(arena->base, arena->tablesSize));
  VMDestroy(space);     /* .vm.create */

  EVENT1(ArenaDestroy, arena);
}


/* ArenaReserved -- return the amount of reserved address space
 * ArenaCommitted -- return the amount of committed virtual memory
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

Size ArenaReserved(Space space)
{
  AVERT(0xA6F30005, Arena, SpaceArena(space));
  return VMReserved(space);
}

Size ArenaCommitted(Space space)
{
  AVERT(0xA6F30006, Arena, SpaceArena(space));
  return VMMapped(space);
}


/* ArenaCheck -- check of the consistency of the arena structure */

Bool ArenaCheck(Arena arena)
{
  CHECKS(0xA6F30007, Arena, arena);
  CHECKD(0xA6F30008, VM, &arena->vmStruct);
  CHECKL(0xA6F30009, arena->base != (Addr)0);
  CHECKL(0xA6F3000A, arena->base < arena->limit);
  CHECKL(0xA6F3000B, arena->pageShift <= MPS_WORD_WIDTH);
  CHECKL(0xA6F3000C, arena->pageSize == 1uL << arena->pageShift);
  CHECKL(0xA6F3000D, VMAlign() == arena->pageSize);
  CHECKL(0xA6F3000E, arena->pages == 
           AddrOffset(arena->base, arena->limit) >> arena->pageShift);
  CHECKL(0xA6F3000F, arena->tablePages <= arena->pages);
  CHECKL(0xA6F30010, arena->tablesSize == arena->tablePages << arena->pageShift);
  CHECKL(0xA6F30011, arena->pageTable != NULL);
  CHECKL(0xA6F30012, (Addr)arena->pageTable >= arena->base);
  CHECKL(0xA6F30013, (Addr)&arena->pageTable[arena->pages] <=
           AddrAdd(arena->base, arena->tablesSize));
  CHECKL(0xA6F30014, arena->freeTable != NULL);
  CHECKL(0xA6F30015, (Addr)arena->freeTable >= arena->base);
  CHECKL(0xA6F30016, (Addr)&arena->freeTable[(arena->pages + MPS_WORD_WIDTH-1)>>MPS_WORD_SHIFT] <=
           arena->limit);
  /* .improve.check-table: Could check the consistency of the tables. */
  return TRUE;
}


Bool SegPrefCheck(SegPref pref)
{
  CHECKS(0xA6F30017, SegPref, pref);
  CHECKL(0xA6F30018, BoolCheck(pref->high));
  /* nothing else to check */
  return TRUE;
}

static SegPrefStruct segPrefDefault = {
  SegPrefSig,                           /* sig */
  FALSE,                                /* high */
  RefSetUNIV,                           /* refSet */
};

SegPref SegPrefDefault(void)
{
  return &segPrefDefault;
}

Res SegPrefExpress (SegPref sp, SegPrefKind kind, void *p)
{
  AVERT(0xA6F30019, SegPref,sp);
  AVER(0xA6F3001A, sp != &segPrefDefault);

  switch(kind) {
  case SegPrefHigh:
    AVER(0xA6F3001B, p == NULL);
    sp->high = TRUE;
    return ResOK;

  case SegPrefLow:
    AVER(0xA6F3001C, p == NULL);
    sp->high = FALSE;
    return ResOK;

  case SegPrefRefSet:
    AVER(0xA6F3001D, p != NULL);
    sp->refSet = *(RefSet *)p;
    return ResOK;

  default:
    /* see design.mps.pref.default */
    return ResOK;
  }
}


/* IndexOfAddr -- return the page index of the page containing an address */

static Index IndexOfAddr(Arena arena, Addr addr)
{
  AVERT(0xA6F3001E, Arena, arena);
  AVER(0xA6F3001F, arena->base <= addr);
  AVER(0xA6F30020, addr <= arena->limit);
  return AddrOffset(arena->base, addr) >> arena->pageShift;
}


/* SegAllocInArea -- try to allocate a segment in an area
 *
 * Search for a free run of pages in the free table, but between
 * base and limit.
 *
 * .improve.bit-twiddle:  This code can probably be seriously
 * optimised by twiddling the bit table.
 */

static Bool SegAllocInArea(Index *baseReturn,
			   Space space, Size size, Addr base, Addr limit)
{
  Arena arena;
  Word pages;				/* number of pages equiv. to size */
  Word count;				/* pages so far in free run */
  Index basePage, limitPage;		/* Index equiv. to base and limit */
  Index i;				/* iterator over page table */
  Index start = (Index)0;		/* base of free run, with warning suppressor */

  AVER(0xA6F30021, baseReturn != NULL);
  AVERT(0xA6F30022, Space, space);  
  arena = SpaceArena(space);
  AVERT(0xA6F30023, Arena, arena);
  AVER(0xA6F30024, arena->base <= base);
  AVER(0xA6F30025, base < limit);
  AVER(0xA6F30026, limit <= arena->limit);
  AVER(0xA6F30027, size <= AddrOffset(base, limit));
  AVER(0xA6F30028, size > (Size)0);
  AVER(0xA6F30029, SizeIsAligned(size, arena->pageSize));

  basePage = IndexOfAddr(arena, base);
  limitPage = IndexOfAddr(arena, limit);

  pages = size >> arena->pageShift;
  count = 0;
  for(i = basePage; i < limitPage; ++i) {
    if(BTGet(arena->freeTable, i)) {
      if(count == 0)
        start = i;
      ++count;
      if(count == pages) {
        *baseReturn = start;
        return TRUE;
      }
    } else
      count = 0;
  }
  
  return FALSE;
}


/* SegAllocWithRefSet
 *   -- try to allocate a segment with a particular RefSet
 * 
 * This function finds the intersection of refSet and the set of free
 * pages and tries to allocate a segment in the resulting set of
 * areas.
 */

static Bool SegAllocWithRefSet(Index *baseReturn,
			       Space space, Size size, RefSet refSet)
{
  Arena arena = SpaceArena(space);
  Addr arenaBase, base, limit;
  Size zoneSize = (Size)1 << space->zoneShift;

  /* .alloc.skip: The first address available for segments, */
  /* is just after the arena tables. */
  arenaBase = PageBase(arena, arena->tablePages);

  base = arenaBase;
  while(base < arena->limit) {
  
    if(RefSetIsMember(space, refSet, base)) {
      /* Search for a run of zone stripes which are in the RefSet and */
      /* the arena.  Adding the zoneSize might wrap round (to zero, */
      /* because limit is aligned to zoneSize, which is a power of two). */
      limit = base;
      do {
        limit = AddrAlignDown(AddrAdd(limit, zoneSize), zoneSize);

        AVER(0xA6F3002A, limit > base || limit == (Addr)0);

        if(limit >= arena->limit || limit < base) {
          limit = arena->limit;
          break;
        }

        AVER(0xA6F3002B, base < limit && limit < arena->limit);
      } while(RefSetIsMember(space, refSet, limit));

      AVER(0xA6F3002C, refSet != RefSetUNIV ||
           (base == arenaBase && limit == arena->limit));

      /* Try to allocate a segment in the area. */
      if(AddrOffset(base, limit) >= size &&
         SegAllocInArea(baseReturn, space, size, base, limit))
        return TRUE;
      
      base = limit;
    } else {
      /* Adding the zoneSize might wrap round (to zero, because base */
      /* is aligned to zoneSize, which is a power of two). */
      base = AddrAlignDown(AddrAdd(base, zoneSize), zoneSize);
      AVER(0xA6F3002D, base > arenaBase || base == (Addr)0);
      if(base < arenaBase) {
        base = arena->limit;
        break;
      }
    }
  }

  AVER(0xA6F3002E, base == arena->limit);

  return FALSE;
}


/* SegAlloc -- allocate a segment from the arena */

Res SegAlloc(Seg *segReturn, SegPref pref, Space space, Size size, Pool pool)
{
  Arena arena = SpaceArena(space);
  Index i, pages, base;
  Addr addr;
  Seg seg;
  Res res;

  AVER(0xA6F3002F, segReturn != NULL);
  AVERT(0xA6F30030, SegPref, pref);
  AVERT(0xA6F30031, Arena, SpaceArena(space));
  AVER(0xA6F30032, size > 0);
  AVERT(0xA6F30033, Pool, pool);
  AVER(0xA6F30034, SizeIsAligned(size, arena->pageSize));
  
  /* NULL is used as a discriminator (see design.mps.arena.vm.table.disc) */
  /* therefore the real pool must be non-NULL. */
  AVER(0xA6F30035, pool != NULL);

  if(!SegAllocWithRefSet(&base, space, size, pref->refSet) &&
     (pref->refSet == RefSetUNIV ||
      !SegAllocWithRefSet(&base, space, size, RefSetUNIV))) {
    /* .improve.alloc-fail: This could be because the request was */
    /* too large, or perhaps the arena is fragmented.  We could return a */
    /* more meaningful code. */
    return ResRESOURCE;
  }
  
  /* .alloc.early-map: Map in the segment memory before actually */
  /* allocating the pages, because the unwind (in case of failure) */
  /* is simpler. */
  addr = PageBase(arena, base);
  res = VMMap(space, addr, AddrAdd(addr, size));
  if(res) return res;

  /* Initialize the generic segment structure. */
  seg = &arena->pageTable[base].the.head;
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page information */
  /* in the page table. */
  AVER(0xA6F30036, BTGet(arena->freeTable, base));
  BTRes(arena->freeTable, base);
  pages = size >> arena->pageShift;
  if(pages > 1) {
    Addr limit = PageBase(arena, base + pages);
    seg->single = FALSE;
    for(i = base + 1; i < base + pages; ++i) {
      AVER(0xA6F30037, BTGet(arena->freeTable, i));
      BTRes(arena->freeTable, i);
      arena->pageTable[i].the.tail.pool = NULL;
      arena->pageTable[i].the.tail.seg = seg;
      arena->pageTable[i].the.tail.limit = limit;
    }
  } else {
    seg->single = TRUE;
  }
  
  AVERT(0xA6F30038, Seg, seg);
  
  EVENT5(SegAlloc, arena, seg, addr, size, pool);

  *segReturn = seg;
  return ResOK;
}


/* SegFree - free a segment in the arena */

void SegFree(Space space, Seg seg)
{
  Arena arena;
  Page page;
  Index i, pl, pn;
  Addr base, limit; 

  AVERT(0xA6F30039, Arena, SpaceArena(space));
  AVERT(0xA6F3003A, Seg, seg);

  arena = SpaceArena(space);
  page = PARENT(PageStruct, the.head, seg);
  limit = SegLimit(space, seg);
  i = page - arena->pageTable;
  AVER(0xA6F3003B, i <= arena->pages);

  SegFinish(seg);

  /* Remember the base address of the segment so it can be */
  /* unmapped .free.unmap */
  base = PageBase(arena, i);

  /* Calculate the number of pages in the segment, and hence the
   * limit for .free.loop */
  pn = AddrOffset(base, limit) >> arena->pageShift;
  pl = i + pn;
  /* .free.loop: */
  while(i < pl) {
    AVER(0xA6F3003C, !BTGet(arena->freeTable, i));
    BTSet(arena->freeTable, i);
    ++i;
  }

  /* .free.unmap: Unmap the segment memory. */
  VMUnmap(space, base, PageBase(arena, i));

  /* Double check that .free.loop takes us to the limit page of the
   * segment.
   */
  AVER(0xA6F3003D, PageBase(arena, i) == limit);

  EVENT2(SegFree, arena, seg);
}


/* ArenaAlign -- return the alignment of segments */

Align ArenaAlign(Space space)
{
  Arena arena;
  AVERT(0xA6F3003E, Arena, SpaceArena(space));
  arena = SpaceArena(space);
  return arena->pageSize;
}


/* SegBase -- return the base address of a segment
 *
 * The segment base is calculated by working out the index of the
 * segment structure in the page table and then multiplying that
 * by the page size and adding it to the arena base address.
 */

Addr SegBase(Space space, Seg seg)
{
  Arena arena;
  Page page;
  Index i;
  
  AVERT(0xA6F3003F, Arena, SpaceArena(space));
  AVERT(0xA6F30040, Seg, seg);

  arena = SpaceArena(space);
  page = PARENT(PageStruct, the.head, seg);
  i = page - arena->pageTable;

  return PageBase(arena, i);
}


/* SegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

Addr SegLimit(Space space, Seg seg)
{
  Arena arena;
  Page page;

  AVERT(0xA6F30041, Arena, SpaceArena(space));
  AVERT(0xA6F30042, Seg, seg);

  arena = SpaceArena(space);
  if(seg->single)
    return AddrAdd(SegBase(space, seg), arena->pageSize);
  else {
    page = PARENT(PageStruct, the.head, seg);
    return page[1].the.tail.limit;
  }
}


/* SegSize -- return the size (limit - base) of a segment
 *
 * .improve.redundant-calc: There is scope for optimizing this,
 * because both base and limit calls do roughly the same thing twice.
 */

Size SegSize(Space space, Seg seg)
{
  AVERT(0xA6F30043, Arena, SpaceArena(space));
  AVERT(0xA6F30044, Seg, seg);
  return AddrOffset(SegBase(space, seg), SegLimit(space, seg));
}


/* SegOfAddr -- return the segment which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If it is a head page, return the segment, otherwise follow the
 * tail's pointer back to the segment in the head page.
 */

Bool SegOfAddr(Seg *segReturn, Space space, Addr addr)
{
  Arena arena;
  
  AVER(0xA6F30045, segReturn != NULL);
  AVERT(0xA6F30046, Arena, SpaceArena(space));
  
  arena = SpaceArena(space);
  if(arena->base <= addr && addr < arena->limit) {
    Index i = IndexOfAddr(arena, addr);
    /* .addr.free: If the page is recorded as being free then */
    /* either the page is free or it is */
    /* part of the arena tables (see .tablepages) */
    if(!BTGet(arena->freeTable, i)) {
      Page page = &arena->pageTable[i];

      if(page->the.head.pool != NULL)
        *segReturn = &page->the.head;
      else
        *segReturn = page->the.tail.seg;
      return TRUE;
    }
  }
  
  return FALSE;
}


/* SegSearch -- search for a segment
 *
 * Searches for a segment in the arena starting at page index i,
 * return NULL if there is none.  A segment is present if it is
 * not free, and its pool is not NULL.
 *
 * This function is private to this module and is used in the segment
 * iteration protocol (SegFirst and SegNext).
 */
static Seg SegSearch(Arena arena, Index i)
{
  while(i < arena->pages &&
        (BTGet(arena->freeTable, i) ||
         arena->pageTable[i].the.head.pool == NULL))
    ++i;
  
  if(i < arena->pages)
    return &arena->pageTable[i].the.head;
  
  AVER(0xA6F30047, i == arena->pages);
  return NULL;
}


/* SegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 * See SEG_FOR (impl.h.mpm).
 */

Seg SegFirst(Space space)
{
  Arena arena;

  AVERT(0xA6F30048, Arena, SpaceArena(space));
  arena = SpaceArena(space);

  /* We start from tablePages, as the tables can't be a segment.
   * See .tablepages */
  return SegSearch(arena, (Index)arena->tablePages);
}


/* SegNext -- return the next segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.  See SEG_FOR (impl.h.mpm).
 */

Seg SegNext(Space space, Seg seg)
{
  Arena arena;
  Page page;
  Index i;
  AVERT(0xA6F30049, Arena, SpaceArena(space));
  AVERT(0xA6F3004A, Seg, seg);
  page = PARENT(PageStruct, the.head, seg);
  arena = SpaceArena(space);
  i = page - arena->pageTable;
  return SegSearch(arena, i + 1);
}


