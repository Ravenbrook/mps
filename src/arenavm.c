/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(MMdevel_config_thread.2) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This is the implementation of the Segment abstraction from the VM
 * abstraction.  Use of this arena implies use of a VM.
 *
 * DESIGN
 *
 * See design.mps.arena.vm.
 *
 *
 * TRANSGRESSIONS
 *
 * .trans.zone-shift: The VM arena pokes around with arena->zoneShift.
 * In fact, the arena implementation really owns this field.
 *
 *
 * IMPROVEMENTS
 *
 * .improve.table.zone-zero: It would be better to make sure that the
 * page tables are in zone zero, since that zone is least useful for
 * GC. (but it would change how SegAllocWithRefSet avoids allocating
 * over the tables, see .alloc.skip)@@@@
 */


#include "mpm.h"
#include "mpsavm.h"


SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(MMdevel_config_thread.2) $");


typedef struct VMArenaStruct *VMArena;
typedef struct PageStruct *Page;


/* VMArenaStruct -- VM Arena Structure */

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */

typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;	/* generic arena structure */
  VM vm;                        /* virtual memory handle */
  Addr base;                    /* base address of arena area */
  Addr limit;                   /* limit address of arena area */
  Size pageSize;                /* size of block managed by PageStruct */
  Shift pageShift;              /* log2 of page size, for shifts */
  Index pages;                  /* number of pages in table */
  Page pageTable;               /* the page table */
  BT allocTable;                /* page allocation table */
  Size tablesSize;              /* size of area occupied by tables */
  Index tablePages;             /* number of pages occupied by tables */
  Sig sig;                      /* design.mps.sig */
} VMArenaStruct;


/* ArenaVMArena -- find the VMArena pointer given a generic Arena */

#define ArenaVMArena(arena) PARENT(VMArenaStruct, arenaStruct, (arena))


/* VMArenaArena -- find the generic Arena pointer given a VMArena */

#define VMArenaArena(VMArena) (&(VMArena)->arenaStruct)


/* PageStruct -- page structure
 *
 * .page-table: The page table (defined as a PageStruct array)
 * is central to the design of the arena.
 * See design.mps.arena.vm.table.*.
 *
 * .page: The "pool" field must be the first field of the "tail"
 * field of this union, so that it shares a common prefix with the
 * SegStruct.  See impl.h.mpmst.seg.pool.
 */

typedef struct PageStruct {     /* page structure */
  union {
    SegStruct segStruct;         /* segment */
    struct {
      Pool pool;                 /* NULL, must be first field (.page) */
      Seg seg;                   /* segment at base page of run */
      Addr limit;                /* limit of segment */
    } tail;                      /* tail page */
  } the;
} PageStruct;


/* PageBase -- map page index to base address of page
 *
 * See design.mps.arena.vm.table.linear
 */

#define PageBase(vmArena, i) \
  AddrAdd((vmArena)->base, ((i) << (vmArena)->pageShift))


/* PageSeg -- segment descriptor of a page */

#define PageSeg(page)           (&(page)->the.segStruct)


/* PageTail -- tail descriptor of a page */

#define PageTail(page)          (&(page)->the.tail)


/* PageOfSeg -- page descriptor from segment */

#define PageOfSeg(seg)          PARENT(PageStruct, the.segStruct, seg)


/* PageIsHead -- is a page a head (contains segment descriptor)?
 *
 * See design.mps.arena.vm.table.disc.
 */

#define PageIsHead(page)        (PageTail(page)->pool != NULL)


static Addr VMSegLimit(Arena arena, Seg seg);


/* VMArenaCheck -- check the consistency of an arena structure */

static Bool VMArenaCheck(VMArena vmArena)
{
  CHECKS(VMArena, vmArena);
  CHECKD(Arena, VMArenaArena(vmArena));
  CHECKL(VMCheck(vmArena->vm));
  CHECKL(vmArena->base != (Addr)0);
  CHECKL(vmArena->base < vmArena->limit);
  CHECKL(ShiftCheck(vmArena->pageShift));
  CHECKL(vmArena->pageSize == 1uL << vmArena->pageShift);
  CHECKL(VMAlign() == vmArena->pageSize);
  CHECKL(vmArena->pages == 
           AddrOffset(vmArena->base, vmArena->limit) >> vmArena->pageShift);
  CHECKL(vmArena->tablePages <= vmArena->pages);
  CHECKL(vmArena->tablesSize == vmArena->tablePages << vmArena->pageShift);
  CHECKL(vmArena->pageTable != NULL);
  CHECKL((Addr)vmArena->pageTable >= vmArena->base);
  CHECKL((Addr)&vmArena->pageTable[vmArena->pages] <=
           AddrAdd(vmArena->base, vmArena->tablesSize));
  CHECKL(vmArena->allocTable != NULL);
  CHECKL((Addr)vmArena->allocTable >= vmArena->base);
  CHECKL(AddrAdd((Addr)vmArena->allocTable, BTSize(vmArena->pages)) <=
         vmArena->limit);
  /* .improve.check-table: Could check the consistency of the tables. */
  return TRUE;
}


/* VMArenaInit -- create and initialize the VM arena
 *
 * .arena.alloc: The arena descriptor will be allocated at the base of
 * the arena, before the tables.
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */

static Res VMArenaInit(Arena *arenaReturn, va_list args)
{
  Size size;
  Res res;
  Size pageTableSize;
  VM vm;
  Addr base;
  VMArenaStruct initialArenaStruct;
  VMArena initArena = &initialArenaStruct;
  VMArena vmArena;
  Arena arena;

  size = va_arg(args, Size);
  AVER(arenaReturn != NULL);
  AVER(size > 0);

  /* VMCreate requires aligned size */
  size = SizeAlignUp(size, VMAlign());
  AVER(size != 0);

  res = VMCreate(&vm, size);
  if(res != ResOK) goto failVMCreate;

  /* .arena.alloc */
  base = VMBase(vm);
  initArena->vm = vm;
  initArena->base = base;
  initArena->limit = VMLimit(vm);
  AVER(AddrOffset(initArena->base, initArena->limit) == size);
  initArena->pageSize = VMAlign();
  initArena->pageShift = SizeLog2(initArena->pageSize);
  initArena->pages = size >> initArena->pageShift;
  /* We generally assume that a page is aligned enough for any */
  /* normal object. */
  AVER(initArena->pageSize >= MPS_PF_ALIGN);

  /* .arena.tables: Place the tables at the base of the arena.  First*/
  /* the arena descriptor, immediately followed by the alloc table */
  /* (a bit table), and then at the next page boundary, the page */
  /* table (a PageStruct array). */
  initArena->allocTable = (BT)AddrAlignUp(AddrAdd(base,
						  (Size)sizeof(VMArenaStruct)),
					  MPS_PF_ALIGN);
  initArena->pageTable = (Page)AddrAlignUp(AddrAdd((Addr)initArena->allocTable,
						   BTSize(initArena->pages)),
					   initArena->pageSize);
  pageTableSize = SizeAlignUp(initArena->pages * sizeof(PageStruct),
                              initArena->pageSize);
  initArena->tablesSize = AddrOffset(base,
				     AddrAdd((Addr)initArena->pageTable,
					     pageTableSize));
  res = VMMap(vm, base, AddrAdd(base, initArena->tablesSize));
  if(res != ResOK) goto failTableMap;

  /* Now that we've mapped the tables, copy in the stuff already computed. */
  vmArena = (VMArena)base;
  *vmArena = *initArena;

  arena = VMArenaArena(vmArena);
  /* impl.c.arena.init.caller */
  ArenaInit(arena, (ArenaClass)mps_arena_class_vm());

  /* .tablepages: pages whose page index is < tablePages are recorded as */
  /* free but never allocated as alloc starts searching after the tables */
  /* (see .alloc.skip).  SegOfAddr uses the fact that these pages are */
  /* marked as free in order to detect "references" to these pages as */
  /* being bogus see .addr.free. */
  vmArena->tablePages = vmArena->tablesSize >> vmArena->pageShift;
  BTResRange(vmArena->allocTable, 0, vmArena->pages);

  /* Set the zone shift to divide the arena into the same number of */
  /* zones as will fit into a reference set (the number of bits in a */
  /* word).  Note that some zones are discontiguous in the arena if the */
  /* size is not a power of 2.  See design.mps.arena.class.fields. */
  arena->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);
  arena->alignment = vmArena->pageSize;

  /* Sign and check the arena. */
  vmArena->sig = VMArenaSig;
  AVERT(VMArena, vmArena);

  EVENT_PP(ArenaCreate, vmArena, vm);

  *arenaReturn = arena;
  return ResOK;

failTableMap:
  VMDestroy(vm);
failVMCreate:
  return res;
}


/* VMArenaFinish -- finish the arena and destroy the VM */

static void VMArenaFinish(Arena arena)
{
  VMArena vmArena;
  VM vm;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  
  vmArena->sig = SigInvalid;
  vm = vmArena->vm;
  ArenaFinish(arena); /* impl.c.arena.finish.caller */
  VMUnmap(vm, vmArena->base, AddrAdd(vmArena->base, vmArena->tablesSize));
  VMDestroy(vm);

  EVENT_P(ArenaDestroy, vmArena);
}


/* VMArenaReserved -- return the amount of reserved address space
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

static Size VMArenaReserved(Arena arena)
{
  VMArena vmArena;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  return VMReserved(vmArena->vm);
}


/* VMArenaCommitted -- return the amount of committed virtual memory
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

static Size VMArenaCommitted(Arena arena)
{
  VMArena vmArena;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  return VMMapped(vmArena->vm);
}


/* indexOfAddr -- return the index of the page containing an address
 *
 * .index.addr: The address passed may be equal to the limit of the
 * arena, in which case the last page index plus one is returned.  (It
 * is, in a sense, the limit index of the page table.)
 */

static Index indexOfAddr(VMArena vmArena, Addr addr)
{
  AVERT(VMArena, vmArena);
  AVER(vmArena->base <= addr);
  AVER(addr <= vmArena->limit);   /* .index.addr */

  return AddrOffset(vmArena->base, addr) >> vmArena->pageShift;
}


/* findFreeInArea -- try to allocate a segment in an area
 *
 * Search for a free run of pages in the free table, but between
 * base and limit.
 */

static Bool findFreeInArea(Index *baseReturn,
                           VMArena vmArena, Size size,
                           Addr base, Addr limit)
{
  Word pages;                   /* number of pages equiv. to size */
  Index basePage, limitPage;	/* Index equiv. to base and limit */
  Index start, end;		/* base and limit of free run */

  AVER(baseReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(AddrIsAligned(base, vmArena->pageSize));
  AVER(AddrIsAligned(limit, vmArena->pageSize));
  AVER(vmArena->base <= base);
  AVER(base < limit);
  AVER(limit <= vmArena->limit);
  AVER(size <= AddrOffset(base, limit));
  AVER(size > (Size)0);
  AVER(SizeIsAligned(size, vmArena->pageSize));

  basePage = indexOfAddr(vmArena, base);
  limitPage = indexOfAddr(vmArena, limit);
  pages = size >> vmArena->pageShift;

  if(!BTFindShortResRange(&start, &end,
                          vmArena->allocTable,
                          basePage, limitPage,
                          pages))
    return FALSE;

  *baseReturn = start;
  return TRUE;
}


/* findFreeInRefSet -- try to allocate a segment with a RefSet
 * 
 * This function finds the intersection of refSet and the set of free
 * pages and tries to find a free run of pages in the resulting set of
 * areas.
 *
 * In other words, it finds space for a segment whose RefSet (see
 * RefSetOfSeg) will be a subset of the specified RefSet.
 */

static Bool findFreeInRefSet(Index *baseReturn,
			     VMArena vmArena, Size size, RefSet refSet)
{
  Arena arena;
  Addr arenaBase, base, limit;
  Size zoneSize;

  AVER(baseReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(size > 0);
  /* Can't check refSet */

  arena = VMArenaArena(vmArena);
  zoneSize = (Size)1 << arena->zoneShift;
  /* .alloc.skip: The first address available for segments, */
  /* is just after the arena tables. */
  arenaBase = PageBase(vmArena, vmArena->tablePages);

  base = arenaBase;
  while(base < vmArena->limit) {
  
    if(RefSetIsMember(arena, refSet, base)) {
      /* Search for a run of zone stripes which are in the RefSet and */
      /* the arena.  Adding the zoneSize might wrap round (to zero, */
      /* because limit is aligned to zoneSize, which is a power of two). */
      limit = base;
      do {
        limit = AddrAlignDown(AddrAdd(limit, zoneSize), zoneSize);

        AVER(limit > base || limit == (Addr)0);

        if(limit >= vmArena->limit || limit < base) {
          limit = vmArena->limit;
          break;
        }

        AVER(base < limit && limit < vmArena->limit);
      } while(RefSetIsMember(arena, refSet, limit));

      /* If the RefSet was universal, then the area found ought to */
      /* be the whole arena. */
      AVER(refSet != RefSetUNIV ||
           (base == arenaBase && limit == vmArena->limit));

      /* Try to allocate a segment in the area. */
      if(AddrOffset(base, limit) >= size &&
         findFreeInArea(baseReturn, vmArena, size, base, limit))
        return TRUE;
      
      base = limit;
    } else {
      /* Adding the zoneSize might wrap round (to zero, because base */
      /* is aligned to zoneSize, which is a power of two). */
      base = AddrAlignDown(AddrAdd(base, zoneSize), zoneSize);
      AVER(base > arenaBase || base == (Addr)0);
      if(base >= vmArena->limit || base < arenaBase) {
        base = vmArena->limit;
        break;
      }
    }
  }

  AVER(base == vmArena->limit);

  return FALSE;
}


/* VMSegAlloc -- allocate a segment from the arena */

static Res VMSegAlloc(Seg *segReturn, SegPref pref, Arena arena, Size size,
		      Pool pool)
{
  VMArena vmArena;
  Index i, pages, base;
  Addr addr;
  Seg seg;
  Res res;

  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  AVER(SizeIsAligned(size, vmArena->pageSize));
  
  /* NULL is used as a discriminator */
  /* (see design.mps.arena.vm.table.disc) therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  if(!findFreeInRefSet(&base, vmArena, size, pref->refSet) &&
     (pref->refSet == RefSetUNIV ||
      !findFreeInRefSet(&base, vmArena, size, RefSetUNIV))) {
    /* .improve.alloc-fail: This could be because the request was */
    /* too large, or perhaps the arena is fragmented.  We could return a */
    /* more meaningful code. */
    return ResRESOURCE;
  }
  
  /* .alloc.early-map: Map in the segment memory before actually */
  /* allocating the pages, so that we can exit straight away if */
  /* we fail.  After this point, there can be no failure. */
  addr = PageBase(vmArena, base);
  res = VMMap(vmArena->vm, addr, AddrAdd(addr, size));
  if(res) return res;

  /* Initialize the generic segment structure. */
  seg = PageSeg(&vmArena->pageTable[base]);
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page information */
  /* in the page table. */
  AVER(!BTGet(vmArena->allocTable, base));
  BTSet(vmArena->allocTable, base);
  pages = size >> vmArena->pageShift;
  if(pages > 1) {
    Addr limit = PageBase(vmArena, base + pages);
    SegSetSingle(seg, FALSE);
    for(i = base + 1; i < base + pages; ++i) {
      AVER(!BTGet(vmArena->allocTable, i));
      BTSet(vmArena->allocTable, i);
      PageTail(&vmArena->pageTable[i])->pool = NULL;
      PageTail(&vmArena->pageTable[i])->seg = seg;
      PageTail(&vmArena->pageTable[i])->limit = limit;
    }
  } else
    SegSetSingle(seg, TRUE);
  
  AVERT(Seg, seg);
  
  EVENT_PPAWP(SegAlloc, vmArena, seg, addr, size, pool);

  *segReturn = seg;
  return ResOK;
}


/* VMSegFree - free a segment in the arena
 *
 * .seg-free.alt: It is possible to re-implement this function without
 * making a call to VMSegLimit, by searching for the end of the segment
 * in .free.loop.
 */

static void VMSegFree(Arena arena, Seg seg)
{
  VMArena vmArena;
  Page page;
  Count pages;
  Index basePage;
  Addr base, limit; 

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  AVERT(Seg, seg);

  page = PageOfSeg(seg);
  limit = VMSegLimit(arena, seg);
  basePage = page - vmArena->pageTable;
  AVER(basePage <= vmArena->pages);

  SegFinish(seg);

  base = PageBase(vmArena, basePage);
  VMUnmap(vmArena->vm, base, limit);

  /* Calculate the number of pages in the segment */
  pages = AddrOffset(base, limit) >> vmArena->pageShift;

  /* There shouldn't be any pages marked free within the segment's */
  /* area of the alloc table. */
  AVER(BTIsSetRange(vmArena->allocTable, basePage, basePage + pages));
  BTResRange(vmArena->allocTable, basePage, basePage + pages);

  EVENT_PP(SegFree, vmArena, seg);
}


/* VMSegBase -- return the base address of a segment
 *
 * The segment base is calculated by working out the index of the
 * segment structure in the page table and then returning the
 * base address of that page.
 */

static Addr VMSegBase(Arena arena, Seg seg)
{
  VMArena vmArena;
  Page page;
  Index i;
  
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  AVERT(Seg, seg);

  page = PageOfSeg(seg);
  i = page - vmArena->pageTable;

  return PageBase(vmArena, i);
}


/* VMSegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

static Addr VMSegLimit(Arena arena, Seg seg)
{
  VMArena vmArena;
  Page page;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  AVERT(Seg, seg);

  if(SegSingle(seg))
    return AddrAdd(VMSegBase(arena, seg), vmArena->pageSize);
  else {
    page = PageOfSeg(seg);
    return PageTail(page+1)->limit;
  }
}


/* VMSegSize -- return the size (limit - base) of a segment
 *
 * .improve.redundant-calc: There is scope for optimizing this,
 * because both base and limit calls do roughly the same thing twice.
 */

static Size VMSegSize(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);

  return AddrOffset(VMSegBase(arena, seg), VMSegLimit(arena, seg));
}


/* VMSegOfAddr -- return the segment which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If it is a head page, return the segment, otherwise follow the
 * tail's pointer back to the segment in the head page.
 */

static Bool VMSegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  VMArena vmArena;
  
  AVER(segReturn != NULL);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  
  if(vmArena->base <= addr && addr < vmArena->limit) {
    Index i = indexOfAddr(vmArena, addr);
    /* .addr.free: If the page is recorded as being free then */
    /* either the page is free or it is */
    /* part of the arena tables (see .tablepages) */
    if(BTGet(vmArena->allocTable, i)) {
      Page page = &vmArena->pageTable[i];

      if(PageIsHead(page))
        *segReturn = PageSeg(page);
      else
        *segReturn = PageTail(page)->seg;
      return TRUE;
    }
  }
  
  return FALSE;
}


/* segSearch -- search for a segment
 *
 * .seg-search: Searches for a segment in the arena starting at page
 * index i, return NULL if there is none.  A page is the first page
 * of a segment if it is marked allocated in the allocTable, and
 * its pool is not NULL.
 *
 * .seg-search.private: This function is private to this module and
 * is used in the segment iteration protocol (SegFirst and SegNext).
 */

static Bool segSearch(Seg *segReturn, VMArena vmArena, Index i)
{
  AVER(segReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(vmArena->tablePages <= i);
  AVER(i <= vmArena->pages);

  /* This is a static function called with checked arguments, */
  /* so we don't bother checking them here as well. */

  while(i < vmArena->pages &&
        !(BTGet(vmArena->allocTable, i) &&
          PageIsHead(&vmArena->pageTable[i])))
    ++i;

  if(i == vmArena->pages)
    return FALSE;
  
  AVER(i < vmArena->pages);
  
  *segReturn = PageSeg(&vmArena->pageTable[i]);
  return TRUE;
}


/* VMSegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 */

static Bool VMSegFirst(Seg *segReturn, Arena arena)
{
  VMArena vmArena;

  AVER(segReturn != NULL);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  /* We start from tablePages, as the tables can't be a segment.
   * See .tablepages */
  return segSearch(segReturn, vmArena, vmArena->tablePages);
}


/* VMSegNext -- return the "next" segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.
 *
 * VMSegNext finds the segment with the lowest base address which is
 * greater than a specified address.  The address must be (or must once
 * have been) the base address of a segment.
 */

static Bool VMSegNext(Seg *segReturn, Arena arena, Addr addr)
{
  VMArena vmArena;
  Index i;

  AVER(segReturn != NULL);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  AVER(AddrIsAligned(addr, arena->alignment));

  i = indexOfAddr(vmArena, addr);

  /* There are fewer pages than addresses, therefore the */
  /* page index can never wrap around */
  AVER(i+1 != 0);

  return segSearch(segReturn, vmArena, i + 1);
}


/* mps_arena_class_vm -- return the arena class VM */

static ArenaClassStruct ArenaClassVMStruct = {
  ArenaClassSig,
  "VM",                                 /* name */
  sizeof(VMArenaStruct),                /* size */
  offsetof(VMArenaStruct, arenaStruct), /* offset */
  VMArenaInit,                          /* init */
  VMArenaFinish,                        /* finish */
  VMArenaReserved,                      /* reserved */
  VMArenaCommitted,                     /* committed */
  ArenaNoExtend,                        /* extend */
  ArenaNoRetract,                       /* retract */
  VMSegAlloc,                           /* segAlloc */
  VMSegFree,                            /* segFree */
  VMSegBase,                            /* segBase */
  VMSegLimit,                           /* segLimit */
  VMSegSize,                            /* segSize */
  VMSegOfAddr,                          /* segOfAddr */
  VMSegFirst,                           /* segFirst */
  VMSegNext,                            /* segNext */
  ArenaTrivDescribe,                    /* describe */
  ArenaClassSig
};

mps_arena_class_t mps_arena_class_vm(void)
{
  return (mps_arena_class_t)&ArenaClassVMStruct;
}
