/* impl.c.arenacl: ARENA IMPLEMENTATION USING CLIENT MEMORY
 *
 * $HopeName: MMsrc!arenacl.c(trunk.4) $
 * 
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MM developers
 * 
 * .design: See design.mps.arena.client.
 *
 *
 * .req: The requirements are not fully known. They are inherited from
 * req.epcore, and from other requirements on the MPS. They include:
 * 
 * .req.client: Manage memory provided by the client.
 * 
 * .req.client.only: Operate entirely within client-provided memory.
 * 
 * .req.extend: Allow extension by the client providing additional chunks
 *   of memory.
 * 
 * .req.extend.few: Normal operation will be in one chunk; "a few" chunks is
 *   unusual; "many" chunks is very unlikely.
 * 
 * .req.extend.slow: Speed may degrade proportionally to the number of 
 *   chunks of memory. 
 *
 * .req.place: Allow preferential placement of segments.
 * 
 * .improve: There are various possible improvements:
 *
 * .improve.twiddle: There are several places in which many bits in a bit
 * table are checked or set in sequence. These could all be made much faster
 * with suitable bit-twiddling code.
 *
 */

#include "mpm.h"

SRCID(arenacl, "$HopeName: MMsrc!arenacl.c(trunk.4) $");


typedef struct ArenaCLStruct *ArenaCL;
typedef struct ChunkStruct *Chunk;      /* chunk type */
typedef struct PageStruct *Page;        /* page type */
typedef Word *ABT;                      /* bool table type */


#define ArenaCLSig	((Sig)0x519A6EC1) /* SIGnature AREna CLient */

typedef struct ArenaCLStruct {  /* arena structure */
  ArenaStruct arenaStruct;	/* generic arena structure */
  RingStruct chunkRing;         /* all the chunks */
  Serial chunkSerial;           /* next chunk number */
  Shift pageShift;              /* log2(pageSize), for shifts */
  Size pageSize;                /* size of block managed by PageStruct */
  Sig sig;                      /* impl.h.misc.sig */
} ArenaCLStruct;


#define ChunkSig        ((Sig)0x519C804C) /* SIGnature CHUNK */

typedef struct ChunkStruct {    /* chunk structure */
  Sig sig;                      /* impl.h.misc.sig */
  ArenaCL arenaCL;              /* the arena */
  RingStruct arenaCLRing;       /* ring of chunks within the arena */
  Serial serial;                /* serial within the arena */
  Size pages;                   /* number of pages in chunk */
  Size freePages;               /* number of free pages in chunk */
  Addr base;                    /* base address of chunk */
  Addr limit;                   /* limit address of chunk */
  Addr pageBase;                /* base of first managed page in chunk */
  Page pageTable;               /* the page table */
  ABT freeTable;                /* page free table */
} ChunkStruct;


/* PageStruct -- page structure
 *
 * The page table is lifted entirely from arenavm. See
 * design.mps.arenavm.table.*
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


static Bool ArenaCLCheck(ArenaCL arenaCL)
{
  CHECKS(ArenaCL, arenaCL);
  CHECKL(RingCheck(&arenaCL->chunkRing));
  /* no possible check on arenaCL->chunkSerial */
  CHECKL(arenaCL->pageShift < MPS_WORD_WIDTH);
  CHECKL(arenaCL->pageSize == 1uL << arenaCL->pageShift);
  return TRUE;
}


static Bool ChunkCheck(Chunk chunk)
{
  CHECKS(Chunk, chunk);
  CHECKU(ArenaCL, chunk->arenaCL);
  CHECKL(RingCheck(&chunk->arenaCLRing));
  CHECKL(chunk->serial <= chunk->arenaCL->chunkSerial);
  CHECKL(chunk->freePages <= chunk->pages);
  /* check base and limit: */
  CHECKL(chunk->base != (Addr)0);
  CHECKL(chunk->limit != (Addr)0);
  CHECKL(chunk->base < chunk->limit);
  /* check the control structures are not NULL: */
  CHECKL(chunk->pageBase != (Addr)0);
  CHECKL(chunk->pageTable != NULL);
  CHECKL(chunk->freeTable != NULL);
  /* check the control structures are between base and limit */
  /* (allowing for the case in which the chunk manages no pages): */
  CHECKL((Addr)chunk >= chunk->base);
  CHECKL((Addr)chunk < chunk->limit);
  CHECKL(chunk->pageBase > chunk->base);
  CHECKL(chunk->pageBase <= chunk->limit);
  CHECKL((Addr)chunk->pageTable > chunk->base);
  CHECKL((Addr)chunk->pageTable <= chunk->limit);
  CHECKL((Addr)chunk->freeTable > chunk->base);
  CHECKL((Addr)chunk->freeTable <= chunk->limit);
  /* check order of control structures within chunk: */
  CHECKL((Addr)chunk < (Addr)chunk->pageTable);
  CHECKL((Addr)chunk->pageTable <= (Addr)chunk->freeTable);
  CHECKL((Addr)chunk->freeTable <= (Addr)chunk->pageBase);
  /* check size of control structures within chunk: */
        /* enough size for chunk struct: */
  CHECKL(AddrOffset(chunk, chunk->pageTable) >= sizeof(ChunkStruct));
        /* enough space for page table: */
  CHECKL(AddrOffset(chunk->pageTable, chunk->freeTable) / sizeof(PageStruct)
         >= chunk->pages);
        /* enough space for free table: */
  CHECKL(AddrOffset(chunk->freeTable, chunk->pageBase) / sizeof(Word)
         >= SizeAlignUp(chunk->pages,MPS_WORD_WIDTH) >> MPS_WORD_SHIFT);
        /* enough space for pages: */
  CHECKL((AddrOffset(chunk->pageBase, chunk->limit) >> chunk->arenaCL->pageShift)
         == chunk->pages);
  /* .check.tables: could check the consistency of the tables, but not O(1) */
  return TRUE;
}

/* would like to be able to write a PageCheck, but Pages don't even
 * have a signature */

/* Page Index to Base address mapping
 *
 * See design.mps.arenavm.table.linear
 */

#define PageBase(chunk, pi) \
  AddrAdd((chunk)->pageBase, ((pi) << (chunk)->arenaCL->pageShift))

/* Index Types
 * 
 * PI is the type of a value used to index into a page table.
 * 
 * BI is the type of a value used to index into a bool table (See ABTGet
 * and ABTSet in this module).
 */

typedef Size PI;
typedef Size BI;

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


#define ArenaArenaCL(arena) PARENT(ArenaCLStruct, arenaStruct, arena)


/* ChunkCreate -- create a chunk */

static Res ChunkCreate(Chunk *chunkReturn, Addr base, Addr limit, ArenaCL arenaCL)
{
  Chunk chunk;
  Addr a;
  Size tablePages;
  Size freeTableWords;
  PI i;

  AVERT(ArenaCL, arenaCL);
  AVER(chunkReturn != NULL);
  AVER(base != (Addr)0);
  AVER(limit != (Addr)0);
  AVER(limit > base);

  /* allocate the chunk */

  a = AddrAlignUp(base,MPS_PF_ALIGN);
  chunk = (Chunk) a;

  /* figure out the sizes and locations of the control structures */
  a = AddrAlignUp(AddrAdd(a,sizeof(ChunkStruct)), MPS_PF_ALIGN);

  if (a > limit) /* the chunk is too small */
    return ResMEMORY;

  tablePages = AddrOffset(a,limit) >> arenaCL->pageShift;

  chunk->pageTable = (Page) a;
  a = AddrAlignUp(AddrAdd(a,sizeof(PageStruct) * tablePages), MPS_PF_ALIGN);

  chunk->freeTable = (ABT) a;
  freeTableWords = SizeAlignUp(tablePages, MPS_WORD_WIDTH) >> MPS_WORD_SHIFT;
  a = AddrAlignUp(AddrAdd(a,freeTableWords * sizeof(Word)), MPS_PF_ALIGN);

  /* the rest is in managed pages; there may be some wastage at the end */
  chunk->pageBase = a;

  /* initialize the remaining slots */
  chunk->base = base;
  chunk->limit = limit;
  chunk->pages = AddrOffset(chunk->pageBase, chunk->limit) >> arenaCL->pageShift;
  chunk->freePages = chunk->pages;
  chunk->arenaCL = arenaCL;

  /* initialize the freeTable */
  /* .improve.twiddle.init: Could go a lot faster with bit twiddling */
  for(i = 0; i < tablePages; ++i)
    ABTSet(chunk->freeTable, i, TRUE);

  /* link to the arena */
  RingInit(&chunk->arenaCLRing);
  RingAppend(&arenaCL->chunkRing, &chunk->arenaCLRing);
  chunk->serial = arenaCL->chunkSerial;
  ++ arenaCL->chunkSerial;

  /* sign it, check it, return it */
  chunk->sig = ChunkSig;
  AVERT(Chunk, chunk);

  *chunkReturn = chunk;
  return ResOK;
}


/* ArenaVMAlloc -- allocate space for the arena */

static Res ArenaCLAlloc(Arena *arenaReturn)
{
  /* @@@@ temporary */
  extern void *malloc(size_t);
  void *p = malloc(sizeof(ArenaCLStruct));
  AVER(p != NULL);
  *arenaReturn = p;
  return ResOK;
}


/* ArenaCLFree -- free the arena */

static void ArenaCLFree(Arena arena)
{
  /* @@@@ temporary */
  extern void free(void *);
  free(arena);
}


/* ArenaCLCreate -- create the arena */

static Res ArenaCLInit(Arena arena, Size size, Addr base)
{
  ArenaCL arenaCL;
  Addr limit;
  Res res;
  Chunk chunk;
  
  AVERT(Arena, arena);
  AVER(base != (Addr)0);

#if 0
  if (size < sizeof(ArenaStruct))
    return ResMEMORY;

  limit = AddrAdd(base, size);

  /* allocate the arena */
  base = AddrAlignUp(base, MPS_PF_ALIGN);
  arena = (Arena)base;
  base = AddrAlignUp(AddrAdd(base, sizeof(ArenaStruct)), MPS_PF_ALIGN);
#else
  limit = AddrAdd(base, size);
  base = AddrAlignUp(base, MPS_PF_ALIGN);
#endif

  if (base > limit)
    return ResMEMORY;

  arenaCL = ArenaArenaCL(arena);
  arenaCL->pageSize = ARENA_CLIENT_PAGE_SIZE;
  arenaCL->pageShift = SizeLog2(arenaCL->pageSize);

  RingInit(&arenaCL->chunkRing);
  arenaCL->chunkSerial = (Serial)0;

  /* have to have a valid arena before calling ChunkCreate */
  arenaCL->sig = ArenaCLSig;
  
  AVERT(ArenaCL, arenaCL);

  res = ChunkCreate(&chunk, base, limit, arenaCL);
  if(res != ResOK) return res;
  
  /* Set the zone shift to divide the initial chunk into the same
   * number of zones as will fit into a reference set (the number of
   * bits in a word). Note that some zones are discontiguous in the
   * arena if the size is not a power of 2.
   */

  arena->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);
  arena->alignment = arenaCL->pageSize;

  return ResOK;
}


/* ArenaCLDestroy -- finish the arena and destroy the space structure */

static void ArenaCLFinish(Arena arena)
{
  ArenaCL arenaCL;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  
  arenaCL->sig = SigInvalid;
}


/* ArenaCLExtend: this extends the arena */

static Res ArenaCLExtend(Arena arena, Addr base, Size size)
{
  ArenaCL arenaCL;
  Chunk chunk;
  Res res;
  Addr limit;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  AVER(base != (Addr)0);
  AVER(size > 0);
  limit = AddrAdd(base,size);
  
  res = ChunkCreate(&chunk, base, limit, arenaCL);
  return res;
}

/* ArenaCLRetract returns ResFAIL if there is no such chunk, or if it
 * exists but is not fully free. [This is really part of the interface
 * design].
 */

static Res ArenaCLRetract(Arena arena, Addr base, Size size)
{
  ArenaCL arenaCL;
  Ring node;
  Addr limit;
  
  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  AVER(base != (Addr)0);
  AVER(size > 0);

  limit = AddrAdd(base, size);

  RING_FOR(node, &arenaCL->chunkRing) {
    Chunk chunk = RING_ELT(Chunk, arenaCLRing, node);
    AVERT(Chunk, chunk);
    if ((chunk->base == base) &&
        (chunk->limit == limit)) {
      /* check that it's empty */
      PI pi;
      for (pi = 0; pi < chunk->pages; pi++) {
        if (ABTGet(chunk->freeTable, pi) == FALSE)
          return ResFAIL;
      }
      return ResOK;
    }
  }

  return ResFAIL;       /* no such chunk */
}


/* ArenaCLReserved -- return the amount of reserved address space
 * ArenaCLCommitted -- return the amount of committed virtual memory
 * 
 * (actually for the client arena, ArenaCLCommitted returns the amount allocated
 *  in segments).
 */

static Size ArenaCLReserved(Arena arena)
{
  ArenaCL arenaCL;
  Size size;
  Ring node;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  
  size = 0;
  RING_FOR(node, &arenaCL->chunkRing) { /* .req.extend.slow */
    Chunk chunk = RING_ELT(Chunk, arenaCLRing, node);
    AVERT(Chunk, chunk);
    size += AddrOffset(chunk->base, chunk->limit);
  }

  return size;
}

static Size ArenaCLCommitted(Arena arena)
{
  ArenaCL arenaCL;
  Size size;
  Ring node;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  
  size = 0;
  RING_FOR(node, &arenaCL->chunkRing) { /* .req.extend.slow */
    Chunk chunk = RING_ELT(Chunk, arenaCLRing, node);
    AVERT(Chunk, chunk);
    size += ((chunk->pages - chunk->freePages) * arenaCL->pageSize);
  }

  return size;
}


/* ChunkSegAlloc: allocate a segment in a chunk */

static Res ChunkSegAlloc(Seg *segReturn, SegPref pref, Size pages, Pool pool,
                         Chunk chunk)
{
  PI pi, count, base = 0;
  Seg seg;
  ArenaCL arenaCL;

  AVER(segReturn != NULL);
  AVERT(Chunk, chunk);

  if (pages > chunk->freePages)
    return ResRESOURCE;

  arenaCL = chunk->arenaCL;
  
  /* Search the free table for a sufficiently-long run of free pages.
   * If we succeed, we go to "found:" with the lowest page number in
   * the run in 'base'. */

  /* .improve.twiddle.search: This code could go a lot faster with
   * twiddling the bit table. */

  /* .improve.clear: I have tried to make this code clear, with
   *  comments &c, but there's room for further clarification. */

  count = 0; /* the number of free pages found in the current run */

  if (pref->high) { /* search down from the top of the chunk */
    pi = chunk->pages;
    while (pi != 0) {
      pi--;
      if (ABTGet(chunk->freeTable,pi)) {
        ++count;
        if (count == pages) { /* then we're done, take the base of this run */
          base = pi;
          goto found;
        }
      } else
        count = 0;
    }
  } else { /* search up from the bottom of the chunk */
    pi = 0;
    while (pi != chunk->pages) {
      if(ABTGet(chunk->freeTable, pi)) {
        if(count == 0)
          base = pi; /* remember the base of this run */
        ++count;
        if(count == pages) /* now we're done */
          goto found;
      } else
        count = 0;
      pi++;
    }
  }
  
  /* No adequate run was found.
   * .improve.alloc-fail: This could be because the request was
   * too large, or perhaps because of fragmentation.  We could return a
   * more meaningful code.
   */
  return ResRESOURCE;

found:
  /* Initialize the generic segment structure. */
  seg = &chunk->pageTable[base].the.head;
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page,
   * allocate the rest of the pages and store the multi-page information
   * in the page table.
   */
  AVER(ABTGet(chunk->freeTable, base));
  ABTSet(chunk->freeTable, base, FALSE);
  if(pages > 1) {
    Addr limit = PageBase(chunk, base + pages);
    seg->single = FALSE;
    for(pi = base + 1; pi < base + pages; ++pi) {
      AVER(ABTGet(chunk->freeTable, pi));
      ABTSet(chunk->freeTable, pi, FALSE);
      chunk->pageTable[pi].the.tail.pool = NULL;
      chunk->pageTable[pi].the.tail.seg = seg;
      chunk->pageTable[pi].the.tail.limit = limit;
    }
  } else {
    seg->single = TRUE;
  }
  chunk->freePages -= pages;
  
  AVERT(Seg, seg);

  *segReturn = seg;
  return ResOK;
}


/* SegAlloc -- allocate a segment from the arena */

static Res ArenaCLSegAlloc(Seg *segReturn, SegPref pref, Arena arena, Size size, Pool pool)
{
  ArenaCL arenaCL;
  Res res;
  Ring node;
  Size pages;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > 0);
  AVERT(Pool, pool);
  
  /* NULL is used as a discriminator (see
   * design.mps.arenavm.table.disc), therefore the real pool must be
   * non-NULL.
   */
  AVER(pool != NULL);

  AVER(SizeIsAligned(size, arenaCL->pageSize));

  pages = size >> arenaCL->pageShift;

  RING_FOR(node, &arenaCL->chunkRing) { /* .req.extend.slow */
    Chunk chunk = RING_ELT(Chunk, arenaCLRing, node);
    res = ChunkSegAlloc(segReturn, pref, pages, pool, chunk);
    if (res == ResOK)
      return res;
  }
  return ResRESOURCE;
}

/* SegChunk: identify the chunk (and index) in which a segment resides */

static Res ArenaCLSegChunk(Chunk *chunkReturn, PI *piReturn, Seg seg, ArenaCL arenaCL)
{
  Page page;
  Ring node;
  
  AVER(chunkReturn != NULL);
  AVERT(Seg, seg);
  AVERT(ArenaCL, arenaCL);

  page = PARENT(PageStruct, the.head, seg);

  RING_FOR(node, &arenaCL->chunkRing) {
    Chunk chunk = RING_ELT(Chunk, arenaCLRing, node);
    if ((page >= chunk->pageTable) &&
        (page < (chunk->pageTable + chunk->pages))) {
      *piReturn = page - chunk->pageTable;
      *chunkReturn = chunk;
      return ResOK;
    }
  }
  return ResFAIL;
}


/* SegBase -- return the base address of a segment
 *
 * The segment base is calculated by identifying the chunk and page
 * index, then multiplying that by the page size and adding it to
 * the chunk base address. */

static Addr ArenaCLSegBase(Arena arena, Seg seg)
{
  ArenaCL arenaCL;
  PI pi;
  Chunk chunk;
  Res res;
  
  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  AVERT(Seg, seg);

  res = ArenaCLSegChunk(&chunk, &pi, seg, arenaCL);
  AVER(res == ResOK);

  return PageBase(chunk, pi);
}


/* SegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

static Addr ArenaCLSegLimit(Arena arena, Seg seg)
{
  ArenaCL arenaCL;
  Page page;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  AVERT(Seg, seg);

  if(seg->single)
    return AddrAdd(ArenaCLSegBase(arena, seg), arenaCL->pageSize);
  else {
    page = PARENT(PageStruct, the.head, seg);
    return page[1].the.tail.limit;
  }
}


/* SegFree - free a segment in the arena */

static void ArenaCLSegFree(Arena arena, Seg seg)
{
  ArenaCL arenaCL;
  Chunk chunk;
  PI pi, pl, pn;
  Addr base, limit; 
  Res res;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  AVERT(Seg, seg);

  res = ArenaCLSegChunk(&chunk, &pi, seg, arenaCL);
  AVER(res == ResOK);

  limit = ArenaCLSegLimit(arena, seg);

  SegFinish(seg);

  /* Remember the base address of the segment so it can be */
  /* unmapped .free.unmap */
  base = PageBase(chunk, pi);

  /* Calculate the number of pages in the segment, and hence the
   * limit for .free.loop */
  pn = AddrOffset(base, limit) >> arenaCL->pageShift;
  pl = pi + pn;
  /* .free.loop: */
  for( ; pi < pl; ++pi) {
    AVER(ABTGet(chunk->freeTable, pi) == FALSE);
    ABTSet(chunk->freeTable, pi, TRUE);
  }

  chunk->freePages += pn;

  /* Double check that .free.loop takes us to the limit page of the
   * segment.
   */
  AVER(PageBase(chunk, pi) == limit);
}


/* SegOfAddr -- return the segment which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If it is a head page, return the segment, otherwise follow the
 * tail's pointer back to the segment in the head page.
 */

static Bool ArenaCLSegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  ArenaCL arenaCL;
  Ring node;
  
  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  AVER(segReturn != NULL);
  
  RING_FOR(node, &arenaCL->chunkRing) {
    Chunk chunk = RING_ELT(Chunk, arenaCLRing, node);
    if(chunk->base <= addr && addr < chunk->limit) {
      PI pi = AddrOffset(chunk->pageBase, addr) >> arenaCL->pageShift;
      if(!ABTGet(chunk->freeTable, pi)) {
        Page page = &chunk->pageTable[pi];
        if(page->the.head.pool != NULL)
          *segReturn = &page->the.head;
        else
          *segReturn = page->the.tail.seg;
        return TRUE;
      }
    }
  }
  
  return FALSE;
}


/* SegSearchChunk -- search for a segment in a given chunk
 *
 * Searches for a segment in the chunk starting at page index pi,
 * return NULL if there is none.  A segment is present if it is
 * not free, and its pool is not NULL.
 *
 * This function is private to this module and is used in the segment
 * iteration protocol (SegFirst and SegNext).
 */
static Seg SegSearchChunk(Chunk chunk, PI pi)
{
  AVERT(Chunk, chunk);
  AVER(pi <= chunk->pages);

  while(pi < chunk->pages &&
        (ABTGet(chunk->freeTable, pi) ||
         chunk->pageTable[pi].the.head.pool == NULL))
    ++pi;
  
  if(pi < chunk->pages)
    return &chunk->pageTable[pi].the.head;
  
  AVER(pi == chunk->pages);
  return NULL;
}


/* SegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 */

static Seg ArenaCLSegFirst(Arena arena)
{
  ArenaCL arenaCL;
  Ring node;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  
  /* must do the right thing for chunks with no pages */
  RING_FOR(node, &arenaCL->chunkRing) {
    Chunk chunk = RING_ELT(Chunk, arenaCLRing, node);
    Seg seg = SegSearchChunk(chunk, 0);
    if (seg != NULL)
      return seg;
  }

  return NULL;
}


/* SegNext -- return the next segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.
 */

static Seg ArenaCLSegNext(Arena arena, Seg seg)
{
  ArenaCL arenaCL;
  Chunk chunk;
  PI pi;
  Res res;
  Seg next;

  AVERT(Arena, arena);
  arenaCL = ArenaArenaCL(arena);
  AVERT(ArenaCL, arenaCL);
  AVERT(Seg, seg);

  res = ArenaCLSegChunk(&chunk, &pi, seg, arenaCL);
  AVER(res == ResOK);

  next = SegSearchChunk(chunk, pi+1);

  while (next == NULL) { /* then we've reached the end of the chunk */
    Ring node = &chunk->arenaCLRing;
    node = node->next;
    if (node == &arenaCL->chunkRing)
      return NULL;
    chunk = RING_ELT(Chunk, arenaCLRing, node);
    next = SegSearchChunk(chunk,0);
  }
  return next;
}


static ArenaClassStruct ArenaClassCLStruct = {
  ArenaClassSig,
  "CL",					/* name */
  sizeof(ArenaCLStruct),		/* size */
  offsetof(ArenaCLStruct, arenaStruct),	/* offset */
  ArenaCLAlloc,				/* alloc */
  ArenaCLFree,				/* free */
  ArenaCLInit,				/* init */
  ArenaCLFinish,			/* finish */
  ArenaCLReserved,			/* reserved */
  ArenaCLCommitted,			/* committed */
  ArenaCLExtend,			/* extend */
  ArenaCLRetract,			/* retract */
  ArenaCLSegAlloc,			/* segAlloc */
  ArenaCLSegFree,			/* segFree */
  ArenaCLSegBase,			/* segBase */
  ArenaCLSegLimit,			/* segLimit */
  ArenaCLSegOfAddr,			/* segOfAddr */
  ArenaCLSegFirst,			/* segFirst */
  ArenaCLSegNext,			/* segNext */
  ArenaClassSig			/* @@@@ */
};

ArenaClass ArenaClassCL(void)
{
  return &ArenaClassCLStruct;
}
