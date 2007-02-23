/*
 * Generational Conservative Garbage Collector for CMUCL x86.
 *
 * This code was written by Douglas T. Crosher, based on Public Domain
 * codes from Carnegie Mellon University. This code has been placed in
 * the public domain, and is provided 'as is'.
 *
 * Douglas Crosher, 1996, 1997, 1998, 1999.
 *
 * $Header: /project/cmucl/cvsroot/src/lisp/gencgc.c,v 1.83.2.1 2006/10/27 15:12:45 rtoy Exp $
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include "lisp.h"
#include "arch.h"
#include "internals.h"
#include "os.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "interr.h"
#include "gencgc.h"

/*
 * This value in a hash table hash-vector means that the key uses
 * EQ-based hashing.  That is, the key might be using EQ or EQL for
 * the test.  This MUST match the value used in hash-new.lisp!
 */
#define EQ_BASED_HASH_VALUE     0x80000000

#define gc_abort() lose("GC invariant lost!  File \"%s\", line %d\n", \
			__FILE__, __LINE__)

#if (defined(i386) || defined(__x86_64))

#define set_alloc_pointer(value) \
  SetSymbolValue (ALLOCATION_POINTER, (value))
#define get_alloc_pointer() \
  SymbolValue (ALLOCATION_POINTER)
#define get_binding_stack_pointer() \
  SymbolValue (BINDING_STACK_POINTER)
#define get_pseudo_atomic_atomic() \
  SymbolValue (PSEUDO_ATOMIC_ATOMIC)
#define set_pseudo_atomic_atomic() \
  SetSymbolValue (PSEUDO_ATOMIC_ATOMIC, make_fixnum (1))
#define clr_pseudo_atomic_atomic() \
  SetSymbolValue (PSEUDO_ATOMIC_ATOMIC, make_fixnum (0))
#define get_pseudo_atomic_interrupted() \
  SymbolValue (PSEUDO_ATOMIC_INTERRUPTED)
#define clr_pseudo_atomic_interrupted() \
  SetSymbolValue (PSEUDO_ATOMIC_INTERRUPTED, make_fixnum (0))

#define set_current_region_free(value) \
  SetSymbolValue(CURRENT_REGION_FREE_POINTER, (value))
#define set_current_region_end(value) \
  SetSymbolValue(CURRENT_REGION_END_ADDR, (value))
#define get_current_region_free() \
  SymbolValue(CURRENT_REGION_FREE_POINTER)

#define set_current_region_end(value) \
  SetSymbolValue(CURRENT_REGION_END_ADDR, (value))

#elif defined(sparc)

/*
 * current_dynamic_space_free_pointer contains the pseudo-atomic
 * stuff, so we need to preserve those bits when we give it a value.
 * This value better not have any bits set there either!
 */

/*
 * On sparc, we don't need to set the alloc_pointer in the code here
 * because the alloc pointer (current_dynamic_space_free_pointer) is
 * the same as *current-region-free-pointer* and is stored in
 * alloc-tn.
 */
#define set_alloc_pointer(value)
#define get_alloc_pointer() \
  ((unsigned long) current_dynamic_space_free_pointer & ~lowtag_Mask)
#define get_binding_stack_pointer() \
  (current_binding_stack_pointer)
#define get_pseudo_atomic_atomic() \
  ((unsigned long)current_dynamic_space_free_pointer & pseudo_atomic_Value)
#define set_pseudo_atomic_atomic() \
  (current_dynamic_space_free_pointer \
   = (lispobj*) ((unsigned long)current_dynamic_space_free_pointer | pseudo_atomic_Value))
#define clr_pseudo_atomic_atomic() \
  (current_dynamic_space_free_pointer \
   = (lispobj*) ((unsigned long) current_dynamic_space_free_pointer & ~pseudo_atomic_Value))
#define get_pseudo_atomic_interrupted() \
  ((unsigned long) current_dynamic_space_free_pointer & pseudo_atomic_InterruptedValue)
#define clr_pseudo_atomic_interrupted() \
  (current_dynamic_space_free_pointer \
   = (lispobj*) ((unsigned long) current_dynamic_space_free_pointer & ~pseudo_atomic_InterruptedValue))

#define set_current_region_free(value) \
  current_dynamic_space_free_pointer = (lispobj*)((value) | ((long)current_dynamic_space_free_pointer & lowtag_Mask))

#define get_current_region_free() \
  ((long)current_dynamic_space_free_pointer & (~(lowtag_Mask)))

#define set_current_region_end(value) \
  SetSymbolValue(CURRENT_REGION_END_ADDR, (value))

#elif defined(DARWIN)
#ifndef pseudo_atomic_InterruptedValue
#define pseudo_atomic_InterruptedValue 1
#endif
#ifndef pseudo_atomic_Value
#define pseudo_atomic_Value 4
#endif

#define set_alloc_pointer(value) 
#define get_alloc_pointer() \
  ((unsigned long) current_dynamic_space_free_pointer & ~lowtag_Mask)
#define get_binding_stack_pointer() \
  (current_binding_stack_pointer)
#define get_pseudo_atomic_atomic() \
  ((unsigned long)current_dynamic_space_free_pointer & pseudo_atomic_Value)
#define set_pseudo_atomic_atomic() \
  (current_dynamic_space_free_pointer \
   = (lispobj*) ((unsigned long)current_dynamic_space_free_pointer | pseudo_atomic_Value))
#define clr_pseudo_atomic_atomic() \
  (current_dynamic_space_free_pointer \
   = (lispobj*) ((unsigned long) current_dynamic_space_free_pointer & ~pseudo_atomic_Value))
#define get_pseudo_atomic_interrupted() \
  ((unsigned long) current_dynamic_space_free_pointer & pseudo_atomic_InterruptedValue)
#define clr_pseudo_atomic_interrupted() \
  (current_dynamic_space_free_pointer \
   = (lispobj*) ((unsigned long) current_dynamic_space_free_pointer & ~pseudo_atomic_InterruptedValue))

#define set_current_region_free(value) \
  current_dynamic_space_free_pointer = (lispobj*)((value) | ((long)current_dynamic_space_free_pointer & lowtag_Mask))

#define get_current_region_free() \
  ((long)current_dynamic_space_free_pointer & (~(lowtag_Mask)))

#define set_current_region_end(value) \
  SetSymbolValue(CURRENT_REGION_END_ADDR, (value))

#else
#error gencgc is not supported on this platform
#endif

/* Define for activating assertions.  */

#if defined(DARWIN)
#define GC_ASSERTIONS 1
#endif

/* Check for references to stack-allocated objects.  */

#ifdef GC_ASSERTIONS

static void *invalid_stack_start, *invalid_stack_end;

static inline void
check_escaped_stack_object(lispobj * where, lispobj obj)
{
#ifndef DARWIN
    void *p;

    if (Pointerp(obj)
	&& (p = (void *) PTR(obj),
	    (p >= (void *) CONTROL_STACK_START
	     && p < (void *) CONTROL_STACK_END))) {
	char *space;

	if (where >= (lispobj *) DYNAMIC_0_SPACE_START
	    && where < (lispobj *) (DYNAMIC_0_SPACE_START + DYNAMIC_SPACE_SIZE))
	    space = "dynamic space";
	else if (where >= (lispobj *) STATIC_SPACE_START
		 && where <
		 (lispobj *) (STATIC_SPACE_START + STATIC_SPACE_SIZE)) space =
		"static space";
	else if (where >= (lispobj *) READ_ONLY_SPACE_START
		 && where <
		 (lispobj *) (READ_ONLY_SPACE_START +
			      READ_ONLY_SPACE_SIZE)) space = "read-only space";
	else
	    space = NULL;

	/* GC itself uses some stack, so we can't tell exactly where the
	   invalid stack area starts.  Usually, it should be an error if a
	   reference to a stack-allocated object is found, although it
	   is valid to store a reference to a stack-allocated object
	   temporarily in another reachable object, as long as the
	   reference goes away at the end of a dynamic extent.  */

	if (p >= invalid_stack_start && p < invalid_stack_end)
	    lose("Escaped stack-allocated object 0x%08lx at %p in %s\n",
		 (unsigned long) obj, where, space);
#ifndef i386
	else if ((where >= (lispobj *) CONTROL_STACK_START
		  && where < (lispobj *) (CONTROL_STACK_END))
		 || (space == NULL)) {
	    /* Do nothing if it the reference is from the control stack,
	       because that will happen, and that's ok.  Or if it's from
	       an unknown space (typically from scavenging an interrupt
	       context. */
	}
#endif

	else
	    fprintf(stderr,
		    "Reference to stack-allocated object 0x%08lx at %p in %s\n",
		    (unsigned long) obj, where,
		    space ? space : "Unknown space");
    }
#endif
}

#endif /* GC_ASSERTIONS */


#ifdef GC_ASSERTIONS
#define gc_assert(ex)		\
  do {				\
    if (!(ex)) gc_abort ();     \
  } while (0)
#else
#define gc_assert(ex)  (void) 0
#endif


/*
 * The number of generations, an extra is added to this for use as a temp.
 */
#define NUM_GENERATIONS 6

/* Debugging variables. */

/*
 * The verbose level. All non-error messages are disabled at level 0;
 * and only a few rare messages are printed at level 1.
 */
unsigned gencgc_verbose = 0;
unsigned counters_verbose = 0;

/*
 * To enable the use of page protection to help avoid the scavenging
 * of pages that don't have pointers to younger generations.
 */
boolean enable_page_protection = TRUE;

/*
 * Hunt for pointers to old-space, when GCing generations >= verify_gen.
 * Set to NUM_GENERATIONS to disable.
 */
int verify_gens = NUM_GENERATIONS;

/*
 * Enable a pre-scan verify of generation 0 before it's GCed.  (This
 * makes GC very, very slow, so don't enable this unless you really
 * need it!)
 */
boolean pre_verify_gen_0 = FALSE;

/*
 * Enable checking for bad pointers after gc_free_heap called from purify.
 */
#if 0 && defined(DARWIN)
boolean verify_after_free_heap = TRUE;
#else
boolean verify_after_free_heap = FALSE;
#endif

/*
 * Enable the printing of a note when code objects are found in the
 * dynamic space during a heap verify.
 */
boolean verify_dynamic_code_check = FALSE;

/*
 * Enable the checking of code objects for fixup errors after they are
 * transported.  (Only used for x86.)
 */
boolean check_code_fixups = FALSE;

/*
 * To enable unmapping of a page and re-mmaping it to have it zero filled.
 * Note: this can waste a lot of swap on FreeBSD and Open/NetBSD(?) so
 * don't unmap.
 */
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
boolean gencgc_unmap_zero = FALSE;
#else
boolean gencgc_unmap_zero = TRUE;
#endif

/*
 * Enable checking that newly allocated regions are zero filled.
 */
#if 0 && defined(DARWIN)
boolean gencgc_zero_check = TRUE;
boolean gencgc_enable_verify_zero_fill = TRUE;
#else
boolean gencgc_zero_check = FALSE;
boolean gencgc_enable_verify_zero_fill = FALSE;
#endif

/*
 * Enable checking that free pages are zero filled during gc_free_heap
 * called after purify.
 */
#if 0 && defined(DARWIN)
boolean gencgc_zero_check_during_free_heap = TRUE;
#else
boolean gencgc_zero_check_during_free_heap = FALSE;
#endif

/*
 * The minimum size for a large object.
 */
unsigned large_object_size = 4 * PAGE_SIZE;

/*
 * Enable the filtering of stack/register pointers. This could reduce
 * the number of invalid pointers accepted. It will probably degrades
 * interrupt safety during object initialisation.
 */
boolean enable_pointer_filter = TRUE;


/*
 * The total bytes allocated. Seen by (dynamic-usage)
 */
unsigned long bytes_allocated = 0;

/*
 * The total amount of bytes ever allocated.  Not decreased by GC.
 */

volatile unsigned long long bytes_allocated_sum = 0;

/*
 * GC trigger; a value of 0xffffffff represents disabled.
 */
unsigned long auto_gc_trigger = 0xffffffff;

/*
 * Number of pages to reserve for heap overflow.  We want some space
 * available on the heap when we are close to a heap overflow, so we
 * can handle the overflow.  But how much do we really need?  I (rtoy)
 * think 256 pages is probably a decent amount.  (That's 1 MB for x86,
 * 2 MB for sparc, which has 8K pages.)
 */

unsigned long reserved_heap_pages = 256;

/*
 * The src. and dest. generations. Set before a GC starts scavenging.
 */
static int from_space;
static int new_space;


/*
 * GC structures and variables.
 */

/*
 * Number of pages within the dynamic heap, setup from the size of the
 * dynamic space.
 */
unsigned dynamic_space_pages;

/*
 * An array of page structures is statically allocated.
 * This helps quickly map between an address and its page structure.
 */
struct page *page_table;

/*
 * Heap base, needed for mapping addresses to page structures.
 */
static char *heap_base = NULL;

/*
 * Calculate the start address for the given page number.
 */
inline char *
page_address(int page_num)
{
    return heap_base + PAGE_SIZE * page_num;
}

/*
 * Find the page index within the page_table for the given address.
 * Returns -1 on failure.
 */
inline int
find_page_index(void *addr)
{
    int index = (char *) addr - heap_base;

    if (index >= 0) {
	index = (unsigned int) index / PAGE_SIZE;
	if (index < dynamic_space_pages)
	    return index;
    }

    return -1;
}


/*
 * A structure to hold the state of a generation.
 */
struct generation {

    /* The first page that gc_alloc checks on its next call. */
    int alloc_start_page;

    /* The first page that gc_alloc_unboxed checks on its next call. */
    int alloc_unboxed_start_page;

    /*
     * The first page that gc_alloc_large (boxed) considers on its next call.
     * Although it always allocates after the boxed_region.
     */
    int alloc_large_start_page;

    /*
     * The first page that gc_alloc_large (unboxed) considers on its next call.
     * Although it always allocates after the current_unboxed_region.
     */
    int alloc_large_unboxed_start_page;

    /* The bytes allocate to this generation. */
    int bytes_allocated;

    /* The number of bytes at which to trigger a GC */
    int gc_trigger;

    /* To calculate a new level for gc_trigger */
    int bytes_consed_between_gc;

    /* The number of GCs since the last raise. */
    int num_gc;

    /*
     * The average age at after which a GC will raise objects to the
     * next generation.
     */
    int trigger_age;

    /*
     * The cumulative sum of the bytes allocated to this generation. It
     * is cleared after a GC on this generation, and update before new
     * objects are added from a GC of a younger generation. Dividing by
     * the bytes_allocated will give the average age of the memory in
     * this generation since its last GC.
     */
    int cum_sum_bytes_allocated;

    /*
     * A minimum average memory age before a GC will occur helps prevent
     * a GC when a large number of new live objects have been added, in
     * which case a GC could be a waste of time.
     */
    double min_av_mem_age;
};

/*
 * An array of generation structures. There needs to be one more
 * generation structure than actual generations as the oldest
 * generations is temporarily raised then lowered.
 */
static struct generation generations[NUM_GENERATIONS + 1];

/* Statistics about a generation, extracted from the generations
   array.  This gets returned to Lisp.
*/

struct generation_stats {
    int bytes_allocated;
    int gc_trigger;
    int bytes_consed_between_gc;
    int num_gc;
    int trigger_age;
    int cum_sum_bytes_allocated;
    double min_av_mem_age;
};


/*
 * The oldest generation that will currently be GCed by default.
 * Valid values are: 0, 1, ... (NUM_GENERATIONS - 1)
 *
 * The default of (NUM_GENERATIONS - 1) enables GC on all generations.
 *
 * Setting this to 0 effectively disables the generational nature of
 * the GC. In some applications generational GC may not be useful
 * because there are no long-lived objects.
 *
 * An intermediate value could be handy after moving long-lived data
 * into an older generation so an unnecessary GC of this long-lived
 * data can be avoided.
 */
unsigned int gencgc_oldest_gen_to_gc = NUM_GENERATIONS - 1;


/*
 * The maximum free page in the heap is maintained and used to update
 * ALLOCATION_POINTER which is used by the room function to limit its
 * search of the heap. XX Gencgc obviously needs to be better
 * integrated with the lisp code.
 *
 * Except on sparc and ppc, there's no ALLOCATION_POINTER, so it's
 * never updated.  So make this available (non-static).
 */
int last_free_page;



/*
 * Misc. heap functions.
 */

/*
 * Count the number of write protected pages within the given generation.
 */
static int
count_write_protect_generation_pages(int generation)
{
    int i;
    int cnt = 0;
    int mmask, mflags;

    mmask = PAGE_ALLOCATED_MASK | PAGE_WRITE_PROTECTED_MASK
	| PAGE_GENERATION_MASK;
    mflags = PAGE_ALLOCATED_MASK | PAGE_WRITE_PROTECTED_MASK | generation;

    for (i = 0; i < last_free_page; i++)
	if (PAGE_FLAGS(i, mmask) == mflags)
	    cnt++;
    return cnt;
}

/*
 * Count the number of pages within the given generation.
 */
static int
count_generation_pages(int generation)
{
    int i;
    int cnt = 0;
    int mmask, mflags;

    mmask = PAGE_ALLOCATED_MASK | PAGE_GENERATION_MASK;
    mflags = PAGE_ALLOCATED_MASK | generation;

    for (i = 0; i < last_free_page; i++)
	if (PAGE_FLAGS(i, mmask) == mflags)
	    cnt++;
    return cnt;
}

/*
 * Count the number of dont_move pages.
 */
static int
count_dont_move_pages(void)
{
    int i;
    int cnt = 0;
    int mmask;

    mmask = PAGE_ALLOCATED_MASK | PAGE_DONT_MOVE_MASK;

    for (i = 0; i < last_free_page; i++)
	if (PAGE_FLAGS(i, mmask) == mmask)
	    cnt++;
    return cnt;
}

/*
 * Work through the pages and add up the number of bytes used for the
 * given generation.
 */
#ifdef GC_ASSERTIONS
static int
generation_bytes_allocated(int generation)
{
    int i;
    int bytes_allocated = 0;
    int mmask, mflags;

    mmask = PAGE_ALLOCATED_MASK | PAGE_GENERATION_MASK;
    mflags = PAGE_ALLOCATED_MASK | generation;

    for (i = 0; i < last_free_page; i++) {
	if (PAGE_FLAGS(i, mmask) == mflags)
	    bytes_allocated += page_table[i].bytes_used;
    }
    return bytes_allocated;
}
#endif

/*
 * Return the average age of the memory in a generation.
 */
static double
gen_av_mem_age(int gen)
{
    if (generations[gen].bytes_allocated == 0)
	return 0.0;

    return (double) generations[gen].cum_sum_bytes_allocated /
	(double) generations[gen].bytes_allocated;
}

/*
 * The verbose argument controls how much to print out:
 * 0 for normal level of detail; 1 for debugging.
 */
void
print_generation_stats(int verbose)
{
    int i, gens;

#if defined(i386) || defined(__x86_64)
#define FPU_STATE_SIZE 27
    int fpu_state[FPU_STATE_SIZE];
#elif defined(sparc)
    /*
     * 32 (single-precision) FP registers, and the FP state register.
     * But Sparc V9 has 32 double-precision registers (equivalent to 64
     * single-precision, but can't be accessed), so we leave enough room
     * for that.
     */
#define FPU_STATE_SIZE (((32 + 32 + 1) + 1)/2)
    long long fpu_state[FPU_STATE_SIZE];
#elif defined(DARWIN)
#define FPU_STATE_SIZE 32
    long long fpu_state[FPU_STATE_SIZE];
#endif

    /*
     * This code uses the FP instructions which may be setup for Lisp so
     * they need to the saved and reset for C.
     */

    fpu_save(fpu_state);


    /* Number of generations to print out. */
    if (verbose)
	gens = NUM_GENERATIONS + 1;
    else
	gens = NUM_GENERATIONS;

    /* Print the heap stats */
    fprintf(stderr, "          Page count (%d KB)\n", PAGE_SIZE / 1024);
    fprintf(stderr,
	    "   Gen  Boxed Unboxed  LB   LUB    Alloc    Waste    Trigger   WP  GCs Mem-age\n");

    for (i = 0; i < gens; i++) {
	int j;
	int boxed_cnt = 0;
	int unboxed_cnt = 0;
	int large_boxed_cnt = 0;
	int large_unboxed_cnt = 0;

	for (j = 0; j < last_free_page; j++) {
	    int flags = page_table[j].flags;

	    if ((flags & PAGE_GENERATION_MASK) == i) {
		if (flags & PAGE_ALLOCATED_MASK) {
		    /*
		     * Count the number of boxed and unboxed pages within the
		     * given generation.
		     */
		    if (flags & PAGE_UNBOXED_MASK)
			if (flags & PAGE_LARGE_OBJECT_MASK)
			    large_unboxed_cnt++;
			else
			    unboxed_cnt++;
		    else if (flags & PAGE_LARGE_OBJECT_MASK)
			large_boxed_cnt++;
		    else
			boxed_cnt++;
		}
	    }
	}

	gc_assert(generations[i].bytes_allocated ==
		  generation_bytes_allocated(i));
	fprintf(stderr, " %5d: %5d %5d %5d %5d %10d %6d %10d %4d %3d %7.4f\n",
		i, boxed_cnt, unboxed_cnt, large_boxed_cnt, large_unboxed_cnt,
		generations[i].bytes_allocated,
		PAGE_SIZE * count_generation_pages(i) -
		generations[i].bytes_allocated, generations[i].gc_trigger,
		count_write_protect_generation_pages(i), generations[i].num_gc,
		gen_av_mem_age(i));
    }
    fprintf(stderr, "   Total bytes alloc=%ld\n", bytes_allocated);

    fpu_restore(fpu_state);
}

/* Get statistics that are kept "on the fly" out of the generation
   array.
*/
void
get_generation_stats(int gen, struct generation_stats *stats)
{
    if (gen <= NUM_GENERATIONS) {
	stats->bytes_allocated = generations[gen].bytes_allocated;
	stats->gc_trigger = generations[gen].gc_trigger;
	stats->bytes_consed_between_gc =
	    generations[gen].bytes_consed_between_gc;
	stats->num_gc = generations[gen].num_gc;
	stats->trigger_age = generations[gen].trigger_age;
	stats->cum_sum_bytes_allocated =
	    generations[gen].cum_sum_bytes_allocated;
	stats->min_av_mem_age = generations[gen].min_av_mem_age;
    }
}

void
set_gc_trigger(int gen, int trigger)
{
    if (gen <= NUM_GENERATIONS) {
	generations[gen].gc_trigger = trigger;
    }
}

void
set_trigger_age(int gen, int trigger_age)
{
    if (gen <= NUM_GENERATIONS) {
	generations[gen].trigger_age = trigger_age;
    }
}

void
set_min_mem_age(int gen, double min_mem_age)
{
    if (gen <= NUM_GENERATIONS) {
	generations[gen].min_av_mem_age = min_mem_age;
    }
}

/*
 * Allocation routines.
 *
 *
 * To support quick and inline allocation, regions of memory can be
 * allocated and then allocated from with just a free pointer and a
 * check against an end address.
 *
 * Since objects can be allocated to spaces with different properties
 * e.g. boxed/unboxed, generation, ages; there may need to be many
 * allocation regions.
 *
 * Each allocation region may be start within a partly used page.
 * Many features of memory use are noted on a page wise basis,
 * E.g. the generation; so if a region starts within an existing
 * allocated page it must be consistent with this page.
 *
 * During the scavenging of the newspace, objects will be transported
 * into an allocation region, and pointers updated to point to this
 * allocation region. It is possible that these pointers will be
 * scavenged again before the allocation region is closed, E.g. due to
 * trans_list which jumps all over the place to cleanup the list. It
 * is important to be able to determine properties of all objects
 * pointed to when scavenging, E.g to detect pointers to the
 * oldspace. Thus it's important that the allocation regions have the
 * correct properties set when allocated, and not just set when
 * closed.  The region allocation routines return regions with the
 * specified properties, and grab all the pages, setting there
 * properties appropriately, except that the amount used is not known.
 *
 * These regions are used to support quicker allocation using just a
 * free pointer. The actual space used by the region is not reflected
 * in the pages tables until it is closed. It can't be scavenged until
 * closed.
 *
 * When finished with the region it should be closed, which will
 * update the page tables for the actual space used returning unused
 * space. Further it may be noted in the new regions which is
 * necessary when scavenging the newspace.
 *
 * Large objects may be allocated directly without an allocation
 * region, the page tables are updated immediately.
 *
 * Unboxed objects don't contain points to other objects so don't need
 * scavenging. Further they can't contain pointers to younger
 * generations so WP is not needed.  By allocating pages to unboxed
 * objects the whole page never needs scavenging or write protecting.
 */

/*
 * Only using two regions at present, both are for the current
 * newspace generation.
 */
struct alloc_region boxed_region;
struct alloc_region unboxed_region;

#if 0
/*
 * X hack. current lisp code uses the following. Need coping in/out.
 */
void *current_region_free_pointer;
void *current_region_end_addr;
#endif

/* The generation currently being allocated to. X */
static int gc_alloc_generation;

extern void do_dynamic_space_overflow_warning(void);
extern void do_dynamic_space_overflow_error(void);

/* Handle heap overflow here, maybe. */
static void
handle_heap_overflow(const char *msg, int size)
{
    unsigned long heap_size_mb;

    if (msg) {
	fprintf(stderr, msg, size);
    }
#ifndef SPARSE_BLOCK_SIZE
#define SPARSE_BLOCK_SIZE (0)
#endif

    /* Figure out how many MB of heap we have */
    heap_size_mb = (dynamic_space_size + SPARSE_BLOCK_SIZE) >> 20;

    fprintf(stderr, " CMUCL has run out of dynamic heap space (%lu MB).\n",
	    heap_size_mb);
    /* Try to handle heap overflow somewhat gracefully if we can. */
#if defined(trap_DynamicSpaceOverflow) || defined(FEATURE_HEAP_OVERFLOW_CHECK)
    if (reserved_heap_pages == 0) {
	fprintf(stderr, "\n Returning to top-level.\n");
	do_dynamic_space_overflow_error();
    } else {
	fprintf(stderr,
		"  You can control heap size with the -dynamic-space-size commandline option.\n");
	do_dynamic_space_overflow_warning();
    }
#else
    print_generation_stats(1);

    exit(1);
#endif
}

/*
 * Find a new region with room for at least the given number of bytes.
 *
 * It starts looking at the current generations alloc_start_page. So
 * may pick up from the previous region if there is enough space. This
 * keeps the allocation contiguous when scavenging the newspace.
 *
 * The alloc_region should have been closed by a call to
 * gc_alloc_update_page_tables, and will thus be in an empty state.
 *
 * To assist the scavenging functions, write protected pages are not
 * used. Free pages should not be write protected.
 *
 * It is critical to the conservative GC that the start of regions be
 * known. To help achieve this only small regions are allocated at a
 * time.
 *
 * During scavenging, pointers may be found that point within the
 * current region and the page generation must be set so pointers to
 * the from space can be recognised.  So the generation of pages in
 * the region are set to gc_alloc_generation.  To prevent another
 * allocation call using the same pages, all the pages in the region
 * are allocated, although they will initially be empty.
 */
static void
gc_alloc_new_region(int nbytes, int unboxed, struct alloc_region *alloc_region)
{
    int first_page;
    int last_page;
    int region_size;
    int restart_page;
    int bytes_found;
    int num_pages;
    int i;
    int mmask, mflags;

    /* Shut up some compiler warnings */
    last_page = bytes_found = 0;

#if 0
    fprintf(stderr, "alloc_new_region for %d bytes from gen %d\n",
	    nbytes, gc_alloc_generation);
#endif

    /* Check that the region is in a reset state. */
    gc_assert(alloc_region->first_page == 0
	      && alloc_region->last_page == -1
	      && alloc_region->free_pointer == alloc_region->end_addr);

    if (unboxed)
	restart_page =
	    generations[gc_alloc_generation].alloc_unboxed_start_page;
    else
	restart_page = generations[gc_alloc_generation].alloc_start_page;

    /*
     * Search for a contiguous free region of at least nbytes with the
     * given properties: boxed/unboxed, generation. First setting up the
     * mask and matching flags.
     */

    mmask = PAGE_ALLOCATED_MASK | PAGE_WRITE_PROTECTED_MASK
	| PAGE_LARGE_OBJECT_MASK | PAGE_DONT_MOVE_MASK
	| PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK;
    mflags = PAGE_ALLOCATED_MASK | (unboxed << PAGE_UNBOXED_SHIFT)
	| gc_alloc_generation;

    do {
	first_page = restart_page;

	/*
	 * First search for a page with at least 32 bytes free, that is
	 * not write protected, or marked dont_move.
	 */

	while (first_page < dynamic_space_pages) {
	    int flags = page_table[first_page].flags;

	    if (!(flags & PAGE_ALLOCATED_MASK)
		|| ((flags & mmask) == mflags &&
		    page_table[first_page].bytes_used < PAGE_SIZE - 32))
		break;
	    first_page++;
	}

	/* Check for a failure */
	if (first_page >= dynamic_space_pages - reserved_heap_pages) {
#if 0
	    handle_heap_overflow("*A2 gc_alloc_new_region failed, nbytes=%d.\n",
				 nbytes);
#else
	    break;
#endif
	}

	gc_assert(!PAGE_WRITE_PROTECTED(first_page));

#if 0
	fprintf(stderr, "  first_page=%d bytes_used=%d\n",
		first_page, page_table[first_page].bytes_used);
#endif

	/*
	 * Now search forward to calculate the available region size.  It
	 * tries to keeps going until nbytes are found and the number of
	 * pages is greater than some level. This helps keep down the
	 * number of pages in a region.
	 */
	last_page = first_page;
	bytes_found = PAGE_SIZE - page_table[first_page].bytes_used;
	num_pages = 1;
	while ((bytes_found < nbytes || num_pages < 2)
	       && last_page < dynamic_space_pages - 1
	       && !PAGE_ALLOCATED(last_page + 1)) {
	    last_page++;
	    num_pages++;
	    bytes_found += PAGE_SIZE;
	    gc_assert(!PAGE_WRITE_PROTECTED(last_page));
	}

	region_size = (PAGE_SIZE - page_table[first_page].bytes_used)
	    + PAGE_SIZE * (last_page - first_page);

	gc_assert(bytes_found == region_size);

#if 0
	fprintf(stderr, "  last_page=%d bytes_found=%d num_pages=%d\n",
		last_page, bytes_found, num_pages);
#endif

	restart_page = last_page + 1;
    }
    while (restart_page < dynamic_space_pages && bytes_found < nbytes);

    if (first_page >= dynamic_space_pages - reserved_heap_pages) {
	handle_heap_overflow("*A2 gc_alloc_new_region failed, nbytes=%d.\n",
			     nbytes);
    }

    /* Check for a failure */
    if (restart_page >= (dynamic_space_pages - reserved_heap_pages)
	&& bytes_found < nbytes) {
	handle_heap_overflow("*A1 gc_alloc_new_region failed, nbytes=%d.\n",
			     nbytes);
    }
#if 0
    fprintf(stderr,
	    "gc_alloc_new_region gen %d: %d bytes: from pages %d to %d: addr=%x\n",
	    gc_alloc_generation, bytes_found, first_page, last_page,
	    page_address(first_page));
#endif

    /* Setup the alloc_region. */
    alloc_region->first_page = first_page;
    alloc_region->last_page = last_page;
    alloc_region->start_addr = page_table[first_page].bytes_used
	+ page_address(first_page);
    alloc_region->free_pointer = alloc_region->start_addr;
    alloc_region->end_addr = alloc_region->start_addr + bytes_found;

    if (gencgc_zero_check) {
	int *p;

	for (p = (int *) alloc_region->start_addr;
	     p < (int *) alloc_region->end_addr; p++)
	    if (*p != 0)
		fprintf(stderr, "** new region not zero @ %lx\n",
			(unsigned long) p);
    }

    /* Setup the pages. */

    /* The first page may have already been in use. */
    if (page_table[first_page].bytes_used == 0) {
	PAGE_FLAGS_UPDATE(first_page, mmask, mflags);
	page_table[first_page].first_object_offset = 0;
    }

    gc_assert(PAGE_ALLOCATED(first_page));
    gc_assert(PAGE_UNBOXED_VAL(first_page) == unboxed);
    gc_assert(PAGE_GENERATION(first_page) == gc_alloc_generation);
    gc_assert(!PAGE_LARGE_OBJECT(first_page));

    for (i = first_page + 1; i <= last_page; i++) {
	PAGE_FLAGS_UPDATE(i, PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK
			  | PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK,
			  PAGE_ALLOCATED_MASK | (unboxed << PAGE_UNBOXED_SHIFT)
			  | gc_alloc_generation);
	/*
	 * This may not be necessary for unboxed regions (think it was
	 * broken before!)
	 */
	page_table[i].first_object_offset =
	    alloc_region->start_addr - page_address(i);
    }

    /* Bump up the last_free_page */
    if (last_page + 1 > last_free_page) {
	last_free_page = last_page + 1;
	set_alloc_pointer((lispobj) ((char *) heap_base +
				     PAGE_SIZE * last_free_page));

    }
}



/*
 * If the record_new_objects flag is 2 then all new regions created
 * are recorded.
 *
 * If it's 1 then it is only recorded if the first page of the
 * current region is <= new_areas_ignore_page. This helps avoid
 * unnecessary recording when doing full scavenge pass.
 *
 * The new_object structure holds the page, byte offset, and size of
 * new regions of objects. Each new area is placed in the array of
 * these structures pointed to by new_areas; new_areas_index holds the
 * offset into new_areas.
 *
 * If new_area overflows NUM_NEW_AREAS then it stops adding them. The
 * later code must detect this an handle it, probably by doing a full
 * scavenge of a generation.
 */

#define NUM_NEW_AREAS 512
static int record_new_objects = 0;
static int new_areas_ignore_page;
struct new_area {
    int page;
    int offset;
    int size;
};
static struct new_area (*new_areas)[];
static int new_areas_index;
int max_new_areas;

/* Add a new area to new_areas. */
static void
add_new_area(int first_page, int offset, int size)
{
    unsigned new_area_start, c;
    int i;

    /* Ignore if full */
    if (new_areas_index >= NUM_NEW_AREAS)
	return;

    switch (record_new_objects) {
      case 0:
	  return;
      case 1:
	  if (first_page > new_areas_ignore_page)
	      return;
	  break;
      case 2:
	  break;
      default:
	  gc_abort();
    }

    new_area_start = PAGE_SIZE * first_page + offset;

    /*
     * Search backwards for a prior area that this follows from.  If
     * found this will save adding a new area.
     */
    for (i = new_areas_index - 1, c = 0; i >= 0 && c < 8; i--, c++) {
	unsigned area_end = PAGE_SIZE * (*new_areas)[i].page
	    + (*new_areas)[i].offset + (*new_areas)[i].size;

#if 0
	fprintf(stderr, "*S1 %d %d %d %d\n", i, c, new_area_start, area_end);
#endif
	if (new_area_start == area_end) {
#if 0
	    fprintf(stderr, "-> Adding to [%d] %d %d %d with %d %d %d:\n",
		    i, (*new_areas)[i].page, (*new_areas)[i].offset,
		    (*new_areas)[i].size, first_page, offset, size);
#endif
	    (*new_areas)[i].size += size;
	    return;
	}
    }
#if 0
    fprintf(stderr, "*S1 %d %d %d\n", i, c, new_area_start);
#endif

    (*new_areas)[new_areas_index].page = first_page;
    (*new_areas)[new_areas_index].offset = offset;
    (*new_areas)[new_areas_index].size = size;
#if 0
    fprintf(stderr, "  new_area %d page %d offset %d size %d\n",
	    new_areas_index, first_page, offset, size);
#endif
    new_areas_index++;

    /* Note the max new_areas used. */
    if (new_areas_index > max_new_areas)
	max_new_areas = new_areas_index;
}


/*
 * Update the tables for the alloc_region. The region may be added to
 * the new_areas.
 *
 * When done the alloc_region its setup so that the next quick alloc
 * will fail safely and thus a new region will be allocated. Further
 * it is safe to try and re-update the page table of this reset
 * alloc_region.
 */
void
gc_alloc_update_page_tables(int unboxed, struct alloc_region *alloc_region)
{
    int more;
    int first_page;
    int next_page;
    int bytes_used;
    int orig_first_page_bytes_used;
    int region_size;
    int byte_cnt;

#if 0
    fprintf(stderr, "gc_alloc_update_page_tables to gen %d: ",
	    gc_alloc_generation);
#endif

    first_page = alloc_region->first_page;

    /* Catch an unused alloc_region. */
    if (first_page == 0 && alloc_region->last_page == -1)
	return;

    next_page = first_page + 1;

    /* Skip if no bytes were allocated */
    if (alloc_region->free_pointer != alloc_region->start_addr) {
	orig_first_page_bytes_used = page_table[first_page].bytes_used;

	gc_assert(alloc_region->start_addr == page_address(first_page) +
		  page_table[first_page].bytes_used);

	/* All the pages used need to be updated */

	/* Update the first page. */

#if 0
	fprintf(stderr, "0");
#endif

	/* If the page was free then setup the gen, and first_object_offset. */
	if (page_table[first_page].bytes_used == 0)
	    gc_assert(page_table[first_page].first_object_offset == 0);

	gc_assert(PAGE_ALLOCATED(first_page));
	gc_assert(PAGE_UNBOXED_VAL(first_page) == unboxed);
	gc_assert(PAGE_GENERATION(first_page) == gc_alloc_generation);
	gc_assert(!PAGE_LARGE_OBJECT(first_page));

	byte_cnt = 0;

	/*
	 * Calc. the number of bytes used in this page. This is not always
	 * the number of new bytes, unless it was free.
	 */
	more = 0;
	bytes_used = alloc_region->free_pointer - page_address(first_page);
	if (bytes_used > PAGE_SIZE) {
	    bytes_used = PAGE_SIZE;
	    more = 1;
	}
	page_table[first_page].bytes_used = bytes_used;
	byte_cnt += bytes_used;

	/*
	 * All the rest of the pages should be free. Need to set their
	 * first_object_offset pointer to the start of the region, and set
	 * the bytes_used.
	 */
	while (more) {
#if 0
	    fprintf(stderr, "+");
#endif
	    gc_assert(PAGE_ALLOCATED(next_page));
	    gc_assert(PAGE_UNBOXED_VAL(next_page) == unboxed);
	    gc_assert(page_table[next_page].bytes_used == 0);
	    gc_assert(PAGE_GENERATION(next_page) == gc_alloc_generation);
	    gc_assert(!PAGE_LARGE_OBJECT(next_page));

	    gc_assert(page_table[next_page].first_object_offset ==
		      alloc_region->start_addr - page_address(next_page));

	    /* Calc. the number of bytes used in this page. */
	    more = 0;
	    bytes_used = alloc_region->free_pointer - page_address(next_page);
	    if (bytes_used > PAGE_SIZE) {
		bytes_used = PAGE_SIZE;
		more = 1;
	    }
	    page_table[next_page].bytes_used = bytes_used;
	    byte_cnt += bytes_used;

	    next_page++;
	}

	region_size = alloc_region->free_pointer - alloc_region->start_addr;
	bytes_allocated += region_size;
	generations[gc_alloc_generation].bytes_allocated += region_size;

	gc_assert(byte_cnt - orig_first_page_bytes_used == region_size);

	/*
	 * Set the generations alloc restart page to the last page of
	 * the region.
	 */
	if (unboxed)
	    generations[gc_alloc_generation].alloc_unboxed_start_page =
		next_page - 1;
	else
	    generations[gc_alloc_generation].alloc_start_page = next_page - 1;

	/* Add the region to the new_areas if requested. */
	if (!unboxed)
	    add_new_area(first_page, orig_first_page_bytes_used, region_size);

#if 0
	fprintf(stderr,
		"  gc_alloc_update_page_tables update %d bytes to gen %d\n",
		region_size, gc_alloc_generation);
#endif
    } else
	/*
	 * No bytes allocated. Unallocate the first_page if there are 0 bytes_used.
	 */
    if (page_table[first_page].bytes_used == 0)
	page_table[first_page].flags &= ~PAGE_ALLOCATED_MASK;

    /* Unallocate any unused pages. */
    while (next_page <= alloc_region->last_page) {
	gc_assert(page_table[next_page].bytes_used == 0);
	page_table[next_page].flags &= ~PAGE_ALLOCATED_MASK;
	next_page++;
    }

    /* Reset the alloc_region. */
    alloc_region->first_page = 0;
    alloc_region->last_page = -1;
    alloc_region->start_addr = page_address(0);
    alloc_region->free_pointer = page_address(0);
    alloc_region->end_addr = page_address(0);

#if 0
    fprintf(stderr, "\n");
#endif
}



static inline void *gc_quick_alloc(int nbytes);

/*
 * Allocate a possibly large object.
 */
static void *
gc_alloc_large(int nbytes, int unboxed, struct alloc_region *alloc_region)
{
    int first_page;
    int last_page;
    int region_size;
    int restart_page;
    int bytes_found;
    int num_pages;
    int orig_first_page_bytes_used;
    int byte_cnt;
    int more;
    int bytes_used;
    int next_page;
    int large = (nbytes >= large_object_size);
    int mmask, mflags;


    /* Shut up some compiler warnings */
    last_page = bytes_found = 0;

#if 0
    if (nbytes > 200000)
	fprintf(stderr, "*** alloc_large %d\n", nbytes);
#endif

#if 0
    fprintf(stderr, "gc_alloc_large for %d bytes from gen %d\n",
	    nbytes, gc_alloc_generation);
#endif

    /*
     * If the object is small, and there is room in the current region
     * then allocation it in the current region.
     */
    if (!large && alloc_region->end_addr - alloc_region->free_pointer >= nbytes)
	return gc_quick_alloc(nbytes);

    /*
     * Search for a contiguous free region of at least nbytes. If it's a
     * large object then align it on a page boundary by searching for a
     * free page.
     */

    /*
     * To allow the allocation of small objects without the danger of
     * using a page in the current boxed region, the search starts after
     * the current boxed free region. XX could probably keep a page
     * index ahead of the current region and bumped up here to save a
     * lot of re-scanning.
     */
    if (unboxed)
	restart_page =
	    generations[gc_alloc_generation].alloc_large_unboxed_start_page;
    else
	restart_page = generations[gc_alloc_generation].alloc_large_start_page;
    if (restart_page <= alloc_region->last_page)
	restart_page = alloc_region->last_page + 1;

    /* Setup the mask and matching flags. */

    mmask = PAGE_ALLOCATED_MASK | PAGE_WRITE_PROTECTED_MASK
	| PAGE_LARGE_OBJECT_MASK | PAGE_DONT_MOVE_MASK
	| PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK;
    mflags = PAGE_ALLOCATED_MASK | (unboxed << PAGE_UNBOXED_SHIFT)
	| gc_alloc_generation;

    do {
	first_page = restart_page;

	if (large)
	    while (first_page < dynamic_space_pages
		   && PAGE_ALLOCATED(first_page)) first_page++;
	else
	    while (first_page < dynamic_space_pages) {
		int flags = page_table[first_page].flags;

		if (!(flags & PAGE_ALLOCATED_MASK)
		    || ((flags & mmask) == mflags &&
			page_table[first_page].bytes_used < PAGE_SIZE - 32))
		    break;
		first_page++;
	    }

	/* Check for a failure */
	if (first_page >= dynamic_space_pages - reserved_heap_pages) {
#if 0
	    handle_heap_overflow("*A2 gc_alloc_large failed, nbytes=%d.\n",
				 nbytes);
#else
	    break;
#endif
	}
	gc_assert(!PAGE_WRITE_PROTECTED(first_page));

#if 0
	fprintf(stderr, "  first_page=%d bytes_used=%d\n",
		first_page, page_table[first_page].bytes_used);
#endif

	last_page = first_page;
	bytes_found = PAGE_SIZE - page_table[first_page].bytes_used;
	num_pages = 1;
	while (bytes_found < nbytes
	       && last_page < dynamic_space_pages - 1
	       && !PAGE_ALLOCATED(last_page + 1)) {
	    last_page++;
	    num_pages++;
	    bytes_found += PAGE_SIZE;
	    gc_assert(!PAGE_WRITE_PROTECTED(last_page));
	}

	region_size = (PAGE_SIZE - page_table[first_page].bytes_used)
	    + PAGE_SIZE * (last_page - first_page);

	gc_assert(bytes_found == region_size);

#if 0
	fprintf(stderr, "  last_page=%d bytes_found=%d num_pages=%d\n",
		last_page, bytes_found, num_pages);
#endif

	restart_page = last_page + 1;
    }
    while ((restart_page < dynamic_space_pages) && (bytes_found < nbytes));

    if (first_page >= dynamic_space_pages - reserved_heap_pages) {
	handle_heap_overflow("*A2 gc_alloc_large failed, nbytes=%d.\n", nbytes);
    }

    /* Check for a failure */
    if (restart_page >= (dynamic_space_pages - reserved_heap_pages)
	&& bytes_found < nbytes) {
	handle_heap_overflow("*A1 gc_alloc_large failed, nbytes=%d.\n", nbytes);
    }
#if 0
    if (large)
	fprintf(stderr,
		"gc_alloc_large gen %d: %d of %d bytes: from pages %d to %d: addr=%x\n",
		gc_alloc_generation, nbytes, bytes_found, first_page, last_page,
		page_address(first_page));
#endif

    gc_assert(first_page > alloc_region->last_page);
    if (unboxed)
	generations[gc_alloc_generation].alloc_large_unboxed_start_page =
	    last_page;
    else
	generations[gc_alloc_generation].alloc_large_start_page = last_page;

    /* Setup the pages. */
    orig_first_page_bytes_used = page_table[first_page].bytes_used;

    /*
     * If the first page was free then setup the gen, and
     * first_object_offset.
     */

    if (large)
	mflags |= PAGE_LARGE_OBJECT_MASK;
    if (page_table[first_page].bytes_used == 0) {
	PAGE_FLAGS_UPDATE(first_page, mmask, mflags);
	page_table[first_page].first_object_offset = 0;
    }

    gc_assert(PAGE_ALLOCATED(first_page));
    gc_assert(PAGE_UNBOXED_VAL(first_page) == unboxed);
    gc_assert(PAGE_GENERATION(first_page) == gc_alloc_generation);
    gc_assert(PAGE_LARGE_OBJECT_VAL(first_page) == large);

    byte_cnt = 0;

    /*
     * Calc. the number of bytes used in this page. This is not
     * always the number of new bytes, unless it was free.
     */
    more = 0;
    bytes_used = nbytes + orig_first_page_bytes_used;
    if (bytes_used > PAGE_SIZE) {
	bytes_used = PAGE_SIZE;
	more = 1;
    }
    page_table[first_page].bytes_used = bytes_used;
    byte_cnt += bytes_used;

    next_page = first_page + 1;

    /*
     * All the rest of the pages should be free. Need to set their
     * first_object_offset pointer to the start of the region, and set
     * the bytes_used.
     */
    while (more) {
#if 0
	fprintf(stderr, "+");
#endif

	gc_assert(!PAGE_ALLOCATED(next_page));
	gc_assert(page_table[next_page].bytes_used == 0);
	PAGE_FLAGS_UPDATE(next_page, mmask, mflags);

	page_table[next_page].first_object_offset =
	    orig_first_page_bytes_used - PAGE_SIZE * (next_page - first_page);

	/* Calc. the number of bytes used in this page. */
	more = 0;
	bytes_used = nbytes + orig_first_page_bytes_used - byte_cnt;
	if (bytes_used > PAGE_SIZE) {
	    bytes_used = PAGE_SIZE;
	    more = 1;
	}
	page_table[next_page].bytes_used = bytes_used;
	byte_cnt += bytes_used;

	next_page++;
    }

    gc_assert(byte_cnt - orig_first_page_bytes_used == nbytes);

    bytes_allocated += nbytes;
    generations[gc_alloc_generation].bytes_allocated += nbytes;

    /* Add the region to the new_areas if requested. */
    if (!unboxed)
	add_new_area(first_page, orig_first_page_bytes_used, nbytes);

    /* Bump up the last_free_page */
    if (last_page + 1 > last_free_page) {
	last_free_page = last_page + 1;
	set_alloc_pointer((lispobj) ((char *) heap_base +
				     PAGE_SIZE * last_free_page));
    }

    return (void *) (page_address(first_page) + orig_first_page_bytes_used);
}

/*
 * Allocate bytes from the boxed_region. It first checks if there is
 * room, if not then it calls gc_alloc_new_region to find a new region
 * with enough space. A pointer to the start of the region is returned.
 */
static void *
gc_alloc(int nbytes)
{
    char *new_free_pointer;

#if 0
    fprintf(stderr, "gc_alloc %d\n", nbytes);
#endif

    /* Check if there is room in the current alloc region. */
    new_free_pointer = boxed_region.free_pointer + nbytes;

    if (new_free_pointer <= boxed_region.end_addr) {
	/* If so then allocate from the current alloc region. */
	char *new_obj = boxed_region.free_pointer;

	boxed_region.free_pointer = new_free_pointer;

	/* Check if the alloc region is almost empty. */
	if (boxed_region.end_addr - boxed_region.free_pointer <= 32) {
	    /* If so finished with the current region. */
	    gc_alloc_update_page_tables(0, &boxed_region);
	    /* Setup a new region. */
	    gc_alloc_new_region(32, 0, &boxed_region);
	}
	return (void *) new_obj;
    }

    /* Else not enough free space in the current region. */

    /*
     * If there is a bit of room left in the current region then
     * allocate a large object.
     */
    if (boxed_region.end_addr - boxed_region.free_pointer > 32)
	return gc_alloc_large(nbytes, 0, &boxed_region);

    /* Else find a new region. */

    /* Finished with the current region. */
    gc_alloc_update_page_tables(0, &boxed_region);

    /* Setup a new region. */
    gc_alloc_new_region(nbytes, 0, &boxed_region);

    /* Should now be enough room. */

    /* Check if there is room in the current region. */
    new_free_pointer = boxed_region.free_pointer + nbytes;

    if (new_free_pointer <= boxed_region.end_addr) {
	/* If so then allocate from the current region. */
	void *new_obj = boxed_region.free_pointer;

	boxed_region.free_pointer = new_free_pointer;

	/* Check if the current region is almost empty. */
	if (boxed_region.end_addr - boxed_region.free_pointer <= 32) {
	    /* If so find, finished with the current region. */
	    gc_alloc_update_page_tables(0, &boxed_region);

	    /* Setup a new region. */
	    gc_alloc_new_region(32, 0, &boxed_region);
	}

	return (void *) new_obj;
    }

    /* Shouldn't happen? */
    gc_assert(0);
    return 0;
}

/*
 * Allocate space from the boxed_region. If there is not enough free
 * space then call gc_alloc to do the job. A pointer to the start of
 * the region is returned.
 */
static inline void *
gc_quick_alloc(int nbytes)
{
    char *new_free_pointer;

    /* Check if there is room in the current region. */
    new_free_pointer = boxed_region.free_pointer + nbytes;

    if (new_free_pointer <= boxed_region.end_addr) {
	/* If so then allocate from the current region. */
	void *new_obj = boxed_region.free_pointer;

	boxed_region.free_pointer = new_free_pointer;
	return (void *) new_obj;
    }

    /* Else call gc_alloc */
    return gc_alloc(nbytes);
}

/*
 * Allocate space for the boxed object. If it is a large object then
 * do a large alloc else allocate from the current region. If there is
 * not enough free space then call gc_alloc to do the job. A pointer
 * to the start of the region is returned.
 */
static inline void *
gc_quick_alloc_large(int nbytes)
{
    char *new_free_pointer;

    if (nbytes >= large_object_size)
	return gc_alloc_large(nbytes, 0, &boxed_region);

    /* Check if there is room in the current region. */
    new_free_pointer = boxed_region.free_pointer + nbytes;

    if (new_free_pointer <= boxed_region.end_addr) {
	/* If so then allocate from the current region. */
	void *new_obj = boxed_region.free_pointer;

	boxed_region.free_pointer = new_free_pointer;
	return (void *) new_obj;
    }

    /* Else call gc_alloc */
    return gc_alloc(nbytes);
}




static void *
gc_alloc_unboxed(int nbytes)
{
    char *new_free_pointer;

#if 0
    fprintf(stderr, "gc_alloc_unboxed %d\n", nbytes);
#endif

    /* Check if there is room in the current region. */
    new_free_pointer = unboxed_region.free_pointer + nbytes;

    if (new_free_pointer <= unboxed_region.end_addr) {
	/* If so then allocate from the current region. */
	void *new_obj = unboxed_region.free_pointer;

	unboxed_region.free_pointer = new_free_pointer;

	/* Check if the current region is almost empty. */
	if ((unboxed_region.end_addr - unboxed_region.free_pointer) <= 32) {
	    /* If so finished with the current region. */
	    gc_alloc_update_page_tables(1, &unboxed_region);

	    /* Setup a new region. */
	    gc_alloc_new_region(32, 1, &unboxed_region);
	}

	return (void *) new_obj;
    }

    /* Else not enough free space in the current region. */

    /*
     * If there is a bit of room left in the current region then
     * allocate a large object.
     */
    if (unboxed_region.end_addr - unboxed_region.free_pointer > 32)
	return gc_alloc_large(nbytes, 1, &unboxed_region);

    /* Else find a new region. */

    /* Finished with the current region. */
    gc_alloc_update_page_tables(1, &unboxed_region);

    /* Setup a new region. */
    gc_alloc_new_region(nbytes, 1, &unboxed_region);

    /* Should now be enough room. */

    /* Check if there is room in the current region. */
    new_free_pointer = unboxed_region.free_pointer + nbytes;

    if (new_free_pointer <= unboxed_region.end_addr) {
	/* If so then allocate from the current region. */
	void *new_obj = unboxed_region.free_pointer;

	unboxed_region.free_pointer = new_free_pointer;

	/* Check if the current region is almost empty. */
	if ((unboxed_region.end_addr - unboxed_region.free_pointer) <= 32) {
	    /* If so find, finished with the current region. */
	    gc_alloc_update_page_tables(1, &unboxed_region);

	    /* Setup a new region. */
	    gc_alloc_new_region(32, 1, &unboxed_region);
	}

	return (void *) new_obj;
    }

    /* Shouldn't happen? */
    gc_assert(0);
    return 0;
}

static inline void *
gc_quick_alloc_unboxed(int nbytes)
{
    char *new_free_pointer;

    /* Check if there is room in the current region. */
    new_free_pointer = unboxed_region.free_pointer + nbytes;

    if (new_free_pointer <= unboxed_region.end_addr) {
	/* If so then allocate from the current region. */
	void *new_obj = unboxed_region.free_pointer;

	unboxed_region.free_pointer = new_free_pointer;

	return (void *) new_obj;
    }

    /* Else call gc_alloc */
    return gc_alloc_unboxed(nbytes);
}

/*
 * Allocate space for the object. If it is a large object then do a
 * large alloc else allocate from the current region. If there is not
 * enough free space then call gc_alloc to do the job.
 *
 * A pointer to the start of the region is returned.
 */
static inline void *
gc_quick_alloc_large_unboxed(int nbytes)
{
    char *new_free_pointer;

    if (nbytes >= large_object_size)
	return gc_alloc_large(nbytes, 1, &unboxed_region);

    /* Check if there is room in the current region. */
    new_free_pointer = unboxed_region.free_pointer + nbytes;

    if (new_free_pointer <= unboxed_region.end_addr) {
	/* If so then allocate from the current region. */
	void *new_obj = unboxed_region.free_pointer;

	unboxed_region.free_pointer = new_free_pointer;

	return (void *) new_obj;
    }

    /* Else call gc_alloc */
    return gc_alloc_unboxed(nbytes);
}

/***************************************************************************/


/* Scavenging/transporting routines derived from gc.c */

static int (*scavtab[256]) (lispobj * where, lispobj object);
static lispobj(*transother[256]) (lispobj object);
static int (*sizetab[256]) (lispobj * where);

static struct weak_pointer *weak_pointers;
static struct scavenger_hook *scavenger_hooks = (struct scavenger_hook *) NIL;

#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))


/* Predicates */

static inline boolean
from_space_p(lispobj obj)
{
    int page_index = (char *) obj - heap_base;

    return page_index >= 0
	&& (page_index =
	    (unsigned int) page_index / PAGE_SIZE) < dynamic_space_pages
	&& PAGE_GENERATION(page_index) == from_space;
}

static inline boolean
new_space_p(lispobj obj)
{
    int page_index = (char *) obj - heap_base;

    return page_index >= 0
	&& (page_index =
	    (unsigned int) page_index / PAGE_SIZE) < dynamic_space_pages
	&& PAGE_GENERATION(page_index) == new_space;
}


/* Copying Objects */


/* Copying Boxed Objects */
static inline lispobj
copy_object(lispobj object, int nwords)
{
    int tag;
    lispobj *new;
    lispobj *source, *dest;

    gc_assert(Pointerp(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

    /* get tag of object */
    tag = LowtagOf(object);

    /* allocate space */
    new = gc_quick_alloc(nwords * sizeof(lispobj));

    dest = new;
    source = (lispobj *) PTR(object);

    /* copy the object */
    while (nwords > 0) {
	dest[0] = source[0];
	dest[1] = source[1];
	dest += 2;
	source += 2;
	nwords -= 2;
    }

    /* return lisp pointer of new object */
    return (lispobj) new | tag;
}

/*
 * Copying Large Boxed Objects. If the object is in a large object
 * region then it is simply promoted, else it is copied. If it's large
 * enough then it's copied to a large object region.
 *
 * Vectors may have shrunk. If the object is not copied the space
 * needs to be reclaimed, and the page_tables corrected.
 */
static lispobj
copy_large_object(lispobj object, int nwords)
{
    int tag;
    lispobj *new;
    lispobj *source, *dest;
    int first_page;

    gc_assert(Pointerp(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

    if (gencgc_verbose && nwords > 1024 * 1024)
	fprintf(stderr, "** copy_large_object: %d\n", nwords * sizeof(lispobj));

    /* Check if it's a large object. */
    first_page = find_page_index((void *) object);
    gc_assert(first_page >= 0);

    if (PAGE_LARGE_OBJECT(first_page)) {
	/* Promote the object. */
	int remaining_bytes;
	int next_page;
	int bytes_freed;
	int old_bytes_used;
	int mmask, mflags;

	/*
	 * Note: Any page write protection must be removed, else a later
	 * scavenge_newspace may incorrectly not scavenge these pages.
	 * This would not be necessary if they are added to the new areas,
	 * but lets do it for them all (they'll probably be written
	 * anyway?).
	 */

	gc_assert(page_table[first_page].first_object_offset == 0);

	next_page = first_page;
	remaining_bytes = nwords * sizeof(lispobj);
	while (remaining_bytes > PAGE_SIZE) {
	    gc_assert(PAGE_GENERATION(next_page) == from_space);
	    gc_assert(PAGE_ALLOCATED(next_page));
	    gc_assert(!PAGE_UNBOXED(next_page));
	    gc_assert(PAGE_LARGE_OBJECT(next_page));
	    gc_assert(page_table[next_page].first_object_offset ==
		      PAGE_SIZE * (first_page - next_page));
	    gc_assert(page_table[next_page].bytes_used == PAGE_SIZE);

	    PAGE_FLAGS_UPDATE(next_page, PAGE_GENERATION_MASK, new_space);

	    /*
	     * Remove any write protection.  Should be able to religh on the
	     * WP flag to avoid redundant calls.
	     */
	    if (PAGE_WRITE_PROTECTED(next_page)) {
		os_protect((os_vm_address_t) page_address(next_page), PAGE_SIZE,
			   OS_VM_PROT_ALL);
		page_table[next_page].flags &= ~PAGE_WRITE_PROTECTED_MASK;
	    }
	    remaining_bytes -= PAGE_SIZE;
	    next_page++;
	}

	/*
	 * Now only one page remains, but the object may have shrunk so
	 * there may be more unused pages which will be freed.
	 */

	/* Object may have shrunk but shouldn't have grown - check. */
	gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

	PAGE_FLAGS_UPDATE(next_page, PAGE_GENERATION_MASK, new_space);
	gc_assert(PAGE_ALLOCATED(next_page));
	gc_assert(!PAGE_UNBOXED(next_page));

	/* Adjust the bytes_used. */
	old_bytes_used = page_table[next_page].bytes_used;
	page_table[next_page].bytes_used = remaining_bytes;

	bytes_freed = old_bytes_used - remaining_bytes;

	mmask = PAGE_ALLOCATED_MASK | PAGE_UNBOXED_MASK | PAGE_LARGE_OBJECT_MASK
	    | PAGE_GENERATION_MASK;
	mflags = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK | from_space;

	/* Free any remaining pages; needs care. */
	next_page++;
	while (old_bytes_used == PAGE_SIZE &&
	       PAGE_FLAGS(next_page, mmask) == mflags &&
	       page_table[next_page].first_object_offset ==
	       PAGE_SIZE * (first_page - next_page)) {
	    /*
	     * Checks out OK, free the page. Don't need to both zeroing
	     * pages as this should have been done before shrinking the
	     * object. These pages shouldn't be write protected as they
	     * should be zero filled.
	     */
	    gc_assert(!PAGE_WRITE_PROTECTED(next_page));

	    old_bytes_used = page_table[next_page].bytes_used;
	    page_table[next_page].flags &= ~PAGE_ALLOCATED_MASK;
	    page_table[next_page].bytes_used = 0;
	    bytes_freed += old_bytes_used;
	    next_page++;
	}

	if (gencgc_verbose && bytes_freed > 0)
	    fprintf(stderr, "* copy_large_boxed bytes_freed %d\n", bytes_freed);

	generations[from_space].bytes_allocated -=
	    sizeof(lispobj) * nwords + bytes_freed;
	generations[new_space].bytes_allocated += sizeof(lispobj) * nwords;
	bytes_allocated -= bytes_freed;

	/* Add the region to the new_areas if requested. */
	add_new_area(first_page, 0, nwords * sizeof(lispobj));

	return object;
    } else {
	/* get tag of object */
	tag = LowtagOf(object);

	/* allocate space */
	new = gc_quick_alloc_large(nwords * sizeof(lispobj));

	dest = new;
	source = (lispobj *) PTR(object);

	/* copy the object */
	while (nwords > 0) {
	    dest[0] = source[0];
	    dest[1] = source[1];
	    dest += 2;
	    source += 2;
	    nwords -= 2;
	}

	/* return lisp pointer of new object */
	return (lispobj) new | tag;
    }
}

/* Copying UnBoxed Objects. */
static inline lispobj
copy_unboxed_object(lispobj object, int nwords)
{
    int tag;
    lispobj *new;
    lispobj *source, *dest;

    gc_assert(Pointerp(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

    /* get tag of object */
    tag = LowtagOf(object);

    /* allocate space */
    new = gc_quick_alloc_unboxed(nwords * sizeof(lispobj));

    dest = new;
    source = (lispobj *) PTR(object);

    /* Copy the object */
    while (nwords > 0) {
	dest[0] = source[0];
	dest[1] = source[1];
	dest += 2;
	source += 2;
	nwords -= 2;
    }

    /* Return lisp pointer of new object. */
    return (lispobj) new | tag;
}


/*
 * Copying Large Unboxed Objects. If the object is in a large object
 * region then it is simply promoted, else it is copied. If it's large
 * enough then it's copied to a large object region.
 *
 * Bignums and vectors may have shrunk. If the object is not copied
 * the space needs to be reclaimed, and the page_tables corrected.
 */
static lispobj
copy_large_unboxed_object(lispobj object, int nwords)
{
    int tag;
    lispobj *new;
    lispobj *source, *dest;
    int first_page;

    gc_assert(Pointerp(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

    if (gencgc_verbose && nwords > 1024 * 1024)
	fprintf(stderr, "** copy_large_unboxed_object: %d\n",
		nwords * sizeof(lispobj));

    /* Check if it's a large object. */
    first_page = find_page_index((void *) object);
    gc_assert(first_page >= 0);

    if (PAGE_LARGE_OBJECT(first_page)) {
	/*
	 * Promote the object. Note: Unboxed objects may have been
	 * allocated to a BOXED region so it may be necessary to change
	 * the region to UNBOXED.
	 */
	int remaining_bytes;
	int next_page;
	int bytes_freed;
	int old_bytes_used;
	int mmask, mflags;

	gc_assert(page_table[first_page].first_object_offset == 0);

	next_page = first_page;
	remaining_bytes = nwords * sizeof(lispobj);
	while (remaining_bytes > PAGE_SIZE) {
	    gc_assert(PAGE_GENERATION(next_page) == from_space);
	    gc_assert(PAGE_ALLOCATED(next_page));
	    gc_assert(PAGE_LARGE_OBJECT(next_page));
	    gc_assert(page_table[next_page].first_object_offset ==
		      PAGE_SIZE * (first_page - next_page));
	    gc_assert(page_table[next_page].bytes_used == PAGE_SIZE);

	    PAGE_FLAGS_UPDATE(next_page,
			      PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK,
			      PAGE_UNBOXED_MASK | new_space);
	    remaining_bytes -= PAGE_SIZE;
	    next_page++;
	}

	/*
	 * Now only one page remains, but the object may have shrunk so
	 * there may be more unused pages which will be freed.
	 */

	/* Object may have shrunk but shouldn't have grown - check. */
	gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

	PAGE_FLAGS_UPDATE(next_page, PAGE_ALLOCATED_MASK | PAGE_UNBOXED_MASK
			  | PAGE_GENERATION_MASK,
			  PAGE_ALLOCATED_MASK | PAGE_UNBOXED_MASK | new_space);

	/* Adjust the bytes_used. */
	old_bytes_used = page_table[next_page].bytes_used;
	page_table[next_page].bytes_used = remaining_bytes;

	bytes_freed = old_bytes_used - remaining_bytes;

	mmask = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK
	    | PAGE_GENERATION_MASK;
	mflags = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK | from_space;

	/* Free any remaining pages; needs care. */
	next_page++;
	while (old_bytes_used == PAGE_SIZE &&
	       PAGE_FLAGS(next_page, mmask) == mflags &&
	       page_table[next_page].first_object_offset ==
	       PAGE_SIZE * (first_page - next_page)) {
	    /*
	     * Checks out OK, free the page. Don't need to both zeroing
	     * pages as this should have been done before shrinking the
	     * object. These pages shouldn't be write protected, even if
	     * boxed they should be zero filled.
	     */
	    gc_assert(!PAGE_WRITE_PROTECTED(next_page));

	    old_bytes_used = page_table[next_page].bytes_used;
	    page_table[next_page].flags &= ~PAGE_ALLOCATED_MASK;
	    page_table[next_page].bytes_used = 0;
	    bytes_freed += old_bytes_used;
	    next_page++;
	}

	if (gencgc_verbose && bytes_freed > 0)
	    fprintf(stderr, "* copy_large_unboxed bytes_freed %d\n",
		    bytes_freed);

	generations[from_space].bytes_allocated -=
	    sizeof(lispobj) * nwords + bytes_freed;
	generations[new_space].bytes_allocated += sizeof(lispobj) * nwords;
	bytes_allocated -= bytes_freed;

	return object;
    } else {
	/* get tag of object */
	tag = LowtagOf(object);

	/* allocate space */
	new = gc_quick_alloc_large_unboxed(nwords * sizeof(lispobj));

	dest = new;
	source = (lispobj *) PTR(object);

	/* copy the object */
	while (nwords > 0) {
	    dest[0] = source[0];
	    dest[1] = source[1];
	    dest += 2;
	    source += 2;
	    nwords -= 2;
	}

	/* return lisp pointer of new object */
	return (lispobj) new | tag;
    }
}


/* Scavenging */

/*
 * Douglas Crosher says:
 *
 * There were two different ways in which the scavenger dispatched,
 * and DIRECT_SCAV was one option.  This code did work at one stage
 * but testing showed it to be slower.  When DIRECT_SCAV is enabled
 * the scavenger dispatches via the scavtab for all objects, and when
 * disabled the scavenger firstly detects and handles some common
 * cases itself before dispatching.
 */

#define DIRECT_SCAV 0

static void
scavenge(void *start_obj, long nwords)
{
    lispobj *start;

    start = (lispobj *) start_obj;

    while (nwords > 0) {
	lispobj object;
	int words_scavenged;

	object = *start;
	/* Not a forwarding pointer. */
	gc_assert(object != 0x01);

#if DIRECT_SCAV
	words_scavenged = scavtab[TypeOf(object)] (start, object);
#else /* not DIRECT_SCAV */
	if (Pointerp(object)) {
#ifdef GC_ASSERTIONS
	    check_escaped_stack_object(start, object);
#endif

	    if (from_space_p(object)) {
		lispobj *ptr = (lispobj *) PTR(object);
		lispobj first_word = *ptr;

		if (first_word == 0x01) {
		    *start = ptr[1];
		    words_scavenged = 1;
		} else
		    words_scavenged = scavtab[TypeOf(object)] (start, object);
	    } else
		words_scavenged = 1;
	} else if ((object & 3) == 0)
	    words_scavenged = 1;
	else
	    words_scavenged = scavtab[TypeOf(object)] (start, object);
#endif /* not DIRECT_SCAV */

	start += words_scavenged;
	nwords -= words_scavenged;
    }

    gc_assert(nwords == 0);
}


#if !(defined(i386) || defined(__x86_64))
/* Scavenging Interrupt Contexts */

static int boxed_registers[] = BOXED_REGISTERS;

static void
scavenge_interrupt_context(os_context_t * context)
{
    int i;
    unsigned long pc_code_offset;

#ifdef reg_LIP
    unsigned long lip;
    unsigned long lip_offset;
    int lip_register_pair;
#endif
#ifdef reg_LR
    unsigned long lr_code_offset;
#endif
#ifdef reg_CTR    
    unsigned long ctr_code_offset;
#endif
#ifdef SC_NPC
    unsigned long npc_code_offset;
#endif

#ifdef reg_LIP
    /* Find the LIP's register pair and calculate it's offset */
    /* before we scavenge the context. */

    /*
     * I (RLT) think this is trying to find the boxed register that is
     * closest to the LIP address, without going past it.  Usually, it's
     * reg_CODE or reg_LRA.  But sometimes, nothing can be found.
     */
    lip = SC_REG(context, reg_LIP);
    lip_offset = 0x7FFFFFFF;
    lip_register_pair = -1;
    for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
	unsigned long reg;
	long offset;
	int index;

	index = boxed_registers[i];
	reg = SC_REG(context, index);
	if (Pointerp(reg) && PTR(reg) <= lip) {
	    offset = lip - reg;
	    if (offset < lip_offset) {
		lip_offset = offset;
		lip_register_pair = index;
	    }
	}
    }
#endif /* reg_LIP */

    /*
     * Compute the PC's offset from the start of the CODE 
     * register.
     */
    pc_code_offset = SC_PC(context) - SC_REG(context, reg_CODE);
#ifdef SC_NPC
    npc_code_offset = SC_NPC(context) - SC_REG(context, reg_CODE);
#endif /* SC_NPC */

#ifdef reg_LR
    lr_code_offset = SC_REG(context, reg_LR) - SC_REG(context, reg_CODE);
#endif    
#ifdef reg_CTR
    ctr_code_offset = SC_REG(context, reg_CTR) - SC_REG(context, reg_CODE);
#endif    

    /* Scanvenge all boxed registers in the context. */
    for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
	int index;
	lispobj foo;

	index = boxed_registers[i];
	foo = SC_REG(context, index);
	scavenge(&foo, 1);
	SC_REG(context, index) = foo;

	scavenge(&(SC_REG(context, index)), 1);
    }

#ifdef reg_LIP
    /* Fix the LIP */

    /*
     * But what happens if lip_register_pair is -1?  SC_REG on Solaris
     * (see solaris_register_address in solaris-os.c) will return
     * &context->uc_mcontext.gregs[2].  But gregs[2] is REG_nPC.  Is
     * that what we really want?  My guess is that that is not what we
     * want, so if lip_register_pair is -1, we don't touch reg_LIP at
     * all.  But maybe it doesn't really matter if LIP is trashed?
     */
    if (lip_register_pair >= 0) {
	SC_REG(context, reg_LIP) =
	    SC_REG(context, lip_register_pair) + lip_offset;
    }
#endif /* reg_LIP */

    /* Fix the PC if it was in from space */
    if (from_space_p(SC_PC(context))) {
        SC_PC(context) = SC_REG(context, reg_CODE) + pc_code_offset;
    }
#ifdef SC_NPC
    if (from_space_p(SC_NPC(context))) {
	SC_NPC(context) = SC_REG(context, reg_CODE) + npc_code_offset;
    }
#endif /* SC_NPC */

#ifdef reg_LR
    if (from_space_p(SC_REG(context, reg_LR))) {
        SC_REG(context, reg_LR) = SC_REG(context, reg_CODE) + lr_code_offset;
    }
#endif	
#ifdef reg_CTR
    if (from_space_p(SC_REG(context, reg_CTR))) {
      SC_REG(context, reg_CTR) = SC_REG(context, reg_CODE) + ctr_code_offset;
    }
#endif	
}

void
scavenge_interrupt_contexts(void)
{
    int i, index;
    os_context_t *context;

#ifdef PRINTNOISE
    printf("Scavenging interrupt contexts ...\n");
#endif

    index = fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX));

#if defined(DEBUG_PRINT_CONTEXT_INDEX)
    printf("Number of active contexts: %d\n", index);
#endif

    for (i = 0; i < index; i++) {
	context = lisp_interrupt_contexts[i];
	scavenge_interrupt_context(context);
    }
}
#endif

/* Code and Code-Related Objects */

/*
 * Aargh!  Why is SPARC so different here?  What is the advantage of
 * making it different from all the other ports?
 */
#if defined(sparc) || defined(DARWIN)
#define RAW_ADDR_OFFSET 0
#else
#define RAW_ADDR_OFFSET (6 * sizeof(lispobj) - type_FunctionPointer)
#endif

static lispobj trans_function_header(lispobj object);
static lispobj trans_boxed(lispobj object);

#if DIRECT_SCAV
static int
scav_function_pointer(lispobj * where, lispobj object)
{
    gc_assert(Pointerp(object));

    if (from_space_p(object)) {
	lispobj first, *first_pointer;

	/*
	 * Object is a pointer into from space - check to see if it has
	 * been forwarded.
	 */
	first_pointer = (lispobj *) PTR(object);
	first = *first_pointer;

	if (first == 0x01) {
	    /* Forwarded */
	    *where = first_pointer[1];
	    return 1;
	} else {
	    int type;
	    lispobj copy;

	    /*
	     * Must transport object -- object may point to either a
	     * function header, a closure function header, or to a closure
	     * header.
	     */

	    type = TypeOf(first);
	    switch (type) {
	      case type_FunctionHeader:
	      case type_ClosureFunctionHeader:
		  copy = trans_function_header(object);
		  break;
	      default:
		  copy = trans_boxed(object);
		  break;
	    }

	    if (copy != object) {
		/* Set forwarding pointer. */
		first_pointer[0] = 0x01;
		first_pointer[1] = copy;
	    }

	    first = copy;
	}

	gc_assert(Pointerp(first));
	gc_assert(!from_space_p(first));

	*where = first;
    }
    return 1;
}
#else
static int
scav_function_pointer(lispobj * where, lispobj object)
{
    lispobj *first_pointer;
    lispobj copy;

    gc_assert(Pointerp(object));

    /* Object is a pointer into from space - no a FP. */
    first_pointer = (lispobj *) PTR(object);

    /*
     * Must transport object -- object may point to either a function
     * header, a closure function header, or to a closure header.
     */

    switch (TypeOf(*first_pointer)) {
      case type_FunctionHeader:
      case type_ClosureFunctionHeader:
	  copy = trans_function_header(object);
	  break;
      default:
	  copy = trans_boxed(object);
	  break;
    }

    if (copy != object) {
	/* Set forwarding pointer */
	first_pointer[0] = 0x01;
	first_pointer[1] = copy;
    }

    gc_assert(Pointerp(copy));
    gc_assert(!from_space_p(copy));

    *where = copy;

    return 1;
}
#endif

#if defined(i386) || defined(__x86_64)
/*
 * Scan an x86 compiled code object, looking for possible fixups that
 * have been missed after a move.
 *
 * Two types of fixups are needed:
 *	1. Absolution fixups to within the code object.
 *	2. Relative fixups to outside the code object.
 *
 * Currently only absolution fixups to the constant vector, or to the
 * code area are checked.
 */
void
sniff_code_object(struct code *code, unsigned displacement)
{
    int nheader_words, ncode_words, nwords;
    void *p;
    void *constants_start_addr, *constants_end_addr;
    void *code_start_addr, *code_end_addr;
    int fixup_found = 0;

    if (!check_code_fixups)
	return;

    /*
     * It's ok if it's byte compiled code. The trace table offset will
     * be a fixnum if it's x86 compiled code - check.
     */
    if (code->trace_table_offset & 0x3) {
#if 0
	fprintf(stderr, "*** Sniffing byte compiled code object at %x.\n",
		code);
#endif
	return;
    }

    /* Else it's x86 machine code. */

    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(*(lispobj *) code);
    nwords = ncode_words + nheader_words;

    constants_start_addr = (void *) code + 5 * sizeof(lispobj);
    constants_end_addr = (void *) code + nheader_words * sizeof(lispobj);
    code_start_addr = (void *) code + nheader_words * sizeof(lispobj);
    code_end_addr = (void *) code + nwords * sizeof(lispobj);

    /* Work through the unboxed code. */
    for (p = code_start_addr; p < code_end_addr; p++) {
	void *data = *(void **) p;
	unsigned d1 = *((unsigned char *) p - 1);
	unsigned d2 = *((unsigned char *) p - 2);
	unsigned d3 = *((unsigned char *) p - 3);
	unsigned d4 = *((unsigned char *) p - 4);
	unsigned d5 = *((unsigned char *) p - 5);
	unsigned d6 = *((unsigned char *) p - 6);

	/*
	 * Check for code references.
	 *
	 * Check for a 32 bit word that looks like an absolute reference
	 * to within the code adea of the code object.
	 */
	if (data >= code_start_addr - displacement
	    && data < code_end_addr - displacement) {
	    /* Function header */
	    if (d4 == 0x5e
		&& ((unsigned long) p - 4 -
		    4 * HeaderValue(*((unsigned long *) p - 1))) ==
		(unsigned long) code) {
		/* Skip the function header */
		p += 6 * 4 - 4 - 1;
		continue;
	    }
	    /* Push imm32 */
	    if (d1 == 0x68) {
		fixup_found = 1;
		fprintf(stderr,
			"Code ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			(unsigned long) p, d6, d5, d4, d3, d2, d1,
			(unsigned long) data);
		fprintf(stderr, "***  Push $0x%.8lx\n", (unsigned long) data);
	    }
	    /* Mov [reg-8],imm32 */
	    if (d3 == 0xc7
		&& (d2 == 0x40 || d2 == 0x41 || d2 == 0x42 || d2 == 0x43
		    || d2 == 0x45 || d2 == 0x46 || d2 == 0x47)
		&& d1 == 0xf8) {
		fixup_found = 1;
		fprintf(stderr,
			"Code ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			(unsigned long) p, d6, d5, d4, d3, d2, d1,
			(unsigned long) data);
		fprintf(stderr, "***  Mov [reg-8],$0x%.8lx\n",
			(unsigned long) data);
	    }
	    /* Lea reg, [disp32] */
	    if (d2 == 0x8d && (d1 & 0xc7) == 5) {
		fixup_found = 1;
		fprintf(stderr,
			"Code ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			(unsigned long) p, d6, d5, d4, d3, d2, d1,
			(unsigned long) data);
		fprintf(stderr, "***  Lea reg,[$0x%.8lx]\n",
			(unsigned long) data);
	    }
	}

	/*
	 * Check for constant references.
	 *
	 * Check for a 32 bit word that looks like an absolution reference
	 * to within the constant vector. Constant references will be
	 * aligned.
	 */
	if (data >= constants_start_addr - displacement
	    && data < constants_end_addr - displacement
	    && ((unsigned long) data & 0x3) == 0) {
	    /*  Mov eax,m32 */
	    if (d1 == 0xa1) {
		fixup_found = 1;
		fprintf(stderr,
			"Abs. const. ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			(unsigned long) p, d6, d5, d4, d3, d2, d1,
			(unsigned long) data);
		fprintf(stderr, "***  Mov eax,0x%.8lx\n", (unsigned long) data);
	    }

	    /*  Mov m32,eax */
	    if (d1 == 0xa3) {
		fixup_found = 1;
		fprintf(stderr,
			"Abs. const. ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			(unsigned long) p, d6, d5, d4, d3, d2, d1,
			(unsigned long) data);
		fprintf(stderr, "***  Mov 0x%.8lx,eax\n", (unsigned long) data);
	    }

	    /* Cmp m32,imm32 */
	    if (d1 == 0x3d && d2 == 0x81) {
		fixup_found = 1;
		fprintf(stderr,
			"Abs. const. ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			(unsigned long) p, d6, d5, d4, d3, d2, d1,
			(unsigned long) data);
		/* XX Check this */
		fprintf(stderr, "***  Cmp 0x%.8lx,immed32\n",
			(unsigned long) data);
	    }

	    /* Check for a mod=00, r/m=101 byte. */
	    if ((d1 & 0xc7) == 5) {
		/* Cmp m32,reg */
		if (d2 == 0x39) {
		    fixup_found = 1;
		    fprintf(stderr,
			    "Abs. const. ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			    (unsigned long) p, d6, d5, d4, d3, d2, d1,
			    (unsigned long) data);
		    fprintf(stderr, "***  Cmp 0x%.8lx,reg\n",
			    (unsigned long) data);
		}
		/* Cmp reg32,m32 */
		if (d2 == 0x3b) {
		    fixup_found = 1;
		    fprintf(stderr,
			    "Abs. const. ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			    (unsigned long) p, d6, d5, d4, d3, d2, d1,
			    (unsigned long) data);
		    fprintf(stderr, "***  Cmp reg32,0x%.8lx\n",
			    (unsigned long) data);
		}
		/* Mov m32,reg32 */
		if (d2 == 0x89) {
		    fixup_found = 1;
		    fprintf(stderr,
			    "Abs. const. ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			    (unsigned long) p, d6, d5, d4, d3, d2, d1,
			    (unsigned long) data);
		    fprintf(stderr, "***  Mov 0x%.8lx,reg32\n",
			    (unsigned long) data);
		}
		/* Mov reg32,m32 */
		if (d2 == 0x8b) {
		    fixup_found = 1;
		    fprintf(stderr,
			    "Abs. const. ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			    (unsigned long) p, d6, d5, d4, d3, d2, d1,
			    (unsigned long) data);
		    fprintf(stderr, "***  Mov reg32,0x%.8lx\n",
			    (unsigned long) data);
		}
		/* Lea reg32,m32 */
		if (d2 == 0x8d) {
		    fixup_found = 1;
		    fprintf(stderr,
			    "Abs. const. ref. @ %lx: %.2x %.2x %.2x %.2x %.2x %.2x (%.8lx)\n",
			    (unsigned long) p, d6, d5, d4, d3, d2, d1,
			    (unsigned long) data);
		    fprintf(stderr, "***  Lea reg32,0x%.8lx\n",
			    (unsigned long) data);
		}
	    }
	}
    }

    /* If anything was found print out some info. on the code object. */
    if (fixup_found) {
	fprintf(stderr,
		"*** Compiled code object at %lx: header_words=%d code_words=%d .\n",
		(unsigned long) code, nheader_words, ncode_words);
	fprintf(stderr,
		"*** Const. start = %lx; end= %lx; Code start = %lx; end = %lx\n",
		(unsigned long) constants_start_addr,
		(unsigned long) constants_end_addr,
		(unsigned long) code_start_addr, (unsigned long) code_end_addr);
    }
}

static void
apply_code_fixups(struct code *old_code, struct code *new_code)
{
    int nheader_words, ncode_words, nwords;
    void *constants_start_addr, *constants_end_addr;
    void *code_start_addr, *code_end_addr;
    lispobj fixups = NIL;
    unsigned long displacement =

	(unsigned long) new_code - (unsigned long) old_code;
    struct vector *fixups_vector;

    /*
     * It's ok if it's byte compiled code. The trace table offset will
     * be a fixnum if it's x86 compiled code - check.
     */
    if (new_code->trace_table_offset & 0x3) {
#if 0
	fprintf(stderr, "*** Byte compiled code object at %x.\n", new_code);
#endif
	return;
    }

    /* Else it's x86 machine code. */
    ncode_words = fixnum_value(new_code->code_size);
    nheader_words = HeaderValue(*(lispobj *) new_code);
    nwords = ncode_words + nheader_words;
#if 0
    fprintf(stderr,
	    "*** Compiled code object at %x: header_words=%d code_words=%d .\n",
	    new_code, nheader_words, ncode_words);
#endif
    constants_start_addr = (void *) new_code + 5 * sizeof(lispobj);
    constants_end_addr = (void *) new_code + nheader_words * sizeof(lispobj);
    code_start_addr = (void *) new_code + nheader_words * sizeof(lispobj);
    code_end_addr = (void *) new_code + nwords * sizeof(lispobj);
#if 0
    fprintf(stderr,
	    "*** Const. start = %x; end= %x; Code start = %x; end = %x\n",
	    constants_start_addr, constants_end_addr, code_start_addr,
	    code_end_addr);
#endif

    /*
     * The first constant should be a pointer to the fixups for this
     * code objects - Check.
     */
    fixups = new_code->constants[0];

    /*
     * It will be 0 or the unbound-marker if there are no fixups, and
     * will be an other pointer if it is valid.
     */
    if (fixups == 0 || fixups == type_UnboundMarker || !Pointerp(fixups)) {
	/* Check for possible errors. */
	if (check_code_fixups)
	    sniff_code_object(new_code, displacement);

#if 0
	fprintf(stderr, "Fixups for code object not found!?\n");
	fprintf(stderr,
		"*** Compiled code object at %x: header_words=%d code_words=%d .\n",
		new_code, nheader_words, ncode_words);
	fprintf(stderr,
		"*** Const. start = %x; end= %x; Code start = %x; end = %x\n",
		constants_start_addr, constants_end_addr, code_start_addr,
		code_end_addr);
#endif
	return;
    }

    fixups_vector = (struct vector *) PTR(fixups);

    /* Could be pointing to a forwarding pointer. */
    if (Pointerp(fixups) && find_page_index((void *) fixups_vector) != -1
	&& fixups_vector->header == 0x01) {
#if 0
	fprintf(stderr, "* FF\n");
#endif
	/* If so then follow it. */
	fixups_vector = (struct vector *) PTR((lispobj) fixups_vector->length);
    }
#if 0
    fprintf(stderr, "Got the fixups\n");
#endif

    if (TypeOf(fixups_vector->header) == type_SimpleArrayUnsignedByte32) {
	/*
	 * Got the fixups for the code block.  Now work through the
	 * vector, and apply a fixup at each address.
	 */
	int length = fixnum_value(fixups_vector->length);
	int i;

	for (i = 0; i < length; i++) {
	    unsigned offset = fixups_vector->data[i];

	    /* Now check the current value of offset. */
	    unsigned long old_value =
		*(unsigned long *) ((unsigned long) code_start_addr + offset);

	    /*
	     * If it's within the old_code object then it must be an
	     * absolute fixup (relative ones are not saved).
	     */
	    if (old_value >= (unsigned long) old_code
		&& old_value <
		(unsigned long) old_code + nwords * sizeof(lispobj))
		/* So add the dispacement. */
		*(unsigned long *) ((unsigned long) code_start_addr + offset) =
		    old_value + displacement;
	    else
		/*
		 * It is outside the old code object so it must be a relative
		 * fixup (absolute fixups are not saved). So subtract the
		 * displacement.
		 */
		*(unsigned long *) ((unsigned long) code_start_addr + offset) =
		    old_value - displacement;
	}
    }

    /* Check for possible errors. */
    if (check_code_fixups)
	sniff_code_object(new_code, displacement);
}
#endif

static struct code *
trans_code(struct code *code)
{
    struct code *new_code;
    lispobj l_code, l_new_code;
    int nheader_words, ncode_words, nwords;
    unsigned long displacement;
    lispobj fheaderl, *prev_pointer;

#if 0
    fprintf(stderr, "\nTransporting code object located at 0x%08x.\n",
	    (unsigned long) code);
#endif

    /* If object has already been transported, just return pointer */
    if (*(lispobj *) code == 0x01) {
	return (struct code *) (((lispobj *) code)[1]);
    }


    gc_assert(TypeOf(code->header) == type_CodeHeader);

    /* prepare to transport the code vector */
    l_code = (lispobj) code | type_OtherPointer;

    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(code->header);
    nwords = ncode_words + nheader_words;
    nwords = CEILING(nwords, 2);

    l_new_code = copy_large_object(l_code, nwords);
    new_code = (struct code *) PTR(l_new_code);

    /* May not have been moved. */
    if (new_code == code)
	return new_code;

    displacement = l_new_code - l_code;

#if 0
    fprintf(stderr, "Old code object at 0x%08x, new code object at 0x%08x.\n",
	    (unsigned long) code, (unsigned long) new_code);
    fprintf(stderr, "Code object is %d words long.\n", nwords);
#endif

    /* set forwarding pointer */
    ((lispobj *) code)[0] = 0x01;
    ((lispobj *) code)[1] = l_new_code;

    /*
     * Set forwarding pointers for all the function headers in the code
     * object; also fix all self pointers.
     */

    fheaderl = code->entry_points;
    prev_pointer = &new_code->entry_points;

    while (fheaderl != NIL) {
	struct function *fheaderp, *nfheaderp;
	lispobj nfheaderl;

	fheaderp = (struct function *) PTR(fheaderl);
	gc_assert(TypeOf(fheaderp->header) == type_FunctionHeader);

	/*
	 * Calcuate the new function pointer and the new function header.
	 */
	nfheaderl = fheaderl + displacement;
	nfheaderp = (struct function *) PTR(nfheaderl);

	/* set forwarding pointer */
	((lispobj *) fheaderp)[0] = 0x01;
	((lispobj *) fheaderp)[1] = nfheaderl;

	/* Fix self pointer */
	nfheaderp->self = nfheaderl + RAW_ADDR_OFFSET;

	*prev_pointer = nfheaderl;

	fheaderl = fheaderp->next;
	prev_pointer = &nfheaderp->next;
    }

#if 0
    sniff_code_object(new_code, displacement);
#endif
#if defined(i386) || defined(__x86_64)
    apply_code_fixups(code, new_code);
#else
    /* From gc.c */
#ifndef MACH
    os_flush_icache((os_vm_address_t) (((int *) new_code) + nheader_words),
		    ncode_words * sizeof(int));
#endif
#endif

    return new_code;
}

static int
scav_code_header(lispobj * where, lispobj object)
{
    struct code *code;
    int nheader_words, ncode_words, nwords;
    lispobj fheaderl;
    struct function *fheaderp;

    code = (struct code *) where;
    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(object);
    nwords = ncode_words + nheader_words;
    nwords = CEILING(nwords, 2);

    /* Scavenge the boxed section of the code data block */
    scavenge(where + 1, nheader_words - 1);

    /*
     * Scavenge the boxed section of each function object in the code
     * data block
     */
    fheaderl = code->entry_points;
    while (fheaderl != NIL) {
	fheaderp = (struct function *) PTR(fheaderl);
	gc_assert(TypeOf(fheaderp->header) == type_FunctionHeader);

	scavenge(&fheaderp->name, 1);
	scavenge(&fheaderp->arglist, 1);
	scavenge(&fheaderp->type, 1);

	fheaderl = fheaderp->next;
    }

    return nwords;
}

static lispobj
trans_code_header(lispobj object)
{
    struct code *ncode;

    ncode = trans_code((struct code *) PTR(object));
    return (lispobj) ncode | type_OtherPointer;
}

static int
size_code_header(lispobj * where)
{
    struct code *code;
    int nheader_words, ncode_words, nwords;

    code = (struct code *) where;

    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(code->header);
    nwords = ncode_words + nheader_words;
    nwords = CEILING(nwords, 2);

    return nwords;
}

#if !(defined(i386) || defined(__x86_64))

static int
scav_return_pc_header(lispobj * where, lispobj object)
{
    fprintf(stderr, "GC lossage.  Should not be scavenging a ");
    fprintf(stderr, "Return PC Header.\n");
    fprintf(stderr, "where = 0x%08lx, object = 0x%08lx",
	    (unsigned long) where, (unsigned long) object);
    lose(NULL);
    return 0;
}

#endif /* not i386 */

static lispobj
trans_return_pc_header(lispobj object)
{
    struct function *return_pc;
    unsigned long offset;
    struct code *code, *ncode;

    return_pc = (struct function *) PTR(object);
    offset = HeaderValue(return_pc->header) * sizeof(lispobj);

    /* Transport the whole code object */
    code = (struct code *) ((unsigned long) return_pc - offset);

    ncode = trans_code(code);

    return ((lispobj) ncode + offset) | type_OtherPointer;
}

/*
 * On the 386, closures hold a pointer to the raw address instead of
 * the function object.
 */
#if defined(i386) || defined(__x86_64)

static int
scav_closure_header(lispobj * where, lispobj object)
{
    struct closure *closure;
    lispobj fun;

    closure = (struct closure *) where;
    fun = closure->function - RAW_ADDR_OFFSET;
    scavenge(&fun, 1);
    /* The function may have moved so update the raw address. But don't
       write unnecessarily. */
    if (closure->function != fun + RAW_ADDR_OFFSET)
	closure->function = fun + RAW_ADDR_OFFSET;

    return 2;
}

#endif /* i386 */

#if !(defined(i386) || defined(__x86_64))

static int
scav_function_header(lispobj * where, lispobj object)
{
    fprintf(stderr, "GC lossage.  Should not be scavenging a ");
    fprintf(stderr, "Function Header.\n");
    fprintf(stderr, "where = 0x%08lx, object = 0x%08lx",
	    (unsigned long) where, (unsigned long) object);
    lose(NULL);
    return 0;
}

#endif /* not i386 */

static lispobj
trans_function_header(lispobj object)
{
    struct function *fheader;
    unsigned long offset;
    struct code *code, *ncode;

    fheader = (struct function *) PTR(object);
    offset = HeaderValue(fheader->header) * sizeof(lispobj);

    /* Transport the whole code object */
    code = (struct code *) ((unsigned long) fheader - offset);
    ncode = trans_code(code);

    return ((lispobj) ncode + offset) | type_FunctionPointer;
}


/* Instances */

#if DIRECT_SCAV
static int
scav_instance_pointer(lispobj * where, lispobj object)
{
    if (from_space_p(object)) {
	lispobj first, *first_pointer;

	/*
	 * object is a pointer into from space.  check to see if it has
	 * been forwarded
	 */
	first_pointer = (lispobj *) PTR(object);
	first = *first_pointer;

	if (first == 0x01)
	    /* Forwarded. */
	    first = first_pointer[1];
	else {
	    first = trans_boxed(object);
	    gc_assert(first != object);
	    /* Set forwarding pointer */
	    first_pointer[0] = 0x01;
	    first_pointer[1] = first;
	}
	*where = first;
    }
    return 1;
}
#else
static int
scav_instance_pointer(lispobj * where, lispobj object)
{
    lispobj copy, *first_pointer;

    /* Object is a pointer into from space - not a FP */
    copy = trans_boxed(object);

    gc_assert(copy != object);

    first_pointer = (lispobj *) PTR(object);

    /* Set forwarding pointer. */
    first_pointer[0] = 0x01;
    first_pointer[1] = copy;
    *where = copy;

    return 1;
}
#endif


/* Lists and Conses */

static lispobj trans_list(lispobj object);

#if DIRECT_SCAV
static int
scav_list_pointer(lispobj * where, lispobj object)
{
    gc_assert(Pointerp(object));

    if (from_space_p(object)) {
	lispobj first, *first_pointer;

	/*
	 * Object is a pointer into from space - check to see if it has
	 * been forwarded.
	 */
	first_pointer = (lispobj *) PTR(object);
	first = *first_pointer;

	if (first == 0x01)
	    /* Forwarded. */
	    first = first_pointer[1];
	else {
	    first = trans_list(object);

	    /* Set forwarding pointer */
	    first_pointer[0] = 0x01;
	    first_pointer[1] = first;
	}

	gc_assert(Pointerp(first));
	gc_assert(!from_space_p(first));
	*where = first;
    }
    return 1;
}
#else
static int
scav_list_pointer(lispobj * where, lispobj object)
{
    lispobj first, *first_pointer;

    gc_assert(Pointerp(object));

    /* Object is a pointer into from space - not FP */

    first = trans_list(object);
    gc_assert(first != object);

    first_pointer = (lispobj *) PTR(object);

    /* Set forwarding pointer */
    first_pointer[0] = 0x01;
    first_pointer[1] = first;

    gc_assert(Pointerp(first));
    gc_assert(!from_space_p(first));
    *where = first;
    return 1;
}
#endif

static lispobj
trans_list(lispobj object)
{
    lispobj new_list_pointer;
    struct cons *cons, *new_cons;
    lispobj cdr;

    gc_assert(from_space_p(object));

    cons = (struct cons *) PTR(object);

    /* copy 'object' */
    new_cons = (struct cons *) gc_quick_alloc(sizeof(struct cons));

    new_cons->car = cons->car;
    new_cons->cdr = cons->cdr;	/* updated later */
    new_list_pointer = (lispobj) new_cons | LowtagOf(object);

    /* Grab the cdr before it is clobbered */
    cdr = cons->cdr;

    /* Set forwarding pointer (clobbers start of list). */
    cons->car = 0x01;
    cons->cdr = new_list_pointer;

    /* Try to linearize the list in the cdr direction to help reduce paging. */
    while (1) {
	lispobj new_cdr;
	struct cons *cdr_cons, *new_cdr_cons;

	if (LowtagOf(cdr) != type_ListPointer || !from_space_p(cdr)
	    || *((lispobj *) PTR(cdr)) == 0x01)
	    break;

	cdr_cons = (struct cons *) PTR(cdr);

	/* copy 'cdr' */
	new_cdr_cons = (struct cons *) gc_quick_alloc(sizeof(struct cons));

	new_cdr_cons->car = cdr_cons->car;
	new_cdr_cons->cdr = cdr_cons->cdr;
	new_cdr = (lispobj) new_cdr_cons | LowtagOf(cdr);

	/* Grab the cdr before it is clobbered */
	cdr = cdr_cons->cdr;

	/* Set forwarding pointer */
	cdr_cons->car = 0x01;
	cdr_cons->cdr = new_cdr;

	/*
	 * Update the cdr of the last cons copied into new space to keep
	 * the newspace scavenge from having to do it.
	 */
	new_cons->cdr = new_cdr;

	new_cons = new_cdr_cons;
    }

    return new_list_pointer;
}


/* Scavenging and Transporting Other Pointers */

#if DIRECT_SCAV
static int
scav_other_pointer(lispobj * where, lispobj object)
{
    gc_assert(Pointerp(object));

    if (from_space_p(object)) {
	lispobj first, *first_pointer;

	/*
	 * Object is a pointer into from space.  check to see if it has
	 * been forwarded.
	 */
	first_pointer = (lispobj *) PTR(object);
	first = *first_pointer;

	if (first == 0x01) {
	    /* Forwarded. */
	    first = first_pointer[1];
	    *where = first;
	} else {
	    first = (transother[TypeOf(first)]) (object);

	    if (first != object) {
		/* Set forwarding pointer */
		first_pointer[0] = 0x01;
		first_pointer[1] = first;
		*where = first;
	    }
	}

	gc_assert(Pointerp(first));
	gc_assert(!from_space_p(first));
    }
    return 1;
}
#else
static int
scav_other_pointer(lispobj * where, lispobj object)
{
    lispobj first, *first_pointer;

    gc_assert(Pointerp(object));

    /* Object is a pointer into from space - not FP */
    first_pointer = (lispobj *) PTR(object);

    first = (transother[TypeOf(*first_pointer)]) (object);

    if (first != object) {
	/* Set forwarding pointer */
	first_pointer[0] = 0x01;
	first_pointer[1] = first;
	*where = first;
    }

    gc_assert(Pointerp(first));
    gc_assert(!from_space_p(first));

    return 1;
}
#endif


/* Immediate, Boxed, and Unboxed Objects */

static int
size_pointer(lispobj * where)
{
    return 1;
}

static int
scav_immediate(lispobj * where, lispobj object)
{
    return 1;
}

static lispobj
trans_immediate(lispobj object)
{
    fprintf(stderr, "GC lossage.  Trying to transport an immediate!?\n");
    lose(NULL);
    return NIL;
}

static int
size_immediate(lispobj * where)
{
    return 1;
}


static int
scav_boxed(lispobj * where, lispobj object)
{
    return 1;
}

static lispobj
trans_boxed(lispobj object)
{
    lispobj header;
    unsigned long length;

    gc_assert(Pointerp(object));

    header = *((lispobj *) PTR(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_object(object, length);
}

static lispobj
trans_boxed_large(lispobj object)
{
    lispobj header;
    unsigned long length;

    gc_assert(Pointerp(object));

    header = *((lispobj *) PTR(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_large_object(object, length);
}

static int
size_boxed(lispobj * where)
{
    lispobj header;
    unsigned long length;

    header = *where;
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return length;
}

/* Not needed on sparc and ppc because the raw_addr has a function lowtag */
#if !(defined(sparc) || defined(DARWIN))
static int
scav_fdefn(lispobj * where, lispobj object)
{
    struct fdefn *fdefn;

    fdefn = (struct fdefn *) where;

    if ((char *) (fdefn->function + RAW_ADDR_OFFSET) == fdefn->raw_addr) {
	scavenge(where + 1, sizeof(struct fdefn) / sizeof(lispobj) - 1);

	/* Don't write unnecessarily */
	if (fdefn->raw_addr != (char *) (fdefn->function + RAW_ADDR_OFFSET))
	    fdefn->raw_addr = (char *) (fdefn->function + RAW_ADDR_OFFSET);

	return sizeof(struct fdefn) / sizeof(lispobj);
    } else
	return 1;
}
#endif

static int
scav_unboxed(lispobj * where, lispobj object)
{
    unsigned long length;

    length = HeaderValue(object) + 1;
    length = CEILING(length, 2);

    return length;
}

static lispobj
trans_unboxed(lispobj object)
{
    lispobj header;
    unsigned long length;


    gc_assert(Pointerp(object));

    header = *((lispobj *) PTR(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_unboxed_object(object, length);
}

static lispobj
trans_unboxed_large(lispobj object)
{
    lispobj header;
    unsigned long length;


    gc_assert(Pointerp(object));

    header = *((lispobj *) PTR(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_large_unboxed_object(object, length);
}

static int
size_unboxed(lispobj * where)
{
    lispobj header;
    unsigned long length;

    header = *where;
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return length;
}


/* Vector-Like Objects */

#define NWORDS(x,y) (CEILING((x),(y)) / (y))

static int
size_string(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    /*
     * NOTE: Strings contain one more byte of data than the length
     * slot indicates.
     */

    vector = (struct vector *) where;
    length = fixnum_value(vector->length) + 1;
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 8) + 2, 2);
#else
    nwords = CEILING(NWORDS(length, 4) + 2, 2);
#endif
    return nwords;
}

static int
scav_string(lispobj * where, lispobj object)
{
    return size_string(where);
}

static lispobj
trans_string(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_string((lispobj *) PTR(object)));
}


/************************************************************************
			     Hash Tables
************************************************************************/

/* This struct corresponds to the Lisp HASH-TABLE structure defined in
   hash-new.lisp.  */

struct hash_table {
    lispobj instance_header;	/* 0 */
    lispobj dummy2;
    lispobj test;
    lispobj test_fun;
    lispobj hash_fun;
    lispobj rehash_size;	/* 5 */
    lispobj rehash_threshold;
    lispobj rehash_trigger;
    lispobj number_entries;
    lispobj table;
    lispobj weak_p;		/* 10 */
    lispobj needing_rehash;
    lispobj next_free_kv;
    lispobj index_vector;
    lispobj next_vector;
    lispobj hash_vector;	/* 15 */
    lispobj next_weak_table;
};

/* The size of a hash-table in Lisp objects.  */

#define HASH_TABLE_SIZE (sizeof (struct hash_table) / sizeof (lispobj))

/* Compute the EQ-hash of KEY.  This must be the same as what's used
   in hash-new.lisp.  */

#define EQ_HASH(key) ((key) & 0x1fffffff)

/* List of weak hash tables chained through their WEAK-P slot.  Set to
   NIL at the start of a collection.

   This is not optimal because, when a table is tenured, it won't be
   processed automatically; only the yougest generation is GC'd by
   default.  On the other hand, all applications will need an
   occasional full GC anyway, so it's not that bad either.  */

static lispobj weak_hash_tables;

/* Return true if OBJ will survive the current GC.  */

static inline int
survives_gc(lispobj obj)
{
    if (!Pointerp(obj) || !from_space_p(obj))
	return 1;
    return *(lispobj *) PTR(obj) == 1;
}

/* If OBJ is a (UNSIGNED-BYTE 32) array, return a pointer to its first
   element, otherwise return null.  If LENGTH is not null, return in it
   the array's length.  */

static inline unsigned *
u32_vector(lispobj obj, unsigned *length)
{
    unsigned *ptr = NULL;

    if (Pointerp(obj)) {
	lispobj *p = (lispobj *) PTR(obj);

	if (TypeOf(p[0]) == type_SimpleArrayUnsignedByte32) {
	    ptr = (unsigned *) (p + 2);
	    if (length)
		*length = fixnum_value(p[1]);
	}
    }

    return ptr;
}

/* Free an entry of hash-table HASH-TABLE whose hash index (index in
   the hash-table's INDEX-VECTOR) is HASH_INDEX, and whose index
   in the hash-table's TABLE vector is KV_INDEX.  */

static inline void
free_hash_entry(struct hash_table *hash_table, int hash_index, int kv_index)
{
    unsigned length;
    unsigned *index_vector = u32_vector(hash_table->index_vector, &length);
    unsigned *next_vector = u32_vector(hash_table->next_vector, 0);
    int free_p = 1;

    if (index_vector[hash_index] == kv_index)
	/* The entry is the first in the collinion chain.
	   Pop it from the list.  */
	index_vector[hash_index] = next_vector[kv_index];
    else {
	/* The entry is not the first in the collision chain.  */
	unsigned prev = index_vector[hash_index];
	unsigned i = next_vector[prev];

	while (i && i != kv_index)
	    prev = i, i = next_vector[i];

	if (i == kv_index)
	    next_vector[prev] = next_vector[kv_index];
	else
	    free_p = 0;
    }

    if (free_p) {
	unsigned count = fixnum_value(hash_table->number_entries);
        lispobj* kv_vector = (lispobj *) PTR(hash_table->table);
        unsigned *hash_vector = u32_vector(hash_table->hash_vector, 0);
        unsigned hash_index;
        lispobj empty_symbol;
        
	gc_assert(count > 0);
	hash_table->number_entries = make_fixnum(count - 1);
	next_vector[kv_index] = fixnum_value(hash_table->next_free_kv);
	hash_table->next_free_kv = make_fixnum(kv_index);
        /*
         * I (rtoy) think we also need to clear out the key and value
         * in the kv-vector.  If we don't, maphash and
         * with-hash-table-iterator thinks this entry is not empty.
         */
        
        kv_vector += 2;         /* Skip over vector header and length slots */
        empty_symbol = kv_vector[1];

        hash_index = EQ_HASH(kv_vector[2 * kv_index]) % length;
        
        kv_vector[2 * kv_index] = empty_symbol;
        kv_vector[2 * kv_index + 1] = empty_symbol;
        if (hash_vector) {
            hash_vector[hash_index] = EQ_BASED_HASH_VALUE;
        }
    }
}

/* Record an entry of hash-table HASH-TABLE whose hash index (index in
   the hash-table's INDEX-VECTOR) is HASH_INDEX, and whose index
   in the hash-table's TABLE vector is KV_INDEX, for rehashing.  */

static inline void
record_for_rehashing(struct hash_table *hash_table, int hash_index,
		     int kv_index)
{
    unsigned *index_vector = u32_vector(hash_table->index_vector, 0);
    unsigned *next_vector = u32_vector(hash_table->next_vector, 0);
    int rehash_p = 1;

    if (index_vector[hash_index] == kv_index)
	/* This entry is at the head of the collision chain.
	   Pop it from that list. */
	index_vector[hash_index] = next_vector[kv_index];
    else {
	unsigned prev = index_vector[hash_index];
	unsigned i = next_vector[prev];

	while (i && i != kv_index)
	    prev = i, i = next_vector[i];

	if (i == kv_index)
	    next_vector[prev] = next_vector[kv_index];
	else
	    rehash_p = 0;
    }

    if (rehash_p) {
	next_vector[kv_index] = fixnum_value(hash_table->needing_rehash);
	hash_table->needing_rehash = make_fixnum(kv_index);
    }
}

static inline boolean
eq_based_hash_vector(unsigned int* hash_vector, unsigned int index)
{
    return (hash_vector == 0) || (hash_vector[index] == EQ_BASED_HASH_VALUE);
}

static inline boolean
removable_weak_key(lispobj old_key, unsigned int index_value, boolean eq_hash_p)
{
  return (!survives_gc(old_key)
          && eq_hash_p
          && (index_value != 0));
}

static inline boolean
removable_weak_value(lispobj value, unsigned int index_value)
{
    /*
     * The entry can be removed if the value can be GCed.
     */
    return (!survives_gc(value)
            && (index_value != 0));
}

static inline boolean
removable_weak_key_and_value(lispobj old_key, lispobj value, unsigned int index_value,
                             boolean eq_hash_p)
{
  boolean removable_key;
  boolean removable_val;
  
  removable_key = (!survives_gc(old_key)
                   && eq_hash_p
                   && (index_value != 0));
  removable_val = (!survives_gc(value)
                   && (index_value != 0));

  /*
   * The entry must stay if the key and value are alive.  In other
   * words, the entry can be removed if the key or value can be GCed.
   */
  return removable_key || removable_val;
}

static inline boolean
removable_weak_key_or_value(lispobj old_key, lispobj value, unsigned int index_value,
                            boolean eq_hash_p)
{
  boolean removable_key;
  boolean removable_val;
  
  removable_key = (!survives_gc(old_key)
                   && eq_hash_p
                   && (index_value != 0));
  removable_val = (!survives_gc(value)
                   && (index_value != 0));

  /*
   * The entry must be kept if either the key or value is alive.  In
   * other words, the entry can be removed only if both the key and
   * value can be GCed.
   */
  return (removable_key && removable_val);
}

static void
maybe_record_for_rehashing(struct hash_table *hash_table, lispobj* kv_vector,
                           unsigned int length,
                           unsigned int old_index,
                           unsigned int i,
                           boolean eq_hash_p,
                           unsigned int index_value)
{
    lispobj new_key;
    unsigned int new_index;
    lispobj empty_symbol;
    lispobj value;
    
    new_key = kv_vector[2 * i];
    value = kv_vector[2 * i + 1];
    new_index = EQ_HASH(new_key) % length;
    empty_symbol = kv_vector[1];

    if (old_index != new_index
        && eq_hash_p
        && index_value != 0
        && (new_key != empty_symbol
            || (value != empty_symbol))) {
        record_for_rehashing(hash_table, old_index, i);
    }
}

/* Scavenge the keys and values of hash-table HASH_TABLE.  WEAK
   non-zero means this function is called for a weak hash-table at the
   end of a GC.  WEAK zero means this function is called for
   scavenging a non-weak hash-table.  Value is the number of entries
   scheduled for rehashing or removed.  */

static void
scav_hash_entries(struct hash_table *hash_table, lispobj weak, int removep)
{
    unsigned kv_length;
    lispobj *kv_vector;
    unsigned *index_vector, *next_vector, *hash_vector;
    unsigned length;
    lispobj empty_symbol;
    unsigned next_vector_length;
    unsigned i;

    kv_vector = (lispobj *) PTR(hash_table->table);
    kv_length = fixnum_value(kv_vector[1]);
    kv_vector += 2;

    empty_symbol = kv_vector[1];

    index_vector = u32_vector(hash_table->index_vector, &length);
    next_vector = u32_vector(hash_table->next_vector, &next_vector_length);
    hash_vector = u32_vector(hash_table->hash_vector, 0);

    gc_assert(index_vector && next_vector);
    gc_assert(next_vector_length * 2 == kv_length);

    for (i = 1; i < next_vector_length; i++) {
	lispobj old_key = kv_vector[2 * i];
        lispobj value = kv_vector[2 * i + 1];
	unsigned int old_index = EQ_HASH(old_key) % length;
        boolean eq_hash_p = eq_based_hash_vector(hash_vector, i);
        unsigned int index_value = index_vector[old_index];

	if (((weak == KEY)
             && removable_weak_key(old_key, index_value,
                                   eq_hash_p))
            || ((weak == VALUE)
                && removable_weak_value(value, index_value))
            || ((weak == KEY_AND_VALUE)
                && removable_weak_key_and_value(old_key, value, index_value, eq_hash_p))
            || ((weak == KEY_OR_VALUE)
                && removable_weak_key_or_value(old_key, value, index_value, eq_hash_p))) {
            if (removep) {
                free_hash_entry(hash_table, old_index, i);
            }
        } else {
	    /* If the key is EQ-hashed and moves, schedule it for rehashing. */
	    scavenge(&kv_vector[2 * i], 2);
#if 0
	    new_key = kv_vector[2 * i];
	    new_index = EQ_HASH(new_key) % length;

	    if (old_index != new_index
		&& eq_hash_p
		&& index_value != 0
		&& (new_key != empty_symbol
		    || (value != empty_symbol))) {
                record_for_rehashing(hash_table, old_index, i);
            }
#endif
            maybe_record_for_rehashing(hash_table, kv_vector, length, old_index, i, eq_hash_p,
                                       index_value);
	}
    }
}

static inline boolean
weak_key_survives(lispobj old_key, unsigned index_value, unsigned int eq_hash_p)
{
    return (survives_gc(old_key)
	    && index_value != 0
	    && eq_hash_p);
}

static inline boolean
weak_value_survives(lispobj value)
{
    return (survives_gc(value));
}

/* Scavenge entries of the weak hash-table HASH_TABLE that haven't
   been already.  Value is 1 if anything new has been scavenged, 0
   otherwise.  */

static int
scav_weak_entries(struct hash_table *hash_table)
{
    lispobj *kv_vector;
    unsigned *index_vector, *hash_vector;
    unsigned length;
    unsigned next_vector_length;
    unsigned i, scavenged = 0;

    kv_vector = (lispobj *) PTR(hash_table->table) + 2;

    index_vector = u32_vector(hash_table->index_vector, &length);
    u32_vector(hash_table->next_vector, &next_vector_length);
    hash_vector = u32_vector(hash_table->hash_vector, 0);

    for (i = 1; i < next_vector_length; i++) {
	lispobj old_key = kv_vector[2 * i];
        lispobj value = kv_vector[2 * i + 1];
	unsigned int old_index = EQ_HASH(old_key) % length;
        boolean eq_hash_p = eq_based_hash_vector(hash_vector, i);
	boolean key_survives = weak_key_survives(old_key, 
                                                 index_vector[old_index], eq_hash_p);
	boolean value_survives = weak_value_survives(value);


	if ((hash_table->weak_p == KEY)
	    && key_survives
            && !survives_gc(value)) {
	    /*
	     * For a weak key hash table, if the key survives,
	     * scavenge its value, for the case that the only
	     * reference to a key in a weak table is a value in
	     * another weak table.  Don't scavenge the value twice;
	     * scan_weak_tables calls this function more than once for
	     * the same hash table.
	     */
	    scavenge(&kv_vector[2 * i + 1], 1);
	    scavenged = 1;
	} else if ((hash_table->weak_p == VALUE)
                   && value_survives
                   && !survives_gc(old_key)) {
	    /*
	     * For a weak value hash table, scavenge the key, if the
	     * value survives gc.
	     */
	    scavenge(&kv_vector[2 * i], 1);
            maybe_record_for_rehashing(hash_table, kv_vector, length, old_index, i, eq_hash_p,
                                       index_vector[old_index]);
	    scavenged = 1;
        } else if ((hash_table->weak_p == KEY_AND_VALUE)
                   && key_survives && value_survives) {
            /* There's nothing to do for key-and-value.  Both are already alive */
        } else if ((hash_table->weak_p == KEY_OR_VALUE)
                   && (key_survives || value_survives)) {
            /* For key-or-value, make sure the other is scavenged */
            if (key_survives && !survives_gc(value)) {
                scavenge(&kv_vector[2 * i + 1], 1);
                scavenged = 1;
            }
            if (value_survives && !survives_gc(old_key)) {
                scavenge(&kv_vector[2 * i], 1);
                maybe_record_for_rehashing(hash_table, kv_vector, length, old_index, i,
                                           eq_hash_p,
                                           index_vector[old_index]);
                scavenged = 1;
            }
	}
    }

    return scavenged;
}

static void
scav_weak_tables(void)
{
    lispobj table, next;
    int more_scavenged;

    /* Scavenge hash values of surviving keys, until there is nothing
       new.  This is for the case that the only reference to a weak key
       is a value in another weak table.  */
    do {
	more_scavenged = 0;

	for (table = weak_hash_tables; table != NIL; table = next) {
	    struct hash_table *ht = (struct hash_table *) PTR(table);

	    next = ht->next_weak_table;
	    if (scav_weak_entries(ht))
		more_scavenged = 1;
	}
    }
    while (more_scavenged);

    for (table = weak_hash_tables; table != NIL; table = next) {
	struct hash_table *ht = (struct hash_table *) PTR(table);

	next = ht->next_weak_table;
	scav_hash_entries(ht, ht->weak_p, 0);
    }
}

    
/* Process weak hash-tables at the end of a GC.  */

static void
scan_weak_tables(void)
{
    lispobj table, next;
    int more_scavenged;

    for (table = weak_hash_tables; table != NIL; table = next) {
	struct hash_table *ht = (struct hash_table *) PTR(table);

	next = ht->next_weak_table;
        /* We're done with the table, so reset the link! */
	ht->next_weak_table = NIL;
        /*
         * Remove the entries in the table.  (This probably does too
         * much work!)
         */
	scav_hash_entries(ht, ht->weak_p, 1);
    }

    weak_hash_tables = NIL;
}

/* Scavenge a key/value vector of a hash-table.  */

static int
scav_hash_vector(lispobj * where, lispobj object)
{
    unsigned int kv_length;
    lispobj *kv_vector;
    lispobj empty_symbol, hash_table_obj;
    struct hash_table *hash_table;

    if (HeaderValue(object) != subtype_VectorValidHashing)
	return 1;

    /* WHERE is a hash table key/value vector.  First word is header,
       second is vector length.  Keys and values follow after the
       length.  The first value is the symbol :empty, the first key is a
       reference to the hash-table containing the key/value vector.
       (See hash-new.lisp, MAKE-HASH-TABLE.)  */

    kv_length = fixnum_value(where[1]);
    kv_vector = where + 2;

    scavenge(kv_vector, 2);

    gc_assert(Pointerp(kv_vector[0]));
    gc_assert(Pointerp(kv_vector[1]));

    hash_table_obj = kv_vector[0];
    hash_table = (struct hash_table *) PTR(hash_table_obj);
    empty_symbol = kv_vector[1];

    /*
     * For some reason, the following GC assert doesn't always hold true
     * on Sparc/gencgc.  I (RLT) don't know why that is.  So turn it off
     * for now.  I leave these printfs here so I can see it happening,
     * just in case.
     *
     * Some checks using an *after-gc-hooks* to check hash tables
     * indicates that the invariant we're testing is actually still
     * true.  It appears that it just happens not to be true when we're
     * scavenging the hash vector.  I don't know why.
     */
#if (0 && defined(sparc))
    if (where != (lispobj *) PTR(hash_table->table)) {
	fprintf(stderr, "Hash table invariant failed during scavenging!\n");
	fprintf(stderr, " *** where = %lx\n", where);
	fprintf(stderr, " *** hash_table = %lx\n", hash_table);
	fprintf(stderr, " *** hash_table->table = %lx\n",
		PTR(hash_table->table));
    }
#endif

#if !(defined(sparc) || defined(DARWIN))
    gc_assert(where == (lispobj *) PTR(hash_table->table));
#endif
    gc_assert(TypeOf(hash_table->instance_header) == type_InstanceHeader);
    gc_assert(TypeOf(*(lispobj *) PTR(empty_symbol)) == type_SymbolHeader);

    /* Scavenging the hash table which fix the positions of the other
       needed objects.  */
#if 0
    if (hash_table >= (void*) 0x40000000) {
        fprintf(stderr, "scav_hash_vector: scavenge table %p\n", hash_table);
    }
#endif

    scavenge((lispobj *) hash_table, HASH_TABLE_SIZE);

    if (hash_table->weak_p == NIL) {
        scav_hash_entries(hash_table, hash_table->weak_p, 1);
    } else if (hash_table->next_weak_table == NIL) {
        /*
         * Make sure we only add the table once, which means
         * next_weak_table is NIL if it isn't already on the list.
         */
        hash_table->next_weak_table = weak_hash_tables;
        weak_hash_tables = hash_table_obj;
    }
      
    return CEILING(kv_length + 2, 2);
}


static int
size_vector(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length + 2, 2);

    return nwords;
}

static lispobj
trans_vector(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_object(object, size_vector((lispobj *) PTR(object)));
}


static int
size_vector_bit(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 64) + 2, 2);
#else
    nwords = CEILING(NWORDS(length, 32) + 2, 2);
#endif
    return nwords;
}

static int
scav_vector_bit(lispobj * where, lispobj object)
{
    return size_vector_bit(where);
}

static lispobj
trans_vector_bit(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_bit((lispobj *) PTR(object)));
}


static int
size_vector_unsigned_byte_2(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 32) + 2, 2);
#else
    nwords = CEILING(NWORDS(length, 16) + 2, 2);
#endif
    return nwords;
}

static int
scav_vector_unsigned_byte_2(lispobj * where, lispobj object)
{
    return size_vector_unsigned_byte_2(where);
}

static lispobj
trans_vector_unsigned_byte_2(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_unsigned_byte_2((lispobj *)
								 PTR(object)));
}


static int
size_vector_unsigned_byte_4(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 16) + 2, 2);
#else
    nwords = CEILING(NWORDS(length, 8) + 2, 2);
#endif
    return nwords;
}

static int
scav_vector_unsigned_byte_4(lispobj * where, lispobj object)
{
    return size_vector_unsigned_byte_4(where);
}

static lispobj
trans_vector_unsigned_byte_4(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_unsigned_byte_4((lispobj *)
								 PTR(object)));
}


static int
size_vector_unsigned_byte_8(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 8) + 2, 2);
#else
    nwords = CEILING(NWORDS(length, 4) + 2, 2);
#endif
    return nwords;
}

static int
scav_vector_unsigned_byte_8(lispobj * where, lispobj object)
{
    return size_vector_unsigned_byte_8(where);
}

static lispobj
trans_vector_unsigned_byte_8(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_unsigned_byte_8((lispobj *)
								 PTR(object)));
}


static int
size_vector_unsigned_byte_16(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 4) + 2, 2);
#else
    nwords = CEILING(NWORDS(length, 2) + 2, 2);
#endif
    return nwords;
}

static int
scav_vector_unsigned_byte_16(lispobj * where, lispobj object)
{
    return size_vector_unsigned_byte_16(where);
}

static lispobj
trans_vector_unsigned_byte_16(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_unsigned_byte_16((lispobj *)
								  PTR(object)));
}


static int
size_vector_unsigned_byte_32(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 2) + 2, 2);
#else
    nwords = CEILING(length + 2, 2);
#endif
    return nwords;
}

static int
scav_vector_unsigned_byte_32(lispobj * where, lispobj object)
{
    return size_vector_unsigned_byte_32(where);
}

static lispobj
trans_vector_unsigned_byte_32(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_unsigned_byte_32((lispobj *)
								  PTR(object)));
}


static int
size_vector_single_float(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 2) + 2, 2);
#else
    nwords = CEILING(length + 2, 2);
#endif
    return nwords;
}

static int
scav_vector_single_float(lispobj * where, lispobj object)
{
    return size_vector_single_float(where);
}

static lispobj
trans_vector_single_float(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_single_float((lispobj *)
							      PTR(object)));
}


static int
size_vector_double_float(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(length + 2, 2);
#else
    nwords = length * 2 + 2;	/* alignment guaranteed */
#endif
    return nwords;
}

static int
scav_vector_double_float(lispobj * where, lispobj object)
{
    return size_vector_double_float(where);
}

static lispobj
trans_vector_double_float(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_double_float((lispobj *)
							      PTR(object)));
}


#ifdef type_SimpleArrayLongFloat
static int
size_vector_long_float(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = length * 2 + 2;	/* alignment guaranteed */
#else
    nwords = CEILING(length * 3 + 2, 2);
#endif
    return nwords;
}

static int
scav_vector_long_float(lispobj * where, lispobj object)
{
    return size_vector_long_float(where);
}

static lispobj
trans_vector_long_float(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_long_float((lispobj *)
							    PTR(object)));
}
#endif

#ifdef type_SimpleArrayDoubleDoubleFloat
static int
size_vector_double_double_float(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = CEILING(length * 4 + 2, 2);

    return nwords;
}

static int
scav_vector_double_double_float(lispobj * where, lispobj object)
{
    return size_vector_double_double_float(where);
}

static lispobj
trans_vector_double_double_float(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_double_double_float((lispobj *)
							    PTR(object)));
}
#endif

#ifdef type_SimpleArrayComplexSingleFloat
static int
size_vector_complex_single_float(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(length + 2, 2);
#else
    nwords = length * 2 + 2;	/* this must be an even number */
#endif
    return nwords;
}

static int
scav_vector_complex_single_float(lispobj * where, lispobj object)
{
    return size_vector_complex_single_float(where);
}

static lispobj
trans_vector_complex_single_float(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_complex_single_float(
								      (lispobj
								       *)
								      PTR
								      (object)));
}
#endif

#ifdef type_SimpleArrayComplexDoubleFloat
static int
size_vector_complex_double_float(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = length * 2 + 2;	/* alignment guaranteed */
#else
    nwords = length * 4 + 2;	/* this must be an even number */
#endif
    return nwords;
}

static int
scav_vector_complex_double_float(lispobj * where, lispobj object)
{
    return size_vector_complex_double_float(where);
}

static lispobj
trans_vector_complex_double_float(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_complex_double_float(
								      (lispobj
								       *)
								      PTR
								      (object)));
}
#endif


#ifdef type_SimpleArrayComplexLongFloat
static int
size_vector_complex_long_float(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = length * 4 + 2;	/* alignment guaranteed */
#else
    nwords = length * 6 + 2;	/* alignment guaranteed */
#endif
    return nwords;
}

static int
scav_vector_complex_long_float(lispobj * where, lispobj object)
{
    return size_vector_complex_long_float(where);
}

static lispobj
trans_vector_complex_long_float(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_complex_long_float((lispobj *)
								    PTR
								    (object)));
}
#endif

#ifdef type_SimpleArrayComplexDoubleDoubleFloat
static int
size_vector_complex_double_double_float(lispobj * where)
{
    struct vector *vector;
    int length, nwords;

    vector = (struct vector *) where;
    length = fixnum_value(vector->length);
    nwords = length * 8 + 2;

    return nwords;
}

static int
scav_vector_complex_double_double_float(lispobj * where, lispobj object)
{
    return size_vector_complex_double_double_float(where);
}

static lispobj
trans_vector_complex_double_double_float(lispobj object)
{
    gc_assert(Pointerp(object));
    return copy_large_unboxed_object(object,
				     size_vector_complex_double_double_float((lispobj *)
									     PTR
									     (object)));
}
#endif



/* Weak Pointers */

/*
 * XX Hack adapted from cgc.c; These don't work too well with the
 * gencgc as a list of the weak pointers is maintained within the
 * objects which causes writes to the pages. A limited attempt is made
 * to avoid unnecessary writes, but this needs a re-think.
 */

#define WEAK_POINTER_NWORDS \
	CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

static int
scav_weak_pointer(lispobj * where, lispobj object)
{
    struct weak_pointer *this_wp = (struct weak_pointer *) where;

    if (this_wp->mark_bit == NIL) {
	this_wp->mark_bit = T;
	this_wp->next = weak_pointers;
	weak_pointers = this_wp;
    }

    return WEAK_POINTER_NWORDS;
}

static lispobj
trans_weak_pointer(lispobj object)
{
    lispobj copy;

    gc_assert(Pointerp(object));
    copy = copy_object(object, WEAK_POINTER_NWORDS);
#if 0
    fprintf(stderr, "Transport weak pointer %p to %p\n", object, copy);
#endif
    return copy;
}

static int
size_weak_pointer(lispobj * where)
{
    return WEAK_POINTER_NWORDS;
}

void
scan_weak_pointers(void)
{
    struct weak_pointer *wp;

    for (wp = weak_pointers; wp; wp = wp->next) {
	lispobj value = wp->value;
	lispobj *first_pointer = (lispobj *) PTR(value);

	wp->mark_bit = NIL;
	if (Pointerp(value) && from_space_p(value)) {
	    if (first_pointer[0] == 0x01)
		wp->value = first_pointer[1];
	    else {
		wp->value = NIL;
		wp->broken = T;
	    }
	}
    }
}


/* Scavenged Hooks */

#define SCAVENGER_HOOK_NWORDS \
	CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

static int
scav_scavenger_hook(lispobj * where, lispobj object)
{
    struct scavenger_hook *scav_hook = (struct scavenger_hook *) where;
    lispobj old_value = scav_hook->value;

#if 0
    fprintf(stderr, "scav scav_hook %x; value %x\n", where, old_value);
#endif

    /* Scavenge the value */
    scavenge(where + 1, 1);

    if (scav_hook->value != old_value) {
	/* Value object has moved */
#if 0
	fprintf(stderr, "   value object moved to %x\n", scav_hook->value);
#endif

	/* Check if this hook is already noted. */
#if 0
	fprintf(stderr, "   next=%x sh hooks=%x\n",
		scav_hook->next, scavenger_hooks);
#endif
	if (scav_hook->next == NULL) {
#if 0
	    fprintf(stderr, "   adding to scavenger_hooks\n");
#endif
	    scav_hook->next = scavenger_hooks;
	    scavenger_hooks = (struct scavenger_hook *) ((size_t) where |
							 type_OtherPointer);
	}
    }

    /* Scavenge the function and the tail scavenge_hook */
    return 2;
}

static lispobj
trans_scavenger_hook(lispobj object)
{
    lispobj copy;

    gc_assert(Pointerp(object));
#if 0
    printf("Transporting scav pointer from 0x%08x\n", object);
#endif
    copy = copy_object(object, SCAVENGER_HOOK_NWORDS);
    return copy;
}

static int
size_scavenger_hook(lispobj * where)
{
    return SCAVENGER_HOOK_NWORDS;
}


/* Initialization */

static int
scav_lose(lispobj * where, lispobj object)
{
    fprintf(stderr, "GC lossage.  No scavenge function for object 0x%08lx\n",
	    (unsigned long) object);
    lose(NULL);
    return 0;
}

static lispobj
trans_lose(lispobj object)
{
    fprintf(stderr, "GC lossage.  No transport function for object 0x%08lx\n",
	    (unsigned long) object);
    lose(NULL);
    return NIL;
}

static int
size_lose(lispobj * where)
{
    fprintf(stderr, "Size lossage.  No size function for object at 0x%08lx\n",
	    (unsigned long) where);
    fprintf(stderr, "First word of object: 0x%08lx\n", (unsigned long) *where);
    return 1;
}

static void
gc_init_tables(void)
{
    int i;

    /* Scavenge Table */
    for (i = 0; i < 256; i++)
	scavtab[i] = scav_lose;

    for (i = 0; i < 32; i++) {
	scavtab[type_EvenFixnum | (i << 3)] = scav_immediate;
	scavtab[type_FunctionPointer | (i << 3)] = scav_function_pointer;
	/* OtherImmediate0 */
	scavtab[type_ListPointer | (i << 3)] = scav_list_pointer;
	scavtab[type_OddFixnum | (i << 3)] = scav_immediate;
	scavtab[type_InstancePointer | (i << 3)] = scav_instance_pointer;
	/* OtherImmediate1 */
	scavtab[type_OtherPointer | (i << 3)] = scav_other_pointer;
    }

    scavtab[type_Bignum] = scav_unboxed;
    scavtab[type_Ratio] = scav_boxed;
    scavtab[type_SingleFloat] = scav_unboxed;
    scavtab[type_DoubleFloat] = scav_unboxed;
#ifdef type_LongFloat
    scavtab[type_LongFloat] = scav_unboxed;
#endif
#ifdef type_DoubleDoubleFloat
    scavtab[type_DoubleDoubleFloat] = scav_unboxed;
#endif    
    scavtab[type_Complex] = scav_boxed;
#ifdef type_ComplexSingleFloat
    scavtab[type_ComplexSingleFloat] = scav_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
    scavtab[type_ComplexDoubleFloat] = scav_unboxed;
#endif
#ifdef type_ComplexLongFloat
    scavtab[type_ComplexLongFloat] = scav_unboxed;
#endif
#ifdef type_ComplexDoubleDoubleFloat
    scavtab[type_ComplexDoubleDoubleFloat] = scav_unboxed;
#endif
    scavtab[type_SimpleArray] = scav_boxed;
    scavtab[type_SimpleString] = scav_string;
    scavtab[type_SimpleBitVector] = scav_vector_bit;
    scavtab[type_SimpleVector] = scav_hash_vector;
    scavtab[type_SimpleArrayUnsignedByte2] = scav_vector_unsigned_byte_2;
    scavtab[type_SimpleArrayUnsignedByte4] = scav_vector_unsigned_byte_4;
    scavtab[type_SimpleArrayUnsignedByte8] = scav_vector_unsigned_byte_8;
    scavtab[type_SimpleArrayUnsignedByte16] = scav_vector_unsigned_byte_16;
    scavtab[type_SimpleArrayUnsignedByte32] = scav_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
    scavtab[type_SimpleArraySignedByte8] = scav_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
    scavtab[type_SimpleArraySignedByte16] = scav_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
    scavtab[type_SimpleArraySignedByte30] = scav_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
    scavtab[type_SimpleArraySignedByte32] = scav_vector_unsigned_byte_32;
#endif
    scavtab[type_SimpleArraySingleFloat] = scav_vector_single_float;
    scavtab[type_SimpleArrayDoubleFloat] = scav_vector_double_float;
#ifdef type_SimpleArrayLongFloat
    scavtab[type_SimpleArrayLongFloat] = scav_vector_long_float;
#endif
#ifdef type_SimpleArrayDoubleDoubleFloat
    scavtab[type_SimpleArrayDoubleDoubleFloat] = scav_vector_double_double_float;
#endif    
#ifdef type_SimpleArrayComplexSingleFloat
    scavtab[type_SimpleArrayComplexSingleFloat] =
	scav_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
    scavtab[type_SimpleArrayComplexDoubleFloat] =
	scav_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
    scavtab[type_SimpleArrayComplexLongFloat] = scav_vector_complex_long_float;
#endif
#ifdef type_SimpleArrayComplexDoubleDoubleFloat
    scavtab[type_SimpleArrayComplexDoubleDoubleFloat] =
	scav_vector_complex_double_double_float;
#endif
    scavtab[type_ComplexString] = scav_boxed;
    scavtab[type_ComplexBitVector] = scav_boxed;
    scavtab[type_ComplexVector] = scav_boxed;
    scavtab[type_ComplexArray] = scav_boxed;
    scavtab[type_CodeHeader] = scav_code_header;
#if !(defined(i386) || defined(__x86_64))
    scavtab[type_FunctionHeader] = scav_function_header;
    scavtab[type_ClosureFunctionHeader] = scav_function_header;
    scavtab[type_ReturnPcHeader] = scav_return_pc_header;
#endif
#if defined(i386) || defined(__x86_64)
    scavtab[type_ClosureHeader] = scav_closure_header;
    scavtab[type_FuncallableInstanceHeader] = scav_closure_header;
    scavtab[type_ByteCodeFunction] = scav_closure_header;
    scavtab[type_ByteCodeClosure] = scav_closure_header;
#ifdef type_DylanFunctionHeader
    scavtab[type_DylanFunctionHeader] = scav_closure_header;
#endif
#else
    scavtab[type_ClosureHeader] = scav_boxed;
    scavtab[type_FuncallableInstanceHeader] = scav_boxed;
    scavtab[type_ByteCodeFunction] = scav_boxed;
    scavtab[type_ByteCodeClosure] = scav_boxed;
#ifdef type_DylanFunctionHeader
    scavtab[type_DylanFunctionHeader] = scav_boxed;
#endif
#endif
    scavtab[type_ValueCellHeader] = scav_boxed;
    scavtab[type_SymbolHeader] = scav_boxed;
    scavtab[type_BaseChar] = scav_immediate;
    scavtab[type_Sap] = scav_unboxed;
    scavtab[type_UnboundMarker] = scav_immediate;
    scavtab[type_WeakPointer] = scav_weak_pointer;
    scavtab[type_InstanceHeader] = scav_boxed;
    /*
     * Note: for sparc and ppc we don't have to do anything special
     * for fdefns, cause the raw-addr has a function lowtag.
     */
#if !(defined(sparc) || defined(DARWIN))
    scavtab[type_Fdefn] = scav_fdefn;
#else
    scavtab[type_Fdefn] = scav_boxed;
#endif

    scavtab[type_ScavengerHook] = scav_scavenger_hook;

    /* Transport Other Table */
    for (i = 0; i < 256; i++)
	transother[i] = trans_lose;

    transother[type_Bignum] = trans_unboxed_large;
    transother[type_Ratio] = trans_boxed;
    transother[type_SingleFloat] = trans_unboxed;
    transother[type_DoubleFloat] = trans_unboxed;
#ifdef type_LongFloat
    transother[type_LongFloat] = trans_unboxed;
#endif
#ifdef type_DoubleDoubleFloat
    transother[type_DoubleDoubleFloat] = trans_unboxed;
#endif
    transother[type_Complex] = trans_boxed;
#ifdef type_ComplexSingleFloat
    transother[type_ComplexSingleFloat] = trans_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
    transother[type_ComplexDoubleFloat] = trans_unboxed;
#endif
#ifdef type_ComplexLongFloat
    transother[type_ComplexLongFloat] = trans_unboxed;
#endif
#ifdef type_ComplexDoubleDoubleFloat
    transother[type_ComplexDoubleDoubleFloat] = trans_unboxed;
#endif
    transother[type_SimpleArray] = trans_boxed_large;
    transother[type_SimpleString] = trans_string;
    transother[type_SimpleBitVector] = trans_vector_bit;
    transother[type_SimpleVector] = trans_vector;
    transother[type_SimpleArrayUnsignedByte2] = trans_vector_unsigned_byte_2;
    transother[type_SimpleArrayUnsignedByte4] = trans_vector_unsigned_byte_4;
    transother[type_SimpleArrayUnsignedByte8] = trans_vector_unsigned_byte_8;
    transother[type_SimpleArrayUnsignedByte16] = trans_vector_unsigned_byte_16;
    transother[type_SimpleArrayUnsignedByte32] = trans_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
    transother[type_SimpleArraySignedByte8] = trans_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
    transother[type_SimpleArraySignedByte16] = trans_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
    transother[type_SimpleArraySignedByte30] = trans_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
    transother[type_SimpleArraySignedByte32] = trans_vector_unsigned_byte_32;
#endif
    transother[type_SimpleArraySingleFloat] = trans_vector_single_float;
    transother[type_SimpleArrayDoubleFloat] = trans_vector_double_float;
#ifdef type_SimpleArrayLongFloat
    transother[type_SimpleArrayLongFloat] = trans_vector_long_float;
#endif
#ifdef type_SimpleArrayDoubleDoubleFloat
    transother[type_SimpleArrayDoubleDoubleFloat] = trans_vector_double_double_float;
#endif
#ifdef type_SimpleArrayComplexSingleFloat
    transother[type_SimpleArrayComplexSingleFloat] =
	trans_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
    transother[type_SimpleArrayComplexDoubleFloat] =
	trans_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
    transother[type_SimpleArrayComplexLongFloat] =
	trans_vector_complex_long_float;
#endif
#ifdef type_SimpleArrayComplexDoubleDoubleFloat
    transother[type_SimpleArrayComplexDoubleDoubleFloat] =
	trans_vector_complex_double_double_float;
#endif
    transother[type_ComplexString] = trans_boxed;
    transother[type_ComplexBitVector] = trans_boxed;
    transother[type_ComplexVector] = trans_boxed;
    transother[type_ComplexArray] = trans_boxed;
    transother[type_CodeHeader] = trans_code_header;
    transother[type_FunctionHeader] = trans_function_header;
    transother[type_ClosureFunctionHeader] = trans_function_header;
    transother[type_ReturnPcHeader] = trans_return_pc_header;
    transother[type_ClosureHeader] = trans_boxed;
    transother[type_FuncallableInstanceHeader] = trans_boxed;
    transother[type_ByteCodeFunction] = trans_boxed;
    transother[type_ByteCodeClosure] = trans_boxed;
    transother[type_ValueCellHeader] = trans_boxed;
    transother[type_SymbolHeader] = trans_boxed;
    transother[type_BaseChar] = trans_immediate;
    transother[type_Sap] = trans_unboxed;
    transother[type_UnboundMarker] = trans_immediate;
    transother[type_WeakPointer] = trans_weak_pointer;
    transother[type_InstanceHeader] = trans_boxed;
    transother[type_Fdefn] = trans_boxed;
    transother[type_ScavengerHook] = trans_scavenger_hook;

    /* Size table */

    for (i = 0; i < 256; i++)
	sizetab[i] = size_lose;

    for (i = 0; i < 32; i++) {
	sizetab[type_EvenFixnum | (i << 3)] = size_immediate;
	sizetab[type_FunctionPointer | (i << 3)] = size_pointer;
	/* OtherImmediate0 */
	sizetab[type_ListPointer | (i << 3)] = size_pointer;
	sizetab[type_OddFixnum | (i << 3)] = size_immediate;
	sizetab[type_InstancePointer | (i << 3)] = size_pointer;
	/* OtherImmediate1 */
	sizetab[type_OtherPointer | (i << 3)] = size_pointer;
    }

    sizetab[type_Bignum] = size_unboxed;
    sizetab[type_Ratio] = size_boxed;
    sizetab[type_SingleFloat] = size_unboxed;
    sizetab[type_DoubleFloat] = size_unboxed;
#ifdef type_LongFloat
    sizetab[type_LongFloat] = size_unboxed;
#endif
#ifdef type_DoubleDoubleFloat
    sizetab[type_DoubleDoubleFloat] = size_unboxed;
#endif
    sizetab[type_Complex] = size_boxed;
#ifdef type_ComplexSingleFloat
    sizetab[type_ComplexSingleFloat] = size_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
    sizetab[type_ComplexDoubleFloat] = size_unboxed;
#endif
#ifdef type_ComplexLongFloat
    sizetab[type_ComplexLongFloat] = size_unboxed;
#endif
#ifdef type_ComplexDoubleDoubleFloat
    sizetab[type_ComplexDoubleDoubleFloat] = size_unboxed;
#endif
    sizetab[type_SimpleArray] = size_boxed;
    sizetab[type_SimpleString] = size_string;
    sizetab[type_SimpleBitVector] = size_vector_bit;
    sizetab[type_SimpleVector] = size_vector;
    sizetab[type_SimpleArrayUnsignedByte2] = size_vector_unsigned_byte_2;
    sizetab[type_SimpleArrayUnsignedByte4] = size_vector_unsigned_byte_4;
    sizetab[type_SimpleArrayUnsignedByte8] = size_vector_unsigned_byte_8;
    sizetab[type_SimpleArrayUnsignedByte16] = size_vector_unsigned_byte_16;
    sizetab[type_SimpleArrayUnsignedByte32] = size_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
    sizetab[type_SimpleArraySignedByte8] = size_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
    sizetab[type_SimpleArraySignedByte16] = size_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
    sizetab[type_SimpleArraySignedByte30] = size_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
    sizetab[type_SimpleArraySignedByte32] = size_vector_unsigned_byte_32;
#endif
    sizetab[type_SimpleArraySingleFloat] = size_vector_single_float;
    sizetab[type_SimpleArrayDoubleFloat] = size_vector_double_float;
#ifdef type_SimpleArrayLongFloat
    sizetab[type_SimpleArrayLongFloat] = size_vector_long_float;
#endif
#ifdef type_SimpleArrayDoubleDoubleFloat
    sizetab[type_SimpleArrayDoubleDoubleFloat] = size_vector_double_double_float;
#endif
#ifdef type_SimpleArrayComplexSingleFloat
    sizetab[type_SimpleArrayComplexSingleFloat] =
	size_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
    sizetab[type_SimpleArrayComplexDoubleFloat] =
	size_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
    sizetab[type_SimpleArrayComplexLongFloat] = size_vector_complex_long_float;
#endif
#ifdef type_SimpleArrayComplexDoubleDoubleFloat
    sizetab[type_SimpleArrayComplexDoubleDoubleFloat] =
	size_vector_complex_double_double_float;
#endif
    sizetab[type_ComplexString] = size_boxed;
    sizetab[type_ComplexBitVector] = size_boxed;
    sizetab[type_ComplexVector] = size_boxed;
    sizetab[type_ComplexArray] = size_boxed;
    sizetab[type_CodeHeader] = size_code_header;
#if 0
    /* Shouldn't see these so just lose if it happens */
    sizetab[type_FunctionHeader] = size_function_header;
    sizetab[type_ClosureFunctionHeader] = size_function_header;
    sizetab[type_ReturnPcHeader] = size_return_pc_header;
#endif
    sizetab[type_ClosureHeader] = size_boxed;
    sizetab[type_FuncallableInstanceHeader] = size_boxed;
    sizetab[type_ValueCellHeader] = size_boxed;
    sizetab[type_SymbolHeader] = size_boxed;
    sizetab[type_BaseChar] = size_immediate;
    sizetab[type_Sap] = size_unboxed;
    sizetab[type_UnboundMarker] = size_immediate;
    sizetab[type_WeakPointer] = size_weak_pointer;
    sizetab[type_InstanceHeader] = size_boxed;
    sizetab[type_Fdefn] = size_boxed;
    sizetab[type_ScavengerHook] = size_scavenger_hook;
}



/*
 * Scan an area looking for an object which encloses the given
 * pointer. Returns the object start on success or NULL on failure.
 */
static lispobj *
search_space(lispobj * start, size_t words, lispobj * pointer)
{
    while (words > 0) {
	size_t count = 1;
	lispobj thing = *start;

	/* If thing is an immediate then this is a cons */
	if (Pointerp(thing)
	    || (thing & 3) == 0	/* fixnum */
	    || TypeOf(thing) == type_BaseChar
	    || TypeOf(thing) == type_UnboundMarker) count = 2;
	else
	    count = (sizetab[TypeOf(thing)]) (start);

	/* Check if the pointer is within this object? */
	if (pointer >= start && pointer < start + count) {
	    /* Found it. */
#if 0
	    fprintf(stderr, "* Found %x in %x %x\n", pointer, start, thing);
#endif
	    return start;
	}

	/* Round up the count */
	count = CEILING(count, 2);

	start += count;
	words -= count;
    }
    return NULL;
}

static lispobj *
search_read_only_space(lispobj * pointer)
{
    lispobj *start = (lispobj *) READ_ONLY_SPACE_START;
    lispobj *end = (lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER);

    if (pointer < start || pointer >= end)
	return NULL;
    return search_space(start, pointer + 2 - start, pointer);
}

static lispobj *
search_static_space(lispobj * pointer)
{
    lispobj *start = (lispobj *) static_space;
    lispobj *end = (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER);

    if (pointer < start || pointer >= end)
	return NULL;
    return search_space(start, pointer + 2 - start, pointer);
}

/*
 * Faster version for searching the dynamic space. This will work even
 * if the object is in a current allocation region.
 */
lispobj *
search_dynamic_space(lispobj * pointer)
{
    int page_index = find_page_index(pointer);
    lispobj *start;

    /* Address may be invalid - do some checks. */
    if (page_index == -1 || !PAGE_ALLOCATED(page_index))
	return NULL;
    start = (lispobj *) (page_address(page_index)
			 + page_table[page_index].first_object_offset);
    return search_space(start, pointer + 2 - start, pointer);
}

#if defined(i386) || defined(__x86_64)
static int
valid_dynamic_space_pointer(lispobj * pointer)
{
    lispobj *start_addr;

    /* Find the object start address */
    if ((start_addr = search_dynamic_space(pointer)) == NULL)
	return FALSE;

    /*
     * Need to allow raw pointers into Code objects for return
     * addresses. This will also pickup pointers to functions in code
     * objects.
     */
    if (TypeOf(*start_addr) == type_CodeHeader)
	/* X Could do some further checks here. */
	return TRUE;

    /*
     * If it's not a return address then it needs to be a valid lisp pointer.
     */
    if (!Pointerp((lispobj) pointer))
	return FALSE;

    /*
     * Check that the object pointed to is consistent with the pointer
     * low tag.
     */
    switch (LowtagOf((lispobj) pointer)) {
      case type_FunctionPointer:
	  /*
	   * Start_addr should be the enclosing code object, or a closure
	   * header.
	   */
	  switch (TypeOf(*start_addr)) {
	    case type_CodeHeader:
		/* This case is probably caught above. */
		break;
	    case type_ClosureHeader:
	    case type_FuncallableInstanceHeader:
	    case type_ByteCodeFunction:
	    case type_ByteCodeClosure:
#ifdef type_DylanFunctionHeader
	    case type_DylanFunctionHeader:
#endif
		if ((size_t) pointer !=
		    (size_t) start_addr + type_FunctionPointer) {
		    return FALSE;
		}
		break;
	    default:
		return FALSE;
	  }
	  break;
      case type_ListPointer:
	  if ((size_t) pointer != (size_t) start_addr + type_ListPointer) {
	      return FALSE;
	  }
	  /* Is it plausible cons? */
	  if ((Pointerp(start_addr[0])
	       || (start_addr[0] & 3) == 0	/* fixnum */
	       || TypeOf(start_addr[0]) == type_BaseChar
	       || TypeOf(start_addr[0]) == type_UnboundMarker)
	      && (Pointerp(start_addr[1])
		  || (start_addr[1] & 3) == 0	/* fixnum */
		  || TypeOf(start_addr[1]) == type_BaseChar
		  || TypeOf(start_addr[1]) == type_UnboundMarker))
	      break;
	  else {
	      return FALSE;
	  }
      case type_InstancePointer:
	  if ((size_t) pointer != (size_t) start_addr + type_InstancePointer) {
	      return FALSE;
	  }
	  if (TypeOf(start_addr[0]) != type_InstanceHeader) {
	      return FALSE;
	  }
	  break;
      case type_OtherPointer:
	  if ((size_t) pointer != (size_t) start_addr + type_OtherPointer) {
	      return FALSE;
	  }
	  /* Is it plausible?  Not a cons. X should check the headers. */
	  if (Pointerp(start_addr[0]) || (start_addr[0] & 3) == 0) {
	      return FALSE;
	  }
	  switch (TypeOf(start_addr[0])) {
	    case type_UnboundMarker:
	    case type_BaseChar:
		return FALSE;

		/* Only pointed to by function pointers? */
	    case type_ClosureHeader:
	    case type_FuncallableInstanceHeader:
	    case type_ByteCodeFunction:
	    case type_ByteCodeClosure:
#ifdef type_DylanFunctionHeader
	    case type_DylanFunctionHeader:
#endif
		return FALSE;

	    case type_InstanceHeader:
		return FALSE;

		/* The valid other immediate pointer objects */
	    case type_SimpleVector:
	    case type_Ratio:
	    case type_Complex:
#ifdef type_ComplexSingleFloat
	    case type_ComplexSingleFloat:
#endif
#ifdef type_ComplexDoubleFloat
	    case type_ComplexDoubleFloat:
#endif
#ifdef type_ComplexLongFloat
	    case type_ComplexLongFloat:
#endif
#ifdef type_ComplexDoubleDoubleFloat
	    case type_ComplexDoubleDoubleFloat:
#endif
	    case type_SimpleArray:
	    case type_ComplexString:
	    case type_ComplexBitVector:
	    case type_ComplexVector:
	    case type_ComplexArray:
	    case type_ValueCellHeader:
	    case type_SymbolHeader:
	    case type_Fdefn:
	    case type_CodeHeader:
	    case type_Bignum:
	    case type_SingleFloat:
	    case type_DoubleFloat:
#ifdef type_LongFloat
	    case type_LongFloat:
#endif
#ifdef type_DoubleDoubleFloat
	    case type_DoubleDoubleFloat:
#endif
	    case type_SimpleString:
	    case type_SimpleBitVector:
	    case type_SimpleArrayUnsignedByte2:
	    case type_SimpleArrayUnsignedByte4:
	    case type_SimpleArrayUnsignedByte8:
	    case type_SimpleArrayUnsignedByte16:
	    case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
	    case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
	    case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
	    case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
	    case type_SimpleArraySignedByte32:
#endif
	    case type_SimpleArraySingleFloat:
	    case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayLongFloat
	    case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayDoubleDoubleFloat
	    case type_SimpleArrayDoubleDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	    case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	    case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
	    case type_SimpleArrayComplexLongFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleDoubleFloat
	    case type_SimpleArrayComplexDoubleDoubleFloat:
#endif
	    case type_Sap:
	    case type_WeakPointer:
	    case type_ScavengerHook:
		break;

	    default:
		return FALSE;
	  }
	  break;
      default:
	  return FALSE;
    }

    /* Looks good */
    return TRUE;
}
#endif

/*
 * Adjust large bignum and vector objects.  This will adjust the
 * allocated region if the size has shrunk, and move unboxed objects
 * into unboxed pages. The pages are not promoted here, and the
 * promoted region is not added to the new_regions; this is really
 * only designed to be called from preserve_pointer. Shouldn't fail if
 * this is missed, just may delay the moving of objects to unboxed
 * pages, and the freeing of pages.
 */
#if (defined(i386) || defined(__x86_64))
static void
maybe_adjust_large_object(lispobj * where)
{
    int first_page;
    int nwords;
    int remaining_bytes;
    int next_page;
    int bytes_freed;
    int old_bytes_used;
    int unboxed;
    int mmask, mflags;

    /* Check if it's a vector or bignum object. */
    switch (TypeOf(where[0])) {
      case type_SimpleVector:
	  unboxed = FALSE;
	  break;
      case type_Bignum:
      case type_SimpleString:
      case type_SimpleBitVector:
      case type_SimpleArrayUnsignedByte2:
      case type_SimpleArrayUnsignedByte4:
      case type_SimpleArrayUnsignedByte8:
      case type_SimpleArrayUnsignedByte16:
      case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
      case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
      case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
      case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
      case type_SimpleArraySignedByte32:
#endif
      case type_SimpleArraySingleFloat:
      case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayLongFloat
      case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayDoubleDoubleFloat
      case type_SimpleArrayDoubleDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
      case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
      case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
      case type_SimpleArrayComplexLongFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleDoubleFloat
      case type_SimpleArrayComplexDoubleDoubleFloat:
#endif
	  unboxed = TRUE;
	  break;
      default:
	  return;
    }

    /* Find its current size. */
    nwords = (sizetab[TypeOf(where[0])]) (where);

    first_page = find_page_index((void *) where);
    gc_assert(first_page >= 0);

    /*
     * Note: Any page write protection must be removed, else a later
     * scavenge_newspace may incorrectly not scavenge these pages.  This
     * would not be necessary if they are added to the new areas, but
     * lets do it for them all (they'll probably be written anyway?).
     */

    gc_assert(page_table[first_page].first_object_offset == 0);

    next_page = first_page;
    remaining_bytes = nwords * sizeof(lispobj);
    while (remaining_bytes > PAGE_SIZE) {
	gc_assert(PAGE_GENERATION(next_page) == from_space);
	gc_assert(PAGE_ALLOCATED(next_page));
	gc_assert(PAGE_LARGE_OBJECT(next_page));
	gc_assert(page_table[next_page].first_object_offset ==
		  PAGE_SIZE * (first_page - next_page));
	gc_assert(page_table[next_page].bytes_used == PAGE_SIZE);

	PAGE_FLAGS_UPDATE(next_page, PAGE_UNBOXED_MASK,
			  unboxed << PAGE_UNBOXED_SHIFT);

	/*
	 * Shouldn't be write protected at this stage. Essential that the
	 * pages aren't.
	 */
	gc_assert(!PAGE_WRITE_PROTECTED(next_page));
	remaining_bytes -= PAGE_SIZE;
	next_page++;
    }

    /*
     * Now only one page remains, but the object may have shrunk so
     * there may be more unused pages which will be freed.
     */

    /* Object may have shrunk but shouldn't have grown - check. */
    gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

    page_table[next_page].flags |= PAGE_ALLOCATED_MASK;
    PAGE_FLAGS_UPDATE(next_page, PAGE_UNBOXED_MASK,
		      unboxed << PAGE_UNBOXED_SHIFT);
    gc_assert(PAGE_UNBOXED(next_page) == PAGE_UNBOXED(first_page));

    /* Adjust the bytes_used. */
    old_bytes_used = page_table[next_page].bytes_used;
    page_table[next_page].bytes_used = remaining_bytes;

    bytes_freed = old_bytes_used - remaining_bytes;

    mmask = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK | PAGE_GENERATION_MASK;
    mflags = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK | from_space;

    /* Free any remaining pages; needs care. */
    next_page++;
    while (old_bytes_used == PAGE_SIZE &&
	   PAGE_FLAGS(next_page, mmask) == mflags &&
	   page_table[next_page].first_object_offset == PAGE_SIZE * (first_page
								     -
								     next_page))
    {
	/*
	 * Checks out OK, free the page. Don't need to bother zeroing
	 * pages as this should have been done before shrinking the
	 * object. These pages shouldn't be write protected as they should
	 * be zero filled.
	 */
	gc_assert(!PAGE_WRITE_PROTECTED(next_page));

	old_bytes_used = page_table[next_page].bytes_used;
	page_table[next_page].flags &= ~PAGE_ALLOCATED_MASK;
	page_table[next_page].bytes_used = 0;
	bytes_freed += old_bytes_used;
	next_page++;
    }

    if (gencgc_verbose && bytes_freed > 0)
	fprintf(stderr, "* adjust_large_object freed %d\n", bytes_freed);

    generations[from_space].bytes_allocated -= bytes_freed;
    bytes_allocated -= bytes_freed;

    return;
}
#endif

/*
 * Take a possible pointer to a list object and mark the page_table so
 * that it will not need changing during a GC.
 *
 * This involves locating the page it points to, then backing up to
 * the first page that has its first object start at offset 0, and
 * then marking all pages dont_move from the first until a page that
 * ends by being full, or having free gen.
 *
 * This ensures that objects spanning pages are not broken.
 *
 * It is assumed that all the page static flags have been cleared at
 * the start of a GC.
 *
 * Also assumes the current gc_alloc region has been flushed and the
 * tables updated.
 *
 * Only needed on x86 because GC is conservative there.  Not needed on
 * sparc or ppc because GC is precise, not conservative.
 */
#if (defined(i386) || defined(__x86_64))
static void
preserve_pointer(void *addr)
{
    int addr_page_index = find_page_index(addr);
    int first_page;
    int i;
    unsigned region_unboxed;

    /* Address is quite likely to have been invalid - do some checks. */
    if (addr_page_index == -1 || !PAGE_ALLOCATED(addr_page_index)
	|| page_table[addr_page_index].bytes_used == 0
	|| PAGE_GENERATION(addr_page_index) != from_space
	/* Skip if already marked dont_move */
	|| PAGE_DONT_MOVE(addr_page_index))
	return;

    region_unboxed = PAGE_UNBOXED(addr_page_index);

    /* Check the offset within the page */
    if (((size_t) addr & 0xfff) > page_table[addr_page_index].bytes_used)
	return;

    if (enable_pointer_filter && !valid_dynamic_space_pointer(addr))
	return;

    /*
     * Work backwards to find a page with a first_object_offset of 0.
     * The pages should be contiguous with all bytes used in the same
     * gen. Assumes the first_object_offset is negative or zero.
     */
    first_page = addr_page_index;
    while (page_table[first_page].first_object_offset != 0) {
	first_page--;
	/* Do some checks */
	gc_assert(page_table[first_page].bytes_used == PAGE_SIZE);
	gc_assert(PAGE_GENERATION(first_page) == from_space);
	gc_assert(PAGE_ALLOCATED(first_page));
	gc_assert(PAGE_UNBOXED(first_page) == region_unboxed);
    }

    /*
     * Adjust any large objects before promotion as they won't be copied
     * after promotion.
     */
    if (PAGE_LARGE_OBJECT(first_page)) {
	maybe_adjust_large_object((lispobj *) page_address(first_page));
	/*
	 * If a large object has shrunk then addr may now point to a free
	 * adea in which case it's ignored here. Note it gets through the
	 * valid pointer test above because the tail looks like conses.
	 */
	if (!PAGE_ALLOCATED(addr_page_index)
	    || page_table[addr_page_index].bytes_used == 0
	    /* Check the offset within the page */
	    || ((size_t) addr & 0xfff) > page_table[addr_page_index].bytes_used) {
	    fprintf(stderr,
		    "*W ignore pointer 0x%lx to freed area of large object\n",
		    (unsigned long) addr);
	    return;
	}
	/* May have moved to unboxed pages. */
	region_unboxed = PAGE_UNBOXED(first_page);
    }

    /*
     * Now work forward until the end of this contiguous area is found,
     * marking all pages as dont_move.
     */
    for (i = first_page;; i++) {
	gc_assert(PAGE_ALLOCATED(i));
	gc_assert(PAGE_UNBOXED(i) == region_unboxed);

	/* Mark the page static */
	page_table[i].flags |= PAGE_DONT_MOVE_MASK;
#if 0
	fprintf(stderr, "#%d,", i);
#endif

	/*
	 * Move the page to the new_space. XX I'd rather not do this but
	 * the GC logic is not quite able to copy with the static pages
	 * remaining in the from space. This also requires the generation
	 * bytes_allocated counters be updated.
	 */
	PAGE_FLAGS_UPDATE(i, PAGE_GENERATION_MASK, new_space);
	generations[new_space].bytes_allocated += page_table[i].bytes_used;
	generations[from_space].bytes_allocated -= page_table[i].bytes_used;

	/*
	 * Essential that the pages are not write protected as they may
	 * have pointers into the old-space which need
	 * scavenging. Shouldn't be write protected at this stage.
	 */
	gc_assert(!PAGE_WRITE_PROTECTED(i));

	/* Check if this is the last page in this contiguous block */
	if (page_table[i].bytes_used < PAGE_SIZE
	    /* Or it is PAGE_SIZE and is the last in the block */
	    || !PAGE_ALLOCATED(i + 1)
	    || page_table[i + 1].bytes_used == 0	/* Next page free */
	    || PAGE_GENERATION(i + 1) != from_space	/* Diff. gen */
	    || page_table[i + 1].first_object_offset == 0)
	    break;
    }

    /* Check that the page is now static */
    gc_assert(PAGE_DONT_MOVE(addr_page_index));

    return;
}
#endif

#ifdef CONTROL_STACKS
/* Scavenge the thread stack conservative roots. */
static void
scavenge_thread_stacks(void)
{
    lispobj thread_stacks = SymbolValue(CONTROL_STACKS);

    if (LowtagOf(thread_stacks) == type_OtherPointer) {
	struct vector *vector = (struct vector *) PTR(thread_stacks);
	int length, i;

	if (TypeOf(vector->header) != type_SimpleVector)
	    return;
	length = fixnum_value(vector->length);
	for (i = 0; i < length; i++) {
	    lispobj stack_obj = vector->data[i];

	    if (LowtagOf(stack_obj) == type_OtherPointer) {
		struct vector *stack = (struct vector *) PTR(stack_obj);
		int vector_length;

		if (TypeOf(stack->header) != type_SimpleArrayUnsignedByte32)
		    return;
		vector_length = fixnum_value(stack->length);
		if (gencgc_verbose > 1 && vector_length <= 0)
		    fprintf(stderr, "*W control stack vector length %d\n",
			    vector_length);
		if (vector_length > 0) {
		    unsigned long stack_pointer = stack->data[0];

		    if ((char *) stack_pointer < (char *) control_stack ||
			(char *) stack_pointer > (char *) control_stack_end)
			fprintf(stderr, "*E Invalid stack pointer %lx\n",
				stack_pointer);
		    if ((char *) stack_pointer > (char *) control_stack
			&& (char *) stack_pointer < (char *) control_stack_end) {
			unsigned int length =
			    ((int) control_stack_end -

			     stack_pointer) / sizeof(lispobj);
			int j;

			if (length >= vector_length)
			    fprintf(stderr,
				    "*E Invalid stack size %d >= vector length %d\n",
				    length, vector_length);
			if (gencgc_verbose > 1)
			    fprintf(stderr,
				    "Scavenging %d words of control stack %d of length %d words.\n",
				    length, i, vector_length);
			for (j = 0; j < length; j++)
			    preserve_pointer((void *) stack->data[1 + j]);
		    }
		}
	    }
	}
    }
}
#endif


/*
 * If the given page is not write protected, then scan it for pointers
 * to younger generations or the top temp. generation, if no
 * suspicious pointers are found then the page is write protected.
 *
 * Care is taken to check for pointers to the current gc_alloc region
 * if it is a younger generation or the temp. generation. This frees
 * the caller from doing a gc_alloc_update_page_tables. Actually the
 * gc_alloc_generation does not need to be checked as this is only
 * called from scavenge_generation when the gc_alloc generation is
 * younger, so it just checks if there is a pointer to the current
 * region.
 *
 * It returns 1 if the page was write protected, else 0.
 */
static int
update_page_write_prot(unsigned page)
{
    int gen = PAGE_GENERATION(page);
    int j;
    int wp_it = 1;
    void **page_addr = (void **) page_address(page);
    int num_words = page_table[page].bytes_used / sizeof(lispobj);

    /* Shouldn't be a free page. */
    gc_assert(PAGE_ALLOCATED(page));
    gc_assert(page_table[page].bytes_used != 0);

    /* Skip if it's already write protected or an unboxed page. */
    if (PAGE_WRITE_PROTECTED(page) || PAGE_UNBOXED(page))
	return 0;

    /*
     * Scan the page for pointers to younger generations or the top
     * temp. generation.
     */

    for (j = 0; j < num_words; j++) {
	char *ptr = *(page_addr + j);
	int index = find_page_index(ptr);

	/* Check that it's in the dynamic space */
	if (index != -1)
	    if (		/* Does it point to a younger or the temp. generation? */
		   (PAGE_ALLOCATED(index)
		    && page_table[index].bytes_used != 0
		    && (PAGE_GENERATION(index) < gen
			|| PAGE_GENERATION(index) == NUM_GENERATIONS))

		   /* Or does it point within a current gc_alloc region? */
		   || (boxed_region.start_addr <= ptr
		       && ptr <= boxed_region.free_pointer)
		   || (unboxed_region.start_addr <= ptr
		       && ptr <= unboxed_region.free_pointer)) {
		wp_it = 0;
		break;
	    }
    }

    if (wp_it == 1) {
	/* Write protect the page */
#if 0
	fprintf(stderr, "* WP page %d of gen %d\n", page, gen);
#endif

	os_protect((os_vm_address_t) page_addr, PAGE_SIZE,
		   OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);

	/* Note the page as protected in the page tables */
	page_table[page].flags |= PAGE_WRITE_PROTECTED_MASK;
    }

    return wp_it;
}

/*
 * Scavenge a generation.
 *
 * This will not resolve all pointers when generation is the new
 * space, as new objects may be added which are not check here - use
 * scavenge_newspace generation.
 *
 * Write protected pages should not have any pointers to the
 * from_space so do need scavenging; Thus write protected pages are
 * not always scavenged. There is some code to check that these pages
 * are not written; but to check fully the write protect pages need to
 * be scavenged by disabling the code to skip them.
 *
 * Under the current scheme when a generation is GCed the younger
 * generations will be empty. So, when a generation is being GCed it
 * is only necessary to scavenge the older generations for pointers
 * not the younger. So a page that does not have pointers to younger
 * generations does not need to be scavenged.
 *
 * The write protection can be used to note pages that don't have
 * pointers to younger pages. But pages can be written without having
 * pointers to younger generations. After the pages are scavenged here
 * they can be scanned for pointers to younger generations and if
 * there are none the page can be write protected.
 *
 * One complication is when the newspace is the top temp. generation.
 *
 * Enabling SC_GEN_CK scavenges the write protect pages and checks
 * that none were written, which they shouldn't be as they should have
 * no pointers to younger generations.  This breaks down for weak
 * pointers as the objects contain a link to the next and are written
 * if a weak pointer is scavenged. Still it's a useful check.
 */

static void
scavenge_generation(int generation)
{
    int i;
    int num_wp = 0;

#define SC_GEN_CK 0
#if SC_GEN_CK
    /* Clear the write_protected_cleared flags on all pages */
    for (i = 0; i < dynamic_space_pages; i++)
	page_table[i].flags &= ~PAGE_WRITE_PROTECTED_CLEADED_MASK;
#endif

    for (i = 0; i < last_free_page; i++) {
	if (PAGE_ALLOCATED(i) && !PAGE_UNBOXED(i)
	    && page_table[i].bytes_used != 0
	    && PAGE_GENERATION(i) == generation) {
	    int last_page;

	    /* This should be the start of a contiguous block */
	    gc_assert(page_table[i].first_object_offset == 0);

	    /*
	     * Need to find the full extent of this contiguous block in case
	     * objects span pages.
	     */

	    /*
	     * Now work forward until the end of this contiguous area is
	     * found. Small areas are preferred as there is a better chance
	     * of its pages being write protected.
	     */
	    for (last_page = i;; last_page++)
		/* Check if this is the last page in this contiguous block */
		if (page_table[last_page].bytes_used < PAGE_SIZE
		    /* Or it is PAGE_SIZE and is the last in the block */
		    || !PAGE_ALLOCATED(last_page + 1)
		    || PAGE_UNBOXED(last_page + 1)
		    || page_table[last_page + 1].bytes_used == 0
		    || PAGE_GENERATION(last_page + 1) != generation
		    || page_table[last_page + 1].first_object_offset == 0)
		    break;

	    /*
	     * Do a limited check for write_protected pages. If all pages
	     * are write_protected then no need to scavenge.
	     */
	    {
		int j, all_wp = 1;

		for (j = i; j <= last_page; j++)
		    if (!PAGE_WRITE_PROTECTED(j)) {
			all_wp = 0;
			break;
		    }
#if !SC_GEN_CK
		if (all_wp == 0)
#endif
		{
		    scavenge(page_address(i), (page_table[last_page].bytes_used
					       + PAGE_SIZE * (last_page -
							      i)) /
			     sizeof(lispobj));

		    /*
		     * Now scan the pages and write protect those that don't
		     * have pointers to younger generations.
		     */
		    if (enable_page_protection)
			for (j = i; j <= last_page; j++)
			    num_wp += update_page_write_prot(j);
		}
	    }
	    i = last_page;
	}
    }

    if (gencgc_verbose > 1 && num_wp != 0)
	fprintf(stderr, "Write protected %d pages within generation %d\n",
		num_wp, generation);

#if SC_GEN_CK
    /*
     * Check that none of the write_protected pages in this generation
     * have been written to.
     */
    for (i = 0; i < dynamic_space_pages; i++)
	if (PAGE_ALLOCATED(i)
	    && page_table[i].bytes_used != 0
	    && PAGE_GENERATION(i) == generation
	    && PAGE_WRITE_PROTECTED_CLEARED(i)) {
	    fprintf(stderr,
		    "*** scavenge_generation %d: write protected page %d written to?\n",
		    generation, i);
	    fprintf(stderr,
		    "*** page: bytes_used=%d first_object_offset=%d dont_move=%d\n",
		    page_table[i].bytes_used, page_table[i].first_object_offset,
		    PAGE_DONT_MOVE(i));
	}
#endif

}


/*
 * Scavenge a newspace generation. As it is scavenged new objects may
 * be allocated to it; these will also need to be scavenged. This
 * repeats until there are no more objects unscavenged in the newspace
 * generation.
 *
 * To help improve the efficiency, areas written are recorded by
 * gc_alloc and only these scavenged. Sometimes a little more will be
 * scavenged, but this causes no harm. An easy check is done that the
 * scavenged bytes equals the number allocated in the previous
 * scavenge.
 *
 * Write protected pages are not scanned except if they are marked
 * don't move in which case they may have been promoted and still have
 * pointers to the from space.
 *
 * Write protect pages could potentially be written by alloc however
 * to avoid having to handle re-scavenging of write_protect pages
 * gc_alloc does not write to write_protected pages.
 *
 * New areas of objects allocated are record alternatively in the two
 * new_areas arrays below.
 */
static struct new_area new_areas_1[NUM_NEW_AREAS];
static struct new_area new_areas_2[NUM_NEW_AREAS];

/*
 * Do one full scan of the new space generation. This is not enough to
 * complete the job as new objects may be added to the generation in
 * the process which are not scavenged.
 */
static void
scavenge_newspace_generation_one_scan(int generation)
{
    int i;

#if 0
    fprintf(stderr, "Starting one full scan of newspace generation %d\n",
	    generation);
#endif

    for (i = 0; i < last_free_page; i++) {
	if (PAGE_ALLOCATED(i) && !PAGE_UNBOXED(i)
	    && page_table[i].bytes_used != 0
	    && PAGE_GENERATION(i) == generation && (!PAGE_WRITE_PROTECTED(i)
						    /* This may be redundant as WP is now cleared before promotion. */
						    || PAGE_DONT_MOVE(i))) {
	    int last_page;

	    /* The scavenge will start at the first_object_offset of page i */

	    /*
	     * Need to find the full extent of this contiguous block in case
	     * objects span pages.
	     */

	    /*
	     * Now work forward until the end of this contiguous area is
	     * found. Small areas are preferred as there is a better chance
	     * of its pages being write protected.
	     */
	    for (last_page = i;; last_page++)
		/* Check if this is the last page in this contiguous block */
		if (page_table[last_page].bytes_used < PAGE_SIZE
		    /* Or it is PAGE_SIZE and is the last in the block */
		    || !PAGE_ALLOCATED(last_page + 1)
		    || PAGE_UNBOXED(last_page + 1)
		    || page_table[last_page + 1].bytes_used == 0
		    || PAGE_GENERATION(last_page + 1) != generation
		    || page_table[last_page + 1].first_object_offset == 0)
		    break;

	    /*
	     * Do a limited check for write_protected pages. If all pages
	     * are write_protected then no need to scavenge. Except if the
	     * pages are marked dont_move.
	     */
	    {
		int j, all_wp = 1;

		for (j = i; j <= last_page; j++)
		    if (!PAGE_WRITE_PROTECTED(j) || PAGE_DONT_MOVE(j)) {
			all_wp = 0;
			break;
		    }
#if !SC_NS_GEN_CK
		if (all_wp == 0)
#endif
		{
		    int size;

		    /* Calc. the size */
		    if (last_page == i)
			size = (page_table[last_page].bytes_used
				-
				page_table[i].first_object_offset) /
			    sizeof(lispobj);
		    else
			size =
			    (page_table[last_page].bytes_used +
			     PAGE_SIZE * (last_page - i) -
			     page_table[i].first_object_offset) /
			    sizeof(lispobj);

		    {
#if SC_NS_GEN_CK
			int a1 = bytes_allocated;
#endif
#if 0
			fprintf(stderr, "scavenge(%x,%d)\n",
				page_address(i) +
				page_table[i].first_object_offset, size);
#endif

			new_areas_ignore_page = last_page;

			scavenge(
				 (page_address(i) +
				  page_table[i].first_object_offset), size);

#if SC_NS_GEN_CK
			/* Flush the alloc regions updating the tables. */
			gc_alloc_update_page_tables(0, &boxed_region);
			gc_alloc_update_page_tables(1, &unboxed_region);

			if (all_wp != 0 && a1 != bytes_allocated) {
			    fprintf(stderr,
				    "*** scav.new.gen. alloc'ed over %d to %d\n",
				    i, last_page);
			    fprintf(stderr,
				    "*** page: bytes_used=%d first_object_offset=%d dont_move=%d wp=%d wpc=%d\n",
				    page_table[i].bytes_used,
				    page_table[i].first_object_offset,
				    PAGE_DONT_MOVE(i), PAGE_WRITE_PROTECTED(i),
				    PAGE_PROTECTED_CLEARED(i));
			}
#endif
		    }
		}
	    }

	    i = last_page;
	}
    }
#if 0
    fprintf(stderr, "Finished one full scan of newspace generation %d\n",
	    generation);
#endif
}

/* Scan all weak objects and reset weak object lists */
static void
scan_weak_objects()
{
    scan_weak_tables();
    scan_weak_pointers();

    /* Re-initialise the weak pointer and weak tables lists. */
    weak_pointers = NULL;
    weak_hash_tables = NIL;
}

/* Do a complete scavenge of the newspace generation */
static void
scavenge_newspace_generation(int generation)
{
    int i;

    /* The new_areas array currently being written to by gc_alloc */
    struct new_area (*current_new_areas)[] = &new_areas_1;
    int current_new_areas_index;

    /* The new_areas created but the previous scavenge cycle */
    struct new_area (*previous_new_areas)[] = NULL;
    int previous_new_areas_index;

#if 0
    fprintf(stderr, "Start scavenge_newspace_generation %d\n", generation);
#endif

#define SC_NS_GEN_CK 0
#if SC_NS_GEN_CK
    /* Clear the write_protected_cleared flags on all pages */
    for (i = 0; i < dynamic_space_pages; i++)
	page_table[i].flags &= ~PAGE_WRITE_PROTECTED_CLEARED;
#endif

    /* Flush the current regions updating the tables. */
    gc_alloc_update_page_tables(0, &boxed_region);
    gc_alloc_update_page_tables(1, &unboxed_region);

    /* Turn on the recording of new areas by gc_alloc. */
    new_areas = current_new_areas;
    new_areas_index = 0;

    /*
     * Don't need to record new areas that get scavenged anyway during
     * scavenge_newspace_generation_one_scan.
     */
    record_new_objects = 1;

    /* Start with a full scavenge */
    scavenge_newspace_generation_one_scan(generation);

    /*
     * XXX: Do we need to scan weak tables here, before the region is
     * updated?  We do it in the code below for other cases before the
     * regions are updated, so it seems to make sense to do it here as
     * well.
     */
    scav_weak_tables();

    
    /* Record all new areas now. */
    record_new_objects = 2;

    /* Flush the current regions updating the tables. */
    gc_alloc_update_page_tables(0, &boxed_region);
    gc_alloc_update_page_tables(1, &unboxed_region);

    /* Grab new_areas_index */
    current_new_areas_index = new_areas_index;

#if 0
    fprintf(stderr, "First scan finished; current_new_areas_index=%d\n",
	    current_new_areas_index);
    if (current_new_areas_index > 0) {
        fprintf(stderr, "Start rescans\n");
    }
#endif

    while (current_new_areas_index > 0) {
	/* Move the current to the previous new areas */
	previous_new_areas = current_new_areas;
	previous_new_areas_index = current_new_areas_index;

	/*
	 * Scavenge all the areas in previous new areas. Any new areas
	 * allocated are saved in current_new_areas.
	 */

	/*
	 * Allocate an array for current_new_areas; alternating between
	 * new_areas_1 and 2.
	 */
	if (previous_new_areas == &new_areas_1)
	    current_new_areas = &new_areas_2;
	else
	    current_new_areas = &new_areas_1;

	/* Setup for gc_alloc */
	new_areas = current_new_areas;
	new_areas_index = 0;

	/* Check if previous_new_areas had overflowed */
	if (previous_new_areas_index >= NUM_NEW_AREAS) {
	    /*
	     * New areas of objects allocated have been lost so need to do a
	     * full scan to be sure! If this becomes a problem try
	     * increasing NUM_NEW_AREAS.
	     */
	    if (gencgc_verbose)
		fprintf(stderr, "** new_areas overflow, doing full scavenge\n");

	    /*
	     * Don't need to record new areas that get scavenge anyway
	     * during scavenge_newspace_generation_one_scan.
	     */
	    record_new_objects = 1;

#if 0
            fprintf(stderr, " Rescan generation %d\n", generation);
#endif            
	    scavenge_newspace_generation_one_scan(generation);

            /*
             * Not sure this call is needed, but I (rtoy) am putting
             * this here anyway on the assumption that since we do it
             * below after scavenging some stuff, we should do it here
             * also because scavenge_newspace_generation_one_scan
             * scavenges stuff too.
             */
            
            scav_weak_tables();

	    /* Record all new areas now. */
	    record_new_objects = 2;

	    /* Flush the current regions updating the tables. */
	    gc_alloc_update_page_tables(0, &boxed_region);
	    gc_alloc_update_page_tables(1, &unboxed_region);
	} else {
	    /* Work through previous_new_areas */
	    for (i = 0; i < previous_new_areas_index; i++) {
		int page = (*previous_new_areas)[i].page;
		int offset = (*previous_new_areas)[i].offset;
		int size = (*previous_new_areas)[i].size / sizeof(lispobj);

		gc_assert((*previous_new_areas)[i].size % 4 == 0);

#if 0
		fprintf(stderr, "*S page %d offset %d size %d\n", page, offset,
			size * sizeof(lispobj));
                fprintf(stderr, "  scavenge(%p, %d)\n", page_address(page) + offset, size);
#endif
		scavenge(page_address(page) + offset, size);
	    }

            /*
             * I (rtoy) am not sure this is 100% correct.  But if we
             * don't scan the weak tables here (or somewhere near
             * here, perhaps), we get problems like live weak pointers
             * that haven't been transported out of oldspace.  Then
             * anything referring to this pointer causes a crash when
             * GC happens later on.
             *
             * This fixes a bug with weak hash tables, reported by
             * Lynn Quam, cmucl-imp, 2006-07-04.
             */ 
            scav_weak_tables();
            
	    /* Flush the current regions updating the tables. */
	    gc_alloc_update_page_tables(0, &boxed_region);
	    gc_alloc_update_page_tables(1, &unboxed_region);
	}

	/* Grab new_areas_index */
	current_new_areas_index = new_areas_index;

#if 0
	fprintf(stderr, "Re-scan finished; current_new_areas_index=%d\n",
		current_new_areas_index);
#endif
    }

#if 0
    fprintf(stderr, "All rescans finished\n");
#endif

    /* Turn off recording of areas allocated by gc_alloc */
    record_new_objects = 0;

#if SC_NS_GEN_CK
    /*
     * Check that none of the write_protected pages in this generation
     * have been written to.
     */
    for (i = 0; i < dynamic_space_pages; i++)
	if (PAGE_ALLOCATED(i)
	    && page_table[i].bytes_used != 0
	    && PAGE_GENERATION(i) == generation
	    && PAGE_WRITE_PROTECTED_CLEARED(i) && !PAGE_DONT_MOVE(i))
	    fprintf(stderr,
		    "*** scav.new.gen. %d: write protected page %d written to? dont_move=%d\n",
		    generation, i, PAGE_DONT_MOVE(i));
#endif
#if 0
    fprintf(stderr, "Finished scavenge_newspace_generation %d\n", generation);
#endif
}



/*
 * Un-write-protect all the pages in from_space. This is done at the
 * start of a GC else there may be many page faults while scavenging
 * the newspace (I've seen drive the system time to 99%). These pages
 * would need to be unprotected anyway before unmapping in
 * free_oldspace; not sure what effect this has on paging?.
 */
static void
unprotect_oldspace(void)
{
    int i;

    for (i = 0; i < last_free_page; i++)
	if (PAGE_ALLOCATED(i)
	    && page_table[i].bytes_used != 0
	    && PAGE_GENERATION(i) == from_space) {
	    void *page_start;

	    page_start = (void *) page_address(i);

	    /*
	     * Remove any write protection.  Should be able to rely on the
	     * WP flag to avoid redundant calls.
	     */
	    if (PAGE_WRITE_PROTECTED(i)) {
		os_protect((os_vm_address_t) page_start, PAGE_SIZE,
			   OS_VM_PROT_ALL);
		page_table[i].flags &= ~PAGE_WRITE_PROTECTED_MASK;
	    }
	}
}

/*
 * Work through all the pages and free any in from_space.  This
 * assumes that all objects have been copied or promoted to an older
 * generation. Bytes_allocated and the generation bytes_allocated
 * counter are updated.  The number of bytes freed is returned.
 */
static int
free_oldspace(void)
{
    int bytes_freed = 0;
    int first_page, last_page;

    first_page = 0;

    do {
	/* Find a first page for the next region of pages. */
	while (first_page < last_free_page && (!PAGE_ALLOCATED(first_page)
					       || page_table[first_page].
					       bytes_used == 0
					       || PAGE_GENERATION(first_page) !=
					       from_space)) first_page++;

	if (first_page >= last_free_page)
	    break;

	/* Find the last page of this region. */
	last_page = first_page;

	do {
	    /* Free the page */
	    bytes_freed += page_table[last_page].bytes_used;
	    generations[PAGE_GENERATION(last_page)].bytes_allocated -=
		page_table[last_page].bytes_used;
	    page_table[last_page].flags &= ~PAGE_ALLOCATED_MASK;
	    page_table[last_page].bytes_used = 0;

	    /*
	     * Remove any write protection.  Should be able to rely on the
	     * WP flag to avoid redundant calls.
	     */
	    {
		void *page_start = (void *) page_address(last_page);

		if (PAGE_WRITE_PROTECTED(last_page)) {
		    os_protect((os_vm_address_t) page_start, PAGE_SIZE,
			       OS_VM_PROT_ALL);
		    page_table[last_page].flags &= ~PAGE_WRITE_PROTECTED_MASK;
		}
	    }
	    last_page++;
	}
	while (last_page < last_free_page && PAGE_ALLOCATED(last_page)
	       && page_table[last_page].bytes_used != 0
	       && PAGE_GENERATION(last_page) == from_space);

	/* Zero pages from first_page to (last_page - 1) */
	if (gencgc_unmap_zero) {
	    char *page_start, *addr;

	    page_start = page_address(first_page);

	    os_invalidate((os_vm_address_t) page_start,
			  PAGE_SIZE * (last_page - first_page));
	    addr =
		(char *) os_validate((os_vm_address_t) page_start,
				     PAGE_SIZE * (last_page - first_page));
	    if (addr == NULL || addr != page_start)
		fprintf(stderr, "gc_zero: page moved, 0x%08lx ==> 0x%08lx!\n",
			(unsigned long) page_start, (unsigned long) addr);
	} else {
	    int *page_start;

	    page_start = (int *) page_address(first_page);
	    memset(page_start, 0, PAGE_SIZE * (last_page - first_page));
	}

	first_page = last_page;
    }
    while (first_page < last_free_page);

    bytes_allocated -= bytes_freed;
    return bytes_freed;
}



/* Print out some information about a pointer at the given address. */
static void
print_ptr(lispobj * addr)
{
    /* If addr is in the dynamic space then print out the page information. */
    int pi1 = find_page_index((void *) addr);

    if (pi1 != -1)
	fprintf(stderr,
		"  %lx: page %d  alloc %d unboxed %d gen %d  bytes_used %d  offset %d  dont_move %d\n",
		(unsigned long) addr, pi1, PAGE_ALLOCATED(pi1),
		PAGE_UNBOXED(pi1), PAGE_GENERATION(pi1),
		page_table[pi1].bytes_used, page_table[pi1].first_object_offset,
		PAGE_DONT_MOVE(pi1));
    fprintf(stderr, "  %lx %lx %lx %lx (%lx) %lx %lx %lx %lx\n", *(addr - 4),
	    *(addr - 3), *(addr - 2), *(addr - 1), *(addr - 0), *(addr + 1),
	    *(addr + 2), *(addr + 3), *(addr + 4));
}

#if defined(sparc)
extern char closure_tramp;
#elif defined(DARWIN)
extern char closure_tramp;
extern char undefined_tramp;
#else
extern int undefined_tramp;
#endif

static void
verify_space(lispobj * start, size_t words)
{
    int dynamic_space = (find_page_index((void *) start) != -1);
    unsigned long readonly_space =
	(READ_ONLY_SPACE_START <= (unsigned long) start
	 && (unsigned long) start < SymbolValue(READ_ONLY_SPACE_FREE_POINTER));

    while (words > 0) {
	size_t count = 1;
	lispobj thing = *(lispobj *) start;

	if (Pointerp(thing)) {
	    int page_index = find_page_index((void *) thing);
	    int to_readonly_space = (READ_ONLY_SPACE_START <= thing &&
				     thing <

				     SymbolValue(READ_ONLY_SPACE_FREE_POINTER));
	    unsigned long to_static_space =
		((unsigned long) static_space <= thing

		 && thing < SymbolValue(STATIC_SPACE_FREE_POINTER));

	    /* Does it point to the dynamic space? */
	    if (page_index != -1) {
		/*
		 * If it's within the dynamic space it should point to a used
		 * page.  X Could check the offset too.
		 */
		if (PAGE_ALLOCATED(page_index)
		    && page_table[page_index].bytes_used == 0) {
		    fprintf(stderr, "*** Ptr %lx @ %lx sees free page.\n",
			    (unsigned long) thing, (unsigned long) start);
		    print_ptr(start);
		}

		/* Check that it doesn't point to a forwarding pointer! */
		if (*((lispobj *) PTR(thing)) == 0x01) {
		    fprintf(stderr, "*** Ptr %lx @ %lx sees forwarding ptr.\n",
			    (unsigned long) thing, (unsigned long) start);
		    print_ptr(start);
		}

		/*
		 * Check that its not in the RO space as it would then be a
		 * pointer from the RO to the dynamic space.
		 */
		if (readonly_space) {
		    fprintf(stderr,
			    "*** Ptr to dynamic space %lx, from RO space %lx\n",
			    (unsigned long) thing, (unsigned long) start);
		    print_ptr(start);
		}

		/*
		 * Does it point to a plausible object? This check slows it
		 * down a lot.
		 */
#if 0
		if (!valid_dynamic_space_pointer((lispobj *) thing)) {
		    fprintf(stderr, "*** Ptr %x to invalid object %x\n", thing,
			    start);
		    print_ptr(start);
		}
#endif
	    } else {
		/* Verify that it points to another valid space */
		if (!to_readonly_space && !to_static_space &&
#if defined(sparc)
		    thing != (int) &closure_tramp
#elif defined(DARWIN)
		    !((thing == (int) &closure_tramp) ||
		      (thing == (int) &undefined_tramp))
#else
		    thing != (int) &undefined_tramp
#endif
		    ) {
		    fprintf(stderr,
			    "*** Ptr %lx @ %lx sees Junk (%s = %lx)\n",
			    (unsigned long) thing, (unsigned long) start,
#if defined(sparc)
			    "closure_tramp",
			    (unsigned long) &closure_tramp
#else
			    "undefined_tramp",
			    (unsigned long) &undefined_tramp
#endif
			);
		    print_ptr(start);
		}
	    }

	} else if (thing & 0x3)	/* Skip fixnums */
	    switch (TypeOf(*start)) {
		  /* Boxed objects. */
	      case type_SimpleVector:
	      case type_Ratio:
	      case type_Complex:
	      case type_SimpleArray:
	      case type_ComplexString:
	      case type_ComplexBitVector:
	      case type_ComplexVector:
	      case type_ComplexArray:
	      case type_ClosureHeader:
	      case type_FuncallableInstanceHeader:
	      case type_ByteCodeFunction:
	      case type_ByteCodeClosure:
#ifdef type_DylanFunctionHeader
	      case type_DylanFunctionHeader:
#endif
	      case type_ValueCellHeader:
	      case type_SymbolHeader:
	      case type_BaseChar:
	      case type_UnboundMarker:
	      case type_InstanceHeader:
	      case type_Fdefn:
	      case type_ScavengerHook:
		  count = 1;
		  break;

	      case type_CodeHeader:
		  {
		      lispobj object = *start;
		      struct code *code;
		      int nheader_words, ncode_words, nwords;
		      lispobj fheaderl;
		      struct function *fheaderp;

		      code = (struct code *) start;

		      /* Check that it's not in the dynamic space. */
		      if (dynamic_space
			  /*
			   * It's ok if it's byte compiled code. The trace table
			   * offset will be a fixnum if it's x86 compiled code - check.
			   */
			  && !(code->trace_table_offset & 0x3)
			  /* Only when enabled */
			  && verify_dynamic_code_check)
			  fprintf(stderr,
				  "*** Code object at %lx in the dynamic space\n",
				  (unsigned long) start);

		      ncode_words = fixnum_value(code->code_size);
		      nheader_words = HeaderValue(object);
		      nwords = ncode_words + nheader_words;
		      nwords = CEILING(nwords, 2);
		      /* Scavenge the boxed section of the code data block */
		      verify_space(start + 1, nheader_words - 1);

		      /*
		       * Scavenge the boxed section of each function object in
		       * the code data block.
		       */
		      fheaderl = code->entry_points;
		      while (fheaderl != NIL) {
			  fheaderp = (struct function *) PTR(fheaderl);
			  gc_assert(TypeOf(fheaderp->header) ==
				    type_FunctionHeader);
			  verify_space(&fheaderp->name, 1);
			  verify_space(&fheaderp->arglist, 1);
			  verify_space(&fheaderp->type, 1);
			  fheaderl = fheaderp->next;
		      }
		      count = nwords;
		      break;
		  }

		  /* Unboxed objects */
	      case type_Bignum:
	      case type_SingleFloat:
	      case type_DoubleFloat:
#ifdef type_ComplexLongFloat
	      case type_LongFloat:
#endif
#ifdef type_DoubleDoubleFloat
	      case type_DoubleDoubleFloat:
#endif
#ifdef type_ComplexSingleFloat
	      case type_ComplexSingleFloat:
#endif
#ifdef type_ComplexDoubleFloat
	      case type_ComplexDoubleFloat:
#endif
#ifdef type_ComplexLongFloat
	      case type_ComplexLongFloat:
#endif
#ifdef type_ComplexDoubleDoubleFloat
	      case type_ComplexDoubleDoubleFloat:
#endif
	      case type_SimpleString:
	      case type_SimpleBitVector:
	      case type_SimpleArrayUnsignedByte2:
	      case type_SimpleArrayUnsignedByte4:
	      case type_SimpleArrayUnsignedByte8:
	      case type_SimpleArrayUnsignedByte16:
	      case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
	      case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
	      case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
	      case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
	      case type_SimpleArraySignedByte32:
#endif
	      case type_SimpleArraySingleFloat:
	      case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayDoubleDoubleFloat
	      case type_SimpleArrayDoubleDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
	      case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	      case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	      case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
	      case type_SimpleArrayComplexLongFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleDoubleFloat
	      case type_SimpleArrayComplexDoubleDoubleFloat:
#endif
	      case type_Sap:
	      case type_WeakPointer:
		  count = (sizetab[TypeOf(*start)]) (start);
		  break;

	      default:
		  gc_abort();
	    }
	start += count;
	words -= count;
    }
}

static void
verify_gc(void)
{
    int read_only_space_size =
	(lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER)

	- (lispobj *) READ_ONLY_SPACE_START;
    int static_space_size = (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER)
	- (lispobj *) static_space;
    int binding_stack_size = (lispobj *) get_binding_stack_pointer()
	- (lispobj *) BINDING_STACK_START;

    verify_space((lispobj *) READ_ONLY_SPACE_START, read_only_space_size);
    verify_space((lispobj *) static_space, static_space_size);
    verify_space((lispobj *) BINDING_STACK_START, binding_stack_size);
    verify_space((lispobj *) & scavenger_hooks, 1);
}

static void
verify_generation(int generation)
{
    int i;

    for (i = 0; i < last_free_page; i++) {
	if (PAGE_ALLOCATED(i)
	    && page_table[i].bytes_used != 0
	    && PAGE_GENERATION(i) == generation) {
	    int last_page;
	    int region_unboxed = PAGE_UNBOXED(i);

	    /* This should be the start of a contiguous block */
	    gc_assert(page_table[i].first_object_offset == 0);

	    /*
	     * Need to find the full extent of this contiguous block in case
	     * objects span pages.
	     */

	    /*
	     * Now work forward until the end of this contiguous area is
	     * found.
	     */
	    for (last_page = i;; last_page++)
		/* Check if this is the last page in this contiguous block */
		if (page_table[last_page].bytes_used < PAGE_SIZE
		    /* Or it is PAGE_SIZE and is the last in the block */
		    || !PAGE_ALLOCATED(last_page + 1)
		    || PAGE_UNBOXED(last_page + 1) != region_unboxed
		    || page_table[last_page + 1].bytes_used == 0
		    || PAGE_GENERATION(last_page + 1) != generation
		    || page_table[last_page + 1].first_object_offset == 0)
		    break;

	    verify_space((lispobj *) page_address(i),
			 (page_table[last_page].bytes_used +
			  PAGE_SIZE * (last_page - i)) / sizeof(lispobj));
	    i = last_page;
	}
    }
}

/* Check the all the free space is zero filled. */
static void
verify_zero_fill(void)
{
    int page;

    for (page = 0; page < last_free_page; page++) {
	if (!PAGE_ALLOCATED(page)) {
	    /* The whole page should be zero filled. */
	    int *start_addr = (int *) page_address(page);
	    int size = 1024;
	    int i;

	    for (i = 0; i < size; i++)
		if (start_addr[i] != 0)
		    fprintf(stderr, "** free page not zero @ %lx\n",
			    (unsigned long) (start_addr + i));
	} else {
	    int free_bytes = PAGE_SIZE - page_table[page].bytes_used;

	    if (free_bytes > 0) {
		unsigned long *start_addr =
		    (unsigned long *) ((unsigned long) page_address(page)
				       + page_table[page].bytes_used);
		int size = free_bytes / sizeof(lispobj);
		int i;

		for (i = 0; i < size; i++)
		    if (start_addr[i] != 0)
			fprintf(stderr, "** free region not zero @ %lx\n",
				(unsigned long) (start_addr + i));
	    }
	}
    }
}

/* External entry point for verify_zero_fill */
void
gencgc_verify_zero_fill(void)
{
    /* Flush the alloc regions updating the tables. */

    boxed_region.free_pointer = (void *) get_current_region_free();
    gc_alloc_update_page_tables(0, &boxed_region);
    gc_alloc_update_page_tables(1, &unboxed_region);
    fprintf(stderr, "* Verifying zero fill\n");
    verify_zero_fill();
    set_current_region_free((lispobj) boxed_region.free_pointer);
    set_current_region_end((lispobj) boxed_region.end_addr);
}

static void
verify_dynamic_space(void)
{
    int i;

    for (i = 0; i < NUM_GENERATIONS; i++)
	verify_generation(i);

    if (gencgc_enable_verify_zero_fill)
	verify_zero_fill();
}



/*
 * Write protect all the dynamic boxed pages in the given
 * generation.
 */
static void
write_protect_generation_pages(int generation)
{
    int i;

    gc_assert(generation < NUM_GENERATIONS);

    for (i = 0; i < last_free_page; i++)
	if (PAGE_ALLOCATED(i) && !PAGE_UNBOXED(i)
	    && page_table[i].bytes_used != 0
	    && PAGE_GENERATION(i) == generation) {
	    void *page_start;

	    page_start = (void *) page_address(i);

	    os_protect((os_vm_address_t) page_start, PAGE_SIZE,
		       OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);

	    /* Note the page as protected in the page tables */
	    page_table[i].flags |= PAGE_WRITE_PROTECTED_MASK;
	}

    if (gencgc_verbose > 1)
	fprintf(stderr, "Write protected %d of %d pages in generation %d.\n",
		count_write_protect_generation_pages(generation),
		count_generation_pages(generation), generation);
}


static void
scavenge_interrupt_handlers(void)
{
    int i;

#ifdef PRINTNOISE
    printf("Scavenging interrupt handlers (%d bytes) ...\n",
	   sizeof(interrupt_handlers));
#endif

    for (i = 0; i < NSIG; i++) {
	union interrupt_handler handler = interrupt_handlers[i];

	if (handler.c != (void (*)(HANDLER_ARGS)) SIG_IGN
	    && handler.c != (void (*)(HANDLER_ARGS)) SIG_DFL)
	    scavenge((interrupt_handlers + i), 1);
    }
}

#if !(defined(i386) || defined(__x86_64))
static void
scavenge_control_stack()
{
    unsigned long control_stack_size;

    control_stack_size = current_control_stack_pointer - control_stack;

#ifdef PRINTNOISE
    printf("Scavenging the control stack (%d bytes) ...\n",
	   control_stack_size * sizeof(lispobj));
#endif

    scavenge(control_stack, control_stack_size);

#ifdef PRINTNOISE
    printf("Done scavenging the control stack.\n");
#endif
}
#endif

/*
 * Garbage collect a generation. If raise is 0 the remains of the
 * generation are not raised to the next generation.
 */
static void
garbage_collect_generation(int generation, int raise)
{
    unsigned long i;
    unsigned long read_only_space_size, static_space_size;

#ifdef GC_ASSERTIONS
#if defined(i386) || defined(__x86_64)
    invalid_stack_start = (void *) CONTROL_STACK_START;
    invalid_stack_end = (void *) &raise;
#else /* not i386 */
    invalid_stack_start = (void *) &raise;
    invalid_stack_end = (void *) CONTROL_STACK_END;
#endif /* not i386 */
#endif /* GC_ASSERTIONS */

    gc_assert(generation <= NUM_GENERATIONS - 1);

    /* The oldest generation can't be raised. */
    gc_assert(generation != NUM_GENERATIONS - 1 || raise == 0);

    /* Initialise the weak pointer list. */
    weak_pointers = NULL;
    weak_hash_tables = NIL;

    /*
     * When a generation is not being raised it is transported to a
     * temporary generation (NUM_GENERATIONS), and lowered when
     * done. Setup this new generation. There should be no pages
     * allocated to it yet.
     */
    if (!raise)
	gc_assert(generations[NUM_GENERATIONS].bytes_allocated == 0);

    /* Set the global src and dest. generations */
    from_space = generation;
    if (raise)
	new_space = generation + 1;
    else
	new_space = NUM_GENERATIONS;

    /*
     * Change to a new space for allocation, reseting the alloc_start_page.
     */

    gc_alloc_generation = new_space;
    generations[new_space].alloc_start_page = 0;
    generations[new_space].alloc_unboxed_start_page = 0;
    generations[new_space].alloc_large_start_page = 0;
    generations[new_space].alloc_large_unboxed_start_page = 0;

    /*
     * Before any pointers are preserved, the dont_move flags on the
     * pages need to be cleared.
     */
    for (i = 0; i < last_free_page; i++)
	page_table[i].flags &= ~PAGE_DONT_MOVE_MASK;

    /*
     * Un-write-protect the old-space pages. This is essential for the
     * promoted pages as they may contain pointers into the old-space
     * which need to be scavenged. It also helps avoid unnecessary page
     * faults as forwarding pointer are written into them. They need to
     * be un-protected anyway before unmapping later.
     */
    unprotect_oldspace();

#if defined(i386) || defined(__x86_64)
    /* Scavenge the stacks conservative roots. */
    {
	lispobj **ptr;

	for (ptr = (lispobj **) CONTROL_STACK_END - 1;
	     ptr > (lispobj **) & raise; ptr--)
	    preserve_pointer(*ptr);
    }
#endif

#ifdef CONTROL_STACKS
    scavenge_thread_stacks();
#endif

    if (gencgc_verbose > 1) {
	int num_dont_move_pages = count_dont_move_pages();

	fprintf(stderr,
		"Non-movable pages due to conservative pointers = %d, %d bytes\n",
		num_dont_move_pages, PAGE_SIZE * num_dont_move_pages);
#if !(defined(i386) || defined(__x86_64))
	/*
	 * There shouldn't be any non-movable pages because we don't have
	 * any conservative pointers!
	 */
	gc_assert(num_dont_move_pages == 0);
#endif
    }

    /* Scavenge all the rest of the roots. */

    /*
     * Scavenge the Lisp functions of the interrupt handlers, taking
     * care to avoid SIG_DFL, SIG_IGN.
     */

#if !(defined(i386) || defined(__x86_64))
    /*
     * If not x86, we need to scavenge the interrupt context(s) and the
     * control stack.
     */
    scavenge_interrupt_contexts();
    scavenge_control_stack();
#endif

    scavenge_interrupt_handlers();

#ifdef PRINTNOISE
    printf("Scavenging the binding stack (%d bytes) ...\n",
	   ((lispobj *) get_binding_stack_pointer() -
	    binding_stack) * sizeof(lispobj));
#endif
    /* Scavenge the binding stack. */
    scavenge(binding_stack,
	     (lispobj *) get_binding_stack_pointer() - binding_stack);

#ifdef PRINTNOISE
    printf("Done scavenging the binding stack.\n");
#endif
    /*
     * Scavenge the scavenge_hooks in case this refers to a hook added
     * in a prior generation GC. From here on the scavenger_hook will
     * only be updated with hooks already scavenged so this only needs
     * doing here.
     */

#ifdef PRINTNOISE
    printf("Scavenging the scavenger hooks ...\n");
#endif
    scavenge(&scavenger_hooks, 1);
#ifdef PRINTNOISE
    printf("Done scavenging the scavenger hooks.\n");
#endif

    if (SymbolValue(SCAVENGE_READ_ONLY_SPACE) != NIL) {
	read_only_space_size =
	    (lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER) -
	    read_only_space;
	fprintf(stderr, "Scavenge read only space: %ld bytes\n",
		read_only_space_size * sizeof(lispobj));
	scavenge(read_only_space, read_only_space_size);
    }

    static_space_size = (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER)
	- static_space;
    if (gencgc_verbose > 1)
	fprintf(stderr, "Scavenge static space: %ld bytes\n",
		static_space_size * sizeof(lispobj));
    scavenge(static_space, static_space_size);

    /*
     * All generations but the generation being GCed need to be
     * scavenged. The new_space generation needs special handling as
     * objects may be moved in - it is handle separately below.
     */
    for (i = 0; i < NUM_GENERATIONS; i++)
	if (i != generation && i != new_space)
	    scavenge_generation(i);

    /*
     * Finally scavenge the new_space generation.  Keep going until no
     * more objects are moved into the new generation.
     */
    scavenge_newspace_generation(new_space);

    /* I think we should do this *before* the rescan check */
    scan_weak_objects();

#define RESCAN_CHECK 0
#if RESCAN_CHECK
    /*
     * As a check re-scavenge the newspace once; no new objects should
     * be found.
     */
    {
	int old_bytes_allocated = bytes_allocated;
	int bytes_allocated_diff;

	/* Start with a full scavenge */
	scavenge_newspace_generation_one_scan(new_space);

	scavenge(&scavenger_hooks, 1);

	/* Flush the current regions, updating the tables. */
	gc_alloc_update_page_tables(0, &boxed_region);
	gc_alloc_update_page_tables(1, &unboxed_region);

	bytes_allocated_diff = bytes_allocated - old_bytes_allocated;

	if (bytes_allocated_diff != 0)
	    fprintf(stderr,
		    "*** rescan of new_space allocated %d more bytes? (%ld vs %ld)\n",
		    bytes_allocated_diff, old_bytes_allocated, bytes_allocated);
    }
#endif

    /* Flush the current regions, updating the tables. */
    gc_alloc_update_page_tables(0, &boxed_region);
    gc_alloc_update_page_tables(1, &unboxed_region);

    /* Free the pages in oldspace, but not those marked dont_move. */
    free_oldspace();

    /*
     * If the GC is not raising the age then lower the generation back
     * to its normal generation number.
     */
    if (!raise) {
	for (i = 0; i < last_free_page; i++)
	    if (page_table[i].bytes_used != 0
		&& PAGE_GENERATION(i) == NUM_GENERATIONS)
		    PAGE_FLAGS_UPDATE(i, PAGE_GENERATION_MASK, generation);
	gc_assert(generations[generation].bytes_allocated == 0);
	generations[generation].bytes_allocated =
	    generations[NUM_GENERATIONS].bytes_allocated;
	generations[NUM_GENERATIONS].bytes_allocated = 0;
    }

    /* Reset the alloc_start_page for generation. */
    generations[generation].alloc_start_page = 0;
    generations[generation].alloc_unboxed_start_page = 0;
    generations[generation].alloc_large_start_page = 0;
    generations[generation].alloc_large_unboxed_start_page = 0;

    if (generation >= verify_gens) {
	if (gencgc_verbose)
	    fprintf(stderr, "Checking\n");
	verify_gc();
	verify_dynamic_space();
    }

    /* Set the new gc trigger for the GCed generation */
    generations[generation].gc_trigger =
	generations[generation].bytes_allocated +
	generations[generation].bytes_consed_between_gc;


    /* If the generation was raised clear num_gc */
    if (raise)
	generations[generation].num_gc = 0;
    else
	/* Else increase it. */
	generations[generation].num_gc++;
}

/* Update last_free_page then ALLOCATION_POINTER */
void
update_dynamic_space_free_pointer(void)
{
    int last_page = -1;
    int i;

    for (i = 0; i < dynamic_space_pages; i++)
	if (PAGE_ALLOCATED(i) && page_table[i].bytes_used != 0)
	    last_page = i;

    last_free_page = last_page + 1;

    set_alloc_pointer((lispobj)
		      ((char *) heap_base + PAGE_SIZE * last_free_page));
}


/*
 * GC all generations below last_gen, raising their objects to the
 * next generation until all generations below last_gen are empty.
 * Then if last_gen is due for a GC then GC it. In the special case
 * that last_gen==NUM_GENERATIONS, the last generation is always
 * GC'ed. The valid range for last_gen is: 0,1,...,NUM_GENERATIONS.
 *
 * The oldest generation to be GCed will always be
 * gencgc_oldest_gen_to_gc, partly ignoring last_gen if necessary.
 */
void
collect_garbage(unsigned last_gen)
{
    int gen = 0;
    int raise;
    int gen_to_wp;
    int i;

    boxed_region.free_pointer = (void *) get_current_region_free();

    /* Check last_gen */
    if (last_gen > NUM_GENERATIONS) {
	fprintf(stderr,
		"** collect_garbage: last_gen = %d. Doing a level 0 GC.\n",
		last_gen);
	last_gen = 0;
    }

    /* Flush the alloc regions updating the tables. */
    gc_alloc_update_page_tables(0, &boxed_region);
    gc_alloc_update_page_tables(1, &unboxed_region);

    /* Verify the new objects created by lisp code. */
    if (pre_verify_gen_0) {
	fprintf(stderr, "Pre-Checking generation 0\n");
	verify_generation(0);
    }

    if (gencgc_verbose > 1)
	print_generation_stats(0);

    scavenger_hooks = (struct scavenger_hook *) NIL;

    do {
	/* Collect the generation */

	/* Never raise the oldest generation. */
	if (gen >= gencgc_oldest_gen_to_gc)
	    raise = 0;
	else
	    /* Raise if: gen < last_gen */
	if (gen < last_gen)
	    raise = 1;
	else
	    /* Only raise if the age is >= the trigger age. */
	if (generations[gen].num_gc >= generations[gen].trigger_age)
	    raise = 1;
	else
	    raise = 0;

	if (gencgc_verbose > 1)
	    fprintf(stderr,
		    "Starting GC of generation %d with raise=%d alloc=%d trig=%d GCs=%d\n",
		    gen, raise, generations[gen].bytes_allocated,
		    generations[gen].gc_trigger, generations[gen].num_gc);

	/*
	 * If an older generation is being filled then update its memory age.
	 */
	if (raise == 1)
	    generations[gen + 1].cum_sum_bytes_allocated +=
		generations[gen + 1].bytes_allocated;

	garbage_collect_generation(gen, raise);

	/* Reset the memory age cum_sum */
	generations[gen].cum_sum_bytes_allocated = 0;

	if (gencgc_verbose > 1) {
	    fprintf(stderr, "GC of generation %d finished:\n", gen);
	    print_generation_stats(0);
	}

	gen++;
    }
    while (gen <= gencgc_oldest_gen_to_gc
	   && (gen < last_gen
	       || (gen <= gencgc_oldest_gen_to_gc && raise
		   && generations[gen].bytes_allocated >
		   generations[gen].gc_trigger
		   && gen_av_mem_age(gen) > generations[gen].min_av_mem_age)));

    /*
     * Now if gen-1 was raised all generations before gen are empty.If
     * it wasn't raised then all generations before gen-1 are empty.
     *
     * Now objects within this gen's pages cannot pointer to younger
     * generations unless they are written to. This can be exploited by
     * write protecting the pages of gen; then when younger generations
     * are GCed only the page written need scanning.
     */
    if (raise)
	gen_to_wp = gen;
    else
	gen_to_wp = gen - 1;

    /*
     * Not much point in WPing pages in generation 0 as it is never
     * scavenged (except promoted pages).
     */
    if (gen_to_wp > 0 && enable_page_protection) {
	/* Check that they are all empty */
	for (i = 0; i < gen_to_wp; i++)
	    if (generations[i].bytes_allocated != 0)
		fprintf(stderr,
			"*** trying to write prot. gen. %d when gen. %d is not empty\n",
			gen_to_wp, i);

	write_protect_generation_pages(gen_to_wp);
    }

    /*
     * Set gc_alloc back to generation 0. The current regions should be
     * flushed after the above GCs.
     */
    gc_assert(boxed_region.free_pointer - boxed_region.start_addr == 0);
    gc_alloc_generation = 0;

    update_dynamic_space_free_pointer();

    set_current_region_free((lispobj) boxed_region.free_pointer);
    set_current_region_end((lispobj) boxed_region.end_addr);

    /* Call the scavenger hook functions */
    {
	struct scavenger_hook *sh;

	for (sh =
	     (struct scavenger_hook *) PTR((unsigned long) scavenger_hooks);
	     sh != (struct scavenger_hook *) PTR(NIL);) {
	    struct scavenger_hook *sh_next =
		(struct scavenger_hook *) PTR((size_t) sh->next);

#if 0
	    fprintf(stderr, "Scav hook %x; next %x; calling scav hook fn %x\n",
		    sh, sh_next, sh->function);
#endif
	    funcall0(sh->function);
	    sh->next = NULL;
	    sh = sh_next;
	}
	scavenger_hooks = (struct scavenger_hook *) NIL;
    }
}


/*
 * The is called by purify when it is finished. All live objects will
 * have been moved to the RO and Static heaps. The dynamic space will
 * need a full re-initialisation. Do not bother having purify flush
 * the current allocation region, as the page_tables are re-initialised,
 * and every page is zeroed to be sure.
 */

void
gc_free_heap(void)
{
    int page;

    if (gencgc_verbose > 1)
	fprintf(stderr, "Free heap\n");

    for (page = 0; page < dynamic_space_pages; page++)
	/* Skip Free pages which should already be zero filled. */
	if (PAGE_ALLOCATED(page)) {
	    char *page_start, *addr;

	    /*
	     * Mark the page free. The other slots are assumed invalid when
	     * it is unallocated and bytes_used is 0 and it should not be
	     * write protected - except that the generation is used for the
	     * current region but it sets that up.
	     */
	    page_table[page].flags &= ~PAGE_ALLOCATED_MASK;
	    page_table[page].bytes_used = 0;

	    /* Zero the page. */
	    page_start = (void *) page_address(page);

	    /* First remove any write protection */
	    os_protect((os_vm_address_t) page_start, PAGE_SIZE, OS_VM_PROT_ALL);
	    page_table[page].flags &= ~PAGE_WRITE_PROTECTED_MASK;

	    os_invalidate((os_vm_address_t) page_start, PAGE_SIZE);
	    addr =
		(char *) os_validate((os_vm_address_t) page_start, PAGE_SIZE);
	    if (addr == NULL || addr != page_start)
		fprintf(stderr, "gc_zero: page moved, 0x%08lx ==> 0x%08lx!\n",
			(unsigned long) page_start, (unsigned long) addr);
	} else if (gencgc_zero_check_during_free_heap && page < 16384) {
	    int *page_start;
	    unsigned i;

	    /* Double check that the page is zero filled. */
	    gc_assert(!PAGE_ALLOCATED(page));
	    gc_assert(page_table[page].bytes_used == 0);

	    page_start = (int *) page_address(page);

	    for (i = 0; i < 1024; i++)
		if (page_start[i] != 0)
		    fprintf(stderr, "** Free region not zero @ %lx\n",
			    (unsigned long) (page_start + i));
	}

    bytes_allocated = 0;

    /* Initialise the generations. */
    for (page = 0; page < NUM_GENERATIONS; page++) {
	generations[page].alloc_start_page = 0;
	generations[page].alloc_unboxed_start_page = 0;
	generations[page].alloc_large_start_page = 0;
	generations[page].alloc_large_unboxed_start_page = 0;
	generations[page].bytes_allocated = 0;
	generations[page].gc_trigger = 2000000;
	generations[page].num_gc = 0;
	generations[page].cum_sum_bytes_allocated = 0;
    }

    if (gencgc_verbose > 1)
	print_generation_stats(0);

    /* Initialise gc_alloc */
    gc_alloc_generation = 0;
    boxed_region.first_page = 0;
    boxed_region.last_page = -1;
    boxed_region.start_addr = page_address(0);
    boxed_region.free_pointer = page_address(0);
    boxed_region.end_addr = page_address(0);

    unboxed_region.first_page = 0;
    unboxed_region.last_page = -1;
    unboxed_region.start_addr = page_address(0);
    unboxed_region.free_pointer = page_address(0);
    unboxed_region.end_addr = page_address(0);

    last_free_page = 0;

    set_alloc_pointer((lispobj) heap_base);

    set_current_region_free((lispobj) boxed_region.free_pointer);
    set_current_region_end((lispobj) boxed_region.end_addr);

    if (verify_after_free_heap) {
	/* Check if purify has left any bad pointers. */
	if (gencgc_verbose)
	    fprintf(stderr, "Checking after free_heap.\n");
	verify_gc();
    }
}



void
gc_init(void)
{
    int i;

    gc_init_tables();

    heap_base = (void *) DYNAMIC_0_SPACE_START;

    /* The number of pages needed for the dynamic space - rounding up. */
    dynamic_space_pages = (dynamic_space_size + (PAGE_SIZE - 1)) / PAGE_SIZE;

    page_table =

	(struct page *) malloc(dynamic_space_pages * sizeof(struct page));
    if (page_table == NULL) {
	fprintf(stderr, "Unable to allocate page table.\n");
	exit(1);
    }

    /* Initialise each page structure. */

    for (i = 0; i < dynamic_space_pages; i++) {
	/* Initial all pages as free. */
	page_table[i].flags &= ~PAGE_ALLOCATED_MASK;
	page_table[i].bytes_used = 0;

	/* Pages are not write protected at startup. */
	page_table[i].flags &= ~PAGE_WRITE_PROTECTED_MASK;
    }

    bytes_allocated = 0;

    /* Initialise the generations. */
    for (i = 0; i < NUM_GENERATIONS; i++) {
	generations[i].alloc_start_page = 0;
	generations[i].alloc_unboxed_start_page = 0;
	generations[i].alloc_large_start_page = 0;
	generations[i].alloc_large_unboxed_start_page = 0;
	generations[i].bytes_allocated = 0;
	generations[i].gc_trigger = 2000000;
	generations[i].num_gc = 0;
	generations[i].cum_sum_bytes_allocated = 0;
	/* The tune-able parameters */
	generations[i].bytes_consed_between_gc = 2000000;
	generations[i].trigger_age = 1;
	generations[i].min_av_mem_age = 0.75;
    }

    /* Initialise gc_alloc */
    gc_alloc_generation = 0;
    boxed_region.first_page = 0;
    boxed_region.last_page = -1;
    boxed_region.start_addr = page_address(0);
    boxed_region.free_pointer = page_address(0);
    boxed_region.end_addr = page_address(0);

    unboxed_region.first_page = 0;
    unboxed_region.last_page = -1;
    unboxed_region.start_addr = page_address(0);
    unboxed_region.free_pointer = page_address(0);
    unboxed_region.end_addr = page_address(0);

    last_free_page = 0;

    set_current_region_free((lispobj) boxed_region.free_pointer);
    set_current_region_end((lispobj) boxed_region.end_addr);
}

/*
 * Pickup the dynamic space from after a core load.
 *
 * The ALLOCATION_POINTER points to the end of the dynamic space.
 *  
 * XX A scan is needed to identify the closest first objects for pages.
 */

void
gencgc_pickup_dynamic(void)
{
    int page = 0;
    unsigned long addr = DYNAMIC_0_SPACE_START;
    unsigned long alloc_ptr = (unsigned long) get_alloc_pointer();

    /* Initialise the first region. */
    do {
	page_table[page].flags |= PAGE_ALLOCATED_MASK;
	page_table[page].flags &= ~(PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK
				    | PAGE_LARGE_OBJECT_MASK);
	page_table[page].bytes_used = PAGE_SIZE;
	page_table[page].first_object_offset =
	    (char *) DYNAMIC_0_SPACE_START - page_address(page);
	addr += PAGE_SIZE;
	page++;
    }
    while (addr < alloc_ptr);

    generations[0].bytes_allocated = PAGE_SIZE * page;
    bytes_allocated = PAGE_SIZE * page;

    set_current_region_free((lispobj) boxed_region.free_pointer);
    set_current_region_end((lispobj) boxed_region.end_addr);
}



/*
 * Alloc is the external interface for memory allocation. It allocates
 * to generations0.  It is not called from within the garbage
 * collector as it's only external uses that need the check for heap
 * size (GC trigger) and to disable the interrupts (interrupts are
 * always disabled during a GC).
 * 
 * It is assumed by the vops that the returned space is zero
 * filled. E.g. the MS word of a 2 word bignum in move-from-unsigned.
 *
 * The check for a GC trigger is only performed when the current
 * region is full, so in most cases it's not needed. Further maybe-gc
 * is only called once because lisp will remember the need to collect
 * garbage and get to it when it can.
 *
 * Note that this code is typically called directly from lisp code,
 * while within a pseudo atomic context.
 */

void do_pending_interrupt(void);

char *
alloc(int nbytes)
{
#if !(defined(sparc) || defined(DARWIN))
    /*
     * *current-region-free-pointer* is the same as alloc-tn (=
     * current_dynamic_space_free_pointer) and therefore contains the
     * pseudo-atomic bits.
     */
    gc_assert(((unsigned) get_current_region_free() & lowtag_Mask) == 0);
#endif
    gc_assert((nbytes & lowtag_Mask) == 0);
    gc_assert(get_pseudo_atomic_atomic());

    bytes_allocated_sum += nbytes;

    for (;;) {
	void *new_obj;
	char *new_free_pointer = (void *) (get_current_region_free() + nbytes);

	if (new_free_pointer <= boxed_region.end_addr) {
	    /* Allocate from the current region. */
	    new_obj = (void *) get_current_region_free();
	    set_current_region_free((lispobj) new_free_pointer);
	    return new_obj;
	} else if (bytes_allocated <= auto_gc_trigger) {
	    /* Call gc_alloc.  */
	    boxed_region.free_pointer = (void *) get_current_region_free();
	    boxed_region.end_addr =
		(void *) SymbolValue(CURRENT_REGION_END_ADDR);

	    new_obj = gc_alloc(nbytes);
	    set_current_region_free((lispobj) boxed_region.free_pointer);
	    set_current_region_end((lispobj) boxed_region.end_addr);
	    return new_obj;
	} else {
	    /* Run GC and try again.  */
	    auto_gc_trigger *= 2;
	    clr_pseudo_atomic_atomic();
	    if (get_pseudo_atomic_interrupted())
		do_pending_interrupt();
	    funcall0(SymbolFunction(MAYBE_GC));
	    clr_pseudo_atomic_interrupted();
	    set_pseudo_atomic_atomic();
	}
    }
}

char *
alloc_pseudo_atomic(int nbytes)
{
    char *result;
    int in_pseudo_atomic;

    in_pseudo_atomic = get_pseudo_atomic_atomic();
    if (!in_pseudo_atomic) {
	clr_pseudo_atomic_interrupted();
	set_pseudo_atomic_atomic();
    }

    result = alloc(nbytes);

    if (!in_pseudo_atomic) {
	clr_pseudo_atomic_atomic();
	if (get_pseudo_atomic_interrupted())
	    do_pending_interrupt();
    }

    return result;
}


/* Noise to manipulate the gc trigger stuff. */
void
set_auto_gc_trigger(unsigned long dynamic_usage)
{
    auto_gc_trigger += dynamic_usage;
}

void
clear_auto_gc_trigger(void)
{
    auto_gc_trigger = 0xffffffff;
}

/* Find the code object for the given pc. Return NULL on failure */
lispobj *
component_ptr_from_pc(lispobj * pc)
{
    lispobj *ptr;

    ptr = search_read_only_space(pc);

    if (!ptr)
	ptr = search_static_space(pc);

    if (!ptr)
	ptr = search_dynamic_space(pc);

    /* Found anything? Check if it is a code object.  */
    if (ptr && TypeOf(*ptr) == type_CodeHeader)
	return ptr;

    return NULL;
}

/*
 * Get lower and upper(middle) 28 bits of total allocation
 */
int
get_bytes_consed_lower(void)
{
    return (int) bytes_allocated_sum & 0xFFFFFFF;
}

int
get_bytes_consed_upper(void)
{
    return ((int) bytes_allocated_sum / 0x10000000) & 0xFFFFFFF;
}

#define current_region_free_pointer get_current_region_free()
#define current_region_end_addr     ((void *) SymbolValue(CURRENT_REGION_END_ADDR))

int
get_bytes_allocated_lower(void)
{
    int size = bytes_allocated;
    static int previous = -1;

    if (current_region_end_addr != boxed_region.end_addr) {
	fprintf(stderr, "NOT BOXED: %lx %lx %lx\n",
		(unsigned long) current_region_end_addr,
		(unsigned long) boxed_region.end_addr,
		(unsigned long) unboxed_region.end_addr);
    }

    if (current_region_end_addr == boxed_region.end_addr) {
	size += current_region_free_pointer - (size_t) boxed_region.start_addr;
    } else {
	size +=
	    current_region_free_pointer - (size_t) unboxed_region.start_addr;
    }

    if (counters_verbose)
	fprintf(stderr, ">%10d%10d%10d%10d%10d (max%d @0x%lX)\n", size,
		previous != -1 ? size - previous : -1,
		(size_t) current_region_free_pointer -
		(size_t) boxed_region.start_addr,
		(size_t) boxed_region.free_pointer -
		(size_t) boxed_region.start_addr,
		(size_t) unboxed_region.free_pointer -
		(size_t) unboxed_region.start_addr,
		(size_t) boxed_region.end_addr -
		(size_t) boxed_region.start_addr,
		(unsigned long) boxed_region.start_addr);

    previous = size;

    return (int) size & 0xFFFFFFF;
}

int
get_bytes_allocated_upper(void)
{
    int size = bytes_allocated;

    if ((void *) current_region_end_addr == boxed_region.end_addr) {
	size += current_region_free_pointer - (size_t) boxed_region.start_addr;
    } else {
	size +=
	    current_region_free_pointer - (size_t) unboxed_region.start_addr;
    }
    return ((int) size / 0x10000000) & 0xFFFFFFF;
}

void
print_bytes_allocated_sum(void)
{
#if 0
    int size;

    /*
       gc_alloc_update_page_tables(0, &);
       gc_alloc_update_page_tables(1, &unboxed_region);
     */

    size = bytes_allocated;

    if (current_region_end_addr == boxed_region.end_addr) {
	size += current_region_free_pointer - boxed_region.start_addr;
	size += unboxed_region.free_pointer - unboxed_region.start_addr;
    } else {
	size += current_region_free_pointer - unboxed_region.start_addr;
	size += boxed_region.free_pointer - boxed_region.start_addr;
    }
    fprintf(stdout, "manually counted: %10d %10d %10d\n", size,
	    size - bytes_allocated, size - bytes_allocated_sum);

    /*
       fprintf(stdout, "%llu -> %d / %d\n", bytes_allocated_sum
       ,get_bytes_consed_upper(), get_bytes_consed_lower());
       fprintf(stdout, "0x%llX -> 0x%x / 0x%x\n", bytes_allocated_sum
       ,get_bytes_consed_upper(), get_bytes_consed_lower());
     */
#endif
}

/*
 * Let Lisp get at the page table entry and return the flags and the
 * bytes used
 */
void
get_page_table_info(int page, int* flags, int* bytes)
{
    *flags = page_table[page].flags;
    *bytes = page_table[page].bytes_used;
}
