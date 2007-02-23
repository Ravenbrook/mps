/* cgc.c -*- Mode: C; comment-column: 40; -*-
 * $Header: /project/cmucl/cvsroot/src/lisp/cgc.c,v 1.13 2005/09/15 18:26:51 rtoy Exp $
 *
 * Conservative Garbage Collector for CMUCL x86.
 *
 * This code is based on software written by William Lott, and
 * Public Domain codes from Carnegie Mellon University, and has
 * been placed in the Public Domain.
 *
 * Received from William 27 Jul 95.
 *
 * Debug, FreeBSD hooks, and integration by Paul Werkowski
 *
 *
 */
#include <stdio.h>
#include <assert.h>
#include <signal.h>
#include <string.h>
#include "os.h"			/* for SetSymbolValue */
#include "globals.h"		/* For dynamic_space_size */
#include "x86-validate.h"	/* for memory layout  */
#include "x86-lispregs.h"
#include "lisp.h"		/* for object defs */
#include "interrupt.h"		/* interrupt_handlers */
#include "internals.h"
#include "cgc.h"

#if !defined MIN
#define MIN(a,b)(((a)<(b))?(a):(b))
#define MAX(a,b)(((a)>(b))?(a):(b))
#endif

#include <unistd.h>
#include <stdlib.h>
#if defined unix
#include <sys/param.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>


#define dprintf(t,exp) if(t){printf exp ; fflush(stdout);}

/* Object representation details. The allocator/collector knows
 * almost nothing about lisp internals and is fairly general.
*/

#define ALIGN_BITS 3
#define ALIGN_BYTES (1<<ALIGN_BITS)
#define ALIGNEDP(addr) ((((int)addr)&(ALIGN_BYTES-1)) == 0)

/* Type of an object. */
typedef struct object {
    long header;
    struct object *data[1];
} *obj_t;

/* Just leave unused space */
#define NOTE_EMPTY(ptr,bytes) {}


/* Collector datastructures */

#define BLOCK_BITS 16
#define BLOCK_BYTES (1<<BLOCK_BITS)
#define BLOCK_NUMBER(ptr) (((long)(ptr))>>BLOCK_BITS)
#define BLOCK_ADDRESS(num) ((void *)((num)<<BLOCK_BITS))

#define CHUNK_BITS 9
#define CHUNK_BYTES (1<<CHUNK_BITS)
#define CHUNK_NUMBER(ptr) (((long)(ptr))>>CHUNK_BITS)
#define CHUNK_ADDRESS(num) ((void *)((num)<<CHUNK_BITS))

#define BLOCK_CHUNKS (1<<(BLOCK_BITS-CHUNK_BITS))


#define ROUNDDOWN(val,x) ((val)&~((x)-1))
#define ROUNDUP(val,x) ROUNDDOWN((val)+(x)-1,x)

#define gc_abort() lose("GC invariant lost!  File \"%s\", line %d\n", \
			__FILE__, __LINE__)

#if 0
#define gc_assert(ex) {if (!(ex)) gc_abort();}
#else
#define gc_assert(ex)
#endif

char *alloc(int);


struct cluster {
    /* Link to the next cluster. */
    struct cluster *next;

    /* The number of blocks in this cluster. */
    int num_blocks;

    /* Pointer to the first region. */
    struct region *first_region;

    /* Table index by the chunk number of some pointer minus the chunk */
    /* number for the first region giving the number of chunks past */
    /* the chunk holding the region header that spans that pointer. */
    /* Actually, it might not be enough.  So after backing up that far, */
    /* try again. */
    unsigned char region_offset[1];
};

/* The first word of this is arranged to look like a fixnum
 * so as not to confuse 'room'.
 */
struct region {
    unsigned
      res1:2, num_chunks:16, contains_small_objects:1, clean:1, hole:7;
    struct region **prev;
    struct region *next;
    struct space *space;
};

#define REGION_OVERHEAD ROUNDUP(sizeof(struct region), ALIGN_BYTES)


struct space {
    struct region *regions;
    struct region **regions_tail;
    char *alloc_ptr;
    char *alloc_end;
};

/* Chain of all the clusters. */
struct cluster *clusters = NULL;
static int num_clusters = 0;	/* for debugging */
int cgc_debug = 0;		/* maybe set from Lisp */

/* Table indexed by block number giving the cluster that block is part of. */
static struct cluster **block_table = NULL;

/* The allocated memory block_table is offset from. */
static struct cluster **block_table_base = NULL;

/* The maximum bounds on the heap. */
static void *heap_base = NULL;
static void *heap_end = NULL;

/* The two dynamic spaces. */
static struct space space_0 = { NULL };
static struct space space_1 = { NULL };

/* Pointers it whichever dynamic space is currently newspace and oldspace */
static struct space *newspace = NULL;
static struct space *oldspace = NULL;

/* Free lists of regions. */
static struct region *small_region_free_list = NULL;
static struct region *large_region_free_list = NULL;
static void move_to_newspace(struct region *region);

#if defined TESTING
static void
print_region(struct region *r)
{
    dprintf(1, ("[region %x %d <%x %x> %x]\n",
		r, r->num_chunks, r->prev, r->next, r->space));
}
static void
print_regions(struct region *r, char *str)
{
    printf("Regions %s:\n", str);
    for (; r != NULL; r = r->next)
	print_region(r);
}

static void
print_space(struct space *s)
{
    struct region *r = s->regions;

    dprintf(1, ("[space %x %s %s <%x - %x>]\n",
		s,
		(s == &space_0) ? "S0" : "S1",
		(s == newspace) ? "NewSpace" : "OldSpace",
		s->alloc_ptr, s->alloc_end));
    print_regions(r, "");

}

void
print_spaces()
{
    print_space(&space_0);
    print_space(&space_1);
    print_regions(large_region_free_list, "LRFL");
    print_regions(small_region_free_list, "SRFL");
}

void
print_cluster(struct cluster *cluster)
{
    printf("[cluster %x >%x %d]\n", cluster, cluster->next,
	   cluster->num_blocks);
    print_regions(cluster->first_region, "cluster");
}

void
print_clusters()
{
    struct cluster *cluster;

    for (cluster = clusters; cluster != NULL; cluster = cluster->next)
	print_cluster(cluster);
}
#endif /* TESTING */


/* Allocation/deallocation routines */

static void
init_region(struct region *region, int nchunks)
{
    int region_block = BLOCK_NUMBER(region);
    struct cluster *cluster = block_table[region_block];
    int offset = CHUNK_NUMBER(region) - CHUNK_NUMBER(cluster->first_region);
    int i;

    dprintf(0, ("init region %x %d\n", region, nchunks));
    *(long *) region = 0;	/* clear fields */
    region->num_chunks = nchunks;
    if (nchunks > UCHAR_MAX) {
	for (i = 0; i < UCHAR_MAX; i++)
	    cluster->region_offset[offset + i] = i;
	for (; i < nchunks; i++)
	    cluster->region_offset[offset + i] = UCHAR_MAX;
    } else {
	for (i = 0; i < nchunks; i++)
	    cluster->region_offset[offset + i] = i;
    }
}

static struct region *
maybe_alloc_large_region(int nchunks)
{
    struct region *region, **prev;

    prev = &large_region_free_list;
    while ((region = *prev) != NULL) {
	if (region->num_chunks >= nchunks) {
	    if (region->num_chunks == nchunks)
		*prev = region->next;
	    else {
		struct region *new
		    =

		    (struct region *) ((char *) region + nchunks * CHUNK_BYTES);
		init_region(new, region->num_chunks - nchunks);
		new->next = region->next;
		new->prev = NULL;
		new->space = NULL;
		*prev = new;
		region->num_chunks = nchunks;
	    }
	    region->next = NULL;
	    region->prev = NULL;
	    region->space = NULL;
	    return region;
	}
	prev = &region->next;
    }
    return NULL;
}


/* from os_zero */
static void
cgc_zero(addr, length)
     os_vm_address_t addr;
     os_vm_size_t length;
{
    os_vm_address_t block_start = os_round_up_to_page(addr);
    os_vm_address_t end = addr + length;
    os_vm_size_t block_size;


    if (block_start > addr)
	memset((char *) addr, 0, MIN(block_start - addr, length))

	    if (block_start < end) {
	    length -= block_start - addr;

	    block_size = os_trunc_size_to_page(length);

	    if (block_size < length)
		memset((char *) block_start + block_size, 0,
		       length - block_size);

	    if (block_size != 0) {
		/* Now deallocate and allocate the block so that it */
		/* faults in  zero-filled. */

		os_invalidate(block_start, block_size);
		addr = os_validate(block_start, block_size);

		if (addr == NULL || addr != block_start)
		    fprintf(stderr,
			    "cgc_zero: block moved, 0x%08x ==> 0x%08x!\n",
			    block_start, addr);
	    }
	}
}

static void
compact_cluster(struct cluster *cluster)
{
    int show = 0;
    struct region *region = cluster->first_region;
    struct region *end =
	(struct region *) ((char *) region + cluster->num_blocks * BLOCK_BYTES);
    int grown = 0;
    unsigned max_chunks = cluster->num_blocks * BLOCK_CHUNKS;
    struct region *large_additions = NULL;
    struct region **large_prev = &large_additions;
    struct region *small_additions = NULL;
    struct region **small_prev = &small_additions;

    dprintf(show, ("compact cluster %x\n", cluster));
    while (region < end) {
	struct region *next =
	    (struct region *) ((char *) region +

			       region->num_chunks * CHUNK_BYTES);
	if (region->space != newspace) {	/* was == NULL */
	    if (next < end && next->space != newspace) {	/* was == NULL */
		gc_assert(region >= cluster->first_region);
		gc_assert(region->space == NULL);
		gc_assert(next->space == NULL);
		gc_assert(region->num_chunks > 0);
		gc_assert(next->num_chunks > 0);
		gc_assert((region->num_chunks + next->num_chunks) <=
			  max_chunks);
		region->num_chunks += next->num_chunks;
		grown = 1;
	    } else {
		if (grown) {
		    init_region(region, region->num_chunks);
		    region->space = NULL;
		    grown = 0;
		}
		{
		    int ovh = REGION_OVERHEAD;

		    cgc_zero((os_vm_address_t) ((char *) region + ovh),
			     (os_vm_size_t) (region->num_chunks * CHUNK_BYTES) -
			     ovh);
		}

		if (region->num_chunks == 1) {
		    *small_prev = region;
		    small_prev = &region->next;
		} else {
		    *large_prev = region;
		    large_prev = &region->next;
		}
		region = next;
	    }
	} else
	    region = next;
    }

    *large_prev = large_region_free_list;
    large_region_free_list = large_additions;
    *small_prev = small_region_free_list;
    small_region_free_list = small_additions;
}

static void
compact_free_regions()
{
    struct cluster *cluster;

    large_region_free_list = NULL;
    small_region_free_list = NULL;

    for (cluster = clusters; cluster != NULL; cluster = cluster->next)
	compact_cluster(cluster);
}

/* WL code arranged to allocate new space via the sbrk() mechanism.
 * However, I am going to start by allocating from the standard dynamic
 * space. The idea is to use the normal allocation scheme for initial
 * system build and switch to the cgc allocator when starting up a
 * saved image when dynamic space is hopefully clean.
 */
static struct region *
new_region(int nblocks)
{
    /* take from existing dynamic space */
    char *new = (char *) SymbolValue(ALLOCATION_POINTER);
    struct region *region =

	(struct region *) (ROUNDUP((long) new, BLOCK_BYTES));
    int bn = BLOCK_NUMBER(region);

    new += (nblocks * BLOCK_BYTES + ((char *) region - new));
    SetSymbolValue(ALLOCATION_POINTER, (lispobj) new);
    return region;
}

static void
new_cluster(int min_blocks)
{
    int nblocks = min_blocks < 4 ? 4 : min_blocks;
    int nchunks = nblocks << (BLOCK_BITS - CHUNK_BITS);
    int i;
    struct cluster *cluster = malloc(sizeof(struct cluster) + nchunks - 1);
    struct region *region = new_region(nblocks);

    int bn = BLOCK_NUMBER(region);

    dprintf(cgc_debug, ("new cluster %x region@%x\n", cluster, region));
    for (i = 0; i < nblocks; i++)
	block_table[bn + i] = cluster;

    num_clusters++;
    cluster->next = clusters;
    clusters = cluster;
    cluster->num_blocks = nblocks;
    cluster->first_region = region;

    init_region(region, nchunks);

    region->next = large_region_free_list;
    large_region_free_list = region;
    region->prev = NULL;
    region->space = NULL;
}

unsigned long bytes_allocated = 0;	/* Seen by (dynamic-usage) */
static unsigned long auto_gc_trigger = 0;
static int maybe_gc_called = 0;

static struct region *
alloc_large_region(int nchunks)
{
    struct region *region;

    {
	region = maybe_alloc_large_region(nchunks);

	if (region == NULL) {
	    new_cluster((nchunks + BLOCK_CHUNKS - 1) >>
			(BLOCK_BITS - CHUNK_BITS));
	    region = maybe_alloc_large_region(nchunks);
	    gc_assert(region != NULL);
	}
    }
    gc_assert(region->space == NULL);
    return region;
}

static struct region *
alloc_small_region()
{
    struct region *region = small_region_free_list;

    if (region == NULL)
	region = alloc_large_region(1);
    else
	small_region_free_list = region->next;
    region->next = NULL;
    region->prev = NULL;
    region->space = NULL;
    move_to_newspace(region);
    return region;
}

static int chunks_freed = 0;

static void
free_region(struct region *region)
{
    gc_assert(region->space && region->space == oldspace);
    gc_assert(region->num_chunks > 0);

    region->space = NULL;	/* for compact_cluster? */
    region->prev = NULL;	/* housekeeping I hope */
    chunks_freed += region->num_chunks;

    if (region->num_chunks == 1) {
	region->next = small_region_free_list;
	small_region_free_list = region;
    } else {
	region->next = large_region_free_list;
	large_region_free_list = region;
    }
}

static void *
alloc_large(int nbytes)
{
    int nchunks = (nbytes + REGION_OVERHEAD + CHUNK_BYTES - 1) >> CHUNK_BITS;
    struct region *region = alloc_large_region(nchunks);

    region->contains_small_objects = 0;
    region->next = NULL;
    region->prev = NULL;
    region->space = NULL;
    bytes_allocated += region->num_chunks * CHUNK_BYTES;
    move_to_newspace(region);
    return (char *) region + REGION_OVERHEAD;
}

void *
cgc_alloc(int nbytes)
{
    void *res;

    dprintf(0, ("alloc %d\n", nbytes));

    if (nbytes > (CHUNK_BYTES - REGION_OVERHEAD))
	res = alloc_large(nbytes);
    else {
	struct space *space = newspace;

	if ((space->alloc_ptr + nbytes) > space->alloc_end) {
	    struct region *region;

	    if (space->alloc_ptr != NULL) {
		int hole = space->alloc_end - space->alloc_ptr;

		if (hole >= ALIGN_BYTES)
		    /* This wastes the space, eg suppose one cons
		     * has been allocated then a request for
		     * a maximum sized small obj comes in. I'd like
		     * to remember that there is still a lot of
		     * room left in this region. Maybe I could actually
		     * use the small_region_free_list in some way.
		     */
		    NOTE_EMPTY(space->alloc_ptr, hole);
	    }
	    region = alloc_small_region();
	    region->contains_small_objects = 1;
	    space->alloc_ptr = (char *) region + REGION_OVERHEAD;
	    space->alloc_end = (char *) region + CHUNK_BYTES;
	    bytes_allocated += region->num_chunks * CHUNK_BYTES;
	}

	res = space->alloc_ptr;
	space->alloc_ptr += ROUNDUP(nbytes, ALIGN_BYTES);
    }
    return res;
}


static void
move_to_newspace(struct region *region)
{
    /* (maybe) unlink region from oldspace and add to tail of 
     * newspace regions. Don't attempt to move a region that
     * is already in newspace.
     */
    struct space *space = newspace;

    if (region->space == oldspace) {
	/* Remove region from list. The prev slot holds
	 * the address of the 'next' slot of the previous
	 * list entry, not a pointer to that region (why?)
	 */
	*region->prev = region->next;
	if (region->next)
	    region->next->prev = region->prev;
	if (region->space->regions_tail == &region->next)
	    region->space->regions_tail = region->prev;
    }
    /* Append to newspace unless it has already been promoted. */
    if (region->space != newspace) {
	region->prev = space->regions_tail;
	region->next = NULL;
	*space->regions_tail = region;
	space->regions_tail = &region->next;
	region->space = space;
    }
}

static struct region *
find_region(void *ptr)
{
    struct cluster *cluster;
    int cluster_chunk_num;
    int chunk_num;
    unsigned char delta;

    ptr = (void *) ((int) ptr & ~0x3);
    if (ptr < heap_base || ptr >= heap_end)
	return NULL;

    cluster = block_table[BLOCK_NUMBER(ptr)];
    if (cluster == NULL)
	return NULL;

    if (ptr < (void *) cluster->first_region)
	return NULL;

    cluster_chunk_num = CHUNK_NUMBER(cluster->first_region);
    chunk_num = CHUNK_NUMBER(ptr);

    while (delta = cluster->region_offset[chunk_num - cluster_chunk_num])
	chunk_num -= delta;

    return CHUNK_ADDRESS(chunk_num);
}

/* Interface to std collector */
static inline boolean
from_space_p(lispobj obj)
{
    struct region *region = find_region((void *) obj);

    return (region != NULL && region->space == oldspace);
}
static inline boolean
new_space_p(lispobj obj)
{
    struct region *region = find_region((void *) obj);

    return (region != NULL && region->space == newspace);
}
static inline boolean
static_space_p(lispobj obj)
{
    return (STATIC_SPACE_START < obj
	    && obj < SymbolValue(STATIC_SPACE_FREE_POINTER));
}

/* Predicate that returns true if an object is a pointer. */
#undef  POINTERP
#define POINTERP(obj) Pointerp((obj)->header)

/* Predicate that returns true if an object has been forwarded. */
#define FORWARDED(obj) ((obj_t)(obj)->header == (obj_t)0x1)

/* Returns the forwarding pointer for the given object. */
#define FORWARDING_PTR(obj) ((lispobj)(obj)->data[0])

/* Marks obj as forwarded to new */
#define DEPOSIT_FORWARDING_PTR(obj,new) \
 ((obj_t)(obj)->header = 0x1, (obj_t)(obj)->data[0] = (obj_t)new)

/* Returns an obj_t for the object starting at addr */
#define OBJECT_AT(addr) ((obj_t)(addr))

/* Returns the size (in bytes) of obj. */
#define OBJECT_SIZE(obj) (sizeOfObject((obj_t)obj)<<2)

/* Scavenges an object. */
#define SCAVENGE_OBJECT(obj) scavengex((lispobj*)obj)

#if 0
/* Makes a region of memory look like some kind of object. */
#define NOTE_EMPTY(ptr,bytes) \
    (((obj_t)ptr)->header = (((bytes+ALIGN_BYTES-1)>>ALIGN_BITS)<<8) | 1)
#endif

static unsigned long bytes_copied = 0;

#   define  HAVE_FASTCOPY
#if defined HAVE_FASTCOPY
#define COPYDUAL(a,b,c) fastcopy16(a,b,c)
void fastcopy16(void *, void *, size_t);
#else
#define COPYDUAL(a,b,c) memmove(a,b,c)
#endif
static inline lispobj
copy(lispobj taggedobj)
{
    obj_t source = (obj_t) PTR(taggedobj);
    int nbytes = OBJECT_SIZE(source);

    gc_assert(Pointerp(taggedobj));
    gc_assert(!(nbytes & (ALIGN_BYTES - 1)));
    {
	int lowtag = LowtagOf(taggedobj);
	obj_t newobj = cgc_alloc(nbytes);

	COPYDUAL(newobj, source, nbytes);
	bytes_copied += nbytes;
	return ((lispobj) newobj | lowtag);
    }
}


#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))
#define NWORDS(x,y) (CEILING((x),(y)) / (y))

#define WEAK_POINTER_NWORDS \
	CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)
static struct weak_pointer *weak_pointers;


/* Scavenging:
 * CMU CL objects can be classified as BOXED, UNBOXED or other.
 * Boxed objects have a header containing length and type followed
 * by LENGTH tagged object descriptors which may be pointers.
 * UNBOXED objects have a header but the data is other than
 * tagged descriptors, such as floats, bignums, saps or code.
 * Others (code) contain a mix of boxed and unboxed and some
 * (cons) are like BOXED but without header. The scavenger needs
 * to consider these different kinds of objects. I will use a
 * table indexed by type to detect the simple cases of boxed
 * or unboxed.
 */
#define IMMED_OR_LOSE(thing) gc_assert(sct[TypeOf(thing)].sc_kind == SC_IMMED)
static void scavenge_pointer(lispobj *);
static int noise = 0;

typedef struct {
    unsigned sc_kind:3, ve_l2bits:5;
} OSC_t;

OSC_t
make_OSC(int kind, int log2bits)
{
    OSC_t thing;

    thing.sc_kind = kind;
    thing.ve_l2bits = log2bits;
    return thing;
}

#define SETSCT(indx,kind,logbits) sct[indx] = make_OSC(kind,logbits)
#define SC_ISBOXED 1
#define SC_UNBOXED 2
#define SC_IMMED   3
#define SC_POINTER 4
#define SC_VECTOR  5
#define SC_STRING  6
#define SC_OTHER   7
#define SC_LOSER   0
static OSC_t sct[256];

int
sizeOfObject(obj_t obj)
{
    int obj_type = TypeOf(obj->header);
    OSC_t class = sct[obj_type];
    struct vector *vector;
    int length = 1;
    int nwords = 1;

    switch (class.sc_kind) {
      case SC_POINTER:
      case SC_IMMED:
	  return 1;
      case SC_ISBOXED:
      case SC_UNBOXED:
	  gc_assert(HeaderValue(obj->header) > 0);
	  nwords = length = HeaderValue(obj->header) + 1;
	  break;
      case SC_STRING:
      case SC_VECTOR:
	  {
	      int log2bits = class.ve_l2bits;
	      int bits_per_el = 1 << log2bits;
	      int extra = 0;
	      int els_per_word = 1 << (5 - log2bits);

	      if (log2bits > 5) {
		  els_per_word = 1;
		  extra = log2bits - 5;
	      }
	      length = ((struct vector *) obj)->length;
	      length = fixnum_value(length);	/* Zero Length IS valid */
	      length += (class.sc_kind == SC_STRING);
	      length <<= extra;
	      nwords = NWORDS(length, els_per_word);
	      nwords += 2;	/* header + length */
	  }
	  break;
      case SC_OTHER:
	  switch (obj_type) {
	    case type_CodeHeader:
		{
		    struct code *code;
		    int nheader_words, ncode_words;

		    code = (struct code *) obj;
		    ncode_words = fixnum_value(code->code_size);
		    nheader_words = HeaderValue(code->header);
		    nwords = ncode_words + nheader_words;
		} break;
	    default:
		fprintf(stderr, "GC losage: no size for other type %d\n",
			obj_type);
		gc_abort();
	  }
	  break;
      default:
	  fprintf(stderr, "GC losage: no size for other type %d\n", obj_type);
	  gc_abort();
    }
    return CEILING(nwords, 2);
}

static void
init_osc()
{
    int i;

    for (i = 0; i < 256; i++)
	SETSCT(i, SC_LOSER, 0);
    for (i = 0; i < 32; i++) {
	SETSCT(type_EvenFixnum | (i << 3), SC_IMMED, 0);
	SETSCT(type_FunctionPointer | (i << 3), SC_POINTER, 0);
	/* OtherImmediate0 */
	SETSCT(type_ListPointer | (i << 3), SC_POINTER, 0);
	SETSCT(type_OddFixnum | (i << 3), SC_IMMED, 0);
	SETSCT(type_InstancePointer | (i << 3), SC_POINTER, 0);
	/* OtherImmediate1 */
	SETSCT(type_OtherPointer | (i << 3), SC_POINTER, 0);
    }
    SETSCT(type_Bignum, SC_UNBOXED, 0);
    SETSCT(type_Ratio, SC_ISBOXED, 0);
    SETSCT(type_SingleFloat, SC_UNBOXED, 0);
    SETSCT(type_DoubleFloat, SC_UNBOXED, 0);
#if defined type_ComplexSingleFloat
    SETSCT(type_ComplexSingleFloat, SC_UNBOXED, 0);
#endif
#if defined type_ComplexDoubleFloat
    SETSCT(type_ComplexDoubleFloat, SC_UNBOXED, 0);
#endif
    SETSCT(type_Complex, SC_ISBOXED, 0);
    SETSCT(type_SimpleArray, SC_ISBOXED, 0);
    SETSCT(type_SimpleString, SC_STRING, 3);
    SETSCT(type_SimpleBitVector, SC_VECTOR, 0);
    SETSCT(type_SimpleVector, SC_VECTOR, 5);
    SETSCT(type_SimpleArrayUnsignedByte2, SC_VECTOR, 1);
    SETSCT(type_SimpleArrayUnsignedByte4, SC_VECTOR, 2);
    SETSCT(type_SimpleArrayUnsignedByte8, SC_VECTOR, 3);
    SETSCT(type_SimpleArrayUnsignedByte16, SC_VECTOR, 4);
    SETSCT(type_SimpleArrayUnsignedByte32, SC_VECTOR, 5);
#if defined type_SimpleArraySignedByte8
    SETSCT(type_SimpleArraySignedByte8, SC_VECTOR, 3);
#endif
#if defined type_SimpleArraySignedByte16
    SETSCT(type_SimpleArraySignedByte16, SC_VECTOR, 4);
#endif
#if defined type_SimpleArraySignedByte30
    SETSCT(type_SimpleArraySignedByte30, SC_VECTOR, 5);
#endif
#if defined type_SimpleArraySignedByte32
    SETSCT(type_SimpleArraySignedByte32, SC_VECTOR, 5);
#endif
    SETSCT(type_SimpleArraySingleFloat, SC_VECTOR, 5);
    SETSCT(type_SimpleArrayDoubleFloat, SC_VECTOR, 6);
#if defined type_SimpleArrayComplexSingleFloat
    SETSCT(type_SimpleArrayComplexSingleFloat, SC_VECTOR, 6);
#endif
#if defined type_SimpleArrayComplexDoubleFloat
    SETSCT(type_SimpleArrayComplexDoubleFloat, SC_VECTOR, 7);
#endif
    SETSCT(type_ComplexString, SC_ISBOXED, 0);
    SETSCT(type_ComplexBitVector, SC_ISBOXED, 0);
    SETSCT(type_ComplexVector, SC_ISBOXED, 0);
    SETSCT(type_ComplexArray, SC_ISBOXED, 0);
    SETSCT(type_CodeHeader, SC_OTHER, 0);
    SETSCT(type_FunctionHeader, SC_OTHER, 0);
    SETSCT(type_ClosureFunctionHeader, SC_OTHER, 0);
    SETSCT(type_ReturnPcHeader, SC_OTHER, 0);
    SETSCT(type_ClosureHeader, SC_ISBOXED, 0);
    SETSCT(type_FuncallableInstanceHeader, SC_ISBOXED, 0);
    SETSCT(type_ByteCodeFunction, SC_ISBOXED, 0);
    SETSCT(type_ByteCodeClosure, SC_ISBOXED, 0);
    SETSCT(type_DylanFunctionHeader, SC_ISBOXED, 0);

    SETSCT(type_ValueCellHeader, SC_ISBOXED, 0);
    SETSCT(type_SymbolHeader, SC_ISBOXED, 0);
    SETSCT(type_BaseChar, SC_IMMED, 0);
    SETSCT(type_Sap, SC_UNBOXED, 0);
    SETSCT(type_UnboundMarker, SC_IMMED, 0);
    SETSCT(type_WeakPointer, SC_UNBOXED, 0);
    SETSCT(type_InstanceHeader, SC_ISBOXED, 0);
    SETSCT(type_Fdefn, SC_ISBOXED, 0);
}

static lispobj *scavenge(lispobj *, int);
static lispobj *scavenge_object(lispobj *);
static lispobj *scavengex(lispobj *);

static inline
scavenge_1word_obj(lispobj * addr)
{
    if (Pointerp(*addr)) {
	if (*addr != NIL && *addr != T)
	    scavenge_pointer(addr);
    } else
	IMMED_OR_LOSE(*addr);
}
static int debug_code = 0;
static int
scav_code_header(lispobj * where)
{
    lispobj object = *where;
    struct code *code;
    int i, nheader_words, ncode_words, nwords;
    lispobj fheaderl;
    struct function *fheaderp;

    dprintf(0, ("code: %x %x\n", where, object));
    code = (struct code *) where;
    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(object);
    nwords = ncode_words + nheader_words;
    nwords = CEILING(nwords, 2);
    /* Scavenge the boxed section of the code data block */
    /* NOTE: seeing a problem where the trace_table_offset slot
     * is a bogus list pointer instead of a fixnum such that 
     * junk gets moved to newspace which causes problems later.
     * Purify doesn't look at that slot (a bug?). Need
     * to figure out how it happens. Ans: from loading top-level
     * forms that init byte-compiled functions like "defun fcn".
     * Fix the loader to not do this and save some space!
     */
    for (i = 1; i < nheader_words; i++)
	scavenge_1word_obj(where + i);

    /* Scavenge the boxed section of each function object in the 
     * code data block.
     */
    fheaderl = code->entry_points;
    while (fheaderl != NIL) {
	fheaderp = (struct function *) PTR(fheaderl);
	gc_assert(TypeOf(fheaderp->header) == type_FunctionHeader);
	scavenge_1word_obj(&fheaderp->name);
	scavenge_1word_obj(&fheaderp->arglist);
	scavenge_1word_obj(&fheaderp->type);
	fheaderl = fheaderp->next;
    }
    return nwords;
}

#define RAW_ADDR_OFFSET (6*sizeof(lispobj) - type_FunctionPointer)
#ifdef i386
static void
scavenge_fcn_header(struct function *object)
{
    struct function *fheader = object;
    unsigned long offset = HeaderValue(fheader->header) * 4;

    /* Ok, we don't transport code here, but we do need to
     * scavenge the constants and functions (of which this is one).
     * This should be done as part of scavenging a live code object
     * and we could now be trying to do CPR on a corpse!
     */
    struct code *code = (struct code *) ((unsigned long) fheader - offset);

    gc_assert(TypeOf(fheader->header) == type_FunctionHeader);
    scav_code_header((lispobj *) code);
}

static int docode = 0;		/* maybe not needed */
static int
scav_closure_header(struct closure *closure)
{
    /* Could also be a funcallable_instance. The x86 port has the
     * raw code address in the function slot, not a lisp object.
     * However, the function object is a known distance from the code.
     */
    lispobj fun, fheader1;
    int i, words;

    gc_assert(ALIGNEDP(closure));
    words = HeaderValue(closure->header);
    fun = closure->function - RAW_ADDR_OFFSET;
    /* This needs to be done to get at live code. I now have no
     * way to know if this has already been scavenged so I assume
     * that it hasn't. Code that has been seen by purify is
     * supposed RO and doesn't (shouldn't) need to be looked at
     * so this maybe really redundant.
     *
     * I have seen one case where FI was incomplete with function
     * and lexenv slots == 0! Is this a bug?
     *
     * Update, it appears this is not needed. I will disable execution
     * by default but leave the code here in case something breaks.
     */
    if (docode && static_space_p(closure->function))
	scavenge_fcn_header((struct function *) PTR(fun));
    else			/* "normal" */
	scavenge_1word_obj(&fun);

    /* Now the boxed part of the closure header. */
    for (i = 0; i < words - 1; i++)
	scavenge_1word_obj(&closure->info[i]);

    return CEILING(words + 1, 2);
}
static int fnoise = 0;		/* experimental */
static int
scav_fdefn(lispobj * where)
{
    /* I don't know if this is really needs to be special cased here.
     * raw_address  should look like a fixnum and function is in static
     * space -- unless it is pointing to something in C like closure_tramp
     * or maybe undefined_tramp.
     * Actually function is in dynamic space if it is a byte-function!
     * Hmm, have seen case of function slot containing 1. Bug?
     */
    struct fdefn *fdefn = (struct fdefn *) where;
    int words = HeaderValue(fdefn->header);
    int fix_func =

	((char *) (fdefn->function + RAW_ADDR_OFFSET) == fdefn->raw_addr);
    scavenge_pointer(&fdefn->name);
    if (fnoise && LowtagOf(fdefn->function) == type_FunctionPointer) {
	obj_t fcnobj = (obj_t) PTR(fdefn->function);

	switch (TypeOf(fcnobj->header)) {
	      /* Can only be in static space and may need to scavenge code object.
	       * Won't be noticed by scavenge_pointer().
	       */
	  case type_FunctionHeader:
	      scavenge_fcn_header((struct function *) fcnobj);
	      break;
	      /* If in static space it was moved there by purify and we are
	       * doing normal scavenge. Handle normally.
	       */
	  case type_FuncallableInstanceHeader:
	  case type_ClosureHeader:
	      scavenge_pointer(&fdefn->function);
	      break;
	  default:
	      dprintf(1, ("Ignoring bogus value %x for fdefn function.\n",
			  *fcnobj));
	}
    } else
	/* NIL for undefined function? */
	scavenge_pointer(&fdefn->function);

    if (fix_func) {		/* This shouldn't be needed yet. */
	fdefn->raw_addr = (char *) (fdefn->function + RAW_ADDR_OFFSET);
    }
    return sizeof(struct fdefn) / sizeof(lispobj);
}

#endif

/* List scavenger taken from gc.c and adapted */

static FILE *log = NULL;
static int scav_ro = 0;		/* for testing  */
static int debug = 0;
static void *trapaddr = 0;
void
check_trap(void *addr)
{
    fprintf(stderr, "Trapped @ %x\n", addr);
}

static lispobj
trans_list(lispobj object)
{
    lispobj new_list_pointer;
    struct cons *cons, *new_cons;
    int n = 0;
    lispobj cdr;

    cons = (struct cons *) PTR(object);

    /* copy 'object' */
    new_cons = (struct cons *) cgc_alloc(sizeof(struct cons));

    new_cons->car = cons->car;
    new_cons->cdr = cons->cdr;	/* updated later */
    new_list_pointer = (lispobj) new_cons | LowtagOf(object);
    bytes_copied += sizeof(struct cons);

#if 0
    if (scav_ro > 1)
	check_trap(object);
    if (log)
	fprintf(log, "( %d cons @ #x%x -> #x%x car #x%x)\n",
		n++, cons, new_cons, new_cons->car);
#endif
    /* Grab the cdr before it is clobbered */
    cdr = cons->cdr;
    /* Set forwarding pointer (clobbers start of list). */
    DEPOSIT_FORWARDING_PTR((obj_t) cons, new_list_pointer);

    /* Try to linearize the list in the cdr direction to help reduce paging. */
    while (1) {
	lispobj new_cdr;
	struct cons *cdr_cons, *new_cdr_cons;

	if (LowtagOf(cdr) != type_ListPointer || !from_space_p(cdr)
	    || FORWARDED((obj_t) PTR(cdr)))
	    break;

	cdr_cons = (struct cons *) PTR(cdr);

	/* copy 'cdr' */
	new_cdr_cons = (struct cons *) cgc_alloc(sizeof(struct cons));

	new_cdr_cons->car = cdr_cons->car;
	new_cdr_cons->cdr = cdr_cons->cdr;
	new_cdr = (lispobj) new_cdr_cons | LowtagOf(cdr);
	bytes_copied += sizeof(struct cons);

#if 0
	if (scav_ro > 1)
	    check_trap(object);
	if (log)
	    fprintf(log, "( %d cons @ #x%x -> #x%x car #x%x)\n",
		    n++, cdr_cons, new_cdr_cons, cdr_cons->car);
#endif
	/* Grab the cdr before it is clobbered */
	cdr = cdr_cons->cdr;
	/* Set forwarding pointer */
	DEPOSIT_FORWARDING_PTR((obj_t) cdr_cons, new_cdr);

	/* Update the cdr of the last cons copied into new
	 * space to keep the newspace scavenge from having to do it.
	 */
	new_cons->cdr = new_cdr;

	new_cons = new_cdr_cons;
    }

    return new_list_pointer;
}

/* Weak Pointers */
static int weak_noise = 0;
static int do_weak = 1;
static int
scav_weak_pointer(lispobj * where)
{
    struct weak_pointer *wp = weak_pointers;

    /* Push the weak pointer onto the list of weak pointers.
     * Do I have to watch for duplicates? Originally this was
     * part of trans_weak_pointer but that didn't work in the
     * case where the WP was in a promoted region.
     */

    while (wp != NULL) {
	if (wp == (struct weak_pointer *) where)
	    break;
	wp = wp->next;
    }
    if (wp == NULL) {
	wp = (struct weak_pointer *) where;
	wp->next = weak_pointers;
	weak_pointers = wp;
	if (!do_weak)
	    scavenge_1word_obj(&wp->value);
    }

    /* Do not let GC scavenge the value slot of the weak pointer
     * (that is why it is a weak pointer).
     */

    return WEAK_POINTER_NWORDS;
}

void
scan_weak_pointers(void)
{
    struct weak_pointer *wp;

    for (wp = weak_pointers; wp != NULL; wp = wp->next) {
	lispobj value = wp->value;
	obj_t obj = (obj_t) PTR(value);
	lispobj first, *first_pointer;

	dprintf(weak_noise, ("Weak pointer at 0x%08x\n", (unsigned long) wp));
	dprintf(weak_noise, ("Value: 0x%08x\n", (unsigned long) value));

	if (Pointerp(value) && from_space_p(value)) {
	    /* Now, we need to check if the object has been forwarded.
	     * If it has been, the weak pointer is still good and needs
	     * to be updated. Otherwise, the weak pointer needs to be nil'ed out.
	     */

	    if (FORWARDED(obj))
		wp->value = FORWARDING_PTR(obj);
	    else {		/* break it */
		dprintf(weak_noise, ("Broken.\n"));
		wp->value = NIL;
		wp->broken = T;
	    }
	}
    }
}

static int
scavenge_random_object(lispobj * addr)
{
    lispobj header = *addr;
    int count = 1;

    dprintf(noise > 1, ("soi: %x @ %x\n", header, addr));
#if 0
    if (trapaddr == addr)
	check_trap(addr);
#endif
    gc_assert(ALIGNEDP(addr));

    switch (TypeOf(header)) {
      case type_SimpleVector:
	  {
	      struct vector *v = (struct vector *) addr;
	      int i, n = fixnum_value(v->length);

	      if (HeaderValue(v->header) == subtype_VectorValidHashing)
		  v->header =
		      (subtype_VectorMustRehash << type_Bits) |
		      type_SimpleVector;
	      /* Look at each of the vector elements which can be any lisp object. */
	      for (i = 0; i < n; i++)
		  scavenge_1word_obj(&v->data[i]);
	      count = CEILING(n + 2, 2);
	  }
	  break;
      case type_CodeHeader:
	  count = scav_code_header(addr);
	  break;
	  /* We should never hit any of these, 'cause they occur buried in
	   * the middle of code objects (and handled by the code just above).
	   */
      case type_ClosureFunctionHeader:
      case type_ReturnPcHeader:
	  gc_abort();

	  /* Except while looking at an fdefn and wanting to ensure
	   * code object is looked at.
	   */
      case type_FunctionHeader:
	  scavenge_fcn_header((struct function *) addr);
	  break;
#if defined i386
      case type_ClosureHeader:
      case type_FuncallableInstanceHeader:
      case type_ByteCodeFunction:
      case type_ByteCodeClosure:
      case type_DylanFunctionHeader:
	  count = scav_closure_header((struct closure *) addr);
	  break;
#endif
      case type_WeakPointer:
	  count = scav_weak_pointer(addr);
	  break;
      case type_Fdefn:
	  /* We have to handle fdefn objects specially, so we can fix
	   * up the raw function address.
	   */
	  count = scav_fdefn(addr);
	  break;
      default:
	  {
	      OSC_t class = sct[TypeOf(header)];

	      switch (class.sc_kind) {
		case SC_IMMED:
		    count = 1;
		    break;
		case SC_ISBOXED:
		    {
			int i, words = 1 + HeaderValue(header);

			for (i = 1; i < words; i++)
			    scavenge_1word_obj(addr + i);
			count = CEILING(words, 2);
		    }
		    break;
		case SC_UNBOXED:
		case SC_STRING:
		case SC_VECTOR:	/* simple vector handled above */
		    count = sizeOfObject((obj_t) addr);
		    break;
		default:
		    gc_abort();
	      }
	  }
    }
    return count;
}

static void
logcopy(lispobj * addr, lispobj tagged, int hdr, lispobj to)
{
    if (log) {
	int kind = TypeOf(hdr);
	int words = sizeOfObject((obj_t) PTR(tagged));

	fprintf(log, "(copy #x%x @ #x%x (#x%x %d) to #x%x)\n",
		tagged, addr, kind, words, to);
    }
}
static void
maybe_transport(lispobj * addr, lispobj tagged, struct region *region)
{
    obj_t obj = (obj_t) PTR(tagged);

    gc_assert(ALIGNEDP(obj));
    gc_assert(Pointerp(tagged));
    gc_assert((void *) region != (void *) obj);
#if 0
    if ((void *) obj == (void *) trapaddr)
	check_trap(obj);
#endif
    if (region->contains_small_objects) {
	lispobj new = copy(tagged);

#if 0
	if (scav_ro > 1)	/* debugging in RO space */
	    check_trap(obj);
#endif
#if defined GOOSE_CHASE
	if (TypeOf(obj->header) == type_Fdefn) {
	    struct fdefn *fdefn = (struct fdefn *) PTR(new);

	    if (fdefn->function < STATIC_SPACE_START)
		check_trap(obj);
	}
#endif
	dprintf(0, ("copy %x @ %x (%x) to %x\n",
		    tagged, addr, TypeOf(obj->header), new));
	logcopy(addr, tagged, obj->header, new);
	DEPOSIT_FORWARDING_PTR(obj, new);
	*addr = new;
    } else {
	move_to_newspace(region);
	dprintf(0, ("move %x\n", region));
    }
}

void
scavenge_pointer(lispobj * addr)
{
    lispobj taggedobj = *addr;	/* descriptor */
    obj_t obj = (obj_t) PTR(taggedobj);	/* pointer to object */

    gc_assert(Pointerp(taggedobj));
#if 0
    if (addr == trapaddr)
	check_trap(addr);
    if (obj == trapaddr)
	check_trap(obj);
#endif
    /* optimize out common static pointers */
    if (taggedobj != NIL && taggedobj != T) {
	struct region *region = find_region(obj);

	/* Only interested in pointers into oldspace */
	if (region && region->space == oldspace) {
	    if (FORWARDED(obj))
		*addr = FORWARDING_PTR(obj);
	    else
		switch (LowtagOf(taggedobj)) {
		  case type_ListPointer:
		      dprintf(noise > 1, ("ListPointer @ %x...\n", addr));
		      *addr = trans_list(taggedobj);
		      dprintf(noise > 1, ("... -> %x\n", addr));
		      break;
		  case type_FunctionPointer:
		      switch (TypeOf(obj->header)) {
			case type_ClosureHeader:
			case type_FuncallableInstanceHeader:
			case type_ByteCodeFunction:
			case type_ByteCodeClosure:
			case type_DylanFunctionHeader:
			    maybe_transport(addr, taggedobj, region);
			    break;
			default:
			    gc_abort();
		      }
		      break;
		  case type_InstancePointer:
		  case type_OtherPointer:
		      maybe_transport(addr, taggedobj, region);
		      break;
		  default:
		      /* It was a pointer, but not one of them? */
		      gc_abort();
		}
	}
    }
}


static lispobj *
scavenge(lispobj * addr, int ptrs)
{
    /* addr points to an aligned 32-bit word in some space. */
    struct region *region;
    lispobj *end = addr + ptrs;
    lispobj obj;

    while (addr < end) {
	int count = 1;

	obj = *addr;		/* the lisp object */
#if 0
	if (trapaddr == addr)
	    check_trap(addr);	/* gdb breakpoint */
#endif
	if (Pointerp(obj))	/* lowtag odd      */
	    scavenge_pointer(addr);
	else if (obj & 0x3)
	    /* some other immediate */
	    /*
	       * Some random header. Process some type dependent number
	       * of words. May still be inside object after call and the
	       * next cell can be any lisp object. We can either recurse
	       * by calling scavenge here or let the caller do it.
	     */
	    count = scavenge_random_object(addr);
	else
	    IMMED_OR_LOSE(obj);

	addr += count;
    }
    return addr;
}

static void
scavenge_cons(lispobj * where)
{
    /* Scavenge a two-word space */
    scavenge_1word_obj(where + 0);	/* car */
    scavenge_1word_obj(where + 1);	/* cdr */
}
static lispobj *
scavenge_object(lispobj * start)
{
    int length = sizeOfObject((obj_t) start);
    int words = scavenge_random_object(start);

    gc_assert(length == words);
    return start + length;
}
static lispobj *
scavengex(lispobj * obj)
{
    /* Thing at this location is one of:
     * a - basic object with header.
     * b - cons object (no header).
     * so that the starting and ending addresses are aligned.
     */
    gc_assert(ALIGNEDP(obj));
    {
	lispobj first_word = *obj;
	OSC_t sc = sct[TypeOf(first_word)];

	if (Pointerp(first_word) || sc.sc_kind == SC_IMMED) {	/* Must be a cons object or unused space */
	    scavenge_cons((lispobj *) obj);
	    return obj + 2;
	} else {		/* Must be a complex object with header */
	    lispobj *next = scavenge_object(obj);

	    return next;
	}
    }
}

static void
scavenge_space(lispobj * where, int words, char *name)
{
    int allocated = bytes_allocated;
    lispobj *end = where + words;
    lispobj *last;

    bytes_copied = 0;
    if (name)
	dprintf(noise, ("  %s", name));
    while (where < end) {
	last = where;
	where = scavengex(last);
    }
    gc_assert(where == end);
    if (name)
	dprintf(noise, (" %ld bytes moved, %ld bytes allocated.\n",
			bytes_copied, bytes_allocated - allocated));
}

static int boxed_registers[] = BOXED_REGISTERS;
static void
preserve_interrupt_context(os_context_t * context)
{
    int i;

    /* Check each boxed register for a valid pointer and promote
     * its region when found.
     */
    for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
	int index = boxed_registers[i];
	lispobj foo = SC_REG(context, index);
	struct region *region = find_region((void *) foo);

	if (region && region->space == oldspace)
	    move_to_newspace(region);
    }
}
static void
preserve_interrupt_contexts(void)
{
    int i, index;
    os_context_t *context;

    index = fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX));
    dprintf(noise, ("Number of active contexts: %d\n", index));

    for (i = 0; i < index; i++) {
	context = lisp_interrupt_contexts[i];
	preserve_interrupt_context(context);
    }
}


static void
flip_spaces()
{
    struct space *temp = oldspace;

    oldspace = newspace;
    newspace = temp;
}

/* There should be no lisp objects on the C stack so will limit search
 * to just the assigned lisp stack area.
 */
#if defined i386
#define BOS (CONTROL_STACK_START+CONTROL_STACK_SIZE)	/* x86-validate.h */
/* Traverse stack in same direction as it was loaded to try and
 * preserve original ordering of pages. Good for the VM system I hope.
 */
#define ACROSS_STACK(var) var=(void**)BOS-1; var > (void**)&var; var--
#endif

void
preserve_pointer(void *ptr)
{
    if (ptr > heap_base && ptr < heap_end) {
	struct region *region = find_region(ptr);

	if (region != NULL && region->space == oldspace) {
	    dprintf(0, ("move %x\n", ptr));
	    move_to_newspace(region);
	}
    }
}

static void
preserve_stack()
{
    void **addr;		/* auto var is current TOS */

    for (ACROSS_STACK(addr))
	preserve_pointer(*addr);
}

#ifdef CONTROL_STACKS
/* Scavenge the thread stack conservative roots. */
void
scavenge_thread_stacks(void)
{
    lispobj thread_stacks = SymbolValue(CONTROL_STACKS);
    int type = TypeOf(thread_stacks);

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
		int length, j;

		if (TypeOf(stack->header) != type_SimpleArrayUnsignedByte32)
		    return;
		length = fixnum_value(stack->length);
		/* fprintf(stderr,"Scavenging control stack %d of length %d words\n",
		   i,length); */
		for (j = 0; j < length; j++)
		    preserve_pointer((void *) stack->data[j]);
	    }
	}
    }
}
#endif

static void
zero_stack()
{
    /* This is a bit tricky because we don't want to zap any
     * stack frames between here and the call to mmap. For now,
     * lets just be slow and careful.
     */
    long *p, *q;
    os_vm_address_t base = (os_vm_address_t) CONTROL_STACK_START;
    os_vm_size_t size = (char *) &base - (char *) base;

#if 0
    cgc_zero(base, size);
#else
    p = (long *) base;
    q = (long *) &size;
    while (p < q)
	*p++ = 0;
#endif

}

#if defined STATIC_BLUE_BAG
static int fast_static = 1;
static void
scavenge_static()
{
    /* Static space consists of alternating layers of
     * code objects that refer to read-only space (from purify),
     * static non-code objects that need looking at, and
     * newly loaded code objects that refer to dynamic space.
     * The number of these areas depends on how many times purify
     * ran while loading the system image. I will extend purify
     * to maintain a list of collectable areas and use that list
     * here to avoid scanning read-only code sections.
     */
    lispobj *ss0 = (lispobj *) PTR(NIL);
    lispobj *ssa = (lispobj *) (PTR(STATIC_BLUE_BAG));
    lispobj *ssz = (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER);
    lispobj bag = SymbolValue(STATIC_BLUE_BAG);
    lispobj *end = NULL;

    ssa += sizeOfObject(OBJECT_AT(ssa));	/* Genesis modifies plist entry */
    if (fast_static) {
	scavenge_space(ss0, ssa - ss0, "Static0");
	if (bag != NIL && LowtagOf(bag) == type_ListPointer) {
	    char sbuf[128];
	    struct cons *cons = (struct cons *) PTR(bag);

	    while (LowtagOf(cons->car) == type_ListPointer) {
		struct cons *pair = (struct cons *) PTR(cons->car);
		lispobj *ss1 = (lispobj *) pair->car;
		lispobj *ss2 = (lispobj *) pair->cdr;

		if (end == NULL)
		    end = ss2;
		sprintf(sbuf, "Static %x %d", ss1, ss2 - ss1);
		scavenge_space(ss1, ss2 - ss1, sbuf);
		if (cons->cdr != NIL && LowtagOf(cons->cdr) == type_ListPointer)
		    cons = (struct cons *) PTR(cons->cdr);
		else
		    break;
	    }
	}
	if (end != NULL)
	    scavenge_space(end, ssz - end, "Static");
    } else
	(scavenge_space(ss0, ssz - ss0, "Static-All"));
}
#endif

static void
scavenge_roots()
{
    /* So what should go here?
     * When cgc starts up after purify/save all live objects
     * are in read-only or static space, and anything in RO
     * can point only to RO or STATIC and can't be changed.
     * Anything in STATIC is subject to change (but not move).
     * . not read-only-space      (probably most of the roots here)
     * . static-space             (all compiled code at least)
     * . binding-stack
     * . weak-pointers
     ?   do I allow GC from interrupt?)
     * . interrupt-context        (regs same as stack)
     ****
     * Well, it turns out that RO space ain't exactly that as
     * somehow apparently cached 'compact-info-environment' stuff
     * modifies at least 2 locations in RO space. There is a note
     * in globaldb.lisp that alludes to this and provides a post GC
     * hook to blow the cache. Not a problem if gc is called from
     * the lisp wrapper. UPDATE: Found purify bug which forced
     * boxed vectors into RO. This may be what led to above.
     *
     */
    lispobj *rs0 = (lispobj *) READ_ONLY_SPACE_START;
    lispobj *rsz = (lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER);
    lispobj *ss0 = (lispobj *) STATIC_SPACE_START;
    lispobj *ssz = (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER);
    lispobj *bs0 = (lispobj *) BINDING_STACK_START;
    lispobj *bsz = (lispobj *) SymbolValue(BINDING_STACK_POINTER);

    if (scav_ro) {
	scav_ro++;
	scavenge_space(rs0, rsz - rs0, "RO");
	scav_ro--;
    }
    scavenge_static();
    scavenge_space(bs0, bsz - bs0, "Binding Stack");

    dprintf(noise, ("Interrupt handlers (%u bytes) ...\n",
		    sizeof(interrupt_handlers)));

    scavenge((lispobj *) interrupt_handlers,
	     sizeof(interrupt_handlers) / sizeof(lispobj));

}

static void
scavenge_newspace()
{
    /* Scavenge is going to start at the beginning of newspace which
     * is presumed to have some "root" object pointers lying about due
     * to promoting regions that may be aimed at by stack resident pointers,
     * copied small objects from scavenge_roots(), or promoted large_object
     * regions. Scavenge() will flush out more copied objects/promoted
     * regions that will get added to the end of newspace and eventually
     * scanned by this code -- until all referenced things (and maybe some
     * extra dead stuff) have been examined. At the end of this loop anything
     * in oldspace is trash.
     */
    struct region *current;

    current = newspace->regions;

    while (current != NULL) {
	if (current->contains_small_objects) {
	    void *obj = (char *) current + REGION_OVERHEAD;
	    void *end = (char *) current + current->num_chunks * CHUNK_BYTES;

	    while (obj < end)
		obj = SCAVENGE_OBJECT(OBJECT_AT(obj));
	    gc_assert(obj == end);
	} else
	    SCAVENGE_OBJECT(OBJECT_AT(((char *) current + REGION_OVERHEAD)));
	current = current->next;
    }
}

static void
free_oldspace()
{
    struct region *region, *next;

    chunks_freed = 0;
    for (region = oldspace->regions; region != NULL; region = next) {
	gc_assert(region->space != newspace);
	next = region->next;
	free_region(region);
    }
    oldspace->alloc_ptr = NULL;
    oldspace->alloc_end = NULL;
    oldspace->regions = NULL;
    oldspace->regions_tail = &oldspace->regions;
}

static void
verify_space(lispobj * start, size_t words)
{
    while (words > 0) {
	size_t count = 1;
	lispobj thing = *(lispobj *) start;

	if (Pointerp(thing)) {
	    struct region *region = find_region((void *) thing);

	    if (region && region->space == NULL)
		fprintf(stderr, "Ptr %x @ %x sees Junk\n", thing, start);
	} else if (thing & 0x3) {
	    obj_t obj = (obj_t) start;

	    switch (TypeOf(obj->header)) {
	      case type_CodeHeader:
		  {
		      lispobj object = *start;
		      struct code *code;
		      int nheader_words, ncode_words, nwords;
		      lispobj fheaderl;
		      struct function *fheaderp;

		      code = (struct code *) start;
		      ncode_words = fixnum_value(code->code_size);
		      nheader_words = HeaderValue(object);
		      nwords = ncode_words + nheader_words;
		      nwords = CEILING(nwords, 2);
		      /* Scavenge the boxed section of the code data block */
		      verify_space(start + 1, nheader_words - 1);

		      /* Scavenge the boxed section of each function object in the
		       * code data block.
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
	      default:
		  {
		      OSC_t class = sct[TypeOf(obj->header)];

		      switch (class.sc_kind) {
			case SC_ISBOXED:
			case SC_IMMED:
			    count = 1;
			    break;
			case SC_UNBOXED:
			case SC_STRING:
			case SC_VECTOR:
			    count = sizeOfObject((obj_t) start);
			    break;
			default:
			    gc_abort();
		      }
		      break;
		  }
	    }
	}
	start += count;
	words -= count;
    }
}

/* For debug/test only. */
static void
verify_gc()
{
    lispobj *rs0 = (lispobj *) READ_ONLY_SPACE_START;
    lispobj *rsz = (lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER);
    lispobj *ss0 = (lispobj *) STATIC_SPACE_START;
    lispobj *ssz = (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER);
    lispobj *bs0 = (lispobj *) BINDING_STACK_START;
    lispobj *bsz = (lispobj *) SymbolValue(BINDING_STACK_POINTER);
    lispobj *cs0 = (lispobj *) & rs0;
    lispobj *csz = (lispobj *) BOS;

    /* can't check stack easily because there may be non-valid 
     * objects there (thats why we're doing this cgc stuff). In
     * particular there are raw return addresses which can be very
     * descriptorish looking!!!

     verify_space(cs0, csz-cs0);
     */
    verify_space(rs0, rsz - rs0);
    verify_space(ss0, ssz - ss0);
    verify_space(bs0, bsz - bs0);
}
static void
fixup_regions(struct region *region)
{
    do {
	lispobj header = (lispobj) OBJECT_AT(region)->header;

	if (static_space_p(header)) {
	    /* Purify thought this header was a cons? Why? */
	    struct cons *wrong = (struct cons *) PTR(header);
	    struct cons *fixme = (struct cons *) region;

	    dprintf(1, ("\n--Fixing region header @ %x.", region));
	    fixme->car = wrong->car;	/* restore region header */
	    fixme->cdr = wrong->cdr;	/* restore prev pointer  */
	    wrong->car = wrong->cdr = 0;
	}
	region = region->next;
    }
    while (region != NULL);
}

static void
post_purify_fixup(struct space *space)
{
    /* Purify may have messed up the region headers. This can happen
     * if there is a dead list pointer on the stack that now aims
     * at a region header (previously was valid memory). Purify attempts
     * to at least check for a valid object header but loses with lists.
     * This hack recovers the correct values and keeps us going. Can
     * this occur with other dead objects?
     */
    if (large_region_free_list)
	fixup_regions(large_region_free_list);
    if (small_region_free_list)
	fixup_regions(small_region_free_list);
    fixup_regions(space->regions);
}


static int dolog = 0;		/* log copy ops to file */
static int dover = 0;		/* hunt pointers to oldspace */
void
cgc_collect_garbage()
{
    unsigned long allocated = bytes_allocated;

    dprintf(noise, ("GC\n"));
    if (dolog && !log)
	log = fopen("LOG.tmp", "w");

    /* Initialize the weak pointer list. */
    weak_pointers = NULL;

    dprintf(noise, ("[Flip Spaces]\n"));
    flip_spaces();
    preserve_interrupt_contexts();
    dprintf(noise, ("[Preserve Stack]\n"));
    preserve_stack();
    scavenge_thread_stacks();
    dprintf(noise, ("[Scavenge Roots]\n"));
    scavenge_roots();
    dprintf(noise, ("[Scavenge New]\n"));
    scavenge_newspace();
    scan_weak_pointers();
    dprintf(noise, ("[Free Oldspace]\n"));
    free_oldspace();
    if (dover) {
	dprintf(noise, ("[Checking]\n"));
	verify_gc();
    }
    dprintf(noise, ("[Compacting]\n"));
    compact_free_regions();
    /* The stack will be zeroed by scrub-control-stack in sub-gc which
       is more effecient. */
    /* zero_stack(); */
    if (log)
	fclose(log);
    log = NULL;
    dprintf(noise, ("  %ld bytes copied.\n", (bytes_allocated - allocated)));
    dprintf(noise, ("  %ld bytes (%ld pages) reclaimed.\n",
		    chunks_freed * CHUNK_BYTES, chunks_freed));
    bytes_allocated -= chunks_freed * CHUNK_BYTES;
    maybe_gc_called = 0;
}

void
cgc_free_heap()
{
    /* Like above but just zap everything 'cause purify has
     * cleaned house!
     */
    unsigned long allocated = bytes_allocated;

    flip_spaces();
    post_purify_fixup(oldspace);
    free_oldspace();
    compact_free_regions();
#if 0				/* purify is currently running on the C stack so don't do this */
    zero_stack();
#endif
    bytes_allocated -= chunks_freed * CHUNK_BYTES;
}


void
cgc_init_collector()
{
    int max_blocks;

    heap_base = (void *) DYNAMIC_0_SPACE_START;

    /* I could actually use both spaces here but just 1 for now */
    heap_end = (char *) heap_base + dynamic_space_size;

    max_blocks = BLOCK_NUMBER(heap_end) - BLOCK_NUMBER(heap_base);
    if ((block_table_base = malloc(sizeof(struct cluster *) * max_blocks))
	!= NULL) {
	memset(block_table_base, 0, sizeof(struct cluster *) * max_blocks);

	block_table = (block_table_base - BLOCK_NUMBER(heap_base));

	space_0.regions_tail = &space_0.regions;
	space_1.regions_tail = &space_1.regions;

	newspace = &space_0;
	oldspace = &space_1;
    } else
	perror("malloc cgc block table");
    init_osc();			/* Object Storage Class table */
}


void do_pending_interrupt(void);

int use_cgc_p = 0;
char *
alloc(int nbytes)
{
    /* Alloc is only called from lisp code to allocate a number of
       words, the cgc GC uses cgc_alloc directly as the checks of the
       heap size and is not needed and interrupts are allways disabled
       during a GC. */

    /* Assumes nbytes includes alignment. Python arranges for that
     * but the C startup code needed some help.
     */
#if 0
    int bytes = (nbytes + (ALIGN_BYTES - 1)) & ~(ALIGN_BYTES - 1);

    if (bytes != nbytes)
	fprintf(stderr, "Fixing unaligned allocation length %d.\n", nbytes);
    nbytes = bytes;
#endif
    if (!use_cgc_p) {
	char *current = (char *) SymbolValue(ALLOCATION_POINTER);
	char *nxtfree = current + nbytes;

	SetSymbolValue(ALLOCATION_POINTER, (lispobj) nxtfree);
	return current;
    } else {
	/* Lacking an interrupt driven scheme to notice when a GC might
	 * be wise, we add some more overhead to the allocator here
	 * before any needed state is acquired. Only need to do it once
	 * though because lisp will remember *need to collect garbage*
	 * and get to it when it can.  */
	if (auto_gc_trigger	/* Only when enabled */
	    && bytes_allocated > auto_gc_trigger && !maybe_gc_called++)	/* Only once         */
	    funcall0(SymbolFunction(MAYBE_GC));

	if (SymbolValue(INTERRUPTS_ENABLED) == NIL)
	    /* Interrupts are disable so no special care is needed */
	    return cgc_alloc(nbytes);
	else {
	    void *result;

	    /* Interrupts are enabled so set *interrupt-enabled* to nil
	       before calling cgc_alloc to prevent cgc_alloc from being
	       re-entered. */
	    SetSymbolValue(INTERRUPTS_ENABLED, NIL);

	    result = cgc_alloc(nbytes);

	    /* Restore *interrupts-enabled* */
	    SetSymbolValue(INTERRUPTS_ENABLED, T);

	    /* Check if an interrupt occured */
	    if (SymbolValue(INTERRUPT_PENDING) == T)
		/* Handle any interrupts that occured during cgc_alloc */
		do_pending_interrupt();

	    return result;
	}
    }
}

/* Interface to history. */
void
set_auto_gc_trigger(unsigned long dynamic_usage)
{
    auto_gc_trigger += dynamic_usage;
}

void
clear_auto_gc_trigger(void)
{
    auto_gc_trigger = 0;
}

void
gc_init(void)
{
    cgc_init_collector();
}

void
collect_garbage()
{
    /* SUB-GC wraps without-interrupt around call, but this
     * is going to absolutely block SIGINT.
     */
    /* #define REALLY_SAFE */
#if defined REALLY_SAFE
    sigset_t newmask, oldmask;

    sigemptyset(&newmask);
    sigaddset(&newmask, SIGINT);
    sigprocmask(SIG_BLOCK, &newmask, &oldmask);
#endif
    cgc_collect_garbage();
#if defined REALLY_SAFE
    sigprocmask(SIG_SETMASK, &oldmask, NULL);
#endif

}

/* Some helpers for the debugger. */

/* Scan an area looking for an object which encloses the given
   pointer. Returns the object start on success or NULL on failure. */
static lispobj *
search_space(lispobj * start, size_t words, lispobj * pointer)
{
    while (words > 0) {
	size_t count = 1;
	lispobj thing = *start;

	/* If thing is an immediate then this is a cons */
	if (Pointerp(thing)
	    || ((thing & 3) == 0)	/* fixnum */
	    ||(TypeOf(thing) == type_BaseChar)
	    || (TypeOf(thing) == type_UnboundMarker))
	    count = 2;
	else
	    count = sizeOfObject((obj_t) start);

	/* Check if the pointer is within this object? */
	if ((pointer >= start) && (pointer < (start + count))) {
	    /* Found it. */
	    /*          fprintf(stderr,"* Found %x in %x %x\n",pointer, start, thing); */
	    return (start);
	}

	/* Round up the count */
	count = CEILING(count, 2);

	start += count;
	words -= count;
    }
    return (NULL);
}

static lispobj *
search_read_only_space(lispobj * pointer)
{
    lispobj *start = (lispobj *) READ_ONLY_SPACE_START;
    lispobj *end = (lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER);

    if ((pointer < start) || (pointer >= end))
	return NULL;
    return (search_space(start, (pointer + 2) - start, pointer));
}

static lispobj *
search_static_space(lispobj * pointer)
{
    lispobj *start = (lispobj *) STATIC_SPACE_START;
    lispobj *end = (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER);

    if ((pointer < start) || (pointer >= end))
	return NULL;
    return (search_space(start, (pointer + 2) - start, pointer));
}

/* Find the code object for the given pc. Return NULL on failure */
lispobj *
component_ptr_from_pc(lispobj * pc)
{
    lispobj *object = NULL;

    if (object = search_read_only_space(pc));
    else
	object = search_static_space(pc);

    /* Found anything? */
    if (object)
	/* Check if it is a code object. */
	if (TypeOf(*object) == type_CodeHeader)
	    return (object);

    return (NULL);
}
