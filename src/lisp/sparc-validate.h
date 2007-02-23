/*

 $Header: /project/cmucl/cvsroot/src/lisp/sparc-validate.h,v 1.21 2005/09/15 18:26:52 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _SPARC_VALIDATE_H_
#define _SPARC_VALIDATE_H_

/*
 * Address map:
 *
 *	0x00000000->0x0f800000  248M C code and stuff(?)
 *      0x0f800000->0x10000000    8M for linkage table area
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Control stack growing up.
 *	0x40000000->0x80000000 1024M Dynamic space 1
 *	0x80000000->0xc0000000 1024M Dynamic space 2
 *      0xc0000000->0xffffffff 1024M C stack, dynamic libs, etc.      
 *
 * With GENCGC:
 *
 *	0x00000000->0x0f800000  248M C code and stuff(?)
 *      0x0f800000->0x10000000    8M for linkage table area
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Control stack growing up.
 *	0x40000000->0xf0000000 2816M Dynamic space 1
 *      0xf0000000->0xffffffff 1024M C stack, dynamic libs, etc.      
 *
 * But look at the definitions below to see how much is really
 * allocated.  The numbers above are the maximums allowed.  We might
 * use less.
 *
 * Almost.  We leave a hole of size 32 KB at the end of each of these
 * spaces.
 *
 *
 * It may be possible to increase the size of the dynamic spaces even
 * further, but Casper H.S. Dik says shared libraries are loaded
 * directly under the stack, so we need to leave some space for the C
 * stack and shared libraries.  He also says the top of stack is
 * 0xffbf0000 for Ultrasparcs in Solaris 7+, but it's 0xf0000000 for
 * sun4m (and 4u in S2.6-) 0xe0000000 for sun4d)
 *
 * Shared libraries can be mapped anywhere there's room.
 */

/* Need this to define the spaces described in Lisp */

/*#include "internals.h"*/

/* 128 MB */
#define MB_128	(0x08000000)
/*
 *
 * Note: I'm not sure why, but the sizes must be on a
 * SPARSE_BLOCK_SIZE (32 KB) boundary.  (See seg_force_resident in
 * sunos-os.c.  If not, then mapping the holes causes segfaults in
 * initialization.)
 *
 * Sparse block size must be larger than the system page size.
 */

#define SPARSE_BLOCK_SIZE (1<<15)
#define SPARSE_SIZE_MASK (SPARSE_BLOCK_SIZE-1)


#ifdef LINKAGE_TABLE
/*
 * This space start better match the value of
 * target-foreign-linkage-space-start defined in sparc/parms.lisp!
 *
 * See the notes there!
 */

#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)

/*
 * This allows for about 510K symbols (assuming each entry is 16 bytes
 * long).  Hope that's enough!  Make sure this doesn't overlap the
 * READ_ONLY_SPACE_START!
 */
#define FOREIGN_LINKAGE_SPACE_SIZE  (0x00800000 - SPARSE_BLOCK_SIZE)	/* 8 MB - 32 KB */
#endif


#define READ_ONLY_SPACE_START	(SpaceStart_TargetReadOnly)
#define READ_ONLY_SPACE_SIZE	((2*MB_128) - SPARSE_BLOCK_SIZE)	/* 256 MB - 32 KB, 256 MB max */

#define BINDING_STACK_START 	(0x20000000)
#define BINDING_STACK_SIZE  	(MB_128 - SPARSE_BLOCK_SIZE)	/* 128 MB - 32 KB, 128 MB max */

#define STATIC_SPACE_START  	(SpaceStart_TargetStatic)
#define STATIC_SPACE_SIZE   	((2*MB_128) - SPARSE_BLOCK_SIZE)	/* 256 MB - 32 KB, 256 MB max */

#define CONTROL_STACK_START 	(0x38000000)
#define CONTROL_STACK_SIZE  	(MB_128 - SPARSE_BLOCK_SIZE)	/* 128 MB - 32 KB, 128 MB max */
#define CONTROL_STACK_END       (CONTROL_STACK_START + CONTROL_STACK_SIZE)

#define DYNAMIC_0_SPACE_START	(SpaceStart_TargetDynamic)

/* This isn't used with GENCGC */
#define DYNAMIC_1_SPACE_START	(0x80000000)

/* The default dynamic space to allocate */
/*
 * On Solaris 10, Martin Rydstrom reports that subtracting off
 * SPARSE_BLOCK_SIZE causes CMUCL not to work (GC lossage).  But
 * specifying -dynamic-space-size 256 works fine.  I don't understand
 * how that can be, but we lets not subtract it off.
 */
#if 0
#define DEFAULT_DYNAMIC_SPACE_SIZE  	(0x10000000 - SPARSE_BLOCK_SIZE)	/* 256 MB - 32 KB */
#else
#define DEFAULT_DYNAMIC_SPACE_SIZE  	(0x10000000)	/* 256 MB */
#endif

/* The maximum dynamic space that we can allocate */
#ifdef GENCGC
/*
 * For GENCGC, we can use both dynamic spaces, so we get at least
 * double the heap size.
 */
#define DYNAMIC_SPACE_SIZE      (0xB0000000 - SPARSE_BLOCK_SIZE)	/* 2816 MB - 32 KB max */
#else
#define DYNAMIC_SPACE_SIZE      (0x40000000 - SPARSE_BLOCK_SIZE)	/* 1GB - 32 KB max */
#endif

#endif /* _SPARC_VALIDATE_H_ */
