/*
 *
 * This code was written as part of the CMU Common Lisp project at
 * Carnegie Mellon University, and has been placed in the public domain.
 *
 *  $Header: /project/cmucl/cvsroot/src/lisp/x86-validate.h,v 1.25 2005/09/15 18:26:53 rtoy Exp $
 *
 */

#ifndef _X86_VALIDATE_H_
#define _X86_VALIDATE_H_

/*
 * Address map:
 *
 *  FreeBSD:
 *	0x00000000->0x0E000000  224M C program and memory allocation.
 *	0x0E000000->0x10000000   32M Foreign segment.
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Reserved for shared libraries.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Binding stack growing up.
 *	0x40000000->0x48000000  128M Control stack growing down.
 *	0x48000000->0xB0000000 1664M Dynamic Space.
 *      0xB0000000->0xB1000000       Foreign Linkage Table
 *	0xE0000000->            256M C stack - Alien stack.
 *
 *  OpenBSD:
 *	0x00000000->0x0E000000  224M C program and memory allocation.
 *	0x0E000000->0x10000000   32M Foreign segment.
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Control stack growing down.
 *	0x40000000->0x48000000  128M Reserved for shared libraries.
 *	0x48000000->0xB0000000 1664M Dynamic Space.
 *      0xB0000000->0xB1000000   16M Foreign Linkage Table
 *	0xE0000000->            256M C stack - Alien stack.
 *
 *  NetBSD:
 *	0x00000000->0x0E000000  224M C program and memory allocation.
 *	0x0E000000->0x10000000   32M Foreign segment.
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Binding stack growing up.
 *	0x40000000->0x48000000  128M Control stack growing down.
 *	0x48800000->0xB0000000 1656M Dynamic Space.
 *      0xB0000000->0xB1000000   16M Foreign Linkage Table
 *	0xE0000000->            256M C stack - Alien stack.
 *
 *  Linux:
 *	0x00000000->0x08000000  128M Unused.
 *	0x08000000->0x10000000  128M C program and memory allocation.
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Control stack growing down.
 *	0x40000000->0x48000000  128M Reserved for shared libraries.
 *	0x58000000->0xBE000000 1632M Dynamic Space.
 *      0xBE000000->0xBF000000   16M Foreign Linkage Table
 *      0xBFFF0000->0xC0000000       Unknown Linux mapping
 *
 *      (Note: 0x58000000 allows us to run on a Linux system on an AMD
 *      x86-64.  Hence we have a gap of unused memory starting at
 *      0x48000000.)
 */

#ifdef __FreeBSD__
#define READ_ONLY_SPACE_START   (0x10000000)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(0x28f00000)
#define STATIC_SPACE_SIZE	(0x0f0ff000)	/* 241MB - 1 page */

#define BINDING_STACK_START	(0x38000000)
#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

#define CONTROL_STACK_START	0x40000000
#define CONTROL_STACK_SIZE	0x07fd8000	/* 128MB - SIGSTKSZ */
#define SIGNAL_STACK_START	0x47fd8000
#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define DYNAMIC_0_SPACE_START	(0x48000000)
#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x40000000)	/* May be up to 2GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000)	/* 64MB */
#endif
#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000)	/* 512MB */
#ifdef LINKAGE_TABLE
#define FOREIGN_LINKAGE_SPACE_START ((unsigned long) LinkageSpaceStart)
#define FOREIGN_LINKAGE_SPACE_SIZE (0x100000)	/* 1MB */
#endif
#endif /* __FreeBSD__ */


#ifdef __OpenBSD__
#define READ_ONLY_SPACE_START   (0x10000000)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(0x28000000)
#define STATIC_SPACE_SIZE	(0x0ffff000)	/* 256MB - 1 page */

#define BINDING_STACK_START	(0x38000000)
#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

#define CONTROL_STACK_START	(0x40000000)
#define CONTROL_STACK_SIZE	(0x07fd8000)	/* 128MB - SIGSTKSZ */

#define SIGNAL_STACK_START	(0x47fd8000)
#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define DYNAMIC_0_SPACE_START	(0x48000000)
#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x68000000)	/* 1.625GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000)	/* 64MB */
#endif
#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000)	/* 512MB */
#endif

#ifdef __NetBSD__
#define READ_ONLY_SPACE_START   (0x10000000)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(0x28000000)
#define STATIC_SPACE_SIZE	(0x0ffff000)	/* 256MB - 1 page */

#define BINDING_STACK_START	(0x38000000)
#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

#define CONTROL_STACK_START	(0x40000000)
#define CONTROL_STACK_SIZE	(0x07fd8000)	/* 128MB - SIGSTKSZ */

#define SIGNAL_STACK_START	(0x47fd8000)
#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define DYNAMIC_0_SPACE_START	(0x48800000)
#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x67800000)	/* 1.656GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000)	/* 64MB */
#endif
#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000)	/* 512MB */
#ifdef LINKAGE_TABLE
#define FOREIGN_LINKAGE_SPACE_START (0xb0000000)
#define FOREIGN_LINKAGE_SPACE_SIZE (0x100000)	/* 1MB */
#endif
#endif

#ifdef __linux__
#define READ_ONLY_SPACE_START   (SpaceStart_TargetReadOnly)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(SpaceStart_TargetStatic)
#define STATIC_SPACE_SIZE	(0x0ffff000)	/* 256MB - 1 page */

#define BINDING_STACK_START	(0x20000000)
#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

#define CONTROL_STACK_START	0x38000000
#define CONTROL_STACK_SIZE	(0x07fff000 - 8192)
#define SIGNAL_STACK_START	CONTROL_STACK_END
#define SIGNAL_STACK_SIZE	8192

#define DYNAMIC_0_SPACE_START	(SpaceStart_TargetDynamic)

#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x66000000)	/* 1.632GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000)	/* 64MB */
#endif
#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000)	/* 512MB */
#ifdef LINKAGE_TABLE
#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)
#define FOREIGN_LINKAGE_SPACE_SIZE (0x100000)	/* 1MB */
#endif
#endif


#define CONTROL_STACK_END	(CONTROL_STACK_START + CONTROL_STACK_SIZE)

/* Note that GENCGC only uses dynamic_space 0. */
#define DYNAMIC_1_SPACE_START	(DYNAMIC_0_SPACE_START + DYNAMIC_SPACE_SIZE)

#endif /* _X86_VALIDATE_H_ */
