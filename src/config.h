/* impl.h.config: MPS CONFIGURATION
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!config.h(MMdevel_annotation.1) $
 */

#ifndef config_h
#define config_h

/* Platform Configuration */

#include "mpstd.h"


/* Variety Configuration
 *
 * Convert CONFIG_VAR_* defined on compiler command line into
 * internal configuration parameters.  See design.mps.config.var.
 */

#if defined(CONFIG_VAR_HI)      /* Hot, Internal */
#define MPS_HOT
#elif defined(CONFIG_VAR_CI)    /* Cool, Internal */
#elif defined(CONFIG_VAR_TI)    /* Telemetry, Cool, Internal (temp) */
#define EVENT
#elif defined(CONFIG_VAR_HE)    /* Hot, External */
#define MPS_HOT
#elif defined(CONFIG_VAR_CE)    /* Cool, External */
#elif defined(CONFIG_VAR_TE)    /* Telemetry, Cool, External (temp) */
#define EVENT
#else
#error "No target variety configured."
#endif


/* Product Configuration
 *
 * Convert CONFIG_PROD_* defined on compiler command line into
 * internal configuration parameters.  See design.mps.config.prod.
 *
 * .prod.arena-size: ARENA_SIZE is currently set larger for the
 * MM/Dylan product as an interim solution.
 * See request.dylan.170170.sol.patch and change.dylan.buffalo.170170.
 *
 * .mac.arena-size: ARENA_SIZE is set to 2Mb when compiling on
 * os.s7 (Mac OS) with the VM Arena, in order to allow development in
 * an environment where real memory is used to simulate memory mapping.
 * 
 * ARENA_CLIENT_PAGE_SIZE is the size in bytes of a "page" (i.e. segment
 * granule) in the client arena. The number 8192 is an initial value with no
 * particular justification.
 *
 * ARENA_CLIENT_DEFAULT_SEG_HIGH is a Bool governing whether segments default
 * 'high' (TRUE) or 'low' (FALSE). For EP-core, non-DL segments should be high
 * to reduce fragmentation of DL pools (See req.epcore.attr.footprint
 * and change 170193/trapping.beta.3
 */

#if defined(CONFIG_PROD_EPCORE)
#define MPS_PROD_EPCORE
#define ARENA_CLIENT
#define ARENA_SIZE                      ((Size)0) /* bogus for client arena */
#define ARENA_CLIENT_PAGE_SIZE          ((Size)8192)
#define ARENA_CLIENT_DEFAULT_SEG_HIGH   TRUE
#elif defined(CONFIG_PROD_DYLAN)
#define MPS_PROD_DYLAN
#define ARENA_SIZE              ((Size)1<<30)
#define AMC_SIZE_LIMIT		((Size)64<<20)  /* experimentally reasonable limit */
#elif defined(CONFIG_PROD_MPS)
#define MPS_PROD_MPS
#ifdef MPS_OS_S7
#define ARENA_SIZE              ((Size)2<<20)
#define AMC_SIZE_LIMIT		ARENA_SIZE
#else
#define ARENA_SIZE              ((Size)64<<20)
#define AMC_SIZE_LIMIT		ARENA_SIZE
#endif /* MPS_OS_S7 */
#else
#error "No target product configured."
#endif


/* Space Configuration -- see impl.c.space */

#define SPACE_CONTROL_EXTENDBY  ((Size)4096)
#define SPACE_CONTROL_AVGSIZE   ((Size)32)
#define SPACE_CONTROL_MAXSIZE   ((Size)65536)
#define SPACE_POLL_MAX          ((Size)262144)
#define SPACE_LD_LENGTH         ((Size)4)


/* Arena Configuration -- see impl.c.arena* */

#define ARENA_ANSI_ALIGN        ((Align)4096)
#define ARENA_ANSI_ZONESHIFT    ((Shift)20)


/* Shield Configuration -- see impl.c.shield */

#define SHIELD_CACHE_SIZE       ((Size)2)
#define SHIELD_DEPTH_WIDTH	((Size)4)
#define SHIELD_DEPTH		((Count)1<<SHIELD_DEPTH_WIDTH)


/* VM Configuration -- see impl.c.vm* */

#define VMAN_ALIGN              ((Align)4096)
#define VM_JUNKBYTE             ((unsigned char)0xA9)
#define VMRM_ALIGN              ((Align)4096)


/* Tracer Configuration -- see impl.c.trace */

#define TRACE_MAX               ((Size)1)


/*  impl.c.event
 *
 *  EVENT_BUFFER_SIZE is the number of words in the global event buffer.
 *  EVENT_HEADER_SIZE is the number of words in each event header
 */

#define EVENT_BUFFER_SIZE       ((Count)4096)
#define EVENT_HEADER_SIZE       ((Count)3)

#endif /* config_h */
