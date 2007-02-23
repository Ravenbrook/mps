/*
 * $Header: /project/cmucl/cvsroot/src/lisp/validate.c,v 1.23 2005/09/15 18:26:53 rtoy Exp $
 *
 * Memory Validation
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#ifdef sparc
#include <alloca.h>
#endif

#include "lisp.h"
#include "os.h"
#include "globals.h"
#include "validate.h"
#include "internals.h"

unsigned long read_only_space_size = READ_ONLY_SPACE_SIZE;
unsigned long binding_stack_size = BINDING_STACK_SIZE;
unsigned long static_space_size = STATIC_SPACE_SIZE;
unsigned long control_stack_size = CONTROL_STACK_SIZE;

#ifdef sparc
extern void make_holes(void);
#endif

static void
ensure_space(lispobj * start, size_t size)
{
    if (os_validate((os_vm_address_t) start, size) == NULL) {
	fprintf(stderr,
		"ensure_space: Failed to validate %ld bytes at 0x%08lx\n",
		(unsigned long) size, (unsigned long) start);
	exit(1);
    }
}


/* We use the linker symbol redefinition trick here to get the dynamic
   space size when the core is built in to the executable.  Note that
   builtin_image_flag is used as a flag indicating that the lisp image
   is built into the executable.  FMG */
extern int builtin_image_flag;
long image_dynamic_space_size = 0;

void
validate(void)
{
    void *dynamic_space_data = NULL;

    /* Note: XXX use alloca here because it's not malloc.  I'm assuming
       that anyone who wants to make this scheme work this will be using
       GCC.  FMG */
    if (builtin_image_flag != 0)
	dynamic_space_data = alloca((int) (&image_dynamic_space_size));

    /* Read-Only Space */
    read_only_space = (lispobj *) READ_ONLY_SPACE_START;
    /* Don't try to map this space if the executable contains the
       image. */
    if (builtin_image_flag == 0)
	ensure_space(read_only_space, READ_ONLY_SPACE_SIZE);

    /* Static Space */
    static_space = (lispobj *) STATIC_SPACE_START;
    /* Don't try to map this space if the executable contains the
       image. */
    if (builtin_image_flag == 0)
	ensure_space(static_space, STATIC_SPACE_SIZE);

    /* Dynamic-0 Space */
    dynamic_0_space = (lispobj *) DYNAMIC_0_SPACE_START;
    if (builtin_image_flag != 0) {
	/* If the executable contains the lisp image, we want to copy the
	   data in the dynamic space out of its segment, then map the
	   dynamic space (which has the side effect of unmapping the
	   dynamic space segment in the executable), then copy the data
	   back into it.  This is necessary to make the data in the
	   dynamic space segment available to the new lisp process.  */
	memcpy(dynamic_space_data, dynamic_0_space,
	       (int) &image_dynamic_space_size);
	ensure_space(dynamic_0_space, dynamic_space_size);
	memcpy(dynamic_0_space, dynamic_space_data,
	       (int) &image_dynamic_space_size);
    } else
	ensure_space(dynamic_0_space, dynamic_space_size);

    current_dynamic_space = dynamic_0_space;

#ifndef GENCGC
    /* Dynamic-1 Space */
    dynamic_1_space = (lispobj *) DYNAMIC_1_SPACE_START;
    ensure_space(dynamic_1_space, dynamic_space_size);
#endif

    /* Control Stack */
    control_stack = (lispobj *) CONTROL_STACK_START;
#if (defined(i386) || defined(__x86_64))
    control_stack_end = (lispobj *) (CONTROL_STACK_START + CONTROL_STACK_SIZE);
#endif
    ensure_space(control_stack, CONTROL_STACK_SIZE);

#ifdef SIGNAL_STACK_START
    ensure_space((lispobj *) SIGNAL_STACK_START, SIGNAL_STACK_SIZE);
#endif

    /* Binding Stack */
    binding_stack = (lispobj *) BINDING_STACK_START;
    ensure_space(binding_stack, BINDING_STACK_SIZE);
#ifdef LINKAGE_TABLE
    ensure_space((lispobj *) FOREIGN_LINKAGE_SPACE_START,
		 FOREIGN_LINKAGE_SPACE_SIZE);
#endif
#ifdef sparc
    make_holes();
#endif

#ifdef PRINTNOISE
    printf(" done.\n");
#endif

#ifdef RED_ZONE_HIT
    os_guard_control_stack(0, 1);
#endif
}
