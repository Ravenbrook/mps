/* $Header: /project/cmucl/cvsroot/src/lisp/globals.h,v 1.10 2005/09/15 18:26:51 rtoy Exp $ */

#ifndef _GLOBALS_H_
#define _GLOBALS_H_

#ifndef LANGUAGE_ASSEMBLY

#include "lisp.h"

extern int foreign_function_call_active;

extern lispobj *current_control_stack_pointer;
extern lispobj *current_control_frame_pointer;

#if !defined(ibmrt) && !defined(i386)
extern lispobj *current_binding_stack_pointer;
#endif

extern lispobj *read_only_space;
extern lispobj *static_space;
extern lispobj *dynamic_0_space;
extern lispobj *dynamic_1_space;
extern unsigned dynamic_space_size;
extern lispobj *control_stack;
extern lispobj *binding_stack;

#if (defined(i386) || defined(__x86_64))
extern lispobj *control_stack_end;
#endif
extern lispobj *current_dynamic_space;

#if !defined(ibmrt) && !defined(i386)
extern lispobj *current_dynamic_space_free_pointer;
extern lispobj *current_auto_gc_trigger;
#endif

extern void globals_init(void);

#else /* LANGUAGE_ASSEMBLY */

/* These are needed by ./assem.s */

#ifdef ppc
#ifdef DARWIN
#define EXTERN(name,bytes) .globl _/**/name
#else
#define EXTERN(name,bytes) .globl _/**/name
#endif
#endif
#ifdef mips
#define EXTERN(name,bytes) .extern name bytes
#endif
#ifdef sparc
#ifdef SVR4
#define EXTERN(name,bytes) .global name
#else
#define EXTERN(name,bytes) .global _ ## name
#endif
#endif
#ifdef ibmrt
#define EXTERN(name,bytes) .globl _/**/name
#endif

#ifdef i386
#ifdef __linux__
#define EXTERN(name,bytes) .globl _/**/name
#else
#define EXTERN(name,bytes) .global _ ## name
#endif
#endif

EXTERN(foreign_function_call_active, 4)

    EXTERN(current_control_stack_pointer, 4)
    EXTERN(current_control_frame_pointer, 4)
#if !defined(ibmrt) && !defined(i386)
    EXTERN(current_binding_stack_pointer, 4)
    EXTERN(current_dynamic_space_free_pointer, 4)
#endif
#ifdef mips
    EXTERN(current_flags_register, 4)
#endif
#endif /* LANGUAGE_ASSEMBLY */
#endif /* _GLOBALS_H_ */
